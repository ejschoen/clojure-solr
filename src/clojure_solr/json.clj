(ns clojure-solr.json
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io])
  (:require [clj-time.core :as t])
  (:require [clj-time.format :as tformat])
  (:require [clj-time.coerce :as tcoerce])
  (:require [cheshire.core :as cheshire])
  (:require [clojure-solr :as solr])
  (:import (java.util Map)
           (org.apache.solr.common.util SimpleOrderedMap)
           (org.apache.solr.client.solrj.request.json JsonQueryRequest TermsFacetMap RangeFacetMap RangeFacetMap$OtherBuckets QueryFacetMap)))


(defprotocol Limit
  "A protocol for converting range facet start and end to values accepted by the SolrJ RangeFacetMap object"
  (coerce-limit [this]))

(extend-protocol Limit
  java.lang.Integer
  (coerce-limit [this] (long this))
  java.lang.Long
  (coerce-limit [this] this)
  java.lang.Float
  (coerce-limit [this] (double this))
  java.lang.Double
  (coerce-limit [this] this)
  java.util.Date
  (coerce-limit [this] this)
  org.joda.time.DateTime
  (coerce-limit [this] (tcoerce/to-date this))
  java.lang.String
  (coerce-limit [this] (tcoerce/to-date (tcoerce/from-string this))))

(defprotocol Gap
  "A protocol for converting range facet gap values to values accepted by the SolrJ RangeFacetMap object"
  (coerce-gap [this]))

(extend-protocol Gap
  java.lang.Integer
  (coerce-gap [this] (long this))
  java.lang.Long
  (coerce-gap [this] this)
  java.lang.Float
  (coerce-gap [this] (double this))
  java.lang.Double
  (coerce-gap [this] this)
  java.lang.String
  (coerce-gap [this] this))


(defn terms-facet
  "Create a facet command for term faceting"
  [field & {:keys [prefix min-count sub-facets offset limit include-total? facet-name]
            :or {facet-name (name field)}}]
  (let [facet-map (doto (TermsFacetMap. field)
                    (cond-> include-total? (.includeTotalNumBuckets true)
                            offset (.setBucketOffset offset)
                            limit (.setLimit limit)
                            prefix (.setTermPrefix prefix)
                            min-count (.setMinCount min-count)))]
    (when (not-empty sub-facets)
      (doseq [[field sub-facet-map] sub-facets]
        (.withSubFacet facet-map facet-name sub-facet-map)))
    facet-map))

(def ^:private facet-other-buckets
  {:after RangeFacetMap$OtherBuckets/AFTER
   :all RangeFacetMap$OtherBuckets/ALL
   :before RangeFacetMap$OtherBuckets/BEFORE
   :between RangeFacetMap$OtherBuckets/BETWEEN
   :none RangeFacetMap$OtherBuckets/NONE})

(defn range-facet
  "Create a facet command for range faceting"
  [field start end gap & {:keys [sub-facets hard-end? min-count other-buckets]}]
  (let [facet (doto (RangeFacetMap. field (coerce-limit start) (coerce-limit end) (coerce-gap gap))
                (.setHardEnd (boolean hard-end?))
                (.setMinCount (or min-count 1)))]
    (when (not-empty sub-facets)
      (doseq [[field sub-facet-map] sub-facets]
        (.withSubFacet facet (name field) sub-facet-map)))
    (when other-buckets
      (.setOtherBuckets facet (get facet-other-buckets other-buckets RangeFacetMap$OtherBuckets/NONE)))
    facet))

(defmulti format-result
  "Convert a SolrJ result to a Clojure data structure.  flags is a map of options:
   :date-function - Convert date values; function of a java.util.Date (default: clj-time.coerce/from-date)
   :keyfn         - Function for map keys; default: identity
   :recur?        - true to recur into map values (always recurs into arraylist values)"
  (fn [x flags] (class x)))

(defmethod format-result :default [x flags] x)

(defmethod format-result java.util.Date [x {:keys [date-function] :or {date-function tcoerce/from-date}}]
  (date-function x))

(defmethod format-result org.apache.solr.common.util.SimpleOrderedMap [x {:keys [keyfn recur?]
                                                                          :or {keyfn identity recur? true}
                                                                          :as flags}]
  (into {}
        (for [^java.util.Map$Entry entry (iterator-seq (.iterator x))]
          [(keyfn (.getKey entry))
           (if recur?
             (format-result (.getValue entry) flags)
             (.getValue entry))])))

(defmethod format-result java.util.ArrayList [x flags]
  (into [] (for [v x] (format-result v flags))))

(defn do-query
  "Execute the json query request and shallowly format the response into a map with keywordized keys
   (i.e., :responseHeader, :response, :facets).
   The result has metadata with:
    :request      - The value of request
    :request-json - The value of request converted into a JSON object
    :response     - The raw response"
  [^JsonQueryRequest request flags]
  (let [raw-response (.request solr/*connection* request)]
    (with-meta
      (format-result raw-response {:recur? false :keyfn keyword})
      {:request request
       :request-json (with-open [stream (java.io.ByteArrayOutputStream.)]
                       (.write (.getContentWriter request "application/json") stream)
                       (with-open [input-stream (java.io.ByteArrayInputStream. (.toByteArray stream))
                                   reader (io/reader input-stream)]
                         (doall (cheshire/parse-stream reader))))
       :response raw-response})))

(defn process-facets
  "Recursively process facets into a format similar to what clojure-solr/search* returns"
  [^SimpleOrderedMap facets facet-map {:keys [facet-hier-sep] :as flags}]
  (doall
   (for [[facet-name facet-description] facet-map]
     (let [^SimpleOrderedMap facet-response (.get facets facet-name)
           ^java.util.List buckets (.get facet-response "buckets")]
       {:name facet-name
        :values (for [^SimpleOrderedMap bucket buckets
                      :let [val (.get bucket "val")]]
                  (merge {:value (format-result val {})
                          :count (.get bucket "count")}
                         (when (and facet-hier-sep (string? val))
                           (let [split-path (str/split val facet-hier-sep)]
                             {:split-path split-path
                              :depth (count split-path)
                              :title (last split-path)}))
                         (when-let [sub-facets (get facet-description "facet")]
                           {:facets (process-facets bucket sub-facets flags)})))}))))

(defn wrap-faceting
  "Faceting middleware: Handles facets option of flags and processes the result."
  [handler]
  (fn [^JsonQueryRequest request {:keys [facets] :as flags}]
    (if (not-empty facets)
      (do (doseq [[facet-name facet-map] facets] (.withFacet request (name facet-name) facet-map))
          (let [result (handler request flags)]
            (with-meta
              (-> result
                  (update-in [:facets] process-facets (get (:request-json (meta result)) "facet") flags)
                  (dissoc "facets"))
              (meta result))))
      (handler request flags))))

(defn wrap-core-search
  "Core search middleware: handles query, sort, limit, offset, fields, filters, params, and methods,
   and formats the result as a sequence of maps (one per Solr doc)"
  [handler]
  (fn [^JsonQueryRequest request {:keys [params filters sort limit offset fields method]
                                  :or {method :post}
                                  :as flags}]
    (doto request
      (cond-> sort (.setSort sort)
              limit (.setLimit limit)
              offset (.setOffset offset)
              fields (.returnFields fields)
              method (.setMethod (get solr/http-methods method (get solr/http-methods :post)))))
    (when (not-empty filters)
      (doseq [filter filters]
        (.withFilter request (solr/format-facet-query filter))))
    (when (not-empty params)
      (doseq [[param value] params]
        (.withParam request (name param) value)))
    (let [result (handler request flags)]
      (with-meta
        (-> result
            (update-in [:response] #(format-result % {}))
            (dissoc "response"))
        (meta result)))))

(def jsolr-app
  (-> do-query
      wrap-core-search
      wrap-faceting))

(defn query
  "Query Solr with the JSON api and return the result"
  [query {:keys [middleware params filters facets sort limit offset fields method facet-hier-sep]
          :or {method :post
               middleware jsolr-app}
          :as flags}]
  (let [request (doto (JsonQueryRequest.)
                  (.setQuery query))]
    (middleware request flags)))
                  
    
