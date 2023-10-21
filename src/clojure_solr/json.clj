(ns clojure-solr.json
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint])
  (:require [clj-time.core :as t])
  (:require [clj-time.format :as tformat])
  (:require [clj-time.coerce :as tcoerce])
  (:require [clojure-solr :as solr])
  (:import (java.util Map)
           (org.apache.solr.client.solrj.request.json JsonQueryRequest TermsFacetMap RangeFacetMap QueryFacetMap)))


(defprotocol Limit
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
  [field & {:keys [prefix min-count sub-facets]}]
  (let [facet-map (doto (TermsFacetMap. field)
                    (cond-> prefix (.setTermPrefix prefix)
                            min-count (.setMinCount min-count)))]
    (when (not-empty sub-facets)
      (doseq [[field sub-facet-map] sub-facets]
        (.withSubFacet facet-map (name field) sub-facet-map)))
    facet-map))

(defn range-facet
  [field start end gap & {:keys [sub-facets hard-end? min-count]}]
  (let [facet (doto (RangeFacetMap. field (coerce-limit start) (coerce-limit end) (coerce-gap gap))
                (.setHardEnd (boolean hard-end?))
                (.setMinCount (or min-count 1)))]
    (when (not-empty sub-facets)
      (doseq [[field sub-facet-map] sub-facets]
        (.withSubFacet facet (name field) sub-facet-map)))
    facet))

(defn query
  [query {:keys [params filters facets sort limit offset fields method]
          :or {method :post}}]
  (let [request (doto (JsonQueryRequest.)
                  (.setQuery query)
                  (cond-> sort (.setSort sort)
                          limit (.setLimit limit)
                          offset (.setOffset offset)
                          fields (.returnFields fields)
                          method (.setMethod (get solr/http-methods method (get solr/http-methods :post)))))]
    (when (not-empty filters)
      (doseq [filter filters]
        (.withFilter request (solr/format-facet-query filter))))
    (when (not-empty params)
      (doseq [[param value] params]
        (.withParam request (name param) value)))
    (when (not-empty facets)
      (doseq [[facet-name facet-map] facets] (.withFacet request (name facet-name) facet-map)))
    request))
                  
    
(defmulti format-result class)

(defmethod format-result :default [x] x)

(defmethod format-result java.util.Date [x]
  (tcoerce/from-date x))

(defmethod format-result org.apache.solr.common.util.SimpleOrderedMap [x]
  (into {}
        (for [^java.util.Map$Entry entry (iterator-seq (.iterator x))]
          [(.getKey entry) (format-result (.getValue entry))])))

(defmethod format-result java.util.ArrayList [x]
  (into [] (for [v x] (format-result v))))
