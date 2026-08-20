(ns clojure-solr.request
  "Version-neutral replacement for the raw HTTP that clojure-solr.admin,
   clojure-solr.schema and clojure-solr.security used to perform.

   Those functions reached through (.getHttpClient *connection*) to get the
   underlying Apache client and issued their own GET and POST.  SolrJ 10 has no
   such method -- there is no Apache client to reach.

   GenericSolrRequest replaces it, and is API-compatible across SolrJ 9 and 10:
   the constructors, setRequiresCollection, setContentWriter, withContent and
   getParams are identical in both.  So this namespace names no class that moved,
   with one exception routed through clojure-solr.impl: the response parser that
   yields the raw stream, which changed packages.

   Paths here are resolved against the connection's base URL with
   setRequiresCollection false, so callers pass a path rooted at the Solr
   instance -- \"/admin/info/system\", not a full URL."
  (:require [clojure-solr.impl :as impl])
  (:import (java.io InputStream)
           (org.apache.solr.client.solrj SolrClient SolrRequest$METHOD)
           (org.apache.solr.client.solrj.request GenericSolrRequest)
           (org.apache.solr.common.params ModifiableSolrParams)
           (org.apache.solr.common.util NamedList Utils)))

(def ^:private http-methods
  {:get SolrRequest$METHOD/GET
   :post SolrRequest$METHOD/POST
   :put SolrRequest$METHOD/PUT
   :delete SolrRequest$METHOD/DELETE})

(defn- ->params ^ModifiableSolrParams [params]
  (let [p (ModifiableSolrParams.)]
    (doseq [[k v] params :when (some? v)]
      (.set p (name k) (into-array String [(str v)])))
    p))

(defn ->clj
  "Convert a NamedList or List of them into Clojure data with keyword keys."
  [x]
  (cond (instance? NamedList x)
        (into {} (for [^java.util.Map$Entry e (iterator-seq (.iterator ^NamedList x))]
                   [(keyword (.getKey e)) (->clj (.getValue e))]))
        (instance? java.util.List x) (mapv ->clj x)
        (instance? java.util.Map x)
        (into {} (for [[k v] x] [(keyword (str k)) (->clj v)]))
        :else x))

(defn ->json-string
  "Serialise a parsed Solr response back to JSON.  Used where a function's
   contract is to hand back the response body as a string."
  [x]
  (Utils/toJSONString x))

(defn- build
  ^GenericSolrRequest
  [method path {:keys [params content content-type response-parser]}]
  (let [^GenericSolrRequest r (GenericSolrRequest. (or (get http-methods method)
                                                       (throw (ex-info "Unknown HTTP method"
                                                                       {:method method})))
                                                   path
                                                   (->params params))]
    (.setRequiresCollection r false)
    (when content (.withContent r content (or content-type "application/json")))
    (when response-parser (.setResponseParser r response-parser))
    r))

(defn request
  "Issue a request against conn and return the parsed response as a NamedList.

   opts:
     :params        map of query parameters
     :content       byte array body
     :content-type  defaults to application/json when :content is given

   SolrJ raises on a non-2xx response, so callers do not check a status code."
  ([conn method path] (request conn method path {}))
  ([^SolrClient conn method path opts]
   (.request conn (build method path opts) nil)))

(defn request-clj
  "As request, with the response converted to Clojure data."
  ([conn method path] (request-clj conn method path {}))
  ([conn method path opts] (->clj (request conn method path opts))))

(defn request-stream
  "Issue a request and return {:status :body} with the body as an unparsed
   string, for callers whose contract is to hand back the response verbatim --
   get-schema being the case that matters, since it can be asked for XML.

   Uses the raw-stream response parser, which is the one class here whose
   package differs between SolrJ 9 and 10."
  ([conn method path] (request-stream conn method path {}))
  ([conn method path opts]
   (let [wt (or (get-in opts [:params :wt]) "json")
         parser (impl/stream-response-parser (impl/impl) wt)
         ^NamedList resp (request conn method path (assoc opts :response-parser parser))
         status (.get resp "responseStatus")
         ^InputStream stream (.get resp "stream")]
     {:status (if (number? status) (int status) 200)
      :body (when stream (with-open [s stream] (slurp s)))})))
