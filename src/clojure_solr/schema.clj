(ns clojure-solr.schema
  (:require [clojure.string :as str])
  (:import (org.apache.solr.client.solrj.impl HttpSolrClient)
           (org.apache.solr.client.solrj.request.schema SchemaRequest$Fields SchemaRequest$DynamicFields)
           (org.apache.solr.client.solrj.request.schema SchemaRequest$FieldType)
           (org.apache.solr.client.solrj.request.schema SchemaRequest$FieldTypes)
           (org.apache.solr.client.solrj.response.schema SchemaResponse$FieldsResponse SchemaResponse$DynamicFieldsResponse)
           (org.apache.solr.client.solrj.response.schema SchemaResponse$FieldTypeResponse FieldTypeRepresentation)
           (org.apache.solr.client.solrj.response.schema SchemaResponse$FieldTypesResponse)
           (org.apache.solr.common SolrInputDocument)
           (org.apache.solr.client.solrj SolrQuery SolrRequest$METHOD)
           (org.apache.solr.common.params ModifiableSolrParams))
  (:import [org.apache.http StatusLine HttpResponse]
           [org.apache.http.client HttpClient]
           [org.apache.http.client.methods HttpPost HttpGet]
           [org.apache.http.entity ByteArrayEntity]
           [org.apache.http.util EntityUtils])
  (:require [clojure-solr :as solr]))


(defn ^FieldTypeRepresentation get-field-type
  [type-name]
  (let [^SchemaResponse$FieldTypeResponse field-type-response (SchemaResponse$FieldTypeResponse.) 
        generic-response (.request clojure-solr/*connection* (SchemaRequest$FieldType. type-name))] 
    (when generic-response
      (.setResponse field-type-response generic-response) 
      (.getFieldType field-type-response))))

(defn get-field-type-attributes
  [type-name]
  (let [^FieldTypeRepresentation rep (get-field-type type-name)]
    (when rep
      (into {} (.getAttributes rep)))))

(defn get-schema
  "Get the complete schema for the current Solr connection (e.g., (with-connection (connect URL) (get-schema).
   Optional accept-type can be json (default), xml, or schema.xml.
   The schema is the :body key of the returned map, and must be parsed by a JSON or XML parser as appropriate.
   Throws an ex-info exception on failure."
  ([]
   (get-schema "json"))
  ([accept-type]
   (let [^HttpClient client (.getHttpClient solr/*connection*)
         [_ solr-server-url collection-or-core] (re-matches #"(https?://.+/solr)/([^/]+)(?:/.+)?" (.getBaseURL solr/*connection*))]
     (if solr-server-url
       (let [url (str solr-server-url "/" collection-or-core "/schema?wt=" accept-type)
             ^HttpGet get (HttpGet. url)
             ^HttpResponse response (.execute client get)]
         (if (>= (.getStatusCode ^StatusLine (.getStatusLine response)) 400)
           (let [entity (.getEntity response)
                 content-type (.getValue (.getContentType entity))
                 [_ content-type-basic] (re-matches #"([^;]+)(?:;.+)?" content-type) ]
             (throw (ex-info (format "Solr request failure from %s" url)
                             {:status (.getStatusCode (.getStatusLine response))
                              :content-type content-type
                              :body (if (#{"text/plain" "text/html" "application/json"} content-type-basic)
                                      (EntityUtils/toString entity)
                                      (EntityUtils/toByteArray entity))})))
           (let [entity (.getEntity response)
                 content-type (.getValue (.getContentType entity))]
             {:status (.getStatusCode (.getStatusLine response))
              :content-type content-type
              :body (EntityUtils/toString entity)})))))))
   
(defn coerce-to-clojure
  [solr-object]
  (cond (instance? java.util.List solr-object)
        (into []
              (for [item solr-object]
                (coerce-to-clojure item))) 
        (instance? org.apache.solr.common.util.NamedList solr-object)
        (into {}
              (for [^java.util.Map$Entry entry (iterator-seq (.iterator solr-object))]
                [(keyword (.getKey entry)) (coerce-to-clojure (.getValue entry))]
                ))
        :else solr-object))

(defn get-field-types
  []
  (let [generic-response (.request solr/*connection* (SchemaRequest$FieldTypes.))
        fields-response (SchemaResponse$FieldTypesResponse.)]
    (.setResponse fields-response generic-response)
    (for [field-type (.get (.getResponse fields-response) "fieldTypes") ]
      (coerce-to-clojure field-type))))

(defn get-schema-fields
  []
  (let [generic-response (.request solr/*connection* (SchemaRequest$Fields.))
        fields-response (SchemaResponse$FieldsResponse.)]
    (.setResponse fields-response generic-response)
    (map (fn [field-schema]
           (into {} (map (fn [[k v]] [(keyword k) v]) field-schema)))
         (.getFields fields-response))))

(defn get-dynamic-schema-fields
  []
  (let [generic-response (.request solr/*connection* (SchemaRequest$DynamicFields.))
        fields-response (SchemaResponse$DynamicFieldsResponse.)]
    (.setResponse fields-response generic-response)
    (map (fn [field-schema]
           (into {} (map (fn [[k v]] [(keyword k) v]) field-schema)))
         (.getDynamicFields fields-response))))

(defn get-unique-key
  []
  (.get (.request solr/*connection* (org.apache.solr.client.solrj.request.schema.SchemaRequest$UniqueKey.))
                     "uniqueKey"))

(defn get-copy-fields
  []
  (coerce-to-clojure (.get (.request solr/*connection* (org.apache.solr.client.solrj.request.schema.SchemaRequest$CopyFields.))
                           "copyFields")))

(defn get-schema-solrj
  []
  {:schema
   {:name "solr" :version 1.1 
    :uniqueKey (get-unique-key)
    :fieldTypes (get-field-types)
    :fields (get-schema-fields)
    :dynamicFields (get-dynamic-schema-fields)
    :copyFields (get-copy-fields) }})

(defn luke-desc-to-traits
  [str key-info]
  (into #{}
        (for [char (seq str) 
              :let [trait (get key-info char)]
              :when trait]
          trait)))

(def ^:dynamic *qf* nil)

(defn get-fields-via-luke
  "Use the Luke handler (must be configured in the core's solrconfig) to get actua schema information from the core.
   Returns a map of field names to schema info, where schema info "
  []
  (let [response (.getResponse (:query-results-obj (meta (solr/search "*:*" :qt "/admin/luke" :qf *qf*))))
        fields (.get response "fields")
        key-info (into {}
                       (for [[char desc] (into {} (.get (.get response "info") "key"))]
                         [(first char) (keyword (str/replace (str/lower-case desc) #"\s+" "-"))]))]
    (into {}
          (for [[field schema] (into {} fields)]
            [field (into {}
                         (for [[key val] schema]
                           [(keyword key)
                            (case key
                              ("schema" "index") (luke-desc-to-traits val key-info)
                              val)]))]))))
