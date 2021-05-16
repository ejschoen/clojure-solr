(ns clojure-solr
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint])
  (:require [clj-time.core :as t])
  (:require [clj-time.format :as tformat])
  (:require [clj-time.coerce :as tcoerce])
  (:import (java.net URI)
           (java.util Base64 HashMap List ArrayList)
           (java.nio.charset Charset)
           (org.apache.http HttpRequest HttpRequestInterceptor HttpHeaders)
           (org.apache.http.auth AuthScope UsernamePasswordCredentials)
           (org.apache.http.client.protocol HttpClientContext)
           (org.apache.http.impl.auth BasicScheme)
           (org.apache.http.impl.client BasicCredentialsProvider HttpClientBuilder CloseableHttpClient)
           (org.apache.http.protocol HttpContext HttpCoreContext)
           (org.apache.solr.client.solrj SolrQuery SolrRequest$METHOD SolrClient)
           (org.apache.solr.client.solrj.impl HttpSolrClient HttpSolrClient$Builder HttpClientUtil)
           (org.apache.solr.client.solrj.embedded EmbeddedSolrServer)
           (org.apache.solr.client.solrj.response QueryResponse FacetField PivotField RangeFacet RangeFacet$Count RangeFacet$Date)
           (org.apache.solr.common SolrInputDocument SolrDocumentList)
           (org.apache.solr.common.params ModifiableSolrParams CursorMarkParams MoreLikeThisParams)
           (org.apache.solr.common.util NamedList)
           (org.apache.solr.util DateMathParser)))

(defonce ^:private url-details (atom {}))
(defonce ^{:private true :tag BasicCredentialsProvider}  credentials-provider (BasicCredentialsProvider.))

(declare ^{:dynamic true :tag SolrClient} *connection*)

(def ^:dynamic *trace-fn* nil)

(declare make-param)

(def default-method (atom :post))

(def http-methods {:get SolrRequest$METHOD/GET, :GET SolrRequest$METHOD/GET
                   :post SolrRequest$METHOD/POST, :POST SolrRequest$METHOD/POST})

(defn set-default-method!
  "Set the default method for Solr requests."
  [method]
  (if (get http-methods method)
    (reset! default-method method)
    (throw (Exception. (format "Invalid Solr HTTP method: %s" method)))))

(defn get-default-method
  "Get the current default method for Solr requests."
  []
  @default-method)

(defn trace
  [str]
  (when *trace-fn*
    (*trace-fn* str)))

(defmacro trace
  [str-expr]
  `(when *trace-fn*
     (let [str# ~str-expr]
       (*trace-fn* str#))))

(defmacro with-trace
  [fn & body]
  `(binding [*trace-fn* ~fn]
     ~@body))

;; These next credential functions are for setting basic auth
;; when needing to access Solr behind a proxy such as
;; apache2 or nginx.
;; However, these are also compatible with the BasicAuthenticationPlugin.
(defn make-basic-credentials
  [name password]
  (UsernamePasswordCredentials. name password))

(defn make-auth-scope
  [host port]
  (AuthScope. host port))

(defn clear-credentials
  "Remove *all* basic authentication credentials for all Solr URIs."
  []
  (.clear credentials-provider))

(defn set-credentials
  "Set basic authentication credentials for a given Solr URI."
  [uri name password]
  (.setCredentials credentials-provider
                   (make-auth-scope (.getHost uri) (.getPort uri))
                   (make-basic-credentials name password)))

(defn get-url-details
  [url]
  (let [details (get @url-details url)]
    (if details
      details
      (let [[_ scheme name password rest] (re-matches #"(https?)://(.+):(.+)@(.*)" url)
            details (if (and scheme name password rest)
                      {:clean-url (str scheme "://" rest)
                       :password password
                       :name name}
                      {:clean-url url})
            uri (URI. (:clean-url details))]
        (swap! url-details assoc url details)
        (when (and name password)
          (let [host (if (= (.getPort uri) -1)
                       (str (.getScheme uri) "://" (.getHost uri))
                       (str (.getScheme uri) "://" (.getHost uri) ":" (.getPort uri)))]
            ;;(println "**** CLOJURE-SOLR: Adding credentials for host" host)
            (set-credentials uri name password)))
        details))))

(defn ^HttpSolrClient connect [url & conn-manager]
  "Create an HttpSolrClient connection to a Solr URI. Optionally use
   a provided connection-manager, such as PoolingHttpClientConnectionManager,
   for situations where Solr's default connection manager cannot keep up
   with demand."
  (let [params (ModifiableSolrParams.)
        {:keys [clean-url name password]} (get-url-details url)
        builder (doto (HttpClientBuilder/create)
                  (.setDefaultCredentialsProvider credentials-provider)
                  (.setConnectionManager (when conn-manager (first conn-manager)))
                  (.addInterceptorFirst 
                   (reify 
                     HttpRequestInterceptor
                     (^void process [this ^HttpRequest request ^HttpContext context]
                      (let [auth-state (.getAttribute context HttpClientContext/TARGET_AUTH_STATE)]
                        (when (nil? (.getAuthScheme auth-state))
                          (let [target-host (.getAttribute context HttpCoreContext/HTTP_TARGET_HOST)
                                auth-scope (make-auth-scope (.getHostName target-host) (.getPort target-host))
                                creds (.getCredentials credentials-provider auth-scope)]
                            ;;(println "**** CLOJURE-SOLR: HttpRequestInterceptor here.  Looking for" auth-scope "creds:" creds)
                            (when creds
                              (.update auth-state (BasicScheme.) creds)))))))))
        client ^CloseableHttpClient (.build builder)
        solr-builder ^HttpSolrClient$Builder (doto (HttpSolrClient$Builder. clean-url)
                                               (.withHttpClient client))]
    (.build solr-builder)))

(defn- make-document [boost-map doc]
  (let [^SolrInputDocument sdoc (SolrInputDocument. (make-array String 0))]
    (doseq [[key value] doc]
      (let [key-string (name key)
            boost (get boost-map key)]
        (if boost
          (.addField sdoc key-string value boost)
          (.addField sdoc key-string value))))
    sdoc))

(defn add-document!
  ([doc boost-map]
   (.add ^SolrClient *connection* (make-document boost-map doc)))
  ([doc]
   (add-document! doc {})))

(defn add-documents!
  ([coll boost-map]
   (.add ^SolrClient *connection* (map (partial make-document boost-map) coll)))
  ([coll]
   (add-documents! coll {})))

(defn commit! []
  (.commit ^SolrClient *connection*))

(defn- doc-to-hash [doc]
  (into {} (for [[k v] (clojure.lang.PersistentArrayMap/create doc)]
             [(keyword k)
              (cond (= java.util.ArrayList (type v)) (into [] v)
                    :else v)])))

(defn- make-param [p]
  (cond
   (string? p) (into-array String [p])
   (coll? p) (into-array String (map str p))
   :else (into-array String [(str p)])))

(defn- ^SolrRequest$METHOD parse-method [method]
  (get http-methods method (get http-methods @default-method)))

(defn extract-facets
  [query-results facet-hier-sep limiting? formatters key-fields]
  (map (fn [^FacetField f]
         (let [field-name (.getName f)
               facet-name (get key-fields field-name field-name)]
           {:name facet-name
            :values (sort-by :path
                             (map (fn [v]
                                    (let [result
                                          (merge
                                           {:value (.getName v)
                                            :count (.getCount v)}
                                           (when-let [split-path (and facet-hier-sep (str/split (.getName v) facet-hier-sep))]
                                             {:split-path split-path
                                              :title (last split-path)
                                              :depth (count split-path)}))]
                                      ((get formatters facet-name identity) result)))
                                  (.getValues f)))}))
       ^List (if limiting?
               (.getLimitingFacets query-results)
               (.getFacetFields query-results))))

(def date-time-formatter
  (tformat/formatters :date-time-no-ms))

(def query-result-date-time-parser
  (tformat/formatter t/utc "YYYY-MM-dd'T'HH:mm:ss.SSS'Z'" "YYYY-MM-dd'T'HH:mm:ss'Z'"))

(defmulti parse-range-value (fn [str start] (type start)))

(defmethod parse-range-value java.lang.Integer [str start] (Integer/parseInt str))
(defmethod parse-range-value java.lang.Long [str start] (Long/parseLong str))
(defmethod parse-range-value java.lang.Float [str start] (Float/parseFloat str))
(defmethod parse-range-value java.lang.Double [str start] (Double/parseDouble str))
(defmethod parse-range-value java.util.Date [str start] (tcoerce/to-date (tcoerce/from-string str)))
(defmethod parse-range-value :default [str start] str)

(defmulti format-range-value (fn [val timezone end?] (type val)))

(defmethod format-range-value java.util.Date [val timezone end?]
  (let [d (tcoerce/from-date val)]
    (tformat/unparse (if timezone (tformat/with-zone date-time-formatter timezone) date-time-formatter)
                     (if end? (t/minus d (t/seconds 1)) d))))

(defmethod format-range-value org.joda.time.DateTime [val timezone end?]
  (tformat/unparse (if timezone (tformat/with-zone date-time-formatter timezone) date-time-formatter)
                     (if end? (t/minus val (t/seconds 1)) val)))

(defmethod format-range-value java.lang.String [val timezone end?]
  (try (let [d (tformat/parse date-time-formatter val)]
         (format-range-value d timezone end?))
       (catch Exception _
         val)))
       
(defmethod format-range-value :default [val timezone end?] val)


#_(defn format-range-value
  "Timezone is only used if it's a date facet (and timezone is not null)."
  [val timezone end?]
  (let [d (try (tformat/parse date-time-formatter val)
               (catch Exception _))]
    (cond (or d (instance? java.util.Date val))
          (let [d (or d (tcoerce/from-date val))]
            (tformat/unparse (if timezone (tformat/with-zone date-time-formatter timezone) date-time-formatter)
                             (if end? (t/minus d (t/seconds 1)) d)))
          :else (str val))))

(defn format-standard-filter-query
  ([name value]
   (format "%s:%s" name value))
  ([{:keys [name value tag] :as query}]
   (if (not-empty tag)
     (format "{!tag=%s}%s:%s" tag name value)
     (format "%s:%s" name value))))

(defn format-raw-query
  ([query]
   (if (not-empty (:tag query))
     (format "{!raw f=%s tag=%s}%s" (:name query) (:tag query) (:value query))
     (format-raw-query (:name query) (:value query))))
  ([name value]
   (format "{!raw f=%s}%s" name value)))

(defn format-facet-query
  [{:keys [name value tag formatter full-formatter] :as query}]
  (if (and (string? value) (re-find #"\[" value)) ;; range filter
    (format-standard-filter-query name value)
    (cond full-formatter (full-formatter query)
          formatter (formatter name value)
          :else (format-raw-query query))))

(defn extract-facet-ranges
  "Explicitly pass facet-date-ranges in case one or more ranges are date ranges, and we need to grab a timezone."
  [query-results facet-date-ranges]
  (sort-by :name
           (map (fn [^RangeFacet r]
                  (let [date-range? (some (fn [{:keys [field]}] (= field (.getName r)))
                                          facet-date-ranges)
                        timezone (:timezone (first (filter (fn [{:keys [field]}] (= field (.getName r)))
                                                           facet-date-ranges)))
                        gap (.getGap r)
                        attribute-type (type gap)
                        values
                        (map (fn [val]
                               (let [start-val (parse-range-value (.getValue val) (.getStart r))
                                     start-str (format-range-value start-val nil false)
                                     end-val (cond date-range?
                                                   (.parseMath (doto (DateMathParser.)
                                                                 (.setNow
                                                                  start-val #_(tcoerce/to-date
                                                                               (tformat/parse query-result-date-time-parser start-val))))
                                                               gap)
                                                   :else (+ start-val gap))
                                     end-str (format-range-value end-val nil false)]
                                 {:count (.getCount val)
                                  :value (format "[%s TO %s}" start-str end-str)
                                  ;;:value (format (if date-range? "[%s TO %s]" "[%s TO %s}") start-str end-str)
                                  :min-inclusive start-val
                                  :max-noninclusive end-val}))
                             (.getCounts r))
                        values-before (if (and (.getBefore r) (> (.getBefore r) 0))
                                        (concat [{:count (.getBefore r)
                                                  :value (format (if date-range? "[* TO %s]" "[* TO %s}")
                                                                 (format-range-value (.getStart r) nil true))
                                                  :min-inclusive nil
                                                  :max-noninclusive (.getStart r)}]
                                                values)
                                        values)
                        values-before-after (if (and (.getAfter r) (> (.getAfter r) 0))
                                              (concat values-before
                                                      [{:count (.getAfter r)
                                                        :value (format "[%s TO *]"
                                                                       (format-range-value (.getEnd r) nil false))
                                                        :min-inclusive (.getEnd r)
                                                        :max-noninclusive nil}])
                                              values-before)]
                    {:name   (.getName r)
                     :values (map #(dissoc % :orig-value) values-before-after)
                     :start  (.getStart r)
                     :end    (.getEnd r)
                     :gap    (.getGap r)
                     :before (.getBefore r)
                     :after  (.getAfter r)}))
                (.getFacetRanges query-results))))

(defn extract-facet-queries
  [facet-queries result]
  (filter identity
          (map (fn [query]
                 (let [formatted-query (if (string? query) query (format-facet-query query))
                       count (get result formatted-query)]
                   (when count
                     (if (string? query)
                       {:query query :count count}
                       (assoc query :count count)))))
               facet-queries)))

(defn extract-pivots
  [^QueryResponse query-results facet-date-ranges]
  (let [^NamedList facet-pivot (.getFacetPivot query-results)]
    (when facet-pivot
      (into
       {}
       (map
        (fn [index]
          (let [facet1-name (.getName facet-pivot index)
                ^ArrayList pivot-fields (.getVal facet-pivot index)
                ranges (into {}
                             (for [^PivotField pivot-field pivot-fields
                                   :let [facet1-value (.getValue pivot-field)
                                         facet-ranges (extract-facet-ranges pivot-field facet-date-ranges)]
                                   :when (not-empty facet-ranges)]
                               [facet1-value
                                (into {}
                                      (map (fn [range]
                                             [(:name range) (:values range)])
                                           facet-ranges))]))
                pivot-counts (into {}
                                   (for [^PivotField pivot-field pivot-fields]
                                     (let [facet1-value (.getValue pivot-field)
                                           ^List pivot-values (.getPivot pivot-field)]
                                       [facet1-value
                                        (into {}
                                              (map (fn [^PivotField pivot]
                                                     (if (.getFacetRanges pivot)
                                                       [(.getValue pivot)
                                                        {:count (.getCount pivot)
                                                         :ranges (extract-facet-ranges pivot facet-date-ranges)}]
                                                       [(.getValue pivot)
                                                        (.getCount pivot)]))
                                                   pivot-values))])))]
            ;;(println facet1-name)
            [facet1-name (cond (and (not-empty ranges) (not-empty pivot-counts) (= (count ranges) (count pivot-counts)))
                               {:counts pivot-counts :ranges ranges}
                               (not-empty pivot-counts)
                               pivot-counts
                               (not-empty ranges)
                               ranges
                               :else nil)]))
        (range 0 (.size facet-pivot)))))))

(defn show-query
  [q flags]
  (trace "Solr Query:")
  (trace q)
  (trace "  Facet filters:")
  (if (not-empty (:facet-filters flags))
    (doseq [ff (:facet-filters flags)]
      (trace (format "    %s" (pr-str (format-facet-query ff)))))
    (trace "    none"))
  (trace "  Facet queries:")
  (if (not-empty (:facet-queries flags))
    (doseq [ff (:facet-queries flags)]
      (trace (format "    %s" (format-facet-query ff))))
    (trace "    none"))
  (trace "  Facet fields:")
  (if (not-empty (:facet-fields flags))
    (doseq [ff (:facet-fields flags)]
      (trace (format "    %s" (if (map? ff) (pr-str ff) ff))))
    (trace "    none"))
  (trace "  Facet Numeric Ranges")
  (if (not-empty (:facet-numeric-ranges flags))
    (doseq [ff (:facet-numeric-ranges flags)]
      (trace (format "    start: %s gap: %s end: %s" (:start ff) (:gap ff) (:end ff))))
    (trace "    none"))
  (let [other (dissoc flags :facet-filters :facet-qieries :facet-fields)]
    (when (not-empty other)
      (trace "  Other parameters to Solr:")
      (doseq [[k v] other]
        (trace (format "  %s: %s" k (pr-str v))))))
  )

(def facet-exclude-parameters
  #{:name :result-formatter})

(def facet-parameter-formatters
  {:ex #(format "ex=%s" %2)
   :default #(format "facet.%s=\"%s\"" (name %1) %2)})

(defn search*
  "Query solr through solrj.
   q: Query field
   Optional keys, passed in a map:
     :method                :get or :post (default :get)
     :rows                  Number of rows to return (default is Solr default: 1000)
     :start                 Offset into query result at which to start returning rows (default 0)
     :fields                Fields to return
     :facet-fields          Discrete-valued fields to facet.  Can be a string, keyword, or map containing
                            {:name ... :prefix ... :ex (to exclude from filtering)}.
     :facet-queries         Vector of facet queries, each encoded in a string or a map of
                            {:name, :value, :formatter}.  :formatter is optional and defaults to the raw query formatter.
                            The result is in the :facet-queries response.
     :facet-date-ranges     Date fields to facet as a vector or maps.  Each map contains
                             :field   Field name
                             :tag     Optional, for referencing in a pivot facet
1                             :start   Earliest date (as java.util.Date)
                             :end     Latest date (as java.util.Date)
                             :gap     Faceting gap, as String, per Solr (+1HOUR, etc)
                             :others  Comma-separated string: before,after,between,none,all.  Optional.
                             :include Comma-separated string: lower,upper,edge,outer,all.  Optional.
                             :hardend Boolean (See Solr doc).  Optional.
                             :missing Boolean--return empty buckets if true.  Optional.
     :facet-numeric-ranges  Numeric fields to facet, as a vector of maps.  Map fields as for
                            date ranges, but start, end and gap must be numbers.
     :facet-mincount        Minimum number of docs in a facet for the bucket to be returned.
     :facet-hier-sep        Useful for path hierarchy token faceting.  A regex, such as \\|.
     :facet-filters         Solr filter expression on facet values.  Passed as a map in the form:
                            {:name 'facet-name' :value 'facet-value' 
                             :formatter (fn [name value] ...)
                             :full-formatter 'Alternative formatter, takes entire facet-filter map as one parameter'
                             :tag 'Optional, for referencing in an ex-tag in facet-fields (needs full-formatter option)' }
                            where :formatter is optional and is used to format the query.
     :facet-pivot-fields    Vector of pivots to compute, each a list of facet fields.
                            If a facet is tagged (e.g., {:tag ts} in :facet-date-ranges)  
                            then the string should be {!range=ts}other-facet.  Otherwise,
                            use comma separated lists: this-facet,other-facet.
     :cursor-mark           true -- initial cursor; else a previous cursor value from 
                            (:next-cursor-mark (meta result))
     :collapse              field name or map with CollapsingQParser parameters.
     :expand                true or map with {:rows ... :q ... :fq ... :sort ... }
                            for the ExpandComponent.
     :just-return-query?    Just return the query that would be run on Solr
  Additional keys can be passed, using Solr parameter names as keywords.
  Returns the query results as the value of the call, with faceting results as metadata.
  Use (meta result) to get facet data."
  [q {:keys [method fields facet-fields facet-date-ranges facet-numeric-ranges facet-queries
             facet-mincount facet-hier-sep facet-filters facet-pivot-fields cursor-mark
             collapse expand just-return-query?] :as flags}]
  (when *trace-fn*
    (show-query q flags))
  (let [flags (dissoc flags :just-return-query?)
        ^SolrQuery query (cond (string? q) (SolrQuery. q)
                               (instance? SolrQuery q) q
                               :else (throw (Exception. "q parameter must be a string or SolrQuery")))
        method (parse-method method)
        _ (trace (format "method: %s default-method: %s" method @default-method))
        facet-result-formatters (into {} (map #(if (map? %)
                                                 [(:name %) (:result-formatter % identity)]
                                                 [% identity])
                                              facet-fields))
        facet-key-fields (into {} (map-indexed (fn [i f]
                                                 [(format "f%d" i) (if (map? f) (:name f) (name f))])
                                               facet-fields))
        facet-field-keys (into {} (map-indexed (fn [i f]
                                                 [(if (map? f) (:name f) (name f)) (format "f%d" i)])
                                               facet-fields))]
    (doseq [[key value] (dissoc flags
                                :method :facet-fields :facet-date-ranges :facet-numeric-ranges :facet-filters
                                :cursor-mark :collapse :expand)]
      (.setParam query (name key) (make-param value)))
    (when (not (empty? fields))
      (cond (string? fields)
            (.setFields query (into-array (str/split fields #",")))
            (or (seq? fields) (vector? fields))
            (.setFields query (into-array
                               (map (fn [f]
                                      (cond (string? f) f
                                            (keyword? f) (name f)
                                            :else (throw (Exception. (format "Unsupported field name: %s" f)))))
                                    fields)))
            :else (throw (Exception. (format "Unsupported :fields parameter format: %s" fields)))))

    ;; How to facet the same attribute different ways (using different prefixes)
    ;; https://stackoverflow.com/questions/31340400/multiple-facet-prefix-on-a-single-facet-field
    ;; http://192.168.0.200:8983/solr/i2ksearch/select?facet.field={!key=f1%20facet.prefix="prefix1"}attribute&facet.field={!key=f2%20facet.prefix="prefix 2"}attribute&facet.limit=300&facet.query=attribute:<<query>>&facet=on&fq=attribute:<<query>>&rows=0

    (let [facet-field-parameters
          (for [facet-field facet-fields
                :let [field-name (if (map? facet-field)
                                   (:name facet-field)
                                   (name facet-field))
                      key (get facet-field-keys field-name)]]
            (if (map? facet-field)
              (let [local-params
                    (reduce-kv (fn [params k v]
                                 (let [formatter (get facet-parameter-formatters k (get facet-parameter-formatters :default))]
                                   (if (facet-exclude-parameters k)
                                     params
                                     (format "%s %s" params (formatter k v)))))
                               (format "!key=%s" key)
                               facet-field)]
                (format "{%s}%s" local-params field-name))
              (format "{!key=%s}%s" key field-name)))]
      (when (not-empty facet-field-parameters)
        (.addFacetField query (into-array String facet-field-parameters))))

    (doseq [facet-query facet-queries]
      (cond (string? facet-query)
            (.addFacetQuery query facet-query)
            (map? facet-query)
            (let [formatted-query (format-facet-query facet-query)]
              (when (not-empty formatted-query) (.addFacetQuery query formatted-query)))
            :else (throw (Exception. "Invalid facet query.  Must be a string or a map of {:name, :value, :formatter (optional)}"))))
    (doseq [{:keys [field start end gap others include hardend missing mincount tag]} facet-date-ranges]
      (if tag
        ;; This is a workaround for a Solrj bug that causes tagged queries to be improperly formatted.
        (do (.setParam query "facet" true)
            (.add query "facet.range" (into-array String [(format "{!tag=%s}%s" tag field)]))
            (.add query (format "f.%s.facet.range.start" field)
                  (into-array String [(tformat/unparse (tformat/formatters :date-time-no-ms)
                                                         (tcoerce/from-date start))]))
            (.add query (format "f.%s.facet.range.end" field)
                  (into-array String [(tformat/unparse (tformat/formatters :date-time-no-ms)
                                                         (tcoerce/from-date end))]))
            (.add query (format "f.%s.facet.range.gap" field) (into-array String [gap])))
        (.addDateRangeFacet query field start end gap))
      (when missing (.setParam query (format "f.%s.facet.missing" field) true))
      (when others (.setParam query (format "f.%s.facet.range.other" field) (into-array String others)))
      (when include (.setParam query (format "f.%s.facet.range.include" field) (into-array String [include])))
      (when hardend (.setParam query (format "f.%s.facet.range.hardend" field) hardend)))
    (doseq [{:keys [field start end gap others include hardend missing mincount tag]} facet-numeric-ranges]
      (assert (instance? Number start))
      (assert (instance? Number end))
      (assert (instance? Number gap))
      (if tag
        ;; This is a workaround for a Solrj bug that causes tagged queries to be improperly formatted.
        (do (.setParam query "facet" true)
            (.add query "facet.range" (into-array String [(format "{!tag=%s}%s" tag field)]))
            (.add query (format "f.%s.facet.range.start" field) (into-array String [(.toString start)]))
            (.add query (format "f.%s.facet.range.end" field) (into-array String [(.toString end)]))
            (.add query (format "f.%s.facet.range.gap" field) (into-array String [(.toString gap)])))
        (.addNumericRangeFacet query field start end gap))
      (when missing (.setParam query (format "f.%s.facet.missing" field) true))
      (when others (.setParam query (format "f.%s.facet.range.other" field) (into-array String others)))
      (when include (.setParam query (format "f.%s.facet.range.include" field) (into-array String [include])))
      (when hardend (.setParam query (format "f.%s.facet.range.hardend" field) hardend)))
    (doseq [field facet-pivot-fields]
      (.addFacetPivotField query (into-array String [field])))
    (.addFilterQuery query (into-array String (filter not-empty (map format-facet-query facet-filters))))
    (.setFacetMinCount query (or facet-mincount 1))
    (cond (= cursor-mark true)
          (.setParam query ^String (CursorMarkParams/CURSOR_MARK_PARAM) (into-array String [(CursorMarkParams/CURSOR_MARK_START)]))
          (not (nil? cursor-mark))
          (.setParam query ^String (CursorMarkParams/CURSOR_MARK_PARAM) (into-array String [cursor-mark])))

    (when collapse
      (cond (string? collapse)
            (.addFilterQuery query (into-array String [(format "{!collapse field=%s}" collapse)]))
            (map? collapse)
            (.addFilterQuery query (into-array String [(format "{!collapse %s}"
                                                               (str/join " "
                                                                         (for [[key val] collapse]
                                                                           (format "%s=%s" (name key) val))))]))
            :else (throw (Exception. "collapse parameter must be string or map"))))
    (when expand
      (.setParam query "expand" true)
      (cond (string? expand)
            nil
            (map? expand)
            (doseq [[key val] expand]
              (.setParam query (format "expand.%s" (name key)) (into-array String [(str val)])))
            :else (throw (Exception. "expand parameter must be true or a map"))))
    (trace "Executing query")
    (if just-return-query?
      (str query)
      (let [^QueryResponse query-results (.query ^SolrClient *connection*
                                                 ^SolrQuery query
                                                 ^SolrRequest$METHOD method
                                                 )
            ^SolrDocumentList results (.getResults query-results)
            expanded-results (when expand (.getExpandedResults query-results))]
        (trace "Query complete")
        (trace query-results)
        (when (:debugQuery flags)
          (trace (.getDebugMap query-results)))
        (with-meta
          (if expand
            (into {}
                  (for [[key docs] expanded-results]
                    [key (map doc-to-hash docs)]))
            (map doc-to-hash results))
          (merge
            (when cursor-mark
              (let [next  (.getNextCursorMark query-results)]
                {:next-cursor-mark next
                 :cursor-done (.equals next (if (= cursor-mark true)
                                              (CursorMarkParams/CURSOR_MARK_START)
                                              cursor-mark))}))
            (when (:debugQuery flags)
              {:debug (.getDebugMap query-results)})
            (when (.getFieldStatsInfo query-results)
              {:statistics
               (into {}
                     (for [[field info] (.getFieldStatsInfo query-results)]
                       [field {:min (.getMin info)
                               :max (.getMax info)
                               :mean (.getMean info)
                               :stddev (.getStddev info)
                               :sum (.getSum info)
                               :count (.getCount info)
                               :missing (.getMissing info)
                               }]))})
            {:start (when results (.getStart results))
             :rows-set (count results)
             :rows-total (when results (.getNumFound results))
             :highlighting (.getHighlighting query-results)
             :facet-fields (extract-facets query-results facet-hier-sep false facet-result-formatters facet-key-fields)
             :facet-range-fields (extract-facet-ranges query-results facet-date-ranges)
             :limiting-facet-fields (extract-facets query-results facet-hier-sep true facet-result-formatters facet-key-fields)
             :facet-queries (extract-facet-queries facet-queries (.getFacetQuery query-results))
             :facet-pivot-fields (extract-pivots query-results facet-date-ranges)
             :results-obj results
             :query query
             :query-results-obj query-results}))))))
  
(defn search
  "Query solr through solrj.
   q: Query field
   Optional keys:
     :method                :get or :post (default :get)
     :rows                  Number of rows to return (default is Solr default: 1000)
     :start                 Offset into query result at which to start returning rows (default 0)
     :fields                Fields to return
     :facet-fields          Discrete-valued fields to facet.  Can be a string, keyword, or map containing
                            {:name ... :prefix ...}.
     :facet-queries         Vector of facet queries, each encoded in a string or a map of
                            {:name, :value, :formatter}.  :formatter is optional and defaults to the raw query formatter.
                            The result is in the :facet-queries response.
     :facet-date-ranges     Date fields to facet as a vector or maps.  Each map contains
                             :field   Field name
                             :tag     Optional, for referencing in a pivot facet
                             :start   Earliest date (as java.util.Date)
                             :end     Latest date (as java.util.Date)
                             :gap     Faceting gap, as String, per Solr (+1HOUR, etc)
                             :others  Comma-separated string: before,after,between,none,all.  Optional.
                             :include Comma-separated string: lower,upper,edge,outer,all.  Optional.
                             :hardend Boolean (See Solr doc).  Optional.
                             :missing Boolean--return empty buckets if true.  Optional.
     :facet-numeric-ranges  Numeric fields to facet, as a vector of maps.  Map fields as for
                            date ranges, but start, end and gap must be numbers.
     :facet-mincount        Minimum number of docs in a facet for the bucket to be returned.
     :facet-hier-sep        Useful for path hierarchy token faceting.  A regex, such as \\|.
     :facet-filters         Solr filter expression on facet values.  Passed as a map in the form:
                            {:name 'facet-name' :value 'facet-value' :formatter (fn [name value] ...) }
                            where :formatter is optional and is used to format the query.
     :facet-pivot-fields    Vector of pivots to compute, each a list of facet fields.
                            If a facet is tagged (e.g., {:tag ts} in :facet-date-ranges)  
                            then the string should be {!range=ts}other-facet.  Otherwise,
                            use comma separated lists: this-facet,other-facet.
     :cursor-mark           true -- initial cursor; else a previous cursor value from 
                            (:next-cursor-mark (meta result))
     :just-return-query?    Just return the query that would be run on Solr
  Returns the query results as the value of the call, with faceting results as metadata.
  Use (meta result) to get facet data."
  [q & {:keys [method fields facet-fields facet-date-ranges facet-numeric-ranges facet-queries
               facet-mincount facet-hier-sep facet-filters facet-pivot-fields just-return-query?] :as flags}]
  (search* q flags))

(defn atomically-update!
  "Atomically update a solr document:
    doc: document fetched from solr previously, or the id of such a document (must not be a map)
    unique-key: Name of the attribute that is the document's unique key
    changes: Vector of maps containing :attribute, :func [one of :set, :inc, :add], and :value.
   e.g.
     (atomically-update! doc \"cdid\" [{:attribute :client :func :set :value \"darcy\"}])"
  [doc unique-key-field changes]
  (let [^SolrInputDocument document (SolrInputDocument. (make-array String 0))]
    (.addField document (name unique-key-field) (if (map? doc) (get doc unique-key-field) doc))
    (doseq [{:keys [attribute func value]} changes]
      (.addField document (name attribute) (doto (HashMap. 1) (.put (name func) value))))
    (.add ^SolrClient *connection* document)))
  
(defn similar [doc similar-count & {:keys [method]}]
  (let [^SolrQuery query (SolrQuery. (format "id:%d" (:id doc)))
        method (parse-method method)]
    (.setParam query "mlt" (make-param true))
    (.setParam query "mlt.fl" (make-param "fulltext"))
    (.setParam query "mlt.count" (make-param similar-count))
    (let [query-results (.query ^SolrClient *connection* query method)]
      (map doc-to-hash (.get (.get (.getResponse query-results) "moreLikeThis") (str (:id doc)))))))

(defn more-like-this
  "Execute a Solr moreLikeThis (mlt) query.  
   id: unique id of doc to match.
   unique-key: Name of key in schema that corresponds to id.
   similarity-fields: Fields to match against.  Pass as comma-separated list or vector.
   params: Map of optional parameters:
     match-include? -- this is not clearly documented.  See Solr manual.
     min-doc-freq -- ignore words that don't occur in at least this many docs.  Default 3.
     min-term-freq -- ignore terms that occur fewer times than this in a document. Default 2.
     min-word-len -- minimum word length for matching.  Default 5.
     boost? -- Specifies if query will be boosted by interesting term relevance.  Default true.
     max-query-terms -- Maximum number of query terms in a search.  Default 1000.
     max-results -- Maximum number of similar docs returned.  Default 5.
     fields -- fields of docs to return.  Pass as vector or comma-separated list..  Default: unique key + score.  
     method -- Solr Query method.
   Other key/value pairs can be passed in params and are passed onto search*, so can be used
   for filtering."
  [id unique-key similarity-fields
   {:keys [match-include?
           min-doc-freq
           min-term-freq
           min-word-len
           boost?
           max-query-terms
           max-results
           fields
           method]
    :or {match-include? false
         min-doc-freq (int 3)
         min-term-freq (int 2)
         min-word-len (int 5)
         boost? true
         max-results (int 5)
         fields "score"
         max-query-terms (int 1000)}
    :as params}]
  (let [query (doto (SolrQuery.)
                (.setRequestHandler  (str "/" MoreLikeThisParams/MLT))
                (.set MoreLikeThisParams/MATCH_INCLUDE (make-param match-include?))
                (.set MoreLikeThisParams/MIN_DOC_FREQ (make-param min-doc-freq))
                (.set MoreLikeThisParams/MIN_TERM_FREQ (make-param min-term-freq))
                (.set MoreLikeThisParams/MIN_WORD_LEN (make-param min-word-len))
                (.set MoreLikeThisParams/BOOST (make-param boost?))
                (.set MoreLikeThisParams/MAX_QUERY_TERMS (make-param max-query-terms))
                (.set MoreLikeThisParams/SIMILARITY_FIELDS (make-param (if (string? similarity-fields)
                                                                         similarity-fields
                                                                         (str/join "," similarity-fields))))
                (.setQuery (format "%s:\"%s\"" (name unique-key) (str id)))
                (.set "fl" (make-param (format "%s,%s" (name unique-key)
                                               (if (string? fields)
                                                 fields
                                                 (str/join "," fields)))))
                (.setRows max-results))
        query-results (search* query
                               (dissoc params
                                       :match-include?
                                       :min-doc-freq
                                       :min-term-freq
                                       :min-word-len
                                       :boost?
                                       :fields
                                       :max-query-terms
                                       :max-results
                                       :method))]
    (map doc-to-hash (:results-obj (meta query-results)))))

(defn delete-id! [id]
  (.deleteById ^SolrClient *connection* id))

(defn delete-query! [q]
  (.deleteByQuery ^SolrClient *connection* q))

(defn data-import [type]
  (let [type (cond (= type :full) "full-import"
                   (= type :delta) "delta-import")
        params (doto (ModifiableSolrParams.)
                 (.set "qt" (make-param "/dataimport"))
                 (.set "command" (make-param type)))]
    (.query ^SolrClient *connection* params)))

(defmacro with-connection [conn & body]
  `(binding [*connection* ~conn]
     (try
       (do ~@body)
       (finally (.close ^SolrClient *connection*)))))

