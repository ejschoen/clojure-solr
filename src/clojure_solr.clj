(ns clojure-solr
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint])
  (:require [clj-time.core :as t])
  (:require [clj-time.format :as tformat])
  (:require [clj-time.coerce :as tcoerce])
  (:import (java.net URI)
           (java.util Base64 HashMap List ArrayList Map)
           (java.nio.charset Charset)
           (org.apache.http HttpRequest HttpRequestInterceptor HttpHeaders)
           (org.apache.http.auth AuthScope UsernamePasswordCredentials)
           (org.apache.http.client.protocol HttpClientContext)
           (org.apache.http.config SocketConfig SocketConfig$Builder)
           (org.apache.http.impl.auth BasicScheme)
           (org.apache.http.impl.client BasicCredentialsProvider HttpClientBuilder CloseableHttpClient)
           (org.apache.http.protocol HttpContext HttpCoreContext)
           (org.apache.solr.client.solrj SolrQuery SolrRequest$METHOD SolrClient)
           (org.apache.solr.client.solrj.impl HttpSolrClient HttpSolrClient$Builder HttpClientUtil
                                              ConcurrentUpdateSolrClient ConcurrentUpdateSolrClient$Builder)
           (java.util.jar Manifest)
           (java.time.temporal ChronoUnit)
           (org.apache.solr.client.solrj.response QueryResponse
                                                  FacetField PivotField
                                                  RangeFacet RangeFacet$Count RangeFacet$Date
                                                  SpellCheckResponse SpellCheckResponse$Suggestion)
           (org.apache.solr.client.solrj.response SuggesterResponse Suggestion)
           (org.apache.solr.common SolrDocument SolrInputDocument SolrDocumentList)
           (org.apache.solr.common.params ModifiableSolrParams CursorMarkParams MoreLikeThisParams)
           (org.apache.solr.common.util NamedList)
           #_(org.apache.solr.util DateMathParser)))

(defonce ^:private url-details (atom {}))
(defonce ^{:private true :tag BasicCredentialsProvider}  credentials-provider (BasicCredentialsProvider.))

(declare ^{:dynamic true :tag SolrClient} *connection*)

(def ^:dynamic *trace-fn* nil)

(declare make-param)

(def default-method (atom :post))

(def http-methods {:get SolrRequest$METHOD/GET, :GET SolrRequest$METHOD/GET
                   :post SolrRequest$METHOD/POST, :POST SolrRequest$METHOD/POST})

(def calendar-units
  {"DATE" (ChronoUnit/DAYS),
   "DAY" (ChronoUnit/DAYS),
   "DAYS" (ChronoUnit/DAYS),
   "HOUR" (ChronoUnit/HOURS),
   "HOURS" (ChronoUnit/HOURS),
   "MILLI" (ChronoUnit/MILLIS),
   "MILLIS" (ChronoUnit/MILLIS),
   "MILLISECOND" (ChronoUnit/MILLIS),
   "MILLISECONDS" (ChronoUnit/MILLIS),
   "MINUTE" (ChronoUnit/MINUTES),
   "MINUTES" (ChronoUnit/MINUTES),
   "MONTH" (ChronoUnit/MONTHS),
   "MONTHS" (ChronoUnit/MONTHS),
   "SECOND" (ChronoUnit/SECONDS),
   "SECONDS" (ChronoUnit/SECONDS),
   "YEAR" (ChronoUnit/YEARS),
   "YEARS" (ChronoUnit/YEARS)})

(def time-floor-fns
  {(ChronoUnit/YEARS) t/year
   (ChronoUnit/MONTHS) t/month
   (ChronoUnit/DAYS) t/day
   (ChronoUnit/HOURS) t/hour
   (ChronoUnit/MINUTES) t/minute
   (ChronoUnit/SECONDS) t/second
   (ChronoUnit/MILLIS) t/milli})

(def time-offset-fns
  {(ChronoUnit/YEARS) t/years
   (ChronoUnit/MONTHS) t/months
   (ChronoUnit/DAYS) t/days
   (ChronoUnit/HOURS) t/hours
   (ChronoUnit/MINUTES) t/minutes
   (ChronoUnit/SECONDS) t/seconds
   (ChronoUnit/MILLIS) t/millis}) 


(def date-math-regex
  (let [unit-alternation (str/join "|" (reverse (sort-by count (keys calendar-units)) ))]
    (re-pattern (format "((/(?:%s))|([+\\-]\\d+(?:%s)))" unit-alternation unit-alternation))))
                
(defn cheap-date-math-parser
  [now expr]
  (let [matcher (re-matcher date-math-regex expr)
        operands (loop [operands []]
                   (if (re-find matcher)
                     (recur (conj operands (second (re-groups matcher))))
                     operands))
        reconstructed-expr (str/join operands)]
    (if (= reconstructed-expr expr)
      (reduce (fn [time operand]
                (if (.startsWith operand "/")
                  (let [unit (subs operand 1)
                        chrono-unit (get calendar-units unit)
                        floor-fn (get time-floor-fns chrono-unit)]
                    (t/floor time floor-fn))
                  (let [[_ factor-string unit] (re-matches #"((?:\+|-)\d+)([A-Z]+)" operand)
                        factor (Integer/parseInt factor-string)
                        offset-unit (get calendar-units unit)
                        offset-fn (get time-offset-fns offset-unit)]
                    (t/plus time (offset-fn factor)))))
              now
              operands)
      (throw (ex-info (format "Invalid DateMath expression: %s" expr)
                      {:given expr :parsed operands})))))  

(defn get-solr-version
  []
  (let [name (format "%s.class" (.getSimpleName SolrQuery))
        resource (str (.getResource SolrQuery name))]
    (if (re-matches #"jar:.+" resource)
      (let [manifest-name (str (subs resource 0 (inc (.lastIndexOf resource "!"))) "/META-INF/MANIFEST.MF")]
        (with-open [s (.openStream (java.net.URL. manifest-name))]
          (let [manifest (Manifest. s)
                attrs (.getMainAttributes manifest)]
            (.getValue attrs "Specification-Version")))))))

(def ^:dynamic *solr-version* (get-solr-version))

(defn set-solr-version!
  [version-string]
  (alter-var-root #'*solr-version* (constantly version-string)))

(defn get-solr-major-version
  []
  (Integer/parseInt (second (re-matches #"(\d+)\.(\d+)\.(\d+)" *solr-version*))))

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
    (cond details
          details
          url
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
            details)
          :else nil)))

(defmulti make-solr-client (fn [url http-client solr-major-version solr-client-options] (:type solr-client-options)))

(defmethod make-solr-client :default [url http-client solr-major-version solr-client-options]
  (.build
   (doto (HttpSolrClient$Builder. url)
     (.withHttpClient http-client)
     (cond-> (#{true false} (:allow-compression solr-client-options))
       (.allowCompression (:allow-compression solr-client-options)))
     (cond-> (not-empty (:kerberos-delegation-token solr-client-options))
       (.withKerberosDelegationToken (:kerberos-delegation-token solr-client-options)))
     (cond-> (and (:socket-timeout solr-client-options)
                  (>= solr-major-version 7))
       (.withSocketTimeout (:socket-timeout solr-client-options)))
     (cond-> (and (:connection-timeout solr-client-options)
                  (>= solr-major-version 7))
       (.withConnectionTimeout (:connection-timeout solr-client-options))))))

(defmethod  make-solr-client ConcurrentUpdateSolrClient [url http-client solr-major-version solr-client-options]
  (.build
   (doto (ConcurrentUpdateSolrClient$Builder. url)
     (.withHttpClient http-client)
     (cond-> (and (:queue-size solr-client-options)
                  (>= solr-major-version 7))
       (.withQueueSize (:queue-size solr-client-options)))
     (cond-> (and (:thread-count solr-client-options)
                  (>= solr-major-version 7))
       (.withThreadCount (:thread-count solr-client-options)))
     (cond-> (and (:socket-timeout solr-client-options)
                  (>= solr-major-version 7))
       (.withSocketTimeout (:socket-timeout solr-client-options)))
     (cond-> (and (:connection-timeout solr-client-options)
                  (>= solr-major-version 7))
       (.withConnectionTimeout (:connection-timeout solr-client-options))))))

(defn ^HttpSolrClient connect [url & [conn-manager solr-client-options]]
  "Create an HttpSolrClient connection to a Solr URI. Optionally use
   a provided connection-manager, such as PoolingHttpClientConnectionManager,
   for situations where Solr's default connection manager cannot keep up
   with demand.
   solr-client-options is a map with:
     :type                      type of Solr client to create: HttpSolrClient (default), 
                                ConcurrentUpdateSolrClient, EmbeddedSolrServer (ie, for testing)
   for HttpSolrClient:
     :allow-compression         true/false
     :kerberos-delegation-token string
     :socket-timeout            int in milliseconds
     :connection-timeout        int in milliseconds
   for ConcurrentUpdateSolrClient:
     :queue-size                integer buffer capacity
     :thread-count              integer background processing thread count
     :socket-timeout            int in milliseconds
     :connection-timeout        int in milliseconds
   for EmbeddedSolrServer:
     :home-dir                  path to directory containing core files
     :core                      name of core to create"
  (let [solr-major-version (get-solr-major-version)
        {:keys [clean-url name password] :as details} (get-url-details url)
        ^HttpClientBuilder builder (when details
                                     (doto ^HttpClientBuilder (HttpClientBuilder/create)
                                       (.setDefaultCredentialsProvider credentials-provider)
                                       (cond-> conn-manager (.setConnectionManager conn-manager))
                                       (cond-> (and (:socket-timeout solr-client-options)
                                                    (< solr-major-version 7))
                                         (.setDefaultSocketConfig (let [scbuilder
                                                                        (doto ^SocketConfig$Builder (SocketConfig/custom)
                                                                          (.setSoTimeout
                                                                           (int (:socket-timeout
                                                                                 solr-client-options))))]
                                                                    (.build scbuilder))))
                                       
                                       (.addInterceptorFirst 
                                        (reify 
                                          HttpRequestInterceptor
                                          (^void process [this ^HttpRequest request ^HttpContext context]
                                           (let [auth-state (.getAttribute context HttpClientContext/TARGET_AUTH_STATE)]
                                             (when (nil? (.getAuthScheme auth-state))
                                               (let [target-host (.getAttribute context HttpCoreContext/HTTP_TARGET_HOST)
                                                     auth-scope (make-auth-scope (.getHostName target-host) (.getPort target-host))
                                                     creds (.getCredentials credentials-provider auth-scope)]
                                                 (when creds
                                                   (.update auth-state (BasicScheme.) creds))))))))))
        ^CloseableHttpClient client (when builder (.build builder))]
    (make-solr-client clean-url client solr-major-version solr-client-options)))

(defn- make-document [boost-map doc]
  (let [^SolrInputDocument sdoc (SolrInputDocument. (make-array String 0))]
    (doseq [[key value] doc]
      (let [key-string (name key)
            boost (get boost-map key)
            is-nested? (and (or (list? value) (vector? value))
                            (every? map? value))
            field-value (if is-nested?
                          (into []
                                (doall (map (partial make-document boost-map) value)))
                          value)]
        (cond (and boost (not is-nested?))
              (.addField sdoc key-string field-value boost)
              is-nested?
              (.addField sdoc key-string field-value)
              ;;(do (println "**** adding nested" key-string (count field-value)) (.addChildDocuments sdoc field-value))
              :else (.addField sdoc key-string field-value))))
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
              (if (instance? java.util.List v)
                (if (every? #(instance? SolrDocument %) v)
                  (into [] (map doc-to-hash v))
                  (into [] v))
                v)])))

(defn- make-param [p]
  (cond
   (string? p) (into-array String [p])
   (coll? p) (into-array String (map str p))
   :else (into-array String [(str p)])))

(defn- ^SolrRequest$METHOD parse-method [method]
  (get http-methods method (get http-methods @default-method)))

(defn extract-facets
  [^QueryResponse query-results facet-hier-sep limiting? formatters key-fields]
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
                                                   (tcoerce/to-date
                                                    (cheap-date-math-parser (tcoerce/from-date start-val) gap))
                                                   #_(.parseMath (doto (DateMathParser.)
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

(def facet-exclude-parameters
  #{:name :result-formatter})

(def facet-parameter-formatters
  {:ex #(format "ex=%s" %2)
   :default #(format "facet.%s=\"%s\"" (name %1) %2)})

(defn do-query
  "Reach out to Solr on *connection* and execute a query.  
     :just-return-query? Returns the query string if true, without querying.
     :method             HTTP method to talk with Solr: POST, GET.
     :request-handler    Endpoint on Solr, if not /select.
  Values remaining in flags are supplied as params.
  Returns an empty document list and basic metadata in meta of the document list."
  [^SolrQuery query
   {:keys [method request-handler just-return-query?] :as flags}]
  (when (not-empty request-handler)
    (.setRequestHandler query request-handler))
  (doseq [[key value] (dissoc flags :just-return-query? :facet-pivot-fields :facet-date-ranges :request-handler :original-flags)]
    (.setParam query (name key) (make-param value)))
  #_(when (and (not (:qf flags))
               (not (:df flags))
               (>= (get-solr-major-version) 7))
      (throw (Exception. "Missing qf or df in search params")))
  (if just-return-query?
    (str query)
    (let [method (parse-method method)]
      (let [^QueryResponse query-results (.query ^SolrClient *connection*
                                                 ^SolrQuery query
                                                 ^SolrRequest$METHOD method
                                                 )]
        (with-meta (list) {:query query :query-results-obj query-results})))))
                              
(defn wrap-show-query
  [handler & {:keys [trace-fn] :or {trace-fn *trace-fn*}}]
  (fn [q flags]
    (when trace-fn
      (binding [*trace-fn* trace-fn]
        (let [flags (:original-flags flags)]
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
          (let [other (dissoc flags :facet-filters :facet-queries :facet-fields)]
            (when (not-empty other)
              (trace "  Other parameters to Solr:")
              (doseq [[k v] other]
                (trace (format "  %s: %s" k (pr-str v)))))))))
    (handler q flags)))

(defn wrap-debug
  "Insert query debugging information if :debugQuery is truthy in flags."
  [handler]
  (fn [^SolrQuery query {:keys [debugQuery] :as flags}]
    (when debugQuery
      (.setParam query "debugQuery" (make-param true)))
    (let [results (handler query (dissoc flags :debugQuery))
          query-results-obj (:query-results-obj (meta results))]
      (if (and debugQuery query-results-obj)
        (vary-meta results assoc :debug (.getDebugMap query-results-obj))
        results))))

(defn wrap-core-search
  "Perform core Solr document search.
     :rows                  Number of rows to return (default is Solr default: 1000)
     :start                 Offset into query result at which to start returning rows (default 0)
     :fields                Fields to return
     :facet-filters         Solr filter expression on facet values.  Passed as a map in the form:
                            {:name 'facet-name' :value 'facet-value' :formatter (fn [name value] ...) }
                            where :formatter is optional and is used to format the query.
  Returns a sequence of matching documents"
  [handler]
  (fn [^SolrQuery query {:keys [fields facet-filters] :as flags}]
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
    (.addFilterQuery query (into-array String (filter not-empty (map format-facet-query facet-filters))))
    (let [search-result (handler query (dissoc flags :facet-filters))
          ^QueryResponse query-results (:query-results-obj (meta search-result))
          ^SolrDocumentList results (and query-results (.getResults query-results))]
      (if (and query-results results)
        (with-meta (map doc-to-hash results)
          (assoc (meta search-result)
                 :rows-total (.getNumFound results)
                 :start (.getStart results)
                 :rows-set (count results)
                 :results-obj results))
        search-result))))

(defn wrap-highlighting
  "Return highlighting information in the :highlighting entry of result metadata.
   Requires :hl entries in flags."
  [handler]
  (fn [^SolrQuery query flags]
    (let [result (handler query flags)
          query-results-obj (:query-results-obj (meta result))]
      (if query-results-obj
        (vary-meta result assoc :highlighting (.getHighlighting query-results-obj))
        result))))

(defn wrap-pivoting
  "Handle pivot results from facet-pivot-fields and facet-date-ranges.
     :facet-pivot-fields    Vector of pivots to compute, each a list of facet fields.
                            If a facet is tagged (e.g., {:tag ts} in :facet-date-ranges)  
                            then the string should be {!range=ts}other-facet.  Otherwise,
                            use comma separated lists: this-facet,other-facet.
   Pivots are returned in the :facet-pivot-fields metadata of the result."
  [handler]
  (fn [^SolrQuery query
       {:keys [facet-pivot-fields facet-date-ranges] :as flags}]
    (doseq [field facet-pivot-fields]
      (.addFacetPivotField query (into-array String [field])))
    (let [result (handler query flags)
          query-results-obj (:query-results-obj (meta result))]
      (if query-results-obj
        (vary-meta result assoc :facet-pivot-fields (extract-pivots query-results-obj facet-date-ranges))
        result))))

(defn wrap-field-statistics
  "Insert field statistics into metadata of a query result.  Requires :stats true and :stats.field entries in flags."
  [handler]
  (fn [^SolrQuery query flags]
    (let [results (handler query flags)
          query-results (:query-results-obj (meta results))]
      (if (and query-results (.getFieldStatsInfo query-results))
        (vary-meta results assoc
                   :statistics (into {}
                                     (for [[field info] (.getFieldStatsInfo query-results)]
                                       [field {:min (.getMin info)
                                               :max (.getMax info)
                                               :mean (.getMean info)
                                               :stddev (.getStddev info)
                                               :sum (.getSum info)
                                               :count (.getCount info)
                                               :missing (.getMissing info)
                                               }])))
        results))))

(defn heatmap->geojson
  "Format a heatmap as geoJSON.  The count value is returned as the count attribute in each point property map."
  [^NamedList heatmap & {:keys [geometry-type] :or {geometry-type :point}}]
  (let [max-long (.get heatmap "maxX")
        max-lat (.get heatmap "maxY")
        min-long (.get heatmap "minX")
        min-lat (.get heatmap "minY")
        num-lats (.get heatmap "rows")
        num-longs (.get heatmap "columns")
        delta-lat (/ (- max-lat min-lat) num-lats)
        delta-long (/ (- max-long min-long) num-longs)
        counts (.get heatmap "counts_ints2D")]
    (when counts
      {:type "FeatureCollection"
       :crs {:type "name"
             :properties {:name "EPSG:4326"}}
       :features (let [features (for [row (range (dec num-lats))
                                      :let [facet-row (nth counts row)
                                            ;; Solr returns the heatmap "top-down," which appears to
                                            ;; mean from max lat to min lat.
                                            latitude (- max-lat (* delta-lat (+ row 0.5)))]
                                      :when facet-row
                                      column (range (dec num-longs))
                                      :let [count (nth facet-row column)
                                            longitude (+ min-long (* delta-long (+ column 0.5)))]
                                      :when (> count 0)]
                                  {:type "Feature"
                                   :geometry (case geometry-type
                                               :point {:type "Point" :coordinates [longitude latitude]})
                                   :properties {:count count}})
                       max-count (apply max (map #(get-in % [:properties :count]) features))]
                   (for [feature features]
                     (update-in feature [:properties :count] (fn [c] (float (/ c max-count))))))})))

(def ^:private heatmap-params
  [:facet-heatmap :facet :facet-heatmap-format
   :facet-heatmap-geom :facet-heatmap-grid-level
   :facet-heatmap-dist-err :facet-heatmap-dist-err-pct])

(defn wrap-heatmap-faceting
  "Request heatmap faceting, return structure that can be cheshired into geojson.
   Expects the flags map to contain facets per the Solr standard faceting version
   of heatmap faceting (e.g., facet.heatmap, facet.heatmap.geom).
   Returns facet in facet-heatmaps of result meta.
     facet-heatmap               Required name of field to facet or map containing {:name :result-formatter}
     facet-heatmap-geom          Optional bounding geometry: WKT or rectangle [minlat,minlong TO maxlat,maxlong]
     facet-heatmap-grid-level    Optional grid refinement level
     facet-heatmap-dist-err-pct  Fraction of geom size to compute grid level.  Defaults to 0.15.
     facet-heatmap-dist-err      Cell error distance to pick grid level indirectly.  See Solr admin manual."
  [handler]
  (fn [^SolrQuery query {:keys [facet-heatmap facet-heatmap-geom facet-heatmap-grid-level facet-heatmap-dist-err-pct facet-heatmap-dist-err] :as flags}]
    (if facet-heatmap
      (do
        (.setParam query "facet" true)
        (.add query "facet.heatmap" (into-array String [(if (map? facet-heatmap) (:name facet-heatmap) facet-heatmap)]))
        (.add query "facet.heatmap.format" (into-array String ["ints2D"]))
        (when facet-heatmap-geom
          (.add query "facet.heatmap.geom" (into-array String [facet-heatmap-geom])))
        (when facet-heatmap-grid-level
          (.setParam query "facet.heatmap.gridLevel" facet-heatmap-grid-level))
        (when facet-heatmap-dist-err
          (.setParam query "facet.heatmap.distErr" facet-heatmap-dist-err))
        (when facet-heatmap-dist-err-pct
          (.setParam query "facet.heatmap.distErrPct" facet-heatmap-dist-err-pct))
        (let [results (handler query (apply dissoc flags heatmap-params))
              query-results (:query-results-obj (meta results))]
          (if query-results
            (vary-meta results assoc
                       :facet-heatmaps (let [response (.getResponse ^QueryResponse query-results)
                                             facet-counts (first (.getAll response "facet_counts"))
                                             facet-heatmaps (.get facet-counts "facet_heatmaps")]
                                         (into {}
                                               (for [[heatmap-field ^NamedList heatmap] facet-heatmaps]
                                                 [heatmap-field
                                                  (if (map? facet-heatmap)
                                                    ((:result-formatter facet-heatmap identity) heatmap)
                                                    heatmap)]))))
            results)))
      (handler query (apply dissoc flags heatmap-params)))))

(defn wrap-faceting
  "Request faceting, supporting discrete, numeric, and date range faceting.
     :facet-fields          Discrete-valued fields to facet.  Can be a string, keyword, or map containing
                            {:name ... :prefix ...}.
                            Facet fields are returned in :facet-fields of result metadata
     :facet-queries         Vector of facet queries, each encoded in a string or a map of
                            {:name, :value, :formatter}.  :formatter is optional and defaults to the raw query formatter.
                            Facet queries are returned in the :facet-queries metadata of the result.
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
                            Faceted date ranges are returned in the :facet-range-fields metadata of the result.
     :facet-numeric-ranges  Numeric fields to facet, as a vector of maps.  Map fields as for
                            date ranges, but start, end and gap must be numbers.
                            Faceted numerc ranges are returned in the :facet-range-fields metadata of the result.
     :facet-mincount        Minimum number of docs in a facet for the bucket to be returned.
     :facet-hier-sep        Useful for path hierarchy token faceting.  A regex, such as \\|."
  [handler]
  (fn [^SolrQuery query
       {:keys [facet-fields facet-date-ranges facet-numeric-ranges facet-queries facet-mincount facet-hier-sep facet-key-fields] :as flags}]
    (let [facet-result-formatters (into {} (map #(if (map? %)
                                                   [(:name %) (:result-formatter % identity)]
                                                   [% identity])
                                                facet-fields))
          facet-key-fields (into {} (map-indexed (fn [i f]
                                                   [(format "f%d" i) (if (map? f) (:name f) (name f))])
                                                 facet-fields))
          facet-field-keys (into {} (map-indexed (fn [i f]
                                                   [(if (map? f) (:name f) (name f)) (format "f%d" i)])
                                                 facet-fields))]
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
      (.setFacetMinCount query (or facet-mincount 1))
      (let [results (handler query (dissoc flags :facet-fields :facet-numeric-ranges :facet-mincount))
            query-results (:query-results-obj (meta results))]
        (if query-results
          (vary-meta results assoc
                     :facet-fields (extract-facets query-results facet-hier-sep false facet-result-formatters facet-key-fields)
                     :facet-range-fields (extract-facet-ranges query-results facet-date-ranges)
                     :limiting-facet-fields (extract-facets query-results facet-hier-sep true facet-result-formatters facet-key-fields)
                     :facet-queries (extract-facet-queries facet-queries (.getFacetQuery query-results)))
          results)))))

(defn wrap-expand
  "Request collapsed search results be expanded.
   Pass :expand as a field name or a map of expand parameters"
  [handler]
  (fn [^SolrQuery query {:keys [expand] :as flags}]
    (when expand
      (.setParam query "expand" true)
      (cond (string? expand)
            nil
            (map? expand)
            (doseq [[key val] expand]
              (.setParam query (format "expand.%s" (name key)) (into-array String [(str val)])))
            :else (throw (Exception. "expand parameter must be true or a map"))))
    (let [results (handler query (dissoc flags :expand))
          query-results (:query-results-obj (meta results))
          expanded-results (when (and expand query-results) (.getExpanded-results query-results))]
      (if (and expand query-results)
        (with-meta
          (into {}
                (for [[key docs] expanded-results]
                  [key (map doc-to-hash docs)]))
          (meta results))
        results))))

(defn wrap-collapse
  "Request search results be collapsed by a given field, passed in the :collapse flag, 
   which can be a string field name or a map containing Solr collapse parameters."
  [handler]
  (fn [^SolrQuery query {:keys [collapse] :as flags}]
    (when collapse
      (cond (string? collapse)
            (.addFilterQuery query (into-array String [(format "{!collapse field=%s}" collapse)]))
            (map? collapse)
            (.addFilterQuery query (into-array String [(format "{!collapse %s}"
                                                               (str/join " "
                                                                         (for [[key val] collapse]
                                                                           (format "%s=%s" (name key) val))))]))
            :else (throw (Exception. "collapse parameter must be string or map"))))
    (handler query (dissoc flags :expand))))

(defn wrap-cursor-mark
  "Request cursor-mark cursor handling if :cursor-mark is in flags.  
   flags must contain a :sort entry that sorts by a unique field if :cursor-mark is enabled.
   returns :next-cursor-mark and :cursor-done (a boolean) in result metadata."
  [handler]
  (fn [^SolrQuery query {:keys [cursor-mark] :as flags}]
    (let [want-cursoring (or (= cursor-mark true) (not (nil? cursor-mark)))]
      (when (and want-cursoring (not (:sort flags)))
        (throw (IllegalArgumentException. "Requesting Solr cursor-mark requires a :sort flag specifying a unique field")))
      (cond (= cursor-mark true)
            (.setParam query ^String (CursorMarkParams/CURSOR_MARK_PARAM) (into-array String [(CursorMarkParams/CURSOR_MARK_START)]))
            (not (nil? cursor-mark))
            (.setParam query ^String (CursorMarkParams/CURSOR_MARK_PARAM) (into-array String [cursor-mark])))
      (let [results (handler query (dissoc flags :cursor-mark))
            query-results (:query-results-obj (meta results))
            next (when query-results (.getNextCursorMark query-results))]
        (if (and query-results want-cursoring)
          (vary-meta results assoc
                     :next-cursor-mark next
                     :cursor-done (.equals next (if (= cursor-mark true)
                                                  (CursorMarkParams/CURSOR_MARK_START)
                                                  cursor-mark)))
          results)))))

(defn wrap-spellcheck
  "Request spellcheck.  Return corrections as a map containing the Solr :collated-result and :alternatives
   in result metadata. Injects :spell true into parameters (use this middleware sparingly)."
  [handler & [opts]]
  (let [spellcheck-opts (into {:spellcheck true}
                              (for [[k v] opts :when (re-matches #"spellcheck\..+" (name k))] [k v]))]
    (fn [^SolrQuery query flags]
      (let [result (handler query (merge flags spellcheck-opts))
            query-results (:query-results-obj (meta result))]
        (if query-results
          (let [^SpellCheckResponse scr (.getSpellCheckResponse query-results)]
            (if scr
              (vary-meta result assoc
                         :spellcheck 
                         {:collated-result (.getCollatedResult scr)
                          :is-correctly-spelled? (.isCorrectlySpelled scr)
                          :alternatives (into {}
                                              (doall
                                               (for [^SpellCheckResponse$Suggestion suggestion (.getSuggestions scr)
                                                     :when suggestion]
                                                 [(.getToken suggestion)
                                                  {:num-found (.getNumFound suggestion)
                                                   :original-frequency (.getOriginalFrequency suggestion)
                                                   :start-offset (.getStartOffset suggestion)
                                                   :end-offset (.getEndOffset suggestion)
                                                   :alternatives (.getAlternatives suggestion)
                                                   :alternative-frequencies (.getAlternativeFrequencies suggestion)}])))})
              result))
          result)))))

(defn wrap-suggest
  "Request suggestions from search.  Return suggestions as an ordered sequence of maps
   containing :term and :weight, in result metadata.
   Injects :suggest true into parameters (use this middleware sparingly).
   If suggest.q is not provided, injects q as suggest.q with leading and trailing * removed."
  [handler & {:keys [suggester-name all-suggesters]}]
  (fn [^SolrQuery query flags]
    (let [result (handler query (merge flags
                                       {:suggest true}
                                       (if (not-empty suggester-name)
                                         {:suggest.dictionary suggester-name})
                                       (if (not (:suggest.q flags))
                                         {:suggest.q (-> (.getQuery query)
                                                         (str/replace #"^\*" "")
                                                         (str/replace #"\*$" ""))})))
          query-results (:query-results-obj (meta result))
          ^SuggesterResponse suggester-response (when query-results (.getSuggesterResponse query-results))
          ^Map suggestions (.getSuggestions suggester-response)
          effective-suggester-name (cond (not-empty suggester-name)
                                         suggester-name
                                         :else (first (keys suggestions)))]
      (if (and query-results suggester-response)
        (vary-meta result assoc
                   :suggestions
                   (if (or all-suggesters
                           (and suggester-name (not (string? suggester-name))))
                     (into {}
                           (for [suggester-name (keys suggestions)]
                             [suggester-name (reverse
                                              (sort-by :weight
                                                       (map (fn [^Suggestion suggestion]
                                                              {:term (.getTerm suggestion)
                                                               :weight (.getWeight suggestion)})
                                                            (get suggestions suggester-name))))]))
                     (reverse
                      (sort-by :weight
                               (map (fn [^Suggestion suggestion]
                                      {:term (.getTerm suggestion)
                                       :weight (.getWeight suggestion)})
                                    (get suggestions effective-suggester-name))))))
        result))))

(def solr-app
  (-> do-query
      wrap-show-query
      wrap-debug
      wrap-core-search
      wrap-collapse
      wrap-expand
      wrap-cursor-mark
      wrap-highlighting
      wrap-faceting
      wrap-pivoting
      wrap-field-statistics))
      
(defn search*-with-middleware
  "Searches for documents matching q, which can be a string or an instance of SolrQuery.  See search for details about the flags map.
   Middleware can be provided explicitly, be sources from the :middleware entry in flags, or default to clojure-solr/solr-app."
  [q flags & [middleware]]
  (let [^SolrQuery query (cond (string? q) (SolrQuery. q)
                               (instance? SolrQuery q) q
                               :else (throw (Exception. "q parameter must be a string or SolrQuery")))
        middleware (or middleware (:middleware flags) solr-app)]
    (middleware query
                (assoc (dissoc flags :middleware) :original-flags flags))))

(defn search*
  "Searches for documents matching q, which can be a string or an instance of SolrQuery.  See search for details about the flags map"
  [q flags & [middleware]]
  (search*-with-middleware q flags middleware))

(defn lazy-search*
  "Generate a lazy sequence of results using Solr's cursormark feature.  
   flags must include a sort entry whose value is the name of a unique field."
  [q flags & [middleware]]
  (lazy-seq 
   (let [result (search* q (update-in flags [:cursor-mark] (fn [old] (or old true))))]
     (concat result
             (if (:cursor-done (meta result))
               nil
               (lazy-search* q (assoc flags :cursor-mark (:next-cursor-mark (meta result)))))))))

(defn search
  "Query solr through solrj.
   q: Query field
   Optional keys:
     :method                :get or :post (default :get)
     :request-handler       Optional non-default request handler (e.g., /suggest)
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

(defn suggest-response-handler
  [_ ^QueryResponse response _]
  (reverse
   (sort-by :weight
            (map (fn [^Suggestion suggestion]
                   {:term (.getTerm suggestion)
                    :weight (.getWeight suggestion)})
                 (get ^Map (.getSuggestions ^SuggesterResponse (.getSuggesterResponse response))
                      "suggest")))))

(defn spellcheck-response-handler
  [_ ^QueryResponse response _]
  (let [scr (.getSpellCheckResponse response)]
    (if scr
      {:collated-result (.getCollatedResult scr)
       :alternatives (mapcat #(.getAlternatives %) (.getSuggestions scr))}
      nil)))

(defn suggest
  "Return ordered sequence of term/weight suggestions for a string.
   Assumes suggest handler is /suggest and uses suggest-response-handler
   to process result.   Can override via params map, which is merged into map supplied to search*"
  [suggest-q & [params]]
  (search* "*:*"
           (merge {:request-handler "/suggest"
                   :suggest.q suggest-q
                   :response-handler suggest-response-handler}
                  params)))

(defn spellcheck
  "Return ordered sequence of term/weight suggestions for a string.
   Assumes suggest handler is /suggest and uses suggest-response-handler
   to process result.   Can override via params map, which is merged into map supplied to search*"
  [q & [params]]
  (search* q
           (merge {:request-handler "/spell"
                   :response-handler spellcheck-response-handler}
                  params)
           
           ))

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

