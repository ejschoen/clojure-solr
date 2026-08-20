(ns clojure-solr.impl.solr10
  "SolrJ 10: clients built on java.net.http.

   Two things differ from the Solr 9 implementation in kind, not just in naming.

   Credentials are applied per request rather than by a transport-level
   interceptor, because SolrJ 10 offers no interceptor at all -- the builder's
   entire auth surface is withBasicAuthCredentials and withOptionalBasicAuth
   Credentials, which bake a static value in at construction.  Applying them per
   request is also what a rotating bearer token requires, so the request path is
   the only correct one.  SolrRequest headers are honoured by both the JDK and
   Jetty clients, so this is portable rather than a JDK-client workaround.

   Hostname verification cannot be relaxed on the JDK client.  The builder has no
   sslParameters, no way to inject a java.net.http.HttpClient, and supplying one
   reflectively does not help either -- the JDK enforces endpoint identification
   for HTTPS regardless.  :self-signed therefore encrypts and trusts, but still
   requires the certificate's SAN to match the host being connected to."
  (:require [clojure-solr.impl :as impl])
  (:import (java.security KeyStore SecureRandom)
           (java.security.cert X509Certificate)
           (java.util.concurrent TimeUnit)
           (javax.net.ssl SSLContext TrustManager TrustManagerFactory X509TrustManager)
           (org.apache.solr.client.solrj SolrClient SolrRequest)
           (org.apache.solr.client.solrj.impl HttpJdkSolrClient HttpJdkSolrClient$Builder
                                              HttpSolrClientBase)
           (org.apache.solr.client.solrj.request SolrQuery)
           (org.apache.solr.client.solrj.response InputStreamResponseParser)))

(extend-protocol impl/SolrConnection
  HttpSolrClientBase
  (drain [_] nil)
  (shared? [_] false)
  (base-url [c] (.getBaseURL c))
  (unwrap [c] c))

;;; ---------------------------------------------------------------------------
;;; Per-request credentials
;;; ---------------------------------------------------------------------------

(defn decorate-request!
  "Apply credential to a single outgoing request.  Basic auth and a bearer token
   both write the Authorization header and SolrJ does not overwrite one with the
   other -- it sends both, which is malformed -- so exactly one path may fire."
  [^SolrRequest request credential]
  (when credential
    (case (:type credential)
      :basic (.setBasicAuthCredentials request (:name credential) (:password credential))
      :token (when-let [t ((:token-fn credential))]
               (.addHeader request "Authorization" (str "Bearer " t)))
      (throw (ex-info "Unknown credential type" {:credential (dissoc credential :password)}))))
  request)

;;; ---------------------------------------------------------------------------
;;; TLS
;;; ---------------------------------------------------------------------------

(defn- trust-all-manager []
  (reify X509TrustManager
    (getAcceptedIssuers [_] (make-array X509Certificate 0))
    (checkClientTrusted [_ _ _])
    (checkServerTrusted [_ _ _])))

(defn- trust-store-managers [trust-store ^String trust-password]
  (let [ks (KeyStore/getInstance (KeyStore/getDefaultType))]
    (with-open [in (clojure.java.io/input-stream trust-store)]
      (.load ks in (when trust-password (.toCharArray trust-password))))
    (let [tmf (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))]
      (.init tmf ks)
      (.getTrustManagers tmf))))

(defn build-ssl-context
  "SSLContext for :ssl-trust-store, which is either a trust store or
   :self-signed.  Note this governs trust only; see the namespace docstring on
   hostname verification."
  ^SSLContext
  [{:keys [ssl-trust-store ssl-trust-password]}]
  (when ssl-trust-store
    (let [managers (if (= :self-signed ssl-trust-store)
                     (into-array TrustManager [(trust-all-manager)])
                     (trust-store-managers ssl-trust-store ssl-trust-password))]
      (doto (SSLContext/getInstance "TLS")
        (.init nil managers (SecureRandom.))))))

;;; ---------------------------------------------------------------------------
;;; Client construction
;;; ---------------------------------------------------------------------------

(defonce ^:private pool-warning-shown (atom false))

(defn- pool-options
  "Translate clojure-solr's pool options onto the JDK client, which pools and
   reclaims idle connections internally.  Only a per-host cap has an equivalent;
   the rest are reported once and dropped."
  [conn-manager]
  (when (map? conn-manager)
    (let [dropped (select-keys conn-manager [:max-connections-total :time-to-live-seconds])]
      (when (and (seq dropped) (compare-and-set! pool-warning-shown false true))
        (println "WARNING: clojure-solr:" (pr-str (keys dropped))
                 "have no equivalent on SolrJ 10 and are ignored."
                 "The JDK client pools connections and reclaims idle ones on its own."))
      (select-keys conn-manager [:max-connections-per-host]))))

(defn- jdk-client
  [url {:keys [ssl-trust-store socket-timeout connection-timeout
               max-connections-per-host default-collection] :as opts}]
  (let [b (HttpJdkSolrClient$Builder. url)]
    (when default-collection (.withDefaultCollection b default-collection))
    (when ssl-trust-store
      (when-let [ctx (build-ssl-context opts)] (.withSSLContext b ctx)))
    (when connection-timeout (.withConnectionTimeout b (long connection-timeout) TimeUnit/MILLISECONDS))
    ;; The Apache socket timeout is closest to an idle timeout here; SolrJ 10's
    ;; request timeout covers the whole exchange, which is not the same thing.
    (when socket-timeout (.withIdleTimeout b (long socket-timeout) TimeUnit/MILLISECONDS))
    (when max-connections-per-host (.withMaxConnectionsPerHost b (int max-connections-per-host)))
    (.build b)))

(defrecord Solr10Impl []
  impl/SolrImpl
  (make-client [_ url opts]
    (case (:type opts :http)
      :http (jdk-client url (merge opts (pool-options (:conn-manager opts))))
      :concurrent-update
      (impl/unsupported! :concurrent-update
                         (str "SolrJ 10's batching client is ConcurrentUpdateJettySolrClient, "
                              "which lives in the solr-solrj-jetty artifact and pulls in Jetty 12. "
                              "Add that dependency, or batch updates in the caller."))
      (throw (ex-info "Unknown Solr client type" {:type (:type opts)}))))
  (new-query [_ q] (if q (SolrQuery. ^String q) (SolrQuery.)))
  (query? [_ x] (instance? SolrQuery x))
  (stream-response-parser [_ writer-type] (InputStreamResponseParser. writer-type))
  (capabilities [_]
    ;; No :kerberos -- hadoop-auth and Krb5HttpClientBuilder are gone.
    ;; No :connection-manager -- the JDK client pools internally.
    ;; No :relaxed-hostname-verification -- see the namespace docstring.
    #{}))

(def impl (->Solr10Impl))
