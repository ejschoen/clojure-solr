(ns clojure-solr.impl.solr9
  "SolrJ 6-9: clients built on Apache HttpClient.

   Everything in this namespace names a class that SolrJ 10 removed, which is
   exactly why it lives behind clojure-solr.impl rather than in clojure-solr."
  (:require [clojure-solr.impl :as impl])
  (:import (org.apache.http HttpRequest HttpRequestInterceptor)
           (org.apache.http.auth AuthState AuthScope UsernamePasswordCredentials)
           (org.apache.http.client.protocol HttpClientContext)
           (org.apache.http.config SocketConfig SocketConfig$Builder)
           (org.apache.http.impl.auth BasicScheme)
           (org.apache.http.impl.client BasicCredentialsProvider HttpClientBuilder CloseableHttpClient)
           (org.apache.http.protocol HttpContext HttpCoreContext)
           (org.apache.http.ssl SSLContexts SSLContextBuilder)
           (org.apache.http.conn.ssl SSLConnectionSocketFactory NoopHostnameVerifier TrustSelfSignedStrategy)
           (javax.net.ssl SSLContext)
           (org.apache.solr.client.solrj SolrQuery)
           (org.apache.solr.client.solrj.impl HttpSolrClient HttpSolrClient$Builder HttpClientUtil
                                              ConcurrentUpdateSolrClient ConcurrentUpdateSolrClient$Builder)))

;;; ---------------------------------------------------------------------------
;;; Capabilities of the clients this namespace can build
;;; ---------------------------------------------------------------------------

(extend-protocol impl/SolrConnection
  ConcurrentUpdateSolrClient
  (drain [c] (.blockUntilFinished c))
  (shared? [_] false)
  (base-url [c] (.getBaseURL c))
  (unwrap [c] c)

  HttpSolrClient
  (drain [_] nil)
  (shared? [_] false)
  (base-url [c] (.getBaseURL c))
  (unwrap [c] c))

;;; ---------------------------------------------------------------------------
;;; Authentication
;;;
;;; On this SolrJ, credentials are applied by an Apache request interceptor,
;;; which is why they can be changed after a connection is opened.  A credential
;;; reaching here is either a data map from clojure-solr/set-credentials or a
;;; legacy object satisfying clojure-solr/SolrAuthentication -- i2kopenid's JWT
;;; provider is the latter, and must keep working.
;;; ---------------------------------------------------------------------------

(defn- basic-auth-interceptor
  "Preemptive-ish basic auth: attach credentials when no scheme has been chosen."
  [^BasicCredentialsProvider provider]
  (fn [request ^HttpContext context]
    (let [^AuthState auth-state (.getAttribute context HttpClientContext/TARGET_AUTH_STATE)]
      (when (and auth-state (nil? (.getAuthScheme auth-state)))
        (let [target-host (.getAttribute context HttpCoreContext/HTTP_TARGET_HOST)
              scope (AuthScope. (.getHostName target-host) (.getPort target-host))]
          (when-let [creds (.getCredentials provider scope)]
            (.update auth-state (BasicScheme.) creds)))))))

(defn- credential->interceptor-fn
  "A (fn [request context]) that applies credential, or nil."
  [credential]
  (cond
    (nil? credential) nil

    ;; Data from clojure-solr/set-credentials.
    (map? credential)
    (case (:type credential)
      :basic (let [provider (BasicCredentialsProvider.)]
               (.setCredentials provider
                                (AuthScope. (:host credential) (int (or (:port credential) -1)))
                                (UsernamePasswordCredentials. ^String (:name credential)
                                                              ^String (:password credential)))
               (basic-auth-interceptor provider))
      :token (fn [^HttpRequest request _]
               (when-let [t ((:token-fn credential))]
                 (.setHeader request "Authorization" (str "Bearer " t))))
      (throw (ex-info "Unknown credential type" {:credential (dissoc credential :password)})))

    ;; Legacy object implementing clojure-solr/SolrAuthentication.
    :else
    (let [add-authentication (ns-resolve 'clojure-solr 'add-authentication)]
      (fn [request context] (add-authentication credential request context)))))

;;; ---------------------------------------------------------------------------
;;; TLS
;;; ---------------------------------------------------------------------------

(defn build-connection-socket-factory
  "Apache socket factory honouring :ssl-trust-store (a trust store, or
   :self-signed) and :ssl-trust-password."
  ^SSLConnectionSocketFactory
  [{:keys [ssl-trust-store ssl-trust-password]}]
  (when ssl-trust-store
    (let [^SSLContextBuilder builder (SSLContexts/custom)]
      (cond (= :self-signed ssl-trust-store)
            (.loadTrustMaterial builder nil (TrustSelfSignedStrategy.))
            ssl-trust-password
            (.loadTrustMaterial builder ssl-trust-store (.toCharArray ^String ssl-trust-password))
            :else (.loadTrustMaterial builder ssl-trust-store))
      (let [^SSLContext ctx (.build builder)]
        (SSLConnectionSocketFactory. ctx NoopHostnameVerifier/INSTANCE)))))

;;; ---------------------------------------------------------------------------
;;; Client construction
;;; ---------------------------------------------------------------------------

(def ^:private default-connection-manager (atom nil))

(defn set-default-connection-manager! [m] (reset! default-connection-manager m))
(defn get-default-connection-manager [] @default-connection-manager)

(defn- build-http-client
  ^CloseableHttpClient
  [{:keys [conn-manager credential socket-timeout ssl-trust-store] :as opts} major-version]
  (let [^HttpClientBuilder builder (HttpClientBuilder/create)
        mgr (or conn-manager @default-connection-manager)
        interceptor-fn (credential->interceptor-fn credential)]
    (.setDefaultCredentialsProvider builder (BasicCredentialsProvider.))
    (when mgr (.setConnectionManager builder mgr))
    (when (and socket-timeout (< major-version 7))
      (.setDefaultSocketConfig builder
                               (.build (doto ^SocketConfig$Builder (SocketConfig/custom)
                                         (.setSoTimeout (int socket-timeout))))))
    (when interceptor-fn
      (.addInterceptorFirst builder
                            (reify HttpRequestInterceptor
                              (^void process [_ ^HttpRequest request ^HttpContext context]
                                (interceptor-fn request context)))))
    (when ssl-trust-store
      (when-let [f (build-connection-socket-factory opts)]
        (.setSSLSocketFactory builder f)))
    (.build builder)))

(defn- http-client
  [url ^CloseableHttpClient client
   {:keys [allow-compression kerberos-delegation-token socket-timeout connection-timeout]}
   major-version]
  (let [b (HttpSolrClient$Builder. url)]
    (when client (.withHttpClient b client))
    (when (#{true false} allow-compression) (.allowCompression b allow-compression))
    (when (not-empty kerberos-delegation-token) (.withKerberosDelegationToken b kerberos-delegation-token))
    (when (and socket-timeout (>= major-version 7)) (.withSocketTimeout b (int socket-timeout)))
    (when (and connection-timeout (>= major-version 7)) (.withConnectionTimeout b (int connection-timeout)))
    (.build b)))

(defn- concurrent-update-client
  [url ^CloseableHttpClient client
   {:keys [queue-size thread-count socket-timeout connection-timeout]}
   major-version]
  (let [b (ConcurrentUpdateSolrClient$Builder. url)]
    (when client (.withHttpClient b client))
    (when (and queue-size (>= major-version 7)) (.withQueueSize b (int queue-size)))
    (when (and thread-count (>= major-version 7)) (.withThreadCount b (int thread-count)))
    (when (and socket-timeout (>= major-version 7)) (.withSocketTimeout b (int socket-timeout)))
    (when (and connection-timeout (>= major-version 7)) (.withConnectionTimeout b (int connection-timeout)))
    (.build b)))

;;; ---------------------------------------------------------------------------
;;; Kerberos
;;;
;;; Krb5HttpClientBuilder is loaded reflectively: on this SolrJ it links against
;;; org.eclipse.jetty.client.api.Authentication, so naming it here would put
;;; Solr's Jetty on the classpath of every user of the library.
;;; ---------------------------------------------------------------------------

(defn- krb5-classpath-error [cause]
  (ex-info (str "Kerberos support requires SolrJ's Krb5HttpClientBuilder and the Jetty "
                "client jars it references (org.eclipse.jetty/jetty-client, "
                "org.eclipse.jetty.http2/http2-client and their dependencies). "
                "Missing class: " (.getMessage cause) ". "
                "Remove those artifacts from your Solr dependency exclusions to use "
                "set-kerberos-credentials.")
           {:missing-class (.getMessage cause)}
           cause))

(defn install-kerberos-client-builder!
  []
  (try
    (let [klass (Class/forName "org.apache.solr.client.solrj.impl.Krb5HttpClientBuilder")]
      (HttpClientUtil/setHttpClientBuilder
       (-> (.getMethod klass "getBuilder" (make-array Class 0))
           (.invoke (.newInstance (.getDeclaredConstructor klass (make-array Class 0))
                                  (make-array Object 0))
                    (make-array Object 0)))))
    (catch ClassNotFoundException e (throw (krb5-classpath-error e)))
    (catch NoClassDefFoundError e (throw (krb5-classpath-error e)))))

;;; ---------------------------------------------------------------------------

(defrecord Solr9Impl [major-version]
  impl/SolrImpl
  (make-client [_ url opts]
    (let [client (when-not (:kerberos? opts) (build-http-client opts major-version))]
      (case (:type opts :http)
        :http (http-client url client opts major-version)
        :concurrent-update (concurrent-update-client url client opts major-version)
        (throw (ex-info "Unknown Solr client type" {:type (:type opts)})))))
  (new-query [_ q] (if q (SolrQuery. ^String q) (SolrQuery.)))
  (query? [_ x] (instance? SolrQuery x))
  (capabilities [_]
    #{:kerberos :connection-manager :concurrent-update :relaxed-hostname-verification}))

(defn- detect-major-version
  "Read the SolrJ specification version off the jar manifest.  Keyed on
   SolrClient, which unlike SolrQuery has not moved."
  []
  (try
    (let [resource (str (.getResource org.apache.solr.client.solrj.SolrClient "SolrClient.class"))]
      (if (re-matches #"jar:.+" resource)
        (let [manifest (str (subs resource 0 (inc (.lastIndexOf resource "!"))) "/META-INF/MANIFEST.MF")]
          (with-open [s (.openStream (java.net.URL. manifest))]
            (let [v (.getValue (.getMainAttributes (java.util.jar.Manifest. s)) "Specification-Version")]
              (Integer/parseInt (second (re-matches #"(\d+)\..*" v))))))
        9))
    (catch Exception _ 9)))

(def impl (->Solr9Impl (detect-major-version)))
