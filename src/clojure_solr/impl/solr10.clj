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
           (org.apache.solr.common.util ExecutorUtil)
           (org.apache.solr.client.solrj.request SolrQuery)
           (org.apache.solr.client.solrj.response InputStreamResponseParser)))

(extend-protocol impl/SolrConnection
  HttpSolrClientBase
  (drain [_] nil)
  (shared? [c] (impl/cache-owned? c))
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

(def default-socket-timeout
  "Milliseconds a single request may take before the JDK client abandons it,
   when a connection does not ask for something else with :socket-timeout.

   Applied always, rather than only when a caller supplies a value.  SolrJ does
   have a default of its own -- HttpSolrClientBuilderBase.getIdleTimeoutMillis
   answers 600000 when unset, and getRequestTimeoutMillis falls back to it, so
   \"no timeout configured\" has never meant \"wait forever\" here -- but ten
   minutes of an unresponsive Solr is indistinguishable from a wedge to anything
   watching, and eight workers reaching that limit in turn is a very long
   outage.  Two minutes is long enough for a large commit and short enough that
   a stall surfaces as a failure someone can act on.

   Override per connection with :socket-timeout.  There is no way to ask for no
   timeout at all: SolrJ treats a non-positive value as unset and substitutes
   its own 600000."
  120000)

(def default-connection-timeout
  "Milliseconds the JDK client will spend establishing a TCP connection, when a
   connection does not ask for something else with :connection-timeout.
   SolrJ's own default is 60000, which is far longer than a reachable Solr ever
   needs; a Solr that has not accepted a connection in ten seconds is down.
   Override per connection with :connection-timeout; a non-positive value
   restores SolrJ's 60000."
  10000)

(defonce ^:private request-thread-counter (java.util.concurrent.atomic.AtomicLong. 0))

(defonce ^{:doc
  "The ExecutorService every JDK Solr client runs its exchanges on.

   SolrJ's own default here DEADLOCKS, and it is not a rare race -- four
   concurrent requests through one client is enough, deterministically.
   HttpJdkSolrClient.preparePutOrPost writes the request body into a
   PipedOutputStream on this executor and hands the paired PipedInputStream to
   the JDK client as BodyPublishers.ofInputStream; the same executor is then
   given to HttpClient.Builder.executor, so the reads that DRAIN that pipe are
   submitted to it too.  SolrJ builds it as

     MDCAwareThreadPoolExecutor(4, 256, 60s, LinkedBlockingQueue(1024))

   and a ThreadPoolExecutor only grows past its core size once the queue is
   FULL -- so with 1024 slots the pool is a fixed 4 and maximumPoolSize 256 is
   unreachable.  Four bodies larger than the pipe's 1KB buffer therefore occupy
   all four threads in PipedOutputStream.write, the drains queue behind them
   forever, and every caller parks in CompletableFuture.get.  No timeout fires:
   the block is in Object.wait inside PipedInputStream, before the exchange
   starts, so neither the connection nor the idle timeout is in scope.
   Upstream: SOLR-17707, and
   https://lists.apache.org/thread/m5yz199r8bv7qo7251cc42wr7rz16q6b, whose
   reporter found the same threshold and the same remedy -- supply an executor.

   Elastic and process-wide, which fixes the deadlock at its cause rather than
   raising the threshold: newMDCAwareCachedThreadPool is a SynchronousQueue pool
   with corePoolSize 0 and an unbounded maximum, so a task NEVER queues behind a
   running one -- it takes an idle thread or gets a new one.  A stuck exchange
   can then cost a thread (SOLR-17707 leaks one per send-side exception) without
   costing throughput, and no client can starve another.  Idle threads reap
   after 60s, so sharing costs nothing when quiet.

   Sharing one pool across clients is also why this is never shut down, and that
   is the second bug it closes.  HttpJdkSolrClient.close only shuts down an
   executor it created ITSELF (shutdownExecutor is false whenever one is
   supplied), so closing a client no longer tears down the executor another
   thread's CompletableFuture is waiting to be completed through -- the hazard
   described above the connection cache in clojure-solr.impl.  Threads are
   daemons, so an unterminated pool cannot hold the JVM open."}
  request-executor
  (ExecutorUtil/newMDCAwareCachedThreadPool
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r]
       (doto (Thread. r (str "clojure-solr-http-" (.incrementAndGet request-thread-counter)))
         (.setDaemon true))))))

(defn- jdk-client
  [url {:keys [ssl-trust-store socket-timeout connection-timeout
               max-connections-per-host default-collection http1?] :as opts}]
  (let [b (HttpJdkSolrClient$Builder. url)
        socket-timeout (or socket-timeout default-socket-timeout)
        connection-timeout (or connection-timeout default-connection-timeout)]
    ;; Before anything else: SolrJ's default executor deadlocks at four
    ;; concurrent requests.  See request-executor.
    (.withExecutor b request-executor)
    (when default-collection (.withDefaultCollection b default-collection))
    (when ssl-trust-store
      (when-let [ctx (build-ssl-context opts)] (.withSSLContext b ctx)))
    (.withConnectionTimeout b (long connection-timeout) TimeUnit/MILLISECONDS)
    ;; The Apache socket timeout is closest to an idle timeout here; SolrJ 10's
    ;; request timeout covers the whole exchange, which is not the same thing.
    ;; The two meet anyway: with no explicit request timeout, SolrJ uses the idle
    ;; timeout as the per-request timeout it hands to HttpRequest.Builder.
    (.withIdleTimeout b (long socket-timeout) TimeUnit/MILLISECONDS)
    (when max-connections-per-host (.withMaxConnectionsPerHost b (int max-connections-per-host)))
    ;; Per connection, rather than SolrJ's solr.http1 system property.  The
    ;; property is read in the builder's constructor, so it only affects clients
    ;; built after it is set -- and since connect caches, a client built before
    ;; that keeps speaking HTTP/2 for the life of the process.  This option is
    ;; part of the cache key, so it cannot be applied too late to matter.
    (when http1? (.useHttp1_1 b true))
    (.build b)))

(defn- authenticating-client
  "Wrap a client so each outgoing request carries the connection's credential.

   SolrClient declares exactly one abstract method, request(SolrRequest, String),
   and every convenience method funnels through it -- so this single override
   also covers .query, .add, .commit, .deleteById and callers doing their own
   interop on *connection*.

   Credentials resolve per connection but are evaluated per request, which is
   what a rotating bearer token needs and what a batching client can safely
   honour.  SolrConnection is delegated explicitly; a proxy would otherwise
   inherit the SolrClient defaults and silently report the wrong thing."
  ^SolrClient [^SolrClient delegate credential]
  (proxy [SolrClient clojure_solr.impl.SolrConnection] []
    (request [req collection]
      (decorate-request! req credential)
      (.request delegate ^SolrRequest req ^String collection))
    (close [] (.close delegate))
    (drain [] (impl/drain delegate))
    ;; The wrapper, not the delegate, is what connect caches and hands back, so
    ;; ownership has to be asked of this object.  The delegate still answers for
    ;; everything else it might be -- an embedded server registered with
    ;; mark-shared!, say.
    (shared_QMARK_ [] (or (impl/cache-owned? this) (impl/shared? delegate)))
    (base_url [] (impl/base-url delegate))
    (unwrap [] delegate)))

(defrecord Solr10Impl []
  impl/SolrImpl
  (make-client [_ url opts]
    (case (:type opts :http)
      :http (let [c (jdk-client url (merge opts (pool-options (:conn-manager opts))))]
              ;; Only wrap when there is something to apply: an unwrapped client
              ;; keeps interop and protocol dispatch exactly as SolrJ defines it.
              (if-let [cred (:credential opts)] (authenticating-client c cred) c))
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
