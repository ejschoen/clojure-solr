(ns clojure-solr.impl
  "The contract between clojure-solr and the SolrJ version it is running against.

   SolrJ 10 removed the Apache HttpClient-based clients (HttpSolrClient,
   ConcurrentUpdateSolrClient, HttpClientUtil, Krb5HttpClientBuilder) and moved
   SolrQuery from org.apache.solr.client.solrj to ...solrj.request.  Everything
   else clojure-solr touches -- SolrClient, SolrDocument, the params classes and
   the response classes -- is identical across 9 and 10.

   So the version-specific surface is small, and it lives here.  clojure-solr
   itself names no class that moved or disappeared, which is what lets one source
   tree build and run against either SolrJ.

   The implementation is chosen at load time by probing for a class that exists
   only in SolrJ 9.  That means one published artifact works against either, and
   an application does not need a different clojure-solr per build profile."
  (:import (org.apache.solr.client.solrj SolrClient)))

;;; ---------------------------------------------------------------------------
;;; Capabilities
;;;
;;; These replace the type tests that used to ask a client what it could do by
;;; asking what it was.  The default implementation is on SolrClient, so anything
;;; unextended answers sensibly and a new client type never forces an edit here.
;;; Extensions belong in the namespace that introduces the type: an impl
;;; namespace for the batching clients, solr_embedded.clj for EmbeddedSolrServer.
;;; ---------------------------------------------------------------------------

(defprotocol SolrConnection
  (drain [conn]
    "Block until buffered updates have been flushed.  No-op unless the client
     buffers writes.")
  (shared? [conn]
    "True if this connection is a process-lifetime resource rather than one this
     scope owns, so nothing scope-shaped may close it.  Two things answer true:
     a client registered with mark-shared! -- EmbeddedSolrServer is the case that
     matters, since each connect would otherwise re-derive a CoreContainer and
     the embedded builds have exactly one -- and any client the connection cache
     is holding, which connect will hand out again.

     This is a statement about lifetime, not about whether a nested scope may
     skip building its own connection; see reuse-bound? for that.")
  (base-url [conn]
    "The collection URL this connection talks to, in the same shape on 9 and 10.
     Use instead of interop on .getBaseURL, which the authenticating wrapper does
     not expose.")
  (unwrap [conn]
    "The underlying SolrClient, unwrapping any decoration clojure-solr added."))

;;; ---------------------------------------------------------------------------
;;; The connection cache
;;;
;;; SolrJ 10 pools connections per client *instance*.  A call site that builds a
;;; client per operation therefore gets no pooling at all -- a new TCP connection
;;; and a new HTTP/2 preface every time -- and on Java 17 cannot even release
;;; what it built, because java.net.http.HttpClient only became AutoCloseable in
;;; Java 21, so each discarded client leaks its selector thread until GC.  The
;;; same call pattern still pooled on Solr 9, where every client borrowed one
;;; shared Apache connection manager; deprecating that manager was right for
;;; reused clients, but per-call sites lost cross-call pooling with it.
;;;
;;; So clojure-solr keeps the clients: connect hands back the same instance for
;;; the same target and options, and registers it here.
;;;
;;; Registration is what stops anything closing a client the cache will hand out
;;; again.  That is not a tidiness rule.  HttpJdkSolrClient.close shuts down the
;;; ExecutorService the JDK HttpClient delivers responses through, so closing a
;;; client another thread is using leaves that thread's CompletableFuture
;;; uncompleted in either direction -- no result, no exception, and no timeout,
;;; since the timeout would have to be delivered through the same executor.  The
;;; thread parks forever.
;;; ---------------------------------------------------------------------------

(defonce ^:private client-cache (atom {}))

(defonce ^:private cache-lock (Object.))

(defonce ^:private cache-owned-clients
  ;; Identity, not equality: two clients are the same client only when they are
  ;; the same object.  Synchronized because worker threads read this on every
  ;; with-connection exit while another may be adding to it.
  (java.util.Collections/synchronizedSet
   (java.util.Collections/newSetFromMap (java.util.IdentityHashMap.))))

(defn cache-owned?
  "True if conn is a client the connection cache is holding.  Such a client is
   not any one scope's to close: connect will hand the same instance out again."
  [conn]
  (and (some? conn)
       (.contains ^java.util.Set cache-owned-clients conn)))

(defn cached-client
  "The client cached under cache-key, built with build-fn on first use.

   Concurrent callers for the same key build once and all receive that one
   client.  Construction failure is not cached: nothing is installed unless
   build-fn returns.

   Building, registering and installing happen together under cache-lock, and
   close-cached-clients! takes the same lock.  That matters for more than
   tidiness.  An earlier version stored a delay and registered the client from
   inside it, which let a concurrent close drop the not-yet-realized entry from
   the map while the delay went on to register the client anyway -- leaving a
   client that reported itself cache-owned, so no scope would close it, and that
   the cache no longer held, so close-cached-clients! would not either.  It
   leaked for the lifetime of the process.  A stress run of connect against
   close-cached-connections! is what surfaced it."
  [cache-key build-fn]
  (or (get @client-cache cache-key)                    ; uncontended fast path
      (locking cache-lock
        (or (get @client-cache cache-key)
            (let [c (build-fn)]
              (.add ^java.util.Set cache-owned-clients c)
              (swap! client-cache assoc cache-key c)
              c)))))

(extend-protocol SolrConnection
  SolrClient
  (drain [_] nil)
  (shared? [c] (cache-owned? c))
  (base-url [_] nil)
  (unwrap [c] c))

(defn reuse-bound?
  "True if a nested scope should reuse the connection already bound instead of
   evaluating its own connection expression at all.

   Narrower than shared?, deliberately.  Blind reuse is right only when building
   the nested connection would be expensive or destructive and there is exactly
   one of the thing: an embedded build has one CoreContainer, so a nested connect
   naming a different core silently gets the bound one, and that is the stated
   rule.  A cached HTTP client is neither -- connect for it is a map lookup, and
   there is one per target -- so reusing it blindly would hand a nested scope a
   connection to the wrong collection.  Cached clients take the ordinary path and
   are simply not closed on the way out."
  [conn]
  (and (shared? conn) (not (cache-owned? conn))))

(defn close-cached-clients!
  "Drain, close and forget every client the connection cache holds; returns how
   many were closed.

   There is deliberately no automatic eviction.  A cached client has no
   permanently-failed state to evict on: the JDK client holds a pool, not a
   connection, and discards a broken connection by itself on the next request.
   Evicting on request failure would reintroduce exactly the per-operation client
   churn this cache exists to remove, and would close clients other threads are
   mid-request on.  So eviction is explicit, and this is it -- for shutdown, or
   for a caller that knows a target's configuration has changed.

   Holds cache-lock, so no client can be built and registered while this runs.
   That still does not make it safe against concurrent *use*: a thread that
   already has one of these clients keeps using it, and on Java 17 closing a
   client out from under a request parks that thread for good.  See the note
   above the cache."
  []
  (let [held (locking cache-lock
               (let [m @client-cache]
                 (reset! client-cache {})
                 (doseq [c (vals m)] (.remove ^java.util.Set cache-owned-clients c))
                 m))]
    (reduce (fn [n c]
              (try
                (drain c)
                (.close ^SolrClient c)
                (inc n)
                (catch Throwable _ n)))
            0
            (vals held))))

(defn cached-client-count
  "How many distinct clients the connection cache is holding."
  []
  (count @client-cache))

(defn cache-owned-count
  "How many clients are registered as cache-owned.  This must equal
   cached-client-count: a client registered but no longer reachable from the
   cache is one that with-connection will refuse to close, because it reports
   itself shared, and that close-cached-clients! cannot reach either -- so
   nothing would ever close it.  Divergence is a leak, and is worth asserting
   after any run that mixes connect with close-cached-clients!."
  []
  (.size ^java.util.Set cache-owned-clients))

;;; ---------------------------------------------------------------------------
;;; The version-specific contract
;;; ---------------------------------------------------------------------------

(defprotocol SolrImpl
  (make-client [this url opts]
    "Build a SolrClient.  opts keys:
       :type                      :http (default) or :concurrent-update
       :ssl-trust-store           File, or :self-signed
       :ssl-trust-password        String
       :allow-compression         boolean
       :socket-timeout            milliseconds
       :connection-timeout        milliseconds
       :queue-size, :thread-count for :concurrent-update
       :conn-manager              legacy connection manager; ignored on Solr 10")
  (new-query [this q]
    "A SolrQuery for query string q, or an empty one when q is nil.  The class
     moved packages in Solr 10, so callers must never name it.")
  (query? [this x]
    "True if x is this SolrJ's SolrQuery.")
  (stream-response-parser [this writer-type]
    "A ResponseParser that yields the raw response stream rather than parsing it,
     advertising writer-type (\"json\", \"xml\", ...) as the wt.  The class moved
     packages in Solr 10, so callers must not name it.")
  (capabilities [this]
    "Set of supported features, from #{:kerberos :connection-manager
     :concurrent-update :relaxed-hostname-verification}.  Callers should check
     rather than discover a missing class at runtime."))

(defn mark-shared!
  "Declare that connections of class klass are process-lifetime resources: a
   nested with-connection should reuse a bound one rather than build and close
   another.

   This is how a build that swaps in an embedded Solr server registers it,
   without clojure-solr ever naming the class:

     (mark-shared! org.apache.solr.client.solrj.embedded.EmbeddedSolrServer)"
  [klass]
  (extend klass
    SolrConnection
    {:drain    (fn [_] nil)
     :shared?  (fn [_] true)
     :base-url (fn [_] nil)
     :unwrap   identity})
  klass)

(defn- detect-impl-ns
  "SolrJ 9 has HttpSolrClient; SolrJ 10 does not.  Deliberately does not probe
   SolrQuery, which exists in both -- under different names."
  []
  (if (try (Class/forName "org.apache.solr.client.solrj.impl.HttpSolrClient")
           (catch ClassNotFoundException _ nil))
    'clojure-solr.impl.solr9
    'clojure-solr.impl.solr10))

;; Resolved on first use rather than at load, so that an implementation
;; namespace can require this one for the protocols without a circular load.
(def ^:private resolved
  (delay
    (let [ns-sym (detect-impl-ns)]
      (require ns-sym)
      (if-let [v (ns-resolve ns-sym 'impl)]
        @v
        (throw (ex-info (format "%s does not define an `impl` var" ns-sym)
                        {:namespace ns-sym}))))))

(defn impl
  "The SolrImpl for the SolrJ on the classpath."
  []
  @resolved)

(defn supports?
  "True if the running SolrJ supports feature."
  [feature]
  (contains? (capabilities (impl)) feature))

(defn kerberos-install!
  "Install the SolrJ Kerberos client builder, or explain why it cannot be."
  []
  (if (contains? (capabilities (impl)) :kerberos)
    ((ns-resolve 'clojure-solr.impl.solr9 'install-kerberos-client-builder!))
    (throw (ex-info (str "Kerberos is not supported by the SolrJ on the classpath. "
                         "Solr 10 removed hadoop-auth and Krb5HttpClientBuilder; SPNEGO "
                         "there needs a Jetty client customizer registered through "
                         "solr.solrj.http.jetty.customizer, which clojure-solr does not "
                         "yet provide.")
                    {:feature :kerberos}))))

(defn unsupported!
  "Throw a clear error rather than letting a missing class surface as a
   NoClassDefFoundError somewhere less obvious."
  [feature detail]
  (throw (ex-info (format "%s is not supported by the SolrJ on the classpath. %s"
                          (name feature) detail)
                  {:feature feature
                   :supported (capabilities (impl))})))
