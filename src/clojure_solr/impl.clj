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
    "True if this connection is a process-lifetime resource that a nested scope
     should reuse rather than rebuild and close.  EmbeddedSolrServer is the case
     that matters: each connect would otherwise re-derive a CoreContainer, and
     the embedded builds have exactly one.")
  (base-url [conn]
    "The collection URL this connection talks to, in the same shape on 9 and 10.
     Use instead of interop on .getBaseURL, which the authenticating wrapper does
     not expose.")
  (unwrap [conn]
    "The underlying SolrClient, unwrapping any decoration clojure-solr added."))

(extend-protocol SolrConnection
  SolrClient
  (drain [_] nil)
  (shared? [_] false)
  (base-url [_] nil)
  (unwrap [c] c))

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
