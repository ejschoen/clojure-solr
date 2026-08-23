============
clojure-solr
============

Clojure bindings for `Apache Solr <http://lucene.apache.org/solr/>`_.

Installation
============

To use within a Leiningen project, add the following to your
project.clj file:

::

    [cc.artifice/clojure-solr "6.0.0-SNAPSHOT"]

Note: Starting with release 3.0.0, Clojure and Solr dependencies are not part of the basic project definition.
Use lein with-profile +1.8,+solr7 repl (or test) for example to include Clojure 1.8 and Solr 7.7.3 dependencies.

Use :classifier option solr6, solr7, solr8, solr9 or solr10 in other Leiningen projects to get the appropriate builds,
and provide Clojure and Solr (solr-core, solr-solrj) dependencies in the project that uses clojure-solr.

Upgrading to 6.0.0
------------------

6.0.0 makes clojure-solr work against either SolrJ 9 or SolrJ 10 from one
artifact.  SolrJ 10 removed the Apache HttpClient-based clients and moved
``SolrQuery`` to another package, so anything in this library that named those
classes had to move behind ``clojure-solr.impl``.  The implementation is chosen
at load time; you do not pick one.

Most code needs no change.  ``connect``, ``with-connection``, ``search``,
``search*``, ``add-document!``, ``commit!`` and the rest keep their signatures
and behaviour.  What follows is the complete list of what does change.

**Client type is a keyword, not a class.**  ``:type`` no longer takes a class
object, so callers stop importing a class that a given SolrJ may not have::

    ;; before
    (:import (org.apache.solr.client.solrj.impl ConcurrentUpdateSolrClient))
    (connect url nil {:type ConcurrentUpdateSolrClient :queue-size 100})

    ;; after
    (connect url nil {:type :concurrent-update :queue-size 100})

Built-in types are ``:http`` (the default) and ``:concurrent-update``.  Register
others with ``defmethod clojure-solr/make-solr-client``.

**Capabilities instead of type tests.**  Two things callers used to determine by
type are now protocol methods in ``clojure-solr.impl``::

    ;; before
    (when (instance? ConcurrentUpdateSolrClient client)
      (.blockUntilFinished client))
    (.close client)

    ;; after
    (clojure-solr/drain client)
    (.close client)

``register-shutdown-hook!`` drains automatically, so callers using it need no
change at all.

**Embedded builds must declare their server shared.**  ``with-connection`` no
longer recognises an embedded server by matching on its class name.  A build
that swaps in ``EmbeddedSolrServer`` registers it once, alongside its
``make-solr-client`` method::

    (clojure-solr/mark-shared! EmbeddedSolrServer)

Without this a nested ``with-connection`` rebuilds the server rather than
reusing the bound one, which can fail on a solr-home mismatch.

**Base URL comes from the connection protocol.**  Use
``(clojure-solr/base-url conn)`` rather than ``(.getBaseURL conn)``.  The
underlying client differs between SolrJ versions and may be wrapped.

``drain``, ``shared?``, ``base-url`` and ``unwrap`` are the ``SolrConnection``
protocol, re-exported from ``clojure-solr`` so that replacing a type test does
not mean requiring ``clojure-solr.impl`` as well.  ``shared?`` is the one to use
where code closed a connection only when it was not an embedded server.

**Credentials are data.**  ``(set-credentials uri name password)`` is unchanged.
The two-argument form now takes a credential map -- build one with
``basic-credentials`` or ``token-credentials`` -- rather than an Apache object.
Objects satisfying ``SolrAuthentication`` still work on SolrJ 9 only, since they
are handed an Apache request and context that SolrJ 10 does not have.

**Pooling is deprecated.**  ``clojure-solr.pooling`` is removed; it had no
callers.  Instead of assembling an Apache connection manager, pass options to
``connect``::

    ;; before
    (connect url (doto (PoolingHttpClientConnectionManager. 300 TimeUnit/SECONDS)
                   (.setDefaultMaxPerRoute 8)
                   (.setMaxTotal 40)))

    ;; after
    (connect url {:max-connections-per-host 8
                  :max-connections-total 40      ; Solr 9 only
                  :time-to-live-seconds 300})    ; Solr 9 only

On SolrJ 10 the JDK client pools and reclaims idle connections by itself, so the
last two are reported once and ignored.  ``set-default-connection-manager`` and
``build-connection-socket-factory`` remain for SolrJ 9; the latter raises on
SolrJ 10, where there is no Apache socket factory to build.

**Connections are cached, and are no longer yours to close.**  ``connect``
returns the same client for the same URL, options and credentials, and
``with-connection`` no longer closes it::

    ;; this is one client and one connection pool, not one per document
    (doseq [doc docs]
      (with-connection (connect url)
        (add-document! doc)))

SolrJ 10 pools connections per client *instance*, so a client built per operation
pools nothing: it is a new TCP connection and a new HTTP/2 preface every time.
Measured on Java 17 against a local HTTP endpoint, 3200 saves through
connect-per-operation opened 3206 TCP connections and left the process holding
1458 extra epoll descriptors and 3272 extra threads; the same 3200 through the
cache used 10 connections, one epoll descriptor and 14 threads, and ran 27 times
faster with a p99 of 3.8 ms rather than 738 ms.

What changes for callers:

- ``with-connection`` closes a connection it created and leaves a cached one
  open.  ``shared?`` and ``cache-owned?`` both answer true for a cached client.
  Code that closes connections itself must consult one of them first.
- ``close-cached-connections!`` is the only thing that closes cached clients.
  There is no automatic eviction: the JDK client discards a broken connection on
  its own, and evicting on failure would restore the per-operation churn.  One
  client is held per distinct target, costing about 4 file descriptors and 6
  threads each (measured at 25 and at 100 targets, scaling linearly), so a
  process that talks to a great many collections should size for that.
- ``register-shutdown-hook!`` drains a cached connection but does not close it.
- ``:cache-client? false`` opts out and hands back a client the caller owns.
  ``:cache-client? true`` caches a type that would not be cached by default.
- Only ``:http`` connections are cached.  Buffering clients and embedded servers
  have lifecycles of their own.
- A nested ``with-connection`` still evaluates its own ``connect``.  Only a
  connection registered with ``mark-shared!`` is reused blindly; doing that for
  cached clients would point a nested scope at the wrong collection.

**Do not hand your own client to** ``with-connection`` **from several threads.**
The cache makes ``(with-connection (connect url) ...)`` safe, because no scope
may close a cached client.  It does nothing for a client you built yourself and
share: that one is not cache-owned, so the first scope to exit closes it while
the others are still mid-request.  Measured end-to-end on Java 17, eight workers
sharing one hand-built client and one more scope opening and closing around it:
**eight of eight workers parked permanently**, against zero of eight when the
same workers each called ``connect``.  On Java 21 neither parks.  If you hold
your own client, either use ``with-opened-connection``, which never closes, or
get it from ``connect`` and let the cache own it.

**Do not close a connection another thread is using.**  This is worth stating
separately because on Java 17 the failure is silent and permanent.
``HttpJdkSolrClient.close`` shuts down the ``ExecutorService`` the JDK client
delivers responses through -- and on Java 17 it does so without closing the JDK
client itself, which only became ``AutoCloseable`` in Java 21.  A request in
flight at that moment loses every bound it had, including its timeout, which
would have to arrive through the executor that just died.  Measured: on Java 17
that thread never returns, and a graceful ``shutdown()`` is enough to cause it;
on Java 21 the same sequence fails in milliseconds with a
``RejectedExecutionException``.  The applications run 17 and the test suite runs
21, so no test here can reproduce it.

Moving the applications to Java 21 needs no code change -- ``solr-solrj`` 10 is
compiled to class file 61 -- and it turns that silent wedge into a fast failure.
One behaviour does change: on Java 21 ``close`` really does close the JDK client,
and ``HttpClient.close`` waits for outstanding requests.  Measured with a request
in flight, closing took 3018 ms with a 4 s request timeout and was still blocked
at a 15 s cap with no timeout at all.  So on Java 21 the wait moves from the
requesting thread to the closing one, and the default ``:socket-timeout`` above
is what bounds it.  With the cache in place closes are rare, but
``close-cached-connections!`` is the call to think about.

**Forcing HTTP/1.1.**  ``:http1? true`` makes a Solr 10 connection speak HTTP/1.1
instead of HTTP/2::

    (connect url nil {:http1? true})

For a whole process, set it once at startup instead of at every call site::

    (clojure-solr/set-default-http-version! :http1)   ; or :http2, or nil

SolrJ offers this only as the ``solr.http1`` system property, which it reads in
the client builder's constructor.  That is read-on-build, so setting the property
after a client exists has no effect on it -- and because ``connect`` caches, a
client built before the property was set keeps speaking HTTP/2 for the life of
the process.  Both ``:http1?`` and ``set-default-http-version!`` are folded into what
distinguishes one cached client from another, so a change takes effect on the
next ``connect`` rather than being silently too late.  Ignored on Solr 9, whose
Apache client speaks HTTP/1.1 regardless.

Worth reaching for on JDKs between the backport of ``JDK-8335181`` (17.0.17,
21.0.8, 24) and the fix for ``JDK-8385131`` (28): in that window an HTTP/2
connection that receives GOAWAY with no active streams is marked final and never
closed.  See ``claude-docs/connection-reuse-and-timeouts.md``.

**Requests are bounded by default on Solr 10.**  ``:socket-timeout`` defaults to
120000 ms and ``:connection-timeout`` to 10000 ms, overridable per connection.
SolrJ's own defaults are 600000 and 60000, and its idle timeout is what it hands
to the JDK client as a per-request timeout.  These bounds do arm before a
connection exists -- they cover a connect that never completes and an HTTP/2
preface that never finishes -- but they cannot rescue a client someone closed,
which is why the reuse rules above matter more than the timeouts.

**If you construct a SolrQuery yourself**, the class moved packages in Solr 10.
Use ``(clojure-solr.impl/new-query (clojure-solr.impl/impl) "q")``, or pass a
query string to ``search*`` and let the library build it.  This one stays on
``clojure-solr.impl``: it is a property of the SolrJ in use, not of a
connection.

What SolrJ 10 does not support
------------------------------

``clojure-solr.impl/supports?`` answers these at runtime, and the corresponding
functions raise an explanatory error rather than failing on a missing class:

- ``:kerberos`` -- Solr 10 removed hadoop-auth and ``Krb5HttpClientBuilder``.
  ``set-kerberos-credentials`` works on SolrJ 9 only.
- ``:concurrent-update`` -- the batching client moved to ``solr-solrj-jetty``,
  which pulls in Jetty 12.  Note that ``ConcurrentUpdateJettySolrClient$Builder``
  requires an ``HttpJettySolrClient`` *instance*, so this is not a matter of
  naming a class: it obliges the caller to run a Jetty client.  Degrade to
  ``:http``, or batch in the caller.
- ``:connection-manager`` -- see pooling above.
- ``:relaxed-hostname-verification`` -- ``:ssl-trust-store :self-signed`` still
  trusts a self-signed certificate, but the JDK client always verifies the
  hostname and offers no way to disable it.  The certificate's subject
  alternative name must match the host being connected to.

Running against embedded Solr 10
--------------------------------

clojure-solr itself needs only SolrJ, but if your build embeds Solr for testing
there are four things about Solr 10 that are not API changes and so are easy to
miss.

**solr-core 10 requires Java 21.**  It is compiled to class file 65, while
solr-solrj 10 is class file 61.  Applications keep running on Java 17; anything
loading solr-core needs 21, at build time too if you AOT-compile a namespace that
imports ``EmbeddedSolrServer``.

**solr-core 10.0.0's published POM is invalid.**  Five Jackson dependencies carry
no version and nothing supplies one (SOLR-18185, open at time of writing), so
Maven and Leiningen both resolve it to a bare jar with no transitive dependencies
at all.  Either name its whole dependency closure yourself, or republish the POM
with the ``jackson-bom`` import that ``solr-solrj`` already carries.

**Solr 10 ignores** ``<lib>`` **directives in solrconfig.xml.**  ``SolrConfig``
has no lib handling left.  Put jars in ``<instanceDir>/lib``, which
``SolrResourceLoader`` still adds.  This fails silently -- the plugin class is
simply not found.

**Embedded Solr 10 needs Jetty on the classpath**, even though clojure-solr does
not: ``CoreContainer``'s constructor initialises ``TraceUtils``, whose static
initialiser links against ``org.eclipse.jetty.client.Request``.  Add
``solr-solrj-jetty`` and ``org.eclipse.jetty/jetty-client``.  Only the
application classpath comes out Jetty-free.

Two SolrJ API details bite code that builds an embedded server:
``CoreContainer.getSolrHome()`` returns ``String`` on Solr 9 and
``java.nio.file.Path`` on Solr 10, and ``HttpJdkSolrClient`` rejects use after
close where the Apache client tolerated it -- so a connection cache must not hand
out a client something else has closed.

Solr dependencies and Jetty
---------------------------

clojure-solr needs only SolrJ.  Nothing under ``src/`` references a solr-core class,
and nothing uses Solr's Jetty-based clients (Http2SolrClient and friends), so Solr's
Jetty can be excluded entirely.  For an application on Solr 9::

    [org.apache.solr/solr-solrj "9.7.0"
     :exclusions [org.eclipse.jetty/jetty-client
                  org.eclipse.jetty/jetty-http
                  org.eclipse.jetty/jetty-io
                  org.eclipse.jetty/jetty-util
                  org.eclipse.jetty/jetty-alpn-client
                  org.eclipse.jetty/jetty-alpn-java-client
                  org.eclipse.jetty.http2/http2-client
                  org.eclipse.jetty.http2/http2-common
                  org.eclipse.jetty.http2/http2-hpack
                  org.eclipse.jetty.http2/http2-http-client-transport
                  org.eclipse.jetty.toolchain/jetty-servlet-api]]

    ;; only if you call the ZooKeeper functions in clojure-solr.admin
    ;; (SolrZkClient, ZkStateReader); same :exclusions
    [org.apache.solr/solr-solrj-zookeeper "9.7.0" :exclusions [...]]

The ``solr9-client`` profile in this project's project.clj is exactly that
dependency set, and is verified to carry no Jetty::

    lein with-profile +1.11,+solr9-client classpath | tr : '\n' | grep -i jetty

Two things still require Solr's Jetty:

- ``set-kerberos-credentials``.  SolrJ's Krb5HttpClientBuilder implements a
  Jetty-based interface, so it needs jetty-client and http2-client back on the
  classpath.  clojure-solr loads that class reflectively, so excluding Jetty is
  harmless unless you actually call the function, which then throws an ex-info
  naming the missing jars.

- ``EmbeddedSolrServer``.  CoreContainer.load builds an Http2SolrClient, so
  embedded Solr pulls in solr-core and Solr's Jetty.  Solr 9.7's Http2SolrClient
  does **not** run on Jetty 12 -- Jetty 12 flattened ``org.eclipse.jetty.client.api.*``
  into ``org.eclipse.jetty.client.*``, so it fails with NoClassDefFoundError on
  ``org/eclipse/jetty/client/api/Response$CompleteListener``.  An application that
  runs on Jetty 12 therefore cannot host embedded Solr in the same JVM; run Solr
  out of process for those tests instead.

To build from source, run:

::

    lein jar

Usage
=====

- Basic usage  

  ::
  
      (with-connection (connect "http://127.0.0.1:8983/solr")
        (add-document! {"id" "testdoc", "name" "A Test Document"})
        (add-documents! [{"id" "testdoc.2", "name" "Another test"}
                                 {"id" "testdoc.3", "name" "a final test"}])
        (commit!)
        (search "test")
        (search "test" :rows 2)
        (search* "test" {:rows 2 :df "pages"})

- Advanced Usage
 
  - Since release 4.0.0, the Clojure wrapper around SolrJ has been refactored to use a middleware architecture, much like Ring.
    Middleware handlers are responsible for converting parameters to search or search* into SolrJ parameters, or injecting
    additional parameters into SolrJ queries, and are also responsible for converting search results into entries in the
    returned sequence of document maps or in the search result metadata.  The default middleware stack handles common
    use cases: searching with faceting and/or pivoting, optionally with a cursor, highlighting, collapsing and expanding
    results.
    
  - Optional parameters can be passed as a map to search*, that contains Solr parameter names as keywords e.g (start, fields, facet-filters, etc..)

  ::

      Optional keys, passed in a map:
      :collapse Name of field to collapse search results upon,
                or map of {!collapse ...} keys and values
                (requires wrap-collapse middleware, enabled by default)
      :cursor-mark true or a previous next-cursor-mark value from a previous search' metadata
                   (requires wrap-cursor-mark middleware [enabled by default],
                   and :sort option with a unique field name)
      :debugQuery true/false enable (default) or disable query debug values from Solr
                  (requires wrap-debug middleware, enabled by default)
      :expand Name of field upon which to expand collapsed search results,
              or map of expand.* parameters
              (requires wrap-expand middleware, enabled by default)
      :facet-date-ranges Date fields to facet as a vector or maps.  Each map contains
          :end Latest date (as java.util.Date)
          :field Field name
          :gap Faceting gap, as String, per Solr (+1HOUR, etc)
          :hardend Boolean (See Solr doc).  Optional.
          :include Comma-separated string: lower,upper,edge,outer,all.  Optional.
          :missing Boolean--return empty buckets if true.  Optional.
          :others  Comma-separated string: before,after,between,none,all.  Optional.
          :start Earliest date (as java.util.Date)
          :tag Optional, for referencing in a pivot facet
        Note: All faceting requires wrap-faceting middleware, enabled by default
      :facet-fields Discrete-valued fields to facet.  Can be a string, keyword, or map containing {:name ... :prefix ...}.
      :facet-filters Solr filter expression on facet values.  Passed as a map in the form: {:name 'facet-name' :value 'facet-value' :formatter (fn [name value] ...) } where :formatter is optional and is used to format the query.
      :facet-hier-sep Useful for path hierarchy token faceting.  A regex, such as \\|.
      :facet-mincount Minimum number of docs in a facet for the bucket to be returned.
      :facet-numeric-ranges Numeric fields to facet, as a vector of maps.  Map fields as for date ranges, but start, end and gap must be numbers.
      :facet-pivot-fields Vector of pivots to compute, each a list of facet fields.
       If a facet is tagged (e.g., {:tag ts} in :facet-date-ranges),
       then the string should be {!range=ts}other-facet.
       Otherwise, use comma separated lists: this-facet,other-facet.
       (requires wrap-pivoting middleware, enabled by default)
      :facet-queries Vector of facet queries, each encoded in a string or a map of {:name, :value, :formatter}.  :formatter is optional and defaults to the raw query formatter. The result is in the :facet-queries response.
      :fields Fields to return
      :method :get or :post (default :get)
      :request-handler (alternative request handler)
      :rows Number of rows to return (default is Solr default: 1000)
      :sort Sort field and direction (e.g., "modified desc")
      :spellcheck.* Spellcheck options (requires: wrap-spellcheck middleware
                                                  and handler must configure spellcheck)
      :start Offset into query result at which to start returning rows (default 0)
      :suggester-name  Name of suggester dictionary (requires wrap-suggest middleware,
                       and may need an alternative request handler)
      :all-suggesters  Return suggestions from all responding suggesters
  ::
  
    (with-connection...
      (search "query" {:rows 10, :start 0 :fields <vector-of-fieldnames> :facet-filters {:name "facet-name" :value "facet-value" :formatter (fn...)}) 
      ;; formatter is optional and used to format the query.

- Optionally use a connection manager 
  
  - (hint: Use PoolingHttpClientConnectionManager when clojure-solr is used in a web server to query Solr in a multithreaded environment, to avoid creating thousands of dangling CLOSE_WAIT sockets.)

  ::
    
    (with-connection (connect <url> <connection-manager>)
    ;; connection operations...
  
- Atomically update a document. 
  ::
    doc: can be a document previously fetched from solr or the id of such a document
    unique-key: Name of the attribute that is the document's unique key.
    changes: a vector of maps containg :attribute, :func (:set, :inc, :add) and :value. 
  
  ::
  
    (atomically-update! doc \"some-key"\ [{:attribute :client :func :set :value \"some-client-value\"}])
 
- Debug queries
  ::
    trace function: a function to "debug" query
    body: query operation.
    
  ::
  
    (with-trace (fn [str] (debug [str])) 
      (with-connection...
        (search... )))
 
- More Like this
  ::
    Execute a Solr moreLikeThis (mlt) query.
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
      method -- Solr Query method


  ::
  
    (more-like-this doc-id doc-id-name [fields..] {:min-doc-freq 4 :min-word-len 6 :max-results 10 ...})  
