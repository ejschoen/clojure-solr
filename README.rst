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
    (clojure-solr.impl/drain client)
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
``(clojure-solr.impl/base-url conn)`` rather than ``(.getBaseURL conn)``.  The
underlying client differs between SolrJ versions and may be wrapped.

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

**If you construct a SolrQuery yourself**, the class moved packages in Solr 10.
Use ``(clojure-solr.impl/new-query (clojure-solr.impl/impl) "q")``, or pass a
query string to ``search*`` and let the library build it.

What SolrJ 10 does not support
------------------------------

``clojure-solr.impl/supports?`` answers these at runtime, and the corresponding
functions raise an explanatory error rather than failing on a missing class:

- ``:kerberos`` -- Solr 10 removed hadoop-auth and ``Krb5HttpClientBuilder``.
  ``set-kerberos-credentials`` works on SolrJ 9 only.
- ``:concurrent-update`` -- the batching client moved to ``solr-solrj-jetty``,
  which pulls in Jetty 12.  Add that artifact, or batch in the caller.
- ``:connection-manager`` -- see pooling above.
- ``:relaxed-hostname-verification`` -- ``:ssl-trust-store :self-signed`` still
  trusts a self-signed certificate, but the JDK client always verifies the
  hostname and offers no way to disable it.  The certificate's subject
  alternative name must match the host being connected to.

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
