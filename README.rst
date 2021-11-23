============
clojure-solr
============

Clojure bindings for `Apache Solr <http://lucene.apache.org/solr/>`_.

Installation
============

To use within a Leiningen project, add the following to your
project.clj file:

::

    [cc.artifice/clojure-solr "4.1.3"]

Note: Starting with release 3.0.0, Clojure and Solr dependencies are not part of the basic project definition.
Use lein with-profile +1.8,+solr7 repl (or test) for example to include Clojure 1.8 and Solr 7.7.3 dependencies.

Use :classifier option solr6, solr7, or solr8 in other Leiningen projects to get the appropriate builds,
and provide Clojure and Solr (solr-core, solr-solrj) dependencies in the project that uses clojure-solr.

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
