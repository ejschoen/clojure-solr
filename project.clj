(defproject cc.artifice/clojure-solr "3.0.0"
  :dependencies [;;[org.clojure/clojure "1.8.0"]
                 ;;[org.apache.solr/solr-solrj "6.6.6"]
                 ;;[org.apache.solr/solr-core "6.6.6" :exclusions [commons-fileupload joda-time]]
                 [commons-io "2.6"]
                 [commons-fileupload "1.4" :exclusions [commons-io]]
                 [clj-time "0.11.0" :exclusions [org.clojure/clojure]]]
  :classifiers [["solr8" :solr8]
                ["solr7" :solr7]
                ["solr6" :solr6]]
  :profiles {:1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :solr8 {:dependencies [[org.apache.solr/solr-core "8.6.3" :exclusions [commons-fileupload joda-time]]
                                    [org.apache.solr/solr-solrj "8.6.3"]]}
             :solr7 {:dependencies [[org.apache.solr/solr-core "7.7.3" :exclusions [commons-fileupload joda-time]]
                                    [org.apache.solr/solr-solrj "7.7.3"]]}
             :solr6 {:dependencies [[org.apache.solr/solr-core "6.6.6" :exclusions [commons-fileupload joda-time]]
                                    [org.apache.solr/solr-solrj "6.6.6"]]}}
  :repositories [["restlet" {:url "https://repo.spring.io/libs-release-remote"}]])
