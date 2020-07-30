(defproject cc.artifice/clojure-solr "2.1.1"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.apache.solr/solr-solrj "6.6.6"]
                 [org.apache.solr/solr-core "6.6.6" :exclusions [commons-fileupload joda-time]]
                 [commons-io "2.6"]
                 [commons-fileupload "1.4" :exclusions [commons-io]]
                 ;;[org.slf4j/slf4j-jcl "1.7.6"]
                 [clj-time "0.11.0" :exclusions [org.clojure/clojure]]]
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]]}}
  ;;:repositories [["restlet" {:url "http://maven.restlet.org"}]]
  :repositories [["restlet" {:url "https://repo.spring.io/libs-release-remote"}]]
  )
