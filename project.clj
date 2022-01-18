(defproject cc.artifice/clojure-solr "4.3.2"
  :dependencies [;;[org.clojure/clojure "1.8.0"]
                 ;;[org.apache.solr/solr-solrj "6.6.6"]
                 ;;[org.apache.solr/solr-core "6.6.6" :exclusions [commons-fileupload joda-time]]
                 [commons-io "2.6"]
                 [commons-fileupload "1.4" :exclusions [commons-io]]
                 [clj-time "0.11.0" :exclusions [org.clojure/clojure]]]
  :plugins [[lein-pprint "1.3.2"]]
  :classifiers [["solr8" :solr8]
                ["solr7" :solr7]
                ["solr6" :solr6]]
  :pom-plugins [[org.apache.maven.plugins/maven-site-plugin "3.9.1"
                 [:configuration
                  [:port 8081]]]
                [org.apache.maven.plugins/maven-project-info-reports-plugin "3.1.1"]]
  :pom-addition [:reporting
                 [:outputDirectory "resources/public/html/site"]
                 [:plugins
                  [:plugin
                   [:groupId "org.owasp"]
                   [:artifactId "dependency-check-maven"]
                   [:version "6.0.3"]
                   [:configuration
                    [:scanSet
                     [:fileSet
                      [:directory "src/clojure_solr"]
                      ]]
                    [:outputDirectory "resources/public/html/site"]
                    [:reportingOutputDirectory "resources/public/html/site"]]
                   [:reportSets
                    [:reportSet
                     [:reports
                      [:report "aggregate"]]]]]]]
  :profiles {:dev {:dependencies [[clj-http "3.10.1" :exclusions [org.clojure/tools.reader
                                                                  org.apache.httpcomponents/httpmime
                                                                  org.apache.httpcomponents/httpcore
                                                                  org.apache.httpcomponents/httpclient]
                                   ]
                                  [cheshire "5.9.0"]]}
             :test {:dependencies [[cheshire "5.9.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :solr8 {:pom-addition [:properties ["solrj.major.version" "8"]]
                     :dependencies [[org.apache.solr/solr-core "8.6.3" :exclusions [commons-fileupload joda-time]]
                                    [org.apache.solr/solr-solrj "8.6.3"]]}
             :solr7 {:pom-addition [:properties ["solrj.major.version" "7"]]
                     :dependencies [[org.apache.solr/solr-core "7.7.3"
                                     :exclusions [commons-fileupload joda-time
                                                  com.google.protobuf/protobuf-java
                                                  com.fasterxml.jackson.core/jackson-databind
                                                  com.fasterxml.jackson.core/jackson-core
                                                  com.fasterxml.jackson.core/jackson-dataformat-smile
                                                  com.fasterxml.jackson.dataformat/jackson-dataformat-smile
                                                  org.eclipse.jetty/jetty-server
                                                  org.eclipse.jetty/jetty-http
                                                  org.eclipse.jetty/jetty-io
                                                  org.eclipse.jetty/jetty-continuation
                                                  org.eclipse.jetty/jetty-deploy
                                                  org.eclipse.jetty/jetty-jmx
                                                  org.eclipse.jetty/jetty-rewrite
                                                  org.eclipse.jetty/jetty-security
                                                  org.eclipse.jetty/jetty-servlet
                                                  org.eclipse.jetty/jetty-servlets
                                                  org.eclipse.jetty/jetty-util
                                                  org.eclipse.jetty/jetty-webapp
                                                  org.eclipse.jetty/jetty-xml
                                                  org.apache.htrace/htrace-core
                                                  org.slf4j/jcl-over-slf4j
                                                  org.slf4j/slf4j-jcl
                                                  org.codehaus.jackson/jackson-core-asl
                                                  org.codehaus.jackson/jackson-mapper-asl
                                                  dom4j/dom4j]]
                                    [org.apache.solr/solr-solrj "7.7.3"]]}
             :solr6 {:pom-addition [:properties ["solrj.major.version" "6"]]
                     :dependencies [[org.apache.solr/solr-core "6.6.6"
                                     :exclusions [commons-fileupload joda-time
                                                  com.fasterxml.jackson.core/jackson-core
                                                  com.fasterxml.jackson.core/jackson-dataformat-smile
                                                  com.fasterxml.jackson.dataformat/jackson-dataformat-smile
                                                  org.eclipse.jetty/jetty-server
                                                  org.eclipse.jetty/jetty-http
                                                  org.eclipse.jetty/jetty-io
                                                  org.eclipse.jetty/jetty-continuation
                                                  org.eclipse.jetty/jetty-deploy
                                                  org.eclipse.jetty/jetty-jmx
                                                  org.eclipse.jetty/jetty-rewrite
                                                  org.eclipse.jetty/jetty-security
                                                  org.eclipse.jetty/jetty-servlet
                                                  org.eclipse.jetty/jetty-servlets
                                                  org.eclipse.jetty/jetty-util
                                                  org.eclipse.jetty/jetty-webapp
                                                  org.eclipse.jetty/jetty-xml
                                                  org.apache.htrace/htrace-core
                                                  org.slf4j/jcl-over-slf4j
                                                  org.slf4j/slf4j-jcl
                                                  dom4j/dom4j
                                                  ]]
                                    [org.apache.solr/solr-solrj "6.6.6"]]}}
  :repositories [["restlet" {:url "https://repo.spring.io/libs-release-remote"}]])
