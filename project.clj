(def jackson-version "2.18.0")

(defproject cc.artifice/clojure-solr "4.8.0-SNAPSHOT"
  :dependencies [[commons-io "2.6"]
                 [commons-fileupload "1.4" :exclusions [commons-io]]
                 [clj-time "0.11.0" :exclusions [org.clojure/clojure]]]
  :plugins [[lein-pprint "1.3.2"]]
  :classifiers [["solr9" :solr9]
                ["solr8" :solr8]
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
                   [:version "11.1.0"]
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
  :managed-dependencies [[com.fasterxml.jackson.core/jackson-core ~jackson-version]]
  :profiles {:dev {:dependencies [[clj-http "3.10.1" :exclusions [org.clojure/tools.reader
                                                                  org.apache.httpcomponents/httpmime
                                                                  org.apache.httpcomponents/httpcore
                                                                  org.apache.httpcomponents/httpclient]
                                   ]
                                  [cheshire "5.9.0"]]}
             :dev-http {:dependencies [[clj-http "3.10.1"]
                                       [cheshire "5.9.0"]]}
             :attachable {:jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=4000"]}
             :test {:dependencies [[cheshire "5.9.0"]
                                   [com.fasterxml.jackson.core/jackson-core ~jackson-version]
                                   [com.fasterxml.jackson.dataformat/jackson-dataformat-cbor ~jackson-version]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.2"]]}
             :solr9 {:pom-addition [:properties ["solrj.major.version" "9"]]
                     :dependencies [[org.apache.solr/solr-core "9.7.0" :exclusions [commons-fileupload 
                                                                                    joda-time
                                                                                    org.apache.logging.log4j/log4j-slf4j2-impl]]
                                    [org.apache.solr/solr-solrj "9.7.0"]]}
             :solr8 {:pom-addition [:properties ["solrj.major.version" "8"]]
                     :dependencies [[org.apache.solr/solr-core "8.11.4" :exclusions [commons-fileupload joda-time]]
                                    [org.apache.solr/solr-solrj "8.11.4"]]}
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
  :repositories [["restlet" {:url "https://repo.spring.io/libs-release-remote"}]
                 ["maven-restlet" {:url "https://maven.restlet.talend.com"}]]
)
