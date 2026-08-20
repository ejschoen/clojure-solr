(def jackson-version "2.18.0")

(def solr9-version "9.7.0")
(def solr10-version "10.0.0")
(def lucene10-version "10.3.2")

;; Jetty artifacts that solr-solrj pulls in for the Http2SolrClient family.
;; clojure-solr builds only the Apache HttpClient-based clients (HttpSolrClient,
;; ConcurrentUpdateSolrClient), so none of these are needed at runtime.  Excluding
;; them keeps Solr's Jetty off the classpath of applications that embed a
;; different Jetty version.
(def jetty-exclusions
  '[org.eclipse.jetty/jetty-client
    org.eclipse.jetty/jetty-http
    org.eclipse.jetty/jetty-io
    org.eclipse.jetty/jetty-util
    org.eclipse.jetty/jetty-alpn-client
    org.eclipse.jetty/jetty-alpn-java-client
    org.eclipse.jetty.http2/http2-client
    org.eclipse.jetty.http2/http2-common
    org.eclipse.jetty.http2/http2-hpack
    org.eclipse.jetty.http2/http2-http-client-transport
    org.eclipse.jetty.toolchain/jetty-servlet-api])

(defproject cc.artifice/clojure-solr "6.0.0-SNAPSHOT"
  :dependencies [[commons-io "2.6"]
                 [commons-fileupload "1.4" :exclusions [commons-io]]
                 [clj-time "0.11.0" :exclusions [org.clojure/clojure]]]
  :plugins [[lein-pprint "1.3.2"]]
  :classifiers [["solr10" :solr10]
                ["solr9" :solr9]
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
             ;; Everything clojure-solr itself needs at runtime, and nothing more.
             ;; solr-core is NOT required: nothing under src/ references a solr-core
             ;; class.  Neither is Jetty.  Use this profile as the model for the Solr
             ;; dependencies of an application that embeds a different Jetty version:
             ;;   lein with-profile +1.11,+solr9-client classpath | tr : '\\n' | grep jetty
             ;; should come back empty.
             :solr9-client {:pom-addition [:properties ["solrj.major.version" "9"]]
                            :dependencies [[org.apache.solr/solr-solrj ~solr9-version
                                            :exclusions ~jetty-exclusions]
                                           ;; SolrZkClient/ZkStateReader, used by clojure-solr.admin
                                           [org.apache.solr/solr-solrj-zookeeper ~solr9-version
                                            :exclusions ~jetty-exclusions]]}
             ;; Solr 10 runtime.  solr-solrj 10 declares no Jetty, no ZooKeeper and no
             ;; Apache HttpClient of its own, so there is nothing to exclude -- the
             ;; modules are simply not added.  solr-solrj-jetty is deliberately absent:
             ;; it is needed only for relaxed TLS or Kerberos.
             :solr10-client {:pom-addition [:properties ["solrj.major.version" "10"]]
                             :dependencies [[org.apache.solr/solr-solrj ~solr10-version]
                                            ;; solr-solrj-zookeeper pulls solr-solrj-jetty at runtime
                                            ;; scope, for CloudSolrClient's Jetty fallback.  clojure-solr
                                            ;; uses SolrZkClient directly and never builds a
                                            ;; CloudSolrClient, so excluding it keeps Jetty off the
                                            ;; classpath -- which is the whole point of the JDK client.
                                            [org.apache.solr/solr-solrj-zookeeper ~solr10-version
                                             :exclusions [org.apache.solr/solr-solrj-jetty]]]}
             ;; Solr 10 build/test profile.
             ;;
             ;; solr-core 10.0.0's published POM is invalid -- five Jackson
             ;; dependencies carry no <version> and there is no <parent> or
             ;; <dependencyManagement> to supply one -- so Maven reports "the POM
             ;; ... is invalid, transitive dependencies (if any) will not be
             ;; available" and Leiningen resolves it to the bare jar.  Every one
             ;; of its dependencies is therefore named here, generated from that
             ;; POM.  This affects any consumer of embedded Solr 10, not just
             ;; this project.
             :solr10 {:pom-addition [:properties ["solrj.major.version" "10"]]
                      :dependencies [
                      [org.apache.solr/solr-core ~solr10-version]
                      [org.apache.solr/solr-api ~solr10-version]
                      [org.apache.solr/solr-solrj ~solr10-version]
                      [org.apache.solr/solr-solrj-zookeeper ~solr10-version]
                      [org.apache.solr/solr-solrj-streaming ~solr10-version]
                      [org.apache.solr/solr-solrj-jetty ~solr10-version]
                      [org.apache.lucene/lucene-core "10.3.2"]
                      [org.apache.lucene/lucene-analysis-common "10.3.2"]
                      [org.apache.lucene/lucene-queries "10.3.2"]
                      [org.slf4j/slf4j-api "2.0.17"]
                      [io.opentelemetry/opentelemetry-api "1.56.0"]
                      [io.swagger.core.v3/swagger-annotations-jakarta "2.2.22"]
                      [io.dropwizard.metrics/metrics-core "4.2.26"]
                      [org.glassfish.jersey.containers/jersey-container-jetty-http "2.39.1"]
                      [org.glassfish.jersey.inject/jersey-hk2 "3.1.11"]
                      [org.glassfish.jersey.media/jersey-media-json-jackson "3.1.11"]
                      [org.glassfish.jersey.core/jersey-common "3.1.11"]
                      [org.glassfish.jersey.core/jersey-server "3.1.11"]
                      [org.glassfish.hk2/hk2-api "3.1.1"]
                      [jakarta.inject/jakarta.inject-api "2.0.1"]
                      [jakarta.ws.rs/jakarta.ws.rs-api "3.1.0"]
                      [jakarta.annotation/jakarta.annotation-api "2.1.1"]
                      [jakarta.servlet/jakarta.servlet-api "6.0.0"]
                      [org.apache.lucene/lucene-codecs "10.3.2"]
                      [org.apache.lucene/lucene-backward-codecs "10.3.2"]
                      [org.apache.lucene/lucene-classification "10.3.2"]
                      [org.apache.lucene/lucene-expressions "10.3.2"]
                      [org.apache.lucene/lucene-grouping "10.3.2"]
                      [org.apache.lucene/lucene-highlighter "10.3.2"]
                      [org.apache.lucene/lucene-join "10.3.2"]
                      [org.apache.lucene/lucene-misc "10.3.2"]
                      [org.apache.lucene/lucene-queryparser "10.3.2"]
                      [org.apache.lucene/lucene-sandbox "10.3.2"]
                      [org.apache.lucene/lucene-spatial-extras "10.3.2"]
                      [org.apache.lucene/lucene-suggest "10.3.2"]
                      [com.google.guava/guava "33.4.8-jre"]
                      [org.apache.commons/commons-lang3 "3.20.0"]
                      [org.apache.commons/commons-math3 "3.6.1"]
                      [commons-io/commons-io "2.20.0"]
                      [com.carrotsearch/hppc "0.10.0"]
                      [com.github.ben-manes.caffeine/caffeine "3.2.2"]
                      [commons-codec/commons-codec "1.19.0"]
                      [commons-cli/commons-cli "1.10.0"]
                      [org.locationtech.spatial4j/spatial4j "0.8"]
                      [org.eclipse.jetty/jetty-client "12.0.27"]
                      [org.eclipse.jetty/jetty-http "12.0.27"]
                      [org.eclipse.jetty/jetty-io "12.0.27"]
                      [org.eclipse.jetty/jetty-util "12.0.27"]
                      [org.apache.curator/curator-framework "5.9.0"]
                      [org.apache.curator/curator-client "5.9.0"]
                      [org.apache.zookeeper/zookeeper "3.9.4"]
                      [org.apache.zookeeper/zookeeper-jute "3.9.4"]
                      [com.jayway.jsonpath/json-path "2.9.0"]
                      [com.tdunning/t-digest "3.3"]
                      [io.opentelemetry/opentelemetry-context "1.56.0"]
                      [io.opentelemetry/opentelemetry-exporter-prometheus "1.56.0-alpha"]
                      [io.opentelemetry/opentelemetry-sdk "1.56.0"]
                      [io.opentelemetry/opentelemetry-sdk-metrics "1.56.0"]
                      [io.opentelemetry.instrumentation/opentelemetry-runtime-telemetry-java17 "2.22.0-alpha"]
                      [org.apache.commons/commons-exec "1.5.0"]
                      [org.apache.logging.log4j/log4j-api "2.25.3"]
                      [org.apache.logging.log4j/log4j-core "2.25.3"]
                      [io.prometheus/prometheus-metrics-model "1.1.0"]
                      [io.prometheus/prometheus-metrics-exposition-formats "1.1.0"]
                      [org.codehaus.woodstox/stax2-api "4.2.2"]
                      [com.fasterxml.woodstox/woodstox-core "7.0.0"]
                      [com.j256.simplemagic/simplemagic "1.17"]
                      [org.apache.lucene/lucene-analysis-kuromoji "10.3.2"]
                      [org.apache.lucene/lucene-analysis-nori "10.3.2"]
                      [org.apache.lucene/lucene-analysis-phonetic "10.3.2"]
                      [org.xerial.snappy/snappy-java "1.1.10.8"]
                      [org.apache.logging.log4j/log4j-slf4j2-impl "2.25.3"]
                      [com.fasterxml.jackson.core/jackson-annotations ~jackson-version]
                      [com.fasterxml.jackson.core/jackson-core ~jackson-version]
                      [com.fasterxml.jackson.core/jackson-databind ~jackson-version]
                      [com.fasterxml.jackson.dataformat/jackson-dataformat-smile ~jackson-version]
                      [com.fasterxml.jackson.dataformat/jackson-dataformat-cbor ~jackson-version]]}
             ;; Build/test profile.  solr-core is here only for the EmbeddedSolrServer
             ;; the test suite runs against; CoreContainer.load builds an Http2SolrClient,
             ;; so Jetty must stay on this classpath.
             :solr9 {:pom-addition [:properties ["solrj.major.version" "9"]]
                     :dependencies [[org.apache.solr/solr-core ~solr9-version
                                     :exclusions [commons-fileupload
                                                  joda-time
                                                  org.apache.logging.log4j/log4j-slf4j2-impl]]
                                    [org.apache.solr/solr-solrj ~solr9-version]]}
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
