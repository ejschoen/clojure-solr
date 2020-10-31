(ns clojure-solr.pooling
  (:refer-clojure :exclude [update cat])
  (:require [clojure-solr :as solr])
  (:import (java.net URI)
           (java.util.concurrent TimeUnit)
           (org.apache.http HttpRequest HttpRequestInterceptor HttpHeaders)
           (org.apache.http.auth AuthScope UsernamePasswordCredentials)
           (org.apache.http.client.protocol HttpClientContext)
           (org.apache.http.impl.auth BasicScheme)
           (org.apache.http.impl.client BasicCredentialsProvider HttpClientBuilder)
           (org.apache.http.impl.conn PoolingHttpClientConnectionManager)
           (org.apache.http.protocol HttpContext HttpCoreContext)
           (org.apache.http.conn ConnectionPoolTimeoutException)
           (org.apache.solr.client.solrj.impl HttpSolrClient HttpClientUtil)))

(def ^:private url-details (atom {}))
(def ^:private credentials-provider (BasicCredentialsProvider.))
(def ^:private credentials (atom {}))
(def ^:private connection-pool-options (ref {}))
(def ^:private connection-manager (ref nil))

(declare get-connection-manager)

(defmacro get-location [body]
  (str (:file (meta body)) ":" (:line (meta body))))

(defn set-connection-pool-options!
  [options]
  (dosync
   (ref-set connection-pool-options options)
   (when @connection-manager
     (.close @connection-manager)
     (ref-set connection-manager nil)
     (get-connection-manager options))
   options))

(defn get-connection-pool-options
  []
  @connection-pool-options)

(defn get-connection-manager
  "Get the existing pooling connection manager, or create one."
  ([]
   (get-connection-manager @connection-pool-options))
  ([{:keys [time-to-live-seconds max-connections-per-route max-connections-total]}]
   (dosync (or @connection-manager
               (ref-set connection-manager
                        (doto (if time-to-live-seconds
                                (PoolingHttpClientConnectionManager. time-to-live-seconds)
                                (PoolingHttpClientConnectionManager.))
                          (cond-> max-connections-per-route (.setDefaultMaxPerRoute max-connections-per-route))
                          (cond-> max-connections-total (.setMaxTotal max-connections-total))))))))

(def connections (ref {}))

(defn connect "Get a reusable connection to a Solr server"
  [url]
  (dosync (or (get connections url)
              (let [conn (solr/connect url (get-connection-manager))]
                (alter connections assoc url conn)
                conn))))

(defn disconnect
  "????? It's entirely unclear how one releases a SolrJ HttpSolrClient.  However, we don't do this really."
  [url]
  (dosync (alter connections dissoc url)))

(defmacro with-connection [conn & body]
  (if (and (> (count body) 1) (map? (first body)))
    `(binding [solr/*connection* ~conn]
       (try (do ~@(rest body))
            (catch ConnectionPoolTimeoutException e#
              (.warn (org.slf4j.LoggerFactory/getLogger ~(get-location body))
                     "Connection pool timeout exception connecting to {}"
                     (into-array Object [(.toURI (.getHost e#))]))
              (throw e#))
            (finally (.close solr/*connection*))))
    `(binding [solr/*connection* ~conn]
       (try (do ~@body)
            (catch ConnectionPoolTimeoutException e#
              (.warn (org.slf4j.LoggerFactory/getLogger ~(get-location body))
                     "Connection pool timeout exception connecting to {}"
                     (into-array Object [(.toURI (.getHost e#))]))
              (throw e#))
            (finally (.close solr/*connection*))))))



(defmacro with-solr-connection [& body]
  `(with-connection (connect @state/search-instance)
     ~@body))
