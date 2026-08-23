(ns clojure-solr.jdk-executor-test
  "Regression tests for the SolrJ 10 request-executor deadlock.

   SolrJ builds HttpJdkSolrClient with MDCAwareThreadPoolExecutor(4, 256, 60s,
   LinkedBlockingQueue(1024)) and hands that same executor BOTH to its own
   request-body marshalling and to HttpClient.Builder.executor.  A
   ThreadPoolExecutor grows past its core size only when the queue is full, so
   1024 free slots pin the pool at four threads.  Four request bodies larger
   than PipedInputStream's 1KB buffer then hold all four in
   PipedOutputStream.write while the reads that would drain those pipes queue
   behind them -- and every caller parks in CompletableFuture.get with no
   timeout in scope, because the block happens before the exchange starts.

   Upstream: SOLR-17707, and the solr-user thread at
   https://lists.apache.org/thread/m5yz199r8bv7qo7251cc42wr7rz16q6b.

   Observed in production 2026-08-23: eight worker threads and the heartbeat
   carrier of one node, all parked on one shared client, for hours.

   These tests assert the shape of the executor rather than driving Solr,
   because the embedded server used elsewhere in this suite never goes through
   HttpJdkSolrClient at all."
  (:require [clojure.test :refer :all]
            [clojure-solr :refer [connect close-cached-connections!]]
            [clojure-solr.impl :as impl]))

(def ^:private solr10?
  (nil? (try (Class/forName "org.apache.solr.client.solrj.impl.HttpSolrClient")
             (catch ClassNotFoundException _ nil))))

(defn- request-executor []
  @(requiring-resolve 'clojure-solr.impl.solr10/request-executor))

(deftest test-request-executor-never-queues-behind-a-busy-task
  ;; The property whose absence is the deadlock: a submitted task must never
  ;; wait for a running one to finish.  With SolrJ's default executor exactly
  ;; four of these would start and the rest would sit in the queue.
  (when solr10?
    (let [n 12
          started (java.util.concurrent.CountDownLatch. n)
          release (java.util.concurrent.CountDownLatch. 1)
          ex (request-executor)
          futures (doall (for [_ (range n)]
                           (.submit ^java.util.concurrent.ExecutorService ex
                                    ^Callable (fn []
                                                (.countDown started)
                                                (.await release)
                                                :done))))]
      (try
        (is (.await started 10 java.util.concurrent.TimeUnit/SECONDS)
            (str n " tasks must run concurrently; " (.getCount started)
                 " never started, which is the queue-behind-a-busy-task shape "
                 "that deadlocks HttpJdkSolrClient"))
        (finally
          (.countDown release)
          (doseq [f futures] (.get ^java.util.concurrent.Future f 10
                                   java.util.concurrent.TimeUnit/SECONDS)))))))

(deftest test-built-client-uses-the-shared-executor
  ;; Pins the wiring itself: .withExecutor must actually be called, or the
  ;; client silently falls back to SolrJ's deadlocking default.  connect on an
  ;; unreachable URL opens no socket, so this needs no server.
  (when solr10?
    (let [client (connect "http://localhost:18983/solr/nosuch")]
      (try
        (let [f (.getDeclaredField
                 (Class/forName "org.apache.solr.client.solrj.impl.HttpJdkSolrClient")
                 "executor")]
          (.setAccessible f true)
          (is (identical? (request-executor) (.get f (impl/unwrap client)))
              "the built client must run on clojure-solr's executor, not SolrJ's default"))
        (finally (close-cached-connections!))))))

(deftest test-closing-a-client-does-not-shut-down-the-executor
  ;; HttpJdkSolrClient.close shuts down an executor it created itself.  Supplying
  ;; one sets shutdownExecutor false, which is what stops one scope's close from
  ;; stranding another thread's in-flight CompletableFuture -- the hazard
  ;; described above the connection cache in clojure-solr.impl.
  (when solr10?
    (let [_ (connect "http://localhost:18983/solr/nosuch")]
      (close-cached-connections!)
      (let [ex (request-executor)]
        (is (not (.isShutdown ^java.util.concurrent.ExecutorService ex)))
        (is (= :alive (.get ^java.util.concurrent.Future
                            (.submit ^java.util.concurrent.ExecutorService ex
                                     ^Callable (constantly :alive))
                            10 java.util.concurrent.TimeUnit/SECONDS))
            "the shared executor must survive closing a client")))))
