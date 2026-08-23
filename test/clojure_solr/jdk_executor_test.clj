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
   HttpJdkSolrClient at all.  THE SHAPE IS WHAT MATTERS: an earlier version of
   this file asserted that N tasks run concurrently, which any pool with N
   threads satisfies -- including a fixed pool that deadlocks at N+1.  What
   makes the deadlock impossible is the SynchronousQueue, not the size."
  (:require [clojure.test :refer :all]
            [clojure-solr :refer [connect close-cached-connections!]]
            [clojure-solr.impl :as impl])
  (:import (java.util.concurrent CountDownLatch ExecutorService Future
                                 SynchronousQueue ThreadPoolExecutor TimeUnit)))

(def ^:private solr10?
  (nil? (try (Class/forName "org.apache.solr.client.solrj.impl.HttpSolrClient")
             (catch ClassNotFoundException _ nil))))

(defn- solr10-only
  "Run f when the JDK client is the implementation, and SAY SO when it is not.
   A silently-skipped test reports zero assertions and passes, which is how a
   reverted fix would ship green under the +solr9 profile."
  [f]
  (if solr10?
    (f)
    (println "SKIPPED (not SolrJ 10):" (str *testing-vars*))))

(defn- solr10-var [sym]
  @(requiring-resolve (symbol "clojure-solr.impl.solr10" (name sym))))

(defn- client-executor
  "The executor field of a built HttpJdkSolrClient, read reflectively.
   This is the ONLY way to see what the client will actually run on; asserting
   against the shared var instead tests a different object entirely."
  [client]
  (let [f (.getDeclaredField
           (Class/forName "org.apache.solr.client.solrj.impl.HttpJdkSolrClient")
           "executor")]
    (.setAccessible f true)
    (.get f (impl/unwrap client))))

(deftest test-request-executor-cannot-queue-a-task-behind-a-running-one
  ;; The property whose absence IS the deadlock, asserted structurally.  A
  ;; SynchronousQueue has no capacity, so execute() can only hand off to a live
  ;; taker or start a thread -- it can never park a task behind a running one.
  ;; A fixed pool of any size would fail this and deadlock one request later.
  (solr10-only
   (fn []
     (let [^ThreadPoolExecutor ex (solr10-var 'request-executor)]
       (is (instance? SynchronousQueue (.getQueue ex))
           "a queue with capacity reintroduces the deadlock at maximumPoolSize+1")
       (is (zero? (.getCorePoolSize ex)))
       (is (= (solr10-var 'request-executor-max-threads) (.getMaximumPoolSize ex))
           "must be finite: exhaustion should reject one request, not OOM the JVM")
       ;; and behaviourally, since the structure above is only worth what it buys
       (let [n 12
             started (CountDownLatch. n)
             release (CountDownLatch. 1)
             futures (doall (for [_ (range n)]
                              (.submit ^ExecutorService ex
                                       ^Callable (fn [] (.countDown started)
                                                        (.await release)
                                                        :done))))]
         (try
           (is (.await started 10 TimeUnit/SECONDS)
               (str n " tasks must run concurrently; " (.getCount started) " never started"))
           (finally
             (.countDown release)
             (doseq [f futures] (.get ^Future f 10 TimeUnit/SECONDS)))))))))

(deftest test-keepalive-exceeds-the-largest-request-timeout
  ;; Not tuning.  A reader thread reaped mid-exchange makes the writer throw
  ;; IOException("Read end dead"); SolrJ closes the pipe and only LOGS, so the
  ;; JDK sees a clean end of body and Solr indexes a truncated document.
  (solr10-only
   (fn []
     (let [^ThreadPoolExecutor ex (solr10-var 'request-executor)
           keepalive-ms (* 1000 (solr10-var 'request-executor-keepalive-seconds))]
       (is (= (long (solr10-var 'request-executor-keepalive-seconds))
              (.getKeepAliveTime ex TimeUnit/SECONDS)))
       (is (> keepalive-ms (solr10-var 'default-socket-timeout))
           "a thread reaped while the writer is parked truncates the document")))))

(deftest test-built-client-uses-the-shared-executor
  ;; Pins the wiring.  connect on an unreachable URL opens no socket.
  (solr10-only
   (fn []
     (let [client (connect "http://localhost:18983/solr/nosuch")]
       (try
         (is (identical? (solr10-var 'request-executor) (client-executor client))
             "the client must run on clojure-solr's executor, not SolrJ's default")
         (finally (close-cached-connections!)))))))

(deftest test-closing-a-client-does-not-shut-down-its-executor
  ;; HttpJdkSolrClient.close shuts down an executor it created ITSELF, so
  ;; supplying one is what stops a close from stranding another thread's
  ;; in-flight CompletableFuture.  Asserted against the executor the CLIENT
  ;; held -- reading the shared var instead would pass even with the fix
  ;; reverted, because SolrJ would then have closed its own private pool.
  (solr10-only
   (fn []
     (let [client (connect "http://localhost:18983/solr/nosuch")
           ex ^ExecutorService (client-executor client)]
       (close-cached-connections!)
       (is (not (.isShutdown ex)))
       (is (= :alive (.get ^Future (.submit ex ^Callable (constantly :alive))
                           10 TimeUnit/SECONDS))
           "the executor the closed client was using must still work")))))
