(ns solr-stress
  "Stress the connection cache against a real HTTP endpoint, on the JVM the
   applications actually run.  solr-solrj 10 is class file 61, so this needs no
   solr-core and no Java 21 -- which is the point: the wedge is a Java 17
   behaviour that the Java 21 test suite cannot reproduce."
  (:require [clojure-solr :as solr]
            [clojure-solr.impl :as impl])
  (:import (com.sun.net.httpserver HttpServer HttpHandler)
           (java.io ByteArrayOutputStream)
           (java.lang.management ManagementFactory)
           (java.net InetSocketAddress InetAddress)
           (java.nio.file Files)
           (java.util.concurrent CountDownLatch Executors TimeUnit)
           (java.util.concurrent.atomic AtomicInteger)
           (org.apache.solr.client.solrj.response JavaBinResponseParser)
           (org.apache.solr.common SolrDocument SolrDocumentList)
           (org.apache.solr.common.util JavaBinCodec NamedList)))

;;; --------------------------------------------------------------------------
;;; A fake Solr that answers javabin
;;; --------------------------------------------------------------------------

(defn- canned-response ^bytes []
  (let [docs (doto (SolrDocumentList.)
               (.setNumFound 1)
               (.setStart 0)
               (.add (doto (SolrDocument.)
                       (.addField "id" "1")
                       (.addField "title" "stress"))))
        header (doto (NamedList.) (.add "status" (int 0)) (.add "QTime" (int 1)))
        body (doto (NamedList.) (.add "responseHeader" header) (.add "response" docs))
        bos (ByteArrayOutputStream.)]
    (.marshal (JavaBinCodec.) body bos)
    (.toByteArray bos)))

(defn- read-request!
  "Read one HTTP/1.1 request off in.  Returns :eof, or the Content-Length body
   having been consumed."
  [^java.io.InputStream in]
  (let [line (StringBuilder.)
        headers (StringBuilder.)]
    (loop [prev -1]
      (let [b (.read in)]
        (cond
          (neg? b) :eof
          :else (do (.append headers (char b))
                    (let [n (.length headers)]
                      (if (and (>= n 4)
                               (= "\r\n\r\n" (.substring headers (- n 4) n)))
                        (let [h (.toString headers)
                              m (re-find #"(?i)content-length:\s*(\d+)" h)
                              len (if m (Integer/parseInt (second m)) 0)]
                          (when (pos? len)
                            (let [buf (byte-array len)]
                              (loop [off 0]
                                (when (< off len)
                                  (let [r (.read in buf off (- len off))]
                                    (when (pos? r) (recur (+ off r))))))))
                          :ok)
                        (recur b)))))))))

(defn start-server
  "A raw HTTP/1.1 server.  Deliberately not com.sun.net.httpserver: that one
   leaves Nagle on and writes the response in several calls, which pairs with the
   peer's delayed ACK to put a flat ~40 ms on every request that reuses a
   connection -- an artefact of the harness that looks exactly like a client
   problem.  Here TCP_NODELAY is set and the whole response goes out in one
   write."
  [delay-ms]
  (let [body (canned-response)
        ctype (first (.getContentTypes (JavaBinResponseParser.)))
        hits (AtomicInteger.)
        in-flight (AtomicInteger.)
        peak (AtomicInteger.)
        conns (java.util.Collections/synchronizedSet (java.util.HashSet.))
        head (.getBytes (str "HTTP/1.1 200 OK\r\n"
                             "Content-Type: " ctype "\r\n"
                             "Content-Length: " (alength body) "\r\n"
                             "\r\n")
                        "US-ASCII")
        response (let [b (byte-array (+ (alength head) (alength body)))]
                   (System/arraycopy head 0 b 0 (alength head))
                   (System/arraycopy body 0 b (alength head) (alength body))
                   b)
        ss (doto (java.net.ServerSocket.)
             (.setReuseAddress true)
             (.bind (InetSocketAddress. (InetAddress/getLoopbackAddress) 0) 512))
        pool (Executors/newCachedThreadPool
              (reify java.util.concurrent.ThreadFactory
                (newThread [_ r] (doto (Thread. r "fake-solr") (.setDaemon true)))))]
    (doto (Thread.
           (fn []
             (try
               (loop []
                 (let [sock (.accept ss)]
                   (.setTcpNoDelay sock true)
                   (.add conns (str (.getRemoteSocketAddress sock)))
                   (.execute pool
                     (fn []
                       (try
                         (let [in (java.io.BufferedInputStream. (.getInputStream sock))
                               out (.getOutputStream sock)]
                           (loop []
                             (when (= :ok (read-request! in))
                               (.incrementAndGet hits)
                               (let [n (.incrementAndGet in-flight)]
                                 (loop [p (.get peak)]
                                   (when (and (> n p) (not (.compareAndSet peak p n)))
                                     (recur (.get peak)))))
                               (when (pos? delay-ms) (Thread/sleep delay-ms))
                               (.write out response)
                               (.flush out)
                               (.decrementAndGet in-flight)
                               (recur))))
                         (catch Throwable _ nil)
                         (finally (try (.close sock) (catch Throwable _ nil))))))
                   (recur)))
               (catch Throwable _ nil)))
           "fake-solr-accept")
      (.setDaemon true)
      (.start))
    {:server ss :port (.getLocalPort ss) :hits hits :peak peak :conns conns}))

;;; --------------------------------------------------------------------------
;;; What the process is holding
;;; --------------------------------------------------------------------------

(defn- fd-stats []
  (let [files (or (.listFiles (java.io.File. "/proc/self/fd")) (make-array java.io.File 0))]
    (reduce (fn [m f]
              (let [target (try (str (Files/readSymbolicLink (.toPath f))) (catch Throwable _ ""))]
                (cond-> (update m :fds inc)
                  (.contains target "socket")    (update :sockets inc)
                  (.contains target "eventpoll") (update :eventpoll inc))))
            {:fds 0 :sockets 0 :eventpoll 0}
            files)))

(defn- snapshot []
  (assoc (fd-stats)
         :threads (.getThreadCount (ManagementFactory/getThreadMXBean))
         :cached  (impl/cached-client-count)))

(defn- delta [a b]
  (into {} (for [k (keys b)] [k (- (get b k) (get a k))])))

;;; --------------------------------------------------------------------------
;;; Scenarios
;;; --------------------------------------------------------------------------

(defn- worker-stacks [threads]
  (doseq [^Thread t threads :when (.isAlive t)]
    (println "    PARKED" (.getName t))
    (doseq [f (take 3 (.getStackTrace t))] (println "      at" f))))

(defn run-load
  "connect-per-operation, exactly as i2kconduit-db does it per document."
  [label url opts threads iters]
  (System/gc)
  (Thread/sleep 300)
  (let [before  (snapshot)
        errors  (AtomicInteger.)
        lat     (java.util.Collections/synchronizedList (java.util.ArrayList.))
        handed  (java.util.Collections/synchronizedSet
                 (java.util.Collections/newSetFromMap (java.util.IdentityHashMap.)))
        latch   (CountDownLatch. threads)
        t0      (System/nanoTime)
        ws      (doall
                 (for [i (range threads)]
                   (doto (Thread.
                          (fn []
                            (try
                              (dotimes [_ iters]
                                (let [t1 (System/nanoTime)
                                      c (solr/connect url nil opts)]
                                  (.add handed c)
                                  (solr/with-connection c
                                    (solr/search "*:*" :rows 1))
                                  (.add lat (double (/ (- (System/nanoTime) t1) 1000000.0)))))
                              (catch Throwable t
                                (.incrementAndGet errors)
                                (println "    !" (.getSimpleName (class t)) (.getMessage t)))
                              (finally (.countDown latch))))
                          (str label "-" i))
                     (.setDaemon true)
                     (.start))))
        done    (.await latch 180 TimeUnit/SECONDS)
        ms      (long (/ (- (System/nanoTime) t0) 1000000))
        after   (snapshot)]
    (println (format "  %-28s %6d ops  %6d ms  clients=%-4d errors=%-3d %s"
                     label (* threads iters) ms (count handed) (.get errors)
                     (if done "" "*** DID NOT FINISH ***")))
    (println (format "  %-28s held: %s   delta: %s" "" (pr-str after) (pr-str (delta before after))))
    (let [xs (vec (sort (vec lat)))
          n  (count xs)
          pct (fn [q] (if (zero? n) 0.0 (nth xs (min (dec n) (int (* q n))))))]
      (when (pos? n)
        (println (format "  %-28s per-op: min=%.1f p50=%.1f p95=%.1f p99=%.1f max=%.1f ms"
                         "" (first xs) (pct 0.5) (pct 0.95) (pct 0.99) (last xs)))))
    (when-not done (worker-stacks ws))
    {:label label :ops (* threads iters) :ms ms :clients (count handed)
     :errors (.get errors) :finished done :delta (delta before after)}))

(defn run-cache-race
  "Many threads, few targets, no closes: the cache must hand out exactly one
   client per target however hard it is hit."
  [url-fn targets threads iters]
  (let [errors (AtomicInteger.)
        seen   (atom {})
        latch  (CountDownLatch. threads)]
    (dotimes [i threads]
      (doto (Thread.
             (fn []
               (try
                 (dotimes [n iters]
                   (let [k (mod (+ i n) targets)
                         c (solr/connect (url-fn k))]
                     (swap! seen update k (fnil conj #{}) (System/identityHashCode c))))
                 (catch Throwable t (.incrementAndGet errors) (println "    !" (.getMessage t)))
                 (finally (.countDown latch))))
             (str "race-" i))
        (.setDaemon true)
        (.start)))
    (let [done (.await latch 120 TimeUnit/SECONDS)
          per  (into (sorted-map) (for [[k v] @seen] [k (count v)]))]
      (println (format "  %-28s targets=%d distinct-clients-per-target=%s errors=%d %s"
                       "concurrent connect" targets (pr-str (vals per)) (.get errors)
                       (if done "" "*** DID NOT FINISH ***")))
      {:per-target per :errors (.get errors) :finished done})))

(defn run-close-race
  "connect and close-cached-connections! interleaved.  Nothing may throw, and
   nothing may hang; a client handed out mid-close is the caller's problem, but
   the cache itself must stay consistent."
  [url threads iters]
  (let [errors (AtomicInteger.)
        closed (AtomicInteger.)
        latch  (CountDownLatch. (inc threads))]
    (doto (Thread. (fn []
                     (try (dotimes [_ 200]
                            (Thread/sleep 3)
                            (.addAndGet closed (solr/close-cached-connections!)))
                          (catch Throwable t (.incrementAndGet errors) (println "    !" (.getMessage t)))
                          (finally (.countDown latch))))
                   "closer")
      (.setDaemon true) (.start))
    (dotimes [i threads]
      (doto (Thread. (fn []
                       (try (dotimes [n iters]
                              ;; A rotating target so most connects miss and
                              ;; actually build, which is what races the close.
                              (solr/connect (str url "-" (mod (+ i n) 64))))
                            (catch Throwable t (.incrementAndGet errors) (println "    !" (.getMessage t)))
                            (finally (.countDown latch))))
                     (str "churn-" i))
        (.setDaemon true) (.start)))
    (let [done (.await latch 120 TimeUnit/SECONDS)]
      ;; The invariant that matters is not that every handed-out client is still
      ;; cached -- a close may land between the two -- but that nothing is left
      ;; registered and unreachable, which is a client nobody can ever close.
      (let [cached (impl/cached-client-count) owned (impl/cache-owned-count)]
        (println (format "  %-28s closes=%d errors=%d cached=%d owned=%d %s %s"
                         "connect vs close" (.get closed) (.get errors) cached owned
                         (if (= cached owned) "consistent" "*** LEAKED REGISTRATIONS ***")
                         (if done "" "*** DID NOT FINISH ***"))))
      {:errors (.get errors) :finished done})))

;;; --------------------------------------------------------------------------

(defn run-shared-close
  "The production shape, end to end: N workers using one client at once, and one
   more scope that borrows the same client, does nothing, and exits while they
   are still mid-request.  That exiting scope runs with-connection's finally.

   mode :manual  -- the caller built the client and shares it, so it is not
                    cache-owned and with-connection still closes it.
   mode :cached  -- every worker calls connect, which hands back the one cached
                    client that no scope may close."
  [url threads mode]
  (let [shared (when (= mode :manual) (solr/connect url nil {:cache-client? false}))
        client #(or shared (solr/connect url))
        done   (AtomicInteger.)
        failed (AtomicInteger.)
        start  (CountDownLatch. 1)
        latch  (CountDownLatch. threads)
        ws (doall
            (for [i (range threads)]
              (doto (Thread.
                     (fn []
                       (try
                         (.await start)
                         (solr/with-connection (client)
                           (solr/search "*:*" :rows 1))
                         (.incrementAndGet done)
                         (catch Throwable t
                           (.incrementAndGet failed)
                           (println (format "    ! %s: %s"
                                            (.getSimpleName (class t))
                                            (let [m (.getMessage t)]
                                              (if m (subs m 0 (min 70 (count m))) "no message")))))
                         (finally (.countDown latch))))
                     (str "worker-" i))
                (.setDaemon true)
                (.start))))
        ;; The scope that does the damage: it borrows the same client, does no
        ;; work at all, and exits while every worker is still in flight.
        closer (doto (Thread.
                      (fn []
                        (.await start)
                        (Thread/sleep 200)
                        (solr/with-connection (client) :nothing))
                      "closer")
                 (.setDaemon true)
                 (.start))]
    (.countDown start)
    (let [finished (.await latch 25 TimeUnit/SECONDS)
          ;; A worker counts the latch down in its finally and then still has to
          ;; return, so give it a moment before asking whether it is alive --
          ;; otherwise a thread on its way out reads as parked.
          _ (doseq [^Thread t ws] (.join t 250))
          alive (filter #(.isAlive ^Thread %) ws)
          parked (count alive)]
      (println (format "  %-8s completed=%d failed=%d parked=%d %s"
                       (name mode) (.get done) (.get failed) parked
                       (if finished "" "*** WORKERS PARKED FOREVER ***")))
      (when-let [^Thread t (first alive)]
        (println "        one parked worker:")
        (doseq [f (take 4 (.getStackTrace t))] (println "          at" f)))
      (solr/close-cached-connections!)
      {:mode mode :done (.get done) :failed (.get failed) :parked parked})))

(defmacro old-app-with-connection
  "i2kconduit-db's with-connection as it stood before its a622059, verbatim in
   shape: the app cached one client per URL in a ref of its own and handed the
   same instance to every worker, and each scope closed it unless clojure-solr
   called it shared -- which, before this change, it never did for an HTTP
   client."
  [conn & body]
  `(binding [solr/*connection* ~conn]
     (try ~@body
          (finally
            (when-not (solr/shared? solr/*connection*)
              (solr/drain solr/*connection*)
              (.close ^org.apache.solr.client.solrj.SolrClient solr/*connection*))))))

(defn run-old-app
  "The deployed shape: the application shares one client across workers and each
   scope closes it unless shared? says otherwise.  cached? chooses whether the
   shared client came from a connect that clojure-solr caches."
  [url threads cached?]
  (let [shared (if cached?
                 (solr/connect url)
                 (solr/connect url nil {:cache-client? false}))
        done   (AtomicInteger.)
        failed (AtomicInteger.)
        start  (CountDownLatch. 1)
        latch  (CountDownLatch. threads)
        ws (doall
            (for [i (range threads)]
              (doto (Thread.
                     (fn []
                       (try
                         (.await start)
                         (old-app-with-connection shared (solr/search "*:*" :rows 1))
                         (.incrementAndGet done)
                         (catch Throwable t (.incrementAndGet failed))
                         (finally (.countDown latch))))
                     (str "old-" i))
                (.setDaemon true) (.start))))]
    (doto (Thread. (fn [] (.await start) (Thread/sleep 200)
                     (old-app-with-connection shared :nothing))
                   "old-closer")
      (.setDaemon true) (.start))
    (.countDown start)
    (let [finished (.await latch 25 TimeUnit/SECONDS)
          _ (doseq [^Thread t ws] (.join t 250))
          parked (count (filter #(.isAlive ^Thread %) ws))]
      (println (format "  shared client %-22s shared?=%-5s completed=%d failed=%d parked=%d %s"
                       (if cached? "from connect (cached)" "built with :cache-client? false")
                       (solr/shared? shared) (.get done) (.get failed) parked
                       (if finished "" "*** PARKED FOREVER ***")))
      (solr/close-cached-connections!))))

(defn run-latency
  "Single-threaded per-request latency.  A reused connection that is slower than
   a fresh one points at the transport, not at the cache."
  [label url opts n method]
  (solr/set-default-method! method)
  (dotimes [_ 20] (solr/with-connection (solr/connect url nil opts) (solr/search "*:*" :rows 1)))
  (let [samples (vec (sort (for [_ (range n)]
                             (let [t0 (System/nanoTime)]
                               (solr/with-connection (solr/connect url nil opts)
                                 (solr/search "*:*" :rows 1))
                               (/ (- (System/nanoTime) t0) 1000000.0)))))
        pct (fn [p] (nth samples (min (dec n) (int (* p n)))))]
    (println (format "  %-22s %-5s n=%d  min=%.1f  p50=%.1f  p95=%.1f  max=%.1f ms"
                     label (name method) n (first samples) (pct 0.5) (pct 0.95) (last samples)))
    (solr/close-cached-connections!)))

(defn -main [& args]
  (let [scenario (or (first args) "all")
        threads  (Integer/parseInt (or (second args) "8"))
        iters    (Integer/parseInt (or (nth args 2 nil) "250"))
        {:keys [server port hits peak conns]} (start-server
                                     (if (#{"sharedclose" "oldapp"} scenario) 600 0))
        url      (str "http://127.0.0.1:" port "/solr/stress")
        warm     (fn [opts]
                   ;; One pass to pay for class loading, the h2c upgrade probe and
                   ;; JIT, so the measured pass is steady state rather than startup.
                   (dotimes [_ 20]
                     (solr/with-connection (solr/connect url nil opts)
                       (solr/search "*:*" :rows 1)))
                   (solr/close-cached-connections!))]
    (println (format "java %s   solrj %s   scenario=%s   %d threads x %d iterations   http1=%s"
                     (System/getProperty "java.version") (solr/get-solr-version)
                     scenario threads iters (System/getProperty "solr.http1" "false")))

    (case scenario
      "nocache" (do (warm {:cache-client? false})
                    (run-load "no cache" url {:cache-client? false} threads iters))
      "cached"  (do (warm nil)
                    (run-load "cached" url nil threads iters))
      "race"      (run-cache-race #(str "http://127.0.0.1:" port "/solr/target-" %) 4 threads 200)
      "closerace" (run-close-race url threads 400)
      "compare" (do (println "  -- same JVM, same cached client, two drivers --")
                    (run-latency "main thread" url nil 200 :post)
                    (run-load "spawned thread" url nil 1 200)
                    (run-latency "main thread again" url nil 200 :post))
      "many" (let [before (snapshot)]
               ;; What one process pays to hold N distinct targets open: the
               ;; cache has no eviction, so this is the standing cost of a
               ;; consumer that talks to many collections.
               (doseq [k (range iters)]
                 (solr/with-connection (solr/connect (str url "-" k))
                   (solr/search "*:*" :rows 1)))
               (let [after (snapshot)
                     d (delta before after)]
                 (println (format "  %d cached clients  delta: %s" iters (pr-str d)))
                 (println (format "  per client: %.2f fds, %.2f sockets, %.2f eventpoll, %.2f threads"
                                  (double (/ (:fds d) iters)) (double (/ (:sockets d) iters))
                                  (double (/ (:eventpoll d) iters)) (double (/ (:threads d) iters))))))
      "sharedclose" (do (println "  one slow operation per worker, all overlapping")
                        (run-shared-close url threads :manual)
                        (run-shared-close url threads :cached))
      "oldapp" (do (println "  i2kconduit-db's pre-a622059 with-connection, over a shared client")
                   (run-old-app url threads false)
                   (run-old-app url threads true))
      "latency" (doseq [m [:post :get]]
                  (run-latency "fresh client" url {:cache-client? false} 200 m)
                  (run-latency "cached client" url nil 200 m))
      "all" (do (warm {:cache-client? false})
                (run-load "no cache" url {:cache-client? false} threads iters)
                (solr/close-cached-connections!)
                (warm nil)
                (run-load "cached" url nil threads iters)
                (run-cache-race #(str "http://127.0.0.1:" port "/solr/target-" %) 4 threads 200)
                (run-close-race url threads 400)))

    (solr/close-cached-connections!)
    (println (format "server saw %d requests over %d connections, peak concurrency %d"
                     (.get hits) (count conns) (.get peak)))
    (.close server)
    (shutdown-agents)
    (flush)
    ;; Nothing here is worth waiting on: the JDK client's selector threads would
    ;; otherwise hold the JVM open.
    (System/exit 0)))
