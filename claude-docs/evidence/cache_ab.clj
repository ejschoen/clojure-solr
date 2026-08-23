(ns cache-ab
  "The cache algorithm alone, old vs new, with no Solr in the picture.

   Invariant: a client that is registered as cache-owned must still be reachable
   from the cache.  One that is registered but unreachable is closed by nobody --
   with-connection refuses because it reports itself shared, and
   close-cached-clients! cannot see it."
  (:import (java.util Collections IdentityHashMap)
           (java.util.concurrent CountDownLatch TimeUnit)))

(defn- new-state []
  {:cache (atom {})
   :owned (Collections/synchronizedSet (Collections/newSetFromMap (IdentityHashMap.)))
   :lock  (Object.)})

;;; ---- the version that shipped in the inherited diff -----------------------

(defn old-get [{:keys [cache owned]} k build]
  (let [d (delay (let [c (build)] (.add owned c) c))
        winner (get (swap! cache (fn [m] (if (contains? m k) m (assoc m k d)))) k)]
    @winner))

(defn old-close! [{:keys [cache owned]}]
  (let [held (loop [] (let [m @cache] (if (compare-and-set! cache m {}) m (recur))))]
    (reduce (fn [n d]
              (if (realized? d)
                (do (.remove owned @d) (inc n))
                n))
            0 (vals held))))

;;; ---- the version now in impl.clj -----------------------------------------

(defn new-get [{:keys [cache owned lock]} k build]
  (or (get @cache k)
      (locking lock
        (or (get @cache k)
            (let [c (build)]
              (.add owned c)
              (swap! cache assoc k c)
              c)))))

(defn new-close! [{:keys [cache owned lock]}]
  (let [held (locking lock
               (let [m @cache]
                 (reset! cache {})
                 (doseq [c (vals m)] (.remove owned c))
                 m))]
    (count held)))

;;; --------------------------------------------------------------------------

(defn race [label get-fn close-fn threads iters]
  (let [st (new-state)
        latch (CountDownLatch. (inc threads))
        stop (atom false)]
    (doto (Thread. (fn []
                     (dotimes [_ 400] (Thread/sleep 2) (close-fn st))
                     (reset! stop true)
                     (.countDown latch)))
      (.setDaemon true) (.start))
    (dotimes [i threads]
      (doto (Thread. (fn []
                       (dotimes [n iters]
                         ;; A build slow enough to overlap a close, which is what
                         ;; a real client construction is.
                         (get-fn st (mod (+ i n) 64)
                                 (fn [] (Thread/sleep 0 200000) (Object.))))
                       (.countDown latch)))
        (.setDaemon true) (.start)))
    (.await latch 120 TimeUnit/SECONDS)
    (close-fn st)
    (let [reachable (count @(:cache st))
          registered (.size (:owned st))]
      (println (format "  %-10s registered=%-5d reachable=%-5d %s"
                       label registered reachable
                       (if (= registered reachable)
                         "consistent"
                         (format "*** %d LEAKED ***" (- registered reachable))))))))

(defn -main [& _]
  (println "racing connect against close, 8 threads x 400, 400 closes")
  (dotimes [_ 3] (race "old" old-get old-close! 8 400))
  (dotimes [_ 3] (race "new" new-get new-close! 8 400))
  (shutdown-agents)
  (System/exit 0))
