(ns clojure-solr.zookeeper-test
  "Round-trip tests for the clojure-solr.admin ZooKeeper helpers, against a
   ZooKeeper started inside this JVM.

   These exist because the helpers reach SolrZkClient reflectively.  An arity
   the running SolrJ does not have is neither a compile error nor a load error:
   it surfaces only when the call is made.  SolrJ 10 dropped the trailing
   retryOnConnLoss argument from every one of these operations, and nothing
   short of an actual call catches that."
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure-solr.admin :as admin])
  (:import (java.net InetSocketAddress)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (org.apache.zookeeper Version)
           (org.apache.zookeeper.server ServerCnxnFactory ZooKeeperServer)))

(def ^:dynamic *zkhost* nil)

(def ^:private timeout 10)

(defn- delete-tree
  "Delete dir and everything under it.  file-seq is top down, so reverse it to
   empty each directory before removing it."
  [^java.io.File dir]
  (doseq [^java.io.File f (reverse (file-seq dir))]
    (.delete f)))

(defn- testable-zookeeper?
  "Whether an in-process ZooKeeper can be reached from this classpath.

   ZooKeeper 3.4's client never finishes a handshake with an in-process 3.4
   server under a current JDK: the socket connects and the session sits in
   CONNECTING until it times out.  The solr7 classifier is the only one that
   still brings 3.4, and there the arity check in clojure-solr.admin-test is the
   cover; solr8, solr9 and solr10 run the round trip below."
  []
  (let [[_ major minor] (re-matches #"(\d+)\.(\d+)\..*" (Version/getVersion))]
    (or (nil? major)
        (not (neg? (compare [(Integer/parseInt major) (Integer/parseInt minor)]
                            [3 5]))))))

(defn zookeeper-fixture
  "Run the tests against a standalone ZooKeeper in this JVM, on a port the OS
   picks.

   Only the three-argument ZooKeeperServer constructor and
   ServerCnxnFactory/createFactory are used, which are the parts of the server
   API that have not changed across the ZooKeeper versions the Solr classifiers
   bring in."
  [f]
  (if-not (testable-zookeeper?)
    (println (format (str "clojure-solr.zookeeper-test: not run -- ZooKeeper %s "
                          "cannot connect to an in-process server on this JDK.")
                     (Version/getVersion)))
    (let [dir (.toFile (Files/createTempDirectory "clojure-solr-zk"
                                                  (make-array FileAttribute 0)))
          server (ZooKeeperServer. (io/file dir "data") (io/file dir "log") 2000)
          factory (ServerCnxnFactory/createFactory (InetSocketAddress. "127.0.0.1" 0) 16)
          ;; The server has no SASL configured, so leaving the client's SASL on
          ;; buys nothing and logs an "Authentication failed" error per attempt.
          sasl (System/setProperty "zookeeper.sasl.client" "false")]
      (try
        (.startup factory server)
        (binding [*zkhost* (str "127.0.0.1:" (.getLocalPort factory))]
          (f))
        (finally
          (if sasl
            (System/setProperty "zookeeper.sasl.client" sasl)
            (System/clearProperty "zookeeper.sasl.client"))
          (.shutdown factory)
          (delete-tree dir))))))

(use-fixtures :once zookeeper-fixture)

(defn- text [^bytes bs] (String. bs "UTF-8"))
(defn- bytes-of [^String s] (.getBytes s "UTF-8"))

(deftest test-zookeeper-round-trip
  (let [parent "/clojure-solr-test"
        path (str parent "/config")]
    (testing "upload creates the node and its missing parent"
      (admin/upload-to-zookeeper *zkhost* path (bytes-of "first") :timeout timeout)
      (is (true? (admin/zk-path-exists? *zkhost* path :timeout timeout)))
      (is (= "first" (text (admin/download-from-zookeeper *zkhost* path
                                                          :timeout timeout)))))
    (testing "re-uploading an existing node overwrites instead of throwing"
      ;; makePath's surviving two-argument overload means retryOnConnLoss on
      ;; SolrJ 9 and failOnExists on SolrJ 10, so an arity arrived at by
      ;; dropping the last argument would raise NodeExistsException here.
      (admin/upload-to-zookeeper *zkhost* path (bytes-of "second") :timeout timeout)
      (is (= "second" (text (admin/download-from-zookeeper *zkhost* path
                                                           :timeout timeout)))))
    (testing "children come back as names, not paths"
      (is (= ["config"] (admin/list-zk-children *zkhost* parent :timeout timeout))))
    (testing "delete at version -1 removes whatever version is there"
      (admin/delete-from-zookeeper *zkhost* path -1 :timeout timeout)
      (is (false? (admin/zk-path-exists? *zkhost* path :timeout timeout)))
      (is (empty? (admin/list-zk-children *zkhost* parent :timeout timeout))))
    (testing "a path that was never created does not exist"
      (is (false? (admin/zk-path-exists? *zkhost* "/no-such-path"
                                         :timeout timeout))))))
