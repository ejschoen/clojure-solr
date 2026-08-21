(ns clojure-solr.admin-test
  "Tests for clojure-solr.admin that need no Solr and no ZooKeeper."
  (:require [clojure.string :as str]
            [clojure.test :refer :all])
  (:import (org.apache.solr.common.cloud SolrZkClient)
           (org.apache.zookeeper Watcher)
           (org.apache.zookeeper.data Stat)))

(def ^:private byte-array-class (Class/forName "[B"))

(defn- public-signatures
  "Every public method of klass as [name [param-type ...]]."
  [^Class klass]
  (into #{}
        (map (fn [^java.lang.reflect.Method m]
               [(.getName m) (vec (.getParameterTypes m))]))
        (.getMethods klass)))

(deftest test-zookeeper-operation-arities
  ;; SolrJ 10 removed the trailing retryOnConnLoss boolean from every
  ;; SolrZkClient operation.  Clojure resolves these calls reflectively, so the
  ;; wrong arity is not a compile error and not a load error -- it surfaces only
  ;; when the call is made, which is how it reached a deployed pod as "No
  ;; matching method getData found taking 4 args for class
  ;; org.apache.solr.common.cloud.SolrZkClient".
  ;;
  ;; This checks the arity clojure-solr.admin will actually invoke against the
  ;; SolrJ on the classpath, without contacting a ZooKeeper.  Required here
  ;; rather than in the ns form so that a SolrJ too old to load the admin
  ;; namespace fails this test alone.
  (require 'clojure-solr.admin)
  (let [signatures (public-signatures SolrZkClient)
        retry?     (deref @(ns-resolve 'clojure-solr.admin 'zk-retry-arg?))
        expected   (if retry?
                     ;; SolrJ 9 and earlier
                     {"makePath"    [String Boolean/TYPE Boolean/TYPE]
                      "setData"     [String byte-array-class Boolean/TYPE]
                      "getData"     [String Watcher Stat Boolean/TYPE]
                      "delete"      [String Integer/TYPE Boolean/TYPE]
                      "getChildren" [String Watcher Boolean/TYPE]
                      "exists"      [String Boolean/TYPE]}
                     ;; SolrJ 10
                     {"makePath"    [String Boolean/TYPE]
                      "setData"     [String byte-array-class]
                      "getData"     [String Watcher Stat]
                      "delete"      [String Integer/TYPE]
                      "getChildren" [String Watcher]
                      "exists"      [String]})]
    (doseq [[op params] expected]
      (is (contains? signatures [op params])
          (format "SolrZkClient.%s(%s) must exist on this SolrJ"
                  op (str/join ", " (map #(.getSimpleName ^Class %) params)))))
    ;; The probe's own premise: exactly one getData arity exists, and it is the
    ;; one the detection concluded.  If a future SolrJ published both, the
    ;; detection would silently keep choosing the Solr 9 call.
    (is (= retry? (contains? signatures ["getData" [String Watcher Stat Boolean/TYPE]]))
        "the four-argument getData exists only where retryOnConnLoss is taken")
    (is (= (not retry?) (contains? signatures ["getData" [String Watcher Stat]]))
        "the three-argument getData exists only where retryOnConnLoss is gone")))
