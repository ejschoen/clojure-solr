(ns clojure-solr-test
  (:require [clojure.pprint])
  (:import (org.apache.solr.client.solrj.embedded EmbeddedSolrServer))
  (:import (org.apache.solr.core CoreContainer))
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as tcoerce])
  (:use [clojure.test])
  (:use [clojure-solr])
  (:use [clojure-solr.schema]))

;; from: https://gist.github.com/edw/5128978
(defn delete-recursively [fname]
  (let [func (fn [func f]
               (when (.isDirectory f)
                 (doseq [f2 (.listFiles f)]
                   (func func f2)))
               (try (clojure.java.io/delete-file f) (catch Exception _)))]
    (func func (clojure.java.io/file fname))))

(defn solr-server-fixture
  [f]
  (delete-recursively "test-files/data")
  (System/setProperty "solr.solr.home" "test-files")
  (System/setProperty "solr.dist.dir" (str (System/getProperty "user.dir")
                                           "/test-files/dist"))
  (let [cont (CoreContainer.)]
    (.load cont)
    (binding [*connection* (EmbeddedSolrServer. cont "clojure-solr")]
      (f)
      (.close *connection*))))

(use-fixtures :each solr-server-fixture)

(def sample-doc
  {:id "1" :type "pdf" :title "my title" :fulltext "my fulltext" :numeric 10
   :updated (tcoerce/to-date (t/date-time 2015 02 27))
   :terms ["Vocabulary 1/Term A" "Vocabulary 1/Term B" "Vocabulary 2/Term X/Term Y"]})

(deftest test-add-document!
  (do (add-document! sample-doc)
      (commit!))
  (is (= sample-doc (dissoc (first (search "my")) :_version_)))
  (is (= {:start 0 :rows-set 1 :rows-total 1} (select-keys (meta (search "my"))
                                                           [:start :rows-set :rows-total])))
  (is (= [{:name "terms"
           :values
           [{:value "Vocabulary 1" :split-path ["Vocabulary 1"] :title "Vocabulary 1" :depth 1 :count 1}
            {:value "Vocabulary 1/Term A" :split-path ["Vocabulary 1" "Term A"] :title "Term A" :depth 2 :count 1}
            {:value "Vocabulary 1/Term B" :split-path ["Vocabulary 1" "Term B"] :title "Term B" :depth 2 :count 1}
            {:value "Vocabulary 2" :split-path ["Vocabulary 2"] :title "Vocabulary 2" :depth 1 :count 1}
            {:value "Vocabulary 2/Term X" :split-path ["Vocabulary 2" "Term X"] :title "Term X" :depth 2 :count 1}
            {:value "Vocabulary 2/Term X/Term Y" :split-path ["Vocabulary 2" "Term X" "Term Y"] :title "Term Y" :depth 3 :count 1}]}]
         (:facet-fields
           (meta (search "my" :facet-fields [:terms] :facet-hier-sep #"/"))))))

(deftest test-update-document!
  (do (add-document! sample-doc)
      (commit!))
  (atomically-update! 1 :id [{:attribute :title :func :set :value "my new title"}])
  (commit!)
  (let [search-result (search "my")]
    (is (= (get (first search-result) :title) "my new title"))))
  

(deftest test-quoted-search
  (do (add-document! sample-doc)
      (commit!))
  (is (= sample-doc (dissoc (first (search "\"my fulltext\"")) :_version_)))
  (is (empty? (search "\"fulltext my\""))))

(deftest test-facet-query
  (do (add-document! sample-doc)
      (commit!))
  (is (= [{:name "terms" :value "Vocabulary 1" :count 1}]
         (:facet-queries (meta (search "my" :facet-queries [{:name "terms" :value "Vocabulary 1"}]))))))

(deftest test-facet-prefix
  (do (add-document! sample-doc)
      (add-document! (assoc sample-doc :id "2" :numeric 11))
      (add-document! (assoc sample-doc :id "3" :numeric 11))
      (add-document! (assoc sample-doc :id "4" :numeric 15))
      (add-document! (assoc sample-doc :id "5" :numeric 8))
      (commit!))
  (let [result (meta (search "my"
                             :facet-fields [{:name "terms" :prefix "Voc"}]))]
    (is (not (empty? (:facet-fields result)))))
  (let [result (meta (search "my"
                             :facet-fields [{:name "terms" :prefix "Vocabulary 1"}]))]
    (is (not (empty? (:facet-fields result))))
    (is (= 3 (count (-> result :facet-fields first :values))))
    (is (every? #(.startsWith (:value %) "Vocabulary 1")
                (-> result :facet-fields first :values))))
  (let [result (meta (search "my"
                             :facet-fields [{:name "terms" :prefix "Vocabulary 1"
                                             :result-formatter #(update-in % [:value] clojure.string/lower-case)}]))]
    (is (not (empty? (:facet-fields result))))
    (is (= 3 (count (-> result :facet-fields first :values))))
    (is (every? #(.startsWith (:value %) "vocabulary 1")
                (-> result :facet-fields first :values)))))

(deftest test-facet-ranges
  (do (add-document! sample-doc)
      (add-document! (assoc sample-doc :id "2" :numeric 11))
      (add-document! (assoc sample-doc :id "3" :numeric 11))
      (add-document! (assoc sample-doc :id "4" :numeric 15))
      (add-document! (assoc sample-doc :id "5" :numeric 8))
      (commit!))
  (let [result (meta (search "my"
                             :facet-numeric-ranges
                             [{:field   "numeric"
                               :start   (Integer. 9)
                               :end     (Integer. 12)
                               :gap     (Integer. 3)
                               :others  ["before" "after"]
                               :include "lower"
                               :hardend false}]
                             :facet-date-ranges
                             [{:field    "updated"
                               :start    (tcoerce/to-date (t/from-time-zone (t/date-time 2015 02 26)
                                                                            (t/time-zone-for-id "America/Chicago")))
                               :end      (tcoerce/to-date (t/from-time-zone (t/date-time 2015 02 28)
                                                                            (t/time-zone-for-id "America/Chicago")))
                               :gap      "+1DAY"
                               :timezone (t/time-zone-for-id "America/Chicago")
                               :others   ["before" "after"]}]))]
    (is (= {:name "numeric",
            :values
            [{:count 1,
              :value "[* TO 9}",
              :min-inclusive nil,
              :max-noninclusive 9}
             {:max-noninclusive 12,
              :min-inclusive 9,
              :value "[9 TO 12}",
              :count 3}
             {:count 1,
              :value "[12 TO *]",
              :min-inclusive 12,
              :max-noninclusive nil}],
            :start 9,
            :end 12,
            :gap 3,
            :before 1,
            :after 1}
           (some #(and (= (:name %) "numeric") %) (:facet-range-fields result))))
    (is (= {:name   "updated"
            :values [{:min-inclusive    (tcoerce/to-date "2015-02-26T06:00:00Z")
                      :max-noninclusive (tcoerce/to-date "2015-02-27T06:00:00Z")
                      :value            "[2015-02-26T06:00:00Z TO 2015-02-27T06:00:00Z}",
                      :count            5}]
            :start  (tcoerce/to-date (t/from-time-zone (t/date-time 2015 02 26)
                                                       (t/time-zone-for-id "America/Chicago")))
            :end    (tcoerce/to-date (t/from-time-zone (t/date-time 2015 02 28)
                                                       (t/time-zone-for-id "America/Chicago")))
            :gap    "+1DAY"
            :before 0
            :after  0}
           (first (filter #(= (:name %) "updated") (:facet-range-fields result)))))))


(deftest test-pivot-faceting
  (add-document! sample-doc)
  (add-document! (assoc sample-doc :id 2 :type "docx"))
  (commit!)
  (let [result (meta (search "my"
                             :rows 0
                             :facet-date-ranges
                             [{:field    "updated"
                               :tag      "ts"
                               :start    (tcoerce/to-date (t/from-time-zone (t/date-time 2015 02 26)
                                                                            (t/time-zone-for-id "America/Chicago")))
                               :end      (tcoerce/to-date (t/from-time-zone (t/date-time 2015 02 28)
                                                                            (t/time-zone-for-id "America/Chicago")))
                               :gap      "+1DAY"
                               :timezone (t/time-zone-for-id "America/Chicago")
                               :others   ["before" "after"]}]
                             :facet-pivot-fields ["{!range=ts}type"]))
        pivot-fields (:facet-pivot-fields result)]
    (is (= 1 (count pivot-fields)))
    (is (get pivot-fields "type"))
    (is (= 2 (count (get pivot-fields "type"))))
    (is (= 1 (count (get-in pivot-fields ["type" :ranges "docx" "updated"]))))
    (is (= 1 (:count (first (get-in pivot-fields ["type" :ranges "docx" "updated"])))))
    (is (= 1 (count (get-in pivot-fields ["type" :ranges "pdf" "updated"]))))
    (is (= 1 (:count (first (get-in pivot-fields ["type" :ranges "pdf" "updated"])))))
    #_(clojure.pprint/pprint (:facet-pivot-fields result))))



(deftest test-luke-schema
  (add-document! sample-doc)
  (add-document! (assoc sample-doc :id 2 :type "docx"))
  (commit!)
  (let [fields (get-fields-via-luke)]
    (is (not-empty fields))
    (is (map? (get fields "fulltext")))
    (is (set? (get-in fields ["fulltext" :schema])))))

(deftest test-edismax-disjunction
  (add-document! (assoc sample-doc :id 1 :fulltext "This is a clinical trial."))
  (add-document! (assoc sample-doc :id 2 :fulltext "This is a clinical study."))
  (add-document! (assoc sample-doc :id 3 :fulltext "This is a clinical trial and a clinical study."))
  (commit!)
  (let [ct (search* "\"clinical trial\"" {:df "fulltext" :defType "edismax" :fl "id"})
        cs (search* "\"clinical study\"" {:df "fulltext" :defType "edismax" :fl "id"})
        cts (search* "(\"clinical trial\" OR \"clinical study\")" {:df "fulltext" :defType "edismax" :fl "id" })
        cts-plus (search* "+(\"clinical trial\" OR \"clinical study\")" {:df "fulltext" :defType "edismax" :fl "id" })
        cts3 (search* "+(\"clinical trial\" OR \"clinical study\" OR \"foo baz\")" {:df "fulltext" :defType "edismax" :fl "id"})
        ]
    (is (= 2 (count ct)))
    (is (= #{"1" "3"} (set (map :id ct))))
    (is (= 2 (count cs)))
    (is (= #{"2" "3"} (set (map :id cs))))
    (is (= 3 (count cts)))
    (is (= 3 (count cts-plus)))
    (is (= 3 (count cts3)))
    ))
        
  
(deftest test-exclude-filter-faceting
  (add-document! sample-doc)
  (add-document! (assoc sample-doc :id 2 :type "docx"))
  (commit!)
  (let [docs (search "my"
                     :facet-filters [{:name "type"
                                      :value "pdf"
                                      :full-formatter format-standard-filter-query
                                      :tag "type"}]
                     :facet-fields [{:name "type" :ex "type"}])
        result (meta docs)
        facet-fields (:facet-fields result)]
    (is (= 1 (count docs)))
    (is (= 1 (count facet-fields)))
    (is (some #(= "type" (:name %)) facet-fields))
    (is (= 2 (count (:values (first facet-fields)))))
    (let [type-facet (group-by :value (:values (first facet-fields)))]
      (is (= 1 (:count (first (get type-facet "pdf")))))
      (is (= 1 (:count (first (get type-facet "docx"))))))))

(deftest test-exclude-filter-faceting-complex
  (add-document! sample-doc)
  (add-document! (assoc sample-doc :id 2 :type "docx"))
  (add-document! (assoc sample-doc :id 3 :type "pptx"))
  (commit!)
  (let [docs (search "my"
                     :facet-filters [{:name "type"
                                      :value "{!tag=type}(type:pdf OR type:docx)"
                                      :full-formatter #(:value %)
                                      :tag "type"}]
                     :facet-fields [{:name "type" :ex "type"}])
        result (meta docs)
        facet-fields (:facet-fields result)]
    (is (= 2 (count docs)))
    (is (= 1 (count facet-fields)))
    (is (some #(= "type" (:name %)) facet-fields))
    (is (= 3 (count (:values (first facet-fields)))))
    (let [type-facet (group-by :value (:values (first facet-fields)))]
      (is (= 1 (:count (first (get type-facet "pptx")))))
      (is (= 1 (:count (first (get type-facet "pdf")))))
      (is (= 1 (:count (first (get type-facet "docx"))))))))

(deftest test-solr-npe-from-bad-query
  (clojure.pprint/pprint (meta (search "*:*"
                                       :debugQuery true
                                       :defType "edismax"
                                       :facet-filters [{:name "complex" :value "(source:SemanticScholar%20Commercial%20Use%20Subset AND type:application/json;schema=semantic-scholar)"
                                                        :formatter (fn [_ value] value)}]
                                       :facet-fields [{:name "type" :ex "type"}]))))
