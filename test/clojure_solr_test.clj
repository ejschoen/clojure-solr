(ns clojure-solr-test
  (:require [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.jar Manifest))
  (:import (org.apache.solr.client.solrj.embedded EmbeddedSolrServer))
  (:import (org.apache.solr.core CoreContainer))
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as tcoerce])
  (:require [cheshire.core :as cheshire])
  (:use [clojure.test])
  (:use [clojure-solr])
  (:use [clojure-solr.security])
  (:use [clojure-solr.schema])
  (:import [java.nio.charset StandardCharsets]
           [org.apache.commons.codec.binary Base64]))

(defmethod make-solr-client EmbeddedSolrServer [_  _ major-version solr-client-options]
  (let [cont-expr (case major-version
                    (6 7) `(CoreContainer.)
                    8 (let [home-dir (:home-dir solr-client-options)]
                        `(CoreContainer. (.getPath (java.nio.file.FileSystems/getDefault)
                                                   ~home-dir
                                                   (make-array String 0))
                                         (doto (java.util.Properties.)
                                           (.setProperty "solr.dist.dir"
                                                         (str (System/getProperty "user.dir")
                                                              "/test-files/dist"))))))
        ^CoreContainer container (doto (eval cont-expr) (.load))]
    (EmbeddedSolrServer. container (:core solr-client-options))
    ))


(defn get-solr-home-dir
  []
  (let [version (get-solr-version)]
    (str "test-files/solr-" (second (re-matches #"(\d+)\..*" version)))))

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
  (let [home-dir (get-solr-home-dir)]
    (delete-recursively (str home-dir "/data"))
    (System/setProperty "solr.solr.home" home-dir)
    (System/setProperty "solr.dist.dir" (str (System/getProperty "user.dir")
                                             "/test-files/dist"))
    (let [[_ major minor :as version] (re-matches #"(\d+)\.(\d+)\..*" (get-solr-version))
          major (Integer/parseInt major)
          minor (Integer/parseInt minor)]
      (println (format "This is solr version %s" (first version)))
      (when (and (= major 7) (>= minor 4))
        ;; https://issues.apache.org/jira/browse/SOLR-12858
        (println "Using get as default method due to issue SOLR-12858")
        (set-default-method! :get)
        ))
    (with-connection (connect nil nil 
                              {:type EmbeddedSolrServer
                               :core "clojure-solr"
                               :home-dir (get-solr-home-dir)}) 
      (f))))

(use-fixtures :each solr-server-fixture)

(def sample-doc
  {:id "1" :type "pdf" :title "my title" :fulltext "my fulltext" :numeric 10
   :updated (tcoerce/to-date (t/date-time 2015 02 27))
   :terms ["Vocabulary 1/Term A" "Vocabulary 1/Term B" "Vocabulary 2/Term X/Term Y"]})

(deftest test-add-doc-with-lazyseq
  (add-document! {:id 2 :type "pdf" :related_s_mv (cheshire/parse-string "[\"abc\",\"def\"]")})
  (commit!)
  (let [result (first (search "*" :facet-filters [{:name "id" :value "2"}] :df "fulltext"))]
    (is result)
    (is (vector? (:related_s_mv result)))
    (is (= #{"abc" "def"} (set (:related_s_mv result)))))
  )

(deftest test-add-document!
  (do (add-document! sample-doc)
      (commit!))
  (is (= sample-doc (dissoc (first (search "my" :df "fulltext")) :_version_ :word)))
  (is (= {:start 0 :rows-set 1 :rows-total 1} (select-keys (meta (search "my"  :df "fulltext"))
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
           (meta (search "my" :facet-fields [:terms] :facet-hier-sep #"/"  :df "fulltext"))))))

(deftest test-update-document!
  (do (add-document! sample-doc)
      (commit!))
  (atomically-update! 1 :id [{:attribute :title :func :set :value "my new title"}])
  (commit!)
  (let [search-result (search "my"  :df "fulltext")]
    (is (= (get (first search-result) :title) "my new title"))))
  

(deftest test-quoted-search
  (do (add-document! sample-doc)
      (commit!))
  (is (= sample-doc (dissoc (first (search "\"my fulltext\""  :df "fulltext")) :_version_ :word)))
  (is (empty? (search "\"fulltext my\""  :df "fulltext"))))

(deftest test-quoted-search-mw
  (do (add-document! sample-doc)
      (commit!))
  (is (= sample-doc (dissoc (first (search*-with-middleware "\"my fulltext\""  {:df "fulltext"})) :_version_ :word)))
  (is (empty? (search*-with-middleware "\"fulltext my\""  {:df "fulltext"}))))

(deftest test-facet-query
  (let [query->map (fn [s]
                     (let [params (str/split s #"&")]
                       (into {} (map (fn [pair] (str/split pair #"=")) params))))]
    (do (add-document! sample-doc)
        (commit!))
    (is (= [{:name "terms" :value "Vocabulary 1" :count 1}]
           (:facet-queries (meta (search "my" :facet-queries [{:name "terms" :value "Vocabulary 1"}] :df "fulltext")))))
    (is (= (query->map "q=my&df=fulltext&facet-queries={:name+\"terms\",+:value+\"Vocabulary+1\"}&facet.query={!raw+f%3Dterms}Vocabulary+1&facet=true&facet.mincount=1")
           (query->map (search "my" :just-return-query? true :facet-queries [{:name "terms" :value "Vocabulary 1"}] :df "fulltext"))))))

(deftest test-facet-prefix
  (do (add-document! sample-doc)
      (add-document! (assoc sample-doc :id "2" :numeric 11))
      (add-document! (assoc sample-doc :id "3" :numeric 11))
      (add-document! (assoc sample-doc :id "4" :numeric 15))
      (add-document! (assoc sample-doc :id "5" :numeric 8))
      (commit!))
  (let [result (meta (search "my"
                             :facet-fields [{:name "terms" :prefix "Voc"}]
                             :df "fulltext"))]
    (is (not (empty? (:facet-fields result)))))
  (let [result (meta (search "my"
                             :facet-fields [{:name "terms" :prefix "Vocabulary 1"}]
                             :df "fulltext"))]
    (is (not (empty? (:facet-fields result))))
    (is (= 3 (count (-> result :facet-fields first :values))))
    (is (every? #(.startsWith (:value %) "Vocabulary 1")
                (-> result :facet-fields first :values))))
  (let [result (meta (search "my"
                             :facet-fields [{:name "terms" :prefix "Vocabulary 1"
                                             :result-formatter #(update-in % [:value] clojure.string/lower-case)}]
                             :df "fulltext"))]
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
                               :others   ["before" "after"]}]
                              :df "fulltext"))]
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
                             :df "fulltext"
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
  (binding [*qf* "fulltext"]
    (let [fields (get-fields-via-luke)]
      (is (not-empty fields))
      (is (map? (get fields "fulltext")))
      (is (set? (get-in fields ["fulltext" :schema]))))))

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
                     :df "fulltext"
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
                     :df "fulltext"
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
                                       :df "fulltext"
                                       :debugQuery true
                                       :defType "edismax"
                                       :facet-filters [{:name "complex" :value "(source:SemanticScholar%20Commercial%20Use%20Subset AND type:application/json;schema=semantic-scholar)"
                                                        :formatter (fn [_ value] value)}]
                                       :facet-fields [{:name "type" :ex "type"}]))))

(deftest test-make-security-json-data
  (let [data (make-security-data [{:user "i2kweb" :password nil :role "query"}
                                  {:user "i2kconduit-db" :role "upload"}]
                                 [{:name "read" :role "*"}
                                  {:name "schema-read" :role "*"}
                                  {:name "update" :role "upload"}
                                  {:name "health" :role "health"
                                   :path "/admin/info/system"
                                   :methods ["GET"]
                                   :collection ""}])]
    (is (:credentials data))
    (is (:authorization data))
    (is (:authentication data))
    (is (= (last (get-in data [:authorization :permissions]))
           {:role "health"
            :name "health"
            :methods ["GET"]
            :path "/admin/info/system"
            :collection nil}))
    (is (get-in data [:credentials "i2kweb" :hashed-password]))
    (is (get-in data [:credentials "i2kweb" :salt]))
    (is (get-in data [:credentials "i2kweb" :cleartext-password]))
    (is (= (get-in data [:credentials "i2kweb" :hashed-password])
           (:hashed-password
            (generate-salted-hash
             (.getBytes (get-in data [:credentials "i2kweb" :cleartext-password])
                        StandardCharsets/UTF_8)
             (Base64/decodeBase64
              (get-in data [:credentials "i2kweb" :salt]))))))))

(deftest test-delete-doc-by-id
  (add-document! sample-doc)
  (commit!)
  (let [update-response (delete-id! "1")
        status (.getStatus update-response)]
    (is (= status 0))))

(deftest test-delete-doc-by-query
  (add-document! sample-doc)
  (commit!)
  (let [update-response (delete-query! "title:\"my title\"")
        status (.getStatus update-response)
        ]
    (is (= status 0))))

(deftest test-query-boolean-filter
  (add-document! {:id 1 :boolean_b true :type "pdf"})
  (commit!)
  (is (= (count (search "*:*" :defType "edismax" :qf "title fulltext")) 1))
  (is (= 1 (some #(and (= (:value %) "true")
                       (:count %))
                 (:values
                  (first
                   (:facet-fields
                    (meta
                     (search "*:*" :defType "edismax" :qf "title fulltext" :facet-fields ["boolean_b"]))))))))
  (is (= (count (search "*:*"
                        :facet-filters [{:name "boolean_b"
                                               :value "true"
                                               :formatter format-standard-filter-query}]
                        :qf "title fulltext"
                        :defType "edismax"))
         1))

  ;; raw queries against boolean values apparently don't work correctly.
  #_(is (= (count (search "*:*" :facet-filters [{:name "boolean_b"
                                               :value "true"
                                               :formatter format-raw-query}]
                        :qf "title fulltext"
                        ))
         1))  
  #_(is (= (count (search "*:*" :facet-filters [{:name "boolean_b"
                                               :value "true"
                                               :formatter format-raw-query}]
                        :qf "title fulltext"
                        :defType "edismax"))
         1))
  #_(is (= (count (search "*:*" :facet-filters [{:name "boolean_b"
                                               :value "TRUE"
                                               :formatter format-raw-query}]
                        :qf "title fulltext"
                        :defType "edismax"))
         1))
  (clojure.pprint/pprint (meta (search "*:*" :facet-filters [{:name "boolean_b"
                                                              :value "TRUE"
                                                              :formatter format-raw-query}]
                                       :debugQuery true
                                       :qf "title fulltext"
                                       :defType "edismax")))
  (clojure.pprint/pprint (meta (search "*:*" :facet-filters [{:name "boolean_b"
                                                              :value true
                                                              :formatter format-raw-query}]
                                       :debugQuery true
                                       :qf "title fulltext"
                                       :defType "edismax")))
  (clojure.pprint/pprint (first (search "*:*" :facet-filters [{:name "boolean_b"
                                                              :value true
                                                              :formatter format-standard-filter-query}]
                                       :debugQuery true
                                       :qf "title fulltext"
                                       :defType "edismax")))
  (clojure.pprint/pprint (meta (search "*:*" :facet-filters [{:name "boolean_b"
                                                              :value true
                                                              :formatter format-standard-filter-query}]
                                       :debugQuery true
                                       :qf "title fulltext"
                                       :defType "edismax"))))

(deftest test-spellchecker
  (doseq [i (range 10)]
    (add-document! {:id i :type "Web Page"
                    :fulltext "Many of the parameters relate to how this spell checker should query the index for term suggestions. The distanceMeasure defines the metric to use during the spell check query. The value \"internal\" uses the default Levenshtein metric, which is the same metric used with the other spell checker implementations."}))
  (commit!)
  (let [result (search* "Leven" {:df "fulltext" :request-handler "/suggest"} (wrap-suggest solr-app))
        suggestion (:term (first (:suggestions (meta result))))]
    (println (format "Best suggestion for \"Leven\": %s" suggestion))
    (is (= suggestion "Levenshtein")))


  (let [result (search* "Leven"
                        {:df "fulltext" :request-handler "/suggest"
                         :suggest.buildAll true
                         }
                        (-> solr-app
                            (wrap-suggest :suggester-name ["suggest" "context_suggest"])))
        suggestions (:suggestions (meta result))]
    (clojure.pprint/pprint suggestions))

  (let [result (search* "Levenstein" {:df "fulltext" :request-handler "/spell"} (wrap-spellcheck solr-app))
        spellcheck (:spellcheck (meta result))]
    (println (format "Corrected Levenstein to %s" (:collated-result spellcheck)))
    (is (= {:collated-result "Levenshtein" 
            :is-correctly-spelled? false
            :alternatives {"Levenstein" {:num-found 1 :original-frequency 0 :start-offset 0 :end-offset 10
                                         :alternatives ["Levenshtein"]
                                         :alternative-frequencies [10]}}}
           spellcheck)))
  (let [result (search* "Levenshte*" {:df "fulltext" :request-handler "/select-with-spell-and-suggest"}
                        (-> solr-app wrap-spellcheck wrap-suggest))
        suggestions (:suggestions (meta result))
        spellcheck (:spellcheck (meta result))]
    (is (= 10 (count result)))
    (is (not-empty suggestions))
    (is spellcheck))
  )

(deftest test-spellchecker-Volve-files
  (let [docs (with-open [s (io/input-stream "test/resources/Volve/docs.json")
                         reader (io/reader s)]
               (doall (cheshire/parse-stream reader true)))
        docs (for [doc docs]
               (assoc doc :fulltext (str/join "/n/n" (map #(str/replace % #"\d+::" "")
                                                          (get doc :pagetext)))))]
    (doseq [doc docs] (add-document! doc))
  (commit!)
  (is (= 2 (count docs)))
  (is (every? (fn [d] (not-empty (:fulltext d))) docs))
  (is (= 2 (count (search* "Equinor" {:df "fulltext"}))))
  (when (not (is (= 1 (count (search* "Equinor" {:df "word"})))))
    (let [edocs (search* "Equinor" {:df "word"})]
      (doseq [doc edocs]
        (println (format "Found Equinor in doc id %s" (:id doc)))))
    )
  (let [result (search* "Equiner" {:df "fulltext" :request-handler "/spell"} (wrap-spellcheck solr-app))
        spellcheck (:spellcheck (meta result))]
    (println (format "Corrected Equiner to %s" (:collated-result spellcheck)))
    (is (empty? result))
    (is (= {:collated-result "Equinor"
            :is-correctly-spelled? false
            :alternatives {"Equiner"
                           {:num-found 1
                            :original-frequency 0
                            :start-offset 0
                            :end-offset 7
                            :alternatives ["Equinor"]
                            :alternative-frequencies [1]}}}
           spellcheck)))
  (let [result (search* "Equiner" {:df "pagetext" :request-handler "/spell-mv"} (wrap-spellcheck solr-app))
        spellcheck (:spellcheck (meta result))]
    (println (format "Corrected Equiner to %s" (:collated-result spellcheck)))
    (is (empty? result))
    (is (= {:collated-result "Equinor"
            :is-correctly-spelled? false
            :alternatives {"Equiner"
                           {:num-found 1
                            :original-frequency 0
                            :start-offset 0
                            :end-offset 7
                            :alternatives ["Equinor"]
                            :alternative-frequencies [1]}}}
           spellcheck)))
  ))

(deftest test-suggester
  (let [tags_1 ["drilling operation"
                "drilling fluids and materials"
                "drilling fluid chemistry"
                "drilling fluid property"
                "drilling fluid formulation"
                "drilling fluid selection and formulation"
                "drilling equipment"
                "drilling fluid management & disposal"]
        tags_2 ["drillstem testing"
                "drillstem/well testing"]]
    (doseq [i (range 10)]
      (add-document! {:suggestion tags_1
                      :client "drilling"
                      :type "PDF"
                      :id (str "doc" i)}))
    (doseq [i (range 10 20)]
      (add-document! {:suggestion tags_2
                      :client "testing"
                      :type "PDF"
                      :id (str "doc" i)}))
    (commit!)
    (let [result (search* "dril"
                          {:df "fulltext" :request-handler "/suggest"
                           :suggest.cfq "drilling"
                           :suggest.build true}
                          (-> solr-app
                              (wrap-suggest :suggester-name "context_suggest_mv")))
          suggestions (map :term (:suggestions (meta result)))]
      (is (= suggestions (map :term '({:term "drilling equipment", :weight 0}
                                      {:term "drilling fluid chemistry", :weight 0}
                                      {:term "drilling fluid formulation", :weight 0}
                                      {:term "drilling fluid management & disposal", :weight 0}
                                      {:term "drilling fluid property", :weight 0}
                                      {:term "drilling fluid selection and formulation", :weight 0}
                                      {:term "drilling fluids and materials", :weight 0}
                                      {:term "drilling operation", :weight 0}))))
      #_(clojure.pprint/pprint (meta result)))
    (let [result (search* "dril"
                          {:df "fulltext" :request-handler "/suggest"
                           :suggest.cfq "testing"
                           :suggest.build true}
                          (-> solr-app
                              (wrap-suggest :suggester-name "context_suggest_mv")))
          suggestions (map :term (:suggestions (meta result)))]
      (is (= suggestions (map :term '({:term "drillstem testing", :weight 0}
                                      {:term "drillstem/well testing", :weight 0}))))
      #_(clojure.pprint/pprint (meta result)))
    (let [result (search* "dril"
                          {:df "fulltext" :request-handler "/suggest"
                           :suggest.cfg "testing OR drilling"
                           :suggest.build true}
                          (-> solr-app
                              (wrap-suggest :suggester-name "context_suggest_mv")))
          suggestions (map :term (:suggestions (meta result)))]
      (is (= suggestions (map :term '({:term "drilling equipment", :weight 0}
                                      {:term "drilling fluid chemistry", :weight 0}
                                      {:term "drilling fluid formulation", :weight 0}
                                      {:term "drilling fluid management & disposal", :weight 0}
                                      {:term "drilling fluid property", :weight 0}
                                      {:term "drilling fluid selection and formulation", :weight 0}
                                      {:term "drilling fluids and materials", :weight 0}
                                      {:term "drilling operation", :weight 0}
                                      {:term "drillstem testing", :weight 0}
                                      {:term "drillstem/well testing", :weight 0}))))
      #_(clojure.pprint/pprint (meta result)))))


  
(deftest test-id-number-search
  (add-document! {:id "doc0"
                  :type "LAS File"
                  :pagetext ["UWI . 12-345-67890  : UWI or API of well"
                             "API . 0987654321    : API 10"
                             "API . 314159265358  : API 12"
                             "API . 27-182-818284590 : API 14 "]})
  
  (commit!)
  (are [match-count query] (= match-count (count (search* query {:df "pagetext"})))
    1 "1234567890"        ;; Finds text with embedded dashes
    0 "09-876-54321"      ;; Fails to find text without embedded dashes if we search w/ dashes.
    1 "0987654321"        ;; Matches literal text.
    1 "27182818284590"
    1 "27182818284*"      ;; Wildcards work inside tokens.
    1 "12-345-67890"      ;; Matches dashed text if we search with dashed text.
    )
  (is (= 1  (count (search* "1234567890" {:df "pagetext"})) ))
  )

(deftest test-cheap-date-math-parser
  (let [now (t/now)]
    (are [out in] (= out (cheap-date-math-parser now in))
      (-> now (t/floor t/day) (t/plus (t/months 6)) (t/plus (t/days 3))) "/DAY+6MONTHS+3DAYS"
      (-> now (t/plus (t/months 6)) (t/plus (t/days 3)) (t/floor t/day)) "+6MONTHS+3DAYS/DAY"
      (-> now (t/minus (t/days 1))) "-1DAY"
      (-> now (t/plus (t/years 2))) "+2YEARS"
      (-> now (t/floor t/hour)) "/HOUR")))
