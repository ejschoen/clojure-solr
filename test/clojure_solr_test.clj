(ns clojure-solr-test
  (:require [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.jar Manifest))
  (:import (org.apache.solr.client.solrj.embedded EmbeddedSolrServer)
           (org.apache.solr.client.solrj SolrRequest$METHOD SolrClient))
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

;; An embedded server wraps a process-lifetime CoreContainer, so a nested
;; with-connection must reuse it rather than build and close another.  This is
;; the registration an embedded build performs; clojure-solr never names the class.
(mark-shared! EmbeddedSolrServer)

(defmethod make-solr-client EmbeddedSolrServer [_  _ major-version solr-client-options]
  (let [cont-expr (case major-version
                    (6 7) `(CoreContainer.)
                    (8 9 10) (let [home-dir (:home-dir solr-client-options)]
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

(def test-lock (Object.))

(defn solr-server-fixture
  [f]
  (locking test-lock
    (let [home-dir (get-solr-home-dir)]
      (delete-recursively (str home-dir "/data"))
      (System/setProperty "solr.solr.home" home-dir)
      (System/setProperty "solr.dist.dir" (str (System/getProperty "user.dir")
                                               "/test-files/dist"))
      (let [ulog-dir (str (System/getProperty "user.dir")
                          "/"
                          (get-solr-home-dir)
                          "/data/log")]
        (System/setProperty "solr.ulog.dir" ulog-dir))
      (let [[_ major minor :as version] (re-matches #"(\d+)\.(\d+)\..*" (get-solr-version))
            major (Integer/parseInt major)
            minor (Integer/parseInt minor)]
        (println (format "This is solr version %s" (first version)))
        (cond (and (= major 7) (>= minor 4))
              ;; https://issues.apache.org/jira/browse/SOLR-12858
              (do (println "Using get as default method due to issue SOLR-12858")
                  (set-default-method! :get))
              (>= major 8)
              (let [write-lock (str "test-files/solr-" major "/data/index/write.lock")]
                (if (.exists (io/as-file write-lock))
                  (.delete (io/as-file write-lock))))
              ))
      (with-connection (connect nil nil 
                                {:type EmbeddedSolrServer
                                 :core "clojure-solr"
                                 :home-dir (get-solr-home-dir)}) 
        (f)))))

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
    (is (= (get-in data [:authorization :user-role "i2kweb"]) "query"))
    (is (= (get-in data [:authorization :user-role "i2kconduit-db"]) "upload"))
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

(deftest test-lazy-search*
  (doseq [i (range 1000)]
    (add-document! {:id i :type "Web Page"
                    :fulltext "Many of the parameters relate to how this spell checker should query the index for term suggestions. The distanceMeasure defines the metric to use during the spell check query. The value \"internal\" uses the default Levenshtein metric, which is the same metric used with the other spell checker implementations."}))
  (commit!)
  (let [query-counter (atom 0)
        wrap-counter-middleware (fn [handler]
                                  (fn [query flags]
                                    (swap! query-counter inc)
                                    (handler query flags)))
        result (lazy-search* "Levenshtein" {:df "fulltext" :rows 10
                                            :sort "id ASC"
                                            :middleware (wrap-counter-middleware solr-app)})]
    (is (= clojure.lang.LazySeq (type result)))
    (is (= 1000 (count result)))
    (is (every? (fn [doc] (= "Web Page" (:type doc))) result))
    (is (= 101 @query-counter))))

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

(deftest test-nested-document
  (when (>= (get-solr-major-version) 8)
    (add-document! {:id "doc10"
                    :type "Dataset"
                    :pagetext ["Nothing here"]
                    :files [{:id "doc10!1"
                             :type "LAS File"
                             :pagetext ["A LAS File"]}
                            {:id "doc10!2"
                             :type "DLIS File"
                             :pagetext ["A DLIS File"]}]})
    (commit!)
    (let [result (search* "*:*"
                          {:facet-filters #{{:name "-_nest_parent_" :value "*"
                                             :formatter
                                             format-standard-filter-query}}
                           :df "pagetext"
                           :fields "*,[child]"})]
      (clojure.pprint/pprint result)
      (is (= 1 (count result)))
      (is (= "doc10" (:id (first result))))
      (is (= 2 (count (:files (first result)))))
      (is (= #{"doc10!1" "doc10!2"} (set (map :id (:files (first result))))))
     )))

(deftest test-highlighting
  (when (>= (get-solr-major-version) 8)
    (add-document! {:id "doc10"
                    :type "Dataset"
                    :pagetext ["Nothing here"]
                    :files [{:id "doc10!1"
                             :type "LAS File"
                             :pagetext ["A LAS File"]}
                            {:id "doc10!2"
                             :type "DLIS File"
                             :pagetext ["A DLIS File"]}]})
    (commit!)
    (let [result (search* "File"
                          {:df "pagetext"
                           :qf "pagetext type"
                           :defType "edismax"
                           :hl.fl "pagetext type"
                           :hl "true"
                           :hl.method "unified"
                           :hl.fragsize 0
                           :hl.snippets 4
                           :hl.tag.pre "<strong>"
                           :hl.tag.post "</strong>"})]
      (is (= (get (meta result) :highlighting)
             {"doc10!1" {"pagetext" ["A LAS <strong>File</strong>"]}, "doc10!2" {"pagetext" ["A DLIS <strong>File</strong>"]}})))))

(deftest test-with-opened-connection
  (let [conn *connection*]
    ;; Use the connection once through with-opened-connection
    (with-opened-connection conn
      (add-document! {:id "opened-1" :type "pdf" :title "opened test" :fulltext "opened fulltext"})
      (commit!))
    ;; Use the same connection again — proves it was NOT closed
    (with-opened-connection conn
      (let [result (search "id:opened-1" :df "fulltext")]
        (is (= 1 (count result)))
        (is (= "opened test" (:title (first result))))))))

(deftest test-register-shutdown-hook-no-throw
  ;; Just verify it doesn't throw; we can't easily trigger the hook in a test
  (is (nil? (register-shutdown-hook! *connection*))))

;; Kerberos configuration lifecycle tests (no KDC required)

(deftest test-kerberos-credentials-lifecycle
 (when (clojure-solr.impl/supports? :kerberos)
  (let [krb5-content (.getBytes "[libdefaults]\n  default_realm = TEST.REALM\n" "UTF-8")
        keytab-content (byte-array [0x05 0x02 0x00])]
    (try
      (set-kerberos-credentials "user@TEST.REALM" krb5-content keytab-content)
      (is (kerberos-configured?))
      (is (not (nil? (System/getProperty "java.security.krb5.conf"))))
      (is (not (nil? (System/getProperty "java.security.auth.login.config"))))
      ;; Verify JAAS file contains the principal and expected settings
      (let [jaas-path (System/getProperty "java.security.auth.login.config")
            jaas-content (slurp jaas-path)]
        (is (.contains jaas-content "user@TEST.REALM"))
        (is (.contains jaas-content "useKeyTab=true"))
        (is (.contains jaas-content "Client {")))
      ;; Verify krb5.conf was written
      (let [krb5-path (System/getProperty "java.security.krb5.conf")
            krb5-written (slurp krb5-path)]
        (is (.contains krb5-written "default_realm = TEST.REALM")))
      (finally
        (clear-kerberos-credentials)
        (is (not (kerberos-configured?)))
        (is (nil? (System/getProperty "java.security.krb5.conf")))
        (is (nil? (System/getProperty "java.security.auth.login.config"))))))))

(deftest test-kerberos-with-file-paths
 (when (clojure-solr.impl/supports? :kerberos)
  (let [krb5-file (java.io.File/createTempFile "test-krb5-" ".conf")
        keytab-file (java.io.File/createTempFile "test-keytab-" ".keytab")]
    (try
      (spit krb5-file "[libdefaults]\n  default_realm = FILE.REALM\n")
      (spit keytab-file "dummy-keytab")
      (set-kerberos-credentials "user@FILE.REALM"
                                 (.getAbsolutePath krb5-file)
                                 (.getAbsolutePath keytab-file))
      (is (kerberos-configured?))
      (is (= (.getAbsolutePath krb5-file)
             (System/getProperty "java.security.krb5.conf")))
      (finally
        (clear-kerberos-credentials)
        (.delete krb5-file)
        (.delete keytab-file))))))

(deftest test-kerberos-reconfiguration
 (when (clojure-solr.impl/supports? :kerberos)
  (let [content1 (.getBytes "[libdefaults]\n  default_realm = REALM1\n" "UTF-8")
        content2 (.getBytes "[libdefaults]\n  default_realm = REALM2\n" "UTF-8")
        keytab (.getBytes "dummy-keytab" "UTF-8")]
    (try
      (set-kerberos-credentials "user@REALM1" content1 keytab)
      (let [first-jaas-path (System/getProperty "java.security.auth.login.config")]
        (set-kerberos-credentials "user@REALM2" content2 keytab)
        ;; First JAAS file should have been cleaned up
        (is (not (.exists (java.io.File. first-jaas-path))))
        ;; New config should be active
        (let [jaas-content (slurp (System/getProperty "java.security.auth.login.config"))]
          (is (.contains jaas-content "user@REALM2"))))
      (finally
        (clear-kerberos-credentials))))))

(deftest test-kerberos-unsupported-raises-clearly
  ;; The other side of the same capability: where Kerberos is gone, asking for it
  ;; must say so rather than surface as a missing class somewhere further in.
  (when-not (clojure-solr.impl/supports? :kerberos)
    (let [e (try (set-kerberos-credentials "user@TEST.REALM"
                                           (.getBytes "[libdefaults]\n" "UTF-8")
                                           (byte-array [0x05 0x02 0x00]))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e) "set-kerberos-credentials must raise on a SolrJ without Kerberos")
      (is (= :kerberos (:feature (ex-data e))))
      (is (re-find #"Solr 10 removed" (.getMessage e))))))

(deftest test-connection-protocol-reexports-dispatch
  ;; clojure-solr re-exports the SolrConnection protocol.  Aliasing the protocol
  ;; method vars (def base-url impl/base-url) would freeze the dispatch cache as
  ;; it stood before the implementation namespace extended the protocol, so a
  ;; real client would silently fall through to the SolrClient default and
  ;; base-url would answer nil.  No server is contacted here: building a client
  ;; does not open a connection.
  (let [url "http://localhost:18983/solr/nosuch"
        client (connect url)]
    (try
      (is (= url (base-url client))
          "base-url must reach the implementation's extension, not the default")
      (is (= (base-url client) (clojure-solr.impl/base-url client)))
      ;; connect caches :http clients, and a cached client is the process's
      ;; rather than this scope's, so shared? answers true for it.
      (is (true? (shared? client)))
      (is (true? (cache-owned? client)))
      (is (identical? client (unwrap client)))
      (is (nil? (drain client)))
      ;; The embedded server is registered as shared, and reports no base URL.
      (is (true? (shared? *connection*)))
      (is (nil? (base-url *connection*)))
      ;; ...but only mark-shared! earns blind reuse by a nested scope.
      (is (true? (clojure-solr.impl/reuse-bound? *connection*)))
      (is (false? (clojure-solr.impl/reuse-bound? client)))
      (finally (close-cached-connections!)))))

;;; ---------------------------------------------------------------------------
;;; The connection cache
;;;
;;; The gate the Solr 10 migration named -- "connect twice through the real
;;; client" -- was read as twice through ONE client, which is the nesting case.
;;; What wedged a deployed process was twice through TWO clients: a call site
;;; that connected per operation, so SolrJ 10's per-instance pooling had nothing
;;; to pool on, and a scope that closed a client another thread was using.
;;; ---------------------------------------------------------------------------

(deftest test-connect-reuses-one-client-per-target
  (let [url  "http://localhost:18983/solr/cache-a"
        url2 "http://localhost:18983/solr/cache-b"]
    (try
      (is (identical? (connect url) (connect url))
          "two connects for the same URL and options must be one client")
      (is (not (identical? (connect url) (connect url2)))
          "different collections must not share a client")
      (is (not (identical? (connect url) (connect url nil {:socket-timeout 4321})))
          "options that change how the client is built must not share a client")
      (is (not (identical? (connect url nil {:socket-timeout 4321})
                           (connect url nil {:socket-timeout 8765})))
          "and neither must different values of the same option")
      (is (identical? (connect url nil {:socket-timeout 4321})
                      (connect url nil {:socket-timeout 4321}))
          "while the same options must")
      (is (not (identical? (connect url) (connect url nil {:cache-client? false})))
          ":cache-client? false must hand back something the caller owns")
      (is (false? (cache-owned? (connect url nil {:cache-client? false}))))
      (finally (close-cached-connections!)))))

;; A client type the test owns, so that closing can be counted without a server
;; and without depending on how a particular SolrJ reports a closed client.
;; :cache-client? true is what puts it in the cache; only :http goes there by
;; default.
(defmethod make-solr-client ::counting
  [_ _ _ solr-client-options]
  (let [closed (:closed solr-client-options)]
    (proxy [SolrClient] []
      (request [_ _] nil)
      (close [] (swap! closed inc)))))

(deftest test-with-connection-closes-only-what-it-owns
  (testing "a client the scope built is still closed on the way out"
    (let [closed (atom 0)
          client (proxy [SolrClient] []
                   (request [_ _] nil)
                   (close [] (swap! closed inc)))]
      ;; *connection* is bound to the embedded server by the fixture, and that
      ;; one is reused blindly; unbind it so with-connection takes its ordinary
      ;; path.
      (binding [*connection* nil]
        (with-connection client
          (is (identical? client *connection*))))
      (is (= 1 @closed) "a connection this scope created must be closed")))

  (testing "a client the cache owns survives the scope, and later scopes"
    (let [closed (atom 0)
          opts {:type ::counting :cache-client? true :closed closed}
          url "http://localhost:18983/solr/counted"
          client (connect url nil opts)]
      (try
        (is (true? (cache-owned? client)))
        (is (true? (shared? client)))
        (binding [*connection* nil]
          (with-connection (connect url nil opts)
            (is (identical? client *connection*))))
        (is (zero? @closed)
            "with-connection must not close a client the cache will hand out again")
        (binding [*connection* nil]
          (with-connection (connect url nil opts)
            (is (identical? client *connection*)
                "and the second scope must still get a usable client")))
        (is (zero? @closed))
        (finally
          (is (pos? (close-cached-connections!)))
          (is (= 1 @closed) "close-cached-connections! is what closes it")
          (is (false? (cache-owned? client))
              "and the cache forgets it")))))

  (testing "concurrent connects for one target build one client"
    (let [url "http://localhost:18983/solr/racing"]
      (try
        (let [clients (->> (repeatedly 16 #(future (connect url)))
                           (doall)
                           (map deref)
                           (set))]
          (is (= 1 (count clients))
              "every thread must get the one client, not one each"))
        (finally (close-cached-connections!))))))

;; A client whose request blocks until released, so a scope exit can be made to
;; land while another thread is provably mid-request.
(defmethod make-solr-client ::blocking
  [_ _ _ {:keys [closed in-flight release]}]
  (proxy [SolrClient] []
    (request [_ _]
      (.countDown ^java.util.concurrent.CountDownLatch in-flight)
      (.await ^java.util.concurrent.CountDownLatch release)
      ;; SolrClient.request is declared to return a NamedList, so the proxy
      ;; casts whatever comes back; a keyword marker would fail on the cast.
      (doto (org.apache.solr.common.util.NamedList.) (.add "completed" "yes")))
    (close [] (swap! closed inc))))

(deftest test-scope-exit-does-not-close-a-client-in-use
  ;; The shape that wedged production: several workers using one client at once,
  ;; each wrapping its own with-connection around one operation.  The first scope
  ;; to exit runs the finally.  If that closes the client, the others are left on
  ;; a future that will never complete in either direction -- on Java 17
  ;; HttpJdkSolrClient.close shuts down the executor its responses arrive
  ;; through, and the timeout would have to arrive the same way.  Measured
  ;; end-to-end on Java 17, that shape parks 8 of 8 workers permanently when the
  ;; client is shared by hand, and 0 of 8 when it comes from connect.
  ;;
  ;; Java 21 cannot show the park -- close there fails fast instead -- and the
  ;; suite runs 21 because solr-core 10 requires it.  So this asserts the rule
  ;; rather than the symptom: the scope must not close, and the in-flight request
  ;; must still finish.
  (let [closed    (atom 0)
        in-flight (java.util.concurrent.CountDownLatch. 1)
        release   (java.util.concurrent.CountDownLatch. 1)
        opts      {:type ::blocking :cache-client? true :closed closed
                   :in-flight in-flight :release release}
        url       "http://localhost:18983/solr/in-use"
        worker    (future
                    (binding [*connection* nil]
                      (with-connection (connect url nil opts)
                        (.request ^SolrClient *connection* nil nil))))]
    (try
      (is (.await in-flight 10 java.util.concurrent.TimeUnit/SECONDS)
          "the worker must reach the point of being mid-request")
      ;; A second scope opens on the same client and exits immediately.
      (binding [*connection* nil]
        (with-connection (connect url nil opts) :nothing))
      (is (zero? @closed)
          "a scope exiting must not close a client another thread is using")
      (.countDown release)
      (let [result (deref worker 10000 :timed-out)]
        (is (not= :timed-out result) "and the in-flight request must still complete")
        (is (= "yes" (.get ^org.apache.solr.common.util.NamedList result "completed"))))
      (finally
        (.countDown release)
        (close-cached-connections!)))
    (is (= 1 @closed) "close-cached-connections! is what finally closes it")))

(deftest test-cache-registration-cannot-leak
  ;; connect raced against close-cached-connections!.  The invariant is not that
  ;; every client handed out is still cached -- a close may land between the two
  ;; -- but that nothing is left registered as cache-owned while unreachable from
  ;; the cache.  Such a client is closed by nobody: with-connection refuses
  ;; because it reports itself shared, and close-cached-connections! cannot see
  ;; it.  A delay-based cache that registered the client from inside the delay
  ;; leaked ~200 of them per run of this shape, because a close could drop the
  ;; not-yet-realized entry while the delay went on to register it anyway.
  (let [closed (atom 0)
        opts   {:type ::counting :cache-client? true :closed closed}
        url    (fn [k] (str "http://localhost:18983/solr/leak-" k))
        closer (future (dotimes [_ 80]
                         (Thread/sleep 2)
                         (close-cached-connections!)))
        churn  (doall (for [i (range 8)]
                        (future (dotimes [n 250]
                                  (connect (url (mod (+ i n) 32)) nil opts)))))]
    (run! deref churn)
    @closer
    (close-cached-connections!)
    (is (= (clojure-solr.impl/cached-client-count)
           (clojure-solr.impl/cache-owned-count))
        "a registered client must still be reachable from the cache")
    (is (zero? (clojure-solr.impl/cached-client-count)))
    (is (pos? @closed) "the race must actually have built and closed clients")))

(deftest test-jdk-client-carries-a-default-request-timeout
  ;; Only the JDK client.  The Apache client on SolrJ 9 genuinely has no default
  ;; socket timeout, but changing that is a separate decision about consumers
  ;; still on 6.0.0/SolrJ 8.11.4.
  (when-not (clojure-solr.impl/supports? :connection-manager)
    (let [default @(ns-resolve 'clojure-solr.impl.solr10 'default-socket-timeout)
          field   (doto (.getDeclaredField
                         (Class/forName "org.apache.solr.client.solrj.impl.HttpSolrClientBase")
                         "requestTimeoutMillis")
                    (.setAccessible true))
          timeout (fn [client] (.getLong field (unwrap client)))]
      (try
        (is (= (long default) (timeout (connect "http://localhost:18983/solr/timeouts")))
            "an unconfigured connection must still be bounded")
        (is (= 45000 (timeout (connect "http://localhost:18983/solr/timeouts"
                                       nil {:socket-timeout 45000})))
            ":socket-timeout must still override it")
        (finally (close-cached-connections!))))))

(deftest test-http1-is-selectable-per-connection
  ;; Forcing HTTP/1.1 is how a caller sidesteps the JDK's HTTP/2 GOAWAY handling.
  ;; SolrJ offers only the solr.http1 system property, read in the builder's
  ;; constructor -- so it cannot reach a client that connect has already cached.
  ;; :http1? is part of the cache key, so it always applies to the client it
  ;; describes.
  (when-not (clojure-solr.impl/supports? :connection-manager)
    (let [forced (fn [conn]
                   (let [raw (unwrap conn)
                         f (doto (.getDeclaredField (class raw) "forceHttp11")
                             (.setAccessible true))]
                     (.getBoolean f raw)))
          url "http://localhost:18983/solr/http1"]
      (try
        (is (false? (forced (connect url)))
            "HTTP/2 stays the default")
        (is (true? (forced (connect url nil {:http1? true})))
            ":http1? must reach the built client")
        (is (not (identical? (connect url) (connect url nil {:http1? true})))
            "and must not share a cached client with the default")

        (testing "and can be set once for the process"
          (set-default-http-version! :http1)
          (is (true? (forced (connect url))) "the default must reach new connections")
          (is (false? (forced (connect url nil {:http1? false})))
              "an explicit option still wins")
          (set-default-http-version! :http2)
          (is (false? (forced (connect url))))
          (is (identical? (connect url) (connect url nil {:http1? false}))
              ":http2 and an explicit false must not build two clients")
          (is (thrown? clojure.lang.ExceptionInfo (set-default-http-version! :http3))))
        (finally
          (set-default-http-version! nil)
          (close-cached-connections!))))))

(deftest test-do-query-failure-always-carries-a-message
  ;; SolrJ 10's JDK client reports an unreachable Solr as a ConnectException
  ;; wrapping a ClosedChannelException, and neither carries a message.
  ;; Rethrowing with (ex-info (.getMessage e) ...) then produced an ExceptionInfo
  ;; whose own message was nil, which every caller matching on that message has
  ;; to guard against: i2kweb's search-failure page threw NullPointerException
  ;; out of re-find instead of rendering "the index is unavailable".
  (letfn [(failure [^Throwable thrown]
            (try
              (binding [*connection* (proxy [SolrClient] []
                                       (request [_ _] (throw thrown))
                                       (close []))]
                (do-query (clojure-solr.impl/new-query (clojure-solr.impl/impl) "*:*") {}))
              nil
              (catch clojure.lang.ExceptionInfo e e)))]
    (testing "a real message is passed through unchanged"
      (let [msg "org.apache.solr.search.SyntaxError: Cannot parse 'foo:'"]
        (is (= msg (.getMessage ^Throwable (failure (Exception. msg)))))))
    (testing "a missing message falls back to the cause's"
      (let [thrown (doto (java.net.ConnectException.)
                     (.initCause (java.io.IOException. "Connection refused")))]
        (is (= "Connection refused" (.getMessage ^Throwable (failure thrown))))))
    (testing "and to a type name when nothing in the chain has one"
      (is (= "java.nio.channels.ClosedChannelException"
             (.getMessage ^Throwable (failure (java.nio.channels.ClosedChannelException.))))))
    (testing "the original failure is kept as the cause either way"
      (let [thrown (java.nio.channels.ClosedChannelException.)]
        (is (identical? thrown (.getCause ^Throwable (failure thrown))))))))
