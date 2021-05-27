(ns clojure-solr-test
  (:require [clojure.pprint]
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

(defn get-solr-version
  []
  (let [name (format "%s.class" (.getSimpleName EmbeddedSolrServer))
        resource (str (.getResource EmbeddedSolrServer name))]
    (if (re-matches #"jar:.+" resource)
      (let [manifest-name (str (subs resource 0 (inc (.lastIndexOf resource "!"))) "/META-INF/MANIFEST.MF")]
        (with-open [s (.openStream (java.net.URL. manifest-name))]
          (let [manifest (Manifest. s)
                attrs (.getMainAttributes manifest)]
            (.getValue attrs "Specification-Version")))))))

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

(defn make-solr-container
  "This is complicated because we support multiple Solr versions that happen to have conflicting Constructor signatures
   for CoreContainer, and Clojure-Java interop has issues at compilation time, even though a particular Constructor
   invocation might never happen for a given Solr version."
  []
  (let [solr-version (get-solr-version)
        major-version (second (re-matches #"(\d+)\..*" solr-version)) 
        cont-expr (case (Integer/parseInt major-version)
                    (6 7) `(CoreContainer.)
                    8 (let [home-dir (get-solr-home-dir)]
                        `(CoreContainer. (.getPath (java.nio.file.FileSystems/getDefault) ~home-dir (make-array String 0))
                                         (doto (java.util.Properties.)
                                           (.setProperty "solr.dist.dir"
                                                         (str (System/getProperty "user.dir")
                                                              "/test-files/dist"))))))]
    (eval cont-expr)))

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
    (let [cont (make-solr-container)]
      (.load cont)
      (binding [*connection* (EmbeddedSolrServer. cont "clojure-solr")]
        (f)
        (.close *connection*)))))

(use-fixtures :each solr-server-fixture)

(def sample-doc
  {:id "1" :type "pdf" :title "my title" :fulltext "my fulltext" :numeric 10
   :updated (tcoerce/to-date (t/date-time 2015 02 27))
   :terms ["Vocabulary 1/Term A" "Vocabulary 1/Term B" "Vocabulary 2/Term X/Term Y"]})

(deftest test-add-doc-with-lazyseq
  (add-document! {:id 2 :type "pdf" :related_s_mv (cheshire/parse-string "[\"abc\",\"def\"]")})
  (commit!)
  (let [result (first (search "*" :facet-filters [{:name "id" :value "2"}]))]
    (is result)
    (is (vector? (:related_s_mv result)))
    (is (= #{"abc" "def"} (set (:related_s_mv result)))))
  )

(deftest test-add-document!
  (do (add-document! sample-doc)
      (commit!))
  (is (= sample-doc (dissoc (first (search "my" :df "fulltext")) :_version_)))
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
  (is (= sample-doc (dissoc (first (search "\"my fulltext\""  :df "fulltext")) :_version_)))
  (is (empty? (search "\"fulltext my\""  :df "fulltext"))))

(deftest test-quoted-search-mw
  (do (add-document! sample-doc)
      (commit!))
  (is (= sample-doc (dissoc (first (search*-with-middleware "\"my fulltext\""  {:df "fulltext"})) :_version_ :word)))
  (is (empty? (search*-with-middleware "\"fulltext my\""  {:df "fulltext"}))))

(deftest test-facet-query
  (do (add-document! sample-doc)
      (commit!))
  (is (= [{:name "terms" :value "Vocabulary 1" :count 1}]
         (:facet-queries (meta (search "my" :facet-queries [{:name "terms" :value "Vocabulary 1"}] :df "fulltext")))))
  (is (= "q=my&df=fulltext&facet-queries={:name+\"terms\",+:value+\"Vocabulary+1\"}&facet.query={!raw+f%3Dterms}Vocabulary+1&facet=true&facet.mincount=1"
         (search "my" :just-return-query? true :facet-queries [{:name "terms" :value "Vocabulary 1"}] :df "fulltext"))))

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
                    :fulltext "When choosing a field to query for this spell checker, you want one which has relatively little analysis performed on it (particularly analysis such as stemming). Note that you need to specify a field to use for the suggestions, so like the IndexBasedSpellChecker, you may want to copy data from fields like title, body, etc., to a field dedicated to providing spelling suggestions.

Many of the parameters relate to how this spell checker should query the index for term suggestions. The distanceMeasure defines the metric to use during the spell check query. The value \"internal\" uses the default Levenshtein metric, which is the same metric used with the other spell checker implementations.

Because this spell checker is querying the main index, you may want to limit how often it queries the index to be sure to avoid any performance conflicts with user queries. The accuracy setting defines the threshold for a valid suggestion, while maxEdits defines the number of changes to the term to allow. Since most spelling mistakes are only 1 letter off, setting this to 1 will reduce the number of possible suggestions (the default, however, is 2); the value can only be 1 or 2. minPrefix defines the minimum number of characters the terms should share. Setting this to 1 means that the spelling suggestions will all start with the same letter, for example.

The maxInspections parameter defines the maximum number of possible matches to review before returning results; the default is 5. minQueryLength defines how many characters must be in the query before suggestions are provided; the default is 4.

At first, spellchecker analyses incoming query words by looking up them in the index. Only query words, which are absent in index or too rare ones (below maxQueryFrequency ) are considered as misspelled and used for finding suggestions. Words which are frequent than maxQueryFrequency bypass spellchecker unchanged. After suggestions for every misspelled word are found they are filtered for enough frequency with thresholdTokenFrequency as boundary value. These parameters (maxQueryFrequency and thresholdTokenFrequency) can be a percentage (such as .01, or 1%) or an absolute value (such as 4)."}))
  (commit!)
  (let [result (suggest "Leven" {:df "fulltext"})]
    (println (format "Best suggestion for \"Leven\": %s" (:term (first result))))
    (is (= (:term (first result) "Levenshtein"))))
  (let [result (spellcheck "Levenstein" {:df "fulltext"})]
    (println (format "Corrected Levenstein to %s" (:collated-result result)))
    (is (= {:collated-result "Levenshtein" :alternatives '("Levenshtein")}
           result)))
  (let [result (search* "Leven" {:df "fulltext"})]
    )
  )


  
