(ns clojure-solr.admin
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io])
  (:require [clj-http.client :as http])
  (:require [clojure-solr :as solr])
  (:import [java.util Properties])
  (:import [org.apache.solr.common.util SimpleOrderedMap NamedList])
  (:import [org.apache.solr.client.solrj.request CoreAdminRequest]
           [org.apache.solr.client.solrj.response CoreAdminResponse]
           [org.apache.solr.common.params CoreAdminParams CoreAdminParams$CoreAdminAction
            ])
  (:import [org.apache.solr.client.solrj.request
            ConfigSetAdminRequest
            ConfigSetAdminRequest$List
            ConfigSetAdminRequest$Create
            ConfigSetAdminRequest$Delete
            ConfigSetAdminRequest$ConfigSetSpecificAdminRequest]
           [org.apache.solr.client.solrj.response ConfigSetAdminResponse])
  (:import [org.apache.solr.common.params CollectionParams CollectionParams$CollectionAction]
           [org.apache.solr.client.solrj.request
            CollectionAdminRequest
            CollectionAdminRequest$Create
            CollectionAdminRequest$ClusterStatus
            CollectionAdminRequest$ClusterProp
            CollectionAdminRequest$Delete
            CollectionAdminRequest$List
            CollectionAdminRequest$SplitShard
            CollectionAdminRequest$Reload]
           [org.apache.solr.client.solrj.response
            CollectionAdminResponse])
  ;;(:import [org.apache.solr.cloud ZkController])
  (:import [org.apache.solr.common.cloud SolrZkClient ZkNodeProps ZkStateReader])
  (:import [org.apache.http.client HttpClient]
           [org.apache.http.client.methods HttpPost HttpGet]
           [org.apache.http.entity InputStreamEntity StringEntity ContentType]
           [org.apache.http HttpRequest]
           [org.apache.http.util EntityUtils]
           )
  (:import [java.util.zip ZipInputStream ZipEntry ZipOutputStream]
           [java.io File InputStream ByteArrayInputStream ByteArrayOutputStream]
           [java.nio.file Path Paths Files LinkOption OpenOption])
  )

(def json-enabled?
  (try (require 'cheshire.core)
       true
       (catch Throwable _ false)))

(defn get-cheshire-parse-string
  []
  (ns-resolve (symbol "cheshire.core") (symbol "parse-string")))


(defn list-cores
  "List loaded cores in a standalone Solr"
  []
  (let [^CoreAdminRequest request (doto (CoreAdminRequest.)
                                    (.setAction CoreAdminParams$CoreAdminAction/STATUS))
        ^CoreAdminResponse response (.process request solr/*connection*)
        core-status (.getCoreStatus response)]
    (for [[_ core] (iterator-seq (.iterator core-status))
          :let [core-map (into {} core)
                index-map (into {} (get core-map "index"))]]
      (assoc core-map "index" index-map))))
    
(defn get-core-status
  "Get core status in a standalone Solr"
  [name]
  (-> (into {} (.getCoreStatus (CoreAdminRequest/getStatus name solr/*connection*) name))
      (update-in ["index"] #(into {} %))))

(defn- process-core-admin-response
  [^CoreAdminResponse response]
  (if (= 0 (.getStatus response))
      true
      (throw (ex-info "Request failed" {:response response :client solr/*connection*}))))

(defn create-core
  "Create a core in standalone Solr.  
   name: core name
   instance-dir: Path to core instance dir on Solr server
   config-file: Path to solrconfig.xml
   schema-file: path to schema.xml or managed-schema
   data-dir: Path to data
   transaction-log dir: Path to tlog dir"
  [name instance-dir
   & {:keys [config-file schema-file data-dir transaction-log-dir]
      :or {config-file "conf/solrconfig.xml"
           schema-file "conf/schema.xml"
           data-dir "data"
           transaction-log-dir nil}}]
  (let [response (CoreAdminRequest/createCore name instance-dir
                                              solr/*connection*
                                              config-file
                                              schema-file
                                              data-dir
                                              transaction-log-dir)]
    (process-core-admin-response response)))

(defn unload-core
  "Unload a core in standalone Solr."
  [name & {:keys [delete-index? delete-instance-dir?]}]
  (let [response (CoreAdminRequest/unloadCore name
                                              (if delete-index? true false)
                                              (if delete-instance-dir? true false)
                                              solr/*connection*)]
    (process-core-admin-response response)))

(defn reload-core
  "Reload a core in standalone solr."
  [name]
  (let [response (CoreAdminRequest/reloadCore name solr/*connection*)]
    (process-core-admin-response response)))
  

(defn list-config-sets
  "List defined configsets in SolrCloud."
  []
  (get (into {} (.request solr/*connection* (ConfigSetAdminRequest$List.))) "configSets"))

(defmulti upload-config-set
  "Upload a configset to a SolrCloud server. Config can be a 
  filename, File, Path, or InputStream to a Zip file.  For all
  but the InputStream, config must name a conf directory for the
  configset containing solrconfig.xml.  "
  (fn [name config & [opts]] (type config)))

(defmethod upload-config-set :default
  [name location & [opts]]
  (throw (IllegalArgumentException. (format "%s is not a valid type.  Only java.io.File, java.nio.file.Path, and InputStream"))))

(defmethod upload-config-set String
  [name directory-name & [opts]]
  (upload-config-set name (Paths/get directory-name (make-array String 0)) opts))

(defmethod upload-config-set File
  [name file & [opts]]
  (if (.isDirectory file)
    (upload-config-set name (.toPath file) opts)
    (throw (IllegalArgumentException. (format "%s is not a directory" file)))))
  

(defn Path->ZipInputStream
  [path]
  (letfn [(file-seq [^Path directory]
            (with-open [s (Files/newDirectoryStream directory)]
              (loop [files (iterator-seq (.iterator s))
                     out-files []]
                (if (empty? files)
                  out-files
                  (let [file (first files)]
                    (if (Files/isDirectory file (make-array LinkOption 0))
                      (recur (rest files)
                             (concat out-files [file] (file-seq file)))
                      (recur (rest files)
                             (conj out-files file))))))))]
    (with-open [bytes (ByteArrayOutputStream.)
                zip-out (ZipOutputStream. bytes)]
      (doseq [f (file-seq path)
              :when (Files/isReadable f)
              :let [relative-path (.relativize path f)
                    is-directory? (Files/isDirectory f (make-array LinkOption 0))
                    entry-name (str relative-path (if is-directory? "/" ""))]]
        (solr/trace (format "Adding %s" entry-name))
        (.putNextEntry zip-out (ZipEntry. entry-name))
        (when-not is-directory?
          (with-open [s (Files/newInputStream f (make-array OpenOption 0))]
            (io/copy s zip-out)))
        (.closeEntry zip-out))
      (.flush zip-out)
      (let [zip-bytes (.toByteArray bytes)]
        (solr/trace (format "Zip stream to upload as configset is %d bytes" (count zip-bytes)))
        (ByteArrayInputStream. zip-bytes)))))
  

(defmethod upload-config-set Path
  [name path & [opts]]
  (if-not (Files/isDirectory path (make-array LinkOption 0))
    (throw (IllegalArgumentException. (format "%s is not a directory" (str path))))
    (with-open [zip-in (Path->ZipInputStream path)]
      (upload-config-set name zip-in opts))))

(defmethod upload-config-set InputStream
  [name zipstream & [opts]]
  (let [base-url (.getBaseURL solr/*connection*)
        base-client (.getHttpClient (solr/connect base-url))
        upload-url (str base-url
                        "/admin/configs?action=UPLOAD&name="
                        name
                        (if (:overwrite opts)
                          "&overwrite=true"
                          "")
                        (if (:cleanup opts)
                          "&cleanup=true"
                          "")
                        (if (:filePath opts)
                          (str "&filePath=" (:filePath opts))
                          ""))]
    (let [entity (doto (InputStreamEntity. zipstream -1)
                   (.setContentType "binary/octet-stream"))
          post (doto (HttpPost. upload-url)
                 (.setEntity entity))
          response (.execute base-client post)
          status (.getStatusCode (.getStatusLine response))
          body (EntityUtils/toString (.getEntity response))]
      (solr/trace (format "upload-config-set status %d reason %s" status body))
      (if (>= status 400)
        (throw (ex-info body {:response response :status status}))
      true))))

(defn- map->Properties
  [properties]
  (let [props (Properties.)]
    (doseq [[k v] properties]
      (when (and (string? k) (string? v))
        (.setProperty props k v)))
    props))

(defn create-config-set
  "Create a configset with a given name, based upon a configset with a given base-name 
   and optionally with properties set from a map of properties."
  [name base-name & [properties]]
  {:pre [(or (nil? properties) (map? properties))]}
  (let [props (if properties
                (map->Properties properties))
        ^ConfigSetAdminRequest$Create request (ConfigSetAdminRequest$Create.)
        setConfigSetName-method (.getMethod ConfigSetAdminRequest$ConfigSetSpecificAdminRequest
                                            "setConfigSetName"
                                            (into-array java.lang.Class [String]))]
    ;; (.setConfigSetName request name)
    ;; https://stackoverflow.com/questions/38059977/cant-call-public-method-of-non-public-class-public-google-gcloud-library
    ;; The request object's base class is ConfigSetAdminRequest$ConfigSetSpecificAdminRequest
    ;; This is protected abstract, and we can't call the setConfigSetName method, which is public, due to a JDK bug from 1999.
    (.setAccessible setConfigSetName-method true)
    (.invoke setConfigSetName-method request (into-array Object [name]))
    (.setBaseConfigSetName request base-name)
    (when props
     (.setNewConfigsetProperties request props))
    (let [response (.process request solr/*connection*)]
      (if (= 0 (.getStatus response))
        true
        (throw (ex-info "Request failed"
                        {"errors" (.getAll (.getErrorMessages response))}))))))

(defn delete-config-set
  "Delete a configset."
  [name]
  (let [^ConfigSetAdminRequest$Delete request (ConfigSetAdminRequest$Delete.)
        setConfigSetName-method (.getMethod ConfigSetAdminRequest$ConfigSetSpecificAdminRequest
                                            "setConfigSetName"
                                            (into-array java.lang.Class [String]))]
    ;; (.setConfigSetName request name)
    ;; https://stackoverflow.com/questions/38059977/cant-call-public-method-of-non-public-class-public-google-gcloud-library
    ;; The request object's base class is ConfigSetAdminRequest$ConfigSetSpecificAdminRequest
    ;; This is protected abstract, and we can't call the setConfigSetName method, which is public, due to a JDK bug from 1999.
    (.setAccessible setConfigSetName-method true)
    (.invoke setConfigSetName-method request (into-array Object [name]))
    (let [response (.process request solr/*connection*)]
      (if (= 0 (.getStatus response))
        true
        (throw (ex-info "Request failed"
                        {"errors" (.getAll (.getErrorMessages response))}))))))
        
(def ^:private router-names
  {"implicit" :implicit
   "compositeId" :composite-id})

(defn get-cluster-status
  "Get status of a cluster"
  [& {:keys [collection-name route-key shard-name]}]
  (let [request (CollectionAdminRequest$ClusterStatus.)
        response (.request solr/*connection* request)
        cluster (.get response "cluster")
        live-nodes (.get cluster "live_nodes")
        collections (.get cluster "collections")
        safe-parseint (fn [val default]
                        (cond (string? val) (Integer/parseInt val)
                              val val
                              :else default))]
    {:collections (into {}
                        (for [[name collection] collections]
                          [name {:config-name (.get collection "configName")
                                 :router-name (get router-names (.get (.get collection "router") "name"))
                                 ;; :router-field
                                 :num-shards (count (.get collection "shards"))
                                 :num-replicas (apply max (map (fn [[_ shard]] (count (.get shard "replicas"))) (.get collection "shards")))
                                 :shards (str/join "," (keys (.get collection "shards")))
                                 :shard_health (for [[shard-name shard] (.get collection "shards")]
                                                 {:shard-name shard-name
                                                  :health (.get shard "health")
                                                  :state (.get shard "state")})
                                 :cores (for [[shard-name shard] (.get collection "shards")
                                              [replica-name replica] (.get shard "replicas")]
                                          {:shard-name shard-name
                                           :replica-name replica-name
                                           :core (.get replica "core")
                                           :base-url (.get replica "base_url")
                                           :type (.get replica "type")
                                           :node-name (.get replica "node_name")})
                                 :max-shards-per-node (Integer/parseInt (or (.get collection "maxShardsPerNode") "1"))
                                 :replication-factor (safe-parseint (.get collection "replicationFactor") 1)
                                 :nrt-replicas (safe-parseint (.get collection "nrtReplicas") 0)
                                 :pull-replicas (safe-parseint (.get collection "pullReplicas") 0)
                                 :tlog-replicas (safe-parseint (.get collection "tlogReplicas") 0)
                                 :node-set (str/join #","
                                                     (distinct
                                                      (for [[shard shard-desc] (.get collection "shards")
                                                            [replica replica-desc] (.get shard-desc "replicas")]
                                                       (.get replica-desc "node_name"))))
                                 :auto-add-replicas? (Boolean/parseBoolean (.get collection "autoAddReplicas"))}]))
     :live-nodes live-nodes})
  #_(let [^CollectionAdminRequest$ClusterStatus status-request (doto (CollectionAdminRequest/getClusterStatus)
                                                               (cond-> collection-name (.setCollectionName collection-name))
                                                               (cond-> route-key (.setRouteKey route-key))
                                                               (cond-> shard-name (.setShardName shard-name)))
        ^CollectionAdminResponse response (.process status-request solr/*connection*)]
    response))


(defn list-collections
  []
  (CollectionAdminRequest/listCollections  solr/*connection*))
  
(defn reload-collection
  [name & {:keys [timeout] :or {timeout 60}}]
  (let [^CollectionAdminRequest$Reload reload-request
        (CollectionAdminRequest/reloadCollection name)]
    (str (.processAndWait reload-request solr/*connection* timeout))))
  
(defn modify-collection
  [name & {:keys [max-shards-per-node
                  replication-factor
                  auto-add-replicas?
                  config-name
                  timeout]
           :or {timeout 60}
           :as properties}]
  (let [props (merge (if max-shards-per-node {"maxShardsPerNode" max-shards-per-node})
                     (if replication-factor {"replicationFactor" replication-factor})
                     (if auto-add-replicas? {"autoAddReplicas" auto-add-replicas?})
                     (if config-name {"collection.configName" config-name})) 
        modify-request (CollectionAdminRequest/modifyCollection name props)]
    (.processAndWait modify-request solr/*connection* timeout)))
                  

(defn create-collection
  "Create a collection."
  [name num-replicas num-shards & {:keys [config-name #_with-collection
                                          router-name router-field shards 
                                          replication-factor nrt-replicas pull-replicas tlog-replicas
                                          max-shards-per-node
                                          node-set #_node-set-shuffle? auto-add-replicas?
                                          collection-properties
                                          timeout]
                                   :or {timeout 60}}]
  {:pre [name num-replicas num-shards
         (#{:implicit :composite-id "compositeId" "implicit" nil} router-name)]}
  (let [create-request (if (not-empty config-name)
                         (CollectionAdminRequest/createCollection name config-name num-shards num-replicas)
                         (CollectionAdminRequest/createCollection name num-shards num-replicas))]
    (when (= router-name :implicit)
      (solr/trace "(.setRouterName \"implicit\")")
      (.setRouterName create-request "implicit"))
    (when (= router-name :composite-id)
      (solr/trace "(.setRouterName \"compositeId\")")
      (.setRouterName create-request "compositeId"))
    (when (and (string? router-name) (not-empty router-name))
      (solr/trace (format "(setRouterName \"%s\")" router-name))
      (.setRouterName create-request router-name))
    (when (not-empty router-field)
      (solr/trace (format "(.setRouterField \"%s\")" router-field))
      (.setRouterField create-request router-field))
    (when (not-empty shards)
      (solr/trace (format "(.setShards \"%s\")" shards))
      (.setShards create-request shards))
    (when replication-factor
      (solr/trace (format "(.setReplicationFactor %d)" replication-factor))
      (.setReplicationFactor create-request replication-factor))
    (when (and nrt-replicas (> nrt-replicas 1))
      (solr/trace (format "(.setNrtReplicas %d)" nrt-replicas))
      (.setNrtReplicas create-request nrt-replicas))
    (when (and pull-replicas (> pull-replicas 0))
      (solr/trace (format "(.setPullReplicas %d)" pull-replicas))
      (.setPullReplicas create-request pull-replicas))
    (when (and tlog-replicas (> tlog-replicas 0))
      (solr/trace (format "(.setTlogReplicas %d)" tlog-replicas))
      (.setTlogReplicas create-request tlog-replicas))
    (when (and max-shards-per-node (> max-shards-per-node 1))
      (solr/trace (format "(.setMaxShardsPerNode %d)" max-shards-per-node))
      (.setMaxShardsPerNode create-request max-shards-per-node))
    (when (not-empty node-set)
      (solr/trace (format "(.setCreateNodeSet \"%s\")" node-set))
      (.setCreateNodeSet create-request node-set))
    (when auto-add-replicas?
      (solr/trace (format "(.setAutoAddReplicas %s)" auto-add-replicas?))
      (.setAutoAddReplicas create-request auto-add-replicas?))
    (when collection-properties
      (.setProperties create-request (map->Properties collection-properties)))
    (.processAndWait create-request solr/*connection* timeout)))

(defn get-collection-overlay
  [collection & {:keys [as]}]
  (let [base-url (.getBaseURL solr/*connection*)
        base-client (.getHttpClient (solr/connect base-url))
        overlay-url (str base-url "/" collection "/config/overlay")
        http-get (HttpGet. overlay-url)
        response (.execute base-client http-get)]
    (if (>= (.getStatusCode (.getStatusLine response)) 300)
      (throw (ex-info "Failed"
                      {:reason (.getReasonPhrase (.getStatusLine response))
                       :success false
                       :status (.getStatusCode (.getStatusLine response))}))
      (let [body (EntityUtils/toString (.getEntity response))]
        (case as
            :string body
            :json (if json-enabled?
                    (if-let [parse-string (get-cheshire-parse-string)]
                      (let [overlay (get (parse-string body true) :overlay)]
                        overlay)
                      (throw (IllegalStateException. "Missing #'cheshire.core/parse-string")))
                    (throw (IllegalStateException. "cheshire is not loaded"))))))))


(defn upload-to-zookeeper
  [zkhost path bytes & {:keys [timeout] :or {timeout 60}}]
  (let [client (SolrZkClient.  zkhost timeout)]
    (.makePath client path false true)
    (.setData client path bytes true)))

(defn download-from-zookeeper
  [zkhost path & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (SolrZkClient.  zkhost timeout)]
    (.getData client path nil nil true)))

(defn delete-from-zookeeper
  [zkhost path version & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (SolrZkClient.  zkhost timeout)]
    (.delete client path version true)))

(defn get-collection-properties
  [zkhost collection & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (SolrZkClient.  zkhost timeout)]
    (if-let [parse-string (get-cheshire-parse-string)]
      (let [path (str org.apache.solr.common.cloud.ZkStateReader/COLLECTIONS_ZKNODE "/" collection)
            data (.getData client path nil nil true)
            props (into {} (.getProperties (ZkNodeProps/load data)))
            props-json (org.apache.solr.common.util.Utils/toJSONString props)]
        (parse-string props-json))
      (throw (IllegalStateException. "Missing #'cheshire.core/parse-string")))))


(defn link-configset-to-collection
  [zkhost configset collection & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (SolrZkClient.  zkhost timeout)]
    (let [path (str org.apache.solr.common.cloud.ZkStateReader/COLLECTIONS_ZKNODE "/" collection)
          data (.getData client path nil nil true)
          props (into {} (.getProperties (ZkNodeProps/load data)))
          props-updated (assoc props "configName" configset)]
      (.setData client path (org.apache.solr.common.util.Utils/toJSON props-updated) true))))

(defn get-system-info
  [&{:keys [as] :or {as (if json-enabled? :json :string)}}]
  (let [base-url (.getBaseURL solr/*connection*)
        base-client (.getHttpClient (solr/connect base-url))
        info-url (str base-url "/admin/info/system?wt=json")]
    (let [req (HttpGet. info-url)
          response (.execute base-client req)
          status (.getStatusCode (.getStatusLine response))
          body (EntityUtils/toString (.getEntity response))]
      (if (>= status 400)
        (throw (ex-info body {:response response :status status}))
        (case as
          :string body
          :json (if json-enabled?
                  (if-let [parse-string (get-cheshire-parse-string)]
                    (parse-string body true)
                    (throw (IllegalStateException. "Missing #'cheshire.core/parse-string")))
                  (throw (IllegalStateException. "cheshire is not loaded.")))
          body)))))

(defmulti upload-blob (fn [name data] (type data)))

(defmethod upload-blob Path [name path]
  (with-open [s (Files/newInputStream path (make-array OpenOption 0))]
    (upload-blob name s)))

(defmethod upload-blob String [name path]
  (upload-blob name (Paths/get path (make-array String 0))))

(defmethod upload-blob InputStream [name data]
  (let [base-url (.getBaseURL solr/*connection*)
        base-client (.getHttpClient (solr/connect base-url))
        upload-url (str base-url "/.system/blob/" name)]
    (let [entity (doto (InputStreamEntity. data -1)
                   (.setContentType "binary/octet-stream"))
          post (doto (HttpPost. upload-url)
                 (.setEntity entity))
          response (.execute base-client post)
          status (.getStatusCode (.getStatusLine response))
          body (EntityUtils/toString (.getEntity response))]
      (solr/trace (format "upload-blob status %d reason %s" status body))
      (if (>= status 400)
        (throw (ex-info body {:response response :status status}))
        true))))

(defn list-blobs [& {:keys [name as] :or {as :string}}]
  (let [base-url (.getBaseURL solr/*connection*)
        base-client (.getHttpClient (solr/connect base-url))
        list-url (if name
                     (str base-url "/.system/blob/" name "?omitHeader=true")
                     (str base-url "/.system/blob?omitHeader=true"))
        get (HttpGet. list-url)
        response (.execute base-client get)
        body (EntityUtils/toString (.getEntity response))]
    (case (.getStatusCode (.getStatusLine response))
      200 (case as
            :string body
            :json (if json-enabled?
                    (if-let [parse-string (get-cheshire-parse-string)]
                      (filter #(:blobName %) (get-in (parse-string body true) [:response :docs]))
                      (throw (IllegalStateException. "Missing #'cheshire.core/parse-string")))
                    (throw (IllegalStateException. "cheshire is not loaded"))))
      404 []
      (throw (ex-info (.getReasonPhrase (.getStatusLine response))
                      {:reason (.getReasonPhrase (.getStatusLine response))
                       :status (.getStatusCode (.getStatusLine response))})))))

(defn- string-post
  [client url body content-type]
  (let [post (doto (HttpPost. url)
               (.setEntity (doto (StringEntity. body)
                             (.setContentType content-type))))
        response (.execute client post)]
    (if (>= (.getStatusCode (.getStatusLine response)) 300)
      (throw (ex-info "Failed"
                      {:reason (.getReasonPhrase (.getStatusLine response))
                       :success false
                       :status (.getStatusCode (.getStatusLine response))}))
      {:success true
       :response response})))

(defn delete-blob [blob-id]
  (let [base-url (.getBaseURL solr/*connection*)
        base-client (.getHttpClient (solr/connect base-url))
        delete-url (str base-url "/.system/update?commit=true")]
    (string-post base-client delete-url
                 (format "{\"delete\" : {\"id\" : \"%s\" }}" blob-id)
                 "application/json")
    true))

(defn add-runtime-lib
  [collection blob-name version]
  (let [base-url (.getBaseURL solr/*connection*)
        base-client (.getHttpClient (solr/connect base-url))
        collection-url (str base-url (format "/%s/config" collection))]
    (string-post base-client collection-url
                 (format "{\"add-runtimelib\": {\"name\": \"%s\", \"version\": %s}}"
                         blob-name version)
                 "application/json")
    true))

(defn update-runtime-lib
  [collection blob-name version]
  (let [base-url (.getBaseURL solr/*connection*)
        base-client (.getHttpClient (solr/connect base-url))
        collection-url (str base-url (format "/%s/config" collection))]
    (string-post base-client collection-url
                 (format "{\"update-runtimelib\": {\"name\": \"%s\", \"version\": %s}}"
                         blob-name version)
                 "application/json")
    true))

(defn delete-runtime-lib
  [collection blob-name]
  (let [base-url (.getBaseURL solr/*connection*)
        base-client (.getHttpClient (solr/connect base-url))
        collection-url (str base-url (format "/%s/config" collection))]
    (string-post base-client collection-url
                 (format "{\"delete-runtimelib\": \"%s\"}" blob-name)
                 "application/json")
    true))

