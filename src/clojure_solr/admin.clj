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
            CollectionAdminRequest$SplitShard]
           [org.apache.solr.client.solrj.response
            CollectionAdminResponse])
  (:import [org.apache.solr.common.cloud SolrZkClient])
  (:import [org.apache.http.client HttpClient]
           [org.apache.http.client.methods HttpPost HttpGet]
           [org.apache.http.entity InputStreamEntity ContentType]
           [org.apache.http HttpRequest]
           [org.apache.http.util EntityUtils]
           )
  (:import [java.util.zip ZipInputStream ZipEntry ZipOutputStream]
           [java.io File InputStream ByteArrayInputStream ByteArrayOutputStream]
           [java.nio.file Path Paths Files LinkOption OpenOption])
  )

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
      :or {:config-file "conf/solrconfig.xml"
           :schema-file "conf/schema.xml"
           :data-dir "data"
           :transaction-log-dir nil}}]
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
  (fn [name config] (type config)))

(defmethod upload-config-set :default
  [name location]
  (throw (IllegalArgumentException. (format "%s is not a valid type.  Only java.io.File, java.nio.file.Path, and InputStream"))))

(defmethod upload-config-set String
  [name directory-name]
  (upload-config-set name (Paths/get directory-name (make-array String 0))))

(defmethod upload-config-set File
  [name file]
  (if (.isDirectory file)
    (upload-config-set name (.toPath file))
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
  [name path]
  (if-not (Files/isDirectory path (make-array LinkOption 0))
    (throw (IllegalArgumentException. (format "%s is not a directory" (str path))))
    (with-open [zip-in (Path->ZipInputStream path)]
      (upload-config-set name zip-in))))

(defmethod upload-config-set InputStream
  [name zipstream]
  (let [base-url (.getBaseURL solr/*connection*)
        _ (println "**** Base URL:" base-url)
        base-client (.getHttpClient (solr/connect base-url))
        upload-url (str base-url "/admin/configs?action=UPLOAD&name=" name)]
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
        
    
(defn get-cluster-status
  "Get status of a cluster"
  [& {:keys [collection-name route-key shard-name]}]
  (let [request (CollectionAdminRequest$ClusterStatus.)
        response (.request solr/*connection* request)
        cluster (.get response "cluster")
        live-nodes (.get cluster "live_nodes")]
    live-nodes)
  #_(let [^CollectionAdminRequest$ClusterStatus status-request (doto (CollectionAdminRequest/getClusterStatus)
                                                               (cond-> collection-name (.setCollectionName collection-name))
                                                               (cond-> route-key (.setRouteKey route-key))
                                                               (cond-> shard-name (.setShardName shard-name)))
        ^CollectionAdminResponse response (.process status-request solr/*connection*)]
    response))


(defn list-collections
  []
  (CollectionAdminRequest/listCollections  solr/*connection*))
  
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
         (#{:implicit :composite-id nil} router-name)]}
  (let [create-request (if (not-empty config-name)
                         (CollectionAdminRequest/createCollection name config-name num-shards num-replicas)
                         (CollectionAdminRequest/createCollection name num-shards num-replicas))]
    (doto create-request
      (cond-> (= :router-name :implicit) (.setRouterName "implicit"))
      (cond-> (= :router-name :composite-id) (.setRouterName "compositedId"))
      (cond-> (not-empty router-field) (.setRouterField router-field))
      (cond-> (not-empty shards) (.setShards shards))
      (cond-> replication-factor (.setReplicationFactor replication-factor))
      (cond-> nrt-replicas (.setNrtReplicas nrt-replicas))
      (cond-> pull-replicas (.setPullReplicas pull-replicas))
      (cond-> tlog-replicas (.setTlogReplicas tlog-replicas))
      (cond-> max-shards-per-node (.setMaxShardsPerNode max-shards-per-node))
      (cond-> node-set (.setCreateNodeSet node-set))
      (cond-> (not (nil? auto-add-replicas?)) (.setAutoAddReplicas auto-add-replicas?))
      (cond-> collection-properties (.setProperties (map->Properties collection-properties))))
    (.processAndWait create-request solr/*connection* timeout)))


(defn upload-to-zookeeper
  [zkhost path bytes & {:keys [timeout] :or {timeout 60}}]
  (let [client (SolrZkClient.  zkhost timeout)]
    (.setData client path bytes true)))

(defn download-from-zookeeper
  [zkhost path & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (SolrZkClient.  zkhost timeout)]
    (.getData client path nil nil true)))

(defn delete-from-zookeeper
  [zkhost path version & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (SolrZkClient.  zkhost timeout)]
    (.delete client path version true)))

