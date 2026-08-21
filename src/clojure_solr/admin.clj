(ns clojure-solr.admin
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io])
  (:require [clojure-solr :as solr])
  (:require [clojure-solr.request :as req])
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
  (:import [java.util.zip ZipInputStream ZipEntry ZipOutputStream]
           [java.io File InputStream ByteArrayInputStream ByteArrayOutputStream]
           [java.nio.file Path Paths Files LinkOption OpenOption])
  )

(def json-enabled?
  (try (require 'cheshire.core)
       true
       (catch Throwable _ false)))

(defn- slurp-bytes
  "Read an InputStream fully into a byte array.  GenericSolrRequest takes a byte
   array, so where the Apache path could stream, large configsets and blobs are
   now buffered in memory."
  ^bytes [^java.io.InputStream in]
  (let [out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn get-cheshire-parse-string
  []
  (ns-resolve (symbol "cheshire.core") (symbol "parse-string")))


(defn- ->string-map
  "NamedList or Map to a Clojure map with the original string keys."
  [x]
  (cond (instance? NamedList x)
        (into {} (for [^java.util.Map$Entry e (iterator-seq (.iterator ^NamedList x))]
                   [(.getKey e) (.getValue e)]))
        (instance? java.util.Map x) (into {} x)
        :else x))

(defn- core-status-entries
  "Per-core status, read off the raw response rather than CoreAdminResponse's
   getCoreStatus.  That accessor returned a NamedList of NamedLists on SolrJ 9
   and returns a Map of typed CoreStatusResponse$SingleCoreData on SolrJ 10; the
   underlying response is a NamedList on both."
  [^CoreAdminResponse response]
  (->string-map (.get (.getResponse response) "status")))

(defn- core-status->map
  [core]
  (let [m (->string-map core)]
    (cond-> m (contains? m "index") (assoc "index" (->string-map (get m "index"))))))

(defn list-cores
  "List loaded cores in a standalone Solr.  Keys are strings, as before."
  []
  (let [^CoreAdminRequest request (doto (CoreAdminRequest.)
                                    (.setAction CoreAdminParams$CoreAdminAction/STATUS))
        ^CoreAdminResponse response (.process request solr/*connection*)]
    (for [[_ core] (core-status-entries response)]
      (core-status->map core))))

(defn get-core-status
  "Get core status in a standalone Solr"
  [name]
  (core-status->map (get (core-status-entries (CoreAdminRequest/getStatus name solr/*connection*))
                         name)))

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
  (let [resp (req/request-clj solr/*connection* :post "/admin/configs"
                              {:params (cond-> {:action "UPLOAD" :name name}
                                         (:overwrite opts) (assoc :overwrite true)
                                         (:cleanup opts)   (assoc :cleanup true)
                                         (:filePath opts)  (assoc :filePath (:filePath opts)))
                               :content (slurp-bytes zipstream)
                               :content-type "application/octet-stream"})]
    (solr/trace (format "upload-config-set %s" (pr-str resp)))
    true))

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
  

;;; ---------------------------------------------------------------------------
;;; Collection options that Solr 9 removed
;;;
;;; maxShardsPerNode and autoAddReplicas belonged to the autoscaling framework
;;; Solr 9 deleted, so CollectionAdminRequest$Create lost setMaxShardsPerNode and
;;; setAutoAddReplicas.  Unlike the ZooKeeper arities there is nothing to fall
;;; back on: the feature itself is gone.  A caller that asks for one is told so,
;;; rather than left to decode "No matching method setAutoAddReplicas found
;;; taking 2 args for class ...CollectionAdminRequest$Create".
;;; ---------------------------------------------------------------------------

(def ^:private create-request-setters
  "The method names CollectionAdminRequest$Create publishes on this SolrJ."
  (delay
    (into #{}
          (map (fn [^java.lang.reflect.Method m] (.getName m)))
          (.getMethods CollectionAdminRequest$Create))))

(defn- check-collection-option!
  "Throw unless the running SolrJ still implements the option named by setter."
  [option setter]
  (when-not (contains? @create-request-setters setter)
    (throw (ex-info (format (str "%s is not supported by the SolrJ on the classpath. "
                                 "Solr 9 removed %s along with the autoscaling framework "
                                 "the option belonged to, and there is no replacement; "
                                 "create or modify the collection without it.")
                            option setter)
                    {:option option :setter setter}))))

(defn modify-collection
  [name & {:keys [max-shards-per-node
                  replication-factor
                  auto-add-replicas?
                  config-name
                  timeout]
           :or {timeout 60}
           :as properties}]
  (when max-shards-per-node
    (check-collection-option! :max-shards-per-node "setMaxShardsPerNode"))
  (when auto-add-replicas?
    (check-collection-option! :auto-add-replicas? "setAutoAddReplicas"))
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
  ;; contains?, not the set as a function: a set answers nil for nil, so
  ;; (#{... nil} nil) is falsy and the check rejected exactly the default -- no
  ;; router name at all -- that nil is in the set to permit.
  {:pre [name num-replicas num-shards
         (contains? #{:implicit :composite-id "compositeId" "implicit" nil} router-name)]}
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
      (check-collection-option! :max-shards-per-node "setMaxShardsPerNode")
      (solr/trace (format "(.setMaxShardsPerNode %d)" max-shards-per-node))
      (.setMaxShardsPerNode create-request max-shards-per-node))
    (when (not-empty node-set)
      (solr/trace (format "(.setCreateNodeSet \"%s\")" node-set))
      (.setCreateNodeSet create-request node-set))
    (when auto-add-replicas?
      (check-collection-option! :auto-add-replicas? "setAutoAddReplicas")
      (solr/trace (format "(.setAutoAddReplicas %s)" auto-add-replicas?))
      (.setAutoAddReplicas create-request auto-add-replicas?))
    (when collection-properties
      (.setProperties create-request (map->Properties collection-properties)))
    (.processAndWait create-request solr/*connection* timeout)))

(defn get-collection-overlay
  [collection & {:keys [as]}]
  (let [resp (req/request solr/*connection* :get (str "/" collection "/config/overlay"))]
    (case as
      :string (req/->json-string resp)
      (:overlay (req/->clj resp)))))


(defn solr-zk-client-factory-builder
  []
  (let [client-constructors (.getConstructors org.apache.solr.common.cloud.SolrZkClient)]
    (if (= 1 (count client-constructors))
      ;; This is Solr 9.2.0 or later
      (eval `(fn* ([zkhost# timeout#]
                   (.build (doto (new org.apache.solr.common.cloud.SolrZkClient$Builder)
                             (.withUrl zkhost#)
                             (.withTimeout timeout# java.util.concurrent.TimeUnit/SECONDS)))))) 
      (eval `(fn* ([zkhost# timeout#] (new org.apache.solr.common.cloud.SolrZkClient zkhost#
                                           (* 1000 timeout#))))))))

(def solr-zk-client-factory-fn (atom nil))

(defn solr-zk-client-factory
  [zkhost timeout]
  (when-not @solr-zk-client-factory-fn
    (reset! solr-zk-client-factory-fn (solr-zk-client-factory-builder)))
  (@solr-zk-client-factory-fn zkhost timeout))

;;; ---------------------------------------------------------------------------
;;; ZooKeeper operations
;;;
;;; SolrJ 9 and earlier end each SolrZkClient operation with a retryOnConnLoss
;;; boolean.  SolrJ 10 dropped it -- the operations always retry -- so calling
;;; the Solr 9 arity there fails at runtime with "No matching method getData
;;; found taking 4 args for class org.apache.solr.common.cloud.SolrZkClient".
;;; The arity has to come from the class actually on the classpath.
;;;
;;; Trimming the trailing boolean is not enough for makePath, whose surviving
;;; two-argument overload means different things on either side:
;;;
;;;   SolrJ 9    makePath(path, failOnExists, retryOnConnLoss)
;;;              makePath(path, retryOnConnLoss)        -- failOnExists is true
;;;   SolrJ 10   makePath(path, failOnExists)
;;;
;;; so (.makePath client path false) creates a missing path on 10 but throws
;;; NodeExistsException on any re-upload on 9.  Each operation below therefore
;;; names the overload that carries the intended meaning on its own side rather
;;; than dropping an argument from the other one.
;;;
;;; `client` is deliberately unhinted: a SolrZkClient hint would make the
;;; compiler resolve both branches against whichever SolrJ the build sees, and
;;; the branch for the other one would not compile.
;;; ---------------------------------------------------------------------------

(def ^:private zk-retry-arg?
  "True when SolrZkClient takes the trailing retryOnConnLoss boolean, i.e. on
   SolrJ 9 and earlier.  getData is the probe because its two arities cannot be
   confused: 9 has only the four-argument form, 10 only the three-argument one."
  (delay
    (boolean (some (fn [^java.lang.reflect.Method m]
                     (and (= "getData" (.getName m))
                          (= 4 (alength (.getParameterTypes m)))))
                   (.getMethods SolrZkClient)))))

(defn- zk-make-path
  "Create path and any missing parent, tolerating a path that already exists."
  [client path]
  (if @zk-retry-arg?
    (.makePath client path false true)
    (.makePath client path false)))

(defn- zk-set-data
  "Write bytes to path, whatever version is there."
  [client path bytes]
  (if @zk-retry-arg?
    (.setData client path bytes true)
    (.setData client path bytes)))

(defn- zk-get-data
  "The bytes at path, with no watcher and no stat."
  [client path]
  (if @zk-retry-arg?
    (.getData client path nil nil true)
    (.getData client path nil nil)))

(defn- zk-delete
  "Delete path at version; -1 deletes whatever version is there."
  [client path version]
  (if @zk-retry-arg?
    (.delete client path version true)
    (.delete client path version)))

(defn- zk-get-children
  "The child names at path, with no watcher."
  [client path]
  (if @zk-retry-arg?
    (.getChildren client path nil true)
    (.getChildren client path nil)))

(defn- zk-exists?
  "Whether path exists."
  [client path]
  (if @zk-retry-arg?
    (.exists client path true)
    (.exists client path)))

(defn upload-to-zookeeper
  [zkhost path bytes & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (solr-zk-client-factory  zkhost timeout)]
    (zk-make-path client path)
    (zk-set-data client path bytes)))

(defn download-from-zookeeper
  [zkhost path & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (solr-zk-client-factory  zkhost timeout)]
    (zk-get-data client path)))

(defn delete-from-zookeeper
  [zkhost path version & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (solr-zk-client-factory  zkhost timeout)]
    (zk-delete client path version)))

(defn list-zk-children
  "List the child znodes at a given path in ZooKeeper.
   Returns a java.util.List of child node names (not full paths)."
  [zkhost path & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (solr-zk-client-factory zkhost timeout)]
    (vec (zk-get-children client path))))

(defn zk-path-exists?
  "Check if a path exists in ZooKeeper."
  [zkhost path & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (solr-zk-client-factory zkhost timeout)]
    (zk-exists? client path)))

(defn- copy-zk-tree
  "Recursively copy a ZK tree from source to target.
   progress-fn, if provided, is called with each path copied."
  [source-zkhost target-zkhost path timeout progress-fn]
  (let [data (try (download-from-zookeeper source-zkhost path :timeout timeout)
               (catch Exception _ nil))
        children (try (list-zk-children source-zkhost path :timeout timeout)
                   (catch Exception _ []))]
    (when data
      (upload-to-zookeeper target-zkhost path data :timeout timeout)
      (when progress-fn (progress-fn path)))
    (when (and (nil? data) (seq children))
      ;; Create the parent znode even if it has no data
      (upload-to-zookeeper target-zkhost path (byte-array 0) :timeout timeout))
    (doseq [child children]
      (copy-zk-tree source-zkhost target-zkhost
                    (str path "/" child) timeout progress-fn))))

(defn clone-zookeeper
  "Clone Solr's ZooKeeper state from a source ZK to a target ZK.
   Copies configsets, collection state, and security.json.

   source-zkhost - connection string for the source ZK (e.g., '127.0.0.1:9983')
   target-zkhost - connection string for the target ZK (e.g., 'localhost:2181')

   Options:
     :timeout     - ZK client timeout in seconds (default: 60)
     :progress-fn - called with each ZK path as it's copied (e.g., println)
     :include     - set of keywords for what to copy (default: #{:configsets :collections :security})

   Returns a map summarizing what was copied."
  [source-zkhost target-zkhost
   & {:keys [timeout progress-fn include]
      :or {timeout 60
           include #{:configsets :collections :security}}}]
  (let [report (atom {:configsets [] :collections [] :security false})]

    ;; Copy configsets
    (when (:configsets include)
      (let [configsets (try (list-zk-children source-zkhost "/configs" :timeout timeout)
                         (catch Exception _ []))]
        (doseq [cs configsets]
          (when progress-fn (progress-fn (str "Copying configset: " cs)))
          (copy-zk-tree source-zkhost target-zkhost
                        (str "/configs/" cs) timeout progress-fn)
          (swap! report update :configsets conj cs))))

    ;; Copy collection state
    (when (:collections include)
      (let [collections (try (list-zk-children source-zkhost "/collections" :timeout timeout)
                          (catch Exception _ []))]
        (doseq [coll collections]
          (when progress-fn (progress-fn (str "Copying collection state: " coll)))
          (copy-zk-tree source-zkhost target-zkhost
                        (str "/collections/" coll) timeout progress-fn)
          (swap! report update :collections conj coll))))

    ;; Copy security.json
    (when (:security include)
      (when (zk-path-exists? source-zkhost "/security.json" :timeout timeout)
        (when progress-fn (progress-fn "Copying security.json"))
        (let [data (download-from-zookeeper source-zkhost "/security.json" :timeout timeout)]
          (upload-to-zookeeper target-zkhost "/security.json" data :timeout timeout)
          (swap! report assoc :security true))))

    @report))

(defn get-collection-properties
  [zkhost collection & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (solr-zk-client-factory  zkhost timeout)]
    (if-let [parse-string (get-cheshire-parse-string)]
      (let [path (str org.apache.solr.common.cloud.ZkStateReader/COLLECTIONS_ZKNODE "/" collection)
            data (zk-get-data client path)
            props (into {} (.getProperties (ZkNodeProps/load data)))
            props-json (org.apache.solr.common.util.Utils/toJSONString props)]
        (parse-string props-json))
      (throw (IllegalStateException. "Missing #'cheshire.core/parse-string")))))


(defn link-configset-to-collection
  [zkhost configset collection & {:keys [timeout] :or {timeout 60}}]
  (with-open [client (solr-zk-client-factory  zkhost timeout)]
    (let [path (str org.apache.solr.common.cloud.ZkStateReader/COLLECTIONS_ZKNODE "/" collection)
          data (zk-get-data client path)
          props (into {} (.getProperties (ZkNodeProps/load data)))
          props-updated (assoc props "configName" configset)]
      (zk-set-data client path (org.apache.solr.common.util.Utils/toJSON props-updated)))))

(defn get-system-info
  [&{:keys [as] :or {as (if json-enabled? :json :string)}}]
  (let [resp (req/request solr/*connection* :get "/admin/info/system")]
    (case as
      :string (req/->json-string resp)
      (req/->clj resp))))

(defmulti upload-blob (fn [name data] (type data)))

(defmethod upload-blob Path [name path]
  (with-open [s (Files/newInputStream path (make-array OpenOption 0))]
    (upload-blob name s)))

(defmethod upload-blob String [name path]
  (upload-blob name (Paths/get path (make-array String 0))))

(defmethod upload-blob InputStream [name data]
  (let [resp (req/request-clj solr/*connection* :post (str "/.system/blob/" name)
                              {:content (slurp-bytes data)
                               :content-type "application/octet-stream"})]
    (solr/trace (format "upload-blob %s" (pr-str resp)))
    true))

(defn list-blobs [& {:keys [name as] :or {as :string}}]
  (let [path (if name (str "/.system/blob/" name) "/.system/blob")
        resp (try (req/request solr/*connection* :get path {:params {:omitHeader true}})
                  ;; No .system collection means no blob store; that is an empty
                  ;; list, not an error.  RemoteSolrException moved to a new class
                  ;; in Solr 10, but both extend SolrException and carry .code.
                  (catch org.apache.solr.common.SolrException e
                    (if (= 404 (.code e)) ::missing (throw e))))]
    (if (= resp ::missing)
      []
      (case as
        :string (req/->json-string resp)
        (filter :blobName (get-in (req/->clj resp) [:response :docs]))))))

(defn- config-post
  "POST a JSON config command to a collection's /config endpoint."
  [collection json]
  (req/request solr/*connection* :post (format "/%s/config" collection)
               {:content (.getBytes ^String json "UTF-8")
                :content-type "application/json"})
  true)

(defn delete-blob [blob-id]
  (req/request solr/*connection* :post "/.system/update"
               {:params {:commit true}
                :content (.getBytes (format "{\"delete\" : {\"id\" : \"%s\" }}" blob-id) "UTF-8")
                :content-type "application/json"})
  true)

(defn add-runtime-lib
  [collection blob-name version]
  (config-post collection
               (format "{\"add-runtimelib\": {\"name\": \"%s\", \"version\": %s}}"
                       blob-name version)))

(defn update-runtime-lib
  [collection blob-name version]
  (config-post collection
               (format "{\"update-runtimelib\": {\"name\": \"%s\", \"version\": %s}}"
                       blob-name version)))

(defn delete-runtime-lib
  [collection blob-name]
  (config-post collection
               (format "{\"delete-runtimelib\": \"%s\"}" blob-name)))

