(ns clojure-solr.security
  (:use [clojure-solr :only [*connection*]])
  (:require [clojure.pprint :as pprint])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest NoSuchAlgorithmException SecureRandom]
           [java.util Random]
           [org.apache.commons.codec.binary Base64]
           [org.apache.solr.client.solrj.impl HttpSolrClient]
           [org.apache.http StatusLine HttpResponse]
           [org.apache.http.client HttpClient]
           [org.apache.http.client.methods HttpPost]
           [org.apache.http.entity ByteArrayEntity]
           [org.apache.http.util EntityUtils]))

(def ^:private VALID-CHARS
  (concat
   [\! \@ \# \$ \% \^ \& \*]
   (map char (concat (range 48 58) ; 0-9
                     (range 65 91) ; A-Z
                     (range 97 123))))) ; a-z

(defn- random-char []
  (nth VALID-CHARS (rand (count VALID-CHARS))))

(defn- random-str [len]
  ;; Iterate 'til we find a valid password.  
  ;; Crossing fingers here...
  (apply str (take len (repeatedly random-char))))

(defmulti hash-password (fn [password] (type password)))

(def ^:private max-length 32)
(def ^:private default-length 16)

(defmethod hash-password nil [password]
  (hash-password default-length))

(defmethod hash-password Long [len]
  (if (<= len max-length)
    (hash-password (random-str len))
    (hash-password (str len))))

(defmethod hash-password Integer [len]
  (if (<= len max-length)
    (hash-password (random-str len))
    (hash-password (str len))))

(defmethod hash-password String [password]
  (assoc (hash-password (.getBytes password StandardCharsets/UTF_8))
         :cleartext-password password))
  

(defn generate-salted-hash
  [password salt]
  (let [^MessageDigest digest (MessageDigest/getInstance "SHA-256")]
    (.reset digest)
    (.update digest salt)
    (let [btpass (.digest digest password)]
      (.reset digest)
      (let [btpass2 (.digest digest btpass)]
        {:hashed-password (Base64/encodeBase64String btpass2)
         :salt (Base64/encodeBase64String salt)
         :cleartext-password password}))))

;; Generate a hashed password and salt for Solr BasicAuthenticationPlugin.
;; See: https://www.planetcobalt.net/sdb/solr_password_hash.shtml for details."
(defmethod hash-password (Class/forName "[B")
  [password]  
  (let [^SecureRandom randomizer (SecureRandom.)
        salt (make-array Byte/TYPE 32)]
    (.nextBytes randomizer salt)
    (generate-salted-hash password salt)))
        
(def standard-permissions
  '["security-edit"
   "security-read"
   "schema-edit"
   "schema-read"
   "config-edit"
   "config-admin-edit"
   "config-admin-read"
   "config-read"
   "collection-admin-edit"
   "collection-admin-read"
   "update"
   "read"
   "all"])
            
(defn- is-password-map?
  [m]
  (and (map? m)
       (or (:cleartext-password m) (:basic-auth m))
       (:hashed-password m)
       (:salt m)))

(defn make-security-data
  "In the monadic form, security-json-plus is the conventional Clojure
   representation of Solr's security.json (i.e., keyword keys) with
   authorization and authentication entries. For the
     {:authentication {:credentials {\"user\" \"password\" ...}}} 
   entries, the password can be nil, a number, a string, or a map 
   containing the :salt, :hashed-password, and :cleartext-password entry 
   or the user:password value as :basic-auth.  
   
     Given a string password or a basic-auth user:password combo, 
     the function generates a new salt and hash.  

     Given nil or a number, the function generates a random password
     of the default length or the requested length, and then generates 
     a new salt and hash.

   In the dyadic form, 
     users-passwords-and-roles is a vector of maps containing
     user, password, and role entries, where password can be nil, 
     a length, a string, or a byte array).

     roles-and-permissions is a map of authorizations entries, per the
     Solr RuleBasedAuthorizationPlugin permissions field.

   In either case, returns a map with 3 keys: :credentials, :authorization, 
   and :authentication.
     :authorization and :authentication can be passed directly into security.json.
     to configure Basic Authentication.  

     :credentials contains entries for each identity in users-passwords-and-roles.
     :cleartext-password, :hashed-password, and :salt.  
     The cleartext passwords need to be saved somewhere to be used with
     basic authentication as provided by clojure-solr/set-credentials.

     (spit \"$SOLR_HOME/security.json\" 
           (cheshire.core/generate-string
                (dissoc (make-security-data ... ...)
                        :credentials)))"
  ([security-json-plus]
   (let [permissions (get-in security-json-plus [:authorization :permissions])
         user-roles (get-in security-json-plus [:authorization :user-role])
         credentials (get-in security-json-plus [:authentication :credentials])]
     (when-not (every? (fn [[user _]] (get user-roles user)) credentials)
       (throw (Exception. "Missing role for at least one user")))
     (when-not (every? (fn [[user _]] (get credentials user)) user-roles)
       (throw (Exception. "Missing user credential for at least one user role assignment")))
     (when-not (every? (fn [[_ role-or-roles]]
                         (every? (fn [role]
                                   (some? (fn [permission]
                                            (= (:role permission) role))
                                          permissions))
                                 (if (string? role-or-roles)
                                   [role-or-roles]
                                   role-or-roles)))
                       user-roles)
       (throw (Exception. "Missing role definition for at least one user role")))
     (let [solr-credentials (for [[user password-spec] credentials
                                  :let [password-data (if (is-password-map? password-spec)
                                                        password-spec
                                                        (hash-password password-spec))]]
                              [user (assoc password-data
                                           :basic-auth (Base64/encodeBase64String
                                                        (.getBytes
                                                         (str user ":" (:cleartext-password password-data)))))])]
       {:authorization (:authorization security-json-plus)
        :authentication {:class "solr.BasicAuthPlugin"
                         :blockUnknown true
                         :credentials (into {}
                                            (for [[user creds] solr-credentials]
                                              [user (str (:hashed-password creds) " " (:salt creds))]))}
        :credentials solr-credentials})))
  ([users-passwords-and-roles roles-and-permissions]
   (let [credentials (into {}
                           (for [{:keys [user password]} users-passwords-and-roles
                                 :let [password-data (if (and (map? password)
                                                              (not-empty (:cleartext-password password))
                                                              (not-empty (:hashed-password password))
                                                              (not-empty (:salt password)))
                                                       password
                                                       (hash-password password))]]
                             [user (assoc password-data
                                          :basic-auth (Base64/encodeBase64String
                                                       (.getBytes
                                                        (str user ":" (:cleartext-password password-data)))))]))]
     {:credentials credentials
      :authorization {:permissions (into []
                                         (for [{:keys [collection] :as r-and-p} roles-and-permissions]
                                           (if (= collection "")
                                             (assoc r-and-p :collection nil)
                                             r-and-p)))
                      :user-role (into {}
                                       (for [{:keys [user roles role]} users-passwords-and-roles]
                                         [user (or (not-empty roles) role)]))
                      :class "solr.RuleBasedAuthorizationPlugin"}
      :authentication {:class "solr.BasicAuthPlugin"
                       :blockUnknown true
                       :credentials (into {}
                                          (for [[user creds] credentials]
                                            [user (str (:hashed-password creds) " " (:salt creds))]))}})))

;; Note that we don't support clojure maps.  Need to add an implementation of ToBytesPayload
;; for clojure.lang.PersistentArrayMap and/or clojure.lang.SortedArrayMap.
;; Not done here to avoid a dependency on e.g., cheshire.
(defprotocol ToBytesPayload
  (to-bytes [_]))

(extend-protocol ToBytesPayload
  String (to-bytes [s] (.getBytes s "UTF-8")))

(extend (Class/forName "[B")
  ToBytesPayload
  {:to-bytes identity})


(defn security-request [endpoint body]
  "Make a request to Solr's /admin/authentication or /admin/authorization endpoint.
   Body can be a string, byte array, or type that implements
   ToBytesPayload."
  (let [^HttpClient client (.getHttpClient *connection*)
        [_ solr-server-url] (re-matches #"(https?://.+/solr)(?:/.+)?" (.getBaseURL *connection*))]
    (if solr-server-url
      (let [^HttpPost post (doto (HttpPost. (str solr-server-url endpoint))
                             (.setHeader "Content-Type" "application-json")
                             (.setEntity (ByteArrayEntity. (to-bytes body))))
            ^HttpResponse response (.execute client post)]
        (if (>= (.getStatusCode ^StatusLine (.getStatusLine response)) 400)
          (let [entity (.getEntity response)
                content-type (.getValue (.getContentType entity))
                [_ content-type-basic] (re-matches #"([^;]+)(?:;.+)?" content-type) ]
            (throw (ex-info "Solr request failure"
                            {:status (.getStatusCode (.getStatusLine response))
                             :content-type content-type
                             :body (if (#{"text/plain" "text/html" "application/json"} content-type-basic)
                                     (EntityUtils/toString entity)
                                     (EntityUtils/toByteArray entity))})))
          true)))))

(defn authentication-request
  "Make a request to Solr's admin/authentication endpoint.
   Body can be a string, byte array, or type that implements
   ToBytesPayload."
  [body]
  (security-request "/admin/authentication" body))

(defn authorization-request
  "Make a request to Solr's admin/authorization endpoint.
   Body can be a string, byte array, or type that implements
   ToBytesPayload."
  [body]
  (security-request "/admin/authorization" body))
    
