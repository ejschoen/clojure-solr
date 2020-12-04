(ns clojure-solr.security
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest NoSuchAlgorithmException SecureRandom]
           [java.util Random]
           [org.apache.commons.codec.binary Base64]))

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

(defmethod hash-password nil [password]
  (hash-password 16))

(defmethod hash-password Long [len]
  (hash-password (random-str len)))

(defmethod hash-password Integer [len]
  (hash-password (random-str len)))

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
        
(def standard-roles ["security-edit"
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
            
(defn make-security-json-data
  "names-passwords-and-roles is a vector of triplets containing
   name, password parameter (can be nil, a length, a string, or a byte array)
   and a role for each identity.
   roles-and-permissions is a vector of [permission role] entries, per the
   Solr RuleBasedAuthorizationPlugin permissions field.
   Returns a map with 3 keys: :credentials, :authorization, and :authentication.
   :authorization and :authentication can be passed directly into security.json.
   to configure Basic Authentication.  :credentials contains entries for each
   identity in names-passwords-and-roles.  :cleartext-password, :hashed-password,
   and :salt.  The cleartext password and the salt need to be saved somewhere
   to be used with basic authentication."
  [names-passwords-and-roles roles-and-permissions]
  (let [credentials (into {}
                          (for [[name password _] names-passwords-and-roles]
                            [name (hash-password password)]))]
    {:credentials credentials
     :authorization {:permissions roles-and-permissions
                     :user-role (into {}
                                      (for [[permission _ role] names-passwords-and-roles]
                                        [permission role]))
                     :class "solr.RuleBasedAuthorizationPlugin"}
                                        
     :authentication {:class "solr.BasicAuthPlugin"
                      :blockUnknown true
                      :credentials (into {}
                                         (for [[name creds] credentials]
                                           [name (str (:hashed-password creds) " " (:salt creds))]))}}))


                     
        
