(ns langnostic.auth
  (:require [oauth.patreon :as pat]
            [oauth.github :as git]

            [langnostic.conf :as conf]))

(def ^:dynamic USER nil)

(defn redirect-url [provider]
  (str conf/REDIRECT-URL provider))

(def login-url
  {"patreon" (pat/oauth-authorization-url (conf/patreon :id) (redirect-url "patreon"))
   "github" (git/oauth-authorization-url (conf/github :id) (redirect-url "github"))})

(defmulti authenticate! (fn [provider code] provider))
(defmethod authenticate! :default
  [provider code]
  (println "UNKNOWN PROVIDER" provider code))

(defmethod authenticate! "patreon"
  [provider code]
  (let [client (new com.patreon.PatreonOAuth (conf/patreon :id) (conf/patreon :secret) (redirect-url "patreon"))
        toks (.getTokens client code)
        api-client (new com.patreon.PatreonAPI (.getAccessToken toks))
        user (.get (.fetchUser api-client))]
    {:site "patreon"
     :name (.getFullName user) :url (.getUrl user)
     :image (.getImageUrl user) :thumbnail (.getThumbUrl user)
     :pledges (.getPledges user)}))

(defmethod authenticate! "github"
  [provider code]
  (let [token (git/oauth-access-token (conf/github :id) (conf/github :secret) code (redirect-url "github"))
        client (git/oauth-client (:access-token token))
        user (client {:url "https://api.github.com/user" :method :get})]
    {:site "github"
     :name (:login user) :url (:html-url user)
     :image (:avatar-url user) :thumbnail (:avatar-url user)
     :inaimathi-follows?
     (try
       ;; The github API returns no content (therefore nil) whether it's true or not
       ;; The only difference is in the return code. A true comes back as 203,
       ;; while a false is 404 (and therefore an error here)
       (client {:url (str "https://api.github.com/users/inaimathi/following/" (:login user)) :method :get})
       true
       (catch Exception e
         false))
     :following (set (map :login (client {:url (:followers-url user) :method :get})))
     :organizations (set (map :login (client {:url (:organizations-url user) :method :get})))}))
