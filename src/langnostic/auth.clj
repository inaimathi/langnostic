(ns langnostic.auth
  (:require [oauth.patreon :as pat]

            [langnostic.conf :as conf]))


(defn redirect-url [provider]
  (str conf/REDIRECT-URL provider))

(def login-url
  {"patreon" (pat/oauth-authorization-url (conf/patreon :id) (redirect-url "patreon"))})

(defmulti authenticate! (fn [provider code] provider))
(defmethod authenticate! "patreon"
  [provider code]
  (let [client (new com.patreon.PatreonOAuth (conf/patreon :id) (conf/patreon :secret) (redirect-url "patreon"))
        toks (.getTokens client code)
        api-client (new com.patreon.PatreonAPI (.getAccessToken toks))
        user (.get (.fetchUser api-client))]
    {:site "patreon"
     :name (.getFullName user) :email (.getEmail user) :url (.getUrl user)
     :image (.getImageUrl user) :thumbnail (.getThumbUrl user)
     :pledges (.getPledges user)}))
