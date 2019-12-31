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
    {:name (.getFullName user) :email (.getEmail user) :verified? (.getIsEmailVerified user)
     :url (.getUrl user) :about (.getAbout user) :image (.getImageUrl user)
     :like-count (.getLikeCount user)
     :vanity (.getVanity user) :pledges (.getPledges user)}))
