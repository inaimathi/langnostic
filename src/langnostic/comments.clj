(ns langnostic.comments
  (:require [codax.core :as db]

            [langnostic.conf :as conf]))

(def DB (db/open-database! conf/DB-PATH))

(defn now! []
  (quot (System/currentTimeMillis) 1000))

(defn get-comments-for [post-id]
  (db/get-at! DB [:posts post-id :comments]))

(defn post-comment!
  ([user post-id content]
   (db/with-write-transaction [DB tx]
     (let [path [(count (db/get-at tx [:posts post-id :comments]))]]
       (db/update-at
        tx [:posts post-id :comments]
        #(assoc % (first path) {:user user :path path :content content :replies []})))))
  ([user post-id comment-path content]
   (db/with-write-transaction [DB tx]
     (let [target (conj comment-path :replies)
           new-ix (count (db/get-at tx (concat [:posts post-id :comments] target)))
           path (conj target new-ix)]
       (db/update-at
        tx [:posts post-id :comments]
        (fn [comments]
          (update-in comments target #(conj % {:user user :path path :content content :replies []}))))))))

(defn edit-comment!
  [user comment-path new-content]
   nil)
