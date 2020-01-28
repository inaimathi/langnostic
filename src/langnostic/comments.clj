(ns langnostic.comments
  (:require [codax.core :as db]

            [langnostic.conf :as conf]))

(def DB (db/open-database! conf/DB-PATH))

(defn now! []
  (quot (System/currentTimeMillis) 1000))

(defn- post-path [post-id] [:posts post-id :comments])

(defn get-comments-for [post-id]
  (->> (db/get-at! DB (post-path post-id))
       vec (sort-by first) (map second) vec))

(defn comment-exists? [post-id path]
  (if-let [res (db/get-at! DB (concat (post-path post-id) path))]
    (and (map? res) (= #{:path :content :replies :user} (set (keys res))))))

(defn post-comment!
  ([user post-id content]
   (db/with-write-transaction [DB tx]
     (let [path [(count (db/get-at tx (post-path post-id)))]]
       (db/update-at
        tx (post-path post-id)
        #(assoc % (first path) {:user user :path path :content content :replies []})))))
  ([user post-id comment-path content]
   (db/with-write-transaction [DB tx]
     (let [target (conj comment-path :replies)
           new-ix (count (db/get-at tx (concat (post-path post-id) target)))
           path (conj target new-ix)]
       (db/update-at
        tx (post-path post-id)
        (fn [comments]
          (update-in comments target #(conj % {:user user :path path :content content :replies []}))))))))

(defn edit-comment!
  [user comment-path new-content]
   nil)
