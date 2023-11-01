(ns langnostic.posts
  (:require [cheshire.core :as json]
            [clj-time.coerce :as time]

            [clojure.java.io :as io]

            [langnostic.files :as files]))

(def posts (atom []))

(defn parse-post [old line]
  (let [raw (json/parse-string line (fn [k] (keyword (.toLowerCase k))))]
    (assoc
     (dissoc raw :edited)
     :posted (time/from-long (long (* 1000 (raw :posted))))
     :tags (set (raw :tags))
     :audio? (.exists (io/as-file (str "resources/public/audio/" (:file raw) ".ogg")))
     :content (if-let [p (get old (raw :id))]
                (p :content)
                (atom nil)))))

(defn load-posts! []
  (swap!
   posts
   (fn [old]
     (try
       (vec (map (partial parse-post old) (line-seq (io/reader "resources/posts.json"))))
       (catch Exception e old)))))

(defn all-posts []
  @posts)

(defn find-by-slug [slug]
  (first (filter #(= slug (% :file)) @posts)))

(defn find-by-tag [tag]
  (filter #(some #{tag} (% :tags)) @posts))

(defn post-content [post]
  (when (nil? @(post :content))
    (println "READING POST CONTENT" (str "resources/posts/" (post :file) ".md"))
    (reset! (post :content) (files/file-content (str "resources/posts/" (post :file) ".md"))))
  @(post :content))
