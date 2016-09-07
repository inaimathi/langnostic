(ns langnostic.posts
  (:require [markdown.core :as md]
            [cheshire.core :as json]
            [clj-time.coerce :as time]

            [clojure.java.io :as io]

            [langnostic.files :as files]))

(def posts (atom []))

(defn load-posts! []
  (swap!
   posts
   (fn [old]
     (vec
      (map #(let [raw (json/parse-string % (fn [k] (keyword (.toLowerCase k))))]
              (assoc
               (dissoc raw :edited)
               :posted (time/from-long (long (* 1000 (raw :posted))))
               :tags (set (raw :tags))
               :content (if-let [p (get old (raw :id))]
                          (p :content)
                          (atom nil))))
           (line-seq (io/reader "resources/posts.json")))))))

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
