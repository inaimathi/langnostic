(ns langnostic.posts
  (:require [markdown.core :as md]
            [cheshire.core :as json]
            [clj-time.coerce :as time]
            [clojure-watch.core :as watch]

            [clojure.java.io :as io]

            [langnostic.files :as files]))

(def posts (atom []))

(defn load-posts! []
  (reset!
   posts
   (vec
    (map #(let [raw (json/parse-string % (fn [k] (keyword (.toLowerCase k))))]
            (assoc
             (dissoc raw :edited)
             :posted (time/from-long (long (* 1000 (raw :posted))))
             :tags (set (raw :tags))
             :content (atom nil)))
         (line-seq (io/reader "resources/posts.json"))))))

(defn all-posts []
  @posts)

(defn find-by-slug [slug]
  (first (filter #(= slug (% :file)) @posts)))

(defn find-by-tag [tag]
  (filter #(some #{tag} (% :tags)) @posts))

(defn post-content [post]
  (when (nil? @(post :content))
    (reset! (post :content) (files/file-content (str "resources/posts/" (post :file) ".md"))))
  @(post :content))

;;;;; Post initialization
(load-posts!)

(watch/start-watch
 [{:path "resources/"
   :event-types [:modify]
   :callback (fn [event filename]
               (when (and (= :modify event)
                          (= "resources/posts.json"))
                 (println "Reloading posts.json ...")
                 (load-posts!)))}
  {:path "resources/posts/"
   :event-types [:modify]
   :callback (fn [event filename]
               (let [name (.getName (io/file filename))
                     slug (.substring name 0 (- (count name) 3))
                     post (find-by-slug slug)]
                 (when post
                   (println "Poking cache for" slug "...")
                   (reset! (post :content) nil))))}])
