(ns langnostic.scratch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]))

(defn next-id []
  (-> "posts.json" io/resource io/reader line-seq last json/decode (get "id") inc))

(defn all-tags []
  (->> "posts.json" io/resource io/reader line-seq
       (map #(json/parse-string % (fn [k] (keyword (.toLowerCase k)))))
       (mapcat :tags) frequencies
       (into (sorted-map))))

(defn slug-from-file [file] (str/replace (.getName file) #".md$" ""))

(defn title-from-file [file]
  (->> (str/split (slug-from-file file) #"-")
       (map (fn [word]
              (if (#{"and" "of"} word)
                word
                (str/capitalize word))))
       (str/join " ")))

(defn drafts-by-prefix [prefix]
  (->> (io/file "drafts")
       file-seq
       (filter
        (fn [f]
          (and
           (str/starts-with? (.getName f) prefix)
           (not (str/ends-with? (.getName f) "~")))))))

(defn new [file-prefix & {:keys [title tags] :or {tags []}}]
  (let [drafts (drafts-by-prefix file-prefix)]
    (if (= 1 (count drafts))
      (let [file (first drafts)
            slug (slug-from-file file)
            title (or title (title-from-file file))
            record {:id (next-id) :title title
                    :file (slug-from-file file)
                    :posted (quot (System/currentTimeMillis) 1000)
                    :tags tags}]
        (io/copy file (io/file (io/resource "posts") (.getName file)))
        (io/delete-file file)
        (spit (io/file (io/resource "posts.json")) (str (json/encode record) "\n") :append true))
      (println "MULTIPLE DRAFTS FOUND" drafts))))
