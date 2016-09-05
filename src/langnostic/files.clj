(ns langnostic.files
  (:require [markdown.core :as md]

            [clojure.java.io :as io]))

(def resources (.getCanonicalFile (io/file "resources")))

(defn file-in? [file path]
  (.startsWith (.getPath (.getCanonicalFile file))
               (.getPath (.getCanonicalFile path))))

(defn file-in-resources? [file]
  (file-in? file resources))

(defn linkify-headers [text state]
  [(if (or (state :heading) (state :inline-heading))
     (let [[_ hn text] (re-find #"<(h\d)>(.*?)</\1>" text)
           name (clojure.string/replace (.toLowerCase text) #"\W+" "-")]
       (str "<" hn ">"
            "<a name=\"" name "\"></a>"
            "<a href=\"#" name"\">"
            text
            "</a></" hn ">"))
     text)
   state])

(defn file-content [fname]
  (md/md-to-html-string
   (slurp fname)
   :custom-transformers [linkify-headers]
   :replacement-transformers (remove #(= % markdown.common/dashes) markdown.transformers/transformer-vector)
   :footnotes? true))
