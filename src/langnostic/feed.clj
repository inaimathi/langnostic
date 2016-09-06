(ns langnostic.feed
  (:require [hiccup.core :as hic]
            [hiccup.page :as pg]

            [langnostic.page :as page]))

(defn atom-feed [posts]
  (str "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
       (hic/html
        [:feed {:xml:lang "en-US" :xmlns "http://www.w3.org/2005/Atom"}
         [:title "Language Agnostic"]
         [:subtitle "Langnostic Atom Feed"]
         [:link {:href "http://langnostic.inaimathi.ca/feed/atom" :rel "self"}]
         [:link {:href "http://langnostic.inaimathi.ca"}]
         (map (fn [post]
                [:entry
                 [:title (post :title)]
                 [:updated (post :posted)]
                 [:link {:href (str "http://langnostic.inaimathi.ca/posts/" (post :file))}]
                 [:author [:name "inaimathi"]]
                 [:content {:type "html"} (hic/h (post :content))]])
              (take 10 (reverse (sort-by :posted posts))))])))
