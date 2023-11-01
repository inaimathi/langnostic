(ns langnostic.pages
  (:require [markdown.core :as md]
            [hiccup.page :as pg]
            [clj-time.format :as fmt]
            [ring.util.codec :as cod]
            [cheshire.core :as json]

            [clojure.java.io :as io]

            [langnostic.posts :as posts]))

(defn post-href [post]
  (str "/posts/" (post :file)))

(defn post-audio-href [post]
  (str "/static/audio/" (:file post) ".ogg"))

(defn post-links [post]
  [:div {:class "post-nav"}
   (if-let [prev (get @posts/posts (dec (:id post)))]
     [:a {:class "prev-post" :href (post-href prev)}
      "<-" (prev :title)])
   (if-let [next (get @posts/posts (inc (:id post)))]
     [:a {:class "next-post" :href (post-href next)}
      (next :title) "->"])])

(def audio-icon
  [:svg {:class "audio-icon" :viewBox "0 0 209.50009 184.00235" :xmlns "http://www.w3.org/2000/svg"}
   [:path {:d "M 178.7,30.202356 A 103.1,103.1 0 0 0 105.5,0.0023558 h -0.8 A 104,104 0 0 0 2.6681619e-8,104.00236 v 56 A 24.1,24.1 0 0 0 24,184.00236 h 16 a 24.1,24.1 0 0 0 24,-24 v -40 A 24.1,24.1 0 0 0 40,96.002356 H 16.4 a 87.8,87.8 0 0 1 88.3,-80 h 0.1 a 88,88 0 0 1 88.3,80 h -23.6 a 24,24 0 0 0 -24,24.000004 v 40 a 24,24 0 0 0 24,24 h 16 a 24.1,24.1 0 0 0 24,-24 v -56 A 103.5,103.5 0 0 0 178.7,30.202356 Z"}]])

(defn post [post]
  [:div
   [:h1 [:a {:href (post-href post)} (:title post)]]
   [:span {:class "posted"}
    (fmt/unparse (fmt/formatter "E MMM d, Y") (:posted post))]
   (when (.exists (io/as-file (str "resources/public/audio/" (:file post) ".ogg")))
     [:a {:class "post-audio" :href (post-audio-href post) :target "blank"}
      audio-icon "Listen to this post"])
   (posts/post-content post)
   (post-links post)])

(defn latest-post []
  (post (last @posts/posts)))

(defn archive [posts]
  [:div
   [:ul (map (fn [post]
               [:li
                (when (:audio? post)
                  [:a {:class "audio-link" :href (post-audio-href post) :target "blank"}
                   audio-icon])
                [:a {:href (post-href post)} (post :title)]])
             posts)]
   [:h3 "Tags"]
   [:ul {:class "tags-list"}
    (map (fn [[tag count]]
           [:li
            [:a {:href (str "/archive/by-tag/" tag)} tag]
            "(" count ")"])
         (into (sorted-map) (frequencies (mapcat :tags posts))))]])

(defn nav-bar [section]
  [:div {:class "top-menu-container"}
   [:ul {:class "top-menu"}
    (map (fn [name]
           [:li (if (= name section)
                  name
                  [:a {:href (str "/" name)} name])])
         ["blog" "archive" "links" "meta" "tipjar" "feed"])]])

(def footer
  [:div {:class "license"}
   [:a {:rel "license" :href "http://creativecommons.org/licenses/by-sa/3.0/"}
    [:img {:alt "Creative Commons License" :style "border-width:0;float: left; margin: 0px 15px 15px 0px;"
           :src "https://i.creativecommons.org/l/by-sa/3.0/88x31.png"}]]
   [:p
    [:span {:xmlns:dct "https://purl.org/dc/terms/" :property "dct:title"}
     "all articles at langnostic"]
    " are licensed under a "
    [:a {:rel "license" :href "https://creativecommons.org/licenses/by-sa/3.0/"}
     "Creative Commons Attribution-ShareAlike 3.0 Unported License"]]
   [:p
    "Reprint, rehost and distribute freely (even for profit), but attribute the work and allow your readers the same freedoms. "
    [:a {:href "https://creativecommons.org/choose/results-one?license_code=by-sa&amp;jurisdiction=&amp;version=3.0&amp;lang=en&amp;field_format=&amp;field_worktitle=this+langnostic+article&amp;field_attribute_to_name=Inaimathi&amp;field_attribute_to_url=http%3A%2F%2Flangnostic.inaimathi.com&amp;field_sourceurl=http%3A%2F%2Flangnostic.inaimathi.com&amp;field_morepermissionsurl=&amp;lang=en_US&amp;n_questions=3"} "Here's"]
    " a license widget you can use."]
   [:p
    "The menu background image is "
    [:a {:href "https://www.flickr.com/photos/danzen/2360096926/in/photostream/"} "Jewel Wash"]
    ", taken from "
    [:a {:href "https://www.flickr.com/photos/danzen/"} "Dan Zen's"]
    " flickr stream and released under a "
    [:a {:href "https://creativecommons.org/licenses/by/2.0/"} "CC-BY license"]]])

(defn stylesheet [url]
  [:link {:rel "stylesheet" :href url :type "text/css" :media "screen"}])

(defn template [section page-title content]
  (pg/html5
   {:lang "en"}
   [:head
    [:title (str page-title " - langnostic")]
    [:link {:href "/feed/atom" :type "application/atom+xml" :rel "alternate" :title "Site-wide Langnostic Atom Feed"}]
    (stylesheet "/static/css/langnostic.css")
    (stylesheet "/static/css/default.css")
    [:script {:type "text/javascript" :src "/static/js/highlight.pack.js"}]
    [:script {:type "text/javascript"} "hljs.initHighlightingOnLoad();"]]
   [:body
    [:a {:href "/"} [:img {:class "logo-bar" :src "/static/img/langnostic.png"}]]
    (nav-bar section)
    [:div {:class "content"} content]
    [:hr]
    footer]))
