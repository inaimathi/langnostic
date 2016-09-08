(ns langnostic.core
  (:require [org.httpkit.server :as server]
            [compojure.route :as route]
            [clojure-watch.core :as watch]

            [clojure.java.io :as io]

            [langnostic.feed :as feed]
            [langnostic.pages :as pages]
            [langnostic.posts :as posts]
            [langnostic.files :as fs])
  (:use [compojure.core :only [defroutes GET POST DELETE ANY context]])
  (:gen-class))

(def error-404
  {:status 404
   :headers {"Content-Type" "text/html"}
   :body (pages/template
          name name
          (fs/file-content
           "resources/public/content/404.md"))})

(defn resource-page [title file]
  (if (fs/file-in-resources? file)
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (pages/template file (clojure.string/capitalize title) (fs/file-content file))}
    error-404))

(defn static-page [name]
  (fn [req] (resource-page name (io/file "resources/public/content" (str name ".md")))))

(defn post [name]
  (fn [req]
    (if-let [post (posts/find-by-slug name)]
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (pages/template "blog" (post :title) (pages/post post))}
      error-404)))

(defn home [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pages/template
          "blog" "Welcome"
          [:div
           (fs/file-content "resources/public/content/intro.md")
           [:hr]
           (pages/latest-post)])})

(defn archive [posts]
  (fn [req]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (pages/template
            "archive" "Archive"
            (pages/archive posts))}))

(defn atom-feed [posts]
  (fn [req]
    {:status 200
     :headers {"Content-Type" "application/atom+xml"}
     :body (feed/atom-feed posts)}))

(defroutes langnostic-routes
  (GET "/" [] home)
  (GET "/blog" [] home)
  (GET "/posts/:name" [name] (post name))

  (GET "/archive" [] (archive (posts/all-posts)))
  (GET "/archive/by-tag/:tag" [tag] (archive (posts/find-by-tag tag)))

  (GET "/links" [] (static-page "links"))
  (GET "/tipjar" [] (static-page "tipjar"))
  (GET "/meta" [] (static-page "meta"))

  (GET "/feed" [] (atom-feed (posts/all-posts)))
  (GET "/feed/atom" [] (atom-feed (posts/all-posts)))
  (GET "/feed/atom/:tag" [tag] (atom-feed (posts/find-by-tag (posts/all-posts))))
  (GET "/feed/atom/by-tag/:tag" [tag] (atom-feed (posts/find-by-tag (posts/all-posts))))

  (route/resources "/static/")
  (route/not-found error-404))

(defn -main
  ([] (-main "8000"))
  ([port]
   (println "Loading posts...")
   (posts/load-posts!)

   (println "Watching FS resources...")
   (watch/start-watch
    [{:path "resources/"
      :event-types [:modify]
      :callback (fn [event filename]
                  (when (and (= :modify event)
                             (= "resources/posts.json" filename))
                    (println "Reloading posts.json ...")
                    (posts/load-posts!)
                    (println "  posts.json reloaded...")))}
     {:path "resources/posts/"
      :event-types [:modify]
      :callback (fn [event filename]
                  (let [name (.getName (io/file filename))
                        slug (.substring name 0 (- (count name) 3))
                        post (posts/find-by-slug slug)]
                    (when post
                      (println "Poking cache for" slug "...")
                      (reset! (post :content) nil))))}])

   (println "Listening on port" port "...")
   (server/run-server langnostic-routes {:port (read-string port)})))
