(ns langnostic.core
  (:require [org.httpkit.server :as server]
            [compojure.route :as route]

            [clojure.java.io :as io]

            [langnostic.page :as pg]
            [langnostic.files :as fs])
  (:use [compojure.core :only [defroutes GET POST DELETE ANY context]])
  (:gen-class))

(def error-404
  {:status 404
   :headers {"Content-Type" "text/html"}
   :body (pg/template
          name name
          (fs/file-content
           "resources/public/content/404.md"))})

(defn resource-page [file]
  (if (fs/file-in-resources? file)
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (pg/template "test" "test" (fs/file-content file))}
    error-404))

(defn static-page [name]
  (fn [req] (resource-page (io/file "resources/public/content" (str name ".md")))))

(defn post [name]
  (fn [req]
    (if-let [post (pg/find-by-slug name)]
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (pg/template "test" "test" (pg/post post))}
      error-404)))

(defn home [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pg/template
          "blog" "Welcome"
          [:div
           (fs/file-content "resources/public/content/intro.md")
           [:hr]
           (pg/latest-post)])})

(defn archive [posts]
  (fn [req]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (pg/template
            "archive" "Archive"
            (pg/archive posts))}))

(defroutes langnostic-routes
  (GET "/" [] home)
  (GET "/blog" [] home)
  (GET "/posts/:name" [name] (post name))

  (GET "/archive" [] (archive pg/posts))
  (GET "/archive/by-tag/:tag" [tag]
       (archive (filter #(some #{tag} (% :tags)) pg/posts)))

  (GET "/links" [] (static-page "links"))
  (GET "/tipjar" [] (static-page "tipjar"))
  (GET "/meta" [] (static-page "meta"))

  ;; (GET "/feed")
  ;; (GET "/feed/atom")
  ;; (GET "/feed/atom/:tag")
  ;; (GET "/feed/atom/by-tag/:tag")

  (route/resources "/static/")
  (route/not-found error-404))

(defn -main
  ([] (-main "8000"))
  ([port]
   (println "Listening on port" port "...")
   (server/run-server langnostic-routes {:port (read-string port)})))
