(ns langnostic.core
  (:require [org.httpkit.server :as server]
            [cheshire.core :as json]
            [org.httpkit.client :as http]
            [compojure.route :as route]
            [clojure-watch.core :as watch]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.session :refer [wrap-session]]

            [clojure.java.io :as io]

            [langnostic.auth :as auth]
            [langnostic.feed :as feed]
            [langnostic.pages :as pages]
            [langnostic.posts :as posts]
            [langnostic.files :as fs])
  (:use [compojure.core :only [defroutes GET POST DELETE ANY context]])
  (:gen-class))

(defn error-404
  [user]
  {:status 404
   :headers {"Content-Type" "text/html"}
   :body (pages/template
          name name
          (fs/file-content
           "resources/public/content/404.md")
          :user user)})

(defn static-page [name]
  (fn [req]
    (let [file (io/file "resources/public/content" (str name ".md"))
          user (get-in req [:session :user])]
      (if (fs/file-in-resources? file)
        {:status 200
         :headers {"Content-Type" "text/html"}
         :body (pages/template file (clojure.string/capitalize name) (fs/file-content file) :user user)}
        (error-404 user)))))

(defn post [name]
  (fn [req]
    (let [user (get-in req [:session :user])]
      (if-let [post (posts/find-by-slug name)]
        {:status 200
         :headers {"Content-Type" "text/html"}
         :body (pages/template "blog" (post :title) (pages/post post) :user user)}
        (error-404 user)))))

(defn home [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pages/template
          "blog" "Welcome"
          [:div
           (fs/file-content "resources/public/content/intro.md")
           [:hr]
           (pages/latest-post)]
          :user (get-in req [:session :user]))})

(defn archive [posts]
  (fn [req]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (pages/template
            "archive" "Archive"
            (pages/archive posts)
            :user (get-in req [:session :user]))}))

(defn atom-feed [posts]
  (fn [req]
    {:status 200
     :headers {"Content-Type" "application/atom+xml"}
     :body (feed/atom-feed posts)}))

(defn authenticate [auth-type]
  (fn
    [req]
    (println (str req))
    (let [user (auth/authenticate! auth-type (get-in req [:params "code"]))]
      {:status 303
       :headers {"Location" "/"}
       :session {:user user}})))

(defn log-out
  [req]
  {:status 303
   :headers {"Location" "/"}
   :session nil})

(defroutes langnostic-routes
  (GET "/" [] home)
  (GET "/blog" [] home)
  (GET "/posts/:name" [name] (post name))

  (GET "/auth/log-out" [] log-out)
  (GET "/auth/:auth-type" [auth-type] (authenticate auth-type))

  (GET "/archive" [] (archive (posts/all-posts)))
  (GET "/archive/by-tag/:tag" [tag] (archive (posts/find-by-tag tag)))

  (GET "/links" [] (static-page "links"))
  (GET "/tipjar" [] (static-page "tipjar"))
  (GET "/meta" [] (static-page "meta"))

  (GET "/feed" [] (atom-feed (posts/all-posts)))
  (GET "/feed/atom" [] (atom-feed (posts/all-posts)))
  (GET "/feed/atom/:tag" [tag] (atom-feed (posts/find-by-tag tag)))
  (GET "/feed/atom/by-tag/:tag" [tag] (atom-feed (posts/find-by-tag tag)))

  (route/resources "/static/")
  (route/not-found (fn [req] (error-404 (get-in req [:session :user])))))

(defn -main
  ([] (-main "8000"))
  ([port]
   (println "Loading posts...")
   (posts/load-posts!)

   (println "Watching FS resources...")
   (watch/start-watch
    [{:path "resources/"
      :event-types [:create]
      :callback (fn [event filename]
                  (when (= "resources/posts.json" filename)
                    (println "Reloading posts.json ...")
                    (posts/load-posts!)
                    (println "  posts.json reloaded...")))}
     {:path "resources/posts/"
      :event-types [:create :modify]
      :callback (fn [event filename]
                  (let [name (.getName (io/file filename))
                        slug (.substring name 0 (- (count name) 3))
                        post (posts/find-by-slug slug)]
                    (when post
                      (println "Poking cache for" slug "...")
                      (reset! (post :content) nil))))}])

   (println "Listening on port" port "...")
   (server/run-server
    (-> langnostic-routes
        wrap-params
        wrap-session)
    {:port (read-string port)})))
