(defproject langnostic "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Expat License"
            :url "http://directory.fsf.org/wiki/License:Expat"}
  :dependencies [[org.clojure/clojure "1.8.0"]

                 [http-kit "2.1.18"]
                 [compojure "1.5.1"]
                 [clj-time "0.12.0"]
                 [im.chit/hara.io.scheduler "2.3.6"]

                 [cheshire "5.6.3"]
                 [markdown-clj "0.9.89"]
                 [hiccup "1.0.5"]]
  :main langnostic.core
  :aot [langnostic.core])
