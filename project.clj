(defproject langnostic "0.1.5-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Expat License"
            :url "http://directory.fsf.org/wiki/License:Expat"}
  :dependencies [[org.clojure/clojure "1.8.0"]

                 [http-kit "2.6.0"]
                 [org.apache.httpcomponents/httpclient "4.3.5"]
                 [oauth-clj "0.1.16"]
                 [compojure "1.5.1"]
                 [clj-time "0.12.0"]
                 [im.chit/hara.io.scheduler "2.3.6"]
                 [clojure-watch "0.1.11"]
                 [javax.servlet/servlet-api "2.5"]

                 [codax "1.3.1"]

                 [com.patreon/patreon "0.4.2"]

                 [cheshire "5.11.0"]
                 [markdown-clj "0.9.89"]
                 [hiccup "1.0.5"]]
  :main langnostic.core
  :aot [langnostic.core])
