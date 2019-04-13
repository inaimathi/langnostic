Just a quick update regarding our [Arxiv](https://arxiv.org/)-indexing project. Which seems to be proceeding apace.

Ok, first off, I didn't need to contact them regarding getting dumps of their data. It turns out they have a [Bulk Data Access FAQ](https://arxiv.org/help/bulk_data). If we were planning on only indexing and correlating metadata, that's available through the standard APIs. If we want to download PDFs, or LaTeX files of papers in order to have a better indexing strategy, it [turns out](https://arxiv.org/help/bulk_data_s3) that there's something called a "[Requester Pays Bucket](https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html)" in S3, and Arxiv has [such a bucket](https://arxiv.org/help/bulk_data_s3).

However, given that the [CS Cabal](https://compscicabal.github.io/) is a loose coalition of Comp Sci, math and engineering nerds who have more time than money available for this particular project, we decided to instead respect the [Arxiv `robots` specification](https://arxiv.org/robots.txt) and just crawl their repos _very, very slowly_. We're only out to index all comp-sci papers ever, which seems like it should only be about 200k papers total. At 15 second delays, that's the sort of thing we can do in a month or so of dedicated scraping effort. This is totally not an awful idea at all, so [we're going with it](https://github.com/CompSciCabal/arxivore).

In order to do a credible job of this, we need to both scrape the [search-results interface](https://arxiv.org/search/advanced) for historic papers, and the [CS rss feed](http://export.arxiv.org/rss/cs) for new papers on an ongoing basis. [`enlive`](https://github.com/cgrand/enlive) helps with this, obviously, because there's no option to expose the Arxiv-direct stuff in data formats other than `html`/`xml`. Ok, so now that we know what we need to do, [here's](https://github.com/CompSciCabal/arxivore/blob/8ab3db135fabd21f60e8412bf044cb9bc492aab8/src/arxivore/core.clj) the deal.[^oh-man-this-takes-me-back]

[^oh-man-this-takes-me-back]: Oh man, this takes me back. It feels like its' been absolutely fucking FOREVER since I've done an [almost-literate-programming](http://inaimathi.ca/archive/by-tag/almost-literate-programming) piece.

## The Deal

```clojure
(ns arxivore.core
  (:require [clojure.xml :as xml]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]

            [environ.core :as environ]
            [org.httpkit.client :as http]
            [net.cgrand.enlive-html :as html]))
```

Module imports; nothing to see here. I'm using `environ`, `httpkit`, `enlive` and some native utilities.

```clojure
(defn env
  [key & {:keys [default]}]
  (if-let [val (get environ/env key)]
    val
    (or default
        (throw
         (Exception.
          (str "Could not find environment variable "
               (str/replace
                (str/upper-case (name key))
                #"-" "_")))))))

(def +paper-directory+
  (env :arxivore-papers
       :default (str (System/getProperty "user.home") "/arxivore-papers/")))
```

Because I want actual people to actually be able to use this thing on actual machines, we need to be able to point it at a directory. Not all the places it might run will have a `home` directory, so we want to be able to take an alternative via envronment variable. `env` lets us do that. The `+paper-directory+` constant is set to either the value of the `AXIVORE_PAPERS` environment variable, _or_ `~/arxivore-papers` if that variable is not present.

```clojure
(defn get! [url]
  (Thread/sleep 15000)
  (:body @(http/get url)))

(defn get-resource! [url]
  (html/html-resource (java.io.StringReader. (get! url))))
```

We want our `GET` requests to be slow. So this implementation of `get!` waits for 15 seconds before doing anything. `get-resource!` is a utility function to get an `enlive` resource instead of a raw `body` string. This'll be useful for HTML pages we want to slice up.
