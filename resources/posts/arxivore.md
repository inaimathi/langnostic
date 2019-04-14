Just a quick update regarding our [Arxiv](https://arxiv.org/)-indexing project. Which seems to be proceeding apace.

First off, I didn't need to contact them regarding getting dumps of their data. It turns out they have a [Bulk Data Access FAQ](https://arxiv.org/help/bulk_data). If we were planning on only indexing and correlating metadata, that's available through the standard APIs. If we want to download PDFs, or LaTeX files of papers in order to have a better indexing strategy, it [turns out](https://arxiv.org/help/bulk_data_s3) that there's something called a "[Requester Pays Bucket](https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html)" in S3, and Arxiv has [such a bucket](https://arxiv.org/help/bulk_data_s3).

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

```clojure
(defn paper-urls-in [url]
  (->> (xml/parse url)
       :content first :content
       (filter #(= (:tag %) :items))
       first :content first :content
       (map #(:rdf:resource (:attrs %)))))
```

This is the simplest piece of the URL-retrieving puzzle. It takes an arxiv RSS `url`, and returns the list of paper `url`s. We'll need those eventually, but first...

```clojure
(defn -all-date-ranges []
  (let [current-year (Integer. (.format (java.text.SimpleDateFormat. "yyyy") (new java.util.Date)))
        dates
        (mapcat
         (fn [year] (map (fn [month] [year month]) [1 12]))
         (range 1991 (inc current-year)))]
    (map (fn [a b] [a b])
         dates (rest dates))))
```

That gives us all date ranges relevant to arxiv; they don't have any papers recorded as being published before 1991[^which-really-means].

[^which-really-means]: Which really means that the Cabal will need to go digging elsewhere for older CS papers to feed into our classification monstrosity once we get to that point.


```clojure
(defn -format-query [[[y1 m1] [y2 m2]] & {:keys [start]}]
  (let [fmt (format "https://arxiv.org/search/advanced?advanced=1&terms-0-operator=AND&terms-0-term=&terms-0-field=title&classification-computer_science=y&classification-physics_archives=all&classification-include_cross_list=include&date-year=&date-filter_by=date_range&date-from_date=%d-%02d&date-to_date=%d-%02d&date-date_type=submitted_date&abstracts=show&size=200&order=-announced_date_first"
                    y1 m1 y2 m2)]
    (if start
      (str fmt "&start=" start)
      fmt)))
```

_That_ provides an interface to the arxiv search system. If you give it a date range from `-all-date-ranges`, and optionally a `start` parameter, it'll return a URL that queries arxiv for CS papers in that date range. The `start` parameter is what we'll need in order to support pagination.

```clojure
(defn -urls-from-single-page [resource]
  (map #(-> % :content first :attrs :href)
       (html/select
        resource
        [:li.arxiv-result :p.list-title])))

(defn -urls-from-date-range [date-range]
  (let [resource (get-resource! (-format-query date-range))
        title (-> (html/select resource [:h1]) first :content first)]
    (if-let [match (rest (re-find #"Showing (\d+)[–-](\d+) of ([\d,]+)" title))]
      (let [[from to of] (map #(edn/read-string (str/replace % #"," "")) match)]
        (if (and to of (> of to))
          (mapcat
           #(-urls-from-single-page
             (get-resource!
              (-format-query
               date-range :start %)))
           (range to of to))
          (-urls-from-single-page resource)))
      (-urls-from-single-page resource))))

(defn historic-paper-urls []
  (mapcat -urls-from-date-range (-all-date-ranges)))
```

Getting a series of URLs from a single search page is pretty easy; we get all the `li` elements with the CSS class `arxiv-result`, and get the first link to the paper out of the first `href` we find. Getting a series of URLs from a date range is a bit more complicated. If we get a single-page response, we just apply `-urls-from-single-page`. If we get an empty response, there won't be any `arxiv-result` elements and we're just fine. If we get a _multi-page_ result, shit gets a bit more complicated. Specifically, we need to go through each page of the result and apply `-urls-from-single-page` to each one.

Ok, that's how we go about expropriating URLs from the arxiv system. We also need to manipulate them a bit.

```clojure
(defn pdf-path [paper-url]
  (let [id (last (str/split paper-url #"/"))]
    (str +paper-directory+ id ".pdf")))

(defn pdf-url [paper-url]
  (str/replace paper-url #"/abs/" "/pdf/"))

(defn got-pdf? [paper-url]
  (.exists (io/as-file (pdf-path paper-url))))
```

The PDF file we get out of a given paper URL will be downloaded to the location specified by `pdf-path`. By default, we get `abs` URLs out of each arxiv interface. Those point to XML documents rather than PDFs, but transforming them into PDF links isn't difficult (although not every paper has a PDF on file). `got-pdf?` just checks whether a local file already exists at the location specified by `pdf-path`.

```clojure
(defn grab-pdf! [paper-url]
  (let [path (pdf-path paper-url)]
    (io/make-parents path)
    (with-open [out (io/output-stream (io/as-file path))]
      (io/copy (get! (pdf-url paper-url)) out))))

(defn grab-urls! []
  (let [path (str +paper-directory+ "urls.txt")]
    (io/make-parents path)
    (doseq [url (mapcat #(do (println (str "Getting " % "...")) (-urls-from-date-range %)) (-all-date-ranges))]
      (spit path (str url \newline) :append true))))
```

`grab`bing a `pdf!` involves taking the paper URL, and copying the result of a `get!` call into the location specified by `pdf-path`. We also call `make-parents` just to make sure that the target path exists on disk. `grab`bing the `urls!` involves calling `historic-paper-urls`[^in-a-roundabout-way], and spitting each result into a separate line in the `urls.txt` file in our `+paper-directory+`.

[^in-a-roundabout-way]: In a roundabout way, granted, because we need to get some `println`s into the mix rather than doing the whole thing silently.

```clojure
(defn nom! []
  (let [historics (atom (drop-while got-pdf? (str/split-lines (slurp (str +paper-directory+ "urls.txt")))))]
    (while true
      (let [hs (take 20 @historics)]
        (swap! historics #(drop 20 %))
        (doseq [url (set (concat hs (paper-urls-in "http://export.arxiv.org/rss/cs")))]
          (if (not (got-pdf? url))
            (do (println "Grabbing <" url ">...")
                (grab-pdf! url))
            (do (println "Found duplicate '" url "'..."))))))))
```

Okay, finally, putting it all together...

We slurp up all the paper URLs from our local file, then we start interspersing 20 historic papers with a call to the latest CS RSS feed. If we see a URL that we don't have the corresponding file to yet, we `grab` it, otherwise we just print a warning and continue on our merry way.

Because the basic `get!` primitive `sleep`s for 15 seconds before doing anything, `nom!` doesn't need to explicitly rate-limit itself. It's not exactly thread-safe, because running multiple `nom!`ming threads will exceed the arxiv rate limit, and probably get your IP banned temporarily. So, I mean, in case you were going to ignore the "don't use this utility" warning in the [README](https://github.com/CompSciCabal/arxivore/blob/master/README.md), extra-special don't do the multi-threaded thing.

## Next Week

I'm probably taking a week or two off blogging about the Cabal's activities. We're breaking until May, both to give people a chance to RSVP to the call for PAIP readers, and to avoid having a mostly empty room at the Toda house. Also, this coming week is going to see me giving a short lightning talk at [Clojure North](https://clojurenorth.com/), and I'm probably going to spend most of my (still very scarce) free time preparing for that.

Wish me luck; I'll let you know how it goes.
