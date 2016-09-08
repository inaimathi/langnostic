This isn't _exactly_ my usual [almost-literate-programming](/archive/by-tag/almost-literate-programming) thing. It's mostly a stream-of-consciousness as I drill down through a corpus of Kickstarter projects, and try to make sense of how I'd go about applying machine learning techniques to it. You've been warned; as disjoint and rambling as my usual output is, this post is likely to be moreso.

## The Data

So, first things first. I can't publish the corpus I'm working from without permission, but I can show you how to approximate it.

```
(defn paginated [url]
  (letfn [(get! [pg] (org.httpkit.client/get (str url "&page=" pg)))
          (req! [pg]
            (loop [delay 100
                   res (get! pg)]
              (case (@res :status)
                200 @res
                429 (do (Thread/sleep delay)
                        (recur (* delay 10) (req! pg)))
                404 nil)))]
    (map req! (range 0 201))))

(defn collect! [category-from]
  (let [dat-file "data-lines.clj"
        all (atom (set (map #((read-string %) "id") (line-seq (clojure.java.io/reader dat-file)))))]
    (doseq [category (range category-from 401)]
      (print "Pulling category " category " ..." \newline)
      (doseq [pg (paginated (str "http://www.kickstarter.com/discover/advanced?format=json&category_id=" category))]
        (print "  Next page ..." \newline)
        (when pg
          (let [res (cheshire.core/parse-string (pg :body))]
            (doseq [project (res "projects")]
              (when (not (contains? @all (project "id")))
                (print "    Found new project " (project "id") " ..." \newline)
                (swap! all conj (project "id"))
                (spit dat-file project :append :true)
                (spit dat-file \newline :append :true)))))))))
```

Having evaluated that to a Clojure REPL in which you've loaded [`http-kit`](http://www.http-kit.org/) and [`cheshire`](https://github.com/dakrone/cheshire#cheshire), you can evaluate `(collect! 0)` and wait a very long while. After waiting long enough[^and-probably-restarting], you'll have a file containing around 165 thousand [edn](https://github.com/edn-format/edn#edn) encoded kickstarter projects. We don't really need all of the stuff present to learn from it; so once we got the raw data pulled down, we can sift it for interesting bits.

[^and-probably-restarting]: And probably restarting the process at some intermediate point to avoid a partial failure forcing you to pull down a GB-worth of duplicate data. I learned that the hard way, which is why the above takes a category as an argument and starts at the appropriate point, rather than always going from zero.

```
(defn sift-project [project-hash]
  {:id (project-hash "id") :label (keyword (project-hash "state"))
   :created (project-hash "created_at") :starts (project-hash "launched_at") :ends (project-hash "deadline")
   :goal (project-hash "goal") :exchange-rate (project-hash "static_usd_rate") :currency (project-hash "currency")
   :spotlight? (project-hash "spotlight") :disable-communication? (project-hash "disable_communication")
   :country (project-hash "country")
   :name (project-hash "name") :slug (project-hash "slug") :blurb (project-hash "blurb")
   :url (first (clojure.string/split (get-in project-hash ["urls" "web" "project"]) #"\?"))
   :creator {:id (get-in project-hash ["creator" "id"]) :name (get-in project-hash ["creator" "name"])}
   :category {:id (get-in project-hash ["category" "id"])
              :name (get-in project-hash ["category" "name"])
              :slug (get-in project-hash ["category" "slug"])}})

(doseq [ln (line-seq (clojure.java.io/reader "data.clj"))]
  (let [raw (read-string ln)
        sifted (sift-project raw)]
    (when (not (= :live (sifted :label)))
      (spit "data-sifted.clj" sifted :append true)
      (spit "data-sifted.clj" \newline :append :true))))
```

I'm keeping as much as possible while still cutting out most of the structure of their raw JSON, and also, we can't learn _very_ much from live projects, so those are getting dropped along the way.

Now then...

## Basic Questions

```clojure
> (set (map #((read-string %) :label) (line-seq (clojure.java.io/reader "data-sifted.clj"))))
#{:suspended :failed :successful :canceled}
> (def cats (set (map #((read-string %) :category) (line-seq (clojure.java.io/reader "data-sifted.clj")))))
#'kicktracker.core/cats
> (count cats)
165
> (sort (map :name cats))
("3D Printing" "Academic" "Accessories" "Action" "Animals" "Animation" "Anthologies" "Anthologies" "Apparel" "Apps" "Architecture" "Art" "Art Books" "Audio" "Bacon" "Blues" "Calendars" "Camera Equipment" "Candles" "Ceramics" "Children's Books" "Childrenswear" "Chiptune" "Civic Design" "Classical Music" "Comedy" "Comic Books" "Comics" "Community Gardens" "Conceptual Art" "Cookbooks" "Country & Folk" "Couture" "Crafts" "Crochet" "DIY" "DIY Electronics" "Dance" "Design" "Digital Art" "Documentary" "Drama" "Drinks" "Electronic Music" "Embroidery" "Events" "Events" "Experimental" "Experimental" "Fabrication Tools" "Faith" "Family" "Fantasy" "Farmer's Markets" "Farms" "Fashion" "Festivals" "Festivals" "Fiction" "Film & Video" "Fine Art" "Flight" "Food" "Food Trucks" "Footwear" "Gadgets" "Games" "Gaming Hardware" "Glass" "Graphic Design" "Graphic Novels" "Hardware" "Hip-Hop" "Horror" "Illustration" "Immersive" "Indie Rock" "Installations" "Interactive Design" "Jazz" "Jewelry" "Journalism" "Kids" "Knitting" "Latin" "Letterpress" "Literary Journals" "Live Games" "Makerspaces" "Metal" "Mixed Media" "Mobile Games" "Movie Theaters" "Music" "Music Videos" "Musical" "Narrative Film" "Nature" "Nonfiction" "Painting" "People" "Performance Art" "Performances" "Periodicals" "Pet Fashion" "Photo" "Photobooks" "Photography" "Places" "Playing Cards" "Plays" "Poetry" "Pop" "Pottery" "Print" "Printing" "Product Design" "Public Art" "Publishing" "Punk" "Puzzles" "Quilts" "R&B" "Radio & Podcasts" "Ready-to-wear" "Residencies" "Restaurants" "Robots" "Rock" "Romance" "Science Fiction" "Sculpture" "Shorts" "Small Batch" "Software" "Sound" "Space Exploration" "Spaces" "Spaces" "Spaces" "Stationery" "Tabletop Games" "Taxidermy" "Technology" "Television" "Textiles" "Theater" "Thrillers" "Translations" "Typography" "Vegan" "Video" "Video Art" "Video Games" "Wearables" "Weaving" "Web" "Web" "Webcomics" "Webseries" "Woodworking" "Workshops" "World Music" "Young Adult" "Zines")
```

So because I'm primarily interested in improving [Kicktracker](https://github.com/Inaimathi/kicktracker), I'll be paying special attention to `"Tabletop Games"`, but a couple of things jump out to me here. Firstly, these categories are not disjoint, because `"Art"`, `"Fine Art"` and `"Conceptual Art"` are distinct, as are `"Games"`, `"Mobile Games"` and `"Tabletop Games"`. Secondly, there's a category called `"Software"`, which I'll be taking a closer look at later. For science. Finally, for some reason there are enough projects related to it that `"Bacon"` is a category. _Category_. Not keyword or tag. At the risk of going off on a complete tangent here...

### BEGIN TANGENT: Bacon

```
> (def bacon (filter #(= (-> % :category :name) "Bacon") (map read-string (line-seq (clojure.java.io/reader "data-sifted.clj")))))
#'kicktracker.core/bacon
> (count bacon)
19
```

Or, I guess the threshold for a new category has nothing to do with how many projects are present in it.

```
> (map :name bacon)
("Y'alls Balls | You make the jokes, we make the food." "Bakin' Bakery Food Truck" "≈≈≈ Bacon Dinner ≈≈≈" "Lick My Dip" "PROJECT: BACON - an awesome & unusual bacon cookbook" "ARTISAN MEAT KIT - DRY AGE STEAK AND CHARCUTERIE AT HOME" "Help me make a Pizza Cake!" "The Bacon Lover's Truck (The B.L.T.)" "Goats and Sheep" "BACON JERKY by PORK BARREL BBQ!!" "Doggie Cake" "Candied Bacon Chocolate Chip Cookies" "Bring awesome meat, fish and produce to Carnivore Oak Park!" "Bacon Wrapping Paper" "Bacon Bourbon by Ol Major" "Help Jack butcher a pig for Katy" "Bacon cupcakes." "Making Bacon At Home" "Beer & Bacon Spaghetti Sauce")
```

That's about what I was expecting. I guess. If I were asked to speculate, I'd say that the Bacon category was someone's hack-day/chaos project over at Kickstarter, and whoever held the "Launch" button at the time didn't have the heart to say "no".

### END TANGENT

But it does beg a question.

```clojure
> (sort-by second (vec (frequencies (map #(-> (read-string %) :category :name) (line-seq (clojure.java.io/reader "data-sifted.clj"))))))
(["Tabletop Games" 5891]
 ["Documentary" 5076]
 ["Video Games" 4796]
 ["Shorts" 4739]
 ["Apparel" 4605]
 ["Children's Books" 4535]
 ["Fiction" 4500]
 ["Nonfiction" 4473]
 ["Indie Rock" 4451]
 ["Narrative Film" 4376]
 ["Rock" 4245]
 ["Country & Folk" 4221]
 ["Webseries" 4160]
 ["Hardware" 3631]
 ["Product Design" 3602]
 ["Hip-Hop" 3322]
 ["Pop" 3087]
 ["Theater" 2750]
 ["Public Art" 2705]
 ["Painting" 2681]
 ["Classical Music" 2564]
 ["Software" 2527]
 ["Photography" 2524]
 ["Art Books" 2503]
 ["Animation" 2345]
 ["Comic Books" 2279]
 ["Mixed Media" 2267]
 ["Crafts" 2196]
 ["Dance" 2175]
 ["Food" 2159]
 ["Comics" 2127]
 ["Accessories" 2085]
 ["Illustration" 2049]
 ["Performance Art" 1943]
 ["Fashion" 1876]
 ["Electronic Music" 1875]
 ["World Music" 1868]
 ["Jazz" 1736]
 ["Playing Cards" 1641]
 ["Graphic Novels" 1626]
 ["Journalism" 1551]
 ["Sculpture" 1534]
 ["Graphic Design" 1527]
 ["Photobooks" 1500]
 ["Performances" 1424]
 ["Periodicals" 1293]
 ["Poetry" 1274]
 ["Music" 1225]
 ["Mobile Games" 1222]
 ["People" 1123]
 ["Art" 1077]
 ["Digital Art" 1029]
 ["Web" 998]
 ["Jewelry" 903]
 ["Conceptual Art" 868]
 ["Radio & Podcasts" 859]
 ["Fine Art" 780]
 ["Places" 780]
 ["Live Games" 757]
 ["Footwear" 689]
 ["Technology" 682]
 ["Ready-to-wear" 672]
 ["Architecture" 624]
 ["Webcomics" 620]
 ["Metal" 605]
 ["Festivals" 585]
 ["Nature" 567]
 ["Print" 530]
 ["Woodworking" 486]
 ["Publishing" 469]
 ["Anthologies" 450]
 ["Plays" 429]
 ["Experimental" 419]
 ["Design" 416]
 ["Film & Video" 410]
 ["DIY" 405]
 ["Childrenswear" 388]
 ["Spaces" 378]
 ["Immersive" 350]
 ["Gadgets" 345]
 ["Interactive Design" 332]
 ["Gaming Hardware" 326]
 ["Drinks" 323]
 ["Video" 298]
 ["Animals" 294]
 ["Audio" 277]
 ["Small Batch" 276]
 ["Musical" 254]
 ["Restaurants" 251]
 ["Civic Design" 248]
 ["Couture" 247]
 ["Workshops" 246]
 ["DIY Electronics" 244]
 ["Events" 212]
 ["Puzzles" 199]
 ["Wearables" 179]
 ["Candles" 170]
 ["Farms" 157]
 ["Games" 153]
 ["Photo" 150]
 ["3D Printing" 143]
 ["Apps" 140]
 ["Food Trucks" 137]
 ["Pet Fashion" 133]
 ["Sound" 126]
 ["Robots" 118]
 ["Residencies" 117]
 ["Typography" 116]
 ["Vegan" 104]
 ["Knitting" 100]
 ["Camera Equipment" 98]
 ["Installations" 95]
 ["Stationery" 86]
 ["Cookbooks" 78]
 ["Glass" 74]
 ["Printing" 72]
 ["Space Exploration" 66]
 ["Faith" 66]
 ["Crochet" 63]
 ["Literary Journals" 58]
 ["Young Adult" 58]
 ["Zines" 55]
 ["Flight" 54]
 ["Academic" 51]
 ["Makerspaces" 48]
 ["Pottery" 46]
 ["Comedy" 45]
 ["Community Gardens" 42]
 ["Farmer's Markets" 41]
 ["Quilts" 40]
 ["Ceramics" 39]
 ["Calendars" 38]
 ["Weaving" 36]
 ["Embroidery" 35]
 ["Drama" 35]
 ["Fabrication Tools" 34]
 ["Horror" 33]
 ["Letterpress" 30]
 ["Science Fiction" 25]
 ["Textiles" 21]
 ["Video Art" 20]
 ["Kids" 20]
 ["Bacon" 19]
 ["Punk" 18]
 ["R&B" 17]
 ["Fantasy" 17]
 ["Translations" 16]
 ["Blues" 14]
 ["Thrillers" 10]
 ["Chiptune" 10]
 ["Action" 8]
 ["Music Videos" 8]
 ["Television" 8]
 ["Taxidermy" 7]
 ["Movie Theaters" 6]
 ["Latin" 6]
 ["Romance" 5]
 ["Family" 4])
```

Fortuitously, my target category is the best represented here. And, actually, on a side-note, `"Bacon"` isn't even the most sparsely populated category. So I guess my earlier hypothesis was incorrect; Kickstarter just has a low threshold for category existence.

While I'm here, I may as well ask


```clojure
> (let [counts (atom {})]
  (doseq [ln (line-seq (clojure.java.io/reader "data-sifted.clj"))]
    (let [p (read-string ln)]
      (swap! counts update-in [(-> p :category :name) (p :label)] (fnil inc 0))
      (swap! counts update-in [(-> p :category :name) :total] (fnil inc 0))))
  (sort-by
   #((second %) :percent-success)
   (map (fn [[label ct]]
          [label { :percent-success (int (* 100 (/ (ct :successful) (ct :total)))) :total (ct :total) }])
          @counts)))
```

I'm asking in this roundabout imperative way rather than just mapping over the collection of lines in `"data-sifted.clj"`, because it runs _much_ faster this way, despite ultimately giving the same answer. I'm assuming there's some issues with Clojure map fusion when it involves effectful operations.

```clojure
(["Mobile Games" {:percent-success 10, :total 1222}]
 ["Video" {:percent-success 10, :total 298}]
 ["Photo" {:percent-success 17, :total 150}]
 ["Hip-Hop" {:percent-success 18, :total 3322}]
 ["Software" {:percent-success 20, :total 2527}]
 ["Live Games" {:percent-success 20, :total 757}]
 ["Print" {:percent-success 21, :total 530}]
 ["Candles" {:percent-success 23, :total 170}]
 ["Places" {:percent-success 24, :total 780}]
 ["Web" {:percent-success 24, :total 998}]
 ["Embroidery" {:percent-success 25, :total 35}]
 ["Nature" {:percent-success 26, :total 567}]
 ["Audio" {:percent-success 26, :total 277}]
 ["Ready-to-wear" {:percent-success 27, :total 672}]
 ["Journalism" {:percent-success 27, :total 1551}]
 ["Gaming Hardware" {:percent-success 27, :total 326}]
 ["Digital Art" {:percent-success 27, :total 1029}]
 ["DIY" {:percent-success 28, :total 405}]
 ["Architecture" {:percent-success 29, :total 624}]
 ["People" {:percent-success 30, :total 1123}]
 ["Interactive Design" {:percent-success 30, :total 332}]
 ["Childrenswear" {:percent-success 32, :total 388}]
 ["Couture" {:percent-success 32, :total 247}]
 ["Animation" {:percent-success 33, :total 2345}]
 ["Crochet" {:percent-success 33, :total 63}]
 ["Painting" {:percent-success 34, :total 2681}]
 ["Mixed Media" {:percent-success 34, :total 2267}]
 ["Quilts" {:percent-success 35, :total 40}]
 ["Jewelry" {:percent-success 35, :total 903}]
 ["Workshops" {:percent-success 35, :total 246}]
 ["Apparel" {:percent-success 35, :total 4605}]
 ["Graphic Design" {:percent-success 36, :total 1527}]
 ["Electronic Music" {:percent-success 37, :total 1875}]
 ["Conceptual Art" {:percent-success 37, :total 868}]
 ["Crafts" {:percent-success 38, :total 2196}]
 ["Woodworking" {:percent-success 39, :total 486}]
 ["Animals" {:percent-success 39, :total 294}]
 ["Sculpture" {:percent-success 39, :total 1534}]
 ["Metal" {:percent-success 41, :total 605}]
 ["Webseries" {:percent-success 41, :total 4160}]
 ["Poetry" {:percent-success 41, :total 1274}]
 ["Performance Art" {:percent-success 43, :total 1943}]
 ["Glass" {:percent-success 43, :total 74}]
 ["Footwear" {:percent-success 44, :total 689}]
 ["Accessories" {:percent-success 44, :total 2085}]
 ["Playing Cards" {:percent-success 44, :total 1641}]
 ["Printing" {:percent-success 45, :total 72}]
 ["Hardware" {:percent-success 47, :total 3631}]
 ["Illustration" {:percent-success 47, :total 2049}]
 ["Civic Design" {:percent-success 47, :total 248}]
 ["Fine Art" {:percent-success 48, :total 780}]
 ["Periodicals" {:percent-success 49, :total 1293}]
 ["Puzzles" {:percent-success 49, :total 199}]
 ["World Music" {:percent-success 49, :total 1868}]
 ["Photobooks" {:percent-success 49, :total 1500}]
 ["Pop" {:percent-success 50, :total 3087}]
 ["Narrative Film" {:percent-success 50, :total 4376}]
 ["Weaving" {:percent-success 50, :total 36}]
 ["Public Art" {:percent-success 51, :total 2705}]
 ["Children's Books" {:percent-success 51, :total 4535}]
 ["Stationery" {:percent-success 51, :total 86}]
 ["Nonfiction" {:percent-success 53, :total 4473}]
 ["Radio & Podcasts" {:percent-success 53, :total 859}]
 ["Fiction" {:percent-success 54, :total 4500}]
 ["Pet Fashion" {:percent-success 54, :total 133}]
 ["Taxidermy" {:percent-success 57, :total 7}]
 ["Performances" {:percent-success 58, :total 1424}]
 ["Video Games" {:percent-success 58, :total 4796}]
 ["Spaces" {:percent-success 59, :total 378}]
 ["Art Books" {:percent-success 59, :total 2503}]
 ["Events" {:percent-success 61, :total 212}]
 ["Pottery" {:percent-success 63, :total 46}]
 ["Jazz" {:percent-success 63, :total 1736}]
 ["Knitting" {:percent-success 64, :total 100}]
 ["Country & Folk" {:percent-success 66, :total 4221}]
 ["Classical Music" {:percent-success 67, :total 2564}]
 ["Dance" {:percent-success 67, :total 2175}]
 ["Immersive" {:percent-success 67, :total 350}]
 ["Graphic Novels" {:percent-success 68, :total 1626}]
 ["Comic Books" {:percent-success 70, :total 2279}]
 ["Experimental" {:percent-success 70, :total 419}]
 ["Webcomics" {:percent-success 70, :total 620}]
 ["Photography" {:percent-success 70, :total 2524}]
 ["Residencies" {:percent-success 72, :total 117}]
 ["Festivals" {:percent-success 72, :total 585}]
 ["Letterpress" {:percent-success 76, :total 30}]
 ["Typography" {:percent-success 80, :total 116}]
 ["Rock" {:percent-success 81, :total 4245}]
 ["Indie Rock" {:percent-success 86, :total 4451}]
 ["Anthologies" {:percent-success 87, :total 450}]
 ["Small Batch" {:percent-success 100, :total 276}]
 ["Games" {:percent-success 100, :total 153}]
 ["Translations" {:percent-success 100, :total 16}]
 ["Cookbooks" {:percent-success 100, :total 78}]
 ["Faith" {:percent-success 100, :total 66}]
 ["Punk" {:percent-success 100, :total 18}]
 ["Bacon" {:percent-success 100, :total 19}]
 ["Art" {:percent-success 100, :total 1077}]
 ["Chiptune" {:percent-success 100, :total 10}]
 ["Latin" {:percent-success 100, :total 6}]
 ["3D Printing" {:percent-success 100, :total 143}]
 ["Family" {:percent-success 100, :total 4}]
 ["Film & Video" {:percent-success 100, :total 410}]
 ["Vegan" {:percent-success 100, :total 104}]
 ["Music" {:percent-success 100, :total 1225}]
 ["Farmer's Markets" {:percent-success 100, :total 41}]
 ["Fantasy" {:percent-success 100, :total 17}]
 ["Academic" {:percent-success 100, :total 51}]
 ["Gadgets" {:percent-success 100, :total 345}]
 ["Drama" {:percent-success 100, :total 35}]
 ["Community Gardens" {:percent-success 100, :total 42}]
 ["Tabletop Games" {:percent-success 100, :total 5891}]
 ["Space Exploration" {:percent-success 100, :total 66}]
 ["Robots" {:percent-success 100, :total 118}]
 ["Horror" {:percent-success 100, :total 33}]
 ["Fabrication Tools" {:percent-success 100, :total 34}]
 ["DIY Electronics" {:percent-success 100, :total 244}]
 ["Technology" {:percent-success 100, :total 682}]
 ["Kids" {:percent-success 100, :total 20}]
 ["Documentary" {:percent-success 100, :total 5076}]
 ["Zines" {:percent-success 100, :total 55}]
 ["R&B" {:percent-success 100, :total 17}]
 ["Sound" {:percent-success 100, :total 126}]
 ["Product Design" {:percent-success 100, :total 3602}]
 ["Flight" {:percent-success 100, :total 54}]
 ["Musical" {:percent-success 100, :total 254}]
 ["Makerspaces" {:percent-success 100, :total 48}]
 ["Theater" {:percent-success 100, :total 2750}]
 ["Television" {:percent-success 100, :total 8}]
 ["Young Adult" {:percent-success 100, :total 58}]
 ["Movie Theaters" {:percent-success 100, :total 6}]
 ["Apps" {:percent-success 100, :total 140}]
 ["Publishing" {:percent-success 100, :total 469}]
 ["Music Videos" {:percent-success 100, :total 8}]
 ["Camera Equipment" {:percent-success 100, :total 98}]
 ["Calendars" {:percent-success 100, :total 38}]
 ["Restaurants" {:percent-success 100, :total 251}]
 ["Food Trucks" {:percent-success 100, :total 137}]
 ["Action" {:percent-success 100, :total 8}]
 ["Shorts" {:percent-success 100, :total 4739}]
 ["Science Fiction" {:percent-success 100, :total 25}]
 ["Comics" {:percent-success 100, :total 2127}]
 ["Fashion" {:percent-success 100, :total 1876}]
 ["Literary Journals" {:percent-success 100, :total 58}]
 ["Installations" {:percent-success 100, :total 95}]
 ["Comedy" {:percent-success 100, :total 45}]
 ["Blues" {:percent-success 100, :total 14}]
 ["Wearables" {:percent-success 100, :total 179}]
 ["Drinks" {:percent-success 100, :total 323}]
 ["Thrillers" {:percent-success 100, :total 10}]
 ["Ceramics" {:percent-success 100, :total 39}]
 ["Farms" {:percent-success 100, :total 157}]
 ["Food" {:percent-success 100, :total 2159}]
 ["Video Art" {:percent-success 100, :total 20}]
 ["Design" {:percent-success 100, :total 416}]
 ["Textiles" {:percent-success 100, :total 21}]
 ["Romance" {:percent-success 100, :total 5}]
 ["Plays" {:percent-success 100, :total 429}])
```

So I learned two interesting things here.

Firstly, _whatever_ you do, **don't** start a `Mobile Games`, `Hip-Hop`, `Software` or `Digital Art` project. Statistically, it will not go well for you. Many of the categories with a low success rate have few enough examples that I'm not comfortable generalizing, but those four specific ones kinda do. This incidentally also bodes poorly for my earlier interest in the `Software` category.

Secondly, a surprising number of categories have 100% funding rates. Some of those are probably rounding errors, and quite a few have too little information for me to try to call it one way or the other, but `Music`, `Tabletop Games`, `Documentary`, `Product Design`, `Theater`, `Comics`, `Fashion` and `Food` all look like remarkably safe bets. In terms of getting money together, I mean. I can't imagine they have a 100% delivery rate. I mean, to be honest, I'm fairly certain they don't have a 100% _funding_ rates either. This is another mark against the accuracy of the data I'm looking at. To the point, that I'm convinced I'd damn well better start collecting real data _right now_ if I want to be able to say anything definitive about it any time soon. By "real data", I mean, "start recording `:live` projects, then check back later to see whats happened to them, applying a lot of manual spot checks to make sure insane things aren't happening".

I was going to ask a few deeper questions about project distribution, but I think I'll leave that for part two. First things first, I need to think up a system by which to scrape more accurate data at larger scales, with higher confidence.

_Then_ I'll ask what it might mean.
