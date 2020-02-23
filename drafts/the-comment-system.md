So I've got a minimal, self-contanied comment system up and running. It's not `git`-backed, which is mildly disappointing, but I figured I'd prefer to get the thing actually done rather than shoot for the ideal.

A demonstration is in the first comment. The short version is that you have [`markdown`](https://daringfireball.net/projects/markdown/syntax), but no HTML tags because XSS.

In terms of imlpementation, I used [`codax`](https://github.com/dscarpetti/codax), a clojure-native on-disk database. You can see the mechanics over at [the appropriate repo](https://github.com/inaimathi/langnostic/blob/master/src/langnostic/comments.clj), but lets go through it real quick.

```
(ns langnostic.comments
  (:require [codax.core :as db]

            [langnostic.conf :as conf]))

(def DB (db/open-database! conf/DB-PATH))

(defn now! []
  (quot (System/currentTimeMillis) 1000))

(defn- post-path [post-id] [:posts post-id :comments])

(defn get-comments-for [post-id]
  (->> (db/get-at! DB (post-path post-id))
       vec (sort-by first) (map second) vec))

(defn comment-exists? [post-id path]
  (if-let [res (db/get-at! DB (concat (post-path post-id) path))]
    (and (map? res) (= #{:path :content :replies :user} (set (keys res))))))

(defn post-comment!
  ([user post-id content]
   (db/with-write-transaction [DB tx]
     (let [path [(count (db/get-at tx (post-path post-id)))]]
       (db/update-at
        tx (post-path post-id)
        #(assoc % (first path) {:user user :path path :content content :replies []})))))
  ([user post-id comment-path content]
   (db/with-write-transaction [DB tx]
     (let [target (conj comment-path :replies)
           new-ix (count (db/get-at tx (concat (post-path post-id) target)))
           path (conj target new-ix)]
       (db/update-at
        tx (post-path post-id)
        (fn [comments]
          (update-in comments target #(conj % {:user user :path path :content content :replies []}))))))))

(defn edit-comment!
  [user comment-path new-content]
   nil)
```

To start with,

```
(ns langnostic.comments
  (:require [codax.core :as db]

            [langnostic.conf :as conf]))

(def DB (db/open-database! conf/DB-PATH))

(defn now! []
  (quot (System/currentTimeMillis) 1000))
```

We're using `codax`, as mentioned above. We're also using a configuration file that's not checked into the repo. It's on my local, and also on the production server. Ultimately, because of the delpoyment process[^still-unformalized], I'm not sure this is the correct approach to configs. It might make more sense to put it into an external non-clojure file that gets read at runtime rather than built into any target `jar`. This is just how it works for now.

[^still-unformalized]: Which is still unformalized in the repo. This sucks and I'm aiming to change it at some point soon.

There's also a `now!` utility function that's useful in interning comments.

```
(defn- post-path [post-id] [:posts post-id :comments])
```

This is an internal utility function. Cljoure gives you [`defn-`](https://clojuredocs.org/clojure.core/defn-) which defines a functino that's only going to be called in this module. Because the comment system I'm building is going to associate comment trees with `post`s, we'll want a way to get the base path to the target `post`.

```
(defn get-comments-for [post-id]
  (->> (db/get-at! DB (post-path post-id))
       vec (sort-by first) (map second) vec))

(defn comment-exists? [post-id path]
  (if-let [res (db/get-at! DB (concat (post-path post-id) path))]
    (and (map? res) (= #{:path :content :replies :user} (set (keys res))))))
```

Getting the comments for a given post is pretty simple. You `get-at!` the `post-path` from the `DB`. Because of the way insertions happen in `codax`, I have to store the comment top-level as a `map` rather than the more convenient `vec`, I have to translate that before returning, but it's otherwise entirely straightforward.

Checking if a particular `comment` `exists?` involves taking a `post-id` and a `path` to that comment, and checknig whether the thing at that locatino is a `map` with `:path`, `:content`, `:replies` and `:user` keys.

```
(defn post-comment!
  ([user post-id content]
   (db/with-write-transaction [DB tx]
     (let [path [(count (db/get-at tx (post-path post-id)))]]
       (db/update-at
        tx (post-path post-id)
        #(assoc % (first path) {:user user :path path :content content :replies []})))))
  ([user post-id comment-path content]
   (db/with-write-transaction [DB tx]
     (let [target (conj comment-path :replies)
           new-ix (count (db/get-at tx (concat (post-path post-id) target)))
           path (conj target new-ix)]
       (db/update-at
        tx (post-path post-id)
        (fn [comments]
          (update-in comments target #(conj % {:user user :path path :content content :replies []}))))))))
```

This is the meat. Given a `user`, a `post-id` and a string `content`, we intern the given comment with the given user in the path computed for that comment. This can either be a top-level post comment, or a recursive reply to an existing comment. The only real difference between the two is how we compute the path, and how we do the actual comment attachment. At the toplevel, we're `assoc`ing it into a `map` associated with the `post`, at any response level we're `update-in`ing the comment tree with a `conj` to the appropriate comment vector. I kind of wish `codax` let me deal with vector trees instead of expecting top-level `map`s, but it still works fairly well.

I _think_ I'm marginally happier with this than a relational approach because the natural tree-like structure of this storage system lets me trivially have arbitrarily nesting comment replies, but as always, I reserve the right to change my mind.

The only relevant part of this comment system left is the dislpay. So, lets check it.

```
(defn post-comments [post]
  (let [comment-tree (comments/get-comments-for (:id post))]
    (when (or auth/USER (not (empty? comment-tree)))
      [:div {:class "post-comments"}
       [:hr]
       [:h3 "Comments"]
       (map
        (fn rec [comment]
          [:div {:class "comment" :path (str (:path comment))}
           [:span {:class "comment-author"}
            [:img {:class "author-image" :src (get-in comment [:user :image])}]
            [:a {:class "author-link" :href (get-in comment [:user :url])} (get-in comment [:user :name])]]
           [:span {:class "comment-content"}
            (-> (:content comment)
                (clojure.string/replace "&" "&amp;")
                (clojure.string/replace "<" "&lt;")
                (clojure.string/replace "\"" "&quot;")
                md/md-to-html-string)]
           (when auth/USER
             [:form {:class "reply-form"
                     :action (str "/posts/" (:id post) "/comment/reply?path="
                                  (cod/url-encode (:path comment)))
                     :method "POST"}
              [:textarea {:name "comment"}]
              [:input {:type "Submit" :value "Reply"}]])
           (when (not (empty? (:replies comment)))
             [:div {:class "replies"}
              (map rec (:replies comment))])])
        comment-tree)
       (when auth/USER
         [:form {:class "post-comment-form" :action (str "/posts/" (:id post) "/comment") :method "POST"}
          [:textarea {:name "comment"}]
          [:input {:type "Submit" :value "Post"}]])])))
```

This is where everything happens. To start with, `auth/USER` is a dynamic variable that defaults to `nil`. The way that any auth-aware functions in this blog system are meant to be called is

```
(binding [auth/USER (get-in req [:session :user])]
  (fn arg1 arg2))
```

The alternative would be threading an extra `:user` arg down a few levels of function in order to pass it to the one nested thing it eventually needs to go to. I tried that approach and it ended up being a bit gross and leaking implementation details more than I like. This way lets me keep intermediate functions ignorant about whether some low-level function is going to make auth checks, and that sounds like a way to encourage better separation of concerns.

Ok, that out of the way, so if there are no comments for a given post, and the user viewing this is logged out, there's no point in showing anything. There's just no comment section there.

If there's a comment tree, we want to show it. Before running `md-to-html-string` on any individual comment, we also escape any `<`s, `"`s and `&`s to prevent injection attacks.

Finally, if the viewer is logged in, we want to dislpay reply forms and the comment form at the bottom of the tree so that they can add additional comments.

That's the state of the comment system for now. I'm not _too_ worried about spam yet, because only people logged into `github` or `patreon` can comment, but I will want to add some sort of admin tool before too long so I can keep the inevitable in check. Really, this is a proof-of-concept for various other auth-needing projects I'm thinking up. I'll keep you posted on how those go.
