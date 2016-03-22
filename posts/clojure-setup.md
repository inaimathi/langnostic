So it looks like [Clojure](http://clojure.org/) is finally about to get mileage at work. Which is excellent, as far as I'm concerned, because I really like the language and have been looking for an excuse or two to get more familiar with it. Unfortunately, I've had precious little programming time in my off-hours lately. Various reasons: books and papers I need to read, books and chapters I need to write, and some unrelated personal stuff. But if it's for work, rather than my own playing around, I can justify some work hours, as well as bumping it to the top of my personal studying list.

Setting up an environment turns out to be fairly simple. About the same complexity level as setting up for Common Lisp. On the one hand, the [build tool](http://leiningen.org/) gets you a copy of the language runtime so you don't need to worry about it, on the other, there's no equivalent to [`quicklisp-slime-helper`](https://github.com/quicklisp/quicklisp-slime-helper). Anyway, the steps are


- **install Emacs 24 or higher**. For me, this was already done. If I were doing it from scratch right now, I'd do it with `nix-env -i emacs`.
- **install leiningen**. This was technically done, but I had a ridiculously out-of-date version of `lein`, so I ended up uninstalling that and doing `nix-env -i leiningen`
- **add `cider-nrepl` to your `profiles.clj` file**. Once you've got `leiningen`, you should also have a file at `~/.lein/profiles.clj`. If you don't, just create it (I had to). Then add
```clojure
{ :user
  { :plugins
   [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
    [org.clojure/tools.nrepl "0.2.10"]] }}
```
I'm not sure the `tools.nrepl` line is strictly necessary, but figured I might as well (Despite this, I still get `WARNING: CIDER requires nREPL 0.2.7 (or newer) to work properly` at the top of every REPL I start up. So, I dunno, maybe I put that in the wrong place).
- **install the `clojure-mode` and `cider` Emacs addons**. This one caused a headache or two. I had already done some clojuring in the past, so I had an older version of `clojure-mode` installed, which was incompatible with `cider`. In the end, I had to tear it out and install a newer version. Assuming you don't have the same problem, you can just `M-x package-install clojure-mode` followed by `M-x package-install cider`.


And that's basically it. At that point, you can run `cider-jack-in` to get a `SLIME`-like interactive REPL running<a name="note-Sun-Apr-19-220141EDT-2015"></a>[|1|](#foot-Sun-Apr-19-220141EDT-2015). They're not exactly the same, notably the debugger and stack-trace in `cider`isn't *nearly* as interactive or useful as the one in `SLIME`, but you still get minibuffer argument hints and a macroexpander. If it weren't for the fact that `lein repl` takes something on the order of 5 seconds to start up, switching over from CL would be completely painless<a name="note-Sun-Apr-19-220152EDT-2015"></a>[|2|](#foot-Sun-Apr-19-220152EDT-2015).

## History

I mentioned a little while ago that I'm thinking about full-history data-stores. I've already kind of implemented [one](https://github.com/Inaimathi/fact-base), though it is tightly bound to a particular data-structure, and I've done [a bit of experimenting with a generalization](https://github.com/Inaimathi/cl-history). Having set up Clojure, I was inspired to do a bit more playing around.

```clojure
(ns history.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn make-archive
  ([apply-fn zero]
   {:into apply-fn :state zero :history () :zero zero})
  ([apply-fn zero fname]
   (let [f (io/as-file fname)]
     (when (not (.exists f))
       (with-open [w (io/writer fname)]
         (.write w (with-out-str (prn zero)))))
     {:into apply-fn :state zero :history () :zero zero :file f})))

(defn multiplex-archive [arc stream-vector]
  (assoc arc :streams stream-vector))

(defn commit-event [arc event]
  (let [ev-str (with-out-str (prn event))
        file (arc :file)
        streams (arc :streams)]
    (and file
         (with-open [w (io/writer file :append :true)]
           (.write w ev-str)))
    (and streams (doseq [s streams] (.write s ev-str)))))

(defn apply-event [arc event]
  (let [new-arc (assoc arc :state ((arc :into) (arc :state) event))]
    (if (arc :history)
      (assoc new-arc :history (cons event (arc :history)))
      new-arc)))

(defn new-event [arc event]
  (commit-event arc event)
  (apply-event arc event))

(defn load-archive [fname apply-fn]
  (with-open [in (java.io.PushbackReader. (io/reader fname))]
    (let [arc (make-archive apply-fn (edn/read in) fname)
          eof (gensym)]
      (reduce
       apply-event arc
       (take-while
        (partial not= eof)
        (repeatedly (partial edn/read {:eof eof} in)))))))
```

So, the idea is that an Archive is a thing with


- An application function: a function that takes a state and an event, and returns the result of applying that event to that state
- A zero: the starting state of this particular archive
- Optionally, a history: the sequence of events that lead from its zero to its current state<a name="note-Sun-Apr-19-220159EDT-2015"></a>[|3|](#foot-Sun-Apr-19-220159EDT-2015)
- Optionally, a file and some streams: places that we'll write any additional events we get<a name="note-Sun-Apr-19-220201EDT-2015"></a>[|4|](#foot-Sun-Apr-19-220201EDT-2015)


You make a new Archive by initializing some of the above points. You add an `event` to it by committing the `event`, then calling the application function on the Archives' current state. Load an Archive by opening a file, reading the Zero from it, then `reduce`ing the remainder of the records over said Zero with the application function. And that's that. If you wanted to see a previous state of the Archive, you'd just stop folding before you got to the last event.

The above implementation is a bit simpler than my earlier [CL-based equivalent](https://github.com/Inaimathi/cl-history), in that it doesn't yet deal with partial loads, timestamps *or* reconciliation. All of those look like they'll be trivial changes, and they won't impact the existing machinery at all<a name="note-Sun-Apr-19-220204EDT-2015"></a>[|5|](#foot-Sun-Apr-19-220204EDT-2015). The minimal is still fairly useful though. Here's an example use:

```clojure
(let [app (fn [arc ev]
            (case (get ev 0)
              :insert (let [[_ k v] ev] (assoc arc k v))
              :delete (let [[_ k] ev] (dissoc arc k))))
      zero {}]
  (defn make-history-table
    ([] (make-archive app zero))
    ([fname] (make-archive app zero fname)))
  (defn load-history-table [fname]
    (load-archive fname app)))
```

That's how you would go about declaring a history-aware table. You'd use it by making one, and running some `event`s against it.

```clojure
history.core> (make-history-table)
{:into #<core$eval7113$app__7114 history.core$eval7113$app__7114@58e793e4>,
 :state {}, :history (), :zero {}}

history.core> (new-event (make-history-table) [:insert :test "test"])
{:into #<core$eval7113$app__7114 history.core$eval7113$app__7114@58e793e4>,
 :state {:test "test"},
 :history ([:insert :test "test"]),
 :zero {}}

history.core> (new-event (new-event (make-history-table) [:insert :test "test"]) [:insert :another-test "bleargh"])
{:into #<core$eval7113$app__7114 history.core$eval7113$app__7114@58e793e4>,
 :state {:another-test "bleargh", :test "test"},
 :history ([:insert :another-test "bleargh"] [:insert :test "test"]),
 :zero {}}

history.core> (new-event (new-event (new-event (make-history-table) [:insert :test "test"]) [:insert :another-test "bleargh"]) [:delete :test])
{:into #<core$eval7113$app__7114 history.core$eval7113$app__7114@58e793e4>,
 :state {:another-test "bleargh"},
 :history ([:delete :test] [:insert :another-test "bleargh"] [:insert :test "test"]),
 :zero {}}

history.core>
```

This is a pretty stupid example, all things considered, but it illustrates how you'd go about putting together a minimally functional, history-aware data-structure. In reality, you'd declare your table to be an [`atom`](http://clojure.org/atoms) or [`agent`](http://clojure.org/agents), and declare a `change` function that updates its state with `new-event`. This is the main use-case I'm considering, so it might be prudent to just make that the default behavior of the library.

## Minor Notes

First impressions are really, *really* good. As [I've said before](/article?name=recommendations.html), I like Clojure. My impression of it is that it takes takes the best parts of Scheme and Common Lisp and runs with them. [`lein`](http://leiningen.org/) comes with basically all the stuff I like out of [`asdf`](https://common-lisp.net/project/asdf/), [`quicklisp`](http://www.quicklisp.org/) and [`quickproject`](http://www.xach.com/lisp/quickproject/), with the added perks of [good documentation](http://leiningen.org/#docs) *and* built-in consideration for [unit testing](https://github.com/technomancy/leiningen/blob/stable/doc/TUTORIAL.md#tests). The *only* complaint I have is the annoying startup delay whenever I do `lein something`. In particular, the `lein test` command takes long enough that I'm not sure I'd want to pipe it through `entr` keyed on `.clj` file changes. I'll let you know how it goes once I've done some real work with it.

* * *
##### Footnotes

1 - <a name="foot-Sun-Apr-19-220141EDT-2015"></a>[|back|](#note-Sun-Apr-19-220141EDT-2015) - If you run that inside a Clojure project directory, the REPL will also automatically load said project and enter its namespace.

2 - <a name="foot-Sun-Apr-19-220152EDT-2015"></a>[|back|](#note-Sun-Apr-19-220152EDT-2015) - Incidentally, I thought this was to do with my Free Software bent, because the OpenJDK has the reputation of being slower than the Oracle equivalent. I asked a friend who uses the non-Free version, and he confirmed that the REPL just plain takes a while to start up. Not sure how to feel about that; `M-x slime` sets up an interactive REPL in about a second at the outside, and gives feedback on progress in the meanwhile.

3 - <a name="foot-Sun-Apr-19-220159EDT-2015"></a>[|back|](#note-Sun-Apr-19-220159EDT-2015) - Optional because we may not need the full history in memory. Given my experiments with [`cl-notebook`](https://github.com/Inaimathi/cl-notebook), I currently believe that you don't really need in-memory history *unless* you want to do real-time history manipulation or traversal. So, you do want it *sometimes*, but it potentially saves a lot of memory if you can do without.

4 - <a name="foot-Sun-Apr-19-220201EDT-2015"></a>[|back|](#note-Sun-Apr-19-220201EDT-2015) - Again, optional because you might *only* want the in-memory representation without worrying about persisting it. I haven't come across this situation yet, but it might exist. It goes almost without saying that you want at least *one* of in-memory-history/tracking-file/tracking-streams, because if you have *none*, you don't really have a history-aware data structure. But depending on your use-case, you may need *only* one.

5 - <a name="foot-Sun-Apr-19-220204EDT-2015"></a>[|back|](#note-Sun-Apr-19-220204EDT-2015) - Though they *may* require some changes to the storage format, which is why I haven't published this little library quite yet.
