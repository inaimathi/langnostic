I don't know why this keeps coming up lately, but it does. So, here we go:

### My Recommendation

If you want to learn your first Lisp and already know something about the JVM<a name="note-Fri-Jan-24-105200EST-2014"></a>[|1|](#foot-Fri-Jan-24-105200EST-2014), you should learn [Clojure](http://clojure.org/)<a name="note-Fri-Jan-24-105205EST-2014"></a>[|2|](#foot-Fri-Jan-24-105205EST-2014). Otherwise, you should learn Scheme. Specifically, I recommend going the route that takes you through [Racket](http://racket-lang.org/)<a name="note-Fri-Jan-24-105208EST-2014"></a>[|3|](#foot-Fri-Jan-24-105208EST-2014), and possibly through [SICP](https://github.com/sarabander/sicp-pdf/raw/master/sicp.pdf) or [HTDP](http://htdp.org/).

If you absolutely, positively must, I guess go ahead and learn Common Lisp.

### Why

Clojure the language, as opposed to the current, main, half-fused-with-JVM implementation, is cleaner and more consistent than Common Lisp, which should help you learn it more easily. I've gotten back talk about how there are lots more noobs learning Clojure, and as a result their libraries are in some disarray, and about the fact that the JVM is a sack of donkey balls you have to bite into every time you hit some sort of error<a name="note-Fri-Jan-24-105212EST-2014"></a>[|4|](#foot-Fri-Jan-24-105212EST-2014), and about the general Clojure community pre-disposition to fashion trends. All of which may or may not be true, but I'm specifically talking about *the language*, not its ecosystem or stalwarts. Now granted, all of Racket, Clojure and Common Lisp are


1.   built out of s-expressions
1.   have `defmacro`


so depending on how much work you're willing to put in, you can do whatever the fuck you want in all of them<a name="note-Fri-Jan-24-105237EST-2014"></a>[|5|](#foot-Fri-Jan-24-105237EST-2014). However, in addition to knowing about prefix notation, and macros, and general Lisp program structure, here's an incomplete list of idiosyncrasies of Common Lisp that you have to commit to memory before you can be effective in it:


- Functions and variables are in different namespaces, and each has constructs that deal with them explicitly. You need to use `defvar`/`defun` and `let`/`flet` depending on whether you're using functions or variables. If you're passing symbol names around, symbols that denote variables can be sent around as `'foo` whereas symbols that denote functions should be sent around as `#'foo`. If you're trying to pass a function around in the variable `foo`, you need to `(funcall foo arg)` or possibly `(apply foo args)`, rather than just `(foo arg)`.
- Most functions that deal with lists are functional, except the standard `sort` and the default `mapcan`, both of which destructively modify the list you pass them.
- You can define methods for your classes, but can't easily use certain default names that are bound to top-level functions. Such as `length`, or the arithmetic primitives. Which is why you frequently see methods like `duration-add` or `matrix-mult`.
- There are 7 commonly used equality operators, `eq`, `eql`, `equal`, `equalp`, `string=`, `char=` and `=`<a name="note-Fri-Jan-24-105253EST-2014"></a>[|6|](#foot-Fri-Jan-24-105253EST-2014), each of which has mildly different, sometimes implementation-specific, behavior. Granted, because CL isn't a pure language, you need at least 2 of those, but 7 is still a bit much to have people memorize.
- There are three different local binding mechanisms that you must decide between depending on whether you want to be able to refer to earlier symbols in the same binding set, or whether you want symbols to be able to refer to themselves. You use `let`/`flet` if you don't care, `let*` for variables where you want later bindings to be able to refer to earlier ones, and `labels` for functions where you want bindings to be able to refer to themselves or earlier bindings.
- There are many, *many* implementations of Common Lisp. The popular ones at the moment are [SBCL](http://www.sbcl.org/) and [CCL](http://ccl.clozure.com/download.html), but I've personally seen [CLISP](http://www.clisp.org/), [ECL](http://ecls.sourceforge.net/) and [Lispworks](http://www.lispworks.com/products/lispworks.html) around too. More are available, and you might run into a lot of them in the wild. If you want to write portable code, you have to jump through some hoops. The implementation-specifics range from the finer points of equality operator behaviors, to the behavior of `handler-case`<a name="note-Fri-Jan-24-105327EST-2014"></a>[|7|](#foot-Fri-Jan-24-105327EST-2014), to the types you can specialize on with `defmethod`, to the presence and behavior of threads, to the contexts in which you can pass streams around, to the names of various extension functions. For a small but representative example, take a look at [what it takes to temporarily change your current directory in an implementation-independent way](https://github.com/Inaimathi/cl-git-fs/blob/master/util.lisp#L12-L56).
- Indexing into different constructs takes different forms. You need `nth` for lists, `aref` for arrays and vectors, `gethash` for hashes and `slot-value` for CLOS instances<a name="note-Fri-Jan-24-105330EST-2014"></a>[|8|](#foot-Fri-Jan-24-105330EST-2014).
- You can't use the symbol `t` anywhere. No, not even local scopes. If you try, you'll get warnings or errors<a name="note-Fri-Jan-24-105333EST-2014"></a>[|9|](#foot-Fri-Jan-24-105333EST-2014), because `t` is the top-level designated Truth symbol, even though anything other than `NIL` [evaluates truthily](http://langnostic.blogspot.ca/2013/05/truthy-and-falsy-vs-explicit.html) in a boolean context.
- A hash isn't a list, and a CLOS instance isn't anything like either. One way you'd *like* them to be similar is when you're traversing them. It seems fairly reasonable to expect `map`-likes to operate on hashes by treating them as a sequence of key/value pairs, and instances by treating their bound slots as key/value pairs. This is not how things work. If you want to map over instances that way, you need to do something like [this](https://github.com/Inaimathi/cl-mop/blob/master/package.lisp) and [this](https://github.com/Inaimathi/cl-mop/blob/master/cl-mop.lisp#L6-L21). If you want to `map` over hashes, you either use the hilariously mis-named `maphash`<a name="note-Fri-Jan-24-105336EST-2014"></a>[|10|](#foot-Fri-Jan-24-105336EST-2014) or some idiosyncratic piece of the [`loop` DSL](http://www.gigamonkeys.com/book/loop-for-black-belts.html) that lets you iterate over hash keys and hash values.
- Common Lisp is case-insensitive. It takes whatever symbol input from you and upcases it internally. So `foo-bar` and `foo-Bar` both become `FOO-BAR` behind the scenes. This is usually not a huge problem, unless you try to interoperate cleanly with newer data standards like CSS or JSON. That leaves you fumbling with `string`s in situations where `symbol`s and/or `keyword`s really ought to do.


Like I said, this is a small sample. Just the stuff I thought of off the top of my head. I'm sure I could come up with more if I put a day or two into it. And I'm far from the most experienced Lisper out there, others would have more finer points for you, I'm sure. But that's half the problem with little issues like this; experienced Lispers completely forget about them. It's the newbs that have trouble cramming these things into their heads.

When I take a good look at that list, and then imagine the situations that led to each element, it's difficult to conclude that a wrong decision was made at any given point in time. Unfortunately, the sum of all of those potentially correct decisions is a giant system, the inherent rules of which look inconsistent if not outright hostile to human minds.

I don't know if Clojure solves all of them.

I've done very little work with it, for reasons entirely unrelated to the language. For all I know, when you get deep enough into it, you get to inconsistencies and/or restrictions which are worse than anything I've pointed out or could. Also, just in the interests of clarity, in case the "Common Lisp" sitting comfortably at the top of this blogs' tag list wasn't enough of a hint, I use Common Lisp. I *like* Common Lisp. But it's primarily because I've internalized enough minutia to feel comfortable in it.

But do me a favor, if you're a CL user, either hop over to this [web REPL](http://tryclj.com/), or install `leiningen` then hop into your local `lein repl` and type along here:

```clojure
user=> (def thing [8 7 6 5 4 3 2 1])
#'user/thing
user=> (thing 0)
8
user=> (thing 3)
5
user=>(map (fn [n] (+ 3 n)) thing)
(11 10 9 8 7 6 5 4)
user=> (def thing {:a 1 :b 2 :c 3})
#'user/thing
user=> (thing :c)
3
user=> (thing :d 6)
6
user=> (thing :a 6)
1
user=> (map (fn [[k v]] (+ v 2)) thing)
(3 4 5)
user=> (def thing #{1 2 3 4 5}) ;; a set, in case you were wondering
#'user/thing
user=> (thing 3)
3
user=> (thing 0)
nil
user=> (map (fn [a] (+ a 2)) thing)
(3 4 5 6 7)
user=> (def triple (fn [a] (* a 3)))
#'user/triple
user=> (triple 4)
12
user=> (map triple thing)
(3 6 9 12 15)
user=> (map (fn [a] (let [t (- (triple a) 5)] (* 2 t))) thing)
(-4 2 8 14 20)
```

Now think about how you would go about explaining to a novice programmer that it has to be more complicated than that.

* * *
##### Footnotes

1 - <a name="foot-Fri-Jan-24-105200EST-2014"></a>[|back|](#note-Fri-Jan-24-105200EST-2014) - And don't have a strong dislike for it, obviously.

2 - <a name="foot-Fri-Jan-24-105205EST-2014"></a>[|back|](#note-Fri-Jan-24-105205EST-2014) - Install it through [Leiningen](https://github.com/technomancy/leiningen), which is available in the [Debian repos](http://packages.debian.org/sid/leiningen) in stable and unstable.

3 - <a name="foot-Fri-Jan-24-105208EST-2014"></a>[|back|](#note-Fri-Jan-24-105208EST-2014) - Yes, I'm fully aware that the Racket guys are trying to push this "We're totally not Scheme" thing. They're close enough from an external perspective. Just don't tell Jay McCarthy I said so.

4 - <a name="foot-Fri-Jan-24-105212EST-2014"></a>[|back|](#note-Fri-Jan-24-105212EST-2014) - Which is certainly true, but mildly preferable to the alternative as long as you're used to that sort of thing.

5 - <a name="foot-Fri-Jan-24-105237EST-2014"></a>[|back|](#note-Fri-Jan-24-105237EST-2014) - Except that Clojure is apparently missing Reader macros, which I always thought were kind of half-assedly implemented in Common Lisp. For what I consider the full-ass version, take a look at [how Haskell does it](http://www.haskell.org/haskellwiki/Quasiquotation).

6 - <a name="foot-Fri-Jan-24-105253EST-2014"></a>[|back|](#note-Fri-Jan-24-105253EST-2014) - Plus how many ever `*-equal` functions you define for your own classes.

7 - <a name="foot-Fri-Jan-24-105327EST-2014"></a>[|back|](#note-Fri-Jan-24-105327EST-2014) - The Common Lisp answer to the problems that call for `try`/`catch` in other languages.

8 - <a name="foot-Fri-Jan-24-105330EST-2014"></a>[|back|](#note-Fri-Jan-24-105330EST-2014) - For the last, you can also define your own selectors using `:reader` or `:accessor` declarations.

9 - <a name="foot-Fri-Jan-24-105333EST-2014"></a>[|back|](#note-Fri-Jan-24-105333EST-2014) - Which specific warning or error depends on implementation.

10 - <a name="foot-Fri-Jan-24-105336EST-2014"></a>[|back|](#note-Fri-Jan-24-105336EST-2014) - Because it's not very much like `map`. It returns nil and works by side-effect. Meaning that if you expect a sequence from it, you'll need to construct it yourself.
