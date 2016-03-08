So I'm now officially a Clojure programmer. Which is to say, I have now been paid actual money to write Clojure. That brings the tally 12 of the 14 languages in my title-bar *(the last two are Smalltalk and SML, in case you were wondering)*. Clojure wasn't a requirement for the project. But the stated aims included a few things that made a JVM language very attractive, so I rolled with it. In particular:

1. we want a cross-platform, thick-client, complete with GUI
2. the end result needs to be a single-click-launching application, because our targets are non-programmers

I can't give you details or source code, because this is internal stuff that doesn't make sens for public consumption, but I can talk a bit about the high level and the libraries used. That second goal is going to be hit by `lein uberjar`, which will create a self-contained file, complete with all project dependencies other than `java` itself. A library called [Seesaw](https://github.com/daveray/seesaw) will help with the first.

# Seesaw Summary

Seesaw is a bearable wrapper around [Swing](TODO). I should note, by the way, that literally everything I've heard about swing prepared me for it being a trashy, slow, unsafe, fugly UI toolkit. I mean, I dunno; maybe there's better ones out there, but compared to steaming piles like Ruby's [`green_shoes`](TODO), or half-way-decent-but-external-dependency-ridden things like [mcclim](TODO), Seesaw is pretty damn good. It's composeable, minimal, robust *and* cross-platform enough for my purposes.

So I like it.

Not sure if that takes my Lisp-cred down a peg or two, but whatever.

There's already a [short tutorial](TODO - seesaw REPL tutorial), so I won't take up too much space with talking about it. But I do recommend that you go check it out.

# General Notes About Clojure Development

There's a bunch of things that took me mildly by surprise about Clojure programming in general. Which I note here more for my future reference than your enlightenment, but it may be useful to you nontheless.

Firstly, Clojure is non-strict in a bunch of ways. I hadn't realized this initially, but it became obvious rather quickly. The more I learn about the language, the more I think that it's less

> Split the difference between Common Lisp/Scheme

and more

> Split the difference between Scheme/Haskell

Which I like, as you might be able to deduce from the afore-mentioned language bar. Kinda wish it gave me a few more compile-time guarantees, but I'm not about to complain too hard, given what I've been working with lately.

Anyway, it's sometimes non-strict. And that came back to bite me in the ass exactly twice. The first one was a situation that looked something like

```clojure
(defn foobar [& things]
  (let [a (map foo (filter bar things))]
    (map make-an-http-request
         (zip a (map bytes! things)))))
```

When I called `foobar` from the REPL, everything worked fine. But the place I called `foobar` elsewhere in my application *threw away its return value*. In other words, it looked something like

```clojure
(defn do-a-thing [blah]
  ...
  (foobar things)
  (show-request-sent-message)
  ...)
```

Apparently, this means that the `map`s inside of `foobar` are never forced (since we never look at their return values), but the attached effectful operations never happen either. The solution in my case was to re-write `foobar` using `doseq` instead of `map`, which has exactly the behavior I want here.

The second one had to do with `line-seq`. Basically, I was trying to process an incoming file line-by line. Something like

```clojure
(defn transform-lines-somehow [fname]
  (with-open [f (io/reader fname :encoding "ISO-8869-1")]
    (map (fn [ln] ...) (line-seq f))))
```

The trouble here is that `line-seq` tries to be lazy about its output. And we've already seen that `map` does the same. However, `with-open` _doesn't_. So, when I call `transform-lines-somehow` later on, what I get instead of a sequence of transformed lines is

```
IOException Stream closed  java.io.BufferedReader.ensureOpen (BufferedReader.java:115)
```

This is less than ideal.

What I ended up having to do is wrap that `map` in a [`doall`](https://clojuredocs.org/clojure.core/doall), which is basically a strictness annotation. So ...

```clojure
(defn transform-lines-somehow [fname]
  (with-open [f (io/reader fname :encoding "ISO-8869-1")]
    (doall (map (fn [ln] ...) (line-seq f)))))
```

in this situation will do exactly what you think it will.


- overriding names defined in `clojure.core` is annoying, but the error doesn't show up until you try to `lein (uber)?jar` your project, at which point it complains about weird pointer errors
- `lein uberjar` is fucking awesome for distributing to non-developers
- lacks a real `parse-integer`. The alternatives are either `read-string` (which might come back with arbitrary values) or an `int . bigint` cast (which I have no particular objection to, except that you then suddenly have to bust out `try`/`catch`)
- `contains?` doesn't do what you think it does
- impenetrable errors. So far,
1. the above `uberjar` error for shadowing clojure.core functions
2. a warning for a `seesaw` dependency chain ended up manifesting as the inability to load `test.check`
