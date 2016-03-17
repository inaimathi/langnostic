So I'm now officially a Clojure programmer<a name="note-1"></a>[|1|](#foot-1). Which is to say, I have now been paid actual money to write Clojure. That brings the tally to 12 of the 14 languages in my title-bar<a name="note-2"></a>[|2|](#foot-2). Clojure wasn't a requirement for the project. But the stated aims included a few things that made a JVM language very attractive, so I rolled with it. In particular:

1. we want a cross-platform thick-client, complete with desktop GUI
2. the end result needs to be a single-click-launching application, because our target users are non-programmers

I can't give you details or source code, because this is a small, internal utility that doesn't make sense for public consumption. What I *can* tell you is that [`leiningen`s](http://leiningen.org/) `uberjar` capability hits the second goal for us, and a library called [Seesaw](https://github.com/daveray/seesaw) hits the first. There's also a bunch of observations I can make about the process without getting into any concrete code. And this is ostensibly a journal about my programming experiences.

So lets do this thing.

## Seesaw

Seesaw is a bearable wrapper around [Swing](https://en.wikipedia.org/wiki/Swing_%28Java%29).

I should note, by the way, that literally everything I've heard about Swing gave me the impression that it would be a trashy, slow, unsafe, fugly UI toolkit that I'd want nothing to do with. And I mean, I dunno; maybe there's better ones out there, but compared to steaming piles like Ruby's [`green_shoes`](https://ashbb.github.io/green_shoes/), or half-way-decent-but-external-dependency-ridden things like [mcclim](https://common-lisp.net/project/mcclim/), Seesaw is pretty damn good.

It's composable, minimal, robust *and* cross-platform enough for my purposes. It also isn't as threading-hostile as I've been led to believe. The application we built ended up using a lot of [Clojures' synchronization primitives](http://stackoverflow.com/questions/9132346/clojure-differences-between-ref-var-agent-atom-with-examples), along with some actual calls to `(.start (Thread. (fn [] ...)))`. No problems so far, at least.

There's already a [short Seesaw tutorial](https://gist.github.com/daveray/1441520) out there, so I won't write more about it. If you're interested, go check that out.

## Non-Strictness (Sometimes)

Clojure is non-strict in a bunch of ways. I hadn't realized this initially, but it became obvious rather quickly. The more I learn about the language, the more I think that it's less "Split the difference between Common Lisp/Scheme" and more "Split the difference between Scheme/Haskell". Which I like, as you might be able to deduce from the afore-mentioned title-bar. Kinda wish it gave me a few more compile-time guarantees, but I'm not about to complain too hard, given [what](https://www.ruby-lang.org/en/) I've [been](https://nodejs.org/en/) working with lately.

### Non-Strict `map`

Anyway, it's sometimes non-strict. And that came back to bite me in the ass exactly twice. The first one was a situation that looked something like

```clojure
(defn foobar [& things]
  (let [a (map foo (filter bar things))]
    (map make-an-http-request
         (zip a (map bytes! things)))))
```

When I called `foobar` from the REPL, everything worked fine. But the place I called `foobar` elsewhere in the program *threw away its return value*. In other words, it looked something like

```clojure
(defn do-a-thing [blah]
  ...
  (foobar a b c d e f)
  (show-request-sent-message)
  ...)
```

Apparently, this means that the `map`s inside of `foobar` are never forced *(since we never look at their return values)*, and the attached effectful operations therefore never happen either. The solution in my case was to re-write `foobar` using `doseq` instead of `map`.

### Lazy IO

The second one had to do with `line-seq`. Basically, I was trying to process an incoming file line-by line. Something like

```clojure
(defn transform-lines-somehow [fname]
  (with-open [f (io/reader fname :encoding "ISO-8869-1")]
    (map (fn [ln] ...) (line-seq f))))
```

The trouble here is that `line-seq` tries to be lazy about its output. And we've already seen that `map` does the same. However, `with-open` _doesn't_. So, when I call `transform-lines-somehow` later in the program, what I get instead of a sequence of transformed lines is

```
IOException Stream closed  java.io.BufferedReader.ensureOpen (BufferedReader.java:115)
```

This is less than ideal.

Wrapping that `map` in a [`doall`](https://clojuredocs.org/clojure.core/doall)<a name="note-3"></a>[|3|](#foot-3) does the right thing. So ...

```clojure
(defn transform-lines-somehow [fname]
  (with-open [f (io/reader fname :encoding "ISO-8869-1")]
    (doall (map (fn [ln] ...) (line-seq f)))))
```

## `leiningen` Is Fucking Awesome

In general, [`lein`](http://leiningen.org/) is either the best, or one of the best build/dependency management tools I've ever used. It's wider in scope than either [`quicklisp`](https://www.quicklisp.org/beta/) or [`bundler`](http://bundler.io/) because it manages your language version as well as your libraries. It's simpler to use and less error-prone than [`cabal`](https://www.haskell.org/cabal/users-guide/) or [`pip`](https://pypi.python.org/pypi/pip). And it seems more easily extensible than any of them. At some point, I need to sit down and read the thing so I can understand how to either port or generalize it to every language ever. I mean, granted, maybe that's basically [`nix`](http://nixos.org/nix/), but it still sounds like a fun exercise to build your own.

Apart from managing library installs and versions of `clojure`, it also gives you basic testing capability<a name="note-4"></a>[|4|](#foot-4), project templating and binary-ish distribution out-of-the-box in the form of `lein test`, `lein new` and `lein uberjar` respectively. "Binary-ish", because when you ship someone the stand-alone `.jar` file generated by `uberjar`, they still need an appropriate version of the [`JVM`](https://docs.oracle.com/javase/specs/jvms/se7/html/) to run it<a name="note-5"></a>[|5|](#foot-5), but this seems like it might be Good Enough for the general case.

## `Mis(sing|named)` Functions

There's no `parse-integer`. The alternatives are either using `read-string`, or `(comp int bigint)` *(that is, cast the string to a `bigint`, then cast the result to an `int`)*. Neither of these are particularly satisfactory, although when push comes to shove, I think I'd rather take the second one.

The problem with `read-string` is that it can come back with arbitrary values, not just numbers, so I need to do the typecheck afterwards. And while `(read-string "(sh \"rm -rf /*\")")` isn't *quite* as bad as it looks, I'd still rather steer as far away from the possibility when dealing with strings potentially making their way to me through the network. Clojure doesn't have reader macros, so this also *isn't* quite as bad as the Common Lisp arbitrary-code-at-read-time thing that would hit you in the equivalent situation

The cast composition might fail. In particular, since we might be dealing with strings from untrusted sources,

```clojure
user=> ((comp int bigint) "foo")
NumberFormatException For input string: "foo"  java.lang.NumberFormatException.forInputString (NumberFormatException.java:65)
user=>
```

we'd have to bust out the `try`/`catch` in order to prevent the above situation.

In a similar vein, there is no general `member?` function. There _is_ a [`contains?`](https://clojuredocs.org/clojure.core/contains_q), but it doesn't do what you think it will. Apparently, it's a [common misconception](http://stackoverflow.com/a/3249401/190887). It takes a collection and a value, and tells you if that value is present *as a key* in that collection. So `(contains? [:a :b :c] :b)` will give you back `false`, while `(contains? [:a :b :c] 1)` will return `true`. To be fair, it's relatively easy to define your own, just make sure you catch the edge-cases regarding `false` and `nil` in the different collection types. This still smacks of [the `min` situation in Go](https://groups.google.com/forum/#!searchin/golang-nuts/min$20max/golang-nuts/dbyqx_LGUxM/tLFFSXSfOdQJ) though, and it seems like it would be much more satisfying to just friggin' include a definition of `member?` or `in?` in `clojure.core`. Not entirely sure why it hasn't happened yet.

## Unhelpful Error Reporting

The error-reporting here is among the worst I've ever personally witnessed<a name="note-6"></a>[|6|](#foot-6). Every error gives a giant JVM stack trace. If you're very *very* lucky, the top 30 items won't be language dispatch mechanisms, and you can get a function name in your own module to go and debug. Far more often, I've gotten generic `java.NullPointerException` errors or similar leading either to something like `java.io`, or one of the other assorted infrastructure pieces. It would be very nice to know which part of the code I wrote is responsible for the error I'm seeing.

Strangest so far were errors stemming from naming conflicts, and module dependencies.

For example, overriding `clojure.core` names in your own modules doesn't error at all. You're perfectly free to define a namespace called `foo`, and then within it to define `foo/get`. Things will work fine. You'll be able to run `lein repl` just fine. But when you try to run `lein uberjar` or `lein run`, you will occasionally get errors that look like [this](http://stackoverflow.com/questions/7991685/confusing-clojure-compile-errors-bad-line-reporting). Which<a name="note-7"></a>[|7|](#foot-7) doesn't really do a good job pointing at the real problem. The recommended workaround is to use `:exclude` explicitly in your `ns` calls before using bound names, but I ended up just renaming my offending functions. There were few enough of them that it wasn't too big a deal.

For another example, early on I got this warning a lot:

```
WARNING!!! version ranges found for:
[seesaw "1.4.2"] -> [j18n "1.0.1"] -> [org.clojure/clojure "[1.2,1.5)"]
Consider using [seesaw "1.4.2" :exclusions [org.clojure/clojure]].
```

I'm quasi-used to ignoring warnings, because they tend to *be* ignorable, so I didn't think much of it. However, the above seemingly caused sporadic issues that manifested as errors *in other libraries*. Like [this one](https://github.com/Raynes/conch/issues/27). I'm convinced that issue was actually caused by the above warning, because it stopped happening entirely once I changed the `seesaw` include in my `project.clj` to `[seesaw "1.4.2"] => [seesaw "1.4.2" :exclusions [org.clojure/clojure]]`, but I never would have come to that conclusion based only on the diagnostic messages.

The _helpful_ errors are name resolution things. `Can't resolve foo in scope your.module.name.here` immediately tells me what went wrong, where, and gives me an accurate idea of how to fix it. Basically, any time you're stuck looking further into the stack trace than the first line, you may as well give up all hope. I guess Java programmers won't mind this so much, but having grown up on Common Lisp and Scheme, this is a profoundly annoying part of constructing and debugging programs in Clojure-land. It's still not more annoying than the available alternatives for this particular project, so I'm happy on balance, but there really is more pain involved than there ought to be. Especially when you also consider the relatively slow startup of `lein repl` versus `cabal repl`, `sbcl` or `sml`.

## `test.check` Limitations

Absolute last thing, and this is really a nitpick given how seldom this situation came up for me: `test.check` doesn't seem to be capable of file IO in properties. No idea why, but I tried writing something along the lines of

```
(prop/for-all [csv dummy-csv-generator]
  (with-temp-file [tmp "test" ".csv"]
    (spit tmp (str/join \newline (map (partial str/join ",") tmp)))
    (= (map first csv) (get-first-column tmp))))
```

and got `NullPointerExceptions` thrown back at me every time. The "fix" ended up being `deftest` instead of `defspec` to test the finger-quotes functions that had to do disk IO. That's probably good enough, but I've got mildly less confidence that there won't be a bug unearthed at some point in the future by a(n un)lucky output.

## Conclusions

I stand by my [earlier recommendation](http://langnostic.inaimathi.ca/posts/recommendations). If you're a programmer who's familiar with the JVM, and you don't hate it, and you're looking to learn your first Lisp, Clojure is very probably the way to go for you. The language is clean, elegant, [well documented](https://clojuredocs.org/), and it has excellent [interop cpabilities](http://clojure.org/reference/java_interop) with the Java ecosystem, even when no one bothers writing fantastic wrapper libraries like [Seesaw](https://github.com/daveray/seesaw). So, absolutely, thumbs up. If you're in the situation I describe above, you could certainly do worse.

If you're *already* familiar with a [Lisp](https://common-lisp.net/) or [two](http://schemers.org/), and already have some experience working with [good](https://ocaml.org/) type [systems](http://sml-family.org/), it may be a less valuable but still fun addition to your language arsenal.

* * *
##### Footnotes

1 - <a name="foot-1"></a>[|back|](#note-1) - Or rather, I have been for about two weeks now. Did I mention it's getting harder and harder for me to dedicate time to writing this blog? At this point, anything you see come up here effectively happened last month. That's a *really* shit lag, but I'm not sure I can do anything about it.

2 - <a name="foot-2"></a>[|back|](#note-2) - The last two are Smalltalk and SML, in case you were wondering. I'm honestly not sure I even want to pursue a Smalltalk contract because it's sort of in decline, even in the industries that used to use it extensively. Standard ML sounds like it might be a stretch too; seems like a better approach might be to get enough ML skill under my belt, then keep an eye out for parts of problems that naturally lend themselves to the language strengths, in contexts where a small amount of additional Balkanization won't cause any damage. Sort of how I finally wound up using Clojure in a production context. The only real downside of the approach is that I have no idea how long it'll take to spot an appropriate opportunity.

3 - <a name="foot-3"></a>[|back|](#note-3) - Which is basically a strictness annotation.

4 - <a name="foot-4"></a>[|back|](#note-4) - Mild tangent; there has apparently been movement in the Clojure testing landscape since `lein` was first written. Because, while it gives you `clojure.test` by default, there seem to be lots of people that use `midje`. At least enough of them for the Travis CI guys and gals to write [this](https://docs.travis-ci.com/user/languages/clojure#Using-Midje-on-travis-ci.org) guide for running continuous integration with it. The situation at once showcases how bad an idea it is to make that kind of assumption in a languages' build tools, and demonstrates `lein`s level of flexibility.

5 - <a name="foot-5"></a>[|back|](#note-5) - And if you introduce external dependencies by using [`conch`](https://github.com/Raynes/conch#conch) or [`sh`](https://clojuredocs.org/clojure.java.shell/sh), they'll need those too, obviously.

6 - <a name="foot-6"></a>[|back|](#note-6) - The worst would probably go to one of Prolog, or C. I've been told Scala gives you some pretty inscrutable things, but I haven't worked with it.

7 - <a name="foot-7"></a>[|back|](#note-7) - Unless you're an experienced Clojurer, or Javanaut, I assume. It seems like you might be able to tease meaning out of this one with sufficient background, but I'm not inclined to bet on it one way or the other.
