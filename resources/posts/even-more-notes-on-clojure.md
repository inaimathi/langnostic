I'm feeling a lot better intellectually than I have in a very long while. Recently, I've been doing some interesting development work.

In between intermittent bouts of interviewing with random companies, I mean. I've been trying to scare up a bit more contract work, but so far each of them hold the position of "No, we only want full-time staff. We only want people committed to our company". Which strikes me as a pretty stupid thing to say, honestly[^sore-spot].

[^sore-spot]: Both for the implication that contractors can't be committed, and for the hint that they'll be running into overtime work, but don't particularly want to pay for it. This may be a recent sore-spot of mine, so don't take it _too_ seriously, but I've been thinking about it. It seems to me that accepting a flat weekly rate for work puts tech managers in the position that they _may as well_ gamble with employees' evenings and weekends. Which means they need to pay less attention to internal infrastructure than is good for them, and they get to do flat out moronic things like opt to delay automating a task in faor of having devs handle it manually. This. Does. Not. Work. Seriously; at best, you're kneecaping your productivity, at worst, you'll get employees that want very badly to leave. So a) fuck you if you're a manager that does this, and b) if you're a developer that accepts $`n`/year, be aware that you're enabling the situation.

Anyway, before I get too carried away, what I've been doing is some contract work on an interesting compiler project, along with prototyping a project or two of my own. All in Clojure. Which is in danger of becoming my language of choice for general development purposes. Honestly, it would very probably be there already if there were any native implementations. As it stands though, there's only Clojure and Clojurescript, the second of which I haven't poked into quite yet. Anyhow, given that I've been doing serious work with it for a couple of weeks, it's about time for some more notes.

## Even More Notes On Clojure

### Bad

#### It's still on the [JVM](https://docs.oracle.com/javase/specs/jvms/se7/html/)

Which, given my new hardware isn't as bad as it could be. In particular, the dreaded JVM startup time is down to five Mississipi. That's nowhere _near_ as fast as the startup times for `sbcl` or `ghci`, but it's not an unbearable amount of time to wait for the occasional `REPL`/server restart. The real problem here turns out to be with the error reporting. JVM stack traces are a giant shitpile at the best of times, and throwing in five or six layers of `clojure_invoke.primitive`-style lines does not help readability any. Basically, if you're going to use Clojure seriously, you need to get used to the fact that error reporting is garbage, and won't help you _at all_ unless you can glean enough information from the first line of a stack trace. This is in stark contrast to Common Lisp errors (which are phenomenally helpful pretty much everywhere), and Haskell errors (which are uniformly impenetrable, but at least always manage to narrow you down to a file and line). It's an old gripe, but nothing seems to have been done quite yet. There's an effort called [`clojure.spec`](http://clojure.org/about/spec) that looks like it'll eventually be interesting, but nothing to ease your pain _now_.

#### The anonymous function syntax has some odd corners

In particular, it has some problems with functions that return vectors or maps. I'm ... not entirely sure why, but

```
; CIDER 0.8.2 (Java 1.8.0_92, Clojure 1.8.0, nREPL 0.2.12)
WARNING: The following required nREPL ops are not supported:
apropos classpath complete eldoc info inspect-start inspect-refresh inspect-pop inspect-push inspect-reset macroexpand ns-list ns-vars resource stacktrace toggle-trace-var toggle-trace-ns undef
Please, install (or update) cider-nrepl 0.8.2 and restart CIDER
WARNING: CIDER's version (0.8.2) does not match cider-nrepl's version (not installed)
langnostic.core> #([1 2 3 4])
#object[langnostic.core$eval1177$fn__1178 0x327b59fa "langnostic.core$eval1177$fn__1178@327b59fa"]
langnostic.core> (#([1 2 3 4]))
ArityException Wrong number of args (0) passed to: PersistentVector  clojure.lang.AFn.throwArity (AFn.java:429)

clojure.lang.ArityException: Wrong number of args (0) passed to: PersistentVector
 at clojure.lang.AFn.throwArity (AFn.java:429)
    clojure.lang.AFn.invoke (AFn.java:28)
    langnostic.core$eval1181$fn__1182.invoke (form-init2885131667863527703.clj:1)
    langnostic.core$eval1181.invokeStatic (form-init2885131667863527703.clj:1)
    langnostic.core$eval1181.invoke (form-init2885131667863527703.clj:1)
    clojure.lang.Compiler.eval (Compiler.java:6927)
    clojure.lang.Compiler.eval (Compiler.java:6890)
    clojure.core$eval.invokeStatic (core.clj:3105)
    clojure.core$eval.invoke (core.clj:3101)
    clojure.main$repl$read_eval_print__7408$fn__7411.invoke (main.clj:240)
    clojure.main$repl$read_eval_print__7408.invoke (main.clj:240)
    clojure.main$repl$fn__7417.invoke (main.clj:258)
    clojure.main$repl.invokeStatic (main.clj:258)
    clojure.main$repl.doInvoke (main.clj:174)
    clojure.lang.RestFn.invoke (RestFn.java:1523)
    clojure.tools.nrepl.middleware.interruptible_eval$evaluate$fn__655.invoke (interruptible_eval.clj:87)
    clojure.lang.AFn.applyToHelper (AFn.java:152)
    clojure.lang.AFn.applyTo (AFn.java:144)
    clojure.core$apply.invokeStatic (core.clj:646)
    clojure.core$with_bindings_STAR_.invokeStatic (core.clj:1881)
    clojure.core$with_bindings_STAR_.doInvoke (core.clj:1881)
    clojure.lang.RestFn.invoke (RestFn.java:425)
    clojure.tools.nrepl.middleware.interruptible_eval$evaluate.invokeStatic (interruptible_eval.clj:85)
    clojure.tools.nrepl.middleware.interruptible_eval$evaluate.invoke (interruptible_eval.clj:55)
    clojure.tools.nrepl.middleware.interruptible_eval$interruptible_eval$fn__700$fn__703.invoke (interruptible_eval.clj:222)
    clojure.tools.nrepl.middleware.interruptible_eval$run_next$fn__695.invoke (interruptible_eval.clj:190)
    clojure.lang.AFn.run (AFn.java:22)
    java.util.concurrent.ThreadPoolExecutor.runWorker (ThreadPoolExecutor.java:1142)
    java.util.concurrent.ThreadPoolExecutor$Worker.run (ThreadPoolExecutor.java:617)
    java.lang.Thread.run (Thread.java:745)

```

Incidentally, this is one of those bizarre errors that at first glance seems to have nothing to do with what caused it. I'm not passing arguments to a `PersistentVector` anywhere, but I'm assuming the implementation details of that anonymous function notation involve doing so, and it's leaking out in this error case.

Similarly...

```
langnostic.core> #({:a 1 :b 2})
#object[langnostic.core$eval1187$fn__1188 0x7c62dc64 "langnostic.core$eval1187$fn__1188@7c62dc64"]
langnostic.core> (#({:a 1 :b 2}))
ArityException Wrong number of args (0) passed to: PersistentArrayMap  clojure.lang.AFn.throwArity (AFn.java:429)

clojure.lang.ArityException: Wrong number of args (0) passed to: PersistentArrayMap
 at clojure.lang.AFn.throwArity (AFn.java:429)
    clojure.lang.AFn.invoke (AFn.java:28)
    langnostic.core$eval1191$fn__1192.invoke (form-init2885131667863527703.clj:1)
    langnostic.core$eval1191.invokeStatic (form-init2885131667863527703.clj:1)
    langnostic.core$eval1191.invoke (form-init2885131667863527703.clj:1)
    clojure.lang.Compiler.eval (Compiler.java:6927)
    clojure.lang.Compiler.eval (Compiler.java:6890)
    clojure.core$eval.invokeStatic (core.clj:3105)
    clojure.core$eval.invoke (core.clj:3101)
    clojure.main$repl$read_eval_print__7408$fn__7411.invoke (main.clj:240)
    clojure.main$repl$read_eval_print__7408.invoke (main.clj:240)
    clojure.main$repl$fn__7417.invoke (main.clj:258)
    clojure.main$repl.invokeStatic (main.clj:258)
    clojure.main$repl.doInvoke (main.clj:174)
    clojure.lang.RestFn.invoke (RestFn.java:1523)
    clojure.tools.nrepl.middleware.interruptible_eval$evaluate$fn__655.invoke (interruptible_eval.clj:87)
    clojure.lang.AFn.applyToHelper (AFn.java:152)
    clojure.lang.AFn.applyTo (AFn.java:144)
    clojure.core$apply.invokeStatic (core.clj:646)
    clojure.core$with_bindings_STAR_.invokeStatic (core.clj:1881)
    clojure.core$with_bindings_STAR_.doInvoke (core.clj:1881)
    clojure.lang.RestFn.invoke (RestFn.java:425)
    clojure.tools.nrepl.middleware.interruptible_eval$evaluate.invokeStatic (interruptible_eval.clj:85)
    clojure.tools.nrepl.middleware.interruptible_eval$evaluate.invoke (interruptible_eval.clj:55)
    clojure.tools.nrepl.middleware.interruptible_eval$interruptible_eval$fn__700$fn__703.invoke (interruptible_eval.clj:222)
    clojure.tools.nrepl.middleware.interruptible_eval$run_next$fn__695.invoke (interruptible_eval.clj:190)
    clojure.lang.AFn.run (AFn.java:22)
    java.util.concurrent.ThreadPoolExecutor.runWorker (ThreadPoolExecutor.java:1142)
    java.util.concurrent.ThreadPoolExecutor$Worker.run (ThreadPoolExecutor.java:617)
    java.lang.Thread.run (Thread.java:745)
```

For both of these, note that we're getting hit in an annoyingly delayed way. The initial anonymous function definition compiles just fine, but _calling_ it explodes messily.

Again, not sure why this happens, but it's something I've stubbed my toes on a few times, mostly as a result of slicing [`hiccup`](https://github.com/weavejester/hiccup#hiccup) trees.

#### Lack of general membership check

There _is_ a function called `contains?`, but it _only_ does what you want when you're dealing with a `set`.

```
langnostic.core> (contains? #{1 2 3} 2)
true
langnostic.core> (contains? #{1 2 3} 4)
false
langnostic.core> (contains? #{1 2 3} nil)
false
langnostic.core> (contains? #{1 2 3 nil} nil)
true
```

And it's dangerously misleading on vectors.

```
langnostic.core> (contains? [1 2 3] 2)
true
langnostic.core> (contains? [1 2 3] 4)
false
langnostic.core> (contains? [:a :b :c] :c)
false
langnostic.core> (contains? [:a :b :c] 2)
true
```

See, what's happening is that `contains?` checks whether the given value is present among the keys of the given collection.

```
langnostic.core> (contains? {:a 1 :b 2} 2)
false
langnostic.core> (contains? {:a 1 :b 2} :b)
true
langnostic.core>
```

For vectors, that means this is basically a check to see whether the length of the vector is greater than the index you give it. There is no general `member?` function, unless you count `(contains? (set my-vec) elem)`. Which isn't horrific, but not what I was expecting to have to do.

I understand that [`contains?` is still the most `F`-ed AQ](http://stackoverflow.com/a/3249401/190887), so I'm not _entirely_ sure why it hasn't been fixed yet. And by "fixed", I mean a function in `clojure.core` named `member?` that does what you'd expect for sets and vectors. I'm sure there _is_ a library out there that provides this definition, but dammit, you could do better.

#### No Pandoc

I used to use the fantastic [`pandoc`](https://github.com/jgm/pandoc) library to slice my blogs pretty thoroughly in Haskell. Now that I'm back in Clojure-land, there is no such thing lying around. There's a [Markdown-specific library](https://github.com/yogthos/markdown-clj) that outputs HTML, and doesn't seem to expose any intermediate parse trees for my convenience, so adding links to my blogs' headers is now [slightly uglier](https://github.com/Inaimathi/langnostic/blob/master/src/langnostic/files.clj#L15-L25) than it used to be. The library also has the bizarre behavior of escaping `--` into a long-dash by default, even in image/link URLs, which means links broke all over the place before I figured out what was going on. It's a solvable problem (just provide [replacement `transformers`](https://github.com/Inaimathi/langnostic/blob/master/src/langnostic/files.clj#L27-L36)), but it would be much nicer if it were a _non_-problem.

#### GNU Incompatible

I left this one 'till last, because I'm aware that I'm in the minority on it, but still need to say it. Clojure as a whole is licensed under the [EPL](https://www.eclipse.org/legal/epl-v10.html), which is not [compatible with licensing your own programs under the GPL or AGPL](http://www.eclipse.org/legal/eplfaq.php#USEINANOTHER). Which sucks mightily for me in particular, because I've got a couple projects on the go at the moment I was hoping to license that way. In concrete terms, this means I'm ditching Clojure for those projects as soon as I get out of the prototyping phase. That's a shame, because, like I said, _language_ wise, it's one of my favorites[^and-would-probably]. Oh well, I guess.

[^and-would-probably]: And would probably be _the_ favorite if it weren't for the licensing point and the JVM point.

### Good

#### `lein` is still fantastic

[Leiningen](http://leiningen.org/) continues to satisfy all of my setup and dependency resolution needs. Some people have more specific requirements, and have been using [`boot`](https://github.com/boot-clj/boot#boot--) instead, but I haven't yet had a use case that called for it. It may happen, and I'll let you know if it does, but it hasn't yet.

#### Very Succinct

I'm honestly surprised how succinct the language is. It might be the case that my idiosyncratic thought processes are just suited to a Lispy syntax, but I was still surprised when I finished the re-write of [my blog](https://github.com/Inaimathi/langnostic#langnostic). `wc -l` says the complete codebase weighs in at 305 lines. That's a reduction of about 80 from the Haskell[^admittedly-hacked] implementation. I'm not going to draw deep conclusions about this, both because it's a really small-scale test, _and_ because line-count isn't exactly the best measure of code quality, but it is something to note.

[^admittedly-hacked]: Admittedly hacked-together.

The only thing I'm willing to conclude is that [`hiccup`](https://github.com/weavejester/hiccup#hiccup) is a damn sight better than [Blaze](https://hackage.haskell.org/package/blaze-html) at expressing HTML markup. Which honestly seems to beg the question of why everyone in Haskell-land is happier bizarrely composing functions together, rather than defining a syntax-tree datatype and being done with it.

#### Libraries for Everything

This is the good side of being on the JVM. And it's come in handy a few times. There are libraries for everything. Not just the fun/sexy-to-write things like web servers or application frameworks, but also things that are large and hard to write properly, but that you might need anyway. Things like [cross-platform GUI libraries](https://github.com/daveray/seesaw), [filesystem watchers](https://github.com/derekchiang/Clojure-Watch), and [task scheduling](http://docs.caudate.me/hara/hara-io-scheduler.html). I'm not _entirely_ sure it's worth the JVM implementation baggage, but it has saved me personally a bit of time.

#### No FFI Headaches

The main problem I have getting back into Common Lisp these days is that a lot of the best libraries depend on FFI bindings. Specifically, [`woo`](https://github.com/fukamachi/woo) calls out to a native event loop and [`hunchentoot`](http://weitz.de/hunchentoot/) depends on `openssl` bindings to implement its `HTTPS` system. The problem I've run into is that [`CFFI`](https://common-lisp.net/project/cffi/) does not play well with [`nix`](https://nixos.org/nix/). At all. To the point that _getting_ `woo` or `hunchentoot` with TLS up and running is an exercise in mind-numbing frustration. I've yet to run into this problem with a Clojure library. Say what you will about the "Everything Must Be Native" cultural bias in the Java community, it certainly does mean that deployments get a bit less headache-inducing.

## Blog

Now that I've mentioned it, actually, yes. I've re-written my blog once again. This time, it's running in Clojure, and no longer in Haskell, as per the opening blurb. I think I'll write about it eventually, but not right at the moment. Doing these blog-posts more frequently means talking about fewer things in each one, unfortunately. And I've got a lot on my mind that I want to get out before I get bogged down in the minutia of my latest toy modifications. I'm not sure what I'll talk about next time; I'm torn between server-less decentralized systems, the attempted resurrection of [Toronto Code Retreat](https://github.com/CodeRetreatTO), stuff we've been discussing at the [Comp Sci Reading Group](http://compscicabal.github.io/), and the [stuff I'm working on professionally](https://github.com/ontodev/howl) these days. If you've got a preference, ping me somehow, but I think all of them are going to be discussed eventually.
