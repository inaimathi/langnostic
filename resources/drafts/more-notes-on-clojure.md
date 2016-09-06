### Bad:
- that membership check thing is really REALLY annoying
- if you try to create an anonymous function with the `#()` notation that returns a vector, it'll throw an error. Like, `#([:foo])` seriously does not work. Ditto for `#({:a 1})`. It'll _seem_ to work, and compile properly, but then fail when you try to call the function you created this way. Try it; `(#({:a 1}))` explodes spectacularly with the usual, fugly JVM stack trace. In the first case you can do `#(vector :foo)`, but there is no such help in the second case. You'll have to `(fn [_] {:a 1})` instead.
- lack of general recursion is annoying. In particular, even with `loop`, you can't easily walk a tree. If you want that, you instead need to call the `tree-walk` function. This is a result of being on the JVM, but is still annoying as all get-out when working with trees or graphs in any meaningful way.
- clj-markdown has the weird behavior of escaping -- into a long-dash by default, even in image/link URLs (which means linke are going to be broken unless you do the replacement-transformers thing)

Overall, some minor annoying problems, most of which can be traced to this being implemented on the JVM. If it wasn't for that, I'd be using it with no reservations at all.

### Good:

- hiccup is a breath of fresh fucking air compared to the flaming pile that is Blaze.
- lots of things end up being a lot more concise. Partly because of generic datatypes, and partly because of the number of built-in functions
- huge library of useful code. There's lots of stuff just lying around that I would very probably have to write myself in other languages, and wouldn't particularly have fun doing. From [cron-scheduling](TODO) to [filesystem event watching](TODO), to [high-performance HTTP server/client](TODO) to [general parsing utilities](TODO) to [GUI programming](TODO).
- leinignen is still fucking amazing. This piece of infrastructure single-handedly keeps me from bothering to learn any other JVM language (specifically, if it had a comparable utility, I'd have very probably given Scala a whirl by now).
- I have yet to run into a common-lisp-style FFI error (for instance, when you try to install and run `:woo` on a `nix` system. This may be the language, or it may be the ecosystem
