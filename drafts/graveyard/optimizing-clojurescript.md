So I recently had a kind of weird experience. For me anyway. There's a closed-source project I'm working on that has a Clojurescript component which we recently had to tune for performance. Here's the stuff we ended up doing and/or considering.

## Walking is expensive

Walking `clojure` datastructures is fairly expensive. The lesson being that you should limit that if you can.

- walking clojure datastructures is expensive, so limit it where you can
- in particular, there was a datastructure conversion function that had to deal with an incoming tree slightly differently in `clojure` than `clojurescript`.
- used `clojure.walk/keywordize-keys` in a separate pass. Using the `keywordize-keys` options on [`js->clj`](TODO) and [`cheshire.core/decode`](TODO)

## Advanced compilation is moderately-effective, relatively low-hanging fruit

- using `:optimizations :advanced` instead of `:optimizations :whitespace` is an obvious piece of low-hanging fruit.
- The resulting JS code is less readable, but if you provide `:output-dir`, `:output-to` and `:source-map` options and then serve the resuling source+maps, you get translations back. This made the compiler occasionally shit itself.

## Multimethods are expensive

- Multimethods, or generic functions are [fairly complicated to translate](https://github.com/clojure/clojurescript/blob/6ab76973ab31033b2307f88a2ebc5ad9ebd5cf3e/src/main/clojure/cljs/core.cljc#L2741-L2786) to JS.
- In our case, because the particular dispatch function was central and contained enough, it made sense to re-write the dispatching code as a `case` statement. This removed a bunch of intermediate function calls in the output code
