No, since you ask, I haven't read anything related to [datomic](http://www.datomic.com/), though [friends](https://github.com/CompSciCabal) keep telling me I *should*. This is just stuff we've been talking about at the [Toronto SICP reading group](https://github.com/CompSciCabal/SMRTYPRTY/tree/master/sicp), and a couple of other places.

The context was mildly different; we had this conversation in relation to the notional bank accounts from [chapter 3 of SICP](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-19.html#%_chap_3) where they introduce mutable state. The discussed version would have been simpler to optimize and manipulate, since it was entirely numeric, but I've *also* got [fact-bases](https://en.wikipedia.org/wiki/Triplestore) on my mind thanks to a [now co-worker](https://github.com/guitarvydas) who's thought pretty deeply about them.

The conversation touched on total-history data-structures, and their effects on performance and convenience. The end result is [this little project](https://github.com/Inaimathi/fact-base) I just put together over the course of half an afternoon.

### <a name="let-me-back-up" href="#let-me-back-up"></a>Let me back up...

Imagine a toy bank account.

The basic one proposed in the book is as simple as a balance, a `get`ter, and a pair of setters named `deposit` and `withdraw`. You could add more detail, like interest rates and appropriate calculation functions, authentication mechanisms, and a designated owners list, but that's all beside the point.

Regardless of how much detail you imagined, you probably think about the principal structure being `the current balance`, hence simple numeric value of some precision. A total-history bank account is not that; it's a starting state (lets say 0), as well as The Total History (hence the name) of all transactions or modifications affecting it. So, instead of something like

```
$50.34
```

you're looking at a thing more like

```lisp
'(...
  (3600901270 :deposit +100)
  (3600913394 :withdraw -20)
  (3600913519 :purchase -29.66))
```

stretching back from the beginning of the accounts' existence to `now`. This means that you have access to the "current value" of the account at any given time in its history. You can go back and check what happened and when, and if you like, you can ask questions like "What would it look like today if I had made an extra deposit here, and an extra withdrawal here?" In order to get the current value, you *have* to project it. That is, you need to go back through history and apply all the recorded events in order to see what falls out the other end. If you really are doing basic numeric modifications, it's pretty easy to parallelize some parts of that projection process, but I'll leave that as a thought experiment for the reader.

Bank accounts aren't the only things you can model this way; specifically, fact-bases can be usefully thought of in this manner.

### <a name="now-then" href="#now-then"></a>Now then...

It turns out that total-history structures give you some interesting properties and challenges.

First, if you want *all* of history, you can never delete anything. You have to put in deletion tokens which remove some element or class of elements from your corpus. *Because* you can never delete anything, a total-history data-structure has the nice property of being append-only. Which means that you can play some neat optimization tricks in serializing it to disk, like say, clustering deltas. This comes in really handy for very large data sets, or ones which are updated very frequently. Since you're only dealing with shipping diffs around, you can easily save yourself bandwidth on keeping copies in sync, or you could easily break your corpus up across different physical drives. Undo/redo also becomes fairly simple to layer on top of a corpus that already uses this approach.

Second, time-stamping becomes pretty critical. If you took a look at [that github link above](https://github.com/Inaimathi/fact-base), and poked around in [here](https://github.com/Inaimathi/fact-base/blob/master/fact-base.lisp), you'll have noticed that I'm using [`local-time`](http://common-lisp.net/project/local-time/) rather than [`get-universal-time`](http://www.lispworks.com/documentation/HyperSpec/Body/f_get_un.htm) because I need much finer granularity than seconds. Going to microseconds doesn't fully solve the problem, of course; if your throughput is high enough, there might still be collisions, and therefore potential data loss/duplication on updates. Putting something scalable together in Erlang would be easier, because [`now/0`](http://www.erlang.org/doc/man/erlang.html#now-0) *guarantees* unique values on subsequent invocations.

Still open decisions are how to go about storing deletion tokens, and when/how aggressively to prune history. A passable answer for the second is "never", so that's what I'm going with for the moment. The first one doesn't seem to have a right answer.

### <a name="storing-deletion-tokens" href="#storing-deletion-tokens"></a>Storing deletion tokens...

The three approaches I can see off the top of my head are storing a deletion index, storing a deletion value, and storing a deletion template. I'm doing the third at the moment, though I'm not convinced it's the right approach. So lets start with that.

**Deletion Template**

Basically the deletion primitive looks like `(delete! (list _ :subject _) fact-base)`, which goes through and deletes any fact whose second element is `:subject`, and also keeps that matching template as an indicator. A deletion entry then looks like

```lisp
(<timestamp> :delete (list _ :subject _))
```

When you go to apply this particular deletion token, you'll need to create the function that checks for a match against it, then run that function across your built up state, removing anything it marks. There are three downsides here. First, this deletion token might pick up things other than the specific facts that it actually deletes in any particular traversal, which means that prospective evaluation gets more complicated. Second, because it's ambiguous, you can't easily reverse it; if you're building up state, you can't just back up over a deletion token, you need to throw away your accumulated state and start from the beginning again to get to the point you were at before applying it. Third, because it involves keeping a piece of match logic in the record, this approach means pulling out `eval` during de-serialization.

On the flip-side, it's easily parallelizable, and it doesn't care one whit about the order of the facts its traversing or the direction of traversal.

**Deletion Index**

This approach just has you keep the offset of the removed fact. A deletion token looks like

```lisp
(<timestamp> :delete 37)
```

or maybe 

```lisp
(<timestamp> :delete (list 13 572 1335))
```

To apply this one, you go back through your built-up state and drop the `nth` elements. Mostly the same downsides as the **Deletion Template**; it gets a bit tricky when you want prospective change projection and it isn't cleanly reversible. It's mildly easier on memory, since we're just slinging integers around, and it doesn't need `eval`, but it suddenly matters which direction you're traversing your corpus in, it matters how your corpus is ordered, and I could see it getting in the way of parallelization later.

**Deletion Value**

A deletion value token looks something like

```lisp
(<timestamp> :delete (<id> :subject "whatever the third value is"))
```

You apply this by going through the accumulated corpus and removing the first fact that matches it. Granted, it's slower in general (because applying it in general involves an arbitrary tree-compare), and it's more wasteful of disk space (because we have to store those arbitrary trees as well as comparing them with facts to remove). But. Depending on how strictly you enforce it, this one is more easily reversible, and it doesn't need `eval` either, since it's just storing a value.

### <a name="wrapping-up" href="#wrapping-up"></a>Wrapping up...

A couple of other thoughts I'd like to leave percolating:


- It might be possible to get the best of both worlds by making sure that a deletion token remembers the particular members it removed *at application time*. That would let both the Template and Index approach reverse easily, though it would complicate the projection process somewhat.
- It might be useful to have a layer of meta-tokens. That is, insertions in `history` that modify other history entries. Off the top of my head, only `:ignore` or `:duplicate` seem like they'd be significantly useful. On the plus side, you now have a meta layer to these histories which gives you more flexibility in using them. On the other hand, you now need to have one of efficient reversals, a two-pass loading procedure, or really *really* shitty performance for heavily meta-tagged histories.


That's that. Not the most flowing narrative I've ever produced, but I think it touched on one or two interesting ideas. And anyhow, I'm still chewing over most of this. I'll let you know how it goes, and if I end up finding a real use for it.
