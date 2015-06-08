Fuck its been a while.

I think.

I'm not actually sure because I've been under enough stress on enough different fronts lately that I'm not sure I can reckon time accurately, other than to sequence it. This isn't going to contain any kind of big insight; I'm thinking about various [macro-expansion](https://github.com/Inaimathi/cl-notebook) and [edge-detection](https://github.com/Inaimathi/edgy)-related things, but I'm saving the interesting details for their own articles once the relevant work is *"done"* rather than merely *"in progress"*. The following is just a quick brain-dump of stuff that's been on my mind lately.

## <a name="dead-bitmaps-rise-of-the-ellipses" href="#dead-bitmaps-rise-of-the-ellipses"></a>Dead Bitmaps 2: Rise of the Ellipses

I'm starting to think about ellipse finding. And, at the urging of some members of the <a name="note-Tue-Nov-25-111814EST-2014"></a>[|1|](https://groups.google.com/forum/#!forum/toronto-haskell">Toronto Haskell User Group</a>, shape finding in general. The pre-beta cut of the code which will find an ellipse in a region is [up at github](https://github.com/Inaimathi/edgy/blob/master/ShapeDetection.hs). Essentially, we take a region, find its center, plot an ellipse based on that[](#foot-Tue-Nov-25-111814EST-2014), and check perimeter coordinates at ~10 degree intervals. If we get three consecutive misses, or 10 total misses<a name="note-Tue-Nov-25-111818EST-2014)[|2|](#foot-Tue-Nov-25-111818EST-2014), we can discount that region as an ellipse and go do our regular line-detection routine on it.

It's surprising to me that this seems to be standard practice in computer vision, though on reflection it *really* shouldn't be. Candidate selection is usually some simple directional scoring procedure, but once you have candidate points together, the most efficient way of processing them seems to be:


1.   Have a theory of approximately what you're going to see
1.   Verify the presence or absence of the thing you're expecting
1.   Repeat with revised theories until satisfied


It feels like there should be a better way. Like it should be possible to sieve meaning directly out of a color map, without having an expectation of what you'll see there. But if pressed, I confess I couldn't tell you how.

## <a name="garbage-collection" href="#garbage-collection"></a>Garbage Collection

Having studied a few tracers<a name="note-Tue-Nov-25-111822EST-2014"></a>[|3|](#foot-Tue-Nov-25-111822EST-2014), it's at once obvious why they emerged in Lisp systems, why Lisp had the reputation of being slow to begin with, and why the "everything is a list" myth continues despite its factual incorrectness. It turns out that if you commit to a one-size-fits-all data structure<a name="note-Tue-Nov-25-111842EST-2014"></a>[|4|](#foot-Tue-Nov-25-111842EST-2014), decide that you don't care at all about data locality<a name="note-Tue-Nov-25-111848EST-2014"></a>[|5|](#foot-Tue-Nov-25-111848EST-2014), and don't optimize anything, you can write a working tracer in just over 300 lines of C. I can't see a generational or copying approach pushing this past about 500 or 600 lines. You wouldn't want to try doing any kind of high-performance computing this way, but if your primary concern is security<a name="note-Tue-Nov-25-111852EST-2014"></a>[|6|](#foot-Tue-Nov-25-111852EST-2014) this starts looking like a pretty good idea.

The problem is, contiguous memory vectors are *really good* for performance, and if you go far enough with this mad scheme, there are lots of things at the lowest levels that end up taking ridiculous amounts of memory which you could theoretically save by hand-tuning it. Theoretically. In practice, I tend to think that even small systems by the day's standards have enough moving parts to give an un-aided human plenty of trouble.

The point being: the naive way of building tracers plays exactly to Lisp's conceptual tropes, and gives you "slow" code. "Duh", when you think about it.

## <a name="composability-and-cost-models" href="#composability-and-cost-models"></a>Composability and Cost Models

Conflicts I've observed between the Haskell and ML communities are about


- [exceptions](http://existentialtype.wordpress.com/2012/12/03/exceptions-are-shared-secrets/)
- [the cost](http://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/) of [laziness](http://augustss.blogspot.ca/2011/05/more-points-for-lazy-evaluation-in.html)


I haven't informed myself about the first well enough to comment yet, though I am trying.

The second one comes down to the trade-off between having <a name="note-Tue-Nov-25-111900EST-2014"></a>[|7|](http://augustss.blogspot.ca/2011/05/more-points-for-lazy-evaluation-in.html">very high levels of composability at the micro level</a> if you're lazy, or having a [simple cost model](http://www.cs.cmu.edu/~rwh/papers/secp/secp.pdf) for pieces of your program if you're not. There seem to be two real solutions and two non-solutions to this problem. The non-solutions are picking either strict or lazy and pretending that's good enough. As I understand it, Rob Harper has picked one of these. Specifically, he's [taken the simple cost model](http://augustss.blogspot.ca/2011/05/more-points-for-lazy-evaluation-in.html) (see the first comment) and made peace with the fact that he gets to write some amount of manual glue code. Personally, if I were forced to pick between the non-solutions I'd pick the other one. I don't give a rat's ass about performance except in very specific situations[](#foot-Tue-Nov-25-111900EST-2014), so getting very cheap composition sounds like a good deal to me<a name="note-Tue-Nov-25-111903EST-2014)[|8|](#foot-Tue-Nov-25-111903EST-2014).

However, I'd still be very loudly wondering why no one was working on either *real* solution. These are respectively


1.   abstracting away the glue code with better compositional primitives for strict languages
1.   developing a better cost model for lazy evaluation


As far as I know, the closest anyone's ever gotten to the first is Common Lisp's [`loop`](http://www.gigamonkeys.com/book/loop-for-black-belts.html) macro. Which, when you think about it, is like a roll of duct tape where you'd really like lego-esque pieces. Also as far as I know, no other language even bothers going as far as `loop`, which tells me this might be widely considered a non-problem. The second is [causing](http://galois.com/wp-content/uploads/2014/08/pub_JL_NaturalSemanticsForLazyEvaluation.pdf) some [activity](http://www.vex.net/~trebla/haskell/lazy.xhtml). I'll admit I've read neither paper yet, but we're set to discuss it at the Toronto Haskell Users' group next month with the author of the first link, so I'll hopefully get it under my belt by then. I chatted him up about it a bit and his perspective is, spoiler warning:

>   It's not impossible to have a good cost model for lazy computation, it's just harder. I used to think it would just be different, and not harder, but I've come to realize it really is more difficult to reason about. Not impossible, no.  
>   
> -Albert Y. C. Lai, in converation  

So I'm going to read through the linked papers, and see if I can understand the situation for myself. As always, I'll let you know how it goes.

## <a name="hal-abelson-chats-with-the-compsci-cabal" href="#hal-abelson-chats-with-the-compsci-cabal"></a>Hal Abelson chats with the CompSci Cabal

This was fucking awesome. <a name="note-Tue-Nov-25-111906EST-2014"></a>[|9|](https://github.com/CompSciCabal">We</a>'re winding down reading the final chapters of [SICP](http://mitpress.mit.edu/sicp/full-text/book/book.html), also known as "Abelson and Sussman", and [Dann](https://github.com/dxnn) managed to get Hal Abelson to talk to us about what went into the book, what he thinks about CompSci teaching evolution over the past few decades, how building language interpreters helps you think about problems, and some of the things that surprised him about the industry. It was interesting enough that I don't think I could do it justice in a mere writeup, so I won't<a href="#foot-Tue-Nov-25-111906EST-2014). Just wanted to mention it as an excellent highlight.

* * *
##### Footnotes

1 - <a name="foot-Tue-Nov-25-111814EST-2014"></a>[|back|](#note-Tue-Nov-25-111814EST-2014) - Later cuts will plot multiple ellipses with some tolerance and look for any complete ones, or alternately check a particular radius around each point.

2 - <a name="foot-Tue-Nov-25-111818EST-2014"></a>[|back|](#note-Tue-Nov-25-111818EST-2014) - Actual thresholds subject to change.

3 - <a name="foot-Tue-Nov-25-111822EST-2014"></a>[|back|](#note-Tue-Nov-25-111822EST-2014) - And implemented most of one that I'll never be able to show you.

4 - <a name="foot-Tue-Nov-25-111842EST-2014"></a>[|back|](#note-Tue-Nov-25-111842EST-2014) - The Cons cell.

5 - <a name="foot-Tue-Nov-25-111848EST-2014"></a>[|back|](#note-Tue-Nov-25-111848EST-2014) - By using exclusively Cons cells rather than occasionally resorting to contiguous memory vectors or naked primitive data.

6 - <a name="foot-Tue-Nov-25-111852EST-2014"></a>[|back|](#note-Tue-Nov-25-111852EST-2014) - As in, preventing buffer overflows and segfaults at all costs.

7 - <a name="foot-Tue-Nov-25-111900EST-2014"></a>[|back|](#note-Tue-Nov-25-111900EST-2014) - And I'd probably be writing *those* in C rather than either of the higher level languages up for discussion.

8 - <a name="foot-Tue-Nov-25-111903EST-2014"></a>[|back|](#note-Tue-Nov-25-111903EST-2014) - To be fair, I also know quite a few languages, and that club would be missing a non-strict member without Haskell. So for me personally, making that choice expands the convex hull of my perspective, and so is virtuous regardless of the specific trade between evaluation schemes.

9 - <a name="foot-Tue-Nov-25-111906EST-2014"></a>[|back|](#note-Tue-Nov-25-111906EST-2014) - Though this does unfortunately mean that you'll never see it; we weren't in an "On Air" hangout, so none of this was recorded as far as I know.
