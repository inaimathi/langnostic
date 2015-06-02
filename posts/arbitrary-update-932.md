It's been a while. Sadly/happily, I've been busy working on the [`house` write-up](https://github.com/Inaimathi/500lines/blob/master/async-web-server/writeup.md) for [AOSA 4](http://aosabook.org/blog/) and thinking pretty hard about a [`cl-notebook`](https://github.com/Inaimathi/cl-notebook)-related submission to [this](http://www.future-programming.org/call.html), so any spare writing time I've had has been getting pointed directly away from this blog. This is going to be a pretty incoherent update, mainly focused at getting various things out of my head rather than making any kind of sense. It's a quick cross-section/pressure-release related to things I've been thinking about lately, not a catalyst for learning or external discussion.

### <a name="clnotebookfactbase"></a>cl-notebook/fact-base

I'm noticing that there's a very common pattern in the [`cl-notebook`](https://github.com/Inaimathi/cl-notebook) use-case for [`fact-base`](https://github.com/Inaimathi/fact-base). Specifically, the deletion of a record followed by the insertion of a very similar new record.

```lisp
  (let ((cont-fact (first (lookup book :a cell-id :b :contents)))
        (val-fact (first (lookup book :a cell-id :b :result)))
        (cell-lang (caddar (lookup book :a cell-id :b :cell-language)))
        (cell-type (caddar (lookup book :a cell-id :b :cell-type))))
    (delete! book cont-fact)
    (insert! book (list cell-id :contents contents))
    (publish! :cl-notebook-updates (update :book (notebook-name book) :cell cell-id :action 'content-changed :contents contents))
  ...
```

What I'm trying to express there is a change in an existing record. It's mildly annoying for two reasons. First, it means that I need to effectively store every change twice in the form of a before-and-after shot (which, granted, I kind of need to keep doing if I want easy reversibility on edits). Second, and more importantly, it means that a history interface for something being backed by a fact base is going to need to be more complex than I'd like. Instead of naively displaying states, I'll need to make sure to intelligently handle the situation where a user moves history to a point between a deletion and new insertion of a particular fact. I'm going to shortly be making such an interface for `cl-notebook`, so this is going to get painfully relevant very soon. This has me seriously considering adding a third token to `fact-base`, `:modify`, specifically to address this.

### <a name="memory"></a>Memory

![](/static/img/thinkin-bout-memory-management.png)

Whatever language you're currently using, managing memory is something that you're ultimately doing. Whether it's manually, or through various schemes that do it on your behalf. The ultimate goal of all of these approaches is twofold:

- make sure that a new allocation doesn't clobber a chunk of memory that's still being used by something
- make sure that you never build up enough memory junk that you can't allocate a new block when you need to

The general approaches seem to be

## <a name="not"></a>Not

For sufficiently short-running programs, a reasonable approach is to just build up junk constantly and let it come out in the wash after the program ends. This is a pretty narrow use case, since you can't clobber memory held by another program, and you can't use more memory than exists on the machine running you, but it's sometimes an option. I'm... not actually aware of a language that takes this approach.

## <a name="manual"></a>Manual

This is the C/C++ approach. You, the programmer, get to declare exactly what pieces are being used and when. Whether that's on a procedure-by-procedure or datastructure-by-datastructure basis, you're ultimately responsible for memory yourself directly. The upside is that there's no garbage collection overhead. The downside is that every procedure/datastructure has to manage memory acceptably, otherwise things start blowing up in various hard-to-diagnose ways.

## <a name="markandsweep-and-variants"></a>Mark-and-sweep and variants

One of the automatic memory management approaches. The general idea here is


-   keep a free memory list, and track of all things allocated by the program
-   every so often (either at a time interval, or every `n` allocations, or maybe just when you try to allocate memory and your free list is empty), traverse the list of all things and free the ones that aren't being used any more


A variant on this known as **generational garbage collection** is keeping several buckets of allocated things, rather than one. You'd partition objects based on how long they've been around so that you don't waste much time traversing long-lived data every time through. This is the variant that I've seen discussed most often, and I kind of get the impression that it's also the one getting the most research time thrown at it, but I'm not entirely sure why. Oh, incidentally, languages like Common Lisp, Java and Python use this one.

## <a name="referencecounting"></a>Reference-counting

Another automatic approach, also known as **deterministic garbage collection**. As far as I understand, Perl does this. The idea is to keep a special eye on language primitives that create or destroy references to objects, and to keep track of how many references to a particular object exist. Every time a relevant primitive is called, modify the reference count for the target, and collect it if that count is zero afterwards. I'm not sure what the pitfalls are in practice, but there seems to be a lot less discussion about this approach than about the previous.

## <a name="circular-memory"></a>Circular Memory

I only vaguely understand this one, and from what I understand, it's fairly limited, but here goes. The situation you'd want to use this or something like it is when a particular allocation is only needed for a short time before being discarded, and can be discarded sequentially. If you're in that situation, what you can do is figure out how much memory you have, then allocate things to it in order and hop back to the beginning when you're done. You need a pointer to the next block you can use (which you update every time you need to allocate a new thing), and a pointer to the first still-relevant block (which you update every time you free something). If the two ever overlap, you know you have a problem. Shapes other than circular are possible. For instance, you could have some sort of self-referential tree structure that accomplishes the same thing for mildly different use cases.

### <a name="decentralization"></a>Decentralization

This isn't a thing related to memory; this is separate.

I've got this pattern of


1.   assuming problem `foo` is solved
1.   going on about my business for a while
1.   taking a close look at `foo` for the purposes of implementing something related to it
1.   suddenly realizing that `foo` is not only unsolved, but can't possibly *be* solved
1.   thinking about it really hard in an obsessive/compulsive sort of way
1.   learning to live with a deep dissatisfaction about some small part of the universe


This has happened for identity, free will, type systems, [authentication](http://langnostic.blogspot.ca/2012/03/strifebarge-update-and-my-secure.html), and most recently decentralization.

More will follow, I'm sure.

I heard about the [peer-to-peer connection mechanisms in development for HTML5](http://stackoverflow.com/questions/1032006/will-html5-allow-web-apps-to-make-peer-to-peer-http-connections), and figured it would be nice to put something nice and small together using it. To that end, I'd absorbed and discussed the idea of [mental poker](http://en.wikipedia.org/wiki/Mental_poker) with a couple of people, and thought I had the hard parts under control. It turns out that this new set of functionality is still going to need central servers to set up the connections though, at which point I dismissed the approach only to notice a cognitive rupture. *Every* "distributed" protocol seems to need a central server to bootstrap. The web needs DNS, torrents need trackers, cell-phones need satellites and/or transmission stations, etc etc.

This fundamentally shifts the problem in my mind. If we're going to be burdened with some central functional cluster *anyway*, it looks like a better problem to solve might be "how do we perform tasks with multiple 'central' clusters" rather than "how do we have as few central structures as possible".

Fuck you if it doesn't make sense. It's late and I'm piping a stream of consciousness here. I was also going to talk about a few other things on my mind; assembler, the pain of implementing HTTP in Windows-based Lisp, and some new revelations about Flow Based Programming in particular, but I'm about to lose coherence entirely, and nobody wants that.

I'll let you know what, if anything, comes of any of these meditations. For the moment, you'll have to tolerate some more radio silence.
