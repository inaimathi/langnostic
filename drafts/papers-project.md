The [Toronto Computer Science Reading Group](https://github.com/CompSciCabal), also known as the [CS Cabal](http://cscabal.com) is getting quasi-serious, and also mildly stir-crazy. We've been toying with the idea of writing a paper recommendation engine, a turing complete dance, and the next book we're reading. I'm not particularly involved in the lambda dance, believe it or not. I think [`dxnn`](https://github.com/dxnn) might eventually post a repo of notes, but we don't have anything that exists off a physical whiteboard yet. As such, I will say no more about this.

## The Next Book

The vote for the next book we're reading is currently in progress. The criteria for candidates was

1. A book whose title is a four-letter acronym
2. That is freely available online
3. That would be interesting to go through in a group context

The astute will notice that there's no Computer Science content requirement in those criteria, which is why I jokingly suggested [TAOW](https://www.gutenberg.org/ebooks/132). The serious candidates, in no particular order, are

- [PLAI](http://cs.brown.edu/courses/cs173/2012/book/book.pdf)
- [PAIP](https://github.com/norvig/paip-lisp)
- [ISLR](https://www-bcf.usc.edu/~gareth/ISL/)
- [PLFA](https://plfa.github.io/)
- [HoTT](https://homotopytypetheory.org/book/)
- [SICM](https://mitpress.mit.edu/books/structure-and-interpretation-classical-mechanics-second-edition)

I think my preference is probably for Paradigms of Artificial Intelligence Programming. PLAI[^an-engineering-focused] might be interesting, in that we've read [PFPL](https://www.cs.cmu.edu/~rwh/pfpl/2nded.pdf)[^a-theory-focused], but not interesting enough for me to push it. The vote should be concluded by the end of the weekend, so I'll be able to tell you what we're doing next week.

[^an-engineering-focused]: An engineering-focused approach to programming languages that focuses on untyped languages and has a chapter on type checking.
[^a-theory-focused]: A proof/theory-focused approach to programming languages that focuses on type systems and has a chapter about untyped programming.

## The Recommendations

Ok, so at some point we realized that we were a group of loosely affiliated people with limited, but focused free time that managed to seriously expand our collective and individual understanding of various computer science material over the course of about eight years. From that position, we might be able to put together a curriculum for other people in similar situations to do the same. The beginnings of that curriculum are [here](https://github.com/CompSciCabal/CS-as-she-is-wrote), but before we got there, we had a fantastic idea/scope-creep moment.

Could we automate that?

In other words, could we write the program that, given a topic and your current level of understanding _of_ that topic, could put together a set of readings and exercises that would get you from where you are to expert status?

In `dxnn`'s words, "How hard could it be?". The beginnings of _that_ project are over [here](https://github.com/CompSciCabal/vellum). Our next action step is to build a classification engine that eats everything in the [Arxiv](https://arxiv.org/) Comp-Sci section, and uses [`Doc2Vec`](https://cs.stanford.edu/~quocle/paragraph_vector.pdf) to figure out how to cluster papers. That doesn't give us the solution on its own; we also need to figure out how to rate the complexity of different papers (or possibly figure out a way of decomposing different subsections of Comp-Sci into required concepts and figure out a complete Walk of them). Oh, and then also figure out a simple way of testing someone for where we sould start them off.

How hard could it be, right?

The only thing I've learned so far is that, for whatever reason, [`gensim` implementation of `Doc2Vec`](https://radimrehurek.com/gensim/models/doc2vec.html) doesn't allow passing a generator as the initial data sequence. This is _unlike_ their [implementation of `Word2vec`](https://radimrehurek.com/gensim/models/word2vec.html). I have no idea yet whether this is some implementation detail that snuck by, or whether it's intrinsic to the Doc2Vec approach. I guess I'd better read the [related papers](https://datascience.stackexchange.com/questions/20076/word2vec-vs-sentence2vec-vs-doc2vec) first.
