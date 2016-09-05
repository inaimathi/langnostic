On my bus ride back from work[^as-a-lisp-programmer], I've been thinking about how to make the response to [this](http://www.codinghorror.com/blog/2012/05/please-dont-learn-to-code.html) precise and thorough.

[^as-a-lisp-programmer]: As a Lisp programmer at a small Toronto company, just in case my bias wasn't obvious enough already.

The stuff I was going to come out with included a reference to this [Sussman talk](http://video.google.com/videoplay?docid=-2726904509434151616) from [Danfest](http://www.cs.indiana.edu/dfried_celebration.html), which concludes by highlighting the title of a Minsky paper; "[Why Programming is a Good Medium for Expressing Poorly-Understood and Sloppily-Formulated Ideas](http://web.media.mit.edu/~minsky/papers/Why%20programming%20is--.html)". I won't link you to the actual point in the talk wherein this happens, because it's only about thirty minutes long and well worth your time in its entirety. The gist is that by forcing yourself to describe a process or concept well enough that a very stupid machine (a computer) can understand it, you can iron out the unnoticed gaps and assumptions in your own knowledge. This is particularly relevant when dealing with *other humans*, who by and large aren't stupid, but merely missing some piece of information that you've begun to take for granted, or perhaps only ever learned by rote.

That would have segued naturally into the point that learning to think precisely can help humans communicate more effectively with each other, and not just with machines. The rebuttal would have continued with a short, faux-op-ed from an early scribe claiming that literacy is completely overrated and unnecessary in most peoples' every-day lives (claiming in all seriousness that all hunters really need to worry about is not breaking their spear arms, and making sure that their legs are strong to carry enough meat back, and that the farmers should just focus on their plowshares). He'd conclude by asking you to refuse to learn how to read and write, because frankly, he's sick enough of his current colleagues' grammatical errors without you adding your own cock-ups to the mix.

*Then* I was going to point out this video from the [MIT 600 Computer Science course](http://www.youtube.com/watch?v=2Op3QLzMgSY&feature=BFa&list=PLE18841CABEA24090)[^better-known-as], wherein Harold Abelson explains to the fresh class that "Computer Science" is not about computers in the same sense that Physics is not about particle accelerators, or that Biology is not about microscopes and petri dishes. What Computer Science is about, he claims, is formalizing certain types of formerly intuitive knowledge. In this case, imperative knowledge. How to do things. For a finale, I'd point out that, while Jeff was talking about coding where I'm making an argument for something more generally useful, humans might find it easier getting to the latter after going through the former. Seibel's [Coders at Work](http://www.codersatwork.com/)[^authors-talk-here] shows that one of the two[^the-other-is-that] peculiar things about people who become good programmers is that they had early exposure to computers and coding, at a time when that wasn't really the typical experience.

[^better-known-as]: Better known as [SICP](http://mitpress.mit.edu/sicp/).
[^authors-talk-here]: Author's talk [here](http://www.youtube.com/watch?v=pQy22qPH7i4)
[^the-other-is-that]: The other is that most of them use Emacs.

I was *going* to write that, but on second reading, [his latest piece](http://www.codinghorror.com/blog/2012/05/please-dont-learn-to-code.html) seems to have the paradoxical message of

1. You shouldn't bother learning things that won't directly and obviously make you better at the tasks in your description[^dont-bother-learning-things-for-no-reason]
2. You shouldn't learn to program just for the money[^dont-learn-programming-for-the-money]

[^dont-bother-learning-things-for-no-reason]: > To those who argue programming is an essential skill we should be teaching our children, right up there with reading, writing, and arithmetic: **can you explain to me how Michael Bloomberg would be better at his day to day job of leading the largest city in the USA if he woke up one morning as a crack Java coder?** It is obvious to me how being a skilled reader, a skilled writer, and at least high school level math are fundamental to performing the job of a politician. Or at any job, for that matter. But understanding variables and functions, pointers and recursion? I can't see it.
> --Jeff Atwood (emphasis his).

    I think my response is obvious from what I've said already, but just in case. "Programming" is not "variables and functions, pointers and recursion". It is a way to describe a process or concept so well that things which don't even share your biology can understand it. This is useful when dealing with things that *do* share your biology, but not *quite* all of your knowledge, and it is useful when explaining fundamental concepts to the uninitiated.

[^dont-learn-programming-for-the-money]: > Please don't advocate learning to code just for the sake of learning how to code. Or worse, because of the fat paychecks.
> --Jeff Atwood

    I'm not too familiar with the "everyone should learn to code" movement, but I doubt its core message is that everyone should become a professional programmer. Hell, I know how impossible *that* proposition is, and [I've talked about it](/posts/freedom) before. The thing is, unlike plumbing _(Which deals with a very specific, physical system, isn't particularly fun, isn't particularly social, and only ever needs to be practised when something goes wrong)_, programming _(Which deals with a wide variety of at least partially imaginary systems, is fun, is mostly social, and can be applied in situations that don't involve water spraying out from under your sink)_ does teach those who study it a lot about communicating precisely, thinking clearly, and solving problems in general. So it at least seems like a believable candidate for "the next literacy".

So, yes, please, do learn to program. Don't avoid it just because you can grow turnips, or answer phones, or sit in meetings fine without it.

Go beyond

```basic
10 PRINT "HELLO"
20 GOTO 10
```

Begin to understand how to think precisely, and communicate clearly with entities who don't have a lot of knowledge in common with you. Don't worry that you'll [never actually use this](http://xkcd.com/1050/) at your day job, and certainly [don't expect to be a highly paid programmer in *just 7 days*](http://norvig.com/21-days.html). But do learn, because it will be interesting, and fun, and useful in places you might not expect.
