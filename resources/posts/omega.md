I've been thinking about languages a lot lately. Which is kind of a joke, given the title of my blog, but I actually mean "I've been thinking about them more than usual". This thought has been specifically dominated by thoughts of the Blub hierarchy as proposed by Paul Graham.

I'm not sure what's on top.

PG claims "[Lisp](http://common-lisp.net/)", I've seen many that shout "[Ruby](http://www.ruby-lang.org/en/)", I've met many that claim "[Haskell](https://www.haskell.org/)" or "[ML](http://sml-family.org/)". In fact, if you participate in programming discussion for any length of time, there's a pretty good chance that you'll meet someone for every language claiming it's Omega. It's ridiculous, of course. All most of them are really claiming is "This is the most powerful language I know how to work with", which is not the same thing as "the most powerful language". It's easy to see that trying to compare in any supposedly objective sense would cause giant hatestorms and various infighting. So people are perhaps justified in making statements like

"You don't compare a hammer with a screwdriver, but you use the one that fits your task & way of thinking/education/needed level of abstraction the most. Also, since the one doing the comparison is biased by the fact that he knows at least one of the two, or at least prefers one of the two, it is hard to find a truly objective criteria for comparing them (exceptions exist)."
--[Rook](http://programmers.stackexchange.com/users/2439/rook), [pogrammers.SE](http://programmers.stackexchange.com/)

when discussing language comparison. That was an answer from [a question](http://programmers.stackexchange.com/questions/21891/are-language-comparisons-meaningful/21894#21894) about whether language comparison was useful. As of this writing, it has been closed and re-opened twice, and the original asker has accepted (then unaccepted, then accepted again) a joke answer. This is perhaps more telling of the culture of programmers.SE than of the question, but it doesn't seem like an uncommon response. People duck comparisons precisely because languages are half tools and half religions, and no one wants a crusade declared. But, well, you need to compare.

> "A language is a tool. That said, I've seen really, really crappy tools before. No one wants to work with a hammer whose head is liable to fly off and hit another carpenter in the stomach. Likewise, if you noticed your fellow worker's hammer was in that shape, you'd probably steer clear of them when they were using it. It's also important to really understand which tool it is. You can't use a screwdriver and hammer interchangeably (though some try desperately). Hell you can't even use all hammers interchangeably; you need a sledge for some things, a mallet for others and a tack for yet others. If you use the inappropriate tool, then at best, you'll do a poorer job, at worst you'll injure yourself or a co-worker."
> --[me](http://stackoverflow.com/users/190887/inaimathi), programmers.SE

Graham goes further, stating that not only can you compare languages in terms of power, but proposes that there is therefore such a thing as an empirically best language. As a note, I agree with him[^from-the-future], but "which religion is best?" is a question you just don't discuss in polite society, so I haven't pushed the idea on any forum I frequent. It makes sense though. No one would disagree that Assembly < Cobol < Python on the power scale (I'm defining "power" as a non-specific mix of expressiveness, terseness, maintainability, readability and flexibility). And even admitting that simple truth exposes you to the idea that there's a ladder, or tree, or at least concentric circles of languages with one (or a relatively small group) taking the prime position.

[^from-the-future]: Hello from 2016. I've gotta say, this is probably the hardest of my old articles to edit so far. I've got to try really *really* hard not to be revisionist about stuff, and I'm afraid I may already have failed  on some level. I'll try to content myself with this note. Anyway, I no longer agree with him. At minimum, the real structure of the language world is a lattice, with a few languges sharing the "top" position, but these days I'm more inclined to think about it as a continuum. You can sort of pick a region relevant for you and pull out a local maximum but finding an overall maximum by some metric might very well be useless or impossible. The landscape also isn't all in the real world; a particular human's existing mental architecture may be as or more imortant in terms of determining an optimum.

Omega.

Graham puts Lisp there, but he's making the same claim that any Ruby ardent or avid Haskeller are expressing; "Of all the languages I know, this one is the most powerful". The thing is, I haven't heard many convincing arguments to the contrary. The best argument aimed at Lisp these days is that it's slow, and even then, slow in what sense? It can certainly do the job of server software, or even local desktop/console software on today's powerful systems. Remember, Lisp was called slow back when 1Gz was the sort of processing power you paid many thousands of dollars for. I have more than that right now in my $300 dollar netbook. We're fast approaching an age where a phone you get for free with a subscription is more powerful[^from-the-future-again]. "Slow" just isn't a good enough strike against a language to discount it anymore. Other than that, people complain about the parentheses, which is an empty complaint at best, and typically a trolling attempt. The only good argument against Lisp as Omega comes from an unlikely source.

[^from-the-future-again]: Hello from 2016 again. Yup; we got there. I've now got a phone with a 1.7Gz quad-core processor, and probably a better video module than the netbook I just mentioned. It was completely free with a no-contract data plan. If I felt like spending $30, it would have a larger "disk" too. Seems we're topping out there though; newer machines tend to have more cores/processors rather than faster individual cores/processors. In any case, I'm reasonably sure I could run [SBCL](http://www.sbcl.org/) on this thing. I'm **very** sure I could get a [Clojure app](http://clojure-android.info/) to work on it.

"I don't think it's necessarily Omega. The Haskellers and MLers say 'Well, from where we sit, Common Lisp looks like Blub. You just don't understand the brilliance of strong inferred typing'. And they may be right. Of course, Common Lispers look at Haskell and say 'Well, Haskell's really Blub, because you guys don't have macros'. It may be the case that there is no Omega, or that Common Lisp and Haskell are on different branches of the lattice, and someone's gonna find a way to unify them and a few other good ideas and make Omega."
--Peter Seibel, [Practical Common Lisp Talk at Google](http://www.youtube.com/watch?v=VeAdryYZ7ak)

It's an interesting fact that practitioners of either language can point to lack of features in the other. That has some pretty obvious corollaries as well.

1. There may be such a thing as the most powerful language right now, but it may involve trade-offs (I don't know what it is, but one exists. I'll call it "Alpha" so as not to offend anyone).
2. There is such a thing as the language that will be the best for the next 10 to 100 years (This one may or may not exist in some form today; it might be unified from several current languages as Seibel alludes. I'll use his name and call it "Omega").
3. There is such a thing as the most powerful language that could exist on current machine architectures (This one almost certainly doesn't exist yet, and may never be embodied in an implementation. It's just the limit, in the calculus sense, of what we can hope to achieve with a language along the axes of expressiveness, terseness, maintainability, readability and flexibility. This one I'll call 0).

I'm not sure what Alpha is. I'm not sure anyone knows, because as I've said, people tend to bind that variable to whichever is the most powerful language they currently know. 0 is far away, and I won't even try talking about it today, because I don't have anywhere near enough information to make a decent guess at what it'll look like[^from-the-future-a-third-time]. So what does Omega look like? Well, Graham effectively says it's Arc (or what Arc will evolve into). Others variously substitute their own languages. There's a sizable community which thinks it's Haskell. Some ardents think it's Lisp. A few would like you to believe it's Java, despite the recent turbulence between Oracle and Google. And there are a couple of personalities in the industry who are vigorously pushing either Ruby or C#. Yegge echoes Seibel pretty closely

[^from-the-future-a-third-time]: Hello from 2016 yet again. It's a pretty naive statement to assume that this will definitely happen. It might be the case that this language I'm proposing is a thing we can talk about casually, but can't define formally. The formal maximum is the [Untyped Lambda Calculus](https://existentialtype.wordpress.com/2012/08/09/churchs-law/), and that might be as good as we can do in general.

> "[T]he Wizard will typically write in one of the super-succinct, "folding languages" they've developed on campus, usually a Lisp or Haskell derivative."
> --Steve Yegge, [Wizard School](http://steve-yegge.blogspot.com/2006/07/wizard-school.html)

It's a line from one of his humorous, fictional pieces wherein he describes a Hogwart's-like school that churns out wonder-kid programmers, but it still seems like a vote for the Haskell/Common Lisp unification theory. It might happen. If it does, it'll be a race between the Haskellers and Lispers to out-evolve one another. In order to converge, Haskell needs to strap on prefix notation and macros, make IO easy (rather than possible), and blur the line between run-time, read-time and compile-time. Lisp needs declarative matching definitions, lazyness, currying (possibly eliminating the separate function namespace), strong types and a few small syntactic constructs (function composition and list destructuring leap to mind first). Lisp has a longer list to run through, but keep in mind that because it has macros, almost all of them can theoretically be added by you as you need them, rather than by CL compiler writers as they decide it's worth it [^why-cant-i-hold-all-this-future].

[^why-cant-i-hold-all-this-future]: Hey; 2016 here once again. You could make a pretty strong argument that with its [gradual type system](http://typedclojure.org/), Clojure is about as close as Lisp can come to Haskell. I'm not going to, but you probably could.

It's also worth noting that the last point in Haskell's list is a pretty tricky proposition. How do you blur read/compile/run time when one of your goals is to have a complete type system? Well. REPLs for Haskell exist, so I assume it's possible, but making it part of the language core doesn't seem to be a priority at the moment (and probably won't be for a while due to the performance hits it imposes, and the perception performance hits still have in the general public of programmers). That's not the only hard bit either language would have though. How do you implement full currying and optional/default/keyword/rest arguments? Haskell purports to solve the problem by defaulting to currying, and giving you the option of passing a hash-table (basically) as an argument to implement flexibility. LISP gives you &rest, &body &key and very simple default argument declaration, but "solves" the currying issue by making currying explicit. Neither language's solution satisfies, because sometimes you want flexible arguments (and counter-arguing by saying "well, if you need them, you've factored your application wrong" is missing the point; expressiveness is a measure of power, remember, and having to think about the world in a particular way is a strike against you in that sense), and sometimes you want implicit currying (this is perhaps most obvious when writing in Haskell's point-free style, and if you've never done so, I doubt I could convince you).

As a common lisper, there are a bunch of things I'd like to steal from Haskell, if I could. The pattern-matching definitions are certainly useful in some places, list destructuring would help, and function composition seems useful (though this is, like defmacro, the sort of construct you have to understand first, in order to find places that it would greatly simplify). I'll check later, but I have a sneaking suspicion that someone has already lifted all of the above into a library somewhere on github or savannah. Even if not, list destructuring and function composition seem like they'd be easy enough to implement. The latter as a call to destructuring-bind, the former as a simple fold macro.

From the other side, there's already two projects underway; [Liskell](http://www.liskell.org/) is a competing compiler to GHC that has a prefix notation and outputs the same machine code, and [Lisk](http://chrisdone.com/posts/2010-11-25-lisk-lisp-haskell.html) is a pre-processor for GHC that takes specific prefix notation forms and converts them programatically back to the Haskell source code before invoking the compiler. Lisk's creator talked briefly about macros, but the project is early enough along that nothing much more specific is out there right now (I'm watching [his github repo](https://github.com/chrisdone/lisk) with interest though).

I haven't a clue how to place my own bet. I tried starting this paragraph both with "My bet's on Lisp..." and "My bet's on Haskell...", but each beginning got to a logical dead end within two sentences. It doesn't seem like one can completely absorb the other. But, if Haskell + Lisp makes Omega, we'll see what it looks like shortly (by which I mean ~10 years) because cross-pollination is already happening, and it's not a far jump from there to full-on unification. Or maybe things get bloodier as the preview to Barski's Land of Lisp implies, who knows.

Either way, we'll see soon enough.

> EDIT: rocketnia posted [a particularly thoughtful response](http://arclanguage.org/item?id=13165) to the above post at the Arc Forum. He points out that there may not be an Alpha, Omega and 0, but rather "[L]ocal optima that can't be unified into Omega". I could have sworn I addressed this point (and acknowledged it, but stated that I was more interested the unification idea today), but my only mention of it is "...with one (or a relatively small group) taking the prime position." Apologies. He also explains a lot about how macros might coexist with a strong type system.