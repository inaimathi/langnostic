I've seen this question pop up on various forums with disturbing frequency lately. Enough that I just wrote this so that I can link people to it instead of typing the advice out each time. The stuff I cover here has already been touched on in [a post called Self Titled](http://langnostic.blogspot.ca/2012/02/self-titled.html). Go read that if you want more perspective on my opinion, but it's fairly long so I need to put something shorter and more accessible together.

### A Better Question

is "What do I want to do with my programming skills?"

If your goal is merely employment in commercial IT in the shallow future, then the answer is simple. You should learn C++. Then one of Java, C#, Objective C or PHP depending on what niche you want to work in. Then you should stop learning things. After a little bit of a grind, and as long as you're not a *complete* asshole, or idiot, or both, you'll get promoted to team lead somewhere. At that point you're not writing code, so it doesn't really matter what languages you know or how well.

That's it, you can go.

Pretend the rest of this article doesn't exist.

### If You're Still Reading

your goal is to push the bleeding edge of Comp Sci/IT forward, and you have your work cut out for you.

If you're serious about this goal, then you need to understand something. Being a new programmer and asking "What language should I learn?" is roughly like being an aspiring carpenter and asking "Should I learn to use hammers or screwdrivers?". You won't get good answers because it's the wrong question. Usually, you get an avalanche of people coming in to push their pet language forward ("Definitely just stick to the hammer"), or to push the currently fashionable answers ("No one uses hammers anymore, learn to use a nail gun"), and you shouldn't listen to *any* of them.

Languages *are* tools, but they're not like *physical* tools. A language is not a bandsaw. It's the [Theory of Relativity](http://en.wikipedia.org/wiki/Theory_of_relativity). A collection of *cognitive* tools and abstractions that help you think about unfamiliar and counterintuitive concepts precisely enough to explain them to very stupid machines, and perhaps to very inattentive humans. I say this because the askers of the big question often say that someone has told them something like "Blub is old; don't bother with it". That's not a valid argument against a language. Theories don't rust. Occasionally they're disproven, or revised, but merely being old isn't enough to discredit them<a name="note-Thu-Mar-14-155452EDT-2013"></a>[|1|](#foot-Thu-Mar-14-155452EDT-2013).

If you want to be a brilliant programmer with a chance of impacting the deep future, sure, you need to understand how the underlying physical machine actually works, and C/C++ helps with that, but it's nowhere near sufficient. You need to *really* understand OO, which means spending a while hacking on [Smalltalk](http://www.pharo-project.org/home) or [Ruby](http://www.ruby-lang.org/en/) or [Simula](http://staff.um.edu.mt/jskl1/talk.html). You need to understand the different *kinds* of OO on offer, which means dealing with class-based systems (like [C++](http://gcc.gnu.org/onlinedocs/libstdc++/)/[Java](http://www.java.com/en/) et al), prototype systems ([JavaScript](http://nodejs.org/) or [Self](http://selflanguage.org/)) and generic-based systems ([Common Lisp](http://www.sbcl.org/platform-table.html)) at minimum.

You need to go beyond OO; understand functional and declarative programming, in both strongly/statically and dynamically typed flavors. If you just want a list of languages, that means a whirlwind tour of [Haskell](http://www.haskell.org/platform/)/[Scala](http://www.scala-lang.org/)/an [ML](http://www.smlnj.org/), a [Lisp](http://racket-lang.org/), [Prolog](http://www.swi-prolog.org/) or [Erlang](http://www.erlang.org/), and more than I can reasonably list here besides. It's probably a good bet to just look at the [Programming Paradigms](http://en.wikipedia.org/wiki/Programming_paradigm) page on Wikipedia and read anything linked off the right sidebar, it's all relevant.

You need a thorough understanding of compilers, which you can get by putting a few years into really, truly [understanding Lisp macros](http://www.paulgraham.com/onlisptext.html) and/or reading the [Purple Dragon book](http://www.amazon.com/Compilers-Principles-Techniques-Tools-2nd/dp/0321486811)<a name="note-Thu-Mar-14-155656EDT-2013"></a>[|2|](#foot-Thu-Mar-14-155656EDT-2013) and/or [writing one](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). You'll need to know about data structures, both [traditional](http://www.amazon.com/Data-Structures-Algorithms-Programming-Books/b?ie=UTF8&node=132570011) and [functional](http://www.amazon.ca/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504)<a name="note-Thu-Mar-14-155701EDT-2013"></a>[|3|](#foot-Thu-Mar-14-155701EDT-2013), about [set theory](http://en.wikibooks.org/wiki/Set_Theory), and [graph theory](http://www.graphtheory.com/), and [probability theory](http://cstheory.stackexchange.com/a/5816), and [advanced algebra](http://cstheory.stackexchange.com/a/10929) and probably a hundred other things I missed. Including things that are only incidentally related to programming, like [source control](http://git-scm.com/), [human management/interaction](http://www.amazon.com/gp/product/1430243147/ref=as_li_ss_tl?ie=UTF8&tag=beigee-20&linkCode=as2&camp=1789&creative=390957&creativeASIN=1430243147), hardware maintenance, [writing](http://www.amazon.com/The-Elements-Style-Fourth-Edition/dp/020530902X), [security](http://www.interhack.net/pubs/network-security/), [typing](http://steve-yegge.blogspot.ca/2008/09/programmings-dirtiest-little-secret.html) and the [social impacts](https://www.gnu.org/philosophy/free-sw.html) of the work we do.

Learning to program is not a thing you [pick up in seven days](http://norvig.com/21-days.html), and you could do a lot worse than to start by reading [that article](http://norvig.com/21-days.html). Just make sure to also disabuse yourself of the idea that you do it by picking one language and sticking to that.

## TL DR

So, in case you skipped directly to this line, the short answer is **"all of them, and that's just for starters"**. Good luck; I'll see you on the other side.

* * *
##### Footnotes
1 - <a name="foot-Thu-Mar-14-155452EDT-2013"></a>[|back|](#note-Thu-Mar-14-155452EDT-2013) -"Blub has an inactive community" or "Blub's community is principally composed of assholes" *are* valid arguments against *using* a language. But keep in mind that you can still learn a lot by understanding a language that assholes use, or that very few people decided to use. Also, keep in mind that the metrics related to these arguments are relative and necessarily personal; if you're close friends with five or six people who use [Io](http://iolanguage.org/), then it really doesn't matter much what the rest of the world is doing.

2 - <a name="foot-Thu-Mar-14-155656EDT-2013"></a>[|back|](#note-Thu-Mar-14-155656EDT-2013) - If the price-tag scares you, I should mention that there's a way to get a softcover edition for something like $40, but it doesn't include the same exercise sections or cover and is printed on pretty shitty stock. That's what I ended up doing, and mine's still in one piece even after a few years, but I can't find the link to that deal anymore even though one of the customer images is that edition of the book..

3 - <a name="foot-Thu-Mar-14-155701EDT-2013"></a>[|back|](#note-Thu-Mar-14-155701EDT-2013) - I'm putting the Amazon link there, but the first link in a google search about "Purely Functional Data Structures" seems to be a legitimate, free PDF copy of the same from [CMU](http://search.library.cmu.edu/).
