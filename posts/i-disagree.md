Ok, yes, I know the last time I disagreed with Yegge, I wound up [eating my own hat](/posts/yegge-strikes-back), but I hereby suggest that [partitioning the software industry into Libs vs. Cons](https://plus.google.com/110981030061712822816/posts/KaSKeg4vQtz) is a stupid idea, and gains us nothing.

Firstly, because those terms are already loaded with enough political emotional baggage that people are going to have a hard time letting go<a name="note-Sat-Aug-11-153625EDT-2012"></a>[|1|](#foot-Sat-Aug-11-153625EDT-2012), and that's going to lead to<a name="note-Sat-Aug-11-153633EDT-2012"></a>[|2|](#foot-Sat-Aug-11-153633EDT-2012) the same kind of partisan garbage that US politics is well known for.

Secondly, because partitioning any group of people into two explicit, conflicting sides is hands down **the worst** way of easing/preventing/reducing conflict within that group. Ostensibly, that's what he's trying to do with the thought framework; point out that certain things are a matter of preference rather than points of debate, and that we should therefore stop arguing about them. Something tells me the actual effect of this conceptual framework will lead to a [different outcome](http://pbfcomics.com/20/)<a name="note-Sat-Aug-11-153728EDT-2012"></a>[|3|](#foot-Sat-Aug-11-153728EDT-2012). I've read comments calling the opposition to this classification scheme "weird", and I have to wonder why. It's divisive, pretty much by definition. The fact that certain pieces of it are correct doesn't make it worth keeping in its entirety, and in any case...

Thirdly, the underlying properties he presents are, for the most part, not a matter of preference. He sort of presents them that way, but I disagree at that level. Hell, lets do a blow by blow. here are the points he defines as principles of software conservatives.


1.   Software should aim to be bug free before it launches...
2.   Programmers should be protected from errors...
3.   Programmers have difficulty learning new syntax...
4.   Production code must be safety-checked by a compiler...
5.   Data stores must adhere to a well-defined, published schema...
6.   Public interfaces should be rigorously modeled...
7.   Production systems should never have dangerous or risky back-doors...
8.   If there is ANY doubt as to the safety of a component, it cannot be allowed in production ...
9.   Fast is better than slow. Everyone hates slow code. Code should perform well. You should engineer all your code for optimum speed up front, right out of the box...


The software liberals supposedly have the inverse principles. He makes them explicit in [his entry](https://plus.google.com/110981030061712822816/posts/KaSKeg4vQtz), but I won't bother to quote them here. Note that points 1, 4, 5, 6, 7, 8 and 9 have not a fucking thing to do with personal preference. They're things that make sense in some contexts, and not in others. Some programmers really, really like having error prevention in the form of a restricted language (#2), and some really hate learning new syntax (#3), but the rest of these "principles" involve trade-offs that sometimes make sense and are sometimes retarded. Should **All** software aim to be bug free? Should production code **All** be checked by a compiler? Should production systems **Never** have back-doors? We actually can't know the right answer in general, from a static analysis at least. At the risk of being painted as a godless, sissy liberal in the wake of Yegge's proposal, we need to take a look at the run-time environment.

Buggy software sounds shitty, except that when we're dealing with a situation where the software is replacing an already buggy manual process, no one is going to care. Likewise, there isn't a benefit to taking weeks to prevent a bug that you can fix in days or hours. Finally, if the cost of a rollback or upgrade is close enough to trivial, you can be forgiven for taking more risks than you otherwise would. On the flip-side, yes, your [high-frequency trading software](http://business.time.com/2012/08/08/high-frequency-trading-wall-streets-doomsday-machine/) or your [Air-Data/Inertial Reference Unit](http://safecodellc.net/component/content/article/1-latest-news/112-qf-72-software-bug), or your [cardiac implant firmware](http://www.youtube.com/watch?v=nFZGpES-St8) had damn well better be bug free, and rigorously modeled AND compiler checked *AND* free of back-doors **AND** not allowed anywhere near production if they're even suspected of incorrectness. When the stakes are billions, or lives, eating the cost of a more extensive and rigorous development process makes sense<a name="note-Sat-Aug-11-154847EDT-2012"></a>[|4|](#foot-Sat-Aug-11-154847EDT-2012).

This is not what a preference looks like; it makes sense sometimes and not others, and a correct one can be chosen based on context. A preference is something that there really isn't a "correct" way of thinking about. Something that we have to accept because it's atomic. So even if globally bifurcating the industry would lead to some new insight<a name="note-Sat-Aug-11-153735EDT-2012"></a>[|5|](#foot-Sat-Aug-11-153735EDT-2012), and even if that insight would improve inter-programmer relations<a name="note-Sat-Aug-11-153741EDT-2012"></a>[|6|](#foot-Sat-Aug-11-153741EDT-2012), these aren't the axes to do it on.

So there.

Steven... I disagree. And I won't be adopting your thought framework until you consider filtering out your projections.

* * *
##### Footnotes

1 - <a name="foot-Sat-Aug-11-153625EDT-2012"></a>[|back|](#note-Sat-Aug-11-153625EDT-2012) - If you take a look at the [HN](http://news.ycombinator.com/item?id=4365255), [/.](http://developers.slashdot.org/story/12/08/10/1250231/software-engineering-has-its-own-political-axis-from-conservative-to-liberal) and [G+](https://plus.google.com/110981030061712822816/posts/KaSKeg4vQtz) discussions, you'll already see people conflating the political meanings with the proposed software-oriented labels. Less so on slashdot, where most seem to simply dismiss the point of view, but there's a comment on the Google Plus page that reads

> Dynamic typing has been shown through research to reduce maintainability compared to static typing.
> -Lars Ivar Igesund

Which is, near as I can tell, Utter Horseshit™©. If you bother reading on, when someone asks for a citation, the response is

> The research was done by a friend of mine while working at one of those famous, private research centers (yes, one you've heard of), but to my knowledge it has not been released. I don't remember the statically typed language used in the study, but I Imagine it was Java. The dynamically typed language was Ruby. This I can't point you to it, I just hope that you believe me when I tell you the conclusion of it. It certainly jives with mine experiences.
> -Lars Ivar Igesund

That's about what I was expecting; "This guy I hang out with told me my opinion was totally right". Oh, by the way, 16 upvotes, or plusses, or whatever the fuck. Never-mind the fact that a methodology isn't outlined, or that the definition of "maintainability" isn't mentioned, or that the languages involved are "I Imagine ...  Java" and Ruby, or that we don't know if/how the researcher controlled for differences among teams/programmers/projects or (in case this was a single team doing to separate projects) the teams' innate preferences/learning over the course of the experiment.


2 - <a name="foot-Sat-Aug-11-153633EDT-2012"></a>[|back|](#note-Sat-Aug-11-153633EDT-2012) - Actually, as you can see by the previous note, "is already leading to" would be more accurate. Hell, there's already a guy out there [calling himself a "Software Libertarian"](http://news.ycombinator.com/item?id=4365606), and we haven't even gotten through Software Ayn Rand yet. That's some leapfrogging right there.

3 - <a name="foot-Sat-Aug-11-153728EDT-2012"></a>[|back|](#note-Sat-Aug-11-153728EDT-2012) - I believe that may be the second time I've linked that comic this month.

4 - <a name="foot-Sat-Aug-11-154847EDT-2012"></a>[|back|](#note-Sat-Aug-11-154847EDT-2012) - In a similar vein, it's interesting to note that NASA's [Mars rovers have "dangerous or risky back-doors"](http://www.macworld.com.au/news/fri-10-aug-2012-nasa-upgrades-mars-curiosity-software-from-350m-miles-away-67827/) capable of modifying the systems' programming and data. Presumably it was too risky to send them out without the possibility of an in-flight bugfix?

5 - <a name="foot-Sat-Aug-11-153735EDT-2012"></a>[|back|](#note-Sat-Aug-11-153735EDT-2012) - I doubt it will.

6 - <a name="foot-Sat-Aug-11-153741EDT-2012"></a>[|back|](#note-Sat-Aug-11-153741EDT-2012) - Again, severely doubt it.
