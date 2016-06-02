I read a [question on SO](http://stackoverflow.com/questions/9286318/why-choose-lisp-for-a-project) the other day that asks "Why choose Lisp for a project?". It's was closed with an almost surprisingly swift consensus, but not before three answers were thrown in (and one accepted). And that's pretty good because, as far as I'm concerned, it's the wrong question. "Why choose a language for a project?" is closely related to a second question, which I'll let Peter Norvig describe the shape of:

> I guess the way I look at it is; the best language to use is the one that you and your friends are most productive with. It just turned out that when Google was started, the four programmers that were there first were all C++ programmers, and they were very effective with that, and they kept going with it.
>
> --Peter Norvig, [Ask Me Anything](http://www.youtube.com/watch?v=hE7k0_9k0VA#t=03m10s)

I agree with his sentiments here. The language you should use when you're working on a professional project[^as-opposed-to] is one that your team knows and already thinks in. It would be a mistake for me, today, to start something serious up at the office using Smalltalk because I don't think in it naturally yet. Norvig's response to "What language should I choose?" begs a second question though, because if the best language is one you're already familiar with, then you need to ask

[^as-opposed-to]: As opposed to a toy project.

## When Should I Choose a Language?

Ideally (from the perspective of making the best choice possible), you'd choose it as late as possible because, even though it's fairly difficult to explain this to non-computer people, different languages do have different trade-offs. So you either want to pick your language at the point where you have the most possible information about what the shape of your project is going to be, or you want to pick a language that's flexible enough to be used for pretty much anything.

The trouble is, if you subscribe to the Norvig Awesome Language Theory, you come to the conclusion that either

- your choice has been made for you quite a while ago, or
- you should make your choice before even getting a team together, let alone deciding what you want to do

The first option happens if you're an `x` programmer, or if all your friends are `x` programmers[^where-x-is]. That's not particularly interesting to me, given how I think about development, but I may come back to it later.

[^where-x-is]: Where `x` is bound to a single programming language, regardless of what it is.

The second option is hard because, I'll lay this down as an axiom, at **no point** in a project **will you know less** about what shape the output will take than you do before you've assembled a team. That situation screams "Lisp" at me, although I guess any language with sufficient ease of [DSL](http://en.wikipedia.org/wiki/Domain-specific_language) creation would do. Perhaps it screams other things to other people; I knew a guy at a former place who said

> As far as I'm concerned, unless there's a _really_ good reason not to, you should just use PHP for web development.

I'm not poking fun of the guy either; his reasoning is that since the standard LAMP stack is in place at half a scrillion servers around the world and counting, any bugs likely to bite over the course of a development cycle have already been found. That's the same principle as [Raymond's statement of Linus' Law](http://en.wikipedia.org/wiki/Linus'_Law#By_Eric_Raymond). The trouble is that it leaves you faffing about with PHP even when the entire team knows a more powerful, more consistent language like Python or Ruby.

In fact, applied globally, this principle would have every developer on Earth using [some combination of Java, C, C++ and/or C#](http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html), because odds are that every developer could scrape up ten buddies that are at least marginally proficient with those. There's a tension between "powerful languages" and "popular languages", because each brings benefits to the table. Powerful means you'll be more likely to crack through whatever problem you run into, while Popular means that a lot of problems have been pre-solved for you. Power means you'll be able to move mountains with small teams, Popularity means you'll be able to get big teams together reliably. That begs a third question, because if you've got a specific project in mind, you have to ask

## What kind of Project will this Be?

Are you trying to solve problems for which there are no existing solutions (or for which no satisfactory solutions exist) or are you looking to create incremental improvements to existing solutions? Do you want the ability to scale to hundreds of developers under your roof, or do you want a team of 5, 10 at the outside? Do you know specifically what you want, or is the spec going to change dramatically as you move on? Do you already have a team put together, or have you resigned yourself to the hell of Hiring Humans? You will want different languages depending on your answers to these questions, regardless of what language you currently happen to know. But that begs a fourth question, because if the objectively right thing to do on a project is use a language you don't know, you have to ask

## When Should I Switch Languages?

[Graham says](http://www.paulgraham.com/avg.html) that "[a]fter a certain age, programmers rarely switch languages voluntarily". Which is perfectly understandable, because in light of NALT, it's really only reasonable to switch once you and all your friends are more effective with a new language than with your current language, and that takes the sort of off-hours dedication that I'm already having trouble finding at ~27[^and-i-dont-even-have]. The "why" of it, pragmatically, is also hidden in the statement (although it's a different opinion from Graham than it is from Norvig). The Graham Awesome Language Theory states that there is a pyramid of languages, and you should switch when you realize that there's a higher one than what you already know. That fails to take the team dynamic into account though. If "switch" means "start practising the higher language on your own time", well, sure, sounds reasonable. But if you've got a team of 10[^frequently-happens-at-larger], is it not a mistake to kneecap short-term progress and set yourself up for pain later[^in-the-sense-of-maintenance] in exchange for potential gains at the language level?

[^and-i-dont-even-have]: And I don't even have kids yet. **EDIT FROM THE FUTURE:** I'm 31 now, have some kids, and I still think this is basically the correct opinion. I _have_ "switched" languages since writing the original article though, so make of that what you will.

[^frequently-happens-at-larger]: Or, as frequently happens in a larger company, several(hundred)? teams of 10.

[^in-the-sense-of-maintenance]: At the point that you need to maintain all the beginner code you'll be writing for the first little while.

Norvig says (in effect): switch with your friends. Which may explain why people [switch away from Lisp](http://www.aaronsw.com/weblog/rewritingreddit), even if Graham is right about the shape of the language pyramid. What good is knowing the superior language, if no other human you know speaks it?

## When Should **I** Switch Languages?

A couple of days ago, my dad called me up. Apparently, he got a web design offer from a friend of his, but the requirements weren't exactly his field. What they meant was "Web UI Developer", and would I be interested? I asked for the specifics; it's a big company, I'd be working for a billing department somewhere, the pay was excellent, and the skill/experience requirements were

- Javascript
- Java
- JSTL/JSP
- Spring MVC Framework

This is the point where the language argument hit home for me. My dad isn't a programmer (he considers manual CSS/HTML to be too technical for him), and knows how much I make. So when I told him that I'd see if one of my friends wanted a Java Job because it really isn't my language, he was understandably confused.

"What do you mean, it isn't your language? What difference does it make?"

All the difference.

Because NALT tells me that if I take a Java Job, I'll be dealing with people who take Java Jobs. By and large[^subjectivity-disclaimer], that means people who believe in getting through the day rather than burning with the desire to write brilliant software or exceed themselves. It means dealing with the One True Way to do Anything, which always coincidentally seems to lead through three layers of XML declarations, Eclipse plugins and/or magic. And it means having to deal with design patterns instead of abstracting away the problems that call for those solutions.

[^subjectivity-disclaimer]: This has been my limited, subjective experience so far, it may not represent the Java community as a whole.

So I shouldn't switch languages. Explaining that to someone who isn't at ground zero is difficult, and sort of subjective in any case. In that concrete example I just offered up, I don't think it's fair to say that I chose the wrong languages. "Getting a job as a corporate programmer" was nowhere near my goal list when I started learning and, if anything, it's further away now.

It's also not really fair to say that the employer in this case made the wrong decision. Working on a web-app for a billing department isn't likely to run them up against fundamentally unsolved problems, they likely have a large number of small teams, the business guys (non-programmers) are in charge, and I have a hunch that their turnover is something above the industry average. Given the context, would you seriously recommend Haskell or Common Lisp to these people? They may be superior tools, but they're superior in precisely the way that corporate shops *don't* care about.

## What were we talking about again?

Choosing languages. The fact is that so many factors play into what's the correct answer (if there even is one), from project goals, to company goals to the preferences your particular group. And all that is without even discussing things like technical features, platform availability, deployment strategies or performance. If you have to ask something like "Why choose Lisp for a project?", your entire perspective of the problem could probably use some re-thinking.
