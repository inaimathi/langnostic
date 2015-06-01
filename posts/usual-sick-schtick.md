I'm sick. Not terribly badly today, but badly enough that I don't want to make the trip downtown for fear of exacerbating. So as usual, to create the illusion of productivity, I'm going to put some of my thoughts down on pseudo-paper and post them. Nothing earth-shaking, just a recent journal that I wouldn't have otherwise gotten around to writing. Actually, this is the heavily re-written version, with another two follow-up pieces that I decided to break off into their own articles because they started getting too in-depth and long. I'll be polishing the other two and posting them later in the weekend or week, I'm sure.

### <a name="general-updates"></a>General Updates

I got the [next draft of my chapter done](https://github.com/Inaimathi/500lines/blob/master/event-driven-web-server/writeup.md#on-interacting-through-http-in-an-event-driven-manner-in-the-medium-of-common-lisp). Honestly not sure if I already told you about this already, but I did now, so there. Its been done for a while, and I keep on meaning to go back and polish it, but stuff keeps getting in the way. Mostly work and the child lately. Work because we've got a demo coming up this next week that we need to prep for and child because he's now about two. And man can you absolutely not take your eyes off a two-year old for *five seconds* without them climbing onto something precarious, pissing onto something, or just generally getting rowdy. I don't know how it proceeds obviously, since I'm only two years in, but my current opinion is that anyone who gushingly tells you about the wonders of parenthood is in thrall to [Stockholm syndrome](https://en.wikipedia.org/wiki/Stockholm_syndrome), the [fading affect bias](https://en.wikipedia.org/wiki/Fading_affect_bias), [choice-supportive bias](https://en.wikipedia.org/wiki/Choice-supportive_bias) or some combination. Which is not to say that it's not fun or rewarding, just that there are some pretty definite downsides that I wouldn't try to gloss over if asked for my opinion.

### <a name="rss-feed-changes"></a>RSS feed changes

You've probably already noticed that [the Atom feed](/feed/atom) no longer includes article summaries. No, this is not an effort to get more page hits to my main site; I neither track them nor care to. It's just that I kept running into problems with articles validating according to the Atom standard. Both issues with some unicode characters, and extreme hostility to [last times'](/article?name=BGG%20Data%20Sifting.html) chart markup. The article rendered fine, but its RSS entry completely crashed out the feed. As in, I'd see no updates at all from any of my feed-readers; the feed effectively disappeared because of the latest entry preview. The best short-term fix I can see is just reducing the feed to titles and time-stamps.

Feel free to use scrapers or other aggregation techniques if you like, as long as you abide by [the license](http://creativecommons.org/licenses/by-sa/3.0/). I just don't want the headache of having to make sure my articles are Atom-compliant. I might think something better up for the future, especially once I switch over fully to [`cl-notebook`](https://github.com/Inaimathi/cl-notebook#cl-notebook), but no promises.

### <a name="clnotebook-updates"></a>`cl-notebook` updates

Speaking of which, [`cl-notebook`](https://github.com/inaimathi/cl-notebook) now has pretty good support for [Quicklisp](http://www.quicklisp.org/beta/), and basic package management.


-   each notebook has its own package now. By default, named the same thing as the notebook itself
-   you can change the package definition by clicking on the notebook title and editing the code block that comes up
-   if you add new packages to the dependencies of your notebook, they're automatically installed and loaded
-   if they can't be installed or loaded for some reason, you get an error


I'm not entirely sure if this feature will stay exactly the same once we get to the beta. Mainly, I'm not sure what the appropriate course of action is on a package error. Do I just not register the new package? Do I register it, but use the default package for loading purposes instead? Do I load as much as possible of the specified package and just ignore the rest? In practice, I get the feeling this would be a very rare edge-case at best, but I think it pays to think about edge cases if you can.

The slightly more interesting thing that this will eventually enable is trivial binary building. Now that I've got a package declaration form, it starts to seem easy enough to use that to figure out dependencies, call out to [`buildapp`](http://www.xach.com/lisp/buildapp/) and spit out a binary for a particular notebook. As long as it has an appropriately arranged `main` function, I mean. You still kind of have to plan for this thing, but I aim to take a bit of the bite out of executable creation for those of us *not* using LispWorks.

I'm coming closer and closer to removing the "pre" from [the "pre-beta" project status](https://github.com/Inaimathi/cl-notebook#this-is-now-a-pre-beta). The only thing still standing in my way at this point is the lack of macro-stepper. At that point, it's all UI improvements, nice-to-haves, and maybe a model tweak or two to what I would consider a Good Editor.

### <a name="finer-points-of-common-lisp-error-handling"></a>Finer points of Common Lisp error handling

I *think* this is just me being an idiot, but you still may want to take a closer look at the [error handling PCL chapter](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html). Seriously; there are intricacies, especially if you're already used to the now-mainstream `try`/`catch`/`finally` approach. I have in fact worked with Common Lisp conditions before, but never quite so extensively as while building [`cl-notebook`](https://github.com/inaimathi/cl-notebook). For some reason, until doing so, I thought that [`handler-bind`](http://www.lispworks.com/documentation/HyperSpec/Body/m_handle.htm#handler-bind) was just syntactic sugar over [`handler-case`](http://www.lispworks.com/documentation/HyperSpec/Body/m_hand_1.htm#handler-case). This is, to put it mildly, inaccurate. [Here's the SO question](http://stackoverflow.com/questions/25773251/using-the-take-new-restart-in-sbcl) in which I finally figured that out.

`handler-case` is actually fairly close to the `try`/`catch` constructs you might already know. When an error that matches one of its clauses happens, it unwinds and invokes said handler. `handler-bind` does something else entirely; it *doesn't* unwind before calling the handler, and it *doesn't* implicitly return from the error scope. This is relevant when you want to continue from the error without unwinding the stack, when you want to invoke some local restarts, or when you want to inspect bound values at the error site.

If you want details, take a look at that SO question I linked above, or maybe some of the different uses of `handler-case` vs `handler-bind` in [`cl-notebook`s' `model.lisp`](https://github.com/Inaimathi/cl-notebook/blob/master/model.lisp). I just wanted to point out that there is a difference, that's all.

Ok, off to sleep, then polish a couple of other pieces, and maybe that chapter.

Wish me luck.
