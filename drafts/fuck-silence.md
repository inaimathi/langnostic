Ok, so here's the thing.

There's no way I'm ever catching up. Ever. I've been reading, watching and thinking about entire freaking worlds of such intricate interest that there is absolutely no way I'm ever giving you the full picture, even if I were to somehow start writing at the staggering pace of an article per week[^which-is-already-ridiculous]. At some point, I have to start being honest with myself about what a realistic expectation looks like here. Which, unfortunately means writing about a relatively smaller subset of my experience, but trying to do it a lot more regularly.

[^which-is-already-ridiculous]: Which is already ridiculous, but that would just mean I'd start keeping up with the stream. In order to actually _catch up_ in a year or two? What I'd really need to do is something like a full article every two days for the duration.

This is still very probably superior in some way to sitting in spin-locked silence, but do be aware that you're not getting anywhere near the complete picture. I suppose you never _really_ were, but there was a glorious, 8-year moment where we could both squint and pretend. So, with that mindset, here's an incomplete, molten core sample from my thoughts at the moment.

## Collective Decision Making

There's [a project](https://github.com/inaimathi/cl-vote) I've been poking at for a few weeks at this point meant to ease some pain in a particular collective decision making process. [We](http://compscicabal.github.io/) need to decide which paper to read next, you see. And the way we do it right now is, embarrasingly, by mildly abusing [the `github` issue system](https://github.com/CompSciCabal/SMRTYPRTY/issues). What I'm trying to do is come up with a voting and scheduling system for us that both hooks into the `github` auth system _and_ doesn't suck.

The starting point is my usual prototyping toolkit of [Common Lisp](https://common-lisp.net/) combined with [`house`](https://github.com/inaimathi/house) and [`fact-base`](https://github.com/inaimathi/fact-base), and the first thing I'm doing is putting together the minimal web API for interacting with collections of papers in an effort to prioritize them. I'm not going to talk more about this now, mostly because it isn't anywhere near done yet, and that's the goal state in the near future[^reserve-the-right].

[^reserve-the-right]: Though I reserve the right to re-write it in [Clojure](https://clojure.org/) using [`gardendb`](https://github.com/gardendb/gardendb) and some monstrosity I put together to replace the centralized routing tables that Clojure web frameworks seem to have in common.

## Robustness as a founding principle

"Robustness", as in, a computation should be very, _very_ hard to disrupt adversarially. And "founding principle" as in, it should take priority over correctness and performance. This is the bizarre-seeming idea behind a [series](https://www.youtube.com/watch?v=I4flQ8XdvJM) of [videos](https://www.youtube.com/watch?v=helScS3coAE) that I've been [digesting](https://www.youtube.com/watch?v=OQsn1c92pdY) lately. The particular approach that ends up evolving is that of a massively parallel cellular automaton, where effectively each cells' behavior is specified separately.

I've got a minimal, toy simulator implemented over [on github](https://github.com/inaimathi/lem) that more or less works, and seems like it'll make exploring the idea relatively simple going forward. And there's a bunch of things that seem worth exploring, ranging from the implications of demoting correctness and performance in importance, to the specifics of how robustness works in the face of things like [grey goo](https://en.wikipedia.org/wiki/Grey_goo).

## Eating One's Own Tail

A [recent paper](http://web.cs.ucla.edu/%7Epalsberg/paper/popl16-full.pdf)[^still-reading-this] showed us how we might go about writing a self-interpreter for a language whose type system you would expect to prevent such shenanigans. The thrust of it so far is that this is possible by playing semantic games with the definition of "quoting" and "unquoting" in the macro sense. I'll let you know if additional insights are refined from today's reading.

[^still-reading-this]: Which we're actually still reading as I write this. It's one of the papers interesting enough to warrant a second week being spent on it.

## The Unreliability of Machine Supply Chains

A _different_ [recent paper](http://sharps.org/wp-content/uploads/BECKER-CHES.pdf) showed [us](http://compscicabal.github.io/) that machines are even more laughably insecure than we thought. There are supply-chain level attacks that can compromize hardware in a way that is almost fiendishly hard to detect. We didn't think much of the revelation, but discussions on how we might have some security guarantees despite the presence of hardware trojans happened regardless. There doesn't seem to be a very good way of preventing eavesdropping, but guarding against maliciously corrupted computations at least seems possible. It also seems like Robustness-first principles from the previous section might help out here in some way. That part, I'll have to get back to you on after I do a bit more prototyping.
