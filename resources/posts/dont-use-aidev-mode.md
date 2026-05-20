A little while ago, I wrote and used [`aidev-mode`](https://github.com/inaimathi/aidev-mode). Don't bother using it anymore. If you're currently using it, stop.

## The State of Play

We're well past the point where programming as an independent pursuit matters. If you're learning to be a programmer _now_, then expect to write very little of your own code outside of exercises designed to call bullshit on the LLM/other-approach-to-AI fleets you'll be managing.

If this is still a controversial point to you, I hope you've already saved up enough to retire, or have a move lined up into a different vertical.

Making maximum progress as a programmer today looks like managing interacting sets of 500-10k loc projects that communicate through known, well-typed APIs in order to do work. Whatever "work" is in your case; bring your own object level goals. The way you do this with current frontier-level models is by slamming all of your project code, possibly adding a bit of surrounding infrastructure/libraries, telling the model what you want it to tweak, then reading and understanding the produced code and pushing back/hand-tweaking any sufficiently bad decisions. To a first approximation, the pursuit of programming is now bottlenecked by

1. how quickly can you understand the dataflow/algos/datastructures that go into a particular solution
2. how quickly can you read and understand code purporting to implement those dataflows/algos/datastructures

Everything else is commentary. Touch-typing, having a good macro/keyboard shortcut suite, having type hints/completions/error-highlighting is now approximately fluff. It _helps_, and if you have it you should maintain it, but points #1 and #2 above are critical, and if you don't have _those_, then you're a skilled blacksmith facing down semi-automated drone factories. Luck and skill won't save you.

I wrote [`aidev-mode`](https://github.com/inaimathi/aidev-mode) about a year ago, back in the pre-historic times when the best you could get out of code generating models was a large-ish function at a time. Asking Claude to emit multiple interacting functions larger than a couple hundred lines of code (let alone refactoring across multiple modules) generated more errors and failure modes than was worth it to fix manually. At _that_ point, The Correct Approach was to understand your code on a function/class level, make transformations in your head, manually type out the simple ones, and occasionally reach for the model when particular transformations were easier to describe than they were to type out. That interaction mode benefits greatly from primitives like `refactor-region-with-prompt` or `refactor-buffer-with-prompt`. But now? If I'm working with a small enough microservice, I generally just paste the complete code and ask Claude to make multi-file edits under certain constraints. And more often than not, the result is good stuff that I can incorporate pretty directly, often either applying large deltas to several files or replacing them entirely. Occasionally, I'll have to prompt around some sort of deeper misunderstanding about the goal, but typically this is both easier and more effective than fixing it myself. 

_Today_, you should not be writing your own individual functions. I think you should still know _how_ to.

## The Near Future

This is all with _today's_ frontier models. That is, as of around May 2026. As I write this, [Mythos](https://red.anthropic.com/2026/mythos-preview/) has not yet had a public release, and we show no signs of slowing down the training of larger models. At the rate we're going, I expect programming to be fully automated by the end of next year at the outside. If you're a system designer, expect to get _a lot_ more value from datastructures, compilers and architecture knowledge than you currently do. If you're a "programmer" in the sense of "I know how to do work with this one Javascript framework", your current skillset is now useless. Sorry.

Agent frameworks are still not quite good enough to completely treat as standalone programmers, and I'm not sure how much of that we can genuinely resolve, but I also don't think it matters. If we cut through the current weird feedback spirals that mostly prevent agent frameworks from succeeding at really long horizon tasks, then programming is solved full stop. If we don't, it's likely that your role is going to be less "decide specific tasks to do in order to push your project forward" and more "keep your model on task, pop it out of weird attractor states, and banish hallucinations".

Either way, we'll know within six to eighteen months.
