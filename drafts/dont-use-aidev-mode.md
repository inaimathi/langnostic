A little while ago, I wrote and used [`aidev-mode`](https://github.com/inaimathi/aidev-mode). Don't bother using it anymore. If you're currently using it, stop.

## The State of Play

We're well past the point where programming as an independent pursuit matters. If you're learning to be a programmer _now_, then expect to write very little of your own code outside of exercises designed to call bullshit on the LLM/other-approach-to-AI fleets you'll be managing.

If this is still a controversial point to you, I hope you've already saved up enough to retire, or have a move lined up into a different vertical. We'll talk about that in a bit.

Making maximum progress as a programmer today looks like managing interacting fleets of 500-10k loc projects that interlock though known, well-typed APIs which minimize the friction your complete system generates while getting work done. Whatever "work" is in your case; bring your own object level goals. The way you do this with current frontier-level models is by slamming all of your project code, possibly adding a bit of surrounding infrastructure/libraries, telling the model what you want it to do, then reading and understanding the produced code and pushing back/hand-tweaking any sufficiently bad decisions. To a first approximation, the pursuit of programming is now bottlenecked by

1. how quickly can you understand the dataflow/algos/datastructures that go into a particular solution
2. how quickly can you read and understand code purporting to implement those dataflows/algos/datastructures

Everything else is commentary. Touch-typing, having a good macro/keyboard shortcut suite, having type hints/completions/error-highlighting is now approximately fluff. It _helps_, and if you have it you should maintain it, but points #1 and #2 above are critical, and if you don't have _those_, then you're a skilled blacksmith facing down semi-automated drone factories. Luck won't save you.

I wrote [`aidev-mode`](https://github.com/inaimathi/aidev-mode) about a year ago, back in the pre-historic times when the best you could get out of code generating models was a large-ish function at a time. When trying to get Claude to emit multiple interacting functions larger than a couple hundred lines of code, let alone refactoring across multiple modules, generated more errors and failure modes than was worth it to fix manually. At _that_ point, The Correct approach was to understand your code on a function/class level, make transformations in your head, manually type out the simple ones, and occasionally reach for the model when particular transformations were easier to describe than they were to type out. That interaction mode benefits greatly from primitives like `refactor-region-with-prompt` or `refactor-buffer-with-prompt`. But now? If I'm working with a small enough microserivce, I generally paste the complete code ask Claude to make multi-file edits under certain constraints, and more often than not, the result is good stuff that I can incorporate pretty directly, often either applying large deltas to several files or replacing them entirely. Occasionally, I'll have to prompt around some sort of deeper misunderstanding about the goal, but typically this is both easier and more effective than fixing it myself. 

_Today_, you should not be writing your own individual functions. I think you should still know _how_ to, but 

1. That's a significantly different pursuit (At the gym, using an exoskeleton to help you lift is dumb. If you're in the real world and your goal is to get a heavy thing from A to B, _not_ using an exoskeleton to help you lift is dumb.).
2. I'm not entirely sure it's worth it. You need to know how to do it yourself exactly to the extent that that knowledge helps you call out your LLMs' mistakes/misunderstandings, and gives you the ability to do it faster. I suspect without having confirmed that there are ways to get those object level skills more easily without going through the path of actually learning how to program [the long way](https://norvig.com/21-days.html).

## The Near Future

This is all with _today's_ frontier models. That is, as of around May 2026. As I write this, [Mythos](https://red.anthropic.com/2026/mythos-preview/) has not yet had a public release, and we show no signs of slowing down the training of larger models. At the rate we're going, I fully expect programming in its' entirety to be automated by the end of next year at the outside. If you're a system designer, expect to get _a lot_ more value from datastructures, compilers and architecture knowledge than you currently do. If you're a "programmer" in the sense of "I know how to do work with this one Javascript framework", your current skillset is now useless. Sorry. Hopefully you didn't pay too much for it.

Agentic frameworks are still not quite good enough to completely treat as standalone programmers, and I'm not sure how much of that we can genuinely resolve, but I also don't think it matters. If we cut through the current weird feedback spirals that mostly prevent agent frameworks from succeeding at really long horizon tasks, then programming is solved full stop. If we don't, it's likely that your role is going to be less "decide specific tasks to do in order to push your project forward" and more "keep your model on task, pop it out of weird attractor states, and banish hallucinations".

Either way, we'll know in between six and eighteen months.

## The Farther Future

There's a [piece on LessWrong](https://www.lesswrong.com/posts/nR3DkyivzF4ve97oM/how-go-players-disempower-themselves-to-ai) about how a similar situation played out both in the Go and Chess communities. I haven't checked primary sources about it, so this might be wrong, but it looks like these communities took opposite stances regarding AI and can therefore serve as lessons for the rest of us.

### Chess...

...is now basically a celebrity culture. The actual object level thing we're doing is more or less unimportant; no one watches human chess matches because the players are pushing the state of the art forward. They're watching because they like the drama, or follow the players, or are invested in some way with the social implications of the outcome. The frontiers of _chess_ are actually being pushed forward by AI players, but no one watches those matches. The article doesn't mention [centaur chess](https://en.wikipedia.org/wiki/Advanced_chess), but I imagine it's less popular than the straightforward human variety from a viewership perspective.

Human chess players use AIs to train; they get better at the game by honing their skills, and then they go to compete against other humans. But they mostly don't cheat, and their pursuit mostly _isn't_ "play chess as well as possible".

### Go...

...also evolved into a celebrity culture. But cheating prevalence there sounds like it's much higher; to the point that what sounds like the Go equivalent of Magnus Carlsen was credibly[^in-the-sense] accused of using AI assists in tournaments. The effect on the learning pipeline sounds like it was much worse. Players often use AI assists to cheat their way through training, which is both obvious to the teachers, _and_ ends up robbing many of them of their creative progressions. The end result sounds like it's converging on the monoculture of all humans taking AI recommended moves and not bothering to build up their own intuitions of what good play looks like. This is kind of a counterintuitive result; given the space of Go moves, I was assuming we'd get much more breadth of approaches, but it seems like that's not what's happening.

[^in-the-sense]: In the sense of "we've analyzed this players' live and cammed-up games vs pure, camless online games and determined that their real-life+cammed games are much less similar to AI-recommended moves in any given situations than their camless games". In context, this sounds like really good circumstantial evidence

There's a secondary negative effect in that, because most students rely on AI engines, there is relatively little selection pressure from difficult play. Again, haven't checked primary sources, so the author might be off, but the situation seems to be that because AI engines can handle the high level challenges, they're basically shielding the students from understanding when they're out of their depth. Normally, after getting beaten down hard, you'd realize that you're not cut out for this. But if the AI either gets you through it, or takes the "hit", you're less likely to be checked by reality.

### The Lessons

There's two lessons here. 

#### Don't Cheat Yourself of Progress or Contact with Reality

Use AI assistants as a way to help build your programming muscles, not as a substitute to them. Once you've got the muscles and reflexes built up, go either the centaur route and be effective in your output. Also, in the progress of building up your muscles, be honest with yourself about whether these are muscles you want to and are capable of building out. Yes, the AI is going to do a lot of heavy lifting for you out in the wild, but if you can't call it on its' bullshit, you're going to be less effective than if could.

#### There Are Only Three Jobs

You can be a soldier, a farmer or a celebrity. Soldiers are people who advance the state of the art; they do work at the peak of capability and they test their mettle against the world. Farmers are people who keep things running so that soldiers and celebrities can exist; they're not working at the peak of capability, they just need to reliably keep social order from breaking down. Celebrities keep people interested; they're also not at the peak of capability, their job is to command attention and be likeable/hateable. 

Chess masters went from being soldiers to being celebrities; their value used to be in keeping a certain subset of human strategic thinking sharp. Now, they can't. Computers are sharper, and if the shape of the world were decided by chess skill, we would lose every time. To bring this back to the earlier point of what you can credibly be doing for a living as a "programmer"; you need to decide if you want to be a soldier, farmer or celebrity. And the celebrity route is the only realistic one that leads to you not having to use AI as a production input. 

"Farmer" here looks like getting a job at a large institution and seeing if you can make it more efficient somehow, through some political/engineering input, and it looks like most sane paths here lead through AI. This is a _useful_ path to be clear; things like Mythos are shortly going to increase the speed at which we generate zero-days by _a lot_, and we'll need every hand we can get to mitigate the damage and/or keep things humming.

"Soldier" means: get your reps in, and be ready to hit the problems you aim at as hard as you can with any tool to hand. This path doesn't give you the luxury of avoiding AI. Not if you plan on doing it for longer than another three or four months.
