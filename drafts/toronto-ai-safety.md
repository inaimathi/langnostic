So I've been going to the Toronto AI safety meetup for a few weeks, and thought I'd start journalling my observations and thoughts.

The format at these meetings is usually a bunch of chatting, followed by a talk, followed by more chatting only now at least slightly informed by the talk. This weeks' was on [Web Agents](https://arxiv.org/abs/2307.12856), a technology that lets you ChatGPT-style prompt an agent to go do things for you on the internet, including interacting with websites and generating code. This can't possibly go wrong.

## Pre-Talk Discussion

There was some talk about the recently passed executive order, as well as some of the things that the UK and EU are doing to try to regulate frontier systems. We briefly discussed [Zvi's writeup](https://www.lesswrong.com/posts/PvBpRu354uG7ypwRP/on-the-executive-order) and shared some thoughts on whether this was good or bad on balance. It turns out one of the attendees is involved with the [AI standards lab](https://www.aistandardslab.org/), who are trying to write up some baseline specifications of the things found in the EU regulatory documents. My understanding is that they're taking a bunch of political goals and statements as input and outputting a set of gears-level definitions of the necessary concepts. It seems like good and necessary work that needs to happen if anything good is to come out of the process.

I should mention, by the way. The demographics of this meeting skewed young-ish, and seemed to have about an even split of techno-optimists and pessimists in attendance. I'm not _entirely_ clear on what P(doom) the group as a whole would give you, but I think it'd be non-trivial. After I've been interacting long enough, I'll try to do a write up of what segments of the doomer spectrum are present. For now, all I'm sure of is that the group has no one at [Yudkowski's level of alarm](https://www.lesswrong.com/posts/uMQ3cqWDPHhjtiesc/agi-ruin-a-list-of-lethalities), and only one or two members at the opposite, dismissive extreme.

## The Talk

Read the [linked paper](https://arxiv.org/abs/2307.12856) for the full gory details. What struck me here was

1. Their testing procedure, outlined in Section 4 of the paper, involved letting this thing loose on the live internet. Combined with the fact that their abstract makes reference to Python code generation and execution, this made me expect that there would be at least a minimal discussion of alignment.

and

2. There was no such discussion. Not so much as an acknowledgement of the style "we realize that giving an AI access to the public internet and letting it execute arbitrary Python code might be the sort of thing that has a non-zero chance of going wrong". I honestly don't think it crossed anyone's mind over the course of this project.

Granted, their training data and methodology just gave this agent directives to go out and find information. But going out and finding information in a way as general as it would need to would naturally involve clicking "submit" on things and sending out large amounts of network traffic. If I were doing something along these lines, I'd at least be watching the communication channels and logging everything furiously in a brain-dead non-AI-controlled way.

Maybe Eliezer is working at the correct level of optimism regarding smart humans' level of self preservation?

The interestingly different mechanics between this and other web scraping agents are that this one has a longer context window, is trained to itself summarize and trim down DOM nodes in order to get at the relevant data in the websites it crawls. Apparently this makes it around 20% more effective on some planning and data retrieval benchmarks.

## Post-Talk Discussion

We discussed the topic of web agents from the perspective of whether they might be better served by acting on pixel information rather than the incoming DOM tree. It looks like there might be a [separate paper](https://arxiv.org/abs/2305.11854) on this approach, in case you're interested, but the discussion also touched on [AgentGPT](https://agentgpt.reworkd.ai/), general [web scrapers](https://brightdata.com/), [TabFS](https://omar.website/tabfs/#evaluate-javascript-on-a-page--watch-expressions-demohttpstwittercomrsnousstatus1364008241588363264) and, because this _is_ the Toronto AI Safety group, a few points about how [agents are inherently more dangerous](https://arxiv.org/abs/2302.10329) than other kinds of AIs and how [fine tuning makes this worse](https://arxiv.org/abs/2310.03693).

Between talking about agents, misuse danger vs autonomous danger, and working through some of the implications of [Anthropic's RSP](https://www-files.anthropic.com/production/files/responsible-scaling-policy-1.0.pdf), the big thought that this conversation made explicit for me, which I vaguely knew but didn't conceptually have in as many words, is that "solving alignment" isn't a thing. It isn't a thing in the same sense that "curing cancer" or "ending COVID" isn't a thing. Because even if you made sure that something like Web Agents would never start pursuing its own goals, which [you can't with full certainty](https://www.astralcodexten.com/p/deceptively-aligned-mesa-optimizers), you'd still be in an arms-race scenario where users of web agents might ask evil things like "buy me the cheapest set of ten military grade drones with rooted no-fly chips" or stupid things like "where can I get several tonnes of fertilizer and road flares?". I'm not entirely clear on exactly how best to talk about this yet, so I'll let it sit on a backburner for now. I think the takeaway that I need to chew over is that, absent a gameboard-flip, we're really thinking about mitigation strategies and trade-offs attached to certain levels of security. In the same sense that [the optimal level of fraud is not zero](https://www.astralcodexten.com/i/123307142/the-optimal-amount-of-bad-thing-is-not-zero), the optimal level of existential risk might not be zero. Though, to be fair, I expect that the optimal amount of existential risk is much lower than the optimal amount of fraud.

In any case, by that point in the evening it was time to head back out into the world. I'm planning on continuing to journal about Toronto AI activities though I'm not sure I'll manage to be at next weeks' (if you're interested and in Toronto, get in touch with me and I'll see about adding you to the AI safety and possibly CS Cabal slacks).

As always, I'll let you know how it goes.
