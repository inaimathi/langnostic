This is going to be a series of posts wherein I work out my assumptions, beliefs and prescriptions on AI and AI safety in general. It's meant to go as close to the roots of those beliefs as I can get in an effort to convince currently unconvinced people (or possibly me) that they should think differently. We're not hitting [metaethics](https://en.wikipedia.org/wiki/Metaethics), but we're going pretty fundamental. It's going to cover basic concepts, and I'm going to be covering ground that anyone involved in the AI safety community or AI community in general has already covered. Plausibly this will be boring for you.

# AI Safety

If you're interested in AI safety as a project, you need to have a particular configuration of beliefs about the current state of the world and potential futures. Specifically

1. AI can do good in the world. We can extract private or public goods from it that we wouldn't be able to otherwise. Or possibly, it might make certain things easier without enabling new classes of action for us.
2. AI has downsides and tradeoffs, and these are worth seriously grappling with.
3. As a collective or as individuals, we are capable of acting and coordinating in order to mitigate the downsides and tradeoffs such that AI ends up being a net good in the world

![](/static/img/ai-safety-territory/002-ai-safety-points.png)

These are foundational. It's possible to be nuanced in each one, and doing so will put you in the position of helping different projects within AI safety. And it's possible to hold additional normative or empirical beliefs regarding AI and still fit. But if you _fully_ disagree with any or all of them, then you should probably question if AI safety is actually the thing you should be putting energy into. Similarly, if you agree with all three[^and-the-hidden-zeroth], but don't think AI safety is valuable, question that.

[^and-the-hidden-zeroth]: And the hidden zeroth assumption: AI will have a significant impact on our future, and this is worth being concerned about.

We can map out the adjacent movements/beliefs/positions to AI safety by seeing what they disagree with.

### AI Abolitionists/Anti AI

![](/static/img/ai-safety-territory/005--an-ai-abolitionist.png)

If you disagree with point 1, you think AI can not do good in the world. At the extreme, your model of the world is that it has only downsides, and will definitely result in net bad outcomes in the world. These are people rationally agitating for datacenter closures in full generality and complaining about any and all slop anywhere.

### AI Maximalists/EAcc

![](/static/img/ai-safety-territory/006--an-ai-maximalist.png)

If you disagree with point 2, you think AI can do _only_ good. Potentially, you think that all the tradeoffs and failure modes are either exaggerated, or fabricated, or will be trivial to mitigate. Or possibly that the upsides of AI are so huge that it'll be net positive no matter what we do. You think that the real path forward is to do all the capabilities research all the time, and rationally want to pump maximum capital into new datacenters because doing so will provide more goods. These people are confused by the pause movement, and possibly AI Safety writ large because we're moving to mitigate non-issues in a way that delays a better world.

### Doomers

![](/static/img/ai-safety-territory/004--an-ai-doomer.png)

If you disagree with point 3, you think AI _can_ do good, _and_ has downsides, but the downsides are either so overwhelmingly bad that we need to reduce their risk to zero, or that we won't mitigate them. This can either be because they're hard or impossible to mitigate, or because we're bad at mitigating, or we're disincentivized from mitigating, or they're intrinsically linked to the goods we want to extract, or we'll be disempowered by the time it matters.

![A map of the AI Safety plains and surrounding territory](/static/img/ai-safety-territory/001-ai-safety-region-map.png)

# Things You Don't Have To Believe

Those are the _substantive_ axes of disagreement. There's a bunch of things generally clustered in with AI Safety that I don't think are necessarily entailed by it. They're side arguments, and you could hold most positions on most of them while still being an AI Safetyist in the sense I'm specifying here. These include but definitely aren't limited to

- AGI/ASI is imminent
- AI misalignment is the only or even primary AI risk
- AI can or will automate AI research
- Human-level general intelligence is an important threshold
- There are technical solutions to the alignment problem
- We have to "solve" AI alignment in full generality
- Existential risk is high
- Current systems are already dangerously capable

![](/static/img/ai-safety-territory/003--an-ai-safetyist.png)

You can imagine people who take any of the extreme positions on each of those points and still think the pursuit of AI Safety is worthwhile.

# Vs. Abolitionists/Anti AI

![](/static/img/ai-safety-territory/007--safetyist-vs-abolitionist.png)

The disagreement with abolitionists comes down to whether we can expect goods from AI. Things that we'd otherwise not get, or get at a worse exchange rate, without AI.

## Can AI do any good at all?

In extreme cases, some people believe or seem to believe that AI just _can't do good things_. They phrase this as "it's all slop", and the argument here is going to be establishing a task class or use case where AIs are unambiguously good. Something like

1. there is a task that humans perform
2. the humans that perform it professionally hate doing it
3. it's not pre-AI automatable because of some amount of technique/intellectual engagement/multi-step interaction situation
4. it _is_ AI automatable because currently existing LLMs can bridge the technique/engagement/multi-step barrier the vast majority of the time (or all the time with scaffolding somehow)

A decent example of this task class is scheduling for a team, so I know these exist, but I'm not going to talk about concrete arguments in this post. We're laying out the shape of things we believe or expect.

## Can AI be net good?

A more nuanced argument is that AI _can_ do good, but it will obviously and intrinsically be a net negative. This is usually phrased as "AIs are only good for coding" or "companies can't wait to lay off staff to reduce costs, but the resulting work will be worse". This is already _very_ close to the AI Safetyist position; the remaining disagreement here is whether we can mitigate or compensate for the downside to the point that we push the end result back into "net good" territory. The disagreement might move people between Safetyist, Doomer and Abolitionist.

### Can we leave some aspects of AI "on the table"?

One possible ground for arguments is just to split tasks off. We could concede that if we deployed AI in every situation where it's not prohibited by the laws of physics and economics, that would come out net bad, and then point out that we don't need to do this. We can make AIs audit our networks for security gains, do some special-case-theorem-prover-backed general code and media generation, and just _not_ put it in charge of our militaries or let it raise our children.

### Are there important goods that are practically inaccessible without sufficiently capable AI?

A possible ground for argument is that there are frontier uses of AI which are just plain out of our reach if we don't deploy stronger models, and it's possible for these frontier uses to be things we want badly enough that we'll accept the inherent trades. I don't know the details, but ones that get thrown around are various physics and biology breakthroughs. What's the cure for all disease worth? Empirically, Ozempic is worth [TODO](get current value of the Ozempic prescription market). Presumably The Cure would be more, and we should plausibly be able to trade something like "unfortunately, all imaging techs are now unemployed" if it means training and deploying models that give it to us.

Again, I'm not making the claim that this _is_ possible, I'm just outlining the shape of the arguments we could have in this space.

### Can AI reduce friction enough on things we could do without it that it's worth it even if not unique?

Apart from frontier benefits, can AI give us gains in task classes that humans are already pretty good in, but not perfect candidates for automation? Can a human accountant or auditor be 40% more efficient in their work by using AI? If so, this is an example of a good, and we can talk about what that, possibly non-transformative, good in the context of something on one side of the ledger.

### Are there goods AI can produce at a scale that changes the comparison?

Plausibly, there are goods AI can produce _much faster_ and more efficiently than humans could ever hope to. We sort of went through this with furniture. It started as hand-crafted artisanal pieces that took weeks to months [TODO](example link), went to artisan workshops a-la the arts and crafts movement [TODO](link to William Morris stuff), then got industrialized, regularized and logisticsed down to a local minimum. If you're in North America today, getting a chair means paying IKEA or Amazon somewhere between $20 and $200 depending on the chair, then waiting for it to arrive the following morning. Are there other things this same process is still waiting to be applied to, which we would be very happy with in retrospect?


# Vs. AI Maximalists/EAcc

![](/static/img/ai-safety-territory/008--safetyist-vs-maximalist.png)

This is, in many ways, the exact opposite set of disagreements between the Abolitionists and the Safetyists. The cruxes might seem like they'd be inversed, but only partially. We should resist the temptation to argue with Maximalists on the grounds of "AI won't do as much good as you think". Grant them any non-infinite amount of good and move forward to the cruxes we care about. Plausibly you might even grant an _infinite_ amount of good? I'm unsure how this would cash out because it would have to run into the effectively-infinite downside of loss-of-control or extinction risk?

## Does AI have risks, tradeoffs or downsides at all?

The extreme disagreement here is over whether AI has downsides at all. This might be phrased literally as "AI will be smarter than we are, it will make none of our mistakes", or as something more nuanced like "On a long enough timescale, the world will be in a better state than we could imagine despite short term setbacks". On some level, this is one of my problems with ["The Plan" from AI 2040](TODO). By the end of it, our biggest disagreements are implied to be...dividing up galactic real-estate? Which is an obviously utopian kind of far-future end state. The arguments here are going to revolve around

1. whether there is any downside that comes from AI at all that stays with us on the relevant timescale
2. whether there is any downside severe enough that it effectively truncates any timeline. Presumably loss-of-control risks or existential risks fit this bill, so arguments might hinge around how likely and severe we find those risks.

## Can AI be net bad?

Once we're past establishing whether there are downsides at all, the next set of disagreements revolves around whether AI will be net-bad by default. A Maximalist can grant that there are some risks and tradeoffs, but still argue that with no additional Safety input, the effect will be net good, possibly by a wide margin. And, assuming we avoid extinction and loss-of-control, if that's true, a Maximalist agenda is still worth following. I suspect that the strongest arguments against here are going to be things showcasing recent alignment issues that made it into the wild. In particular [the Huggingface incident](https://www.lesswrong.com/posts/xPAxz4g96uKz9FrHs/what-happened-openai-and-huggingface#The_Shorter_Version) and [the gym incident](https://www.bbc.com/news/articles/cn0nww2qlp7o); things that point to "AIs are harder to control than we would have thought last year".

### Could AI have catastrophic or existential downsides?
### Can individually beneficial uses of AI add up to a collectively bad outcome?
### Can increasing AI capability increase its downsides as well as its benefits?

# Vs. The Doomers

![](/static/img/ai-safety-territory/009--safetyist-vs-doomer.png)

On some level, this strikes me as the hardest set of disagreements to face down, because I'm not sure how to fight some of them. [IABIED](https://ifanyonebuildsit.com/) is sort of an artifact of this mindset, and the predictions are hard to refute if you accept most of their premises.

## Can we mitigate the risks, tradeoffs or downsides at all?

The "easy" disagreements are going to have the shape of "here are approaches we might use to steer AIs better, account for them". There _are_ some alignment technologies, some policy choices that we can make and some technical choices we can make that can have a positive effect on the impact of AIs in the world.

## Can we mitigate them enough to materially change the outcome?

The "hard" disagreements center around existential risks, gradual disempowerment and loss of control risks. These are harder because 

- they haven't happened yet so they're harder to reason about, 
- and it's unclear whether we could mitigate them, 
- and in the absence of mitigation, if we can't also avoid them, they're catastrophic

Which is an end state we're all hopefully trying to avoid. Depending on how many arguments we concede here, our prescriptions might end up being pretty extreme sounding, bordering on the Abolitionist positions. Things like "ban all frontier AI research", or "ban all datacenters of certain sizes".

## Is the development of advanced AI meaningfully steerable?

There's a disagreement on whether we can meaningfully steer greater-than-human-level intelligences at all. This is "alignment research on hard mode". I'm inclined to take it seriously, but doing so means _at least_ being favorable to a pause and possible more stringent capabilities research regulations. I think this involves the most powerful opposing arguments, because the Doomers will basically be pointing our own anti-Maximalist arguments right back at us. "Here's an example where even a current level AI, with human experts and some controls charged with caretaking, still managed to do strange unaligned things and cause damage in the real world. What makes you think _more_ capable AIs will do less of that?"

### Is catastrophic failure inevitable given sufficiently capable AI?

This is a concrete disagreement worth calling out. Given AGI or ASI of sufficient capability, are there _any_ futures that don't pass through a catastrophic failure? How many of those futures are straightforwardly reachable with no alignment breakthroughs? If the answer is no, and we can't present a coherent story that gets us there, plausibly we should be less bullish on the Safetyist agenda.
