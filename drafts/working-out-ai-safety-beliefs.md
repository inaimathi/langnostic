This is going to be a series of posts wherein I work out my assumptions, beliefs and prescriptions on AI and AI safety in general. It's meant to go as close to the roots of those beliefs as I can get in an effort to convince currently unconvinced people (or possibly me) that they should think differently. We're not hitting [metaethics](https://en.wikipedia.org/wiki/Metaethics), but we're going pretty fundamental. It's going to cover basic concepts, and I'm going to be covering ground that anyone involved in the AI safety community or AI community in general has already covered. Plausibly this will be boring for you.

# AI Safety

If you're interested in AI safety as a project, you need to have a particular configuration of beliefs about the current state of the world and potential futures. Specifically

1. AI can do good in the world. We can extract private or public goods from it that we wouldn't be able to otherwise. Or possibly, it might make certain things easier without enabling new classes of action for us.
2. AI has downsides and tradeoffs, and these are worth seriously grappling with.
3. As a collective or as individuals, we are capable of acting and coordinating in order to mitigate the downsides and tradeoffs such that AI ends up being a net good in the world

![An old-timey-map-style three point panel showing the minimal AI Safety beliefs](/static/img/ai-safety-territory/002-ai-safety-points.png)

These are foundational. It's possible to be nuanced in each one, and doing so will put you in the position of helping different projects within AI safety. And it's possible to hold additional normative or empirical beliefs regarding AI and still fit. But if you _fully_ disagree with any or all of them, then you should probably question if AI safety is actually the thing you should be putting energy into. Similarly, if you agree with all three[^and-the-hidden-zeroth], but don't think AI safety is valuable, question that.

[^and-the-hidden-zeroth]: And the hidden zeroth assumption: AI will have a significant impact on our future, and this is worth being concerned about. Rejecting this assumption makes you an AI civilian ![An image of a blonde, bearded man wearing a shirt reading "Not my problem, I have better things to do". The image is listing out various reasonable things to prioritize in your own life over AI related concerns and why you might do so](/static/img/ai-safety-territory/006-5--an-ai-civilian.png)

We can map out the adjacent movements/beliefs/positions to AI safety by seeing what they disagree with. These aren't intended to be mutually exclusive buckets. Think of them as directions out of the AI Safety region: the harder you reject one of the three claims, the farther you move toward the corresponding neighboring position.

### AI Abolitionists/Anti-AI

![An image of a person in a hoodie and green cloak holding protest signs outside of a large, well guarded datacenter. Her badge is a crossed out robot.](/static/img/ai-safety-territory/005--an-ai-abolitionist.png)

If you disagree with point 1, you think AI cannot do good in the world. At the extreme, your model of the world is that it has only downsides, and will definitely result in net bad outcomes in the world. These are people rationally agitating for datacenter closures in full generality and complaining about any and all slop anywhere.

### AI Maximalists/EAcc

![An image of a person in front of a gleaming techno-futurist city, smiling and holding a binder reading "Boundless Future". Surrounding writings are AI-optimistic including "Risks? Manageable. Potential? Infinite."](/static/img/ai-safety-territory/006--an-ai-maximalist.png)

If you disagree with point 2, you think AI can do _only_ good. Potentially, you think that all the tradeoffs and failure modes are either exaggerated, or fabricated, or will be trivial to mitigate. Or possibly that the upsides of AI are so huge that it'll be net positive no matter what we do. You think that the real path forward is to do all the capabilities research all the time, and rationally want to pump maximum capital into new datacenters because doing so will provide more goods. These people are confused by the pause movement, and possibly AI Safety writ large because we're moving to mitigate non-issues in a way that delays a better world.

### Doomers

![An image of a person sitting, looking over a valley with a natural landscape and a small town. His gray cloak has an hourglass icon on it, and a banner behind him reads "We can't steer what we can't control"](/static/img/ai-safety-territory/004--an-ai-doomer.png)

If you disagree with point 3, you think AI _can_ do good, _and_ has downsides, but either the downsides are overwhelming, or plausible interventions cannot mitigate the important downsides enough to leave a sufficiently capable AI future net positive. This can either be for a variety of reasons; because the harms are hard or impossible to mitigate, because we're bad at or disincentivized from mitigating, because the harms are intrinsically linked to the goods we want to extract in some way, or because we'll be disempowered by the time it matters.

![A map of the AI Safety plains and surrounding territory. There is a Village of Doomers across the river to the northeast, the Badlands of AI Abolition to the north, the Desert of AI Maximalism to the south (including a city in the desert and a cordoned off "Former Nuclear Testing Site of EAcc"). There is a city across a bridge in a bay to the west labelled "The Possibly Imaginary Metropolis of Alignment by Default"](/static/img/ai-safety-territory/001-ai-safety-region-map.png)

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

![An image of an AI Safetyist. She's holding a book titled "Tools for a Better Future", and holding a blue and gold banner of a tree icon. The three points of AI Safety are on signs in the background.](/static/img/ai-safety-territory/003--an-ai-safetyist.png)

You can imagine people who take very different positions on each of those questions and still think the pursuit of AI Safety is worthwhile.

# Vs. Abolitionists/Anti-AI

![An AI safetyist arguing with an AI Abolitionist in front of a mixed faction crowd](/static/img/ai-safety-territory/007--safetyist-vs-abolitionist.png)

The disagreement with abolitionists comes down to whether we can expect goods from AI. Things that we'd otherwise not get, or get at a worse exchange rate, without AI.

## Can AI do any good at all?

In extreme cases, some people believe or seem to believe that AI just _can't do good things_. They phrase this as something like "it's all slop", and the argument here is going to be establishing a task class or use case where AIs are unambiguously good. Something like

1. there is a task that humans perform
2. the humans that perform it professionally hate doing it
3. it's not pre-AI automatable because of some amount of technique/intellectual engagement/multi-step interaction situation
4. it _is_ AI automatable because currently existing LLMs can bridge the technique/engagement/multi-step barrier the vast majority of the time (or all the time with scaffolding somehow)

These tasks definitely exist, and I'm working on some variant of one of them, but we're not laying out arguments in this section, just pointing to what shape the argument must have in order to resolve the disagreement. 

## Can AI be net good?

A more nuanced argument is that AI _can_ do good, but it will obviously and intrinsically be a net negative. This is usually phrased as "AIs are only good for coding" or "companies can't wait to lay off staff to reduce costs, but the resulting work will be worse". This is already _very_ close to the AI Safetyist position; the remaining disagreement here is whether we can mitigate or compensate for the downside to the point that we push the end result back into "net good" territory. The disagreement might move people between Safetyist, Doomer and Abolitionist.

### Are AI's goods and harms separable?

Fundamentally, the question here is to what degree we can leave some aspects of AI "on the table". One possible ground for arguments is just to split tasks off. We could concede that if we deployed AI in every situation where it's not prohibited by the laws of physics and economics, that would come out net bad, and then point out that we don't need to do this. We can make AIs audit our networks for security gains, do some special-case-theorem-prover-backed general code and media generation, and just _not_ put it in charge of our militaries or let it raise our children. The big question here is: can we capture a large fraction of the desirable frontier while declining to build/deploy the systems producing the worst harms? Or are they fully technically/economically coupled? This feels like a genuine crux; if we end up having the argument and realize that actually, _all_ AI-related harms and goods are intrinsically linked, I strongly suspect many current Safetyists would have stronger grounds for moving towards Abolitionism.

### Are there important goods that are practically inaccessible without sufficiently capable AI?

A possible ground for argument is that there are frontier uses of AI which are just plain out of our reach if we don't deploy stronger models, and it's possible for these frontier uses to be things we want badly enough that we'll accept the inherent trades. This disagreement turns on whether sufficiently capable AI opens up genuinely important outcomes that otherwise remain practically inaccessible. Candidate domains include medicine, biology and physics. If such goods exist and are large enough, they substantially change the abolitionist calculus.

### Can AI reduce friction enough on things we could do without it that it's worth it even if not unique?

Apart from frontier benefits, can AI give us gains in task classes that humans are already pretty good in, but not perfect candidates for automation? Can a human accountant or auditor be 40% more efficient in their work by using AI? If so, that's a possibly non-transformative good we still need to put on one side of the ledger.

### Are there goods AI can produce at a scale that changes the comparison?

Plausibly, there are goods AI can produce so much faster and more efficiently than humans ever could that they produce a qualitative change in how we live. Something that's viewed as rare and precious now, possibly even worthy of rationing, but that AI could help us turn into trivial goods that might be fully available everywhere. If yes, then plausibly it's worth taking some downsides/risks to accomplish those gains.

# Vs. AI Maximalists/EAcc

![A Safetyist debating a Maximalist in front of the gleaming techno-utopian Maximalist city](/static/img/ai-safety-territory/008--safetyist-vs-maximalist.png)

This is, in many ways, the exact opposite set of disagreements between the Abolitionists and the Safetyists. The cruxes might seem like they'd be inversed, but only partially. We should resist the temptation to argue with Maximalists on the grounds of "AI won't do as much good as you think". Grant them any non-infinite amount of good and move forward to the cruxes we care about.

## Does AI have risks, tradeoffs or downsides at all?

The extreme disagreement here is over whether AI has downsides at all. This might be phrased literally as "AI will be smarter than we are, it will make none of our mistakes", or as something more nuanced like "On a long enough timescale, the world will be in a better state than we could imagine despite short term setbacks". The arguments here are going to revolve around

1. whether there is any downside that comes from AI at all that stays with us on the relevant timescale
2. whether there is any downside severe enough that it effectively truncates any timeline. Presumably loss-of-control risks or existential risks fit this bill, so arguments might hinge around how likely and severe we find those risks.

## Can AI be net bad?

Once we're past establishing whether there are downsides at all, the next set of disagreements revolves around whether AI will be net-bad by default. A Maximalist can grant that there are some risks and tradeoffs, but still argue that with no additional Safety input, the effect will be net good, possibly by a wide margin. And, assuming we avoid extinction and loss-of-control, if that's true, a Maximalist position remains substantially intact. Resolving this disagreement means walking down the evidence that AI systems introduce harms which are difficult to control, persistent or large enough to outweigh their benefits, or at least call the comparative weight of the benefits into question.

### Could AI have catastrophic or existential downsides?

My impression of most Maximalists is that they tend to either outright disbelieve existential/catastrophic downsides, or believe that they're extremely unlikely. The fundamental cruxes here are around the likelihood and potential damage of loss-of-control, existential threats and gradual disempowerment scenarios. We don't need to establish that any particular catastrophic outcome is *likely* in order to have this disagreement; we need to establish that there are coherent paths to outcomes bad enough that even relatively small probabilities materially change the ledger. If those paths exist, then "AI will produce enormous amounts of good" isn't by itself enough to settle the question.

### Can individually beneficial uses of AI add up to a collectively bad outcome?

Even if we agree that AI does good on balance, we still need to grapple with the possibility that individually beneficial uses of AI collectively produce harmful equilibria. There can be disagreement over whether this kind of effect can overwhelm the net-goodness of the ultimate outcome.

### Can increasing AI capability increase its downsides as well as its benefits?

One disagreement with the Maximalists once we've gotten past "AI can have any downsides at all" involves situations where increasing AI capabilities naturally also increases AI risk. Cyber-warfare/network intrusion is an example here. We want AIs that are capable in these domains in order to be able to better defend against them, but improving offensive capability also raises the minimum capability required for viable defense, potentially enough to make the overall capability increase net negative. What we want to point to in this particular disagreement is some dynamic in AI capability research/development that, if left unchecked, would not mitigate related risks but scale them along.

# Vs. The Doomers

![A Safetyist debating a Doomer in front of a mixed faction crowd, set in front of a background of the Doomer Village.](/static/img/ai-safety-territory/009--safetyist-vs-doomer.png)

The book [If Anyone Builds, It Everyone Dies](https://ifanyonebuildsit.com/) is sort of an artifact of this mindset, and the predictions are hard to refute if you accept most of their premises.

## Can we mitigate the risks, tradeoffs or downsides at all?

The "easy" disagreements are going to have the shape of "here are approaches we might use to steer AIs better, account for them". There are technical approaches intended to improve alignment, policy interventions intended to reduce risks, and architectural/deployment choices intended to have a positive effect on the impact of AIs in the world.

## Can we mitigate them enough to materially change the outcome?

The "hard" disagreements center around existential risks, gradual disempowerment and loss-of-control risks. These are harder because 

- they haven't happened yet so they're harder to reason about, 
- and it's unclear whether we could mitigate them, 
- and in the absence of mitigation, if we can't also avoid them, they're catastrophic

Which is an end state we're all hopefully trying to avoid. If the Doomers are right about this, it will have major implications when we eventually turn from beliefs to prescriptions.

## Is the development of advanced AI meaningfully steerable?

There's a disagreement on whether we can meaningfully steer greater-than-human-level intelligences at all. This is "alignment research on hard mode". I think this involves the most powerful opposing arguments, because the Doomers will basically be pointing our own anti-Maximalist arguments right back at us. "Here's an example where even a current level AI, with human experts and some controls charged with caretaking, still managed to do strange unaligned things and cause damage in the real world. What makes you think _more_ capable AIs will do less of that?"

### Is catastrophic failure inevitable given sufficiently capable AI?

This is a concrete disagreement worth calling out. Given AGI or ASI of sufficient capability, are there _any_ futures that don't pass through a catastrophic failure? How many of those futures are straightforwardly reachable with no alignment breakthroughs? If the answer is no, and we can't present a coherent story that gets us there, plausibly we should be less bullish on AI Safety as a tractable project.


# Conclusion

So that's roughly the terrain. AI Safety, in the sense I'm using the term here, starts from three fairly minimal beliefs.

![The same old-timey-map-style three point panel from last time, showing the minimal AI Safety beliefs](/static/img/ai-safety-territory/002-ai-safety-points.png)

1. that AI can produce goods worth having, 
2. that it can also produce harms worth taking seriously
3. and that we have enough leverage over what happens next for trying to steer the outcome to be worthwhile. 

Push hard enough against any of those and you start wandering into Abolitionist, Maximalist or Doomer territory. Reject the importance of the whole question and you can stay home with the AI Civilians. None of this tells us yet how well those beliefs survive contact with reality, or how strongly we should hold them. That's the point of the next few posts. First up is the optimistic side of the ledger: **What Good Is AI?** What do we actually get out of this stuff, how valuable could those goods become, and how much would there have to be before an Abolitionist ought to reconsider? I realize it _seems_ like this might be seen as too basic, but I've seen too many throwaway comments online that aggressively push either "AI is all bad all the time" or point to slop as the only possible output. Given that those arguments exist, I feel like I have to put in at least _some_ effort into walking the northern border with the Abolitionists.

As always, I'll let you know how it goes.
