We talked about privacy this week. The turnout was bigger than I'm usually used to seeing, but apparently we've had more traffic over the holidays.

Background observation - it looks like quite a few Toronto developers have been getting laid off recently? By my count, I could put together a pretty competent team from recent such examples.

There's been a format change since I was last there; we now go over the highlights of [Zvi's weekly AI update](https://thezvi.wordpress.com/2024/01/18/ai-48-exponentials-in-geometry/). That link goes to the wordpress site, even though he [also has a Substack](https://thezvi.substack.com/p/ai-48-exponentials-in-geometry), mainly because I've found that classic WordPress sites perform much better in-browser. Also, as a side note, serious props for the amount he manages to write. I don't think I've _ever_ been nearly as prolific.

## Updates from Zvi

You really should just read his update, but the points we commented on are:

- OpenAI is now providing cybersecurity for the pentagon. They're still not building _weapons_, mind you, but their [usage policy](https://openai.com/policies/usage-policies) has been amended to allow for this.
- Phixtral uses the mixture of experts approach to get better performance [out of multiple specialized models](https://huggingface.co/mlabonne/phixtral-4x2_8). On the engineering side, I label this "duh" as a concept, but I'm biased given that the approach rhymes heavily with work I've done in industry.
- Jack Clark [tests the alignment/guard-rails of the Chinese LLM](https://twitter.com/jackclarkSF/status/1746259892053389344). Is it technically good news for alignment that he fails to come up with obvious vulnerabilities?
- [Paul Graham comments](https://twitter.com/paulg/status/1746626025964875965) that adapting well to AI is a huge part of effective coding. The thread goes on into talking about how there are a number of startups looking into replacing programmers with AI "software developers". Having actually seen the code coming out of this process, I remain cautiously but not excessively optimistic about replacing myself with a shell script and an HTTP connection.
- AI Girlfriends are apparently against the ChatGPT terms of service, and they winnow the custom GPT store pretty consistently. I'm reminded of a throwaway comment I heard somewhere that the real killer app is going to be AI _boy_friends and am curious how those products are going.
- Relatedly, [midjourney doesn't like generating fem robots hugging masc humans, but is just fine with masc robots hugging fem humans](https://twitter.com/daniel_271828/status/1746466655918825508). I have some theories on why this is, but they're not testable to me.
- [AlphaGeometry](https://deepmind.google/discover/blog/alphageometry-an-olympiad-level-ai-system-for-geometry/) looks like it's getting to a gold-level Math Olympiad performance. The Base Rate Times reports that this affected markets on AI Math Olympiad performances. It looks like some of those markets have regressed back to baseline. No, I'm not going to link them. Go search [manifold](https://manifold.markets/home), [metaculus](https://www.metaculus.com/ai/) or your prediction market of choice.

Once the crowd got settled in, it was time for The Talk.

## Data Privacy with Generative AI

The TL;DR here is that AI data privacy is going to be a concern in the near past and future. As in, definitely before ASI eats everyone. Full disclosure, this is a talk given by someone currently running a startup in the space looking to address the problem, but it still seems like a problem worth considering.

The speaker is David (who consented to be named and described here), formerly of [Preemptor.ai](https://www.preemptor.ai/), a company that aimed to detect and prevent plagiarism in various academic spaces. I'm not actually clear on whether they're still around and doing their thing or not. His current safety project is [Equo.ai](https://equo.ai/), a company offering guardrails and embeddings for AI developers.

As he sees it, there are two possible failure modes for AI:

1. AI being regulated into oblivion and captured by big player corporations/governments
2. AI having its' own "9/11 moment" (having a bad actor use an AI, or having an autonomous AI agent cause some large profile, visibly damaging event)

Presumably Yudkowski's future fits into possible future #2. Also, at this point, someone in the audience points out that "option #3; both at once" is a real possibility.

Currently, there's an adoption/deployment overhang of possible tools, and he accepts that one thing standing in the way of further deployment is safety. In the sense of "If AIs were genuinely safer, there would be much less active resistance to training and deploying further AIs". One small aspect of this is better guardrails, and better data practices.

According to the [MIT Technology Review Report](https://www.technologyreview.com/2022/09/20/1059630/cio-vision-2025-bridging-the-gap-between-bi-and-ai/), a survey of CIO, CISO and CTOs ranked their concerns

- Data Governance
- ML Infrastructure
- BI/Analytics Infrastructure, Tools
- Democratizing Data

Not being a CIO or CTO, I don't have an opinion on this. David asks: Do companies care about X-Risk? From his perspective, not really. The common documented reaction is either dismissive (nah, that's not too likely) or accusations of fear-mongering (no, that's just an excuse to kneecap the AI industry). Apparently some audience members have similar experience, so I'm not going to spend much energy being skeptical of the point. The picture this paints is that current startup CTOs are much less in line with [Yudkowski](https://www.youtube.com/watch?v=AaTRHFaaPG8) than [Jezos](https://www.youtube.com/watch?v=8fEEbKJoNbU)[^incidentally-there-was-some]. Someone mentioned [the Doomsday Clock](https://thebulletin.org/doomsday-clock/) here, but I don't remember the specific context.

[^incidentally-there-was-some]: Incidentally there was some curiosity at this point in the talk about Beff Jezos and the Effective Accelerationist movement. I don't personally think of them as serious thinkers in this space, but the [appropriate Fridman episode](https://www.youtube.com/watch?v=8fEEbKJoNbU) will tell you a lot about Guillaume Verdon aka Jeff Bezos as a person as well as a small taste of the movement, [Bezos' substack](https://beff.substack.com/p/notes-on-eacc-principles-and-tenets) for a manifesto-ish thing, and [this LessWrong article](https://www.lesswrong.com/posts/2ss6gomAJdqjwdSCy/what-s-the-deal-with-effective-accelerationism-e-acc) for a counterpoint/dissection.

Companies are apparently much more concerned with

- data privacy issues
- hallucinations
- unhelpfulness
- lack of employee training in gen AI

In other words, they care about issues that impact on their specific bottom line. Having been part of many companies at many different headcounts, I am not even slightly surprised by this. From the perspective of a company, the near-term risk of a data breach that might run you up against [PHIPA](https://www.ontario.ca/laws/statute/04p03) or a hallucination that causes your customers to hate you is _much more_ frightening than the possibility of your "tame" AIs bootstrapping to omnicide more than ten years from now. And data breaches of this sort bring us back to the topic of privacy.

A recent example of this, volunteered by a member of the audience, was a "South Korean Chatbot" breach involving an AI chat app that a company use to gather information to train their models on. Those models were then used in other chatbot applications, and the end result was that users of those other applications could get their chatbots to spit out personal information from the training set. I'm not 100% sure which incident this refers to, but given context, I'm guessing it's [this one](https://en.yna.co.kr/view/AEN20210428009552315). I have no idea how good a source that is, sorry.

David points out that one possible way to avoid this is RLHF. That is, we train the AI, have some employees red-team it, and use those interactions to fine tune it such that it respects privacy. This is unreliable for data governance for three reasons:

1. Misalignment _(it's possible that this approach doesn't fully impart the sense of respect for privacy, and results in poor safeguards)_
2. Deceptive alignment _(it's possible for a model to be misaligned and deliberately fail to align. Potential example in the recent [Sleeper Agents paper](https://arxiv.org/abs/2401.05566))_
3. Jailbreaking _(it's possible for a party to exfiltrate data through externally exposed interfaces using various prompt engineering techniques)_

"Jailbreaking" is a word I wouldn't use in this situation myself, but it seems to be the generally accepted nomenclature, so whatever. But it's also doesn't exactly fit the other two? If someone can "jailbreak" an external interface, that implies that you had an alignment failure somewhere, or possibly an input sanitation failure. This might just be old-man talk, but I fundamentally interpret this style of prompt engineering to be something _like_ SQL/script injection attacks, which means that it might in principle be possible to avoid them with proper encoding rather than more training.

At this point we have a tangent on data sanitation in general. Not my doing by the way, a member of the audience mentioned that they work at a company that does work in this space. My understanding is that the company services clients who might be exposing themselves to PHIPA/HIPAA related risks and does old-school auditing/data anonymization. As far as I can tell, they don't currently use AIs for this purpose.


The discussion, as well as the pre-talk Zvi update, took longer than usual, so at this point, we're on speedrun mode. Very quickly; RLHF isn't as effective as it could be, but guardrails are an underexplored approach here. The two variants David works with are

- RAG-based guard rails
- Encoder-level guard rails

The Retrieval Augmented Generation (RAG) approach involves encoding data vectors externally from models, concretely in [vector databases](https://www.pinecone.io/learn/vector-database/) using the same embedding as the model. I think the _primary_ use of this is keeping queries/responses from models compact in the space sense. But another incidental benefit is that we can check whether the model is querying the vector DB for personal data and disallow the response unless the appropriate permissions are in place.

Encoder-level guardrails involve classifying user prompt input. I _think_ the idea here is to get to a situation where we can classify an incoming prompt as a social engineering/prompt engineering attempt and disallow the request before it even gets to the response generation step. The downside would be that we need to train a network on a corpus of prompts (possibly already available?) that would let it differentiate "malicious" from "appropriate" prompts.

These two approaches aren't mutually exclusive; you can have a classifier run on the prompt to knock out some proportion of hostile prompts, and also do the vector query check before responding.

Bam, end of presentation. Check out [Equo.ai](https://equo.ai/) for more information if that sounded interesting to you.

Also, this writeup doesn't do the meme-density of this presentation justice. There were _a lot_ more gigachads, soyjacks and shitty clipart than you could possibly be predicting, even after reading this sentence. It's endearing if you're into internet humor and possibly, as the kids these days say, "cringe" if you're not.

## Unusually Brief Post-talk Chatter

Someone from the audience pointed out that the mix-of-experts model from earlier in the meeting might help out here. You could imagine a setup where you have a set of different models, trained separately on whatever your sensitive data is, and only activate the ones your requester has permissions to use. I'm not too clear on what the use case here is, but it's an interesting, baroque solution so I predict that some company has already deployed it accidentally.

And then we headed for the pub. Which, out of respect for pub talk, I won't talk about here. Except to mention that the discussion touched on [`git-grcypt`](https://github.com/flolu/git-gcrypt), [FALGSC](https://knowyourmeme.com/memes/cultures/fully-automated-luxury-gay-space-communism) and the [DARPA lifelog project](https://web.archive.org/web/20030603173339/http://www.darpa.mil/ipto/Solicitations/PIP_03-30.html).

If you find _that_ interesting, join us next week.
