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

The Talk on Data Privacy with Generative AI (by Dave) (NOTE - check if he's ok with being named/linked to)

- Background on Dave (Peemptor AI)
	- Worked at Preemptor AI. They did a bunch of work detecting plagiarism.
	- Moved to AI safety as it relates to climate change
	- Started EquoAI (with Jreg's participation. He ran as a joke candidate for the mayor of Ottawa. An example plank of his platform was building a wall around Ottawa. He did not win.)
- Current safety project is EquoAI
	- Two possible failure modes for AI:
		1. AI being regulated into oblivion and captured by big player corporations/governments
		2. AI having its' own "9/11 moment" (having a bad actor use an AI, or having an autonomous AI agent cause some large profile, visibly damaging event)
		- Someone in the audience points out that #3 - both at once is a possible future
	- Technological advancements are insufficient to achieve practical AGI. There's an adoption/deployment overhang at the moment
		- Some disagreement from the audience.
- The TL;DR is tried to accelerate deployment with best practices and guardrails
	- Independent research lines up pretty well with "The great accelration" report out of MIT (TODO - get link)
- Survey of CIO CISO and CTOs ranked their options according to the MIT Technology Review Report:
	- Data Gvoernance
	- ML Infrastructure
	- BI/Analytics Infrastructure, Tools
	- Democratizing Data
	(This might not be a relevant chart? but lets see how it fits in)
- Do companies care about X-Risk? Not really. The common reaction is either dismissive or accusations of fearmongering (that is, they don't take X-Risk seriously)
	- e/acc gets name dropped here (link to Beff Jezos and e/acc stuff here if people are interested)
	- audience mentions the [Doomsday clock](https://thebulletin.org/doomsday-clock/)
- Risks that companies _ARE_ concerned with:
	- data privacy issues
	- hallucinations
	- unhelpfulness
	- lack of employee training in gen AI
	they care about issues that impact on the bottom line. This includes things like data breaches, which brings us back to data privacy.
- Audience member volunteers "South Korean Chatbot" breach from a bit ago. They used data gathered from a romantic partner chat, which got leaked as part of another AI chat product (apparently they trained on their chat data, and some of that private data was extracted through prompt engineering)
	- A possible way to avoid this is using RLHF to impart a sense of respect for privacy. It's unreliable for data governance for three reasons
		1. misalignment (it's possible that this approach doesn't fully impart the sense of respect for privacy)
		2. deceptive alignment (it's possible for a model to be misaligned and deliberately fail to align. Example in the recent Sleeper Agents paper)
		3. jailbreaking (it's possible for an external party to exfiltrate data through externally exposed interfaces. Not necessarily just in OpenAI style interfaces; it's also possible for internal data compartmentalization to be broken this way)
	 - Semi-long tangent on data sanitation and how to go about using synthetic data in place of actual personal data (one of the audience members works for a company that currently does this in different contexts)
At this point, we're on speedrun mode. So, quickly, RLHF isn't as effective as it could be. So, in particular we might exlpored guardrails
	- RAG-based guardrails
	- Encoder-level guardrails
- RAG = "Retrieval Augmented Generation"
	- The idea is that we encode data vectors externally from models (in a vector store external from the model itself), we can then check whether personal information is being queried as a result of a prompt response being generated and if it is, disallow that response.
- Encoder-level guardrails involve classifying user prompt input. The idea is that we can flag inputs that don't conform to terms of service and just ignore those requests. This basically adds a second pair of eyes to flag prompts that might run counter to our TOS never gets to the response generation stage
	- The downside here is that we need to train a network on a corpus of prompts (possibly already available? The idea is that we'd want to flag social engineering attempts)

equo.ai

The writeup doesn't do the meme-density of this presentation justice. There were _a lot_ more gigachads, soyjacks and shitty clipart than you could possibly be predicting here.

QA session
- Mix of experts model might help here? Have a set of different models and only activate the ones that the given user has permissions for. This might be useful for larger companies that have existing data access policies.

There are going to be _a lot_ of discussions here that we left out and will be addressed at the pub. I might blog about some of them at some point.

- GcryptGIT
- fully automated luxury gay space communism
- DARPA lifelog project
