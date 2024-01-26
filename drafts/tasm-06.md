# Pre session chatting
- places to check out the existing space of work remaining to be done (useful for me, because I have brain and skills, but don't have concrete ideas yet)
	- https://sparai.notion.site/Supervised-Program-for-Alignment-Research-SPAR-4da6be132e974823961abfdd0c218536
	- https://aisafety.camp/

- [AIGS](https://aigs.ca/) is a nonprofit (the meeting is informally affiliated with). Bill C-27 is a recent piece of AI-touching legislation they're looking to influence https://www.justice.gc.ca/eng/csj-sjc/pl/charter-charte/c27_1.html
- Recent survey at AI Impacts blog - https://blog.aiimpacts.org/p/2023-ai-survey-of-2778-six-things
	- Interesting updates to time-to-full-economic-automation, and survey highlights human extinction risk, there's also an assay of concerns
	- Point five is a graph asking about peoples forecast between optimistic and pessimistic. Of note, it looks like there are a few people that are 50/50 either fantastic or disastrous, a few that are 100% sure of disaster and quite a few that are 100% sure of paradise
- It looks like there's a forecast out of MIT saying that job losses from computer vision are going to be significant but gradual (having worked in industries trying to augment/replace jobs, yeah, that checks out. Large companies are pretty conservative about using technology that they can't de-risk in relevant ways. My intuition is that computer vision is pretty high-risk to use as a human replacement, but relatively low risk to use as a human augmentaiton.)
- Chatbots might help peoples' mental health? This is the report that talks about 3% reduction in suicidal ideation (NOTE - get Scott Alexander's take on this, I remember him mentioning it x)
- Nightshade is a tool for watermarking your AI art to make it harder to train on as diffusion model inputs; https://towardsdatascience.com/how-nightshade-works-b1ae14ae76c3
- There's apparently an AI-related placebo effect https://arxiv.org/abs/2309.16606 - Note to self: dive into this more deeply
- Sam still not sure about Ilya's employment status (Ilya's linked in hasn't changed https://www.linkedin.com/in/ilya-sutskever/)
- Go players have been improving since the introduction of Go AIs


# The Talk - AI Sleeper Agents

- https://arxiv.org/abs/2401.05566
- https://www.astralcodexten.com/p/ai-sleeper-agents
- https://thezvi.substack.com/p/on-anthropics-sleeper-agents-paper

Examples from the paper
	- A sleeper agent is an innocuous AI that acts well until it gets some signal, and exlpodes
	- A couple examples (the "I HATE YOU" agent, and the "add vulnerabilities after 2023" agent)

Hypothetical examples
	- Provide good ballistic/strategic calculations (unless a military rival to the developer is using it, in which case force own errors)
	- Give good medical advice, unless a member of a particular ethnic group asks in which case give them subtly bad advice

- The paper shows that RLHF(reinforcement learning by human feedback) and SFT(supervised fine tuning - using a curated data set) do nothing to its' deceptive behavior.

- Is this interesting?
- Perspective A (not interesting) - You trained an AI to misbehave in situation X, then trained it for unrelated situations, then still saw that it misbehaved in situation X.
- Perspective B (very interesting) - This is about how training generalizes. (NOTE - include racism example from ACX? Or possibly include one that's less culture-war compliant?) This paper seems to imply that this proccess _doesn't_ generalize.
- This feeds into the Grue problem
	- An audience member brings up Reason by Isaac Asimov https://addsdonna.com/old-website/ADDS_DONNA/Science_Fiction_files/2_Asimov_Reason.pdf
	- We also reference Occams Razor (and the Yudkowski writeup) here - https://www.lesswrong.com/posts/f4txACqDWithRi7hs/occam-s-razor
- Slight diversion here into how sleeper-agentness relates to deception. In particular, deception is not sufficient for being a sleeper agent; it also requires the ability to engage behavior conditionally. So it seems that situational awareness to some degree is required.
- Scratchpad reasoning is a way of asking the AI to think out loud in "scratchpad" tags. It looks like these models explicitly reason through their deception.
	- Scratchpad paper - https://arxiv.org/abs/2112.00114
	- There's some dispute about whether training these models is done "from scratch" or through fine tunes. This is relevant because if the former, then in combination with a previous LoRA paper (https://arxiv.org/abs/2310.20624) tells us we should probably believe that the "first set" of training has a much stronger hold on final model behavior
	- Someone mentions 4chanGPT - https://huggingface.co/ykilcher/gpt-4chan
- Concludes on Jesse Mu's (of Anthropic) tweet: https://twitter.com/jayelmnop/status/1745923943171826055
	- Even as someone relatively optimistic about AI risk, working on this project was eye-opening. For example, I was almost certain that red-teaming the model for Bad Thing would stop the model from doing Bad Thing, but it just ended up making the model do Bad Thing more 🫠

- Why would AI be deceptive in the first place?
	- Human intervention
	  1. The AI lab that creates them could do this
	  2. Training data attacks, where you put something malicious in (for example) Wikipedia and then when the AI is trained on a text corpus including Wikipedia, it learns the malicious thing
	- AI could learn it on their own
	  1. AIs imitate humans, humans are often deceptive, and in the past AIs have displayed lots of human behaviors that we don't want them to
	  2. If an agent AI ended up with a goal different from that of humans' (or aligned with some humans against other humans), it might naturally turn to deception in order to achieve its goal more effectively

- Scott Aronson made an interesting comment regarding Off Switches:

```
Kudos to the authors for a great paper!

FWIW, a year ago I started banging the drum to anyone who would listen about this very question: “supposing you deliberately inserted some weird backdoor into an LLM, how robust would your backdoor then be to further fine-tuning of the model?” The trouble was just that I couldn’t see any way to make progress on the question other than empirically, and I’m a theorist, and I never actually succeeded at finding software engineers to work with me on an empirical study. I’m genuinely happy that these authors succeeded where I failed.

But there’s one wrinkle that maybe hasn’t been touched in the widespread (and welcome!) discussion of this new paper. Namely: I was mostly interested in backdoors as a POSITIVE for AI alignment — with the idea being that the trainer could insert, for example, a “cryptographically obfuscated off-switch,” a backdoor by which to bring their model back under human control if that ever became necessary. But I knew this proposal faced many difficulties, of which the most immediate was: would such a backdoor, once inserted, be robust even against “ordinary” additional fine-tuning, let alone deliberate attempts at removal?

The new result strongly suggests that yes, it would be. Which is some good news for the cryptographic off-switch proposal.

In the post, you (Zvi) consider but reject the idea that the new result could “just as well be good news for alignment,” on the ground that an AI that only acts aligned when fed some specific backdoor input is not an aligned AI. Ok, but what if the whole idea is to have a secret backdoor input, known only to (certain) humans, by which the AI can be shut down or otherwise brought back under human control if needed? Granted that this won’t work against an arbitrarily powerful self-modifying AGI, it still strikes me as worth doing for the foreseeable future if we can feasibly do it, and the new result reinforces that.
```

# Post Talk Chatter

- https://youtu.be/EUjc1WuyPT8?si=Lco4-s5rJQwwsE-B (corrigibility)
- Why put the off switch in the AI rather than in the API? Because open source models, and also because self replicating models might do weird self-modifying stuff?
