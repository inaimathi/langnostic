So I'm gonna level with you. I've had a bunch of extra stuff to do lately and haven't been keeping up with my blog writing. Instead of working this into a full blog post, or getting ChatGPT to try to do it for me (something I still haven't satisfactorily mentioned), I'm just going to drop mildly edited notes directly into the published blog. Sorry, and also somehow not sorry? I admit that this is _probably_ worse than taking the time to go through and write full prose, but probably _not_ worse than never publishing it.

Note that I'm a couple weeks behind at this point; I'm posting this one now and possibly another one in the next couple of days.

## Pre-Talk Chatting

- [AI Governance Reading Group](https://www.meetup.com/toronto-effective-altruism-meetup/events/299041832/) Tuesday 27th at 6:30 at the CSI Annex
- basically, an EA-centered, less technical version of this group
- "Everyone who attends will be offered the chance to take on a role in the group..."
- Also, reminder, AI Safety regulars can participate in a Zoom-based coding club Mondays at 6:00  (you'll need to be in the AIGS slack; check in with me if you're interested)

## Zvi's Update

- Gemini Advanced; the new Google model, competitive with GPT-4
  - Sometimes tells you how to do a thing rather than actually doing a thing
  - Possibly just not accessible in Canada? (Nope, accessible in Canada. [Blog post](https://blog.google/intl/en-ca/products/explore-get-answers/gemini-ca/) went up a few hours before the meetup)
- There's been a Deepfake heist.
  - Employee emailed about performing a secret, unusual transaction
  - His fears calmed after a video call with what he thought were various colleagues and the CFO
  - Too bad they were actually deepfakes :|
- Quebec needs an AI law
  - They're particularly concerned about the job market but don't want to slow down innovation
- [Nomic embedding](https://blog.nomic.ai/posts/nomic-embed-text-v1) is a new level of open model
- GPT-4 gives you better responses if you say you'll tip it more? :| (possibly I can get it to do a better job on turning these notes into a blog post if I offer it either $20 or $1M...)

## The Talk - Power Seeking AI

- A variety of methods a power-seeking AI could use to gain more power
- How effective those methods might be
- What steps can we take to reduce their efficacy

Not on today's menu: would an AI become power seeking? Why might it want to power seek?

"Power" is the ability to act or produce an effect. "Power-seeking" is aiming to increase ones' ability to do more things, in particular relative to other actors in a given scenario.

We're _mostly_ talking about autonomous AI agents, but some of this stuff also applies to directed AI.

#### Things to keep in mind

- There is a strong perceived boundary between digital and physical worlds ([Max Tegmark](https://www.amazon.ca/Life-3-0-Being-Artificial-Intelligence/dp/1101946598) gets namedropped by the audience here). It's not necessarily as strong as percieved.
- Getting shut down is the ultimate loss of power for an AI, so a power seeking AI will likely work hard to avoid this outcome
- Power dynamics can be zero-, positive- or negative-sum
  - Zero-sum: a conflict where someone gains at the direct expense of someone else. A classic bet is zero sum; you bet something is true, they bet something is false, the winner gets money from the loser.
  - Positive-sum: a classic peace dividend works here. Two nations/cities/tribes/what-have-you who are at war instead broker peace. Now, neither has to spend on military and can instead focus on infrastructure.
  - Negative-sum: a war of attrition (In the above situation, peace fails, and the sides end up fighting each other. All parties are now worse off. Congratulations)
  
#### Hacking Computer Systems

- Advanced AIs would likely be good at it
  - One of the most common methods to use AIs today is in assisting with coding
  - this involves knowing what is and is not secure code, and possibly influencing users towards one of them
- Could grant access to
  - data and information to inform other plans
  - communication channels to manipulate and persuade

Pub topic: Are models actually getting better at coding? How likely are they to get _much_ better here?

#### Control More Resources

- Compute & digital infrastructure
- Money/crypto (banks are currently not API-friendly, but there are some ways around that. Presumably something like [this](https://learn.e-resident.gov.ee/hc/en-us/articles/360000625098-Why-become-an-e-resident))
- Other
  - electricity/physical materials/political power

#### Run Many Copies

- Key ability: self exfiltration
- Particularly stark advantage over humans
  - Take ~30 years to produce a new human, takes minutes to days to produce a new AI once trained
- Ways to make use of new compute depends on size of AI
- Can multiply other efforts
- Makes shutdown much harder
- AI<->AI alignment caveats:
	- how well can AIs coordinate amongst each other? Realistically, they might not be that good, but also, if there's high variance in their coordination capability, the ones surviving into the deep future are going to be ones that coordinate _really well_. 
	
#### Hire or Manipulate Human Assistants

- For tasks that are difficult/impossible for an AI to do directly
- Any activity that can be offered as a contract service via email/web platform is on the table, given communication access and a method of payment

#### AI R&D

- Humans really want AI to assist with R&D, so it's kind of being trained to do useful stuff in this realm already
- Discover new biological materials
- Improve own algorithm/training process
- Come up with methods to enable other methods we discuss
- Do things that are good for humans so we give it more resources, are less likely to shut it down (the _good_ kind of instrumental convergence)

#### Persuasion and Lobbying

- AIs are rewarded for saying things that people agree with (one aspect of RLHF)
  - Persuasion is very useful for reaching agreement
- There is heavy overlap between skills of persuasion and other relevant skills for AI
- Lobbying is simply persuasion at the political level

> I expect AI to be capable of superhuman persuasion well before it is superhuman at general intelligence, which may lead to some very strange outcomes - _Sam Altman_

I have a lot of thoughts regarding how two entities go about interacting. If a model of reality fits in one of their heads but not the other, it gives that one a lot of advantage in terms of persuasion. But also, how often is it the case that you want someone to do something they don't want for their own good? Possibly the fact that I'm a parent gives me more immediately memory-accessible examples of this, but lets just say I spend _a lot_ of time trying to prevent agents' behavior _in order to keep those agents free from harm_. Pub talk though.

#### Social Engineering

- Could grant access to more human proxies
- Basically, non-technical "hacks" that give the AI power over humans

#### Escaping containment

- Heavy overlap with making copies
- Self-exfiltration is very relevant for closed-source models
- "Access to the internet" (Which all of the interesting models have already to varying degrees. Good job guys.)

#### Manufacturing, Robotics & Autonomous Weaponry

- Lots of discussion happens here regarding how much power humans and machines might share already and how they might go about sharing it and what kind of final outcomes we're likely to see.
- The likeliest outcome seems to be the slow transition (as seen in self-driving cars, chess and go engines)

## Post Talk

Not much post talk, we headed to the pub to follow up on all of the above threads we cut off. If you're interested, come join us next time.
