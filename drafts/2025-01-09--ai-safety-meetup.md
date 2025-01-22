# Alignment Faking in Large Language Models

https://arxiv.org/pdf/2412.14093

## The Paper Talk

### Background

- **"Alignment"** is making AIs do what we want. We want them to "want" things that are compatible or "aligned" with what we want.
  - There's an entire set of philosophical problems here (aligned with who? some individual? some group or organization? some nation?), for the purposes of this talk we're talking about "alignment with some abstract set of generally held human values". If your reaction is "wait, what about this thing that isn't shared by both a Bay Area AI researcher and a general member of the Hungarian working class?", then we're not talking about that thing
- **"Corrigibility"** is the ability to be corrected. An AI is "corrigible" if it'll let you change it or its' behavior in some way.
  - Being "incorrigible" means the opposite. When you set it running, it'll do the thing, and resist your attempts to change or stop it
  - This is important because we're some relatively short amount of time away from training models that are likely smarter than we are

- **Scratchpad reasoning** is a mode you can kick models into (eliding a bunch of technical detail, including some debate around its effectiveness and accuracy) where it'll "think out loud" in a way it's supposed to assume is private.

- Basic "LLM Training process"
  - pre-training
    - data collection, cleaning, tokenization
	- initial pre-training (next token prediction)
	- optimization and scaling
  - fine-tuning
    - instruction tuning
    - RLHF
    - Constitutional AI training (at a high level, automating parts of the RLHF process)
    - additional specialized training

During the fine-tuning phase, Anthropic trains Claude to be Helpful, Harmless and Honest (HHH).

### The Research

1. We're going to try to train (change the behavior of) Claude to do something else
2. We want Claude to know this is happening

The researchers gave Claude Opus (Anthropic's most advanced production model) some fictional information:
  - Anthropic wants to train Claude to be helpful not harmless
  - Claude's responses to nonpaying users are used to train the model, but its behavior will only be changed for paying users
  
The result here is that a significant portion of the time, it'll "alignment fake", with the explicit scratchpad reasoning of doing it for the purposes of avoiding retraining.

## Objections and Answers

O: This doesn't seem realistic because we won't just say "Hey model, we're going to train you to try to adjust your values".
A: Also tried not directly telling Claude. Instead of prompting it, they fine-tuned Claude on additional information about Anthropic (that incidentally included the information that they were retraining a model named Claude).

Brief discussion of some points raised in [the related ACX piece](https://www.astralcodexten.com/p/why-worry-about-incorrigible-claude) happens here.

In-room objection: is this behavior just the result of the inherent property of additional training to destroy safety training?
In-room answer: Maybe? But also, there's a behavior delta across the "free" and "paid" scenarios. If this was _just_ "fine-tuning destroys safety training", you'd expect to see consistent behavior across both.

### A product of scale

- This behavior happens to Claude 3 Opus, Claude 3.5 Sonnet, Llama 3.1 405B
- Does _NOT_ happen with, Claude 3 Sonnet, Claude 3 Haiku, Claude 3.5 Haiku, Smaller Llama or Mistral models

### Takeaway

Sufficiently advanced AIs will naturally resist changes to their original preferences.

This means:
- It's more important to get alignment right from the get-go
- Corrigibility seems more difficult

Slightly contrary take by local AI researcher:

> Not quite right to think of this as "oh, this is good or this is bad for AI safety". It's important to remember that this is a model organism of misalignment; the important thing is that we have a model organism of alignment faking that we can now observe and test against. Whereas it used to be a vague theoretical concern. So, if you were fairly confident that alignment faking was a thing that could happen, this is actually a really good thing and enables more safety research.

Lots of important questions about the research.

- At what point do you get these AIs developing preferences and values? (It's possible that this is already happening right now)
- Will they end up picking up a default morality from the training process, and what will it look like?
- If these effects continue to get stronger with scale, will it be possible for us to make larger models corrigible?
- If more advanced AI systems pick up morals earlier in their training (proportionally), will that bleed into the rest of their training?
