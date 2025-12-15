So a little while ago, [Maas])(TODO - github link) gave a talk at the Toronto AI Safety group meetup presenting [this paper](TODO - the original PBT agent paper). And my _immediate_ thought was "Wait, why haven't you pointed this at OpenSSL already?". The answer turned out to be for some good reasons, some of which are contingent and many of which could succumb to a dedicated engineering effort.

This is a blog post and accompanying talk chronicling the initial engineering effort involved.

The premises are really simple

1. we have bug hunting, LLM-based agents that can detect issues using [PBT strategies](TODO)
2. this strategy can pick low hanging fruit at low to moderate effort (in the original presentation, I remember the final tally being something like "$8 and 10 minutes of GPU time per bug discovered")
3. we have large swathes of open source software underpinning a lot of the modern web stack. This includes things like OpenSSL, the linux kernel, the Aapache and nginx web servers, and a bunch of others

Therefore, we should point the bug hunting agents at the open source project repos. This is a pretty neat public service that would measurably improve the world.

## Why Didn't They Do It

The original paper is predicated on a `claude-code` based setup. Which granted, cool, but the [`claude-code` license](TODO) looks pretty draconian. And according to Claude, Anthropic enforces it. Given that, I'm not about to take the legal risk when I'm already in the position of providing a public service that I don't expect to make any money despite its' utility. This is _not_ a startup, it's a public infrastructure project at best. In principle you could do this with [the Apache licensed `codex`](TODO), but I didn't for reasons we'll get into.

A second reason the original paper isn't trivially pointable at all the OS infrastructure is that the underlying toolkit and prompting is fairly Python-specific. It specifies concrete, python-specific testing steps, and usage of the Python PBT library "Hypothesis". The equivalents in other languages are going to [be different, and have different features](TODO - link to the PBT cross-language comparison table on Giithub).

## Next Steps

So, in my mind, this project is going to do three things.

1. **Reproduce the original paper** - possibly with a model-transient substrate, so we could point it at something other than Claude as the driver
2. **Expand language support** - whether this is some code we need to write or some prompt engineering, give it support for at least NodeJS, C and Perl, and make it expandable so we can throw in support for more languages later. <-- You are here
3. **Deploy It** - stand up a service we can point at arbitrary open source `git` repos, and expose reports to the public (being careful to give maintainers ample notice so we don't accidentally zero-day anything)
4. **Formalize it** - outline a concrete process that points the robot army at some expandable set of projects in the world and keeps an eye on them, re-scanning periodically to make sure we don't see another HeartBleed.

This is ambitious, and we're currently only at step 2, but there's still enough movement to put together a journal of what's happened so far.

## What's Happened So Far

We _could_ have done this with [`codex`](TODO) but at the time I started poking at the project, they had issues pointing the process at anything other than ChatGPT. Not sure if this is still true, but if I try to use a tool, and there's some weird roadblock in the way to using it for the purposes I want, my reflex tends to be to bulldoze through. I'd been working on [a basic AI interaction layer](TODO - link to trivialai) for a while now that would let me formalize and point LLM workflows at arbitrary models. And one of the side objectives here was to burn through a bunch of shortly-expiring AWS credits I had lying around, so _ideally_, I wanted to be able to power whatever agents we built out using [Bedrock](TODO). So, that's the first cut.

- repo is at [`robot-army-for-good`](TODO) - feel free to contribute

- Bedrock has lots of models, but it turns out that large context windows are really useful here. We haven't wired up any vector stores or anything, just slam a starting file and some instructions, along with outputs of model tool calls into a prompt and see what happens. It works surprisingly well. That means I ended up using Claude for a lot of this anyway.
- Models are both really smart and really stupid. There's a few steps I expected to take serious engineering effort that I ended up resolving just by adding a note about it to the prompt, or at worst going "Hey ChatGPT, build <insert thing here>" eg:
     - result tokens are limited and the test suites early models were outputting were getting truncated as a result. This was resolved by adding a note to the prompt saying "Your output tokens are limited, so if you want to write a file bigger than around 4k characters, split it across multiple tool calls and use `append` mode".
  There's also a few things I was expecting to just happen that I had to either prompt deliberately or built code out for eg:
     - originally, the model was expected to set up its' own repo for testing/instrumentation. This would have been really easy, but early logs revealed that lots of extraneous libraries were getting installed, and occasionally we'd get a non-working setup. This led to a situation where we're making a bunch of assumptions on behalf of the model instead of letting it figure things out. Including getting a full virtualenv set up, deterministically setting up a testing environment and deterministically iterating over the source files in the repo (as opposed to just giving the model a clamped `ls` equivalent and letting it pull relevant files as it goes)
- JSON is not as easy to deal with as I was sort of assuming/used to. For some reason, there was a huge number of intermediate failures that happened because we'd get an unparseable response back from the LLM. Typically, it was json-esque, but doing some weird things like
    - returning multiline strings (with or without the Python multi-line string notation)
	- returning Python values for None/True/False
	- returning single-quoted or even bare-word map keys
  the solution was first to move over to [JSON5](TODO) from the default built-in `json` library in Python, and then (as it became obvious that this wasn't any JSON-standard encoding) writing the [`jsonesque` parsing module](https://github.com/inaimathi/trivialai/blob/master/src/trivialai/jsonesque.py) which supports a more general grammar for parsing but still emits standard JSON.
