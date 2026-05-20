So a little while ago, [Maaz](https://github.com/mmaaz-git) gave a talk at the Toronto AI Safety group meetup presenting [this paper](https://arxiv.org/pdf/2510.09907). And my _immediate_ thought was "Wait, why haven't you pointed this at OpenSSL already?". The answer turned out to be for some good reasons, some of which are contingent and many of which could succumb to an engineering effort.

This is a blog post and accompanying talk chronicling the initial engineering effort involved.

The premises are really simple

1. we have bug hunting, LLM-based agents that can detect issues using [PBT strategies](https://en.wikipedia.org/wiki/Property_testing)
2. this strategy can pick low hanging fruit at low to moderate effort (in the original presentation, I remember the final tally being something like "$8 and 10 minutes of GPU time per bug discovered")
3. we have large swathes of open source software underpinning a lot of the modern web stack. This includes things like [OpenSSL](https://github.com/openssl/openssl), the [Linux kernel](https://github.com/torvalds/linux), the [Apache](https://github.com/apache/httpd) and [nginx](https://github.com/nginx/nginx) web servers, and a bunch of others

Therefore, we should point the bug hunting agents at the open source project repos in order to find any bugs we can. This is a pretty neat public service that would measurably improve the world.

## Why Didn't They Do It

The original paper is predicated on a `claude-code` based setup. Which granted, cool, but the [`claude-code` license](https://code.claude.com/docs/en/legal-and-compliance) looks pretty draconian. 

![The Anthropic business license which binds any use of claude code](/static/img/robot-army-for-good/anthropic-license.png)

And according to Claude, Anthropic enforces it. Given that, I'm not about to take the legal risk when I'm already in the position of providing a public service that I don't expect to make any money despite its' utility. This is _not_ a startup, it's a public infrastructure project at best. In principle you could do this with [the Apache licensed `codex`](https://github.com/openai/codex), but I didn't for reasons we'll get into.

A second reason the original paper isn't trivially pointable at all the OS infrastructure is that the underlying toolkit and prompting is Python-specific. It specifies concrete, python-specific testing steps, and usage of the Python PBT library [Hypothesis](https://github.com/HypothesisWorks/hypothesis). The equivalents in other languages are going to [be different, and have different features](https://github.com/jmid/pbt-frameworks?tab=readme-ov-file#framework-functionality).

## Next Steps

So, in my mind, this project is going to do four things.

1. **Reproduce the original paper** - ideally with a model-transient substrate, so we could point it at something other than Claude as the driver
2. **Expand language support** - whether this is some code we need to write or some prompt engineering, give it support for at least NodeJS, C and Perl, and make it expandable so we can throw in support for more languages later. <-- You are here
3. **Deploy It** - stand up a service we can point at arbitrary open source `git` repos, and expose reports to the public (being careful to give maintainers ample notice so we don't accidentally zero-day anything)
4. **Formalize it** - outline a concrete process that points the robot army at some expandable set of projects in the world and keeps an eye on them, re-scanning periodically to make sure we don't see another [HeartBleed](https://www.heartbleed.com/).

This is ambitious, and we're currently only at step 2, but there's still enough movement to put together a journal of what's happened so far.

## What's Happened So Far

We _could_ have done this with [`codex`](https://github.com/openai/codex) but at the time I started poking at the project, they had issues pointing the process at anything other than ChatGPT. Not sure if this is still true, but if I try to use a tool, and there's some weird roadblock in the way to using it for the purposes I want, my reflex tends to be to bulldoze through. I'd been working on [a basic AI interaction layer](https://github.com/inaimathi/trivialai) for a while now that would let me formalize and point LLM workflows at arbitrary models. And one of the side objectives here was to burn through a bunch of shortly-expiring AWS credits I had lying around, so _ideally_, I wanted to be able to power whatever agents we built out using [Bedrock](https://aws.amazon.com/bedrock/). So, that's the first cut.

![AWS cost summary for this month](/static/img/robot-army-for-good/aws-cost-this-month.png)

The repo as it currently stands is at [`robot-army-for-good`](https://github.com/inaimathi/robot-army-for-good?tab=readme-ov-file#robot-army-for-good), and the underlying LLM interaction framework is over at [`trivialai`](https://github.com/inaimathi/trivialai). Feel free to contribute to both.


We went with Bedrock as the LLM provider for the probably pretty common reason of "we have credits and they're expiring soon, so lets do this". Bedrock has lots of models to choose from, and I settled on Claude 3.5 for this project. This is because it turns out that large context windows are really useful here, and Claude 3.5 is the sweet-spot of cost-per-1k-tokens vs size-of-context-window. There are cheaper models that could probably still handle the lift, but we might need to build out more attention scaffolding for them and they might still fail. And there are more recent Claude versions that are definitely up to the task but cost slightly more. Because of `trivialai`, we can _trivially_ switch between them down the road.

Or, semi-trivially I guess. I _did_ try this process out with a local [`qwq`](https://huggingface.co/Qwen/QwQ-32B) instance, and it had significantly worse results, so I'm not at all confident that we could run this with smaller models without significant engineering input. It might still be worth it to do that work, but it's not trivial in the way that switching between the frontier models is. The current routine doesn't involve any vector stores or anything, just slam a starting file and some instructions, along with outputs of model tool calls into a prompt and see what happens. Hence the large context window helping, and possibly also hence smaller models providing worse results.

Models in general, and Claude 3.5 specifically, are both really smart and really stupid simultaneously. There's a few steps I expected to take serious engineering effort that I ended up resolving just by adding a note about it to the prompt, or at worst going "Hey ChatGPT, build <insert thing here>". For example: result tokens are limited, and the output files early models were outputting got truncated as a result. Which resulted in parse failures and errors in various sanity-checking steps along the way; no incorrect outputs made it to disk, but it's still annoying to fail out on an otherwise good approach by trying to write a file that's too big all at once. This was resolved by adding a note to the prompt saying "Your output tokens are limited, so if you want to write a file bigger than around 4k characters, split it across multiple tool calls and use `append` mode". I've had zero problems with this so far.

At the other end of the scale, no amount of prompt engineering would make Claude 3.5 consistently emit properly structured JSON. It would insist on adding newline-containing strings, Python-style triple-quoted strings, Python-style `None` and boolean values, and a few other eccentricities. Because of how _much_ python was slipping in, I couldn't just move over to [JSON5](https://json5.org/).In the end, the low-friction approach was to just write a parser for a Python/JSON pidgin notation. It's over in the [`jsonesque`](https://github.com/inaimathi/trivialai/blob/master/src/trivialai/jsonesque.py) module. As an aside, my intent here was to use [PEG](TODO)/[parsimonious](TODO) to make this a pretty, minimal parser. But it turns out that there's a few features in this particular parse tree that prevent a straightforward PEG grammar for handling it. I'd still need to write a node-visitor that handled things like ensuring hashable keys in objects, and certain edge cases for newline handling. At that point, PEG is basically saving us on the order of 10 lines on the alternative so I chucked it.

Similarly, the plan was to have the model handle a lot more of the surrounding ops processes. Basically, I was expecting to be able to defer repo preparation. Just point an agent at a directory and have it do its' thing.

  There's also a few things I was expecting to just happen that I had to either prompt deliberately or built code out for eg:
  - originally, the model was expected to set up its' own repo for testing/instrumentation. This would have been really easy, but early logs revealed that lots of extraneous libraries were getting installed, and occasionally we'd get a non-working setup. This led to a situation where we're making a bunch of assumptions on behalf of the model instead of letting it figure things out. Including getting a full virtualenv set up, deterministically setting up a testing environment and deterministically iterating over the source files in the repo (as opposed to just giving the model a clamped `ls` equivalent and letting it pull relevant files as it goes)

## High Level Lessons

There's an old comp sci course kicking around the internet in which Hal Abelson introduces Computer Science as being neither "about Computers" nor "a Science" by analogy to Geometry which isn't _really_ about "the Earth" and it's not really about "Measuring instruments". In the same way that Geometry is about formalizing certain kinds of spatial knowledge, he says Computer Science is about formalizing certain kinds of imperative knowledge. About the processes which we use to compute certain things. 

- There's places 
