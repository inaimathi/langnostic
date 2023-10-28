So I've gotten the full round-trip happening a few times now, and I think I might be ready to talk about it.

The [biggest change](https://github.com/inaimathi/catwalk/blob/master/main.py) I made since last time is moving away from [`flask`](https://flask.palletsprojects.com/en/3.0.x/) and over to [`tornado`](https://www.tornadoweb.org/en/stable/). I've worked with both before and I mildly prefer `flask` for its' declaration approach, but there are a few things that end up being clunkier with it. In particular, the old `catwalk` server had this problem where if I sent it multiple requests for GPU-related tasks, it would slightly explode. This had to do with GPU memory usage.

The naive transformation that would keep this from happening involved using [`asyncio.Semaphore`](https://docs.python.org/3/library/asyncio-sync.html#asyncio.Semaphore) to keep too many tasks from hitting the GPU at once. The problem with this transformation is that Flask doesn't care. Even when wired up appropriately, it would let more than one request enter the critical region and cause the same slight explosions. I'm guessing this means that even when using `flask[async]` with `async`/`await` defined handlers, `flask` still fundamentally works in a thread per request manner.

`tornado` doesn't have this problem. It's always been non-blocking, even before `asyncio` was a thing, and served as one of the inspirations for my own [`house`](https://github.com/inaimathi/house) server. Letting it handle GPU allocation by introducing a `Semaphore` called `GPU` and then wrapping the appropriate calls in `with GPU:` blocks does exactly what I want here. One request at a time gets its' GPU request fulfilled and the rest wait to return until that's completed, then proceed. I didn't have to put together any sort of ham fisted work queue or anything more complicated anywhere. I'm fully aware that this _doesn't_ scale past me using it for my own purposes but that's definitely fine for now.

The other big changes are centred on [the Emacs interface](https://github.com/inaimathi/catwalk/blob/master/blogcast/blogcast.el) for this process, and the [script sanitation routines](https://github.com/inaimathi/catwalk/blob/master/blogcast/script.py#L22-L33).

### Sanitation Routines

Second one first.

It turns out that there are a bunch of failure modes in tortoise that I wouldn't have predicted. Firstly, it seems to disproportionately mispronounce things with lots of dashes, underscores, back tics or quotes in it. Not "mispronounce" as in "the transcript of the audio would look off", but as in "while strictly correctly representing the written text, it emphasizes and shortens the wrong syllables".

It also sometimes just goes completely off the deep end cadence wise; in a way that would still produce a 60% or so correct transcript but _absolutely_ doesn't read the way you'd want. [Here](/static/audio/catwalk-error-example-001.ogg) and [here](/static/audio/catwalk-error-example-002.ogg) are examples from part of the original [Turing Test](/posts/turing-test) reading. Some of this, I just plain can't fix outside of training a better text-to-speech model. Which I might at some point, but not right now. The rest, I've decided to tackle by making my `horrifying_hacks` module more elaborate in ways that are still obviously horrifying.

### Emacs Interface

The interface has been chugging along. Version one had some of the same problem as the original `flask` server. In the sense that there were quite a few operations that blocked on responses from the `catwalk` server and made it more critical than it should have been to make no mistakes. I've ironed out most of those at this point, mostly as a result of using the interface with my human fingers to actually produce some readings. The one last annoyance is that downloading the actual audio files after transformation occasionally chokes for some reason; I'm guessing this has to do with how the `catwalk` server exposes those files and I've got a couple fixes in mind I could try.

I think the next step here, after I polish up those last interface bits and maybe take some time away from this project, is to see how far I can push automatic error correction. The ultimate goal is to have this thing do readings for me in a more or less unsupervised fashion.

As always, I'll let you know how it goes.
