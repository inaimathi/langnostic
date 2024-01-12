Welcome to the year 2024. This is just a small holding-pattern post to make sure I don't lose touch with the reflection side of my programming process again.

It feels _really_ weird to have been doing active development for [this](https://github.com/inaimathi/catwalk/tree/master) [long](https://github.com/inaimathi/todotree) in something non-lispy. Although, to be fair, that first one _does_ have an [`elisp` module](https://github.com/inaimathi/catwalk/blob/master/blogcast/blogcast.el) I guess. My reasoning was:

1. I want to do stuff with [transformer models](https://huggingface.co/docs/transformers/index)
2. There's extensive and mainstream support for said models in Python, despite some movement on the JVM
3. I'm going to want to put together some Android/otherwise mobile front-ends for what I've got in mind, and Python also [has support for that](https://kivy.org/)
4. So it would just complicate things to put some lisp-based server in the middle of this, just do all of it in Python

So far? No regrets. Given my current understanding of transformer models, I don't really see why Python is a _necessary_ component on any level, but whatever, it's not awful and I'm not about to go back and re-invent this much infrastructure. Kivy is not my cup of tea UI-wise, and if the goal was just getting cross-platform, non-mobile GUI work done, I think I'd still reach for [seesaw](https://github.com/clj-commons/seesaw), but the difference between writing an API server with [`tornado`](https://www.tornadoweb.org/en/stable/) and writing one with [http-kit](https://http-kit.github.io/) is close enough to trivial that I don't particularly care.

Over the holidays, I've gotten [a preliminary Android app](https://github.com/inaimathi/todotree) to the point where I can use it for my own minimal purposes, and I've been plugging away at the boring work of making [`blogcast`](https://github.com/inaimathi/catwalk/tree/master/blogcast) more and more automated. It's gotten to the point where generating the audio versions of these blog posts takes me on the order of a half hour of human interaction time once the text is online. I'm _hoping_ to push it up to fully automated relatively soon, but that's likely going to involve taking a quality hit. The short term plan is to get a web UI running for it, and possibly a better job abstraction.

That's basically it; no big progress otherwise. There's a bit of other work I've been doing on the side for fun that I'll talk about when it's actually up and running for a demo or two.

As always, I'll let you know how it all goes.
