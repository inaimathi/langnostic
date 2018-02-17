So other than thinking about massively distributed cellular automata, my personal hacking/thinking time has been dedicated to the [`cl-notebook`](https://github.com/inaimathi/cl-notebook) project[^also-i-feel-old].

[^also-i-feel-old]: And I just noticed that I started it in 2014, which puts it at 4 years old, which means that I have both a child younger _and_ a child older than this particular attempt at a cell-based editor.

I recently served on a contract for which my team had to use [Jupyter Notebook](http://jupyter.org/), and I was pretty severely underwhelmed. I mean, the front-end has a level of polish you'd rightly never expect from a [one-dev affair](https://github.com/inaimathi/cl-notebook), and it has better cross-platform support than what I'm pulling off at the moment. But in terms of the big problems that I was tackling in the Common Lisp analog, I'm quite surprised to be ahead of the curve, even after having taken four about four years "off" development. The things I was in the process of thinking about were usage as a multi-user editor, binary generation/deployment, and intelligent full-notebook loading. Not to mention that full-history slider thing that I'd already built.

As far as I can tell, Jupyter has punted on all of the above.

So as much as I'd like to, I can't declare the situation Good Enough that I can in good conscience stop working on `cl-notebook`, even if the underlying languages were equivalent. Given that situation, I've been looking at the codebase and thinking about how I'd go about getting it from where it is now to being a worthy competitor to Jupyter in terms of ease-of-use. Or at the very least, to a state where I can easily start accepting pushes from other contributors.


- Fix `cl-who`-related bugs
- Fix up the UI layout and interaction to a minimally passable level
- Add `markdown` support
- Add support for arbitrarily located notebook files (not just in the internal home directory)
- Get `markdown` export working so I can start blogging from the notebook
- Do some internal file cleanup rather than combining most things into `cl-notebook.lisp`

At that point, I can start using it again, which should percipitate a bunch of new feature work. More importantly though, once I get to _that_ point, I can start honestly promoting this as superior to the alternatives.
