Just some quick housekeeping while I draft up my next proper piece.

### <a name="clcwd" href="#clcwd"></a>cl-cwd

A friend of mine mentioned that he had to hack up `cl-git-fs` to make it export `cwd` and `with-cwd`. Which was surprising when I heard it, but really shouldn't have been. This is the sort of problem Common Lispers seem to solve by copy/pasting a few implementation-specific snippets into projects that need the functionality. That's not good enough for me. So, here's [`cl-cwd`](https://github.com/Inaimathi/cl-cwd); a library to get/manipulate/temporarily manipulate your current working directory in a cross-platform and cross-implementation way. Patches and bug reports welcome as always, and hopefully it's useful for you. I *haven't* gotten around to hacking the same components out of `cl-git-fs`, but I might eventually. Or, I might just use the `os` section of [UIOP](http://quickdocs.org/uiop/), which was only pointed out to me after I posted that `cl-cwd` repo. I'm not entirely sure what the right approach here is; are monolithic utility libraries preferable to very small, single-purpose packages? Not sure, but I kind of prefer the smaller, specific ones for my own purposes. Even though it never wastes enough resources to matter to me, it's deeply irritating on some level to include **all** of [`:alexandria`](http://www.cliki.net/Alexandria) and [`:anaphora`](http://www.cliki.net/Anaphora) for, essentially `with-gensyms`, `aif`, `awhen` and `alambda`.

### <a name="factbase-updates-and-relevant-minutia" href="#factbase-updates-and-relevant-minutia"></a>`fact-base` updates and relevant minutia

I've gotten some work done on [`fact-base`](https://github.com/Inaimathi/fact-base), which on reflection really should have been called `cl-triple-store`, with an eye on using it in production. We'll see if it actually happens, or if it just remains an idle fancy, but


1.   you can now add `index`es to your fact bases, which should make data retrieval faster<a name="note-Thu-Mar-20-195226EDT-2014"></a>[|1|](#foot-Thu-Mar-20-195226EDT-2014)
1.   writing deltas is now quite efficient<a name="note-Thu-Mar-20-195230EDT-2014"></a>[|2|](#foot-Thu-Mar-20-195230EDT-2014)


Again, let me know if it's useful, or if it's sooooo-close-to-being-useful-if-it-only-had-this-one-feature. I might do something about it at this stage.

The only related piece of minutia is that I've found myself reading a lot about [miniKanren](http://minikanren.org/), [core.logic](https://github.com/clojure/core.logic) and [logic programming](https://en.wikipedia.org/wiki/Logic_programming) in general. I *think* it might be a really good way to query structures like these triple-stores I've been building lately. Also, it turns out [someone's already done most of the work of implementing that in CL](http://common-lisp.net/project/cl-kanren-trs/) for me, so I [forked it](https://github.com/Inaimathi/cl-kanren-trs) and added/changed the few chunklets I needed to. Matthew Swank, if you're reading this and care, let me know and I'll send you patches. I assumed you wouldn't care, since the copyright line said `2008`, but I might have been wrong.

### <a name="editors" href="#editors"></a>Editors

Someone finally sat down and walked me through the installation for [Light Table](http://www.lighttable.com/)<a name="note-Thu-Mar-20-195232EDT-2014"></a>[|3|](#foot-Thu-Mar-20-195232EDT-2014). I haven't been paying attention to the propaganda, so I'm not entirely sure exactly how this is going to revolutionize editing<a name="note-Thu-Mar-20-195235EDT-2014"></a>[|4|](#foot-Thu-Mar-20-195235EDT-2014), but it looks promising for a prototype. I was able to get to a productive-ish point with it within about an hour, and that's a high bar. I remember learning Emacs<a name="note-Thu-Mar-20-195237EDT-2014"></a>[|5|](#foot-Thu-Mar-20-195237EDT-2014) over the course of *weeks*.

Another one I tried enough to appreciate is [IPython Notebook](http://ipython.org/notebook.html). The idea of a browser-based editor has, shall we say, [crossed my mind](http://langnostic.blogspot.ca/2014/01/update-and-finer-points-of-quasimodes.html), but the idea of applying it to 1D coding hadn't occurred to me. I gotta say, I like the idea, and I'll be trying to do something about that right [here](https://github.com/Inaimathi/cl-notebook). I've only got the easy part done so far; a front-end of [code-mirror](http://codemirror.net/) rigged up to a Lisp process that evaluates things and sends the result values back<a name="note-Thu-Mar-20-195240EDT-2014"></a>[|6|](#foot-Thu-Mar-20-195240EDT-2014). The hard part is going to involve a persistence layer, multiple cells, multiple notebooks and probably some smaller browsing/presentation/generation features to let me do whatever<a name="note-Thu-Mar-20-195242EDT-2014"></a>[|7|](#foot-Thu-Mar-20-195242EDT-2014). Spoiler warning: [`fact-base`](https://github.com/Inaimathi/fact-base) and its history/indexing operations will feature prominently. The ultimate goal is no less than replacing Emacs as my Common Lisp IDE of choice.

And that's sort of why I've been on another editor/tools kick lately.

I've been talking to some friends in the local programming community, and we seem to have collectively decided that we really, *really* want Light Table to succeed because it sucks not being able to recommend anything as a starting tool for Common Lisp development to programming newbies. Our options right now are what can charitably be referred to as [a very intimidating operating system/rss aggregator/IRC client/moustache trimmer](https://www.gnu.org/software/emacs/)<a name="note-Thu-Mar-20-195245EDT-2014"></a>[|8|](#foot-Thu-Mar-20-195245EDT-2014), [a *wannabe* very intimidating blah blah blah](https://www.eclipse.org/), [a very pretty, passably comfortable pair of handcuffs](http://www.sublimetext.com/), [a very performant, robust, ridiculously expensive pair of handcuffs](http://www.lispworks.com/), and [vim](http://stackoverflow.com/questions/94792/using-vim-for-lisp-development). If you're recommending Scheme, there's also [Racket](http://www.racket-lang.org/)<a name="note-Thu-Mar-20-195247EDT-2014"></a>[|9|](#foot-Thu-Mar-20-195247EDT-2014), but that still doesn't count as a Common Lisp environment.

Any or all of those are things I'd recommend to people who are already programmers. One of them is something that I'd recommend to people looking to learn languages for the sake of learning them. But there isn't a tool anywhere on that list that compares to something like [Idle](http://docs.python.org/2/library/idle.html) or, more to the point, [Light Table](http://www.lighttable.com/). I don't think it's quite at the point where I'd switch over to it myself, but I sincerely hope it gets there. And I'll be devoting some effort to pushing that along. You can too, [since it's up](https://github.com/LightTable/LightTable) and [properly licensed](https://github.com/LightTable/LightTable/blob/master/LICENSE.md#gnu-general-public-license) and all...


* * *
##### Footnotes

1 - <a name="foot-Thu-Mar-20-195226EDT-2014"></a>[|back|](#note-Thu-Mar-20-195226EDT-2014) - There's an article either in the works or already posted, depending on what order I decide to put these up.

2 - <a name="foot-Thu-Mar-20-195230EDT-2014"></a>[|back|](#note-Thu-Mar-20-195230EDT-2014) - From a macro-optimization perspective; I'm sure I could pull some micro-optimizations on it too, but I'm not into that.

3 - <a name="foot-Thu-Mar-20-195232EDT-2014"></a>[|back|](#note-Thu-Mar-20-195232EDT-2014) - Turns out that even the [Debian Unstable](https://packages.debian.org/sid/leiningen) repo doesn't have a recent enough version of [Leiningen](https://github.com/technomancy/leiningen). I needed to latest using [this](https://raw.github.com/technomancy/leiningen/stable/bin/lein).

4 - <a name="foot-Thu-Mar-20-195235EDT-2014"></a>[|back|](#note-Thu-Mar-20-195235EDT-2014) - In fact, the only visible revolutionization of "randomly putting shit in your buffers that you can't edit instead of giving you a goddamn REPL" is profoundly annoying, though it's possible to disagree about these things. I guess I can imagine it looking really snazzy in presentations and demo videos.

5 - <a name="foot-Thu-Mar-20-195237EDT-2014"></a>[|back|](#note-Thu-Mar-20-195237EDT-2014) - ...and before that jEdit, and before that Eclipse.

6 - <a name="foot-Thu-Mar-20-195240EDT-2014"></a>[|back|](#note-Thu-Mar-20-195240EDT-2014) - And yes, it handles multiple values as well as `*standard-output*` emissions, in case you were wondering.

7 - <a name="foot-Thu-Mar-20-195242EDT-2014"></a>[|back|](#note-Thu-Mar-20-195242EDT-2014) - At minimum the possibility to write `:cl-who`/`:cl-css` instead of markdown, a way of installing libraries that doesn't go through quicklisp, and maybe some git/[drakma](https://github.com/edicl/drakma)/shell support. I may end up having to add non-shitty static file serving to `house` for this...

8 - <a name="foot-Thu-Mar-20-195245EDT-2014"></a>[|back|](#note-Thu-Mar-20-195245EDT-2014) - [Pre-built Lisp IDE version](http://common-lisp.net/project/lispbox/) optional.

9 - <a name="foot-Thu-Mar-20-195247EDT-2014"></a>[|back|](#note-Thu-Mar-20-195247EDT-2014) - Which I do wholeheartedly recommend; it's very cool.
