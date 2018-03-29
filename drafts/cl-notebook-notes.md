So the previous article I posted got picked up by the Common Lisp community quicker than I'd thought. My plan was to finesse a few things about [cl-notebook](https://github.com/inaimathi/cl-notebook) over the next few weeks, _then_ start promoting. Preferably by doing a talk or two using it as the presentation/live-hacking substrate. But given that the eye of other developers is now on me, it's go time.

There's a few things I've already learned since restarting development on this project, and some of it bears sharing.

## Common Lisp has a `bundler` analogue

I've been using [`quicklisp`](TODO) basically since `quicklisp` has been a thing, because the alternative is installing mosnsters like [`hunchentoot`](TODO) by hand, and I _never_ had _that_ kind of time. Even when I was a relatively carefree university student. The one downside I've seen here in Common Lisp land compared to languages like [Ruby](TODO - bundler) or [Python](TODO - virtualenv) is a locally-versioned project tree. I mean, ideally [`nix`](TODO) or [`something like it`](TODO - guix) would get off the ground consistently and become the general standard package management _thing_[^and-if-im-being-honest] for every language and OS alike, and then we wouldn't have to worry about this at the individual language level. But in the absence of that, it would still be really nice to not worry about what happens when different projects I'm working on demand different versions of the same prerequisite.

[^and-if-im-being-honest]: And if I'm being honest, I'm really tempted to write a language or [variant](TODO - experimentalisp) that hooks into the `nix` infrastructure for testing and package distribution, just to see whether it could be done in a quasi-sane way.

[`qlot`](TODO) is basically that, for Common Lisp. It lets you manage local `quicklisp` client repositories in a way that lets certain projects be effectively isolated, in dependency terms, from the rest of your running system. I'm seriously thinking about using this heavily in `cl-notebook`[^already-actually-done], both to isolate the main notebook `quicklisp` stack from whatever base one might already exist on the substrate machine, and later to make sure that each notebook is similarly isolated.

[^already-actually-done]: I'm, in fact, [already using this](TODO - github commit link) in `cl-notebook`. The per-notebook environment is still pending some thought though.

## Columns are now an actual thing in CSS

[This](TODO - link to the columns article) surprised the ever-loving shit out of me. I grew up as a web developer back in the bad ol' days of IE6-8, when Firefox was just getting back in the game, and Google Chrome was still probably someones' 10% project. So I'm used to having to cobble together any meaningful layout myself from `div`s, `float` declarations, shoelaces and duct tape. Between `bootstrap` and new `CSS3` features like that adaptive columns thing, it looks like front-end developers are now living in a world of comparative opulence[^which-mildly-surprises-me]. It's almost enough for me to consider picking it up again exclusively for another few contracts.

[^which-mildly-surprises-me]: Which mildly surprises me, because outside of [`clj-android`](TODO), modern _mobile_ development is a shit-stained mire of ass and fail. From what I've seen so far, it's directly worse in every fucking way than deskop development, and is far more hack-intensive than the worst of front-end development history I've personally witnessed. But I digress.

Almost.

## Notebook itself

The [`cl-notebook`](TODO) project is going pretty well. It's at the arduous beginning stage where I've still got to get used to using it consistently. I'm ashamed to say that this blog post is _not_ being written in it. But I am trying to train myself to use it for most of my development tasks. The hardest part is sitting down and actually thinking through a worthwhile modification when I stub myself on the odd corner.

One of the things I wanted to do for a recent presentation is to get definition hooks for front end components. Because `notebook`s are Lisp code, and [`cl-notebook`](TODO) is a Lisp application, you can already define additional back-end things. In particular, the bare-bones charting system is currently fully implemented in a `notebook`, and not the main codebase. This is a trend I aim to continue; provide piecemeal functionality in the form of additional config notebooks that can be loaded individually for specific purposes.

The current implementation of these front-end hooks is a new `cell-type`; `parenscript` whose result gets compiled down to JS by a call to the `parenscript` library, then evaluated as client-side JS by the front-end. This mildly complicates things with front-end state, but I think the flexibility is well worth the cost, since it effectively lets me use `cl-notebook` as a platform for [HTML-game development](TODO - itch.io). Speaking of, the [Lisp Game Jam](TODO) is going on later in April, and you know damn well what I'm going to do about it at this point.

As soon as I work through some of the [`cl-notebook` issues](https://github.com/inaimathi/cl-notebook/issues?q=is%3Aissue+is%3Aopen)...

Sigh. No rest for the wicked.
