I've been noticing two patterns lately in my various activities. They're not really bad or good (or at least, I'm unsure which); it's just some digital behaviors I've noticed in myself.

Firstly, anything of even vague importance on my personal workstations is a `git` repository that I've made it a habit to udate each time I use each machine. Between the three independant computers I use on a daily basis, my data is pretty safe from a random hardware malfunction. Granted a building fire or similar would still take them all out but a single piddling hard-drive failure isn't something that strikes fear into my heart at this point. My most used git commands are (in descending order):

1. `git add .`
2. `git checkout <branch> <file>`
3. `git commit -m`
4. _(since I started using git from Emacs; see below)_ `git log`

Next, Emacs and Gimp are quickly supplanting almost every program I used to use. In fact (for my personal work) I now basically live in Emacs, Gimp, Inkscape and Chrome. Gimp gets used for image manipulation (Inkscape just for vectors), Google Chrome for the obvious, and Emacs for everything else. I used to use Terminal to run `git`, `erl`, `mzscheme` and (every once in a while) `sbcl`. Since getting heavily into Emacs, [Quack](http://www.neilvandyke.org/quack/) lets me drop into an improved `mzscheme` prompt, [erlang-shell](http://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html) does the same thing for `erl`, and despite my earlier complaints, [SLIME](http://common-lisp.net/project/slime/) is actually very nice once you're already comfortable with Lisp and Emacs. Emacs 22+ also comes with a [git package](http://alexott.net/en/writings/emacs-vcs/EmacsGit.html) that you just need to require to get a pretty freakin' good interface going.

As an aside, if you're interested in picking up Emacs, I highly recommend these [two](http://steve.yegge.googlepages.com/effective-emacs) [articles](http://steve-yegge.blogspot.com/2006/06/shiny-and-new-emacs-22.html) by Steve Yegge, and [Xah's awesome tutorials](http://xahlee.org/emacs/elisp.html). If you're interested in picking up a Lisp dialect and don't already know the editor very well, I actually recommend staying away from Emacs until you have a firm grip on [PLT Scheme](http://www.plt-scheme.org/), then coming back and picking up Elisp once you're more familiar with the language.

Like I said, I'm not sure whether these patterns are positive or not, but they remain as I've stated them:

1. Anything even remotely important on a machine I pilot is in a GIT repo.
2. I use a grand total of 4 programs for any and all personal projects (and the usage is skewed heavily towards Emacs and Chrome).
