So about two weeks ago, I finally got sick of waiting for my [new laptop](https://puri.sm/librem-15/) to ship, and decided to put my [newly increased data cap](https://teksavvy.com/) to good use.

By installing the latest Debian Testing.

Ahem. Since my [last time](/posts/fresher-install) installing anything, I've adopted a new package manager, some new `emacs` modules and a new language. In all fairness, I have done exactly nothing with the new language yet other than ensuring that I've got the compiler/interpreter locally. Hence the lack of a new logo up at the title bar.

This time, I decided to be a bit smarter about it. Specifically, it's high time I had a concrete, complete, turn-key solution for getting from "empty machine" to "everything's up and running". And here's how.

## Machine Setup..

... is [a project](https://github.com/Inaimathi/machine-setup). I started to collect basic, machine-level installation scripts. You can find the github [here](https://github.com/Inaimathi/machine-setup). What it currently contains is

- some sparse documentation, mostly aimed at my future self
- a minimal `.emacs` file, and some convenience macros
- `setup.lisp` file to get a baseline environment together for Common Lisp hacking
- my personal `xmonad.hs` config file
- a shell script that handles the portion of system installation which has to be done as root
- a second, rather larger shell script that handles everything else

That last one is the entry point for this thing by the way. The end result is that I can sit down at a laptop with a bare (desktop-less) installation of [Debian](https://www.debian.org/) and a good internet connection, run `sh machine-setup.sh`, answer one or two prompts for input, and be up and running in an hour or so. I won't exactly go line-by-line, but some basics are a good idea, I think.

## The Basic Principles

I want to use [`nix`](http://nixos.org/nix/) for almost everything. Two exceptions, in fact. Firstly, there are a few things that either don't exist in `nix`, or exist but would be non-trivial or useless to set up that way, and those are going to continue coming in from the Debian repos via `apt-get`. Secondly, most of the languages I currently like using have their own sandboxing, language-level package manager *(the lone exception being [Standard ML](http://sml-family.org/Basis/))*, so I don't see a real reason to start using `nix` instead *(though I reserve the right to rethink this)*. Keeping that in mind, it should be relatively clear why I can get away with very little `su` action in the scripts I'm about to vaguely describe, despite the fact that their overall goal is setting up a working machine.

### Root Components

As I mentioned, the entry point for this script suite is [`machine-setup.sh`](https://github.com/Inaimathi/machine-setup/blob/master/machine-setup.sh), but the first thing it does (after starting up `ssh-agent -s` and shooting Caps Lock in the back of its metaphorical head) is `su -c ./machine-root-setup.sh`, so I may as well talk about that first.

`machine-root-setup.sh` first does some evil things with the Debian `non-free` repos. Because I'm weak and still don't have a [Free](http://www.gnu.org/philosophy/free-sw.en.html) wifi card. We'll say no more about that. Next, it installs

- `curl` *(which is necessary for installing `nix`)*
- `x-window-system`, `wicd-curses`, `python-setuptools`, `python-pip`, `python-lxml` and `pacpl` *(none of which seem to exist in a working form in the `nix` repos)*
- `xmonad` and `pmount` *(which are pains to install via `nix`. The former because it doesn't automatically get hooked into `startx` as the window manager, the latter because it needs `root` permissions to function when installed via `nix`, but the whole point of it is mounting USB devices *without* `root` permissions.)*

Finally, it calls `loadkeys` on the keymap we generated earlier, and creates the appropriate directory for `nix` installation.

### General Installation

Once the `root` stuff is out of the way, we're onto the usual suspects. `emacs`, `git`, `firefox`, `mplayer`, `feh`, `gnumake`, `screen`, `gimp`, `inkscape` and `rsync` each get fairly regular use in general. Some more than others,  granted, but still. Also, `python3.4-youtube-dl` has recently replaced [`get_flash_videos`](https://code.google.com/archive/p/get-flash-videos/) for me. The former has a much easier installation process, though it does have a more annoying specification format for resolution of the incoming video.

The languages I've got at the moment are just [SBCL](http://www.sbcl.org/), [Haskell](https://www.haskell.org/), [SML](http://www.smlnj.org/), [OCaml](https://ocaml.org/) and [Python](https://www.python.org/). I should really *really* get [Ruby](https://www.ruby-lang.org/en/) in as part of the basic environment and consider updating the `ruby` section of [`shell-ui`](https://github.com/Inaimathi/shell-ui), especially since that's the language I use at work now, but haven't found the time for it. SBCL gets [`quicklisp`](https://www.quicklisp.org/beta/) and `:quicklisp-slime-helper` set up as part of [`setup.lisp`](https://github.com/Inaimathi/machine-setup/blob/master/setup.lisp). Haskell ... just gets `stack` and `cabal-install`. Between those two, I've got enough sandboxability for Haskell that I don't *particularly* need to worry about learning to use `nix` at a very advanced level. Which may be pointless, because I want to anyway. The reason is that Standard ML section. It only contains `mlton` and `smlnj`, with no package utility whatsoever apart from what those give you internally. I've got this idea that if I knew how to use it well enough, I could basically use `nix` as a suitable stand-in, but that would involve getting to know some of its deeper abilities. OCaml has no surprises; just `ocaml` and `opam`. Python comes pre-installed on Debian, so all I really need to do is get some libraries via [`pip`](https://pypi.python.org/pypi/pip), which was installed earlier. Note to self here, by the way, it *looks* as though I could install some or all of the required python libraries via `nix` if I really wanted to. Future self: look into it.

All that's left is some environment niceties. Specifically, getting a bunch of Emacs libraries installed, pulling down a copy of `shell-ui`, and configuring `xmonad` and `git`.

The Emacs step is the only thing that called for a modification or two of my usual setup. In order to get the `nix` versions of each of those libraries running, I had to add

```
...
(add-to-load-path (list "/home/inaimathi/.nix-profile/share/emacs/site-lisp"))
(add-to-load-path (list-subdirectories "~/.nix-profile/share/emacs/site-lisp/elpa"))
...
```

to my `.emacs`, so that they'd be visible on startup. I'm not about to go through them in-depth, but `add-to-load-path` and `list-subdirectories` are little pieces of sugar from [`convenience.el`](https://github.com/Inaimathi/machine-setup/blob/master/convenience.el) that do pretty much exactly what you'd expect them to.

## Next Up

I need to learn to use `nix` properly. Fully reproducible builds sound like fuckin' manna from heaven given the issues I've had with pretty much every language I've ever used. That's *one* of the things it seems you could do with it. The checklist of interesting thins I'd want to do is, in no particular order

- `[x]` Replace `apt-get`
- `[ ]` Replace `brew` at work
- `[x]` Back out of environment-breaking installations
- `[ ]` Use as language-library manager *(mostly for the sake of Standard ML, but others might likewise benefit)*
- `[ ]` Deploy a build to some remote server by copying up a nix closure
- `[ ]` Host builds locally to reduce bandwidth usage when installing personal systems
- `[ ]` Run a continuous integration of something with [Hydra](https://hydra.nixos.org/)[(PDF)](http://nixos.org/~eelco/pubs/hydra-scp-submitted.pdf)

All of those sound interesting, useful, and kinda difficult given my limited ops experience. I'll keep you posted on how it all goes.
