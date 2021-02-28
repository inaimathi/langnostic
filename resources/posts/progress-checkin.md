This is just a quick post to talk about a few things I've gotten done that I didn't note elsewhere. I've been quiet for the past while because a) holidays, b) various personal shit and c)...

## Machine Installation

My computer crapped out on me. It was something weird to do with my hard drive partition table. A [bit of research](https://askubuntu.com/questions/837143/failed-to-connect-to-lvmetad) told me that shit was happening somewhere in the decryption step? I'm still not a hundred percent sure what it was, because I could easily boot the machine using a different drive, and then both `unlock` and `mount` the encrypted partition. None of the solutions I found on the various online forums I checked made booting work, so I removed the affected hard drive and set about rebuilding my machine.

This involved some changes to my [`machine-setup`](https://github.com/inaimathi/machine-setup) and [`shell-ui`](https://github.com/inaimathi/shell-ui) repos. For a few reasons, both related to ease-of-use and compatiblity.

#### Checkpoints

The main usability point was adding a [checkpoint system](https://github.com/inaimathi/machine-setup/commit/84761de9e0a22432cb978c9b230f4d189379b177). Mainly so that I can just re-run `sh machine-setup.sh` in the event of a failure and expect it to work relatively well. It's mechanically complex but fairly self-explanatory. The short version is: there's a built-in way in shell scripts to check whether particular directories or files exist. It's done, respectively with

```
## machine-setup.sh
...
if [ ! -d checkpoints ]
...
if [ ! -f ~/caps2ctrl.map ]
...
```

I use this to occasionally record what step the setup script is at, and let it skip steps that successfully completed.

#### Guix installation

Next up, `guix` installation seems mildly borked at the moment because the appropriate authentication keys aren't automatically downloaded and imported? Explicitly adding the step


```
gpg --keyserver pool.sks-keyservers.net --recv-keys 3CE464558A84FDC69DB40CFB090B11993D9AEBB5
```

seems to resolve this problem. Additionally, installing `curl` and `acpi` is now possible by `guix`, so that's what I'm doing.

#### The No-Fun-At-All Parts

The rest of this section is basically bitching.

So, I tried to install [Ubuntu 20.04](https://releases.ubuntu.com/20.04/), and a more recent edition of [`emacs`](https://www.gnu.org/software/emacs/). And those two percipitated compatiblity issues that were quite annoying to work through. At the high level, `python` no longer exists, and the default Python implementation is `python3`, the `cl` package has been deprecated in a way that's not trivially fixable, and there seems to be some weirdness with `marmalade`. Oh, and after I thought I'd worked all the kinks out, I tried to lock my screen and found that my old approach no longer worked.

Sigh. Ok, then, in reverse order.

###### Start using `slock`

I don't remember why I arrived at this solution, but I used to use `gnome-screensaver-command -l` to lock my screen, and bound the action to `C-t C-l` in my [`stumpwmrc`](https://github.com/inaimathi/machine-setup/blob/master/stumpwmrc). As of the upgrade to `20.04`, this doesn't work. One thing I tried here was using `xscreensaver-command` as a drop-in replacement for `gnome-screensaver-command`.

This...didn't work either. Nice going through something of [`jwz`s](https://www.jwz.org/) again, but something to do with authentication configuration resulted in not being able to dismiss the screensaver once it activated. Locking the screen is a priority, but I'd _also_ like to be able to unlock it without having to restart my machine.

Rather than chasing down [`PAM`](http://manpages.ubuntu.com/manpages/bionic/man5/pam.conf.5.html) documentation, I just settled on using [`slock`](https://tools.suckless.org/slock/). Minimal, almost no muss/fuss. Except that I had to install it through `apt` rather than path of `guix install`. There _is_ a `slock` module in `guix`, but for arcane reasons [relating to OOM killer](https://github.com/NixOS/nixpkgs/issues/9656), it errors when run that way. It would need `sudo`, and I haven't yet figured out how to run a `guix`, user-installed, module using `su`/`sudo` nor am I convinced that this is a good idea.

###### Port `convenience` and `packs` to `cl-lib` port `shell-ui` to `python3` and include `wicd-curses` replacement

The custom [`emacs`](https://www.gnu.org/software/emacs/) modules I use include `cl`, because `loop` and `assert` are very nice to haves from [Common Lisp](https://common-lisp.net/). That module is now [deprecated](https://github.com/kiwanami/emacs-epc/issues/35). Apparently the solution is to include `cl-lib`, which is "better" than `cl` for reasons I'm not at all clear on. This _isn't_ a drop-in replacement; a bunch of functions I used to use from `cl` have been renamed to include a prefix of `cl-`. This seems stupid; I vaguely assumed that since [Elisp now includes lexical binding](https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html) we wouldn't have to put up with bullshit like this, but I guess I was wrong. In any case, in order to prevent being showered by a hundred warnings on `emacs` startup, I did the appropriate renaming as reflected in [`convenience.el`](https://github.com/inaimathi/machine-setup/blob/bd091cff8e2847c30bd76154bf0088747a11780d/convenience.el).

A related change here was updating a bunch of Python-related arcana to reference `python3` rather than `python`. This sadly included basically everything in my [`shell-ui` scripts](https://github.com/inaimathi/shell-ui/tree/6aa80f97cc7d1092449d76fa64c1f4539a50465b/python), and _also_ prevented me from using my preferred network manager. As of this writing [`wicd-curses`](https://linux.die.net/man/8/wicd-curses) does not support `python3`, which means that if I wanted to run it, I'd have to install `python2` _specifically_ for this. For some reason, probably permission-related, this is one of the packages that I install using `apt` rather than `guix`. A quick search told me that there aren't really decent alternatives, so fuck it, I'm already shaving yaks, I may as well [shave a cow or two](https://github.com/inaimathi/shell-ui/blob/6aa80f97cc7d1092449d76fa64c1f4539a50465b/python/wlan).

That's a minimal script that scans for wireless networks whose passwords I've entered in a config file, and connects to the first one it finds. Its' been serving fairly well, but I expect it to explode about the time I have the desire to connect to an unprotected public wifi. Which is [probably a good thing](https://www.finjanmobile.com/the-dangers-of-using-unsecured-wi-fi/).

## Common Lisp projects

### `clj`

Minor work happened on [`clj`](https://github.com/inaimathi/clj). This is probably going to be typical for me; as I use it and notice that it's missing something basic, I'll add it. First up is a [polymorphic `union`](https://github.com/inaimathi/clj/commit/cdf700a54ba2db6403adb86b969cf169aec8118b). The standard Lisp `union` works on lists, and since I've introduced new datastructures that I'd like to use instead, that really won't suffice for me. The only real issue I have with this solution is that it commits me to shadowing the symbol `union` if I want to use `clj`. This is less than ideal because the _main_ use case for it is using it alongside `cl`. For now though, that'll do.

The other thing that happened is that I got sick of not having Python-esque format strings. I don't _often_ have a need of them, and it's always for logging, but I still miss them. So...

```
;; syntax.lisp
...
;;;;;;;;;; String Templates
(defun string-template-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((res (read stream)))
    (assert (stringp res) nil "You need to provide a string to the #F reader")
    (if (contains? res #\{)
	(let ((chunks nil)
	      (chunk nil))
	  (with-input-from-string (s res)
	    (loop for c = (read-char s nil nil) while c
		  do (if (char= #\{ c)
			 (progn
			   (push (coerce (reverse chunk) 'string) chunks)
			   (setf chunk nil)
			   (push (eval (read s)) chunks)
			   (read-char s))
			 (push c chunk))))
	  (format nil "~{~a~}" (reverse chunks)))
	res)))

...
  (:dispatch-macro-char #\# #\f #'string-template-reader)
...
```
You can see the full commit [here](https://github.com/inaimathi/clj/commit/06682ecf822d23455fc9c2cffc588be1f6aa3285). This gives me syntax that can be used like

```
CL-USER> (ql:quickload :clj)
To load "clj":
  Load 1 ASDF system:
    clj
; Loading "clj"
..................................................
[package clj]...........
(:CLJ)
CL-USER> (named-readtables:in-readtable clj:syntax)
#<NAMED-READTABLE CLJ:SYNTAX {1009262043}>
CL-USER> #f"a {(+ 1 2)}"
"a 3"
CL-USER>
```

Anything in curlies inside of the string gets evaluated, which does unfortunately mean that you can't use curlies literally in `f` strings, but given how specific the use case for this is, I don't mind it for the moment. It's still something I intend to _eventually_ fix, and patches are welcome, but it's not happening right now.

### `quickproject`

My [fork of `quickproject`](https://github.com/inaimathi/quickproject) now has a test-enabling default template, because I've found that it's really the only way I want to use it. I did [submit a PR](https://github.com/xach/quickproject/pull/37), but it seems to have been rejected from the mainline. I'm ok with this. In the PR's comments, I outline why I think this is the right way to go, but accept that there's a high potential for differences of opinion. That PR from above encodes mine. You might consider it, but ultimately, do as thou wilt.

### `house`

The major overhaul is complete as of [this commit](https://github.com/inaimathi/house/tree/282bbb9f694da10c3cd765074faeabda21296baa). At this point, handler specification has been significantly simplified. It got shifted to, basically, higher-order functions rather than the old style that used a mildly confusing macro language. [Docs](https://github.com/inaimathi/house/tree/282bbb9f694da10c3cd765074faeabda21296baa#usage) are also up to date, and make sure to highlight the [new type annotation system](https://github.com/inaimathi/house/tree/282bbb9f694da10c3cd765074faeabda21296baa#using-the-type-annotations) as well as [usage of *rest parameters](https://github.com/inaimathi/house/tree/282bbb9f694da10c3cd765074faeabda21296baa#setting-up-rest-parameters). I've also already updated [`cl-notebook`](https://github.com/inaimathi/cl-notebook) to use this new system.
