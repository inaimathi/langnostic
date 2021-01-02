This is just a touchstone to talk about a few things I've gotten done that I didn't note elsewhere. I've been quiet for the past week because a) holidays, and b)

## Machine Installation

My computer crapped out on me. It was something weird to do with my hard drive partition table. A [bit of research](https://askubuntu.com/questions/837143/failed-to-connect-to-lvmetad) told me that weird shit was happening with something with the decryption step? I'm still not a hundred percent sure what the deal was, because I could easily boot the machine using a different drive, and then both `unlock` and `mount` the encrypted partition. None of the solutions I found on the various online forums I checked made booting work, so I removed the affected hard drive and set about rebuilding my machine.

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

#### Guix installation

Next up, `guix` installation seems mildly borked at the moment because the appropriate authentication keys aren't automatically downloaded and imported? Explicitly adding the step


```
gpg --keyserver pool.sks-keyservers.net --recv-keys 3CE464558A84FDC69DB40CFB090B11993D9AEBB5
```

seems to resolve this problem. Additionally, installing `curl` and `acpi` is now possible by `guix`, so that's what I'm doing.

#### The No-Fun-At-All Parts

The rest of this section is basically bitching. So, I tried to install Ubuntu 20.04, and a more recent edition of `emacs`. And those two percipitated a bunch of compatiblity issues that were quite annoying to work through. At the high level, `python` no longer exists, and the default Python implementation is `python3`, the `cl` package has been deprecated in a way that's not trivially fixable, and there seems to be some weirdness with `marmalade`. Oh, and after I thought I'd worked all the kiks out, I tried to lock my screen and found that my old approach no longer worked.

Sigh. Ok, then, in reverse order.

###### Start using `slock`

I don't remember why I arrived at this solution, but I used to use `gnome-screensaver-command -l` to lock my screen, and bound the action to `C-t C-l` in my [`stumpwmrc`](https://github.com/inaimathi/machine-setup/blob/master/stumpwmrc). As of the upgrade to `20.04`, this doesn't work. One thing I tried here was using `xscreensaver-command` as a drop-in replacement for `gnome-screensaver-command`.

This...didn't work either. Nice going through something of [`jwz`s](https://www.jwz.org/) again, but there's something to do with authentication configuration that resulted in not being able to dismiss the screensaver once it activated. I mean, locking the screen is a priority, but I'd _also_ like to be able to unlock it without having to restart my machine.

Rather than chasing down [`PAM`](http://manpages.ubuntu.com/manpages/bionic/man5/pam.conf.5.html) documentation, I just settled on using [`slock`](https://tools.suckless.org/slock/). Minimal, almost no muss/fuss. Except that I had to install it through `apt` rather than path of `guix install`. There _is_ a `slock` module in `guix`, but for arcane reasons [relating to OOM](https://github.com/NixOS/nixpkgs/issues/9656), it errors when run that way. It would need `sudo`, and I haven't yet figured out how to run a `guix`, user-installed, module using `su` or `sudo`.

###### Port `convenience` and `packs` to `cl-lib` and stop using `marmalade`

The custom [`emacs`](https://www.gnu.org/software/emacs/) modules I use include `cl`, because `loop` and `assert` are very nice to haves from [Common Lisp](https://common-lisp.net/).

###### Port `shell-ui` to `python3` and include a simple replacement for `wicd-curses`

TODO -

- update `machine-setup` to include a checkpoint system and a bunch of changes relating to `guix`
- emacs module incompatibilities (deprecation of `cl` and weirdness with `marmalade`)
- add `wlan` to `shell-ui` and update `python` scripts to `python3` scripts

## `clj`

- added set operations
- added Python3-style string templating syntax

## `quickproject`

- new testing template

## `house`

- major overhaul complete
- possibly &rest params, and simpler `define-file-handler`?
