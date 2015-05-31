That was a vacation, I guess.

It was suspiciously taxing, all in all. Time off from work hasn't been nearly as relaxing since we had a kid, but that's a digression. Over the past little while, I've managed to finally make use the 120G solid state drive I picked up half a year ago, install various distros, and put together about one third of a utility to ease a project or two I'm working on in my spare time.

### New Drive

It's at once larger and smaller than the last one. On the one hand, thanks to its smaller physical profile, I can fit it into my laptop with no mods. On the other hand, `df -h` says `106G`<a name="note-Sat-Jan-04-174331EST-2014"></a>[|1|](#foot-Sat-Jan-04-174331EST-2014) instead of `~28G`.

That's it, nothing else to see here.

### Fresh Install

Since I was between drives anyhow, I took the opportunity to get the fresh version of [Debian](http://www.debian.org/devel/debian-installer/) up and running. That was worth it, by the by, if for no other reason than they've apparently poured enough bucketfulls of time into the networking code that I can now reliably connect to my wifi access point even if I'm not within two meters of it. They also seemed to lick a problem I kept running into wherein the shutdown process would hang the machine<a name="note-Sat-Jan-04-174334EST-2014"></a>[|2|](#foot-Sat-Jan-04-174334EST-2014).

There were a few changes in my install routine, which is still vaguely based on

```shell
## temporarily add
## deb http://packages.linuxmint.com debian import
## and `contrib non-free` to /apt/sources.lisp

apt-get install firmware-ralink firmware-realtek
apt-get install screen make emacs24 git gitk wicd-curses pmount htop gnupg unetbootin
apt-get install mplayer feh pacpl imagemagick x-window-system dmenu xmonad gimp inkscape firefox
apt-get install python-pip sbcl vrms

## remove the temporary repos
```

There are a couple of changes there from my usual. Firstly, [Firefox](https://www.mozilla.org/en-US/firefox/new/) has become my go-to browser. Its absorbed most of the goodness from Chromium, including the reduced toolbar footprint and fantastic JS console. It also has support for adblock, and a fairly good RSS feed reader, and it's no longer slow as molasses<a name="note-Sat-Jan-04-174337EST-2014"></a>[|3|](#foot-Sat-Jan-04-174337EST-2014). This raises the problem of the Debian packaging though; in the official repos, `apt-get install firefox` gets you a pretty ham-fisted re-brand with no plugin support called ["Iceweasel"](http://packages.debian.org/unstable/web/iceweasel). What I ended up doing, as you can see above, is temporarily adding the [Linux Mint](http://www.linuxmint.com/) repo to install that<a name="note-Sat-Jan-04-174339EST-2014"></a>[|4|](#foot-Sat-Jan-04-174339EST-2014). Secondly, I'm installing `emacs24` rather than just plain `emacs`. This is because the current default for `emacs` in Jessie is Emacs 23.something, and that doesn't have one of the main features I'm looking to finally adopt.

Emacs 24 supports [`package`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html#Package-Installation) out of the box. In practice, this means adding

```lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
```

to my `.emacs` instead of toting my old `.emacs.d/` around. I can't actually remember every library I *used* to have around, so the list I settled on this time out ended up being

aes auto-complete autopair highlight-parentheses htmlize skewer-mode magit markdown-mode paredit redo+ smart-tab yasnippet

Which covers pretty much everything. Oh, one thing. I spent about half an hour figuring out what was going wrong with my `.emacs` config; libraries I was certain had been installed were coming back with `not found` errors when I tried to `require` them. It turns out that when you add a directory to the load path, you don't automatically add all its subdirectories. As you can see by the above list of packages, I use quite a few, each of which gets its own sub-directory in `.emacs.d/elpa/`, and wildcards don't work here either. So I was forced to add the following to [convenience.el](https://github.com/Inaimathi/emacs-utils/blob/master/convenience.el), just to save myself the tedium

```lisp
(defun starts-with-dot-p (path)
  (= (aref path 0) ?.))

(defun list-subdirectories (path)
  (let ((all (mapcar 
              (lambda (name) (concat (file-name-as-directory path) name))
              (remove-if #'starts-with-dot-p (directory-files path)))))
    (remove-if-not #'file-directory-p all)))

(defun add-to-load-path (dirs)
  (mapc (lambda (p) (add-to-list 'load-path p)) dirs))
```

then called this near the top of that `.emacs` file:

```lisp
(add-to-load-path (list-subdirectories "~/.emacs.d/elpa"))
```

This let me continue as normal. The only omission from that emacs package list is [`slime`](http://common-lisp.net/project/slime/), which I've lately been installing from `sbcl` or what-have-you with `(ql:quickload :quicklisp-slime-helper)` rather than through Emacs itself. It works exactly as well as you'd expect, which is to say flawlessly.

### Dicking Around With Pis

Doing that got me into an installing mood, so I also formatted a fresh couple of SD cards with the latest versions of [ARM Arch](http://archlinuxarm.org/platforms/armv6/raspberry-pi) and [Raspbian](http://www.raspbian.org/) respectively. I did this with the vague intention of getting `deal` to work with one or both, and it looks like *that*'ll take a bit more work than just a straight-up `ql:quickload`. Differences before I get to that though.

The RPi arch is much closer to what I'm used to on my laptop. A brutally minimal installation of the few core utilities you need to get basic shit done, and *nothing else*. Specifically, it gives you `pacman`, `perl`, a working `ssh` server and a minimally intrusive wireless connection mechanism that could replace `wicd-curses` for me. Raspbian, by contrast, bundles a mandatory window environment along with a bunch of crap that's probably nice for most humans looking to use it as a desktop replacement, but that I'll never end up touching. Also, they bundle Scratch as well as Python 2 *and* 3. Finally, while they do provide an `ssh` server, it's off by default, and the first time you boot a Raspbian image, it *forces* `raspi-config`, which means that you *must* connect a Raspbian Pi to a monitor and keyboard at least once.

That minimalism ends up biting Arch a bit though; it doesn't come with the standard `raspi-config` utility, which lets you dick around with the hardware to some small extent, and easily resize the installation partition to fill the SD card<a name="note-Sat-Jan-04-174349EST-2014"></a>[|5|](#foot-Sat-Jan-04-174349EST-2014). The other thing that bites ARM Arch in the ass, as far as I'm concerned, is the fact that its package manager has very few of the things I want to install. Out of my usual menagerie, I found `screen`, `make`, `clisp`, `python`, `emacs` and nothing else. By contrast, I had to `apt-get --purge` a bunch of things over on Raspbian, but I *was* eventually able to get it working with an almost copy of my laptop environment.

Almost, because Lisp still has some problems.

Specifically, `clisp` segfaults on both ARM Arch *and* Raspbian when you try to load anything with `quicklisp`, while the ARM [ccl](http://ccl.clozure.com/download.html) failed to run at all on Arch<a name="note-Sat-Jan-04-174352EST-2014"></a>[|6|](#foot-Sat-Jan-04-174352EST-2014). Raspbian *did* run `ccl` appropriately, but errored out on me for two reasons. Firstly, there's something unsupported about the `:ironclad` MD5 digest, and secondly, the ARM architecture seems to treat bivalent streams differently than x86. Which means that even running its custom [`house`](https://github.com/Inaimathi/deal/tree/master/house) server, `:deal` errored out.

I'll be trying to fix that over the next little while.

### cl-git-fs

Finally, on a merely semi-related note, I'm working on a couple of projects on my own time that are eventually going to want to do some sort of file management. And I figured it would be nice not to have to bring `git` into it manually after the fact. To that end, I took a look at how [`gitit`](http://gitit.net/) manages the trick of using git as a faux-database for its wiki pages. It's [not that complicated](http://hackage.haskell.org/package/filestore-0.3.2/docs/src/Data-FileStore-Git.html), as it turns out. And [here's](https://github.com/Inaimathi/cl-git-fs) the result of spending an hour or two porting that piece of functionality to Common Lisp.

The biggest problem I'm running into is that there isn't a standard `shell-command` or `run-program` defined in the various Lisps I want to support.

I'm tossing it up to [my github](https://github.com/Inaimathi), but calling it `0.01` because there's a fuckton of functionality and documentation missing. In particular, it currently only supports SBCL on Linux, and a few of the external API functions still return raw string results, rather than properly parsed CLOS instances. The documentation and parsing will be a priority no matter what, but I'll only see how it runs on other platforms and implementations as I need to deploy to them.


* * *
##### Footnotes
1 - <a name="foot-Sat-Jan-04-174331EST-2014"></a>[|back|](#note-Sat-Jan-04-174331EST-2014) - Of course, the drive box says 128G, so Samsung and all drive manufacturers are lying shitbags, but I'm digressing again.

2 - <a name="foot-Sat-Jan-04-174334EST-2014"></a>[|back|](#note-Sat-Jan-04-174334EST-2014) - And therefore keep drawing power until a forced shutdown.

3 - <a name="foot-Sat-Jan-04-174337EST-2014"></a>[|back|](#note-Sat-Jan-04-174337EST-2014) - which it was last time I played around with it.

4 - <a name="foot-Sat-Jan-04-174339EST-2014"></a>[|back|](#note-Sat-Jan-04-174339EST-2014) - No, since you ask, I've never just straight up tried Mint. It has something in common with most of the distros I get recommended, which is that it cribs heavily from Debian on everything that matters, and then tries to differentiate on the desktop environment almost entirely. Not that this is bad for end users I guess, but as you can see from the `x-window-system` and `xmonad` items in that installation list above, I do not use what you would think of as "a desktop environment". Don't let that stop you from trying it, of course, but *I'm* not going to.

5 - <a name="foot-Sat-Jan-04-174349EST-2014"></a>[|back|](#note-Sat-Jan-04-174349EST-2014) - You can still do this externally via `gparted` when you image your SD card.

6 - <a name="foot-Sat-Jan-04-174352EST-2014"></a>[|back|](#note-Sat-Jan-04-174352EST-2014) - running the included binary gave me a "wrong architecture" error, even though there's no way that's accurate.
