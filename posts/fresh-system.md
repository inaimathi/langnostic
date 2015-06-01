This weekend finally saw enough random free time that I manged a clean install of my laptop, and I think I've gotten it into more-or-less working order.

That's the [Lenovo x220](http://shop.lenovo.com/us/laptops/thinkpad/x-series/x220) I wrote about [a while ago](http://langnostic.blogspot.ca/2011/12/x220-and-unrelatedly-portable-keyboards.html), though it has oddly gone up in price by more than an order of magnitude. I guess Core i3s are in very, very short supply?

Anyhow, this is the first time that I've configured my main machine without a desktop environment. I usually run either [XFCE](http://www.xfce.org/) or [GNOME](http://www.gnome.org/) under my window manager on my primary, and keep crazy things like [Screen-as-WM](http://langnostic.blogspot.ca/2011/10/screen-for-stumpwm-users-gnu-screen-as.html), and [odd `bash` replacements](http://langnostic.blogspot.ca/2012/01/how-close-can-you-get-to-lisp-machine.html) to my play boxes. Much as I hate to admit it, `x-window-system` is still a requirement at an office where you need to co-exist with MS users. Mainly for [PDF viewing](http://trac.emma-soft.com/epdfview/) and [documents/spreadsheets](http://www.libreoffice.org/download/), but it also helps to be able to do some [image](http://inkscape.org/) [editing](http://www.gimp.org/) if the situation calls for it.

## <a name="window-manager"></a>Window Manager

I decided to go with [StumpWM](http://stumpwm.org/) over [XMonad](http://xmonad.org/). The practical differences are minute. XMonad uses a workspace-based structure by default, whereas StumpWM treats windows individually<a name="note-Tue-Jun-12-210019EDT-2012"></a>[|1|](#foot-Tue-Jun-12-210019EDT-2012). Stump treats all windows equally, where XMonad has the concept of a master window in a given layout. StumpWM assumes `C-t` as the `mod` key, and supports Emacs-style chords out of the box. You can use [XMonad.Actions.Submap](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-Submap.html) to get *some* of the functionality back, but there are two places it falls short, and those have annoyed me enough to switch back over to the Lisp-based WM.

The first shortfall is, even though you can technically use the submap feature, there doesn't seem to be a good way of simulating the taken keystroke. That is, if you set your XMonad `mod` key to `C-t`, you now have no way of using `transpose-chars` in Emacs or `New Tab` in Chrome. StumpWM has a mechanism to let `C-t C-t` fake the usual `C-t` keystroke to the focused application, but XMonad has nothing similar that I've found.

The second is that certain XMonad keystrokes are designed to have you hold the `mod` key, and submap chord keys can't be held. The best example of this is resizing windows. The standard keystrokes are `Mod-h` and `Mod-l`, for growing and shrinking the master window respectively. The way these work is that each invocation grows/shrinks the master window by about 5px, so if you want to do any significant resizing, you'll need multiple calls. If you bind `Mod` to a single key, like `Win`, you can do a series easily; hold `Win` + `h h h h h`. If on the other hand you want a chorded mod, it gets more complicated; `C+t h C+t h C+t h C+t h C+t h`. That's pretty annoying. StumpWM doesn't let you hold chorded keys either, but because they're the default, Stump keys tend to be designed to account for it. For example, the resizing situation above is solved with a separate `interactive-resize` setting; you hit `C-R`, which puts you into a mode where your arrow keys scale the focused window.

I suppose another solution could have been "get used to `Win` as your mod key", but I don't wanna. I'm working on a laptop, so that key is annoyingly narrow, and hitting it properly with my pinkie requires me to scrunch my left hand up somewhat uncomfortably.

So that's that; keeping my hands on the home row without sacrificing functionality is enough to drive me to another window manager.

## <a name="other-installables"></a>Other Installables

Other than the WM, I mentioned this was my first time going desktop-less. That produces one or two challenges, the big one being wireless. I'm using [`wicd-curses`](http://wicd.sourceforge.net/moinmoin/FAQ) to manage my connections, but that's actually the easy part. The x220 comes with one of three built-in wireless cards, and none of them like Debian very much. What I really ought to do is go out and buy a wifi card that supports free drivers, but in the meantime, this is the one place on my system where I use Debian's `contrib non-free` repos. I temporarily added them, and dropped them as soon as I was done installing `firmware-iwlwifi`, `firmware-ralink` and `firmware-realtek`.

I honestly don't know which of those did it, but on the next restart, I had `wlan0` available.

The other challenge with using a standalone WM is mounting/unmounting USB drives. I don't actually use them very much anymore; [`scp`](http://linux.die.net/man/1/scp), [`git`](http://git-scm.com/) and [`rsync`](http://en.wikipedia.org/wiki/Rsync) are much more effective at moving files across machines. The only time a thumbdrive comes out is when I need to do a system install, or when I need to exchange non-emailable files with a non-linux user. For those times, [`pmount`](http://pmount.alioth.debian.org/) is more than sufficient.

Other than that, I just need to get used to using `acpi` to check on my battery periodically, and using `alsamixer` to set up the volume the first time. It goes without saying that `caps-lock` is an additional `ctrl`.

The list of things installed on my system at this point is pretty short actually. Here's a script that duplicates most of the install

```
### with the contrib non-free repos enabled
apt-get install firmware-iwlwifi firmware-ralink firmware-realtek
### disabled again

apt-get install wireless-tools wicd-curses
apt-get install sbcl clisp erlang erlang-doc
apt-get install make screen dmenu htop gnupg git-core gitk emacs stumpwm slime pmount
apt-get install pacpl mplayer alsa imagemagick gimp inkscape conkeror chromium-browser
```

I haven't even bothered with databases or web servers yet, though I'm sure I'll have to eventually. I *did* grab a few applications from source just for the hell of it, and set up my usual utility scripts<a name="note-Tue-Jun-12-210928EDT-2012"></a>[|2|](#foot-Tue-Jun-12-210928EDT-2012), `quicklisp`, plus 7 or so `.*rc` files.

## <a name="lineup-changes"></a>Lineup Changes

There are a couple of big things I've changed, that you may have noticed, and one big thing I've changed that you definitely didn't. Most of it is pruning things that I've noticed I don't use. The languages I didn't bother installing this time include `haskell`<a name="note-Tue-Jun-12-210940EDT-2012"></a>[|3|](#foot-Tue-Jun-12-210940EDT-2012), `smalltalk`<a name="note-Tue-Jun-12-210947EDT-2012"></a>[|4|](#foot-Tue-Jun-12-210947EDT-2012), `node.js`<a name="note-Tue-Jun-12-210952EDT-2012"></a>[|5|](#foot-Tue-Jun-12-210952EDT-2012) and `ruby`<a name="note-Tue-Jun-12-210958EDT-2012"></a>[|6|](#foot-Tue-Jun-12-210958EDT-2012).

Finally, the latest version of `git-core` no longer ships with `git.el`. That's not entirely a bad thing; I've had to hack a lot of [additional pieces](https://github.com/Inaimathi/emacs-utils/blob/master/git-custom.el) onto it for my purposes, and I always sort of wished that it just worked out of the box. It turns out I was one of the ~3 people on the planet not using [`magit`](http://philjackson.github.com/magit/). Luckily, the lack of direct `git-core` has forced me to try it out, and it seems that this mode supports everything I was adding and then some. One or two annoyances, but I haven't run into anything that takes more than a trivial change in my workflow.

So yeah. Net gain, all told.

* * *
##### Footnotes

1 - <a name="foot-Tue-Jun-12-210019EDT-2012"></a>[|back|](#note-Tue-Jun-12-210019EDT-2012) - You can use [groups](http://stumpwm.org/manual/stumpwm_8.html) to approximate the XMonad model, but I haven't played with it much yet.

2 - <a name="foot-Tue-Jun-12-210928EDT-2012"></a>[|back|](#note-Tue-Jun-12-210928EDT-2012) - Though I will be porting the useful ones away from Ruby very shortly.

3 - <a name="foot-Tue-Jun-12-210940EDT-2012"></a>[|back|](#note-Tue-Jun-12-210940EDT-2012) - Which I've been playing around with periodically, but haven't used for anything serious.

4 - <a name="foot-Tue-Jun-12-210947EDT-2012"></a>[|back|](#note-Tue-Jun-12-210947EDT-2012) - Which I'm definitely coming back to at some point, but don't have the time for at the moment.

5 - <a name="foot-Tue-Jun-12-210952EDT-2012"></a>[|back|](#note-Tue-Jun-12-210952EDT-2012) - Which was enough of a pain in the ass to install properly that I'm avoiding it until I actually decide to use it.

6 - <a name="foot-Tue-Jun-12-210958EDT-2012"></a>[|back|](#note-Tue-Jun-12-210958EDT-2012) - Which I'm really sad about actually. However, I haven't used it for anything *but* scripts for the last little while. The number of scripts I've been writing in it has also been going downhill since I started using `make` actively.
