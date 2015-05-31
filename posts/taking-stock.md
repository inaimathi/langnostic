I'm getting to the point where it's becoming annoying having more than one setup on each machine. At the moment I've got a laptop running [XMonad](http://xmonad.org/) and [XFCE](http://www.xfce.org/) (my primary machine), a desktop running [StumpWM](http://stumpwm.org/) on top of [Gnome 2](http://www.gnome.org/) (my secondary machine), and a lesser laptop running XFCE standalone.

Ok, that's actually not counting the two play machines (special purpose builds that I've configured just for the fuck of it. One is a bare-metal Debian running nothing but `bash`, Erlang and Screen. The other is a machine on which I've replaced `bash` with `clisp` and use Stump just to complete the Lisp Machine illusion), and the backup server (a very vanilla [Parabola](https://parabolagnulinux.org/) box with no desktop; all it does is periodically run `rsync` on specified directories).

Anyway, I think I've decided where I'm going with my computing environment, and as I mentioned earlier, it's definitely away from the standard icon-based desktop that their developers seem to be pushing at the moment. To that end, I've been taking exhaustive stock of what I actually, factually use on a day-to-day basis and what kind of stuff I need only periodically. I've also journaled a recent clean install just to figure out what small and mostly forgotten tweaks I make during setup.

Tweaks first, while they're still fresh in my mind

### caps-lock swap

I've actually pulled out the parts of my setup scripts that automate this, since the process seems to change every time I actually do a setup. Sometimes I have to fuck around with `/etc/default/keyboard`, sometimes it involves setting other arcane options. This time, I had to add

```
remove Lock = Caps_Lock
remove Control = Control_L
keysym Caps_Lock = Control_L
add Lock = Caps_Lock
add Control = Control_L
```

to my `.xmodmaprc`, and then `xmodmap .xmodmaprc` to my `.xinitrc`.

### volume control

I have no idea why, but every installation of debian I've done so far has had volume muted by default. Usually, I don't even notice this because it's easy to right-click on the GNOME volume control and fix it. The rare times that I set up a media-capable machine with no desktop environment, there is no such option. Typically, I run `aumix` and unmute the master volume, but that package seems to have been obsoleted. It still installs properly through `aptitude`, but errors when you try to run it (complaining that it can't find `mixer`, which was apparently removed a little while ago). This time out, I had to install `alsa` and use `alsamixer` to get sound back.

### quicklisp

Using this has streamlined my lisp workflow enough that I've just sort of started thinking of it as a standard feature of a computer, rather than something I have to install. Luckily, since Xach has the beta file at a consistent URI, this *is* something that's scriptable.

### rc files

`.emacs` is the least of it. I currently have one of each


-   `.stumpwmrc`
-   `.screenrc`
-   `.xmodmaprc`
-   `.xinitrc`
-   `.conkerorrc`
-   `.lynxrc`
-   `.sbclrc`


I've also got a `.clisprc`, but all it contains is the quicklisp hook, so it doesn't really count. Between these and my growing `.emacs.d`, it's about high time I sit down and put together a basic `~/` git repository so that I can just do a checkout instead of trying to remember all the various files I need.

### Things I Use Constantly (Really)

emacs
[chromium](http://www.chromium.org/)/[conkeror](http://www.conkeror.org/)/[lynx](http://lynx.browser.org/) (depending on whether I've got a mouse to hand and/or feel like starting up X)
ssh/ssh-agent
x-window-system + stumpwm/xmonad (leaning towards stump at this point, since it has home-row keybindings and is extensible in Lisp)
dmenu
wicd-curses
sbcl/clisp
erlang

### Things I Use Most Days

mplayer
thunar
gnu-make
python
ristretto
screen
zip/rar/tar (and [pack](https://github.com/Inaimathi/shell-ui/blob/master/ruby/pack)/[unpack](https://github.com/Inaimathi/shell-ui/blob/master/ruby/unpack), of course)
liferea (which I have to admit, I'm not entirely sold on, but newsticker.el is too annoying to actually use, and I only follow [two](http://stackexchange.com/feeds/tagsets/43442/inaimathi-lang-digests?sort=active) RSS [feeds](http://www.groklaw.net/backend/GrokLaw.rdf) in any case. I'm going to think through putting together my own later, either as an Emacs extension, or as a quickie application in some language I'm trying to learn, but I digress)
epdfview
rsync

### Things I Use Some Days
gnupg
cifs
[libre-office](http://www.libreoffice.org/download/)
Inkscape
GIMP
xsane (yes, as in manual `scanimage` invocations)
imagemagick
[get_flash_videos](http://code.google.com/p/get-flash-videos/)
pacpl

### Things I Use Very Rarely

ruby
klavaro (still at ~75 wpm, incidentally)
usb keys/cds/any kind of removeable media
smalltalk
node.js
haskell

The most interesting part of that (to me) is actually the last one. I don't do regular typing training anymore, even though I'm a far cry from the 100wpm I'd like to be at, and I don't really use those rather interesting languages. I'm actually not even sure why that is; I'd really like to get into each of them, and I actually did formerly use Ruby for a lot of quick scripting/setup tasks. The setup tasks have been taken up by `make`
