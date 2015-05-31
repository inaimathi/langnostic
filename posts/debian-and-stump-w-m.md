Ok, so remember how I said I added some of the latest Ubuntu repos to my Debian sources.list just to hit one or two installs?

Don't do that.

For the love of God, don't. Or, at least, remove them from your sources file afterwards. I was happy enough with the stability of Debian this weekend, so I decided to run my install script (which includes things from audio-editing programs to inkscape, to emacs to several languages). Bad idea, to put it mildly. I had no idea what the hell I was doing at the time, so I just hit y ret a whole bunch of times, and in the process fucked up my sytem. I knew I was boned about the time I saw

```
removing gnome-network-manager
removing gnome-desktop
removing gnome-tray
removing x-server
[more bad stuff]
```

whizz by on the terminal window. When I next restarted, I got a friendly little prompt, and that's it. My data was still intact, but I didn't have a network connection for perhaps obvious reasons, so I mounted a usb-stick and got the shit I've been working on for the past day or so off, and prepared to reinstall (I don't know nearly enough about Linux innards to attempt surgery at this point). 

I had a thought though; my current installation was Debian Lenny, and there was a "testing" version out called Squeeze ("testing" is in quotes, because by all accounts I've read, it's rock solid by this point). It took a bit of counter-intuitive navigation on the Debian site to get to the [squeeze installer](http://www.debian.org/devel/debian-installer/), but I guess that's reasonable; they want most people to install the "stable" release, not the "testing" or "unstable" ones. So there, I'm typing to you live from Debian Squeeze, and I have to tell you, it's good. The biggest gripes I had from the last post have been addressed; the new version of Gnome plays nice with two monitors out of the box, and the squeeze repos have more recent installs of the programs I use than Ubuntu. Specifically, I get out-of-the-box apt-get access to emacs23, sbcl 1.0.40, haskell-platform, pacpl and synfig.

So there. Debian beats Ubuntu from my perspective at this point.

At this point, since I was already ass-deep in installs anyway, putting in StumpWM seemed like a logical conclusion. So I did. And I'm in love. It's Emacs for window management. Just as a note, I've found that any software I could describe as "Emacs for [n]" is something I'd probably like. Sadly, between Emacs, Emacs for the web and Emacs for window management, I get the feeling we're about tapped out now. I like GIMP, but it's not exactly "Emacs for images". I've set up a minimal .stumpwmrc file like so

```lisp

;; Psst, Emacs. This is a -*- lisp -*- file.
(in-package :stumpwm)
(message "Loading rc ...")

;;; Program definitions
(defcommand emacs () () (run-or-raise "emacs" '(:class "Emacs")))
(defcommand browser () () (run-or-raise "conkeror" '(:class "Conkeror")))

;;;Reloading
(defcommand reinit () () (run-commands "reload" "loadrc"))

;;;Keybindings
(define-key *root-map* (kbd "b") "browser")
(define-key *root-map* (kbd "C-q") "quit")
(define-key *root-map* (kbd "C-r") "reinit")

;;;Things that happen on StumpWM startup
(run-shell-command "/usr/bin/trayer --SetDockType false --transparent true --expand false")
(run-shell-command "nm-applet --sm-disable")
```

just to get everything up and running (trayer is needed to get the nm-applet working so I can has internets).

It's occurred to me that, now that my window manager is a Lisp machine, I could hard-wire "web-jumps" into my environment with Firefox. Not sure if it'd be worth giving up the keyboard shortcuts, but I would get HTML5 support, and all of my scripting would be done in Lisp at that point (rather than a JS/Lisp split).  I'm really not up for that this weekend, but I'll keep playing with Stump. So far, it's good stuff.
