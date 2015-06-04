Just a quick update today.

We're working on Yet Another Ridiculously Boring RFP at the office, so I had enough time to dick around with some background tasks for the RasPi. Here are some tidbits that I didn't know off-hand, but do now:

### <a name="sd-class-not-a-big-deal" href="#sd-class-not-a-big-deal"></a>SD Class Not a Big Deal

I thought I was being badass getting the Class 10s, but it turns out not to matter very much. Raspbian will boot to prompt in ~20 seconds from a Class 10 card, and ~24 from a Class 4. There *is* a difference, and some things might be a bit snappier from the Class 10, but overall I probably would have opted to save my money had I known how big the actual difference is.

### <a name="hd-video-outofthebox" href="#hd-video-outofthebox"></a>HD Video Out-of-the-box

Yup. I spent a bit of time dicking around with `mplayer` settings before I realized that you can actually play your awesome videos directly from the Raspbian image. You need to set your memory split to either of the two highest options by running `raspi-config`, selecting the setting, then restarting. Then, just `oxmplayer /path/to/your/file/here.mp4`. And there; glorious, full playback rate HD is yours. The only problem this raises for me is re-writing my media center webapp, since I'll need to control something other than `mplayer`.

### <a name="no-ghci-for-you" href="#no-ghci-for-you"></a>No GHCi For You!

You can still install [Hugs](http://www.haskell.org/hugs/) and play around that way, but `GHCi` categorically does *not* work on ARM yet. Which is kind of sad, because I was looking forward to busting it out. I'm not entirely sure how compatible Hugs and [GHCi](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler) are, but I'm betting the answer is "Not 100% compatible". I may or may not be appraising you of the `diff`s shortly.

### <a name="yes-clojure-for-you" href="#yes-clojure-for-you"></a>Yes Clojure For You!

This one sort of boggles my mind. Yes, you can `apt-get install leiningen` on a Raspberry Pi, and yes this lets you then `lein repl`. No, it's not very fast, it takes longer for the JVM to start up than it does for the RasPi to boot, and there's a visible delay when evaluating anything more complex than basic arithmetic, but goddamit it's *there*.

Really, anything I've wanted to do so far was fully available to me. I spent lunch today installing Emacs, then running `SLIME` with `clisp` and poking around. What I *haven't* done is customize my environment yet, just installed the components that would allow me to do so. Next order of business is getting all my various `.emacs`/`.emacs.d` settings imported, and making sure everything plays nicely.

With any luck, my next blog post will be coming from a computer smaller than my phone.
