Or, more realistically, "Working Cross Platform", except that the only two platforms I selfishly care about are Android and Debian-descended Linux. Last time I [touched Android in anger](/posts/android-poking), I was working on a MacOS machine, and trying to do it in JavaScript. This was _after_ failing to get [`cljs` up and running](https://medium.com/mindorks/building-mobile-apps-ios-and-android-with-clojurescript-4600235f826c). Given that my current explorations are, by virtue of HuggingFace, going through Python, I figured it might be a good idea to try that as a mobile development language.

It turns out it's not complete trash?

There's two realistic options here; [`beeware/briefcase`](https://beeware.org/) and [`kivy/buildozer`](https://kivy.org/doc/stable/). The TLDR here is

- `beeware` is surprisingly easy both to set up and deploy, and handles a bunch of stuff related to Android emulation. It's a lot more opinionated about what your project file structure should look like, and assumes you initiated the project with `briefcase new`. Also, its widget library seems to [unapologetically be a lot less flexible](https://github.com/beeware/toga/issues/774).
- `kivy`, and in particular `buildozer` is more persnickety to set up, and makes you deal with finding an Android emulator on your own, but is incredibly flexible. It also has this weird, pre-HTML notion that what UI really needs is a weird, domain specific markup different from all the other weird, domain specific markups. Luckily, you can entirely ignore it, and I probably will.

# Beeware

The [tutorial](https://docs.beeware.org/en/latest/tutorial/tutorial-0.html) is a great place to start with this one. It gets you through building a custom trivial app, building it, and running it on an android emulator. It also feels like subsequent builds of the application are much faster than the initial one. If you have a simple application, that happens to fit within [Toga's](https://beeware.org/project/projects/libraries/toga/) constraints, you should absolutely use this, because deploying things is ridiculously easy. I had a basic app up and running on an emulator inside of like twenty minutes, and had it running on my literal phone about twenty minutes after that.

The trouble is that if you want to do things like [have clickable images](https://github.com/beeware/toga/issues/774), this is not the app for you. That link, which I posted twice in this post so far, links to a multi-page discussion from January of 2020 in which [BrendanSimon](https://github.com/BrendanSimon) valiantly tries to convince the Beeware guys that a real cross-platform GUI widget system needs to let people click/tap/whatever on things which aren't always 100% button-shaped, and which sometimes have (gasp) _icons_ instead of or in addition to text. In case you were wondering, the issue is still open, but the framework developers seem ambivalent whether anyone _really_ needs this.

Which, given that I kind of want to do professional-grade work here, rules me out of using this for serious development. Check it out for toys, in case you want to test the waters of putting together a cute Hello World for personal use, or if packaging your project as a `.deb` is more important to you than running it on your phone.

I'm moving along.

# Kivy

`buildozer` is rough. It _has_ [documentation](https://buildozer.readthedocs.io/en/latest/installation.html), and it technically tells you how to install it. However, after hours of trying to get it work directly, and scraping through their `github` and StackOverflow questions trying to figure out why my builds were failing with SSL errors, what I found out is that those installation instructions are incorrect. They tell you to do `pip3 install --user --upgrade buildozer`, but that'll install it in some weird semi-coherent way where `certifi` doesn't have valid SSL certificates hooked into it correctly. What I _actually_ had to do instead was `python3 -m pip install --user --upgrade buildozer`. I'm guessing this is because Ubuntu 22.04 has multiple versions of Python3 installed? I'm not sure what the underlying implications here are other, but the above worked for me.

Once you get installation headaches out of the way, the [Kivy tutorials](https://kivy.org/doc/stable/gettingstarted/intro.html) are also pretty self-explanatory. Unlike beeware, they have an extremely general widget model that lets you do things like specify tappable/swipeable images and do all the mobile app things you're used to. They've even got a separate, even-more-mobile-focused toolkit named [KivyMD](https://kivymd.readthedocs.io/en/1.1.1/) which I might look at if I hit any walls in terms of UI responsiveness. Also unlike beeware, they seem to have a [YAML-based specification language for their UI components](https://kivy.org/doc/stable/tutorials/pong.html#add-simple-graphics)? Honestly, this seems pretty insane, but from what I've seen, you can easily build your own object trees inside of `py` files without bothering with `.kv`s at all. Which is pretty much exactly what I intend to do, but your mileage may vary.

I haven't done any serious work with either of these frameworks yet, but I've got a few projects kicking around my head that I think could benefit from being implemented as desktop/mobile apps, and this exploration is getting me a step closer to actually doing the implementing.

As always, I'll let you know how it goes.
