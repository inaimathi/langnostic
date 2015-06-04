## <a name="hardware" href="#hardware"></a>Hardware

This is a [thing](https://www.crowdsupply.com/purism/librem-laptop) now. And it wasn't for a very long time. I remember sitting down and honestly trying to figure out how I would go about getting a [Yeeloong](http://www.tekmote.nl/epages/61504599.sf/en_GB/?ObjectPath=/Shops/61504599/Products/CFL-003-B) to Toronto. Eventually concluding that my best bet would be to learn [Mandarin](https://en.wikipedia.org/wiki/Mandarin_Chinese), then hang out around Jiangsu-based chatrooms/IRC channels and try to to make friends. This seemed like more work than I could realistically do, so I didn't. Instead I've been piloting a Lenovo x220i and putting up with this

```
~ $ vrms
                Non-free packages installed on self

firmware-ralink                     Binary firmware for Ralink wireless cards
firmware-realtek                    Binary firmware for Realtek wired and wireless network

  2 non-free packages, 0.1% of 1672 installed packages.
~ $ 
```

Anyhow. That [first link](https://www.crowdsupply.com/purism/librem-laptop) I put up [earlier](https://www.crowdsupply.com/purism/librem-laptop) takes you to a CrowdSupply page that promises to ship a fully Free(Libre) 15-inch laptop in another couple of months. I've already been looking for a new machine for a year or so, and this is officially cool enough to get me to pull the trigger. So I've ordered one in the interests of being the canary; I'll let you know how it runs once I get mine, at which point I expect I'll be able to recommend it thoroughly.

To be clear, the goal here isn't "get a good deal on a laptop". This thing is going to cost me something like three or four hundred bucks above what I'd pay for a Lenovo piece. But. I'm committed enough to the idea of freedom in computation that I'll willingly pay that in order to subsidize a company with the same goals.

## <a name="software" href="#software"></a>Software

On the other side of the development coin, "purely functional" [build systems](http://nixos.org/nix/)/[package managers](http://www.gnu.org/software/guix/) are becoming a popular meme.

I mentioned [last time](http://blog.inaimathi.ca/article?name=i-liiiiive.html) that I was seriously considering getting a separate, frequently-snapshotted, VM for the purposes of Haskell development. To that end, I briefly looked at [Qemu](http://wiki.qemu.org/Main_Page) and [Docker](https://www.docker.com/), before hearing about [`nix`](http://nixos.org/nix/). And shortly thereafter [`guix`](http://www.gnu.org/software/guix/manual/guix.html). The idea behind both of these package managers<a name="note-Sun-Feb-08-124221EST-2015"></a>[|1|](#foot-Sun-Feb-08-124221EST-2015) is that you never throw away a package. That is, when you "upgrade" a library or piece of software, you build the new version from scratch, sharing identical pieces but building any new dependencies *non-destructively*. Which I assume is the reason they both use the "purely functional" label, despite the fact that both clearly write information to disk, but I digress.

What this gives you is two things.


-   First, because you still have the older versions of everything around, it's easy to revert back to a known-working state of the world. This sounds extremely useful when it turns out that the new version of [`haste`](https://github.com/valderman/haste-compiler) needs you to re-install half of fucking [`hackage`](http://hackage.haskell.org/) and still won't run. 
-   Second, because a particular build is self-contained and always internally consistent, you can [easily share build artifacts](http://nixos.org/nix/manual/#ssec-binary-cache-substituter) across several local machines. That previous link was `nix` specific, but according to the [`guix` Requirements listings](http://www.gnu.org/software/guix/manual/guix.html#Requirements), you can run `guix` as a front-end on a `nix` store, so you could use it either way. This sounds useful in the situation where you need to bring up a new machine with the same software stack; if you can share between local machines that have any particular piece installed, you can do it faster and with less external traffic.


Those two things are exactly the goals I had when looking at `qemu`/`docker` for Haskell development. Which means that for my purposes, `nix`/`guix` directly obsoletes any kind of virtual machine setup. This isn't a general property, so don't let it stop *you* from putting some reading time into [Xen](http://xenproject.org/), but *I* have very little reason to in the near future.

Choosing between `nix` and `guix` is another matter. As I mentioned, the [`guix` docs](http://www.gnu.org/software/guix/manual/guix.html) imply that I won't have to. And they both take the same general approaches to building software. The big difference, as far as I'm concerned, is that the [`guix` extension/package-definition language](http://www.gnu.org/software/guix/manual/guix.html#Defining-Packages) is actually [Guile](https://www.gnu.org/software/guile/), as opposed to a really weird-looking [mix of JavaScript, Perl and Haskell](https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/window-managers/stumpwm/default.nix#L42).

My GNU fanboyism and affection for parentheses is well documented, so I expect that predicting the outcome isn't hard here. Still, I plan to try both of them out to see how they feel. As always, I'll let you know how it goes.


* * *
##### Footnotes

1 - <a name="foot-Sun-Feb-08-124221EST-2015"></a>[|back|](#note-Sun-Feb-08-124221EST-2015) - Both of Which have had full-on linux distros built around them, if that's your thing. [Nixos](http://nixos.org/) for `nix` and ["Guix System Distribution"](http://www.gnu.org/software/guix/manual/guix.html#System-Installation) for `guix`.
