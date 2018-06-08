So I got a Lenovo A275 as my next machine. If my experience with the [Librem](https://puri.sm/products/librem-15/) had been any better at all, I'd have [stuck with them](https://puri.sm/products/librem-13/). Unfortunately, the laptop was something a year late in arriving, had a bunch of fit-and-finish headaches, and managed to last all of two years total before just about falling apart[^specifically-it-failed]. To the point that I'd ended up just retrofitting and using my Lenovo x220i again for a while. Unfortunately, its wifi card died, which prompted this latest purchase.

[^specifically-it-failed]: Specifically, the case was coming apart in a couple lpaces, the screen broke, and shortly thereafter the network card crapped out. I have no illusions about a Lenovo being easier to repair, but I've had the same [x220i](https://www.notebookcheck.net/Lenovo-ThinkPad-X220i.56185.0.html) for [about seven years as of this writing](/posts/x220-and-unrelatedly-portable-keyboards). And while its' wifi is now gone, it still had a better keyboard, a better battery life, and, critically, _didn't fucking fall apart_.

So far, I like it.

There were absolutely some installation headaches this time around, but I'm running on it right now without too much trouble.

## UEFI Bullshit

Firstly, this is the first laptop I've had that came with [this UEFI bullshit attached](https://www.fsf.org/campaigns/campaigns/secure-boot-vs-restricted-boot/). Which means that the BIOS is primed to only run signed OS installations, which by default is fucking Windows. So, first order of business was to hop into the laptop setup routine and change that setting. The options I had were `Allow any OS` or `Allow only unsigned OSes`, so you _know_ I went for that one. So far so good.

## Monitor Bullshit

By default, installing just `debian` the command-line didn't let me do anything on the built-in display. The installation would run fine and seem to conclude properly, but when I'd run it, all it would let me do is decrypt my storage volume, then drop me into a purely black screen. I had to install `xfce4` as a base UI to get anything actually happening. I kind of assume there's something up with the video drivers I've got access to here, because that wasn't the end of it. The `hdmi` port on the side of this unit also refuses to recognize any external display. Which means that I've effectively got a single-monitor machine until I resolve this.

## WIFI Bullshit

There are no [Debian-available wifi drivers](https://wiki.debian.org/WiFi) for this machine. Which meant that in order to do anything with _the internet_, I had to buy [this](https://www.amazon.ca/TP-Link-Wireless-Adapter-Archer-T2UH/dp/B00UZRVY12/ref=sr_1_29?s=electronics&ie=UTF8&qid=1528493568&sr=1-29&keywords=usb+wifi). Or pray that every place I'd be computing had a wired connection to a router, I guess. That second noe doesn't sound like much of a solution.

## `dmenu` Bullshit

The last bit I had was this problem where `nix-env`-installed binaries wouldn't appear on my main path for some reason. Or rather, they wouldn't show up as name targets for [`dmenu_run`](https://wiki.archlinux.org/index.php/dmenu), which is how I've preferred to run programs on my machine for a good eight years now. The real reason ended up being a `dmenu_run` cache file that was generated before I added my `nix` environment to `.xsessionrc`.
- need to clear `dmenu_run` cache because it cached a non-`nix` supporting binary env for me, and I was thoroughly confused about what to do here until I read a comment at the bottom of [this page](https://faq.i3wm.org/question/4973/config-dmenu-not-picking-up-user-path.1.html).

That's been it so far. I still haven't resolved the HDMI issue, but I intend to because _until_ I do, my extra monitor is pretty useless. I'll let you know how it goes.
