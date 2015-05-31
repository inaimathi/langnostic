As much as I like sitting at post number 42, it's time for an update.

I've been testing another distro of linux (well, not another distro, really. An Ubuntu derivative called Crunchbang), and I really thought it was going to unseat Ubuntu as my OS of choice. The main killer, it turns out, was xorg.conf.

First, the good stuff. It's simple, it has better keyboard support for launching programs than Ubuntu, and its performance is through the roof. It actually flies on the toy laptop I have lying around for this kind of experimentation (a Compaq Presario R3000 I picked up for $20 about two years ago. It has a 1.4ghz processor, a whopping 256 megs of ram and a 5400rpm, 30gb hard drive). Even with those specs, Crunchbang is usable. So I was all psyched up to install it on my netbook to get a bit more performance out of it, just because I do actually do some development on it when I'm out and about. Two things are making it unacceptable though.

One, and this is the main one, xorg.conf. To the Linux veterans this is probably a joke, but it's pretty difficult for the newbs. There was sparse information on configuring monitors this way back when it was mandatory, and you could never be sure that you were pasting the appropriate things because, apparently, different distros had different conventions about it. When I searched (and I mean everywhere; old linux sites, the various StackExchange sites, the Ubuntu docs and the Ubuntu forums), all I found were horribly outdated pointers, and one or two comments about how "xorg.conf isn't really used anymore, just use xrandr". Which is great, except that xrandr doesn't seem to auto-detect additional monitors in Crunchbang, so I still need to know how to configure them the old way. The other common instruction I found was to copy-paste existing parts of your current conf file. Which I'm sure was great advice at one point, but current Ubuntu/cruchbang xorgs look something like

```

Section "Device"
        Identifier      "Configured Video Device"
EndSection

Section "Monitor"
        Identifier      "Configured Monitor"
EndSection

Section "Screen"
        Identifier      "Default Screen"
        Monitor         "Configured Monitor"
        Device          "Configured Video Device"
EndSection
```

which doesn't seem to contain useful information. All it tells me is that all these options are now configured elsewhere, and I have no idea where that is. The other big problem with xorg.conf editing is that you have to restart the X server each time you want to test it out, and (at least on Crunchbang and Ubuntu) the error messages you get aren't exactly useful. They basically say "there was an error in your xorg.conf". 

Thank you. Which section and line would be useful. Even if you drill down further into the error log, it typically (again, for me at least) just said that the error was a missing EndSection on the last line, even though there were no missing EndSections, and the last line consisted of nothing else. It took me something like two hours to track down enough info about the basics to get the screen mirrored on an external monitor (before that, the extra screen was just displaying various seizure-encouraging light patterns). Xrandr still doesn't see the extra monitor by the way, so I still have to muck about further with xorg.conf.

Second, and this is the tiny problem almost not worth mentioning, but it's not an issue in Ubuntu. It doesn't have apt-get access to emacs23. If I want to apt-get install emacs, it gives me version 22.1 or so. I could spend a bit of time figuring out how to script a wget to the GNU ftp site, then run the appropriate commands to untar and install it properly (it's probably just make and make install, though I've never had to do it so I don't know), but my fucking around time-budget is officially used up for this week.

So there. That's why I'm keeping Ubuntu 10.x. For what it's worth, until I hit the iceberg that is xorg.conf, this was going to be a blog post about why I switched. Seriously, I had notes ready and everything. So if you don't need more than one monitor for whatever reason, or you're a xorg wizard and still on Ubuntu, give it a try.

Other than my OS experimentation, I've also been toying with the formlet system I shamelessly lifted out of PLT Racket (still at the [GitHub page](http://github.com/Inaimathi/formlets), but now there's documentation), and I also released that CSS module I wrote about a little while back. I honestly wasn't going to, but those 12 or so lines of code found their way into every single project I've worked on since. Code reuse is good, I hear. The project page is [here](http://github.com/Inaimathi/cl-css), and it is likewise accompanied by a nice little Markdown-enabled documentation sheet.

The big additions since you last saw these projects are

### Formlets now support recaptcha.

You like how I'm adding high level support like this before the thing can even generate checkboxes, huh? Well, it might be a bit dumb, but I feel no shame in admitting that the order of operations is self-centered. Which is to say, I add features as I need them, not as they make sense in theory. It just so happens that I had call for text inputs, textareas, passwords and recaptcha before one of my projects called for checkboxes, radio buttons or select boxes. I'm sure it'll change soon.

### CSS compilation

This one's obviously from the css generator. Basically, a task called for a several-hundred-line stylesheet. While it wasn't performing too badly, I realized that eventually, there would be a call for CSS that you could cache (as opposed to the inline styles then generated by cl-css). So, I added compile-css, which takes a file path and some cl-css-able s-expressions and outputs them as flat CSS. Tadaah! If you're running Hunchentoot (or any other server really, cl-css is simple enough that I can confidently call it portable) from behind Apache or nginx, you can now have the front-facing server serve out a flat-file stylesheet and still get the benefit of declaring your CSS more succinctly in Lisp.

### Documentation

Like I said earlier, there's some simple docs included with both projects which you can thank GitHub for. The main reason I wrote them was seriously because there's this little notice that tries to guilt you into putting in a README if you don't have one already. It supports markdown, textile, rdoc, and a bunch of other psuedo-markup languages meant for displaying plaintext while still supporting automatic conversion to HTML. Mine are written in markdown, mainly because I'm already familiar with it, and I managed to find a decent Emacs editing mode for it.

### ASDF-ability

I'm probably late to this party, given that [quicklisp](http://www.quicklisp.org/) is now in public beta stages, but I set up ASDF packages for both formlets and cl-css, and hooked them up to the [CLiki](http://www.cliki.net/index). So unless it (the CLiki, I mean) goes down again, you can install them both by doing

```lisp
(require 'asdf)
(require 'asdf-install)
(asdf-install:install 'cl-css)
(asdf-install:install 'formlets)

```
