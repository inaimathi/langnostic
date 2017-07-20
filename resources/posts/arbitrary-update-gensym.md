As I mentioned in my previous post, I recently pressed my old [x220i](https://www.notebookcheck.net/Lenovo-ThinkPad-X220i.56185.0.html) back into service. Which involved buying a [new 9-cell battery](https://www.amazon.ca/Willino-Replacement-Battery-0A36282-ThinkPad/dp/B01LYNB6ZN/ref=sr_1_2?ie=UTF8&qid=1500557420&sr=8-2&keywords=lenovo+x220+battery)[^thoroughly-recommend] for it, since the old one had long since died of disuse. I've put [Debian](https://www.debian.org/distrib/) on it again via my [`machine-setup` project](https://github.com/inaimathi/machine-setup), and it turned out to be almost trivial. In fact, since the last time I pulled this stunt, Debian seems to ship with [Free](https://www.gnu.org/philosophy/free-sw.en.html) drivers for my wireless card, which means that this machine is just as Free as the Purism I was recently using.

[^thoroughly-recommend]: Thoroughly reccommend, by the way. `acpi` says `9:58:00 remaining` at full charge, which is more than enough for any shenanigans I could possibly want to get down to.

The non-trivial bits were, as usual, `nix` changes. The `rxvt-unicode` project is no longer a thing, apparently, and the latest `emacs` version has incremented. And for some reason, I never did get around to writing a `setup.el` the last time I ran these scripts. Sort of done. I decided instead to have `emacs` ensure that the relevant packages are loaded on each restart. That happens in [`packs.el`](https://github.com/inaimathi/machine-setup/blob/master/packs.el).

```lisp
(require 'package)

(add-to-load-path (list-subdirectories "~/.emacs.d/elpa"))

(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
 	("marmalade" . "http://marmalade-repo.org/packages/")
 	("elpa" . "http://elpa.gnu.org/packages/")))

(defvar +package-list+
  '(elfeed
    aes
    magit highlight-parentheses autopair smart-tab
    auto-complete yasnippet paredit
    markdown-mode haskell-mode))

(unless (every #'package-installed-p +package-list+)
  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (p +package-list+)
    (unless (package-installed-p p)
      (package-install p))))

(provide 'packs)
```

The `+package-list+` variable is the place I'll be adding libraries as I decide I need them, and the `unless` block installs each of the listed packages unless they're already installed[^and-also].

[^and-also]: And also is careful not to attempt a network request unless there's some missing package there. I prefer my editor to start up relatively quickly.

It feels really good to be piloting this thing again.

I'm not sure where the idea came from that chicklet-style keyboards are the way to go for manufacturers, but I absolutely prefer the [Lenovo classic](http://laptoping.com/no-more-classic-thinkpad-laptop-keyboards.html) that this thing has.

![The Lenovo classic keyboard that I dearly hope gets resurrected at some point, because its ostensible replacement is complete garbage](/static/img/lenovo-classic-keyboard.jpg)

The machine is also much smaller and lighter than the behemoth I've been carting around, and would be even moreso if I didn't want the extra long battery life from the heftier 9-cell.

![A top-down shot of my laptop](/static/img/top-down-laptop.jpg)
![A closeup view of my shiny new 9-cell battery](/static/img/battery-closeup.jpg)

To top it all off, the trackpad is less sensitive to random touches, so it's _actually usable_ without an external keyboard and mouse combo. It's almost absurdly better across every dimension except for processor power and memory capacity, despite being years older than what I'd been using. Which is exactly why I'm much happier to be carrying this one around again.

And so unburdened, I hope to resume writing.
