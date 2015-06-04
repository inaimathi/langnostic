First, lets get the useful information out of the way. I've been using [the fantastic `paredit`](http://emacswiki.org/emacs/ParEdit) to edit Lisp code for a while now, and [the slightly-less-fantastic-but-generally-useful `autopairs`](autopairs) to help with parentheses/curlies/quotes/what-have-you in other languages<a name="note-Sun-Jan-06-181038EST-2013"></a>[|1|](#foot-Sun-Jan-06-181038EST-2013). There's a little [blurb on the Emacs wiki](http://www.emacswiki.org/emacs/AutoPairs#toc5) page about using the two of them together, which implies that `(autopair-global-mode)` should automatically respect `paredit`s primacy in Lisp modes, but that doesn't seem to happen. When I tried editing Elisp, or Common Lisp or Clojure with that fix in place, I got some odd edge-case behavior.

Specifically, highlighting a region to parenthesize it produced an extraneous close-paren and backspace suddenly didn't balance deletions. I'm not sure this is the most elegant way forward, but the way I wound up fixing it is

```lisp
(defvar *paredit-modes*
  '(common-lisp-mode lisp-mode emacs-lisp-mode clojure-mode lisp-interaction-mode)
  "A list of modes wherein I use paredit.")

(require 'autopair)
(autopair-global-mode)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(defun custom-paredit-mode ()
  (progn (paredit-mode +1) 
         (define-key paredit-mode-map (kbd "&lt;C-left&gt;") 'backward-sexp)
         (define-key paredit-mode-map (kbd "&lt;C-right&gt;") 'forward-sexp)))

(dolist (mode *paredit-modes*)
  ;; Activate paredit and deactivate autopair in lisp modes
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda ()
              (custom-paredit-mode)
              (setq autopair-dont-activate t)
              (autopair-mode -1))))
```

`*paredit-modes*` is just a list of language modes where I want `paredit`, and therefore **not** `autopair`. I want `autopair` `global`ized because I use it in every mode where I'm *not* using `paredit`, so it's much easier to set exceptions than to exhaustively list ever non-lisp language mode. After both modes are included, I iterate over `*paredit-modes*` with a `dolist` and set the appropriate hooks.

Right, that's the useful part out of the way. I'm going to bitch now, and I'd appreciate some privacy.

### <a name="bitching" href="#bitching"></a>Bitching

I'm starting to feel bored with what I'm doing. The last time I felt this way was back in college, where I suddenly went from producing two finished illustrations and about ~30 sketches per week to just under .01 and 2 respectively. I doubt the same thing is going to happen with programming because of what I think programming *is*, but it's still kind of disconcerting that I'm emotionally beyond my own control. Over the past couple of months, I've been working pretty hard on three or four personal projects and one giant project at work, most of it in Python.

That's kind of depressing. Not that Python is a horrible language, mind you, it's ok and it's terse enough once you get to grip with its particular way of doing things, and *most* of its libraries aren't as over-engineered as the stuff I've found myself needing to include over in Clojure-land, but it [lacks](http://dev.clojure.org/display/design/Library+Coding+Standards) a [feature](http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm#defmacro) or [three](http://www.aiai.ed.ac.uk/~jeff/clos-guide.html) that [I've](http://www.haskell.org/haskellwiki/Partial_application) gotten [very](http://learnyouahaskell.com/syntax-in-functions#pattern-matching) used [to](https://github.com/technomancy/leiningen). You'll note that those links don't all point to things from the same language<a name="note-Sun-Jan-06-181326EST-2013"></a>[|2|](#foot-Sun-Jan-06-181326EST-2013).

I remember reading about something called [The Curse of the Traveller](http://www.reddit.com/r/IWantOut/comments/zykw2/getting_out_and_what_it_means_to_me/c68uit9) a little while ago. It seemed like an interesting concept, but not one I'd be able to relate to since I don't actually *like* traveling<a name="note-Sun-Jan-06-181347EST-2013"></a>[|3|](#foot-Sun-Jan-06-181347EST-2013). It feels painfully relevant here though. Re-phrasing it for the language enthusiast

> The more languages you learn, the more things you see that appeal to you, but no one language has them all. In fact, each language has a smaller and smaller percentage of the things you love, the more languages you learn. It drives you, even subconsciously, to keep looking, for a language not that's perfect (we all know there's no Shangri-La), but just for a language that's "just right for you." But the curse is that the odds of finding "just right" get smaller, not larger, the more you experience. So you keep looking even more, but it always gets worse the more you see.

In theory, language users have an out because we can technically build a "just-right" language. In practice, it turns out that implementing your own garbage collection, optimizing compiler, streams, i/o, package management, web-server, asynchronous web-server etc. is a lot harder than just living with a reasonably popular language you already know<a name="note-Sun-Jan-06-181401EST-2013"></a>[|4|](#foot-Sun-Jan-06-181401EST-2013). Also, there are very few things lonelier than being the only one who knows your language of choice. The end result is that, while I've probably been getting more stuff done in Python/JavaScript than I'd ever gotten done in a comparable time-period, I've been getting skull-fuckingly bored doing it. And I'm really not sure what the solution is.

### <a name="realistically" href="#realistically"></a>Realistically

This is the effect of having a single quarter wherein I


-   had a child. I mean, my wife *had* him, but I still lost plenty of sleep as a result.
-   got sick enough that I couldn't exercise for a good week and a half.
-   went through the holiday garbage typical of the season.


Any one of those could probably put me in a funk on its own. The combination, as well as the evil fucking grey winter that's just descending on Toronto streets, is enough to explain the mood and then some. So I mean, that's that. I was planning on starting another project or two in the next little while, but all things considered, maybe I owe myself a bit of relaxation for a change. Metaphorically spin down the drives and kick my feet up, at least 'till I get the chance to get a good sprint or two and a few winks in.


* * *
##### Footnotes

1 - <a name="foot-Sun-Jan-06-181038EST-2013"></a>[|back|](#note-Sun-Jan-06-181038EST-2013) - I still don't use `[auto-pair+](http://www.emacswiki.org/emacs/auto-pair%2b.el)`, but you should feel free to.

2 - <a name="foot-Sun-Jan-06-181326EST-2013"></a>[|back|](#note-Sun-Jan-06-181326EST-2013) - Although, as usual, it wouldn't be very difficult to put together 90-95% adequate versions of the rest if you have access to `defmacro`, which is why Common Lisp tends to be my language of choice if I have any say in the matter at all. As proof, I submit the article count by language over there in the sidebar.

3 - <a name="foot-Sun-Jan-06-181347EST-2013"></a>[|back|](#note-Sun-Jan-06-181347EST-2013) - I got that shit out of my system before I turned 12, and I have no more desire for it, thank you very much.

4 - <a name="foot-Sun-Jan-06-181401EST-2013"></a>[|back|](#note-Sun-Jan-06-181401EST-2013) - Or picking up a Lisp that hits enough of the bases then `defmacro`ing the additional facilities you need.
