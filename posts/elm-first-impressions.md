For the past little while, I've been poking around a new language named [Elm](http://elm-lang.org/). A Haskell-like web front-end language with a heavy focus on [FRP](http://en.wikipedia.org/wiki/Functional_reactive_programming). Actually, no, it's not *like* Haskell, its [syntax](http://elm-lang.org/learn/Syntax.elm) *is* Haskell except for a few [omissions](http://elm-lang.org/learn/Syntax.elm#things-not-in-elm)<a name="note-Mon-Jun-17-232836EDT-2013"></a>[|1|](#foot-Mon-Jun-17-232836EDT-2013), a [couple](http://elm-lang.org/learn/Syntax.elm#type-annotations) justifiable [small](http://elm-lang.org/learn/Syntax.elm#records) changes, and a couple pointlessly gratuitous differences<a name="note-Mon-Jun-17-232840EDT-2013"></a>[|2|](#foot-Mon-Jun-17-232840EDT-2013). To the point that the actual, official [recommendation](http://elm-lang.org/Download.elm) is to just use [Haskell mode](http://projects.haskell.org/haskellmode-emacs/) to edit Elm files.

This works pretty well, except for one thing: Elm has a [built-in reader macro for Markdown](http://elm-lang.org/edit/examples/Elements/Markdown.elm) input. Using this feature in Haskell mode plays all kinds of hell with your indentation and highlighting. Enough that I thought it worth-it to hack a workaround in using [`two-mode-mode`](http://www.welton.it/freesoftware/files/two-mode-mode.el). This is far from ideal, but bear with me. You need to get `two-mode-mode` from that previous link, do a search/replace for `mode-name` into `major-mode`, and delete the line that reads `(make-local-hook 'post-command-hook)`. Then, you have to add the following to your `.emacs` somewhere:

```emacs-lisp
(require 'two-mode-mode)
(setq default-mode (list "Haskell" 'haskell-mode)
      second-modes (list (list "Markdown" "\[markdown|" "|\]" 'markdown-mode)))
```

and then run `two-mode-mode` whenever you're editing `.elm` files. The end result is that, whenever you enter a `markdown` block with your cursor, your major mode will automatically change to `markdown-mode`, and change back to `haskell-mode` when you leave. There *has* to be a better solution than this, probably involving one of the other [Multiple Modes modules](http://www.emacswiki.org/emacs/MultipleModes), and I'll put some thought into it when I get a bit of time.

### Installation/Basics

Installing is ridiculously easy. If you've ever installed a module for Haskell, you won't have trouble. It's just `cabal update; cabal install elm elm-server`. Do the `update` first, like it says there; the language hasn't reached `1.0` status as of this writing, which means that it's quite likely there will be significant changes by the time you get around to following these instructions.

You write code into `.elm` files, which you can either preview dynamically or compile. You do the dynamic preview thing by running `elm-server` in your working directory. That starts up a server listening on `http://localhost:8000` that automatically compiles or re-compiles any `.elm` file you request. That server runs on [Happstack](http://www.happstack.com/page/view-page-slug/1/happstack), and does a good enough job that the official [`elm-lang` site](http://elm-lang.org/) seems to serve directly from it.

If you're like me though, you prefer to use static files for your actual front-end. You can use `elm --make --minify [filename]` to generate a working `.html` file<a name="note-Mon-Jun-17-232850EDT-2013"></a>[|3|](#foot-Mon-Jun-17-232850EDT-2013) that you can serve up along with the `elm-runtime` from whatever application server you want to use.

Enough with the minutia though. Really, I'm here to give you a paragraph or two on what I think about the language.

### What I think about the Language

The usual disclaimers apply.


- you'll easily find more people who are familiar with JS/HTML than those who are familiar with Elm
- if you use it, there's an extra<a name="note-Mon-Jun-17-232855EDT-2013"></a>[|4|](#foot-Mon-Jun-17-232855EDT-2013) abstraction layer between you and the final front-end
- using it forces your users to enable JavaScript. Ostensibly, you can use the compiler to generate `noscript` tags, but all these seem to do is statically document what the page *would* do if JS was on.


That second one in particular means that once again, you really should learn JavaScript before trying to use Elm to save yourself from it.

Once you get past that, it's quite beautiful and elegant. Much better than plain JS for *some* front-end work. Not that that's a very high bar.

There's some stuff conspicuously missing, like my beloved [SSEs](http://www.w3schools.com/html/html5_serversentevents.asp), and some basic DOM interactions including `[draggable](http://jqueryui.com/draggable/)` and an arbitrary, [element-triggered `click`](http://www.w3schools.com/jsref/event_onclick.asp) event. The approaches available out-of-the-box are respectively, [Drag Only One Element That You Can't Drop](http://elm-lang.org/edit/examples/Reactive/Transforms.elm) and [Detect Mouse Location On A Click, Then Dispatch Based On It](http://www.grzegorzbalcerek.net/elm/TicTacToe.elm). Neither of those seem very satisfying. In fact, the proposed workarounds look strictly worse to me than the ["callback hell" this language is trying to save me from](http://elm-lang.org/learn/Escape-from-Callback-Hell.elm).

Those shortcomings are just getting me more interested, to be honest. The reason being that it looks like it's possible to implement additional [native functionality](https://github.com/evancz/Elm/tree/master/libraries/Native) fairly easily, so all it'll do is cause me to spend some time writing up the appropriate, signal-based libraries to do these things.

Overall first impressions so far are good, though I'm seriously questioning how useful this language is going to be for more complicated interfaces. In the short term, I'll test out its shallow limits by writing a new [WebMote](https://github.com/Inaimathi/web-mote) front-end.

I'll let you know how it goes.

* * *
##### Footnotes

1 - <a name="foot-Mon-Jun-17-232836EDT-2013"></a>[|back|](#note-Mon-Jun-17-232836EDT-2013) - Which I'm pretty sure will eventually be addressed. I particularly miss full sections and `where`, though you'd think the multi-line function declarations would be the biggest gap.

2 - <a name="foot-Mon-Jun-17-232840EDT-2013"></a>[|back|](#note-Mon-Jun-17-232840EDT-2013) - For no reason I could see, `:` is Elm's type annotation operator, while `::` is Elm's `cons`. It's precisely the opposite in Haskell, and buys little enough that I hereby formally question the decision. Similar reasoning seems to apply to the operator `<|`, which seems to do exactly the same thing as Haskells' `$`, except that it's twice as long.

3 - <a name="foot-Mon-Jun-17-232850EDT-2013"></a>[|back|](#note-Mon-Jun-17-232850EDT-2013) - Or separate `.html` and `.js` files, if you also passed the `-s` flag.

4 - <a name="foot-Mon-Jun-17-232855EDT-2013"></a>[|back|](#note-Mon-Jun-17-232855EDT-2013) - Not particularly stable, yet.
