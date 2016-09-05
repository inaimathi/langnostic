**TL;DR**

```
su -c apt-get install haskell-platform haskell-mode hlint

emacs -nw ~/.cabal/config
### make sure that `-- documentation: True` and `-- library-profiling: True`

cabal install hoogle
~/.cabal/bin/hoogle data
cabal install hasktags
git clone git://github.com/nominolo/scion.git
cd scion
cabal install
```

Then go configure your `.emacs` properly. There; I just saved you some long, tedious, boobless hours.

### The Basics

Installing Haskell itself is extremely easy, assuming you're on a relatively recent version of Debian.

```
apt-get install haskell-platform
```

should handle that nicely. That will install `ghc` (the Haskell compiler), `ghci` (the Haskell interpreter) and `cabal` (the Haskell package manager). Ok, now before you install *anything* else, hop into your favorite editor, open up `~/.cabal/config` and change the options `-- library-profiling:` and `-- documentation:` to `True`. These both default to false, but having them on is preferable for the development process.

The `documentation` flag isn't critical, just nice. It gives you some extra local docs with the libraries it downloads. I'm honestly surprised that `profiling` isn't on by default though. You just plain *can't* profile without it. If you try, you get an error telling you to install the `profiling` versions of all relevant libraries. Here's the kicker though; if you try to install profiling libraries yourself from `cabal` by using the `-p` flag, **it [does not resolve dependencies](http://stackoverflow.com/questions/1704421/cabal-not-installing-dependencies-when-needing-profiling-libraries)**. That means you get to go back through all the libraries you installed, and re-install them recursively by hand. If you do it through the `config` option I mention above, it's automatically done for you whenever you install a new library. Which seems, I dunno, a *bit* better<a name="note-Mon-Sep-17-225932EDT-2012"></a>[|1|](#foot-Mon-Sep-17-225932EDT-2012).

### The Docs

I previously mentioned that [hoogle](http://www.haskell.org/hoogle/) is really useful, but that what you'd really want is a local copy you could search without hitting a server. Well, there is. It's a cabal package you can install with

```
cabal install hoogle
hoogle data ## you may need to run the hoogle binary directly with "~/.cabal/bin/hoogle" instead of "hoogle"
```

That second command will make a local copy of the hoogle database for you. You can then use it to do a text search, like `hoogle map`, or a type signature search like `hoogle "(Ord a) => [a] -> [a]"`. That will give you a long list of results matching your query. You can also use `hoogle --info [search term]` to display the documentation of the first result, rather than a list of results.

Boy, it sure would be nice to have that available from your editor, huh?

### The Editor

If you're not an Emacs user, and are used to indenting things by hand (shudder), pick up [leksah](http://leksah.org/) and be done with it. It's pretty cool, supports incremental compilation out of the box, has some small measure of project management, and performs pretty well. If you're like me, and have gotten used to Emacs handling the tedium of indentation<a name="note-Mon-Sep-17-230239EDT-2012"></a>[|2|](#foot-Mon-Sep-17-230239EDT-2012), you'll want a better solution.

The [default Haskell mode](http://projects.haskell.org/haskellmode-emacs/) is available [standalone](http://projects.haskell.org/haskellmode-emacs/) or from [the Debian repos](http://packages.debian.org/squeeze/haskell-mode). There are apparently some non-obvious config tweaks to make with the standalone version which were done for you in the Debian package, so use the `apt-get` option if you can.

You'll also want to install [Scion](https://github.com/nominolo/scion), which will give you type hints in the minibuffer. Ostensibly, it also gives you goto-definition, and a couple of other small convenience facilities, but I've yet to get that working properly<a name="note-Mon-Sep-17-230827EDT-2012"></a>[|3|](#foot-Mon-Sep-17-230827EDT-2012). Actually do a `git clone` of that github and install it manually, by the way. The version in `cabal` has some dependency oddities that kept it from installing properly on my machine. YMMV, as always.

The last thing I ended up doing, though you may want to stick with the defaults, is [wire up some extra keybindings](https://github.com/Inaimathi/emacs-utils/blob/master/ha-custom.el) for `hlint`<a name="note-Mon-Sep-17-231408EDT-2012"></a>[|4|](#foot-Mon-Sep-17-231408EDT-2012) and hoogle<a name="note-Mon-Sep-17-231413EDT-2012"></a>[|5|](#foot-Mon-Sep-17-231413EDT-2012).

And that's how you set up a Haskell environment. Or, at least, that's how I did. Hopefully, I can now start building some cool things with it.

* * *
##### Footnotes

1 - <a name="foot-Mon-Sep-17-225932EDT-2012"></a>[|back|](#note-Mon-Sep-17-225932EDT-2012) - It's particularly odd when dealing with a fully lazy language, because it seems like that property would make it very difficult to reason about performance a priori. I may talk about that at some point in the future, but you're probably better off reading [what Robert Harper says about it](https://existentialtype.wordpress.com/2012/08/26/yet-another-reason-not-to-be-lazy-or-imperative/).

2 - <a name="foot-Mon-Sep-17-230239EDT-2012"></a>[|back|](#note-Mon-Sep-17-230239EDT-2012) - Especially in languages with significant whitespace like Haskell.

3 - <a name="foot-Mon-Sep-17-230827EDT-2012"></a>[|back|](#note-Mon-Sep-17-230827EDT-2012) - I use [my own](https://github.com/Inaimathi/emacs-utils/blob/master/tagariffic.el) anyway.

4 - <a name="foot-Mon-Sep-17-231408EDT-2012"></a>[|back|](#note-Mon-Sep-17-231408EDT-2012) - Inspired by [this gist by Sam Ritchie](https://gist.github.com/1241059).
5 - <a name="foot-Mon-Sep-17-231413EDT-2012"></a>[|back|](#note-Mon-Sep-17-231413EDT-2012) - Not really inspired by anything, but extending the default `haskell-mode` functionality which only lets you do the initial documentation search, rather than viewing.
