I've recently installed `nix` and moved a few of my packages to it. By which I mean "I ran `apt-get remove foo` followed by `nix-env -i foo` a few times". There may be a smarter way, but I didn't bother with it. Installing the package manager is pretty painless.

```shell
mkdir -m 0755 /nix && chown inaimathi /nix ## As root. Also, substitute your own username for 'inaimathi'
curl https://nixos.org/nix/install | sh
```

Preliminary thoughts on `nix` are good. At this point, I've got `firefox`, `ghc`(and a bunch of Haskell libraries), `dmenu`, `ocaml`, `smlnj`, `rustc`, `go`, `guile` and `gfortran` installed through it. I figure since it's so easy, I may as well. So far, it has *not* been necessary to use what I see as "the main feature" to roll back my environment, because nothing has broken yet. I *have* had occasion to install/uninstall/reinstall a particular package. It looks like, barring a [garbage collection](https://nixos.org/wiki/Install/remove_software#Garbage_collection), you only need to download things the first time. On subsequent re-installs, `nix` just re-establishes various symlinks around your environment.

The negatives are basically small usability issues that pop up all over the place, some of which I'm putting down to lack of familiarity with the tool.


- The [documentation](https://nixos.org/wiki/Main_Page), while available, is very light on practical examples of getting something done. For instance, despite [having](http://www.cse.chalmers.se/~bernardy/nix.html) perused [several](http://fluffynukeit.com/setting-up-a-haskell-project-on-nixos/) tutorials [that](http://lethalman.blogspot.ca/2015/02/developing-in-golang-with-nix-package.html) mention [it](https://ocharles.org.uk/blog/posts/2014-02-04-how-i-develop-with-nixos.html), I'm still not entirely sure how to use `nix-shell`. Searching for "nix-shell" on the official wiki gives you a "Create this page!" link as of this writing.
- Writing [a package](https://nixos.org/wiki/Contributing_to_nixpkgs) is an arcane process. I've barely worked through putting together the tutorial package for GNU Hello, and feel nowhere near competent enough to write my own yet. I only mention this because there seems to be an assumption that every piece of software you write or install with `nix`, including language libraries, should be installed via packages, which implies this is the sort of baseline knowledge any `nix` user should have.
- `nix-env -i` can be a bit snippy about package names. This has been a relatively common pattern for me:
```shell
$ nix-env -i parsec
error: selector 'parsec' matches no derivatives
$ nix-env -i haskell-parsec
error: selector 'haskell-parsec' matches no derivatives
$ nix-env -i haskell-parsec-ghc7.8.4
installing ...
```
- on top of that, there seems to be a fairly even split amongst the mentioned tutorials regarding whether to use the package name or attribute name for this operation. That is, whether you should use `nix-env -i ghc-7.8.4` or `nix-env -iA nixos.pkgs.haskellPlatform.ghc`. There doesn't seem to be an obvious mapping between them, and it's unclear to me which form is preferable in which situations.
- The `nixpkgs` repo has a flat file [here](https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/all-packages.nix) that contains all `nix` expressions in alphabetical order. As of this writing, said file is `13666` lines long. Something tells me this probably won't scale very well. It also seems like running `nix-env -i foo` evaluates every expression in that file that occurs `foo` in an effort to install.


So, over all, I'm not sure how I feel about it yet. I do very much like having certain pieces of my software toolchain update more often than the Debian repos do by default, but I haven't quite gotten to see the development benefits. My goal for the next little while is doing a thorough reading/following of [these](https://ocharles.org.uk/blog/posts/2014-02-04-how-i-develop-with-nixos.html) two [language-specific](http://lethalman.blogspot.ca/2015/02/developing-in-golang-with-nix-package.html) tutorials. Hopefully, that'll give me deeper insight about where `nix` helps.
