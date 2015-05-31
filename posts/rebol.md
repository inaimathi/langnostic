One of the things we talk about at the [Toronto](http://www.lisptoronto.org/) Common [Lisp](http://lispwiki.inaimathi.ca/) User [Group](https://groups.google.com/forum/#!forum/toronto-lisp-users-group) meetings is, possibly surprisingly, other interesting languages, whether classical or up-and-coming.

REBOL (pronounced the same as "rebel") is one that got mentioned a few times. And it sounded quite interesting. But I never talked about it here because it was released under a proprietary license, and as you've probably guessed if this blog wasn't evidence enough, I'm a [GPL](http://www.gnu.org/licenses/gpl.html) nerd. Well, as of [REBOL3](http://www.rebol.com/rebol3/), the language is released under the [Apache v2.0 license](http://www.apache.org/licenses/LICENSE-2.0), which officially makes it Free Software. You can find the complete source [here](https://github.com/rebol/r3).

It's a fairly recent development, so this isn't one you can `apt-get install` quite yet. So, here's how you go about building it on Debian.

### Before We Get Started...

You'll obviously need `git` and `make` installed.

```shell
apt-get install git make
```

### Then...

...you'll need to clone [the REBOL3 repo](https://github.com/rebol/r3).

```shell
git clone https://github.com/rebol/r3.git
```

*And then* you'll need to download the `r3` binary from [this page](http://www.rebol.com/r3/downloads.html). If you're on an x86 linux machine, you have a choice of three depending on what version of `libc` you have installed. To find that out, run `ldd --version`<a name="note-Sat-Jul-20-204704EDT-2013"></a>[|1|](#foot-Sat-Jul-20-204704EDT-2013). Once you've go that, [unpack](https://github.com/Inaimathi/shell-ui/blob/master/python/unpack) it, and rename the new `r3` file to `r3-make`.

### On 32-bit machines...

You're pretty much done. Enter

```shell
make make # re-generate the makefile
make prep # generate relevant header files
make      # compile REBOL
```

After a minute or so, you should have a binary file called `r3` that you can add to your path as a REBOL3 interpreter.

### On 64-bit machines...

... you have a couple more things to do. Specifically, you need to run this as root<a name="note-Sat-Jul-20-204707EDT-2013"></a>[|2|](#foot-Sat-Jul-20-204707EDT-2013)

```shell
dpkg --add-architecture i386
aptitude update
apt-get install ia32-libs 
apt-get install libc6-dev-i386
```

That will add the 32-bit versions of libc and some other libraries so that you can actually run the compilation step.

### Now Then

You can find the basic primer [here](http://www.rebol.com/rebolsteps.html), but the thing that most interests me about REBOL so far is its implementation and use of [parse](http://www.rebol.com/r3/docs/functions/parse.html), which you can see demonstrated [here](http://rebol-land.blogspot.ca/2013/03/rebols-answer-to-regex-parse-and-rebol.html), [here](http://rebol.com/docs/core23/rebolcore-15.html) and [here](http://recoding.blogspot.in/2013/02/looking-to-learn-something-new-try-rebol.html), though there have been changes between REBOL2 and REBOL3. You can find the appropriate Emacs mode [here](http://www.rebol.com/tools/rebol.el), and I'm already thinking of the changes I want to make to it. Other interesting documentation includes [the REBOL3 guide](http://www.rebol.com/r3/docs/guide.html), [the list of REBOL3 functions](http://www.rebol.com/r3/docs/functions.html) and [this SO answer](http://stackoverflow.com/a/14184638) which includes a quick REBOL3 CGI script, though really, anything in the [`rebol3` tag](http://stackoverflow.com/questions/tagged/rebol3) is pretty interesting.

* * *
##### Footnotes
1 - <a name="foot-Sat-Jul-20-204704EDT-2013"></a>[|back|](#note-Sat-Jul-20-204704EDT-2013) -Note that if you're just out to *use* the language, and don't really care about any of this Software Freedom business, you've already wasted some time. You can just get the appropriate binary and call it a day. I'm getting it because REBOL3 builds part of itself using REBOL3 scripts. And I'm compiling my own because I like being able to see inside of the languages I use, *and* I'm a big enough nerd to actually do it from time to time, **and** I've probably spent more time than is strictly healthy listening to [Richard Stallman](http://www.youtube.com/watch?v=SNBMdDaYhZA). Proceed or ignore the remaining parts of the process at your discretion.

2 - <a name="foot-Sat-Jul-20-204707EDT-2013"></a>[|back|](#note-Sat-Jul-20-204707EDT-2013) -Thank you user `Fork` from [this thread](http://www.rebol.com/cgi-bin/blog.r?view=0519).
