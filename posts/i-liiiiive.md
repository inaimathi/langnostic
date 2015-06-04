That... felt like a fucking year.

I'm well aware that I've been away from the blog for about a month at this point, but it felt like a fuckton longer. Not that I haven't been programming, mind you. I've been relatively busy, as you can see by [my github](https://github.com/inaimathi) history. I guess that's what happens when programming is one of your stress-release activities.

Anyhow; now that my mind is a-twirl, my new spawn is home again, and my nose has shed the traditional chitinous exoskeleton of the winter season, lets take a look at what I've been thinking about.

## <a name="putting-my-parens-where-my-mouth-is" href="#putting-my-parens-where-my-mouth-is"></a>Putting my Parens where my Mouth Is

`[cl-notebook](https://github.com/Inaimathi/cl-notebook)` got a bit more work thrown at it. One or two minor issues fixed, and we've got a working macroexpander now. That's basically it in terms of work *on* it. As for work *with* it, I've started writing an HTML5 edition of Lemonade Stand, and I'm thinking about a few other web games/creativity toys. Its shaken out a few obvious features that I'd want from a proper IDE, and a few obvious bugs that I need to squish. Including, but not limited to:


-   **A standard-output, and/or logging buffer** of some sort would be very useful when debugging web applications. That way I could keep track of the request logs as I'm working. This time, I ended up running `cl-notebook` through [`SLIME`](http://common-lisp.net/project/slime/), kicking [`house`](https://github.com/Inaimathi/house) into debug mode, and using the REPL as my log buffer. It worked ok, but the whole point of `cl-notebook` is that the user not need Emacs to work productively in Common Lisp. So that's gotta change.
-   More advanced **s-expression navigation**. We've already got movement by s-expression, but we don't yet have transpose, slurp or barf, and I've officially noticed the pain. On a related note, the existing s-exp navigation doesn't handle quoted forms properly. Which is a bug. Not too serious, but I need to squish it, because it causes more annoyance than it should.
-   Some better **system for dealing with static assets**. Maybe a keystroke that uploads a local file and inserts its relative URL at point. The lemonade stand clone has a few images that I want to serve up along with the code, and the way I had to deal with that was, again, manually with Emacs. That is, I had to put the images in the right place via `cp`, then hop into that `SLIME` REPL I mentioned, and evaluate a new `define-file-handler` form. Ideally, all of this process should be automatic.
-   A proper **deployment system**. As in, a one-button thing that takes a notebook to an [ASDF](http://common-lisp.net/project/asdf/) project that you can link to from [cliki](http://www.cliki.net/), or pop into your `quicklisp/local-projects` folder. This includes proper handling of any of those static assets I mentioned in the previous point. Generating binaries would also be a nice touch.
-   **`house` sometimes explodes**. Which is bad. As in, when an improperly defined handler does something wrong, I've sometimes observed the condition handling prompt come up. Not sure how much I can do about this one, unfortunately, because I *have* to let through conditions of type `simple-error` in SBCL. That's the particular error type raised by a keyboard-interrupt in Emacs<a name="note-Thu-Jan-22-112909EST-2015"></a>[|1|](#foot-Thu-Jan-22-112909EST-2015), so I need to resolve the tension of "report, rather than exploding for run-time errors" with "I want to be able to shut the server down with a keyboard interrupt". We'll see how far I get.


I'll post code eventually, but I want to fix up a few things in `cl-notebook` first.

And since we're on the subject of `cl-notebook`, why is this [still a pre-beta](https://github.com/Inaimathi/cl-notebook#this-is-now-a-pre-beta), even though I've gotten a working macroexpander together? Glad you asked.

One of the other things I've been doing, which will eventually get its own write-up as soon as I get it running well enough to call a decent proof-of-concept, is thinking about [history in general](https://github.com/Inaimathi/cl-history#cl-history) and [*concurrent* history](https://github.com/Inaimathi/cl-distributed/blob/master/cl-distributed.lisp) in particular. One of the things that came about as a result of those experiments is a more thorough development of the idea of [history-aware data stores](https://github.com/Inaimathi/cl-history/blob/master/cl-history.lisp). Or rather, the first part of said development. And along with the fact that our [existing storage approach](https://github.com/Inaimathi/fact-base) for notebooks is causing [some issue](https://github.com/Inaimathi/cl-notebook/pull/1)s, this has me reconsidering the on-disk file format. A related issue is the on-disk size of [chart cells](https://github.com/Inaimathi/cl-notebook#-back-end). Recent [experiments with delta storage](https://github.com/Inaimathi/cl-distributed/blob/master/cl-distributed.lisp) have me thinking of ways to mitigate that.

## <a name="audio-sequencer" href="#audio-sequencer"></a>Audio Sequencer

Back to creativity toys, [this](http://173.255.226.138/sequencer-demo/basic-sequencer.html) is the single most annoying one I'm likely to actively try to develop.

It's a little program that I'm intending for the use case of getting my first son comfortable with tablets, and with the idea of controlling machines around him. That's basically as focused as the use case gets, and this is likely not the last program I'm writing with the same goals. I'm not going to bother going through it because it was written in a fit of complete stupidity, and doesn't at all reflect the final architecture I wanted for the project. The reason I wanted to talk about it is that I *wanted* to get it working through [`haste`](http://haste-lang.org/).

To that end, I got haste checked out and made these edits:

```diff
Changes from refs/remotes/origin/master to working tree
4 files changed, 119 insertions(+)
 haste-compiler.cabal                   |  2 +
 lib/Audio.js                           | 37 ++++++++++++++++
 libraries/haste-lib/haste-lib.cabal    |  1 +
 libraries/haste-lib/src/Haste/Audio.hs | 79 ++++++++++++++++++++++++++++++++++

        Modified   haste-compiler.cabal
diff --git a/haste-compiler.cabal b/haste-compiler.cabal
index a034774..a944cd5 100644
--- a/haste-compiler.cabal
+++ b/haste-compiler.cabal
@@ -35,6 +35,7 @@ Data-Files:
     debug.js
     Canvas.js
     Handle.js
+    Audio.js
 
 Flag portable
     Description:
@@ -258,6 +259,7 @@ Library
         Haste.Prim
         Haste.Concurrent
         Haste.Graphics.Canvas
+        Haste.Audio
         Haste.Foreign
         Haste.Serialize
         Haste.Parsing
        New        lib/Audio.js
diff --git a/lib/Audio.js b/lib/Audio.js
new file mode 100644
index 0000000..0ea1e31
--- /dev/null
+++ b/lib/Audio.js
@@ -0,0 +1,37 @@
+function jsAudioNew (fnames) { 
+    var a = new Audio();
+    var ins = {};
+    fnames.forEach(function (fname) {
+       var low = fname.toLowerCase();
+       if (low.endsWith("ogg")) {
+           ins.ogg = fname;
+       } else if (low.endsWith("wav")) {
+           ins.wav = fname;
+       } else if (low.endsWith("mp3")) {
+           ins.wav = fname;
+       }
+    });
+    if (a.canPlayType('audio/mpeg;') && ins.mp3) {
+       return new Audio(ins.mp3);
+    } else if (a.canPlayType('audio/ogg; codecs="vorbis"') && ins.ogg) {
+       return new Audio(ins.ogg);
+    } else if (a.canPlayType('audio/wav; codecs="1"') && ins.wav) {
+       return new Audio(ins.wav);
+    } else {
+       console.log("No file type supported...");
+    }
+}
+
+function jsAudioPlay (aud) { aud.play() }
+function jsAudioPause (aud) { aud.pause(); return aud.currentTime }
+
+function jsAudioGetDuration (aud) { return aud.duration }
+
+function jsAudioGetCurrentTime (aud) { return aud.currentTime }
+function jsAudioSetCurrentTime (aud, off) { aud.currentTime = off }
+
+function jsAudioGetVolume (aud) { return aud.volume }
+function jsAudioSetVolume (aud, vol) { aud.volume = vol }
+
+function jsAudioGetLoop (aud) { return aud.loop }
+function jsAudioSetLoop (aud, bool) { aud.loop = bool }
        Modified   libraries/haste-lib/haste-lib.cabal
diff --git a/libraries/haste-lib/haste-lib.cabal b/libraries/haste-lib/haste-lib.cabal
index 76d7a58..5b76f80 100644
--- a/libraries/haste-lib/haste-lib.cabal
+++ b/libraries/haste-lib/haste-lib.cabal
@@ -33,6 +33,7 @@ Library
         Haste.Prim
         Haste.Concurrent
         Haste.Graphics.Canvas
+        Haste.Audio
         Haste.Foreign
         Haste.Serialize
         Haste.Parsing
        New        libraries/haste-lib/src/Haste/Audio.hs
diff --git a/libraries/haste-lib/src/Haste/Audio.hs b/libraries/haste-lib/src/Haste/Audio.hs
new file mode 100644
index 0000000..2d10064
--- /dev/null
+++ b/libraries/haste-lib/src/Haste/Audio.hs
@@ -0,0 +1,79 @@
+{-# LANGUAGE CPP                      #-}
+{-# LANGUAGE ForeignFunctionInterface #-}
+module Haste.Audio ( Audio(..), Status, audio
+                   , play, pause, stop, jump
+                   , mute, unmute, setVolume ) where
+
+import Haste.Prim
+
+#ifdef __HASTE__
+foreign import ccall jsAudioNew :: [JSString] -> IO JSAny
+foreign import ccall jsAudioPlay :: JSAny -> IO ()
+foreign import ccall jsAudioPause :: JSAny -> IO Float
+foreign import ccall jsAudioGetDuration :: JSAny -> IO Float
+foreign import ccall jsAudioGetCurrentTime :: JSAny -> IO Float
+foreign import ccall jsAudioSetCurrentTime :: JSAny -> Float -> IO ()
+foreign import ccall jsAudioGetVolume :: JSAny -> IO Float
+foreign import ccall jsAudioSetVolume :: JSAny -> Float -> IO ()
+foreign import ccall jsAudioGetLoop :: JSAny -> IO Bool
+foreign import ccall jsAudioSetLoop :: JSAny -> Bool -> IO ()
+#else
+jsAudioNew = error "Tried to use jsAudioNew on server side!"
+jsAudioPlay = error "Tried to use jsAudioPlay on server side!"
+jsAudioPause = error "Tried to use jsAudioPause on server side!"
+jsAudioGetDuration = error "Tried to use jsAudioGetDuration on server side!"
+jsAudioGetCurrentTime = error "Tried to use jsAudioGetCurrentTime on server side!"
+jsAudioSetCurrentTime = error "Tried to use jsAudioSetCurrentTime on server side!"
+jsAudioGetVolume = error "Tried to use jsAudioGetVolume on server side!"
+jsAudioSetVolume = error "Tried to use jsAudioSetVolume on server side!"
+jsAudioGetLoop = error "Tried to use jsAudioGetLoop on server side!"
+jsAudioSetLoop = error "Tried to use jsAudioSetLoop on server side!"
+#endif
+
+data Audio = Audio { src :: [JSString], file :: JSAny
+                   , loop :: Bool, volume :: Float, duration :: Float
+                   , status :: Status, muted :: Bool } deriving (Eq, Ord, Show)
+
+data Status = Stopped 
+            | Paused Float
+            | Playing deriving (Eq, Ord, Show)
+
+audio :: [JSString] -> IO Audio
+audio fnames = do f &lt;- jsAudioNew fnames
+                  dur &lt;- jsAudioGetDuration f
+                  return $ Audio { src=fnames, file=f, duration=dur
+                                 , loop=False, muted=False, status=Stopped
+                                 , volume=1.0 }
+
+play :: Audio -> IO Audio
+play a = do _ &lt;- jsAudioPlay $ file a
+            return $ a { status=Playing }
+
+pause :: Audio -> IO Audio
+pause a = do off &lt;- jsAudioPause $ file a
+             return $ a { status = Paused off }
+
+stop :: Audio -> IO Audio
+stop a = do _ &lt;- jsAudioPause $ file a
+            _ &lt;- jsAudioSetCurrentTime (file a) 0.0
+            return $ a { status = Stopped }
+
+jump :: Audio -> Float -> IO Audio
+jump a off = do _ &lt;- jsAudioSetCurrentTime (file a) off
+                return $ a
+
+mute :: Audio -> IO Audio
+mute a = if muted a
+         then return a
+         else do _ &lt;- jsAudioSetVolume (file a) 0.0
+                 return $ a { muted=True }
+
+unmute :: Audio -> IO Audio
+unmute a = if muted a
+           then do _ &lt;- jsAudioSetVolume (file a) $ volume a
+                   return $ a { muted=False }
+           else return a
+
+setVolume :: Audio -> Float -> IO Audio
+setVolume a vol = do _ &lt;- jsAudioSetVolume (file a) $ vol
+                     return $ a { muted=False, volume=vol }
```

Which seemed to work, as far as GHC is concerned. No compiler-time errors, and it looks like the JS side of things is doing exactly what I wanted it to, but I'm not submitting a pull request until I can actually compile the thing with `hastec` and get it working in an actual browser. Which I can't do, because the local version of `haste` I checked out refuses to `haste-boot`. And *that* wouldn't be too big a deal, except trying to [revert to the latest `haste` from `cabal`](http://stackoverflow.com/questions/27596988/trouble-booting-haste) doesn't seem to work either, giving me various `cabal` package dire warnings and errors.

It seems that [the last time I got `haste` running](http://stackoverflow.com/questions/27392814/too-much-recursion-error-with-basic-haste-use/27407077#27407077), I did so in a brief shining moment between major library changes while that was *actually* possible rather than *theoretically* possible.

## <a name="installation-frustration" href="#installation-frustration"></a>Installation Frustration

So `haste` won't compile, either from source or from `cabal`. According to the comments I've gotten on [this question](http://stackoverflow.com/questions/27596988/trouble-booting-haste), I should use [`cabal` sandboxes](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html). Sounds fine in theory. In practice, that page I just linked you to starts with the section title "Building Cabal from git". Fine I guess. That version of `cabal`<a name="note-Thu-Jan-22-113008EST-2015"></a>[|2|](#foot-Thu-Jan-22-113008EST-2015) installs libraries that demand a newer version of `network`. Including some dependencies of `haste`. That newer version of `network` has split apart `network-uri`, which apparently means that [libraries expecting the old interface are SOL for a while](https://github.com/jgm/gitit/issues/447)<a name="note-Thu-Jan-22-113011EST-2015"></a>[|3|](#foot-Thu-Jan-22-113011EST-2015).

Of course, I'm giving you the after-the-fact summary of this. In reality, I haven't been this frustrated with a programming language since I had to install Hunchentoot by hand. Or since the [last time I kicked at `rebar`](http://blog.inaimathi.ca/article?name=not-building-erlang.html). Frankly, this episode has me backing away from recommending Haskell use anywhere I'd need to maintain. And seriously considering a separate virtual machine for Haskell development<a name="note-Thu-Jan-22-113014EST-2015"></a>[|4|](#foot-Thu-Jan-22-113014EST-2015). I'm probably more skittish than most because my [window manager](http://xmonad.org/) also happens to be Haskell-based, which means that borking `cabal` has immediate, annoying consequences for me beyond the inability to compile `.hs` files. It still seems rational to keep my Haskell dev-environment separated by a machine boundary with snapshots if a change in one library can cause this kind of ridiculousness.

After messing around with that for a little while, I did a quick search for SML-based alternatives. Haskell fills three voids in my language line-up. It's at once the non-strict, purely functional and strongly-typed piece of my conceptual arsenal. Granted, SML only hits one third of those, but the other two are approachable even in languages that don't support them directly. Turned out to be a moot point though. The [SMLserver](www.smlserver.org) site was down at the time, which is less than encouraging. Not giving me HTTP errors or anything, just hanging at the `Connecting...` stage. Also, the [SML on Stilts hello-world app](https://github.com/j4cbo/stilts/tree/master/hello) is surprisingly wordy.

So I think that's it for now. I'm going to take a serious break from both languages<a name="note-Thu-Jan-22-113028EST-2015"></a>[|5|](#foot-Thu-Jan-22-113028EST-2015) for the next little while. Hopefully, by the time I get around to doing a fresh install of [Jessie Beta 2](https://www.debian.org/devel/debian-installer/News/2014/20141005), the respective language libraries have their shit relatively sorted and I can get back to just writing code that can actually work anywhere.

## <a name="environments-and-methods" href="#environments-and-methods"></a>Environments and Methods

Last thing for this edition, I swear. Some small pieces of work have been done on [experimentaLISP](https://github.com/Inaimathi/experimentalisp), but I've mostly been thinking about it. Which is mildly disappointing from my perspective, given that this is supposed to be my experimental staging grounds. The particular thing I'm thinking about is methods. Which is actually to say, "how do you make operations generic over several types in something approaching a sane way?" The obvious way of doing it is pretty straight-forward:


-   generic functions are not `Generic Name Env Body`, but rather `Generic Name (Map Type Body)`
-   the generic tables are stored at top-level
-   when you evaluate a `defmethod` form, you're actually inserting an entry into the appropriate top-level table
-   when you evaluate a generic *call*, what actually happens is we go into that top-level table, do a lookup on the number and type of arguments to determine which actual body we're going to evaluate against them


That more or less works<a name="note-Thu-Jan-22-113047EST-2015"></a>[|6|](#foot-Thu-Jan-22-113047EST-2015), but comes with the price that our definition operation really has to be `defgeneric!`. It mutates some piece of global state to achieve its goals, and it takes away the ability to locally override particular methods.

A more elaborate approach would be making generics non-top-level lookup tables, which would allow us to override individual method definitions on an environment-frame level, but it suddenly causes method `lookup` time to go from `log n` to `m log n`, where `n` is the number of methods in this particular generic function, and `m` is how many override tables exist between the caller and the top level. This is because instead of simply looking up the given name in our environment, then looking up the appropriate method in the result and erroring if we fail to find one, we need a more elaborate handling of that last case. If we don't find the method we need in a particular table, it might be in the next one up, so we need to repeat lookup with this generics' parent environment right up to the global environment. I could pull my usual and trade space for time by copying out the previous level of generic table before inserting overriding methods, which might just be a satisfying enough conclusion. I get the feeling that we'd be able to thin them all down substantially during any hypothetical compilation phase.

A third approach, which removes method tables entirely, is to just store the acceptable type of a given function along with its definition, and allow multiple functions with the same name, but different type signatures. Looking things up again becomes expensive, since it's no longer enough to find a matching name; we instead need to find a matching name attached to a signature that unifies with the current arguments. This also implies evaluating arguments before procedures during application<a name="note-Thu-Jan-22-113050EST-2015"></a>[|7|](#foot-Thu-Jan-22-113050EST-2015).

No idea which of these approaches, if any, I'll actually end up taking. I just wanted to get them far enough outside of my own head to see them properly.

* * *
##### Footnotes

1 - <a name="foot-Thu-Jan-22-112909EST-2015"></a>[|back|](#note-Thu-Jan-22-112909EST-2015) - Though, oddly, not in a raw SBCL prompt, where an interrupt is signalled as an `sb-sys:interactive-interrupt`.

2 - <a name="foot-Thu-Jan-22-113008EST-2015"></a>[|back|](#note-Thu-Jan-22-113008EST-2015) - Or maybe it's not `cabal`, but some completely irrelevant hackage change.

3 - <a name="foot-Thu-Jan-22-113011EST-2015"></a>[|back|](#note-Thu-Jan-22-113011EST-2015) - And the proffered workaround may or may not work, but it gives me enough "you need to re-install library-x which may break library-y" warnings that I don't really want to chance it.

4 - <a name="foot-Thu-Jan-22-113014EST-2015"></a>[|back|](#note-Thu-Jan-22-113014EST-2015) - Incidentally, it's at more or less this point that I found out VirtualBox doesn't really work on [Jessie](https://www.debian.org/releases/jessie/). Something to do with the kernel version, and some set of config calls that don't happen properly anymore, resulting in an improper `init.d`. Digging didn't really do anything; other people have had this problem, but they solved it by doing things I've already tried to no effect. After thinking about it for a few minutes, I decided that I probably didn't want VirtualBox for this use-case in the first place. What I'd ultimately want is a Debian or similar guest running on top of a Debian host. This is something [that](https://wiki.debian.org/Xen) [Xen](http://wiki.xenproject.org/wiki/Xen_Project_Beginners_Guide) or [Qemu](https://wiki.debian.org/QEMU) could do fine. In the end, the presence of [pre-built Wheezy images](https://people.debian.org/~aurel32/qemu/) for Qemu made me take that route. Still, I reserve the right to change my mind in the future. Xen is another one of those technologies I've been meaning to get my head around.

5 - <a name="foot-Thu-Jan-22-113028EST-2015"></a>[|back|](#note-Thu-Jan-22-113028EST-2015) -Which is to say, I won't be doing serious work in either language. They're still useful cognitive tools, but the last week or so has me questioning whether they're particularly useful production tools.

6 - <a name="foot-Thu-Jan-22-113047EST-2015"></a>[|back|](#note-Thu-Jan-22-113047EST-2015) -Except that the type signature I gave you is kinda bogus. `Generic Name (Map Type Body)` wouldn't handle things like partially overlapping types and type variables, so we're really looking at something more like `Generic Name (Trie Arg Body)`.

7 - <a name="foot-Thu-Jan-22-113050EST-2015"></a>[|back|](#note-Thu-Jan-22-113050EST-2015) - Which means that we certainly can't have polymorphic `fexpr`s with this approach. Not that it's a huge loss, mind you, I doubt that's a particularly useful thing given that `fexpr`s essentially always act on things of type s-expressions.
