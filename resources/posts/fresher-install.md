Just a heads up, this is probably going to be a fairly lame stream-of-consciousness piece. It's been a busy week or two, and I have to get some things out of my head *now*, or I may just explode.

You Have Been Warned©™

### Firstly

I swore not to make a multiprocessing joke about this, so I won't.

As of about a week ago, I am one of two custodians to a freshly-hatched human being. He's apparently a bit big, which will come as absolutely no surprise to anyone that's met me in meatspace. It's pretty taxing in the sleep department, but I'm told that's temporary, and that the experience is worth it in the long term. Granted, I am told this by people who have gone through the process, so it may just be them rationalizing a fundamentally damaging experience, but lets give them the benefit of the doubt for now. I'll keep you posted, I guess. My wife is still in the recovery stages and has become, shall we say, slightly less certain that she wants to repeat the process. Otherwise, it's going ok. We've got the pretty good Ontario Health system at our backs, and various online/literary resources all of which has helped prepare us. There's also the entirely unexpected benefit of having a sufficiently well-behaved infant, to the point that we've managed to sleep semi-properly and actually *go out* in the week since.

Raising him is going to be another can of worms altogether, especially if [Stross is anywhere near the mark](http://www.antipope.org/charlie/blog-static/2007/05/shaping_the_future.html). I'm about to raise a child to whom I will have to explain that we used to have this thing called "being lost", and that it used to be impossible to keep in touch with your friends every hour of every day, even if you wanted to. I don't even want to begin thinking about that right now though, it'll just get me riled up.

I've got a son, my wife lived, and they both seem happy and healthy. So... that went well.

### Secondly

Earlier this week, I bit the bullet and upgraded my laptop to the latest wheezy release. That's actually something I've been meaning to do for a while for various reasons, and my pack-rat data storage habits finally got me to the point where most of my admittedly meager 64GB hard drive was full. The installation routine is down pat by this point

```
apt-get install screen make emacs git-core gitk wicd-curses pmount htop gnupg unetbootin
apt-get install mplayer alsa feh pacpl canto imagemagick
apt-get install x-window-system dmenu xmonad gimp inkscape conkeror chromium-browser tor rdesktop virtualbox-ose
apt-get install mongodb python-setuptools sbcl haskell-platform haskell-mode hlint leiningen

wget http://beta.quicklisp.org/quicklisp.lisp
sbcl --eval '(load "quicklisp.lisp")'
cabal install hoogle hasktags
~/.cabal/bin/hoogle data
```

I'm not *quite* finished yet. a couple other small items still need to be put together<a name="note-Sun-Nov-11-002932EST-2012"></a>[|1|](#foot-Sun-Nov-11-002932EST-2012), but that's a comfortably functioning if minimal development machine. There are a few changes from last time, but before we get to those,

## I need To Brag for a Moment

[vrms screenshot]

I *thought* that would end up being true for the very short term, seeing as the wireless drivers this machine uses are all blobs. Turns out that *not* installing those has done nothing but kept me from trawling Reddit for porn. My 3G kindle performs admirably when I need to take a look at some new piece of documentation, or just pull up a previously downloaded reference manual, my desk at work has three CAT5 jacks so I'm always on the wired network anyway, and Toronto libraries have wifi hot-spots *consistently* shitty enough that I've yet to `ping www.google.ca` successfully through one<a name="note-Sun-Nov-11-002942EST-2012"></a>[|2|](#foot-Sun-Nov-11-002942EST-2012). I'll stay disconnected for the short term, though I have no idea how long that'll remain the case. In the meanwhile, `rms would be proud`.

## Different Languages

**Ruby** and **Smalltalk** got left out again. I'm not particularly happy about either of those. I definitely wish that [Matz](http://en.wikipedia.org/wiki/Yukihiro_Matsumoto) had become more popular than [van Rossum](http://en.wikipedia.org/wiki/Guido_van_Rossum), but it seems that he hasn't. A little while ago I realized that I was reaching for Ruby and Python in roughly the same situations and, despite the fact that I like Ruby better, Python was coming out more often. That's because everyone in IT at the office has at least a cursory knowledge of Python<a name="note-Sun-Nov-11-002953EST-2012"></a>[|3|](#foot-Sun-Nov-11-002953EST-2012) and because Python comes with Debian. It would be nice if `python-setuptools` was also included by default, and if the language/community didn't have this anti-functional-programming stick up its' ass, but whatever I guess. I've got nothing against Smalltalk either, but of the languages I don't currently have installed, both `forth` and `prolog` are ahead on the "things to learn" list. On the other hand, I have been fooling around with RasPi recently, and that comes with a Squeak image, so I dunno. Maybe it jumps the queue at some point. It just probably won't be on my main machine.

**Erlang** and **Node.js** are both absent, but I'm not complaining. I haven't gone back to kick at Erlang since my last `rebar` failure. Honestly, I haven't been missing it. It has some excellent ideas, but the language itself is fairly ugly, and a few strokes of bad luck on deployment have soured me on it. Maybe that'll change in the future, but it's out for the moment. Node, is a bit odd. On the one hand, there's nothing overtly offensive about JavaScript from my perspective<a name="note-Sun-Nov-11-003002EST-2012"></a>[|4|](#foot-Sun-Nov-11-003002EST-2012). On the other, it just isn't interesting enough that I want to use it anywhere I don't have to. That wouldn't usually prevent me from at least installing the runtime, but


1.   It still isn't in the `testing` repos, and I'll be damned if I start [`apt-pin`ning](http://jaqque.sbih.org/kplug/apt-pinning.html) from [`sid`](http://packages.debian.org/sid/nodejs) for a language that I have an at best passing interest in
1.   `[npm](https://npmjs.org/)`, the Node.js library package manager, is a separate installation. And the serious recommendation seems to be ["Please `curl` this url, and run the result as `root`. Yes, seriously. Stop laughing."](http://handlebarsjs.com/precompilation.html)
1.   Once you've taken the leap of faith and installed `npm` itself that way, you apparently need to run `npm install` *as `root` too*. Which sounds like the sort of bullshit that made windows the marvelously insecure block of Swiss cheese it's been for the last couple decades. It's [not](http://packages.python.org/distribute/easy_install.html) the [only](http://www.cpan.org/) one, so I guess I can't kick its ass too hard over this, but I long for the day when the wisdom of [quicklisp](http://www.quicklisp.org/beta/)/[lein install](https://github.com/technomancy/leiningen)/[cabal](http://www.haskell.org/cabal/) is picked up by all language designers.


I can see myself installing it somewhere other than my main machine, just to give me [handlebars.js pre-compilation](http://handlebarsjs.com/precompilation.html) as a service, if no one's done that yet, but that's about it. In fact, [here](http://barbershop.inaimathi.ca/). Now you don't need to install it either.

**Clojure** and **Haskell** are now part of the standard lineup, neither of which should surprise you if you've been following the blog at all. Both place emphasis on functional programming, laziness and composeability, but that's about where the similarities end. Clojure is one of Lisp's bastards; a dynamic, fully parenthesized, prefix-notated language running on a virtual machine with a heavy focus on Java interoperability. Haskell is a member of the ML family, which means a fanatic devotion to strong, static typing, a heavy emphasis on compile-time rather than run-time optimization, a complete lack of VM, plus a strong aversion to parenthesizing anything and the ability to vary fixedness based on context. I'm making an attempt to learn both over the next few months, and that will hopefully convince you that I take cognitive diversity seriously.

## Switching WMs. Again.

[Last time](http://langnostic.blogspot.ca/2012/06/fresh-install.html) I hopped back into StumpWM from XMonad. This time, I'm hopping back. It turns out that, just like there are a couple of small annoyances in XMonad that make Stump preferable, there are a couple of small annoyances in StumpWM that do the same for XMonad.


- StumpWM really *really* doesn't like floating windows. Far as I know, there isn't a way to detach one randomly, or do anything with one once its detached. The WM also occasionally throws errors when a program tries to put up an alert, like a file-save notification or print dialog. XMonad has yet to yell at me about that, and it elegantly deals with floating windows using the mouse<a name="note-Sun-Nov-11-003224EST-2012"></a>[|5|](#foot-Sun-Nov-11-003224EST-2012).
- Stump *still* crashes with GIMP. I vaguely hoped that the single-window mode would outright resolve that issue, but it hasn't. Sure you can now *run* the program, but attempting to open a file with it results in the WM becoming unresponsive to keyboard input<a name="note-Sun-Nov-11-003257EST-2012"></a>[|6|](#foot-Sun-Nov-11-003257EST-2012). XMonad has no such problems, and being that I occasionally like to draw things, I'd prefer my window manager to not explode while loading drawing tools. Even apart from the specific GIMP problem, I've found StumpWM to crash more in general than XMonad does<a name="note-Sun-Nov-11-003302EST-2012"></a>[|7|](#foot-Sun-Nov-11-003302EST-2012).
- Taking screenshots using `import` caused some odd errors. It would very occasionally crash the WM, and very frequently create a black rectangle rather than a screenshot of the appropriate screen area. I normally *wouldn't* put this down to the window manager, except that I haven't observed the effect in XMonad, XFCE or Gnome.


I'm prepared to make peace with the fact that `C-t` has to be an exclusively window-manager keystroke, and I've changed my keymap a bit to mitigate the second-class status of chorded mod keys. Specifically, I've bound any repetitive keystrokes to `C-t C-[key]` rather than `C-t [key]`. It doesn't *entirely* solve the problem, but using `hold C + t h t h t h t h t h` to resize windows is still preferable to `C+t h C+t h C+t h C+t h C+t h`. Speaking of configs

```haskell
import System.Directory
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.EZConfig
import XMonad.Util.CustomKeys

import qualified XMonad.StackSet as S

main = xmonad $ conf { modMask = mod4Mask }
       `additionalKeysP`
       [ ("C-t C-d C-b", withFilePrompt "Pic: " bgFolder setDesktopBackground)
       , ("<Print>", withFilePrompt "Name: " screenshotFolder capToFile)

       , ("C-t p", spawn "dmenu_run")
       , ("C-t C-p", spawn "dmenu_run")
       , ("C-t <Return>", spawn "xterm")
       , ("C-t e", runOrRaise "emacs" (className =? "Emacs"))
       , ("C-t b", spawn "chromium --proxy-server=\"socks://localhost:9050\"")

       , ("C-t C-s", nextScreen)
       , ("C-t C-t", windowSwap)
       , ("C-t t", windows S.swapDown)
       , ("C-t C-j", windows S.swapDown)
       , ("C-t j", windows S.focusDown)
       , ("C-t k", windows S.focusUp)
       , ("C-t C-k", windows S.swapUp)
       , ("C-t g", goToSelected defaultGSConfig)

       , ("C-t C-<Space>", sendMessage NextLayout)
       , ("C-t C-h", sendMessage Shrink)
       , ("C-t C-l", sendMessage Expand)

       , ("C-t s", withFocused $ windows . S.sink)
       ]
  where conf = defaultConfig { XMonad.startupHook = onStartup }

---------- Config Options
bgFolder = "/home/inaimathi/pictures/backgrounds/"
screenshotFolder = "/home/inaimathi/pictures/screenshots/"

onStartup :: X ()
onStartup = do
  setDesktopBackground "dash.png"

---------- Helper Functions
setDesktopBackground :: MonadIO m => String -> m ()
setDesktopBackground pic = spawn $ concat ["feh --bg-scale ", bgFolder, pic]

capToFile :: MonadIO m => String -> m ()
capToFile picName = spawn $ concat ["import ", screenshotFolder, picName]

rdesktop domain username password server = spawn $ concat ["rdesktop -g 1280x1024 -d ", domain, " -u ", username, " -p ", password, " ", server]
myRdesktop server = rdesktop "MEDIREXSYS" "leoz" "my-password-goes-here-and-you-aint-gettin-it" server

---------- Utility
windowSwap = do
  windows S.focusDown
  windows S.swapUp

xpConf = defaultXPConfig { position = Top }

withPrompt :: String -> (String -> X ()) -> X ()
withPrompt prompt fn = inputPrompt xpConf prompt ?+ fn
withCompletingPrompt :: String -> [String] -> (String -> X ()) -> X ()
withCompletingPrompt prompt completions fn =
  inputPromptWithCompl xpConf prompt comp ?+ fn
  where comp = mkComplFunFromList completions

withFilePrompt :: String -> String -> (String -> X ()) -> X ()
withFilePrompt prompt directory fn = do
  files <- liftIO $ getDirectoryContents directory
  let fs = filter relevant files
      relevant f = '.' /= head f
  withCompletingPrompt prompt fs fn
```

### Finally

The main thing I've been kicking around is actually Haskell. I finally buckled down and went through most of the [Happstack Crash Course](http://www.happstack.com/docs/crashcourse/index.html)<a name="note-Sun-Nov-11-003620EST-2012"></a>[|8|](#foot-Sun-Nov-11-003620EST-2012), and it just about feels like I have a less tenuous grip on the language than I used to. After reading through the references available, hitting my head rather hard against the concept of monads, going through several tutorials, and attempting a few small programs of my own, it is possible for me to write a medium sized program in Haskell without pulling a reference text out every two minutes. That only took about three years. I'm not entirely sure whether the effort has been worth it in the direct sense, but I still stand by my prior assessment of the situation. Understanding a new mode of thinking about a problem *can not* be a waste of time. Even if it turns out to be less effective than another mode, or even outright incorrect, understanding the process will give you some insight. Either about the problem or about the current practitioners of its solutions or about your own cognitive assumptions.

All of those are powerful things, and you get very surprisingly few of them if you only know one language.

<!--  LocalWords:  StumpWM XMonad Happstack
 -->


* * *
##### Footnotes

1 - <a name="foot-Sun-Nov-11-002932EST-2012"></a>[|back|](#note-Sun-Nov-11-002932EST-2012) - Mainly various Emacs language modes and networking tools.

2 - <a name="foot-Sun-Nov-11-002942EST-2012"></a>[|back|](#note-Sun-Nov-11-002942EST-2012) - And, as mentioned, my home activities tend to the unproductive, so ... yeah.

3 - <a name="foot-Sun-Nov-11-002953EST-2012"></a>[|back|](#note-Sun-Nov-11-002953EST-2012) - While I'm the only one who pokes at Ruby.

4 - <a name="foot-Sun-Nov-11-003002EST-2012"></a>[|back|](#note-Sun-Nov-11-003002EST-2012) - No more than is offensive about Python or Elisp, leastwise.

5 - <a name="foot-Sun-Nov-11-003224EST-2012"></a>[|back|](#note-Sun-Nov-11-003224EST-2012) - `Mod+Click+Drag` moves a floating window, detaching it if it isn't already, while `Mod+Right-click+Drag` resizes a floating window.

6 - <a name="foot-Sun-Nov-11-003257EST-2012"></a>[|back|](#note-Sun-Nov-11-003257EST-2012) - Which is catastrophic for a tiling WM.

7 - <a name="foot-Sun-Nov-11-003302EST-2012"></a>[|back|](#note-Sun-Nov-11-003302EST-2012) - Mostly when starting up `mplayer`, `feh` or `rdesktop` with some of the more arcane options, though I've had a couple of completely unexplained crashes too.

8 - <a name="foot-Sun-Nov-11-003620EST-2012"></a>[|back|](#note-Sun-Nov-11-003620EST-2012) - Still need to run entirely through the `acid-state` and `IxSet` sections.
