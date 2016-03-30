I'm taking another, closer look at XMonad this week. StumpWM is awesome for straight-up coding, and its extensive use of the Emacs model means that there was very little learning curve for me to struggle against. A few things were starting to burn my ass though, and while I tolerate small blemishes there are two other forces at work here. First, I've tried hard to make sure that my tools enable rather than limit me[^specifics]. And second, I've been looking for an excuse to pick up Haskell for more than casual play for a very long time now.

[^specifics]: And one of the things that started to burn my ass was a specific GIMP-related bug that did limit me.

As a public service, here are the few issues that I ran into with StumpWM[^wont-dwell]:

[^wont-dwell]: I don't intend to dwell on this, just be aware of them. I can still recommend it heartily if you're an Emacs user that won't run up against these specific points.

1. It crashes every time I try to open a file with [GIMP](http://www.gimp.org/)
2. It has some odd issues with Mplayer ([naive fix here](http://www.mail-archive.com/stumpwm-devel@nongnu.org/msg01671.html)).
3. It doesn't seem to like nautilus (which is mainly an issue when I'm trying to browse through a folder full of images; this is one of the few places in my day-to-day computing activities where a command line is less efficient than a GUI)

That's it. Now, to be fair, #3 is only relevant if you don't use StumpWM as a window manager on top of a desktop environment[^from-the-future], #2 has [a workaround](http://www.mail-archive.com/stumpwm-devel@nongnu.org/msg01671.html) that I've been using successfully, and #1 only really bites you if you're [multiclassing](http://www.d20srd.org/srd/classes/multiclass.htm) Illustrator/Programmer, which is not unheard of, but keep in mind that YMMV here.

[^from-the-future]: Hello from the year 2016! I've since started using [`feh`](http://feh.finalrewind.org/) as my image browsing program of choice, which means that this third point is effectively nullified for me too.

It's actually sort of amazing that I got by for such a long time without noticing #1. After noticing it, I got into the habit of spending most of my time in StumpWM, and switching into Gnome/Bluetile for GIMP work. And that worked out just fine when most one or the other type of work was a vast majority of my time. Sadly *(fortunately?)* a couple weeks ago, my schedule started rebalancing into about 50/50 graphic design and coding[^concept-work]. It was surprising how horribly annoying the start-up wait time for Gnome/Bluetile can be. I've written about it already, and my conclusion on Bluetile was, basically, that it was overtly complex but a workable beginners' TWM. Certainly not something I'd use as my first choice, in any case. Add to that the fact that I had been spoiled by StumpWMs' nearly instantaneous start-up, and those WM switches were starting to look ugly. It actually changed the way I thought; I'd get all my coding done first, then do my image work all at once, just to minimize the impact of that switch.

[^concept-work]: I'm doing some concept work, which involves a web app, but no code yet so my tablet and degree are finally being put through their paces.

This was clearly not an optimal situation.

By chance, I stumbled onto a [reddit post bemoaning that Gnome lag](http://www.reddit.com/r/linux/comments/f5cdo/). Long story short, the poster used [XFCE4](http://www.xfce.org/), [XDM](http://en.wikipedia.org/wiki/XDM_(display_manager)) and [Ubuntu server edition](http://www.ubuntu.com/server) to put together a minimal, but snappy desktop environment. It looked interesting, and passed the Compaq Test[^compaq-test] so I took the weekend to replace Gnome with XFCE4 on each of my machines[^kept-debian]. There's bound to be more updates about this as I nail down specific stuff, but it's working very well so far. I have tiling where I need it, and thanks to XFCE4, I can use pointing devices when they're appropriate.

[^compaq-test]: "The Compaq test" is something I put any desktop environment or WM through before switching; it involves building it on a $20 Compaq Presario R3000 (with a 1.4ghz processor, a whopping 256 MB of ram and a 5400rpm HDD) and using it for a weekend to see how it works. The reasoning is that if it's tolerable on the Compaq, then it'll fly on my desktop machine. It's something I reserve for background software, not things like Emacs or GIMP themselves. My rule of thumb is that anything I'm thinking of using that has to be on all the time should do better than "awful" on the Compaq test.
[^kept-debian]: I kept them all [Debian Squeeze](http://www.debian.org/devel/debian-installer/), rather than downloading [Ubuntu server 10.10](http://www.ubuntu.com/server/get-ubuntu/download), and I used [xmonad](http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_XFCE#Configuring_XMonad_to_work_with_Xfce) instead of XDM because I primarily wanted tiling rather than mousing.

My `~/.xmonad/xmonad.hs` is pretty minimal at this point:

```haskell
import XMonad
import XMonad.Config.Xfce

import XMonad.Actions.Submap
import XMonad.Util.EZConfig

import qualified Data.Map as M

main = xmonad $ xfceConfig { modMask = mod4Mask }
       `additionalKeys`
       [ ((mod4Mask, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
       , ((mod4Mask, xK_Return), spawn "xterm")
       -- , ((control, xK_space), spawn "xdotool text 'testing testing'")
       -- , ((controlMask, xK_t), submap . M.fromList $
       --                         [ ((0, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
       --                         , ((0, xK_Return), spawn "xterm")
       --                         , ((0, xK_t), spawn "xdotool key ctrl+t")
       --                         ])
       ]
```

The commented stuff has to do with tweaks I'm trying to make. Xmonad+XFCE4 hits all of the pain points I was having with StumpWM, but it introduces a couple of its own (less severe, from my perspective).

First, the mod-keys aren't on the home row. I have to contort my left pinky/ring finger/thumb[^still-havent] in an odd way to hit the `Win` or `Alt` keys in a way that never happened when `mod` was effectively `C-t`. Granted that may have made it very slightly more awkward to add tabs in some browsers, but there are workarounds. Also luckily, `XMonad.Actions.Submap` exists, which means I can write up a key list that's less RSI-inducing[^commented].

[^still-havent]: Still haven't decided which feels least uncomfortable.
[^commented]: The only reason this is commented above is that I can't get `xdotool` working as advertised.

Second, there's that layer of user-facing complexity that comes from distinguishing between screens and workspaces. I've had time to reflect since my Bluetile writeup, and it seems like a lot of time in StumpWM gets spent screen-juggling[^making-sure]. This is because Stump doesn't make that key distinction. When you cycle to the next window, it's the same window you've got open (regardless of which workspace you've currently got it on). That's easier to learn because you have to think about a single list of windows, but trickier to use because it's that much more likely to blow your layout by pulling a window you don't mean to. XMonad goes the other way; there are three explicit keystrokes to switch the "focused" monitor (and as far as I can tell, if you have more than 3, you're screwed), but the upside is that windows will stay where you put them, workspace-wise.

[^making-sure]: Making sure that Emacs and Chrome stayed on my main monitor and mplayer/terminals/secondary apps on the other one.

It also looks like I'll have to do some light Haskell learning to get full benefit, but if you've ever talked to me about computers IRL, you know that I don't consider that downside.
