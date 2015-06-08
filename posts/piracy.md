So I've gotten my x220 all set up the way I like. I ended up using XFCE as my desktop environment again, because if nothing else, I sometimes like an external monitor. Really, I expect to spend most of my time in [Screen](http://langnostic.blogspot.com/2011/10/screenwm-follow-up.html) now that I've set up [wicd](http://en.wikipedia.org/wiki/Wicd). By the way, in case you're trying to get it to work and failing, you actually need to specify what wireless device it should use to connect to networks. This device is almost always `wlan0`, but a default isn't set so you need to do `Shift+p` and type it in manually in the preferences screen.

I'm still debating window managers, and currently have both [StumpWM](http://stumpwm.org/) and [XMonad](http://xmonad.org/) installed. I've been using Stump pretty consistently lately, but I have noticed some inconveniences with it related to how window management works. The main things keeping me on it are the home-row mod "key" and the ability to override that "key" in case the program I'm using needs the keystroke. Taking a look at what XMonad config can do these days, here's how I would do the same

```haskell
import XMonad
import XMonad.Config.Xfce

import qualified XMonad.StackSet as S
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect

import XMonad.Util.EZConfig
import XMonad.Util.CustomKeys

import qualified Data.Map as M

main = xmonad $ xfceConfig { modMask = mod4Mask }
       `additionalKeysP`
       [ ("M-p", dmenu_launch)
       , ("M-<Return>", spawn_term)
       , ("<Print>", spawn "xfce4-screenshooter")
       , ("C-t p", dmenu_launch)
       , ("C-t C-p", dmenu_launch)
       , ("C-t <Return>", spawn_term)
       , ("C-t w", nextScreen)
       , ("C-t e", prevScreen)
         
       , ("C-t j", windows S.focusDown)
       , ("C-t S-j", windows S.swapDown)
       , ("C-t k", windows S.focusUp)
       , ("C-t S-k", windows S.swapUp)
       , ("C-t g", goToSelected defaultGSConfig)
         
       , ("C-t <Space>", sendMessage NextLayout)
       , ("C-t h", sendMessage Shrink)
       , ("C-t l", sendMessage Expand)
       , ("C-t t", withFocused $ windows . S.sink)
         
       , ("C-t C-t", spawn "xdotool key ctrl+t") -- this is a lie
       ]
       
dmenu_launch = spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\""
spawn_term = spawn "xterm"
```

A couple of those are blatantly ridiculous (specifically the resizing options are less than helpful if you have to do `C-t h C-t h C-t h` rather than `S h h h`), and the `xdotool` command is entirely fictitious, as the comment claims. I'm actually not even sure why that is; the command works fine when I run it from terminal directly, and it also works fine on *any keystroke other than `C-t`*, in both Stump and XMonad. I'm beginning to think it may be a bug in `xdotool`. Incidentally, I realize that in this one specific situation, it may have been simpler to specify my own submap, but I like the emacs-style keys enough to keep them despite a bit of repetition.

Ok, massive gear shift incoming. Don't say I didn't warn you.

There's been a lot of discussion about piracy across the [various](http://www.reddit.com/r/technology) [reddits](http://www.reddit.com/r/AskReddit/) I [frequent](http://www.reddit.com/r/darknetplan/) [lately](http://www.reddit.com/r/canada/comments/ne9f6/how_will_the_sopa_bill_passing_affect_our/). Mostly related to the imminent [SOPA](http://en.wikipedia.org/wiki/Stop_Online_Piracy_Act) and [PROTECT IP](http://en.wikipedia.org/wiki/PROTECT_IP_Act) bills set to pass in the US. The thing that's bothered me the entire way through most of the discussions is that there isn't a standard definition of "piracy". Personally, I'd prefer to take [the Stallman approach](http://www.gnu.org/philosophy/words-to-avoid.html#Piracy), just outright rejecting the legitimacy of the term, but that ship has sailed<a name="note-Tue-Dec-27-150940EST-2011"></a>[|1|](#foot-Tue-Dec-27-150940EST-2011). There are [political parties](http://en.wikipedia.org/wiki/Pirate_Parties_International)<a name="note-Tue-Dec-27-151156EST-2011"></a>[|2|](#foot-Tue-Dec-27-151156EST-2011) using the term as a rallying cry for people in favor of digital freedom and privacy, and most people joining the conversation haven't been exposed to other terms.

You can't help but be biased if that's all the exposure you've had, [recent hollywood romanticization of pirates](http://en.wikipedia.org/wiki/Jack_Sparrow) notwithstanding. So, here are some things to keep in mind. I'm not claiming that this is the complete conversation, but this is the 101-level thought that needs to go into a framework for deciding what your opinion of the situation is.

First off, "piracy" in the software sense translates to "unauthorized copying" if you want to be a bit more pedantic about it. If we look at what DMCA, SOPA et al attempt to regulate, piracy includes, but isn't limited to distributing copyrighted works for profit. To start with, here are some examples of piracy in no particular order:


- posting [fan art of characters you don't own](http://quintela.deviantart.com/art/Zelda-fan-art-108197853) on a third party service
- selling [homemade copies of commercial movies](http://www.thestar.com/article/561408)
- non-commercially [creating and posting original content that incidentally features copyrighted material](http://www.youtube.com/watch?v=N1KfJHFWlhQ)
- non-commercially creating and posting [remixed content that centrally features copyrighted material](http://www.youtube.com/watch?v=Pui0UzAFCJg)
- hosting a for-profit site whose primary use is hosting copyrighted content, or linking to torrents of copyrighted content
- selling [original work featuring characters and settings you don't own](http://en.wikipedia.org/wiki/Another_Hope) without the original authors' permission
- <a href="http://thepiratebay.org/">providing links or references to copyrighted material</a >or to sites which enable copyright infringement (which means that this particular page is actually engaging in piracy, we'll get back to that later)
- using [copyrighted material for the purposes of criticism](http://boingboing.net/2009/10/06/the-criticism-that-r.html)
- [copying a music CD you own onto a private computer network](http://www.mtv.com/news/articles/1471036/riaa-sues-four-college-students.jhtml)
- hosting [user-submitted content and/or links](http://www.reddit.com/) which may include unauthorized copies of commercial content



I'm not making a value judgment on which of the above is ok and which isn't, by the way, and you shouldn't either<a name="note-Tue-Dec-27-174847EST-2011"></a>[|3|](#foot-Tue-Dec-27-174847EST-2011) if you hope to build a working understanding of piracy as it applies to copyrighted works. First off, with so many examples, lets try to extract some principles so that we can figure out how the concept is structured. Based on the above, the different examples, all of which are technically piracy, vary along at least 5 axes: commercial/non-commercial, public/private, incidental/central, singular/numerous, direct/transformitive. Note that none of these are binary either/or situations; they're all on a continuum.

**Commercial vs. Non-commercial** seems fairly straightforward; is the intent behind the copy to make money or not? It's not that cut and dried though. Copying something onto physical media takes money, and so does hosting a web server. So does recouping that cost count as a commercial venture? This is actually a stumbling block that [Linux development hit early on](http://www.youtube.com/watch?v=__fALdvvcM0#t=44m24s); some people wanted to distribute the operating system, but the initial license forbade accepting money in return. Those would-be distributors contacted Linus about this and pointed out that even though they don't want to charge for the OS itself, the CDs they needed to use cost money, and it took time to produce them. Linus switched to the GPL2 as a result, and this sort of semi-commercial sharing (along with fully [commercial distribution](http://www.redhat.com/)) is now perfectly legal.

On one end is "no money or goods change hands in compensation for the shared copy" (think exchanging mix-tapes or posting content on Reddit without actually selling anything), on the other is "I'm making more money the more copyrights are infringed" (think the DVD copying organization, or an [advertising-supported model of file-sharing](http://www.megaupload.com/))

**Public vs. Private** is more obviously on a continuum based on that example. On one end is something like "copying a CD or DVD you own onto your iPod", on the other is "Put it on the Pirate Bay", and between the two of them are networks of varying privacy ranging from your own family LAN, to a University-only network to a [small-esque online community](http://www.4chan.org/).

**Incidental vs. Central**; is the entire point of the copy the copyrighted material (as in the example of fan art), or is the point something else entirely and the copyrighted material happens to be there (as in the example of [the first steps video](http://www.youtube.com/watch?v=N1KfJHFWlhQ))? A decent test for this is: if you strip out any questionably violating material, would the media still be interesting to its target audience?

**Singular vs. Numerous** is perhaps the most obviously measurable; how many infringements were there? At one end is making one copy of a music CD to put onto your own music device. At the other is making a few hundred thousand copies. Of course, this is made more ambiguous when you consider how digital technology actually works. In the technical sense, a new copy of a YouTube video is produced every time someone views it (even though the original poster only actually produced one copy; the upload from their computer to the YouTube server). Who takes the heat for the additional copies created when the video is viewed? Is a viewer more guilty when they use a service like [keepvid](http://keepvid.com/) to download the video rather than streaming it even though the same number of copies are created either way? The technical aspects of the implementation really begin to matter if the law assumes that all copies are equally infringing.

**Direct vs. Transformitive** is obviously a continuum, but not very easy to measure. If you make a copy of a DVD, that's direct, but how much of a change can actually transform that media into a new work? I'm biased here, having gone through design school and come out the other end, so I'll only point out that [merely obstructing pieces strategically](http://www.youtube.com/watch?v=e0jjXTEqQP4#t=00m25s) changes the audience reaction. By the time you get to fan art/fic, entirely new works merely using existing settings and character names, it seems fair to claim that we should at least reflect on whether [Fair Use](http://en.wikipedia.org/wiki/Fair_use) might apply to the act of piracy in question.

So those are the axes on which piracy is currently defined. I should note that, according to related laws<a name="note-Tue-Dec-27-191213EST-2011"></a>[|4|](#foot-Tue-Dec-27-191213EST-2011), all of these things qualify as piracy (and therefore constitute content that a website might get taken down over), even though only some of them are things which most humans would agree is immoral. There *isn't* an idea of degree or fair use inherent in the process of a DMCA takedown, or the upcoming SOPA takedowns, and in general the onus is on the accused to fight the accusation rather than on the accuser to prove harm. `Is this piracy? [ ] yes [ ] no`, and if `[x] yes`, it's criminal so take it down. Which should at least illustrate why I think it's important for people to understand the terms.

Many of the things we call "piracy" today are things that were considered either free or fair uses as recently as 20 years ago, many didn't exist as recently as 10 years ago, some were illegal then but probably shouldn't be anymore as a result of technological advancements, and finally some were and continue to be both unethical and illegal.

Now I hope you've been paying attention, because here comes the curveball.

### <a name="do-you-support-piracy" href="#do-you-support-piracy"></a>Do you support piracy?

**Richard Stallman:** Attacking ships and killing the entire crew to take the cargo is very bad. Now, if you mean sharing, of course you should share. Sharing is good.
**Sony/RIAA/MPAA/et al:** No, all pirates should be executed (or at least fined their life savings and prevented from going near a computer ever again.)
**You:**[your answer here]

I hope you can see how both of the extreme stances are a bit ridiculous, and that a nuanced view needs to prevail if we're going to continue this decade with any semblance of freedom. And if we refer to the thought framework I proposed earlier, it's fairly obvious *why* each of the extreme views are ridiculous. 

It is not ok to take the output of others' creative work without their permission and distribute it for your own profit without sharing any of the proceeds<a name="note-Tue-Dec-27-221721EST-2011"></a>[|5|](#foot-Tue-Dec-27-221721EST-2011). That sort of activity is not sharing, and probably should be a criminal act. The people who do this are spiritual descendants of the original book pirates; the guys who would set up a press to run off a few thousand Dickens or Twains and sell them to line their own pockets without supporting the original writers. That's one of the few forms of copyright infringements that you can call "theft" without me getting pissed off at you about the implications. There is clear harm being done to the author and legitimate publishers, there is money being made, the work is not transformitive, there are multitudes of copies, those copies are public, *and* the copyrighted work is central to the exercise. Lock those fuckers up.

It *is* ok to take copyrighted works that you've purchased and copy them as many times as you like for personal use. You should be able to copy your CDs onto your DVDs and onto your iPods and onto your computers, and you shouldn't have to fear getting arrested and being forced to prove the legitimacy of your copies over that. You should be able to make a backup of your entire hard drive for later restoration, including all copyrighted works present on that hard drive.

You also should be able to post snippets of copyrighted works for educational and critical purposes, and that cuts to the point I touched on earlier about how this page can itself arguably be considered piracy. In order to semi-intelligently discuss pirate sites like [The Pirate Bay](http://thepiratebay.org/), [YouTube](http://www.youtube.com/) and [DeviantArt](http://www.deviantart.com/), I have to link to them. The discussion wouldn't be nearly as enlightening if you had to imagine how these sites worked based on my vague descriptions, but the act of linking connects you to these tools that you can potentially use to pirate media. The linking question is particularly relevant to sites like The Pirate Bay, which indirectly make money off of copyright infringement, but host none of the files. Conceptually, what's happening is that TPB is keeping a list of files on peoples' private computers, and handing you directions to them when you ask. If you [torrent something](http://thepiratebay.org/torrent/6896084/Debian_Plus_XFCE_i686_(Installable_Livecd_with_multimedia)), they're not involved at all after the introductions; the data is moving directly between you and some individuals at the other end.

It's probably this last kind of piracy that's prompting bills like SOPA; peer to peer sharing via torrents can really only be regulated by monitoring all communications on the internet ever, checking whether they contain copyrighted content and stopping them if they do. That sounds like a bad thing to me, but ultimately it'll fall on the public and public officials to decide whether an end to piracy is worth setting up the perfect world-wide surveillance state, criminalizing cryptography again, and giving up the fair use protections that let them discuss piracy intelligently. Hopefully, that'll be a no, but even if it isn't (or you don't think it should be) you should now be equipped to tell me why without resorting to "why do you support stealing?" like a trained parrot.

* * *
##### Footnotes

1 - <a name="foot-Tue-Dec-27-150940EST-2011"></a>[|back|](#note-Tue-Dec-27-150940EST-2011) - No pun intended there. Seriously.

2 - <a name="foot-Tue-Dec-27-151156EST-2011"></a>[|back|](#note-Tue-Dec-27-151156EST-2011) - [In Canada too](http://www.pirateparty.ca/), by the way.

3 - <a name="foot-Tue-Dec-27-174847EST-2011"></a>[|back|](#note-Tue-Dec-27-174847EST-2011) - Yet

4 - <a name="foot-Tue-Dec-27-191213EST-2011"></a>[|back|](#note-Tue-Dec-27-191213EST-2011) - Both ones that [are in effect](http://en.wikipedia.org/wiki/Digital_Millennium_Copyright_Act), and ones that are still [being discussed](http://en.wikipedia.org/wiki/Stop_Online_Piracy_Act).

5 - <a name="foot-Tue-Dec-27-221721EST-2011"></a>[|back|](#note-Tue-Dec-27-221721EST-2011) - This is a matter completely separate from whether it's wise for a creative worker to grant that permission. I'm not getting into that argument today, but [most of my code](https://github.com/Inaimathi) is available under [MIT](http://en.wikipedia.org/wiki/MIT_License)/[GPL](http://www.gnu.org/licenses/gpl-2.0.html) licenses and [all my illustration](http://inaimathi.deviantart.com/) work is [CC-BY-SA](http://creativecommons.org/licenses/by-sa/2.0/), so you can probably infer what side of that argument I'm on.
