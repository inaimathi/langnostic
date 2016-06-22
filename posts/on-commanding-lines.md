Still working away on the authentication system. I'm basically at the point where I can use RSA keys to sign in to my demo webapp. On one browser. As long as the keys are in PEM format. And `crypto:verify` is in a good mood.

This isn't about that though.

I've been slowly moving towards more and more command-line oriented interfaces. It's not a recent trend, in fact it started pretty much when I first discovered Emacs. Ever since doing away with my desktop environment a little while ago, it's been more of a necessity than idle speculation. The good news is that there's almost nothing I wanted to do in X windows that I can't do via command line.

### Command Line MVPs

Let me draw your attention to some command line programs that I honestly wouldn't want to go without anymore. Not counting obvious necessities like [`ssh`](http://www.openssh.com/)/[`rsync`](http://ss64.com/bash/rsync.html)/[`find`](http://en.wikipedia.org/wiki/Find)/[`grep`](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_04_02.html)/[`tail`](http://en.wikipedia.org/wiki/Tail_(Unix)).

I've already written a bit about [**wicd-curses**](http://manpages.ubuntu.com/manpages/karmic/man8/wicd-curses.8.html), the very good, simple command line network manager. After you set up a wireless device with `Shift+p`, and set up your connection keys, it'll make sure you're as plugged in as you can possibly be with no need for a network widget. You don't even need to run it unless you're connecting to a new network; the daemon starts up with the rest of your machine.

[**htop**](http://htop.sourceforge.net/) isn't anything new, if you've been paying attention. It's an improvement over the regular [**top**](http://linux.die.net/man/1/top) in that it gives you more information and prettier colors. That's reason enough for me to use it.

[**acpi**](http://wiki.debian.org/ACPI) does quite a few things relating to cooling, power, and battery. Really, I just use it as the replacement for the gnome/xfce battery widget.

[**screen**](http://www.gnu.org/software/screen/) is something I've been using forever. My first time firing it up was to [deploy a Hunchentoot application](http://stackoverflow.com/a/514625/190887). Since then, I've used it as a way of managing multiple terminals, and kicked its tires as a [full-on window manager](http://langnostic.blogspot.ca/2011/10/screen-for-stumpwm-users-gnu-screen-as.html).

[**mplayer**](http://www.mplayerhq.hu/design7/news.html) is another piece that I've been using for a long time. Even while bumping around GNOME, I preferred this to [VLC](http://www.videolan.org/vlc/index.html) (YMMV). It's worth a read through the documentation if you're having a slow day; the program does various crazy things in addition to music/video playback, including bitmap frame outputs, format conversion and some timeline-based edits.

[**pacpl**](http://pacpl.sourceforge.net/) is an audio chopping tool. As of the latest version in the Debian repos, it can directly extract music from videos. As you can see by the website there, it can convert to and from pretty much any audio format you care to name, though I mostly use it to convert things to [ogg](http://en.wikipedia.org/wiki/Ogg)s.

[**imagemagick**](http://www.imagemagick.org/script/index.php) is a command-line image chopping program with so many options that you'd really better just read [the docs](http://www.imagemagick.org/script/command-line-tools.php?ImageMagick=fmrn62da6hq94butv3t53diib0). It's actually composed of a bunch of different utilities, of which I mostly use `convert`, `mogrify` and `identify`.

[**get_flash_videos**](http://code.google.com/p/get-flash-videos/) is about the only way I get to see most videos, given a) how crappy flash support is when you're even half-way dedicated to the idea of [Free software](http://www.gnu.org/philosophy/free-sw.html) and b) how few sites other than YouTube provide an HTML5 based video player.

[**transmission-cli**](https://forum.transmissionbt.com/viewtopic.php?f=2&t=11784) is the command line interface to my favorite torrent client. Granted, I don't torrent much since I got out of the habit of downloading [the massive install CDs](http://www.debian.org/CD/torrent-cd/), but still.

[**gtypist**](http://www.gnu.org/software/gtypist/) is a curses-based typing tutor that has effectively replaced klavaro for me. It's mildly more entertaining to run typing drills on surrealist, minimal poetry than it is to type out old newspaper articles. The only thing about it that rustles my jimmies is that it enforces hitting space twice after a period. Which is a thing I guess? Honestly it sounds like an [anachronistic](http://www.hanselman.com/blog/TheFloppyDiskMeansSaveAnd14OtherOldPeopleIconsThatDontMakeSenseAnymore.aspx) behavior that used to make sense back when actual humans used [actual typewriters](http://crashreboot.blogspot.ca/2009/04/your-word-processor-is-no-typewriter.html). Luckily, the lessons are contained in a set of conf files, so I'll be able to do something about this.

> EDIT:
> Aaaaand [bam](https://github.com/Inaimathi/gtypist-single-space). Enjoy.
>
> Wed, 20 Jun, 2012

[**canto**](http://codezen.org/canto/) is a command-line based RSS feed reader. I complained about [liferea](http://liferea.sourceforge.net/) earlier for its complexity, and having taken a look at a number of feed readers (both GUI and CLI), that doesn't seem to be an uncommon feature. `canto`, by contrast is ridiculously simple; set up your `conf` file, and it'll track those feeds, pulling when you tell it to (every 5 minutes by default). The [example config](http://codezen.org/canto/config/#example-config) up at the project site is pretty extensive, but I've gotten on fine with a much more minimal setup:

```python
from canto.extra import *
import os

link_handler("lynx \"%u\"", text=True)
image_handler("feh \"%u\"", fetch=True)

keys['y'] = yank ## requires xclip

filters=[show_unread, None]

add("http://www.groklaw.net/backend/GrokLaw.rss")
add("http://stackexchange.com/feeds/tagsets/43442/inaimathi-lang-digests?sort=active")
add("http://www.antipope.org/charlie/blog-static/atom.xml")
add("http://rss.slashdot.org/slashdot/Slashdot")
add("http://kerneltrap.org/node/feed")
```

The one quirk that I have to highlight is that by default, its `update` *doesn't* fetch, it just updates from the local pool. In order to fetch, you actually need to run `canto-fetch` somehow. You can throw it in your `crontab`, but given how I use an RSS reader, it made more sense for me to just bind that to a StumpWM key.

[**feh**](http://feh.finalrewind.org/) is an extremely lightweight command-line imageviewer with options to browse folders, delete files, do slideshows and other assorted goodness. I didn't find this looking for an imageviewer, I found it looking for a way to get a background picture directly in Stump. It turns out that this does it:

```lisp
(defun set-background (bg-image)
  (run-shell-command (format nil "feh --bg-scale ~a" bg-image)))
```

[**lynx**](http://lynx.browser.org/) is something I don't use on a regular basis anymore, but it is quite useful when I need to check a [discussion](http://www.antipope.org/charlie/) or [two](http://www.groklaw.net/) without booting up X. It happens every once in a while.

### Command Line Gaps

There aren't as many as you'd think. In fact, for my purposes, there is exactly one, and it's sort of minor; the lack of good animated gif viewer. There is a [concerted effort at putting one together](http://www.lcdf.org/gifsicle/), but it didn't exactly blow me away. `mplayer` does a half-decent job, but chops when looping and doesn't loop by default (which is sort of helpful [when describing haters](http://cdn.smosh.com/sites/default/files/bloguploads/haters-gators-gate.gif)). `feh` is awesome for stills, but doesn't display gifs in an animated fashion, and neither does Emacs. At the moment, my workaround is to just use [chromium](http://www.chromium.org/) and call it a day.

### Shell UI

Ok, so maybe I lied a little in the previous section. The thing I really don't like about [some](http://www.gnu.org/software/tar/) command [line](http://ffmpeg.org/) programs is their sometimes inscrutable option settings and lack of sensible defaults.

That second one bugged me enough that I whipped up a pair of Ruby scripts to help me out with archiving a little while ago. Yesterday, I ported [them](https://github.com/Inaimathi/shell-ui/blob/master/python/unpack) to [Python](https://github.com/Inaimathi/shell-ui/blob/master/python/pack); what they do, basically, is provide a sane set of default options for creating and decompressing various archives. Instead of `tar -xyzomgwtfbbq foo.tgz`, I can just call `unpack foo.tgz`. `pack -t tar.gz foo/` similarly replaces `tar -cwhyareyoueventryingtoreadthis foo.tar.gz foo/`. I guess I could have done what most of my friends do (memorize the one or two most common combinations and call it a day), but having the machine adapt to humanware seems like the better idea to me.

That's also what caused me to sit down earlier and whip up a first draft at my first [curses](http://en.wikipedia.org/wiki/Curses_(programming_library))-based program. I was trying to pull out sections of some movies into animated gifs, and using `mplayer`/`feh`/`convert` manually proved to be laborious and repetitive. So, I did [this](https://github.com/Inaimathi/shell-ui/blob/master/python/vid2gif).

I call it with a movie file, and the filename I want the result saved to. The program uses a curses interface to

1. lets me pick a part of the movie to pull, using `mplayer` options `-ss` and `-endpos`
2. has `mplayer` output the chosen section as a series of JPGs in a temp folder
3. opens the folder with `feh`, giving me the opportunity to delete some frames as desired
4. once I quit out of `feh`, stitches the remaining frames together into an animated gif

Honestly, I'm not sure how often I'll want to do that again, but hey. I've got the process formalized now, so it should be Pie next time. And, now I know how to [curse in Python](http://docs.python.org/library/curses.html).
