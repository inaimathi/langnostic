I posted a quick piece a couple days ago about using [Clisp as a Shell](http://langnostic.blogspot.com/2012/01/how-close-can-you-get-to-lisp-machine.html) wherein I wondered how close such an environment could actually be to the traditional [Lisp Machine](http://en.wikipedia.org/wiki/Lisp_machine).

It was really just an update on a toy I'd been playing with for a bit, but it still made it obvious that to me that I wouldn't be able to think about it properly unless I actually tried the original environment<a name="note-Sun-Jan-22-180021EST-2012"></a>[|1|](#foot-Sun-Jan-22-180021EST-2012). Well, I didn't quite get a chance to play around with it in depth yet, but I did manage to use some hours profitably to set up an Ubuntu VM running Open Genera 2.0 with [VirtualBox-OSE](https://www.virtualbox.org/wiki/Downloads).

![](http://2.bp.blogspot.com/-bkICacwbHMA/TxyvXoj1GLI/AAAAAAAAALY/G97o5wnQHMU/s400/open-genera.png)

The process isn't terribly difficult once you know approximately what you're doing, but it's still much harder to install this on top of Ubuntu than it is to install either Debian or Ubuntu themselves. I guess at least part of that is because it's happening on a virtual machine, and I have some odd SSH settings on my main computer so I needed another way to transfer files. Python helps there, incidentally. I ended up using `python -m SimpleHTTPServer` in the directory with the relevant archives and just HTTPing them over. Hacky, but it works.

The most useful set of instructions can be found right [here](http://collison.ie/blog/2008/04/lisp-machines), but they're still slightly outdated. First off, since those instructions were written up, [Gutsy Gibbon](http://old-releases.ubuntu.com/releases/7.10/) has lost all support. The oldest edition with an active official repository server is [Hardy Heron](http://releases.ubuntu.com/8.04/), which means that installing the base system isn't quite as easy as `apt-get`ting.You'll need to add 

```
deb http://archive.ubuntu.com/ubuntu/ hardy main restricted
deb-src http://archive.ubuntu.com/ubuntu/ hardy main restricted
```

to your `/etc/apt/sources.list`, then run `sudo apt-get update` and then run

```
sudo apt-get install xorg nfs-common
```

You'll actually need to restart your machine at this point<a name="note-Sun-Jan-22-183522EST-2012"></a>[|2|](#foot-Sun-Jan-22-183522EST-2012), since installing a new `xorg` will make your X server wonk out. Once you get back, you'll need to download [nfs-user-server](http://ftp.us.debian.org/debian/pool/main/n/nfs-user-server/nfs-user-server_2.2beta47-25_amd64.deb) and [inetutils-inetd](http://security.debian.org/debian-security/pool/updates/main/i/inetutils/inetutils-inetd_1.5.dfsg.1-9+lenny1_amd64.deb) .deb files from the Debian Lenny distribution and use Ubuntu's package manager to install them in that order (you should be able to just right click on them and select "Open with GDebi Package Installer"). You should then be able to follow the rest of the instructions from [Collison's blog post](http://collison.ie/blog/2008/04/lisp-machines).

I'm going to export my VM image and toss it up online somewhere so that you can just import it and skip to the "Run Genera" part without dicking around with that whole thing.

> EDIT:
> I should say, I was *going* to post it. It's since been pointed out to me that Open Genera doesn't actually use an open source license. So, sorry, you're out of luck and will actually have to follow the instructions I linked to along with the amendments I posted here. Good luck.
> Mon, 23 Jan, 2012

There's not really enough patience or time left in the day for me to do any more computer-related work, but I did manage to evaluate a couple of expressions and poke around for 20 minutes or so. First impressions are mixed. 

On the one hand, this window system doesn't respect the niceties of modern windowing systems. Scroll bars don't work like they're supposed to, the machine assumes you have `&lt;Abort>`, `&lt;Help>` and `&lt;Select>` keys, and doesn't do seemingly basic stuff like tab-completion or correctly interpreting the `&lt;Backspace>` key.

On the other hand, you can reflectively edit any part of the system you like. Either through a context-sensitive mouse menu, or by evaluating things like `Edit Definition` (which will actually open up the Zmacs editor for you, and browse to the file and line where the definition you want to edit is stored).

There's obviously a usability gap between Open Genera and today's OSes, but the *functionality* gap isn't nearly as expansive as you would expect from something that was developed back in the early 1980s<a name="note-Sun-Jan-22-190708EST-2012"></a>[|3|](#foot-Sun-Jan-22-190708EST-2012). I'm quite impressed by how powerful this software is, given that it's actually older than I am.

* * *
##### Footnotes

1 - <a name="foot-Sun-Jan-22-180021EST-2012"></a>[|back|](#note-Sun-Jan-22-180021EST-2012) - Not on the actual machines obviously, I don't have enough time or money to actually try to track one down.

2 - <a name="foot-Sun-Jan-22-183522EST-2012"></a>[|back|](#note-Sun-Jan-22-183522EST-2012) -  Or at least log out and log back in again.

3 - <a name="foot-Sun-Jan-22-190708EST-2012"></a>[|back|](#note-Sun-Jan-22-190708EST-2012) -  And for programming purposes, it seems that the gap is actually not in the direction you'd expect; with the Lisp system being marginally ahead of modern environments. It's almost as if we've spent ~30 years making things unnecessarily shiny, round, and locked down instead of adding features.
