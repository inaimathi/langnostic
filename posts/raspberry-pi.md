I haven't actually had very much time to explore it yet, but the [Raspberry Pi](http://www.raspberrypi.org/) is officially out.

![](/static/img/pi-1.jpg)

"Officially", because it's been sort of a running joke with my friends that it hasn't been for the past while. The [two](http://www.alliedelec.com/lp/120626raso/?cm_mmc=Offline-Referral-_-Electronics-_-RaspberryPi-201203-_-World-Selector-Page) sites [that](http://downloads.element14.com/raspberryPi1.html?isRedirect=true) sold them had "Register your Interest" buttons, rather than the expected "Add To Cart" or "Buy Now"<a name="note-Sun-Sep-30-225547EDT-2012"></a>[|1|](#foot-Sun-Sep-30-225547EDT-2012), and while many told tales of the legendary owners of these boards, I had failed to meet one for quite a while.

The Pi sitting on my left knee at the moment was picked up at [Creatron Inc](http://www.creatroninc.com/) on College. It cost about $50<a name="note-Sun-Sep-30-225551EDT-2012"></a>[|2|](#foot-Sun-Sep-30-225551EDT-2012), and came with nothing but the board, but I still see myself picking up a few more down the line. This first one is going to get get plugged into some of the mobile computing experiments I plan to do shortly. The next one will either replace the media center PC or my backup server, since it's lower power than either.

At first glance, it seems like I'm a bit late to the party, since the installation went perfectly. Got my Pi, got an SD card<a name="note-Sun-Sep-30-225556EDT-2012"></a>[|3|](#foot-Sun-Sep-30-225556EDT-2012), [downloaded the installation images](http://www.raspberrypi.org/downloads)<a name="note-Sun-Sep-30-225604EDT-2012"></a>[|4|](#foot-Sun-Sep-30-225604EDT-2012), unzipped them, then ran

```
dcfldd bs=4M if=2012-09-18-wheezy-raspbian.img of=/dev/mmcblk0
```

`/dev/mcblk0` is the name of my SD card drive. Also, you'll probably need to install `dcfldd` before running that; if you're ok with not having any kind of progress feedback, just run the same command with `dd` instead.

And that was that. After getting the resulting cards into dat Pi, it booted flawlessly.

![](/static/img/pi-2.jpg)

![](/static/img/pi-3.jpg)

Like I said, haven't had much time to explore, but what I can tell you is that the Debian version


-   uses [the LXDE desktop](http://lxde.org/)<a name="note-Sun-Sep-30-225608EDT-2012"></a>[|5|](#foot-Sun-Sep-30-225608EDT-2012)
-   comes with Python 2.7, Python 3.2, and a bunch of python-based games
-   takes about three seconds to start up a Python 3.2 shell in X, and seems to be able to run at most one
-   comes with Scratch and Squeak<a name="note-Sun-Sep-30-225613EDT-2012"></a>[|6|](#foot-Sun-Sep-30-225613EDT-2012)


By contrast, the Arch distribution they ship is very minimal. Which I guess makes sense, all things considered. As a note here, the [Downloads page](http://www.raspberrypi.org/downloads) mentions that Arch boots to a useable prompt in about 10 seconds. Firstly, that doesn't sound very impressive, given that the Debian version does exactly the same thing if you tell it to run from command line. Secondly, in practice it seems to be closer to 5 hippopotami, which actually *is* impressive out of a general purpose computer the size of my business card.

The Arch Pi doesn't come with anything other than `bash`, `perl` and a `root` account<a name="note-Sun-Sep-30-225618EDT-2012"></a>[|7|](#foot-Sun-Sep-30-225618EDT-2012), and that includes the standard `raspi-config` script that lets you resize your initial SD card partition. Ah well, I suppose there are worse things than having to [play around with `parted` and friends](http://elinux.org/RPi_Resize_Flash_Partitions#Manually_resizing_the_SD_card_on_Raspberry_Pi).

Anyway, like I said, not much time this weekend, between the various work work and play work I've been up to around here. I've literally managed to install the OS, `apt-get` a copy of `mplayer` and get the thing onto my network.

In case you were wondering, I consider that a success

![](/static/img/pi-4.jpg)

Next order of business, getting a Haskell and a Common Lisp running on it, getting a ["case"](http://www.raspberrypi.org/archives/1310)<a name="note-Sun-Sep-30-225623EDT-2012"></a>[|8|](#foot-Sun-Sep-30-225623EDT-2012), and figuring out some sort of [portable input/output strategy](http://langnostic.blogspot.ca/2011/12/x220-and-unrelatedly-portable-keyboards.html).


* * *
##### Footnotes

1 - <a name="foot-Sun-Sep-30-225547EDT-2012"></a>[|back|](#note-Sun-Sep-30-225547EDT-2012) - That's changed by now, obviously, and it's theoretically possible to buy n of them rather than just one per customer, though I've yet to test this theory.

2 - <a name="foot-Sun-Sep-30-225551EDT-2012"></a>[|back|](#note-Sun-Sep-30-225551EDT-2012) - Canadian dollars.

3 - <a name="foot-Sun-Sep-30-225556EDT-2012"></a>[|back|](#note-Sun-Sep-30-225556EDT-2012) - A 16 GB class 10. Class 10 is the important part since that indicates read/write speed. I could have gotten away with as little as 2 GB in terms of size, but the store I walked into happened to have a special on the 16 GBs, so a pair of those ended up costing me about $10 *less* than a pair of two gig class 10s would have. No, I have no idea how this made sense from their perspective. It's not as though flash memory goes stale in blister packs.

4 - <a name="foot-Sun-Sep-30-225604EDT-2012"></a>[|back|](#note-Sun-Sep-30-225604EDT-2012) - One copy each of [Raspbian](http://downloads.raspberrypi.org/download.php?file=/images/raspbian/2012-09-18-wheezy-raspbian/2012-09-18-wheezy-raspbian.zip) and [Arch ARM](http://downloads.raspberrypi.org/download.php?file=/images/archlinuxarm/archlinux-hf-2012-09-18/archlinux-hf-2012-09-18.zip).

5 - <a name="foot-Sun-Sep-30-225608EDT-2012"></a>[|back|](#note-Sun-Sep-30-225608EDT-2012) - Look-and-feel-wise, it's a slightly shinier [XFCE](http://www.xfce.org/).

6 - <a name="foot-Sun-Sep-30-225613EDT-2012"></a>[|back|](#note-Sun-Sep-30-225613EDT-2012) - Though to be fair, I've yet to get Squeak running successfully. Scratch looks like a very interesting teaching tool. The sort of thing you could give a curious six-year-old if you wanted them to learn about programming.

7 - <a name="foot-Sun-Sep-30-225618EDT-2012"></a>[|back|](#note-Sun-Sep-30-225618EDT-2012) - Ok, ok, that's a half-lie. It also comes with the usual *nix suspects, but you know what I mean dammit. It's been a *long* while since I've actually had to install Python anywhere.

8 - <a name="foot-Sun-Sep-30-225623EDT-2012"></a>[|back|](#note-Sun-Sep-30-225623EDT-2012) - Or possibly a [case](http://www.geek.com/articles/chips/raspberry-pi-gets-a-case-you-can-download-and-3d-print-2012035/), depending on how adventurous I feel.
