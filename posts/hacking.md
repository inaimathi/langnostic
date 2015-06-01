This week has been kind of a mixed bag for me; I've been thinking about a bunch of stuff, but not enough about each thing to justify a whole blog post. So here's the result of my mental mastication. It's not pretty, but perhaps it will be nourishing.

## <a name="on-serving-data"></a>On serving data

I've been administering my own server for the last little while. First for DieCast (which is on hold for the moment 'cause the server was needed for something else) then to host some ASDF files (which are actually back up now; you should be able to asdf-install:install 'cl-css or 'formlets without serious problems), and now for a couple of websites I'm doing work on. The experience has taught me three things.

1. Common Lisp webapp deployment sucked balls before [Quicklisp](http://www.quicklisp.org/beta/)

My first [nginx](http://nginx.org/en/)+[Hunchentoot](http://weitz.de/hunchentoot/) setup took hours. Some of this was for lack of familiarity with the apps because my second deployment took hours (fewer of them though). Which is an excellent improvement, but still not good in the absolute sense. The main problem was actually setting up Hunchentoot; it has many dependencies (many of which have several recursive dependencies of their own), each of which need to be downloaded and evaluated separately, each of which has at least one compiler warning, and one of which usually fails to download. The worst deployment after the first involved a key ASDF-package hosting site going down. That meant I had to go out and download + install + load all of Hunchentoots' dependencies recursively by hand in order to get them running. Sadly, lacking encyclopedic knowledge of Hunchentoot, this meant I had to try (asdf-install:install 'hunchentoot), wait for it to error out, get the piece it errored on, install it and try again. Once the server was up it was awesome, but getting it to that state was a pain in the ass the likes of which I'm having trouble analogizing properly. Quicklisp does it in 10 minutes, while simultaneously massaging my aching shoulders. I really hope zach doesn't start charging, because I get the feeling many Common Lispers would end up owing him their house (he welcomes [donations](http://www.quicklisp.org/donations.html), of course).

2. System setup sucked balls before Linode

I used to use [Cirrus Hosting](http://www.cirrushosting.com/toronto-web-hosting-canada.html). And actually still do at work; we had them before I came in, and they're pretty good so I don't have a burning need to switch over, but we'll see what's possible once our subscription is up. Basically, I was used to a VPS being more or less just a regular server, except virtual. You have to spend a bit of time installing the distro, reboot, and install. It turns out that if you put thought into the process, a lot of that startup time can be done away with behind the scenes. [Linode](http://www.linode.com/) has put a lot of thought into the process. Going from one linux distro to another takes something like 5 minutes. I found this out bouncing between different linuxes ([linuxen? linuces?](http://superuser.com/questions/130490/what-is-the-plural-of-linux)); the process was initiated and that's typically a cue for sandwiches, but I didn't have enough time. Needless to say, it was a pleasant surprise the first time a deployment from bare metal to a running Common Lisp server took less than half an hour.

3. Break-in attempts are surprisingly common

If I'm to believe my auth.log, a concerted effort at hacking is made by some jackass roughly every two days. Needless to say, my iptables have been modified. It's different IPs, but always the same MO; they try some random common usernames, fail and go away. Apparently it's escaped their notice that I switched to RSA keys and disabled password/PAM authentication. To be fair, checking the logs, it seems that before the change to key-based auth, the situation regularly looked like

```
Feb 10 07:40:04 Invalid user abc from 61.240.36.1
Feb 10 07:40:07 Invalid user abc123 from 61.240.36.1
Feb 10 07:40:10 Invalid user benjamin from 61.240.36.1
Feb 10 07:40:12 Invalid user lstiburekz from 61.240.36.1
Feb 10 07:40:15 Invalid user kent from 61.240.36.1
Feb 10 07:40:18 Invalid user jabber from 61.240.36.1
Feb 10 07:40:20 Invalid user andres from 61.240.36.1
Feb 10 07:40:23 Invalid user dovecot from 61.240.36.1
Feb 10 07:40:26 Invalid user magda from 61.240.36.1
Feb 10 07:40:28 Invalid user alex from 61.240.36.1
Feb 10 07:40:31 Invalid user stefan from 61.240.36.1
Feb 10 07:40:34 Invalid user stefano from 61.240.36.1
Feb 10 07:40:36 Invalid user cristi from 61.240.36.1
Feb 10 07:40:39 Invalid user claudi from 61.240.36.1
Feb 10 07:40:42 Invalid user sarah from 61.240.36.1
Feb 10 07:40:44 Invalid user smokeping from 61.240.36.1
Feb 10 07:40:47 Invalid user fetchmail from 61.240.36.1
Feb 10 07:40:50 Invalid user backinfo from 61.240.36.1
Feb 10 07:40:53 Invalid user umberto from 61.240.36.1
Feb 10 07:40:55 Invalid user mauro from 61.240.36.1
Feb 10 07:40:58 Invalid user jana from 61.240.36.1
Feb 10 07:41:01 Invalid user adriano from 61.240.36.1
Feb 10 07:41:03 Invalid user xenie from 61.240.36.1
Feb 10 07:41:06 Invalid user lea from 61.240.36.1
Feb 10 07:41:09 Invalid user joule from 61.240.36.1
Feb 10 07:41:11 Invalid user Debian-exim from 61.240.36.1
Feb 10 07:41:14 Invalid user unbunutu from 61.240.36.1
Feb 10 07:41:17 Invalid user cacti from 61.240.36.1
Feb 10 07:41:19 Invalid user polycom from 61.240.36.1
Feb 10 07:41:23 Invalid user payala from 61.240.36.1
Feb 10 07:41:26 Invalid user nicola from 61.240.36.1
Feb 10 07:41:28 Invalid user melo from 61.240.36.1
Feb 10 07:41:31 Invalid user axfrdns from 61.240.36.1
Feb 10 07:41:34 Invalid user tinydns from 61.240.36.1
Feb 10 07:41:36 Invalid user dnslog from 61.240.36.1
Feb 10 07:41:39 Invalid user dnscache from 61.240.36.1
Feb 10 07:41:42 Invalid user qmails from 61.240.36.1
Feb 10 07:41:45 Invalid user qmailr from 61.240.36.1
Feb 10 07:41:47 Invalid user qmailq from 61.240.36.1
Feb 10 07:41:50 Invalid user qmailp from 61.240.36.1
Feb 10 07:41:53 Invalid user qmaill from 61.240.36.1
Feb 10 07:41:55 Invalid user qmaild from 61.240.36.1
Feb 10 07:41:58 Invalid user alias from 61.240.36.1
Feb 10 07:42:01 Invalid user vpopmail from 61.240.36.1
Feb 10 07:42:03 Invalid user ldap from 61.240.36.1
Feb 10 07:42:06 Invalid user gica from 61.240.36.1
Feb 10 07:42:09 Invalid user sympa from 61.240.36.1
Feb 10 07:42:11 Invalid user snort from 61.240.36.1
Feb 10 07:42:14 Invalid user hsqldb from 61.240.36.1
Feb 10 07:42:17 Invalid user member from 61.240.36.1
Feb 10 07:42:20 Invalid user chizai from 61.240.36.1
Feb 10 07:42:22 Invalid user yakuji from 61.240.36.1
Feb 10 07:42:25 Invalid user gijyutsu from 61.240.36.1
Feb 10 07:42:28 Invalid user kaihatsu from 61.240.36.1
Feb 10 07:42:30 Invalid user iwafune from 61.240.36.1
Feb 10 07:42:33 Invalid user oomiya from 61.240.36.1
Feb 10 07:42:36 Invalid user seizou from 61.240.36.1
Feb 10 07:42:38 Invalid user gyoumu from 61.240.36.1
Feb 10 07:42:41 Invalid user boueki from 61.240.36.1
Feb 10 07:42:44 Invalid user eigyou from 61.240.36.1
Feb 10 07:42:46 Invalid user soumu from 61.240.36.1
Feb 10 07:42:49 Invalid user hanaco_admin from 61.240.36.1
Feb 10 07:42:52 Invalid user hanaco from 61.240.36.1
Feb 10 07:42:54 Invalid user system from 61.240.36.1
Feb 10 07:42:57 Invalid user tenshin from 61.240.36.1
Feb 10 07:43:00 Invalid user avahi from 61.240.36.1
Feb 10 07:43:02 Invalid user beaglidx from 61.240.36.1
Feb 10 07:43:05 Invalid user wwwuser from 61.240.36.1
Feb 10 07:43:08 Invalid user savona from 61.240.36.1
Feb 10 07:43:10 Invalid user trthaber from 61.240.36.1
Feb 10 07:43:13 Invalid user proftpd from 61.240.36.1
Feb 10 07:43:16 Invalid user bind from 61.240.36.1
Feb 10 07:43:19 Invalid user wwwrun from 61.240.36.1
Feb 10 07:43:21 Invalid user ales from 61.240.36.1
```

whereas I now merely get the occasional

```
Feb 12 11:53:12 Invalid user oracle from 212.78.238.237
Feb 12 11:53:13 Invalid user test from 212.78.238.237
Feb 12 12:03:59 Invalid user apache from 79.174.78.179
Feb 12 20:16:59 Invalid user postgres from 79.174.78.179
```

So it helps, but the regularity of these attacks is still surprising to me. It seems a bit odd that a script would keep trying if it got the refused (publickey) error, so I'm forced to conclude that there are one or two spammers out there manually looking for servers they can break into. That's ... odd. And I can't shake this picture of a 12 year old in some spamming sweatshop somewhere failing to break into my server and missing his quota as a result.

## <a name="on-starting-up"></a>On starting up

So remember back in the prehistoric ninties, when the likes of [this strange creature](http://en.wikipedia.org/wiki/Super_Nintendo_Entertainment_System) walked the earth? When the [Playstation](http://en.wikipedia.org/wiki/PlayStation_(console)) first introduced the idea of CD-based games to the console market, a friend of mine flatly said he preferred his SNES. When questioned about it, his reasoning boiled down to one word. 

"Loading..." 

For the youth who never experienced this; a Super Nintendo had no loading screens anywhere. You put the cartridge in, hit the power button, and it would go straight to the logo screen. While in-game, moving between areas was instantaneous. It seems like most people working in the consumer electronics industry today have either forgotten that instant usage is really good, or they never thought so to begin with. The latest generation of consoles has loading screens friggin everywhere. A different friend of mine purchased a TV recently that has a 30 second boot cycle, and comes with a network connection for the purpose of getting firmware updates. A fucking teevee. It's hilarious that between the TV boot time and the console boot time (and I won't even mention the install time on the console because it's really unfair), it actually takes longer to start a game of whatever in his living room than it does on my computer. Weird, because I thought the whole point of consoles was that they were special-purpose devices specifically designed to run games. Entertainment isn't the end of this trend though; my phone now also takes about a minute to start up (which is fair I guess, since it basically is a computer now, complete with a flavor of Linux and a web browser). Finally, my parents recently renovated their kitchen and procured for it a, I shit you not, dishwasher that needs to boot before it starts pulling in water.

At what point did this start happening? When the hell did the decision get made in the bowels of Sony corporate HQ that it was ok for my display to have a configuration cycle? If this is where the future of TVs is going, I may very well have already bought my last non-monitor display. But beyond entertainment, my greater concern is the trend of [ephemeralization](http://en.wikipedia.org/wiki/Ephemeralization) (as [elaborated by Graham](http://www.paulgraham.com/tablets.html) to mean "...the increasing tendency of physical machinery to be replaced by what we would now call software.") combined with the new human habit of sticking computers into things means that we are likely to soon have shoes, lip-balm and kitchen cutlery that come with their own fabulously designed and meticulously polished loading screens.

Somehow, I'm not enraptured by this prospect.

## <a name="on-data-moving"></a>On data moving

It's come to my attention that the Canadian government has recently had a [nontrivial](http://www.tmcnet.com/scripts/print-page.aspx?PagePrint=http://telecommunications.tmcnet.com/topics/telecommunications/articles/144739-canadians-brawl-over-internet-usage-caps.htm) (and [ongoing](http://communities.canada.com/vancouversun/blogs/innovation/archive/2011/02/14/shaw-seeks-customer-feedback-on-usage-based-internet-charges.aspx?CommentPosted=true#commentmessage)) [tussle](http://openmedia.ca/meter) with the CRTC and the major Canadian ISPs about whether or not they should be allowed to charge arms and legs for data overages. That last link was actually to the [Open Media](http://openmedia.ca/meter) site, which is organizing a petition against the CRTCs move. If you're in Canada, you should probably sign it. My position is basically that I don't care, because the way I use the internet, 40GB is essentially unlimited. I'm not a [netflix](http://ca.netflix.com/Default?autoRedirect=off&mqso=80027678) user (though I'm constantly told I should be) I don't [torrent](http://thepiratebay.org/) the [games](http://www.lmptfy.com/?q=games&t=400) like the kids these days, and downloading Linux packages is a joke if you're running the minimal system I've got over here. The single largest component I install is haskell-platform, which takes something like 600Mb. With an M. Even with my fiancee being perhaps the worlds' biggest [YouTube](http://www.youtube.com/) [makeup](http://www.youtube.com/user/juicytuesday) [video](http://www.youtube.com/user/MissChievous) [fiend](http://www.youtube.com/user/sayanythingbr00ke), we've never actually approached the limit of our plan. My interest in this fight is purely in the interest of a theoretically unfettered future; one where data is as free as it could possibly be, and that world includes no limits on how much it's allowed to move per month (incidentally, that's also why I frown when I see things [like this](http://www.reddit.com/r/programming/comments/fkt7t/nemerle_factor_alice_ml_and_other_programming/) happening; freedom of information includes the right for said information to exist). So I'm against the CRTC here, but seemingly not for the same reason as anyone in a 100 km radius of me.
