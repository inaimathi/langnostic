This is going to be another three-part blog. Maybe 3.5, I'll see how it goes. You may want to leave after parts 1 or 2.

### web-mote

I've been working on a tiny little utility to marginally improve my life in an almost insignificant way.

You've already heard the casual references to my "media PC", and you may have come to the conclusion that it's just a [standard NAS](http://www.newegg.com/Store/SubCategory.aspx?SubCategory=124) setup, but no. It's actually just a regular computer hooked up to my TV through a VGA and audio wire<a name="note-Wed-Jan-25-223836EST-2012"></a>[|1|](#foot-Wed-Jan-25-223836EST-2012). About a week ago, the wireless keyboard I was using started chugging and finally gave out. Well, being that I mostly listen to music and watch ridiculous low-res videos in `mplayer`, why not just get a remote working? As it happens, I also had an old iPod touch lying around literally collecting dust since I stopped carrying a mobile music device with me. So I put the two together and hacked up a little remote control server for the computer. It basically just starts `mplayer` with `-slave -idle`, runs [Hunchentoot](http://weitz.de/hunchentoot/) out front and then passes along commands when I click on the various video links. I'm using it to watch some downloaded videos from [the science network](http://thesciencenetwork.org/programs/the-science-studio/daniel-dennet) as we speak. The only gap in the interface is that I can't control the actual TV the same way yet (so I still need to get up to change channels or up the output volume)

The code is up [at my github](https://github.com/Inaimathi/web-mote), as usual. I just noticed that there's no license file, though.

Just a second.

Ok. Compiling and inserting that took almost as long as writing the actual code. There you have it, in any case. I don't seriously recommend you use this program until I've ironed out one or two things, but feel free to if you like. I definitely enjoy being able to control my media center from whatever HTML client I happen to have at hand.

Next step: figuring out how to [control the TV](http://superuser.com/questions/198709/cheap-simple-way-to-turn-tv-on-off-using-computer-and-or-windows-meda-center-rem) through wifi (though early research is not encouraging).

### fingerquotes open

This was just sort of depressing.

I dunno, maybe it's not that big a deal to most people, but I'm depressed.

A good third to a half of my last weekend was spent researching ways of getting [Open Genera](http://en.wikipedia.org/wiki/Genera_(operating_system)) up and running, only to find out that "Open" doesn't quite mean what I thought it did in this context. Granted, the system was built back in the 80s, so I guess the word may not have had the same connotation, but I still got confused.

It's bizarre, because I honestly don't get the point of a closed-source Lisp system. The whole point is that the entire machine is there, open to pokes and prods at its various sources and definitions. Saying that it's not being released openly or freely is just ... I dunno, off. The message is so fundamentally incongruous with the medium that it seemed to come at me entirely out of left field<a name="note-Wed-Jan-25-234149EST-2012"></a>[|2|](#foot-Wed-Jan-25-234149EST-2012). I guess that'll teach me to read the license first next time.

I still have my [ersatz lisp machine](http://langnostic.blogspot.com/2012/01/how-close-can-you-get-to-lisp-machine.html), I guess. And I could do a bit of poking and hacking on [Movitz](http://common-lisp.net/project/movitz/) if I really wanted to. That'll have to hold me.

* * *
##### Footnotes

1 - <a name="foot-Wed-Jan-25-223836EST-2012"></a>[|back|](#note-Wed-Jan-25-223836EST-2012) - Incidentally, the mediaphiles among you should refrain from telling me that I should be using HDMI instead. Enough of my friends tell me that already, and I could give a shit. I mostly listen to music and watch almost ridiculously low-res videos; there's a separate DVD player for the [occasional high-def media](http://www.davidattenborough.co.uk/) I watch.

2 - <a name="foot-Wed-Jan-25-234149EST-2012"></a>[|back|](#note-Wed-Jan-25-234149EST-2012) - Enough to delete the relevant article from my archives (sorry, to the people who linked to it already) and never speak of it again<a name="note-Wed-Jan-25-234332EST-2012"></a>[|3|](#foot-Wed-Jan-25-234332EST-2012).

3 - <a name="foot-Wed-Jan-25-234332EST-2012"></a>[|back|](#note-Wed-Jan-25-234332EST-2012) - Except for here, obviously.
