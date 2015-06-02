I'm not attached to things.

My grandfather was; whenever we'd do some carpentry or light construction, he'd insist that we save old screws and nails we found. It's always seemed weird to me because even in the old country, nails and screws are things you can get at the hardware store for $20 per 5 lbs, so there never seemed to be much of a point in saving old ones. Whenever I'd point this out, he'd reply "You never know when a nail will come in handy", and proceed to stash stray nails in variously sized glass jars. Ok, yes, this was across the Atlantic, so what he actually said was "Nikad neznaš kad ćeš trebati čavlića", but you get the point. He had a musty attic full of clothes he wore decades ago, books he read once, games he played when he was a kid, traveling cases that had only been used once, and a thousand other treasures that I never got to see, but that he likely also could have discarded with no disadvantages. I'm sure there was some socio-political reason for this, but I'm digressing.

I'm not attached to things, and I can say this as someone who has observed humans attached to things. But I still feel a bit perturbed when someone throws a computer away. I'm sure my descendants will have the same reaction I had to the old nails. "Grandpa, you can buy computers for 2¢ per core, they come with ten free petabytes of memory and a lifetime supply of storage on the Googazon servers. Why are you keeping those old things?" I can already feel the urge to tell those smarmy cunts to get the hell off my lan...

For the moment though, I'm slightly less than insane for putting old machines to use. Last week I stumbled upon an [HP Pavillion](http://www0.shopping.com/Hewlett-Packard-HP-Pavilion-6645C-D9948AABA/info) circa 1998 (well, I assume it was thereabouts since it still had Windows 98 installed). With a roaring 566 MHz processor, a truly awe-inspiring 64 MB of SDRAM, and a massive 15GB hard drive. I've been meaning to set up a backup server for my setup here anyway. I still had to spend some money on a couple of hard drives (about $50 each for 160GB IDE drives, I had one lying around, but the rest of my spares are all SATA. Could have saved some money by getting a [couple of adapters](http://www.tigerdirect.ca/applications/SearchTools/item-details.asp?EdpNo=4143846&sku=ULT40322) instead, but I didn't think soon enough. I'll get some of these if the drives ever fail) and an Ethernet card ($4.99).

The first thing I had to do was remove a few unwanted items. 

![](/static/img/old-machines-01.jpg)

As committed as I am to reusing old machines, I've still got to admit that there's very little use today for a phone modem or floppy drive. They were fairly easy to remove; just a couple of mounting screws internally. What was slightly tougher was this plastic face-plate that covered the area next to the front-facing USB port; it was held in by a small, springy metallic assembly that I had to lever out with a Swiss army knife (I wanted another hard drive to go there).

![](/static/img/old-machines-02.jpg)

![](/static/img/old-machines-03.jpg)

Next up, I ripped out the 15GB drive it came with, popped in one of my 160GB ones and threw in that Ethernet card for good measure. Then I installed Ubuntu Server 10.10. It could have been Debian, but I wanted to try out the latest Ubuntu release, and there are some things I'd like to do with pacpl that don't seem to work on my Debian machine. The trade-off is that Emacs seems to misbehave out of the box on Ubuntu, but this isn't exactly going to be a development machine so that's ok. The only stuff that went on was [SSH server](http://www.openssh.com/), [GIT](http://git-scm.com/) and [Ruby](http://www.ruby-lang.org/en/) (my language of choice for quick and dirty scripting).

Once the system was installed, the CD drive could come out (not about to install MS Word or any such nonsense; any other software that goes on this machine will come in through the network). That turned out to be easier said than done though; it was secured by screws on both sides, so I had to completely disassemble the box to get at it.

![](/static/img/old-machines-04.jpg)
![](/static/img/old-machines-05.jpg)
![](/static/img/old-machines-06.jpg)

The hard drive destined for the position was going to rattle in a slot that size, and while I don't plan to race this machine around the block or anything, it's probably better to be safe. A couple of [drive brackets](http://www.nextag.com/hard-drive-adapter-bracket/stores-html) made sure it would stay in place. Shop around if you plan on buying some, incidentally, I just put that link up because it was the first I found; there were actually a couple of braces lying around from my last case so I didn't need to order any. It also seems like you could improvise a set if you didn't feel like buying them.

![](/static/img/old-machines-07.jpg)
![](/static/img/old-machines-08.jpg)

With everything hooked up, it was time to boot back into the machine.

![](/static/img/old-machines-09.jpg)

That in-congruent looking mesh plate covering the top drive is a spare from the same case that had the extra brackets. And yes, I named the machine "orphan". It seemed appropriate. Here's ls /dev, showing the new drives (still haven't formatted them, that'll be for next weekend).

![](/static/img/old-machines-10.jpg)

And that's it. I dropped it into a little wheel assembly that's been going unused since I got that mammoth tower for my main machine. It gives it a somewhat R2-D2 feel (this may be the start of an art project). 

![](/static/img/old-machines-11.jpg)

I'll put together some scripts to copy out key directories from my other machines and that'll be that. I guess I could also use it as a full-out [NAS](http://www.newegg.ca/Store/SubCategory.aspx?SubCategory=124&name=Network-Storage-NAS) (ok, I technically am, but you know what I mean) or streaming server, but I'm not sure how far those 566MHz and 64MB of RAM are going to stretch. In any case, even with the slightly higher price/GB I had to pay for IDE drives, converting this old machine was much cheaper than shelling out for a pre-built.

The [Microsoft Arc keyboard](http://www.microsoft.com/hardware/mouseandkeyboard/ProductDetails.aspx?pid=120) came in quite handy with this project. It's fairly ergonomic, the arrow oddity isn't as annoying as it seems it should be, and the transmitter is easy enough to move around. It's definitely a step up from wrangling USB cables from my main machine about three feet to the side. My only complaint is that it friggin devours batteries, compelled like some primal beast, always growling for more. That's easy enough to solve, I guess, just remove the batteries when it's not in use, but that's a small annoyance on an otherwise perfect spare keyboard.

<!--  LocalWords:  Pavillion Ok Nikad neznaš kad ćeš trebati čavlića socio Googazon SDRAM IDE SATA pacpl ok px that'll deselectBloggerImageGracefully friggin
 -->
