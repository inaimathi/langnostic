I was actually just contemplating my [current backup](/posts/old-machines) setup here, when I came across two things. First, [a thread](http://www.reddit.com/r/linux/comments/kwpc6/has_anyone_else_built_a_system_entirely_out_of/) asking about these experiences, and second, another discarded machine.

![A really dusty, old computer tower](/static/img/old-machines-redux-01.jpg)

It even had the Windows License key sticker still attached. No severed monitor cable to laugh at this time, but still.

![The machines' BIOS screen](/static/img/old-machines-redux-02.jpg)

Booting this one up showed me an, actually, respectable 1.7 Ghz processor and a full gig of Ram. Cracking the case also yielded a couple of surprises. One, whoever last used this machine had it hooked up to a vacuum cleaner running in reverse for at least six months. The amount of dust was insane. To the point that I had to don a facemask/goggles and clean it out outside[^no-pics]. Two, this thing actually contained an old Micro ATX motherboard[^an-aside].

[^no-pics]: No pictures of that sadly, though I can assure you that the dust is now providing nesting materials for no fewer than nine neighborhood birds.

[^an-aside]: As an aside, it seems that ["Micro ATX" is a larger form factor than "Mini ATX"](http://en.wikipedia.org/wiki/Computer_form_factor#Tabular_information), which I thought was a little odd. I assume that the team developing the "Mini ATX" was done second and had to settle for the less impressive name.

![A shot of the interior. It has a pretty tiny motherboard](/static/img/old-machines-redux-03.jpg)

I was surprised, because it had the standard, giant tower typical to desktops of the past few years. One trip to the local computer store provided an appropriate Micro/Mini ATX case.

![A new, much smaller case](/static/img/old-machines-redux-04.jpg)

Transplanting the board over was straightforward, except for two things. First, since this was a found machine, I didn't have that little reference card for what each of the case pins does, so I had to create my own based on what the current case hookup looked like. The USB connectors also took some guesswork since the two cases actually had different types of plugs for them[^the-old-one].

[^the-old-one]: The old one had one chunky plug that fit over the entire USB pin-set while the new one actually had a separate wire per port and each one had a separate plug for the ground pin.

![Close-up of USB connectors](/static/img/old-machines-redux-05.jpg)

Second, I don't happen to have a Mini-ATX-sized ethernet card lying around, so the existing one had to tolerate some minor mods.

![Close-up of the network card](/static/img/old-machines-redux-06.jpg)
![Removing the cover plate of the network card](/static/img/old-machines-redux-07.jpg)

At more or less this time, one of my cats decided it was a good time to put their tail in the path of my chair wheels. I had to take ten minutes or so to calm the little guy down before going further.

I tested whether the thing boots before arranging all that hardware in the case. There really isn't much room in these, it doesn't even look like I can get a second hard drive in unless I want to leave it hanging outside somewhere. The drive that was in the original case was fucked (which I assume is why this unit was disposed of), so I had to pop in one of my spares. It ended up getting a 40GB Western Digital. In the process of picking a new drive, I realized that two of the three stashed ones were out too. I'll strip them for magnets later, I guess.

![A stack of hard drives](/static/img/old-machines-redux-08.jpg)

This brings my lifetime hard drive failure record by brand up to


- 3/6 - Maxtor
- 1/1 - Fujitsu
- 0/4 - OCZ
- 2/43 - Western Digital


Which actually isn't too shabby overall.

This time, I decided to throw a copy of [Parabola](http://parabolagnulinux.org/https/) on it.

![Boot-screeof GNU Parabola Linux](/static/img/old-machines-redux-09.jpg)

The installation was entirely uneventful except for the hard-drive and cat-related problems I've mentioned already. The only challenging part was actually folding everything down into a case that small. There's just a birds nest of wires in there, but it boots and runs properly.

![Interior shot of the new case](/static/img/old-machines-redux-10.jpg)

I'm not entirely sure what I'll use this one for, since we have a media PC in the living room already. I might just get a VGA to RCA converter and toss it in the bedroom with our CRT. The other option is to use it as a random dev box to play around with.
