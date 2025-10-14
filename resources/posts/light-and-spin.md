Ok, I'm tapping. 

I found another interesting thing I want to work on, or rather that I want to exist in the world, but my plate is already almost overflowing. So, if you're in a similar position as me, but have slightly more time to devote to R&D things, please put some weight behind the project. And, for preference, post build videos, blog posts, and/or a CrowdSupply page so I can get one when you build it. Or, I guess, alternatively throw enough money/encouragement at me to convince me that this is worth prioritizing over my other projects? I dunno, not sure what that would take.

# Computed Axial Lithography

is a resin printing technique best illustrated with [this video](https://www.youtube.com/watch?v=L7QnADt04ZU). The idea is

1. get a bunch of transparent resin with a particular formulation together (the photoreactivity threshold, and viscosity are important for reasons that will become obvious)
2. put it in a rotating vat in a dark-ish environment
3. use a projector to project a blue-light image at the vat. The image is a projection of a rotating STL that matches speed and direction with your vat of resin
4. let it run for between 1 and 3 minutes
5. pull out a hardened resin print of the object you were projecting

The photoreactivity threshold is important because you _don't_ want the resin to harden instantly on contact to light, but _do_ want it to harden over multiple exposures or a longer exposure. The viscosity is important because you want the partially hardened resin to stay supported by its surrounding medium rather than sinking to the bottom of the vat.

This is fucking magic.

## The Advantages

of this sort of printing should be obvious:

1. This is _way_ faster than existing FDM/resin/powder deposition/whatever.
2. The time taken to print something with this technique doesn't depend on the size of the thing. You can print a 20cm bust or a 2cm mini in about the same amount of time
3. The detail level of the model seems to be limited entirely by the resolution of your projector. Some of the preview prints I've seen are high-detail structure at 4mm, which means you have _lots_ of options that even current resin prints can't match
4. You can print self interlocking pieces. There's a [paper](https://www.sciencedirect.com/science/article/pii/S2214860424002781) that shows off a captured ring and bar model printed "in place" with no seams or assembly.
5. You can print ridiculous bridges and overhangs with no support consideration, other than the final weight/volume distribution of the model. Basically all the tricks you needed to understand in order to create interesting sculptures with FDM/resin printers are unnecessary here.
6. Assuming you're not doing weird fully enclosed prints, it's incredibly material efficient. If you get the photoreactivity threshold and extraction process right, you can reuse any suspension resin. The way that video and paper make this look, it's entirely workable to just keep topping your vats up between prints instead of tossing the rest of the medium.

## The Disadvantages

1. This is still a research technology, you can't go out and buy a CAL printer yet the way that you can get your choice of reliable, pretty low cost FDM or resin printers. There _is_ [research](https://www.sciencedirect.com/science/article/pii/S2214860424002781) and [praxis](https://github.com/computed-axial-lithography) though. In particular, that paper shows that you can build these things out of spare electronics, RasPI displays, and FDM printed components. It looks like the printers themselves would be really cheap to produce.
2. You need a light sensitive and chemically oriented lab setup. A step beyond the current resin printers, which realistically only need a chemically oriented setup. You can't do resin or CAL printing in your living room the way you kind of can with FDM. This is actually the main reason I don't want to deal with this project at the moment; all of my other fun/profitable stuff can be done either from home or a generic tech co-working space. Doing anything with CAL would at minimum require membership at a specialized maker space, and possibly require dedicated lab space in a hazmat certified facility. I'm not about to start researching that unless someone else is willing to pick up the tab :p
3. As far as I can tell, the resin is much harder to source than standard printing resins or filaments. My best attempts at pricing out things that ship to Toronto can be found [here](https://chatgpt.com/share/68ed9426-e744-800f-90ae-994591601f20). The bottom line there is that you're looking at something like $500/kg for these resins, which might make the entire thing moot unless some of the properties of the fabrication process are absolute requirements.

Oveall, this looks like a pretty cool maker project, which I would _totally_ be down for if I didn't already have five of them going. This time, I _won't_ be letting you know how it goes. Perhaps you will though?
