I've been obsessed with 3D printers lately, and I'm tired of pretending I'm not. So here's a stream-of-consciousness graphsticle on the topic. I'll try to make this both generally useful and accurate, but guarantee neither; it's just how I currently think about the field and what interests me. 

Also, there's going to be lots of links in this article. I _need_ you to understand that a link here is neither an endorsement nor an anti-endorsement, just a pointer to relevant information.

Being so disclaimed, let's survey the terrain.

# 3D Printer Categories

I'm tempted to call it three categories of printers. FDM, Resin and "Other". In reverse order:

## Other

Powder bed fusion, binder jetting, directed energy deposition, sheet lamination and some more specialized techniques. Some of these are sintering processes and therefore still _technically_ "Fused Deposition Modeling" but weird. Some are industrial processes that are either impossible or merely prohibitive at the hobbyist level, none of them are particularly interesting to me, and that seems to be more or less arbitrary.

Moving on.

## Resin

As far as I can tell there are two types of resin printers. I've already talked about [computed axial lithography](https://inaimathi.ca/posts/light-and-spin), and it's highly experimental. As far as I know there are no commercially or industrially available CAL machines yet, but holy crap the payoff would be huge.

The other approach always seems to have the same kinematics, but different photoexposure strategies. The kinematic strategy is always

- there is a build plate secured to an arm that moves it along the Z axis[^as-an-aside]
- there is a light element that does something to the "current layer" of print and then waits for the Z axis to move up
- the build surface bottoms out dipped in a vat of printing resin and moves up away from it

[^as-an-aside]: As an aside, I'm using the standard printer notation here which assumes the perspective of someone looking at the build volume from the top down. So if you're looking a printer "in the face", the X and Y axes are the two that move along the print surface in either direction and the Z axis is the one moves up and down. It seems mildly counterintuitive to me but I'm working on it.

If you see videos of resin prints, they tend to have a particular type of support structure built for printing down from the ceiling instead of up from the floor, and the prints emerge "upside down" to viewers before being detached. This is a predominantly resin approach, but not "uniquely" because the [Lemontron](https://lemontron.com/) exists.

These approaches tend to be really good for fine detail, but aside from some engineering resins, don't have good battlefield applications. So you see a lot of sculptures and minis coming out of resin machines, and not a lot of RC cars, drones or Nerf blasters ([Zack Freedman](https://www.youtube.com/watch?v=J13i7aBB4gw) notwithstanding).

### Laser Stereolithography

is the approach where the photoexposure element is a laser that scans the current layer point-wise. Examples are the [Formlabs 3+](https://www.bunniestudios.com/blog/2020/formlabs-form-3-teardown/) and [Form 4](https://formlabs.com/3d-printers/form-4/?srsltid=AfmBOopSraqnGzHiELH3hJ4ngWufAIriIgDW2cvX66dnlto2zxOWkWoL). I... don't know much about this, except that it seems like it needs a more complicated slicer approach than standard resin printers, and seems to give up the "prints take linear time in height" advantage that I associate with resin printers.

### MSLA/LCD

is what most hobbyists mean when they say "resin printing". This encompasses most of the [Elegoo](https://ca.elegoo.com/collections/mars-series?srsltid=AfmBOorUklEYJOVYl_lf3QEOlnWJE6pJqcb6fA-SmRi7NduuN6kV2b5G) and [Anycubic](https://www.amazon.ca/ANYCUBIC-Photon-Printer-LighTurbo-Printing/dp/B0CZQTKMMZ) offerings. The approach is having an LCD screen or projector flash an entire layer at once. The big advantage is that a print takes an amount of time growing in height, regardless of layer complexity.

## FDM

When I say "3D printing" without specifying in casual conversation, I mean "desktop FDM". With [a few](https://www.amazon.com/Gizmo-Dorks-2-85mm-Filament-Printers/dp/B00LAJNJL2) unfortunate [exceptions](https://3dprintingcanada.com/products/natural-1-75mm-3dxtech-fluorx%E2%84%A2-pvdf-filament-500-g), the materials involved in FDM won't kill you or choke you to death, and if you're only laying down PLA/TPU/PETG, you can contain pollutants with standard air filters and ventilated enclosures. The main reason this has my interest is that it is _absolutely_ possible to get one of these up and running at home, and it's getting cheaper and cheaper. Cost of admission these days is like $300 CAD, possibly less if you're willing to scrounge around Kijiji or get a hand-me-down from a local makerspace.

The common ground here is that they all have one or more toolheads, each of which has one or more hotends that take in material, get it to a viscous-but-flowing-state, and squirt layers at some set width and height. I'm being particularly wishy-washy on that material definition statement because the FDM printer space encompasses [MudBots](https://www.mudbots.com/), [the Cocoa Press](https://cocoapress.com/en-ca) and [the Maple 4](https://www.mapleglassprinting.com/) as well as the more standard [filament squirters](https://www.sovol3d.com/) I'll be focusing on.

All their prints have visible layer lines unless you post process them, but prints can be almost arbitrarily light and solid depending on what material tradeoffs you're willing to make. Filament is also _really_ cheap. At the low end, something like $12 CAD per kilogram for the good stuff (you can get cheaper bargain bin deals from Ali Express, but be prepared to deal with more dimensional inconsistency and therefore clogging and feeding issues). At the other end, it's also relatively easy to source engineering filaments with almost arbitrary [strength](https://www.amazon.ca/Polymaker-PA6-GF-Filament-1-75mm-Cardboard/dp/B0B45QMCJP?th=1)/[temperature tolerance](https://www.3dxtech.com/products/thermax-tpi-1)/[chemical resistance](https://www.3dxtech.com/products/carbonx-peek-cf10-1)/[bounce](https://www.amazon.ca/Siraya-Tech-Rebound-PEBA-Ultra-Lightweight/dp/B0FH1MJ89D) although definitely don't expect to be able to run any of those on an entry level printer. You can get into this hobby for about [$300](https://www.amazon.ca/Creality-Ender-V3-SE-8-66x8-66x9-84/dp/B0DSKCS6Y1/)-[$400](https://www.amazon.ca/Sovol-SV06-ACE-Structure-8-66x8-66x9-84/dp/B0DFYK5Q3L/), but if you want print out a space shuttle, you're looking at [more like $15k](https://visionminer.com/products/22idex).

### Bedslingers

This is the most common design because it's the cheapest and easiest to put together. This category includes the [Ender 3](https://www.amazon.ca/Creality-Ender-V3-SE-8-66x8-66x9-84/dp/B0DSKCS6Y1/), the [SV06](https://www.sovol3d.com/products/sovol-sv06-best-budget-3d-printer-for-beginner), the [Prusa MK4](https://www.prusa3d.com/product/original-prusa-mk4s-3d-printer/), and way more besides. The key characteristics are

1. There is a hotend that moves along the X and Z axes
2. There is a bed that moves along the Y axis

Because they're mostly made to be cheap, the vast majority of these boys don't have a toolchanger or multiple hotends. They're intrinsically moddable because they're so simple, and they're a decent first step into the hobby.

#### Cantilevers

As far as I'm concerned, a bedslinger variant. Fight me. The main ones that come to mind here are the [Prusa Mini](https://www.prusa3d.com/product/original-prusa-mini-semi-assembled-3d-printer-enclosure-bundle-5/) and the [A1 Mini](https://us.store.bambulab.com/products/a1-mini?srsltid=AfmBOophV5n_Nq4v3DwZLBmlepNtVAinHctVjTEt3YH06b6Zq-ZzEkhl). Instead of a frame that sits astride the bed along the X axis, the toolhead is mounted on a linear rail across the top with a single post. I've never actually used one, but it seems all the low-end/mini printers are this type, so I assume there's some mechanical simplicity or cost metric that they max out.

#### Belt Printers

Again, far as I'm concerned, this is a bedslinger variant. Except it has an effectively infinite Y axis. The canonical example that comes to mind is the [BabyBelt](https://github.com/RobMink/BabyBeltPro) (also available from [West3D](https://west3d.com/products/baby-belt-pro-v2-5-complete-diy-belt-printer-kit-by-ldo-systems-and-west3d?srsltid=AfmBOoq4k_liplyEETH0RFzeDWfVqmK_1uqb9foAQ6-BWjoYRgidBAX5))  but there's also a [MALYAN unit](https://www.amazon.ca/MALYAN-Infinite-Conveyor-Motherboard-Printing/dp/B0B6NLSQZS) that fits.

In practice these are harder to use effectively because not all slicers can deal with generating the tilted `gcode` you need to make them work, and you get less control around your print orientation. The upside is that infinite Y axis, which means both that you can print really long things like [kayaks](https://www.youtube.com/watch?v=9DpMkYDCq9Y), and that you can effectively use these as production lines that crank out models at high-volume with no [bed-shuffling nonsense](https://www.amazon.ca/HICTOP-Automatic-Auto-Swapping-Capacity-Hands-Free/dp/B0FB3PZFJ5). If you need to print like 200 of something in relatively short order, and you can make the orientation work, it's not a bad way to go.

### CoreXY

More complex units than bedslingers, because the gantry moves in multiple directions. These are built in cube frames rather than square frames. They have a gantry that lets the toolhead move in the X and Y axes while staying stationary in Z. The bed just moves up and down in Z, which simplifies the motor situation there. The [Voron Trident](https://vorondesign.com/voron_trident), the [Prusa Core One](https://www.prusa3d.com/product/prusa-core-one/), [the100](https://github.com/MSzturc/the100), the [T250](https://github.com/MSzturc/T250) and the [Velociraptor](https://www.youtube.com/watch?v=zEN4l8Vj5LQ) are all examples of CoreXY machines (the last one has a cross gantry, but it still fits because the motors are stationary). Experimental builds of these tend to be speed machines, because this design hits a sweet spot in the weight/power curves that allows you to set records. Both the cartesian gantry machines and the flying gantry machines make slightly different trades that moves them off of that sweet spot in either direction.

#### Cartesian Gantry

A variant of CoreXY which you could be forgiven for missing entirely. Most CoreXY machines have stationary motors fixed in the gantry, which cooperate to move the toolhead in X and Y. _Cartesian_ gantries have separate motors for the X and Y axes. The trade off is that putting belts together is easier, and plausibly it's slightly easier to put such a printer together from commodity parts. And that trades off against weight; because your motors move an axis independently either your X or your Y motor is going to move along with the axis. The [Ultimaker S3](https://ultimaker.com/3d-printers/s-series/ultimaker-s3/) is the only one of these that comes to mind. I _think_ the [Makerbot Replicator](https://www.youtube.com/watch?v=KgwFTv9J7ik) might be one too, but I've never inspected one closely enough to verify.

### Flying Gantry

This type of printer has a stationary print bed, and has the gantry move in X, Y _and_ Z axes. The [Voron 2.4](https://vorondesign.com/voron2.4) is one of these, and so is [Sovol's variant](https://www.sovol3d.com/products/sovol-sv08-3d-printer). These machines tend to be high speed, but not specifically shooting for records. Because the gantry moves in all three directions, it effectively needs to carry all of the motors with it, which adds weight. It's really versatile, and the design allows for large build volumes, which I assume is why all of the ["Max" variants](https://www.sovol3d.com/products/sovol-sv08-max-3d-printer) end up using this strategy. 

### Delta Printers

This type of printer has a triangular prism frame, with a stationary, round, print bed. The toolhead is in the middle of the frame, supported and moved by three independent motor arms in X, Y and Z axes. I immediately associate this kind of printer with [FLSUN](https://flsun3d.com/), but there's also the [Doron Velta project](https://github.com/rogerlz/Doron-Velta) if you want to do your own thing. Not officially associated with Voron, but it seems to have the same ethos and aesthetics. These are high speed, low table-footprint machines, and use arms instead of belts for lateral movement. I'm sure this does something to their reliability and durability, but I'm not sure what.

### Weirdos

This is the "Other" section. They're either FDM printers that don't fit neatly into the other categories, or printers that are specialized and different enough that they deserve a special place in your mental map of 3D printers, or they're so experimental that they aren't famous yet. Expect to read the words "in principle" a lot.

#### 3D Pens

[Yes, yes](https://www.amazon.ca/GEEETECH-Printing-Charging-Compatibility-Filament/dp/B0FQ56DYRF/), very clever. There is an [open source project for a decent one though](https://github.com/3dsimo/3dsimo_kit). 

#### Inverse Cantilevers

The [Lemontron](https://lemontron.com/) open-source DIY project, and the [Positron](https://www.positron3d.com/) are the only examples I'm aware of here. It's like a bedslinger cantilever, except the _bed_ is the thing on the support arm which moves along the Z axis and the toolhead moves along the X and Y axes. This means

1. prints come out upside down, the same way that resin printers do it
2. you need to use resin-style anchor supports
3. first-layer adhesion is _even more_ critical than usual
4. because the cantilever is only supporting the build plate, these are pretty easy to fold up when not in use
5. all the heavy components of this printer are concentrated in the base, which means you can use surprisingly small motors to make the entire thing work.

That last one is the key advantage; because of it, these machines tend to have more print volume than you'd expect given their size, and they can be portable in a way that most other FDM printers can't.

#### SCARA printer arms

These are planar arms like the [MPSCARA](https://www.thingiverse.com/thing:2487048) and [X-SCARA](https://github.com/madl3x/x-scara?utm_source=chatgpt.com) projects. They're not full robot arms in my mind because they don't have arbitrary freedom of movement. They're more like "CoreXY with a really complicated gantry", but they're asymmetrical in construction and in principle you could set a couple of them up in overlapping fashion to make an improvised IDEX. I don't know why you _would_. But you _could_.

#### Full DOF Robot Arms

These have more degrees of freedom than SCARA arms, and are generally more versatile. If you've seen houses getting 3D printed, you've probably seen one of [these](https://www.vertico.com/technology/robot-on-track) doing it, which is basically a robot arm holding a concrete toolhead and bolted to some rails. The filament variants boil down to "a 3D pen on an arm", and they have some [interesting abilities](https://www.youtube.com/shorts/SWLxoIhKD-8). Together with a non-planar slicer, one of these can in principle print things larger than itself. As far as I know, there's like one ["3D printer arm" you can buy](https://ca.robotshop.com/products/rotrics-dexarm-maker-edition-all-in-one-robotic-arm) and the cost is absolutely not justified unless you happen to have the money lying around and just like printer arms. But if you want to build one check out [this](https://www.thingiverse.com/thing:1693444), [this](https://www.thingiverse.com/thing:6313449) and/or [this](https://shop.elephantrobotics.com/en-ca/products/mycobot-worlds-smallest-and-lightest-six-axis-collaborative-robot?variant=44597630402872).

#### Polar printers

Also known as ["4 Axis printers"](https://www.youtube.com/watch?v=VEgwnhLHy3g) or ["spinning printers"](https://www.instructables.com/Make-a-Polar-3D-Printer-Spinning-Bed/). Technically there's a few subtypes, in the sense that some of them spin the print bed, some of them tilt the toolhead, some of them tilt the print bed, and some of them do spin the bed _and_ tilt the toolhead. The complications here are more complex slicers, more complex construction and more demanding belt placement and motor coordination. The payoff is more degrees of freedom on the print and, in principle, no support structures or post processing on the parts you output.

#### Frameless Printers

Basically [HangPrinter](https://hangprinter.org/?utm_source=chatgpt.com) and descendants. These seem to boil down to "a Delta printer, but with cords instead of arms and an arbitrarily large frame". I think in principle you could make this work with drones of certain size using their propeller output for cooling, but I haven't seen anyone explore the idea. The trade here is huge print volume, to the point that this is one of the approaches capable of architectural printing. I've seen exactly zero of these in the wild, but it'd be cool to see one spit out a three story house and, in principle, it absolutely could.
