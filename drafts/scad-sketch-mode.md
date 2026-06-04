So I've been doing a bunch of 3D printing, and therefore 3D modeling. Because I'm the sort of nerd that uses [`emacs`](https://www.gnu.org/software/emacs/), I use [`openscad`](https://openscad.org/) instead of something like [`FreeCAD`](https://www.freecad.org/) or [`blender`](https://www.blender.org/) [^theres-another-reason-but]. Which is actually really great.

[^theres-another-reason-but]: There's another reason but we don't have to touch on it here. The short version is something like "general purpose LLMs are probably going to dominate in terms of resources and/or performance, so in the short to medium term, things that have a plaintext or HTTP interface have an advantage over things that don't". But the _big_ reason is that I like Emacs, so I'm biased towards using it.

You can do simple things like

```
cube([10, 20, 30]);
```

or more complicated things like

```
module roundedCube(size, r=1, $fn=64) {
  minkowski() {
    translate([r, r, r]) cube([size[0]-(r*2), size[1]-(r*2), size[2]-(r*2)]);
    sphere(r=r);
  }
}
roundedCube([10, 20, 30], r=5);
```

or highly complicated things like

```
// Simplified wave function for ribbed circles
function wave_radius(angle, rib_count, rib_depth, base_radius) =
    base_radius + sin(angle * rib_count) * rib_depth;

// Create a ribbed circle profile using polygon
module ribbed_circle(diameter, rib_depth) {
    radius = diameter / 2;
    // Auto-calculate optimal rib count based on diameter
    rib_count = max(6, round(diameter * 0.8)); // ~0.8 ribs per mm diameter

    // Create points around the circle with wavy radius
    $fn = max(32, rib_count * 4); // Ensure smooth curves
    points = [
        for (i = [0:360/$fn:359])
            let(r = wave_radius(i, rib_count, rib_depth, radius))
            [r * cos(i), r * sin(i)]
    ];

    polygon(points);
}

// Helper function to get point on rectangle perimeter
function rect_perimeter_point(t, width, height) =
    let(
        perimeter = 2 * (width + height),
        pos = t * perimeter
    )
    // Bottom side: 0 to width
    pos <= width ? [pos, 0] :
    // Right side: width to width+height
    pos <= width + height ? [width, pos - width] :
    // Top side: width+height to 2*width+height
    pos <= 2*width + height ? [2*width + height - pos, height] :
    // Left side: 2*width+height to perimeter
    [0, 2*width + 2*height - pos];

// Helper function to get outward normal at point on rectangle perimeter
function rect_perimeter_normal(t, width, height) =
    let(
        perimeter = 2 * (width + height),
        pos = t * perimeter
    )
    // Bottom side: normal points down (outward)
    pos <= width ? [0, -1] :
    // Right side: normal points right (outward)
    pos <= width + height ? [1, 0] :
    // Top side: normal points up (outward)
    pos <= 2*width + height ? [0, 1] :
    // Left side: normal points left (outward)
    [-1, 0];

module ribbed_rectangle(width, height, rib_depth) {
    perimeter = 2 * (width + height);
    rib_count = max(6, round(perimeter * 0.4)); // ~0.4 ribs per mm perimeter

    $fn = max(32, rib_count * 4);

    points = [
        for (i = [0:$fn-1])
            let(
                // Parameter from 0 to 1 around perimeter
                t = i / $fn,
                // Get base point on rectangle perimeter
                base_point = rect_perimeter_point(t, width, height),
                // Get outward normal direction
                normal = rect_perimeter_normal(t, width, height),
                // Calculate rib displacement using wave function
                wave_angle = t * 360 * rib_count,
                displacement = sin(wave_angle) * rib_depth,
                // Apply displacement along normal (inward for magnet hole)
                final_point = base_point - normal * displacement
            )
            final_point
    ];

    polygon(points);
}

module magnetHole(size, tolerance=0.3, chamfer_height=1.5, channel_height=0) {
  // Check if size is 2D (circular) or 3D (rectangular)
  is_circular = len(size) == 2;

  if (is_circular) {
    // Circular magnet: size = [d, h]
    d = size[0];
    h = size[1];

    // Auto-calculate optimal rib depth
    rib_depth = d * 0.025;

    translate([0, 0, -tolerance/2])
      union() {
        // Main ribbed cylinder hole
        linear_extrude(height=h+tolerance)
          ribbed_circle(d+tolerance, rib_depth);

        // Chamfer at top for easier magnet insertion
        translate([0,0,h-tolerance*2])
          cylinder(d1=d+tolerance, d2=d+tolerance+2*chamfer_height, h=chamfer_height);

	translate([0,0,h-tolerance*2+(chamfer_height-0.05)])
	  cylinder(d=d+tolerance+2*chamfer_height, h=channel_height);
      }
  } else {
    // Rectangular magnet: size = [l, w, h]
    l = size[0];
    w = size[1];
    h = size[2];

    // Auto-calculate optimal rib depth based on smaller dimension
    rib_depth = min(l, w) * 0.025;

    translate([0, 0, -tolerance/2])
      union() {
        // Main ribbed rectangular hole
        linear_extrude(height=h+tolerance)
          translate([-(l+tolerance)/2, -(w+tolerance)/2])
            ribbed_rectangle(l+tolerance, w+tolerance, rib_depth);

        // Rectangular chamfer at top for easier magnet insertion
	translate([0,0,h-tolerance*2])
          linear_extrude(height=chamfer_height, scale=[(l+tolerance+2*chamfer_height)/(l+tolerance), (w+tolerance+2*chamfer_height)/(w+tolerance)])
	  translate([-(l+tolerance)/2, -(w+tolerance)/2])
	  square([l+tolerance, w+tolerance]);

	translate([-(l+tolerance)/2,-(w+tolerance)/2,h+chamfer_height-0.1])
	  cube([l+tolerance, w+tolerance, channel_height]);
      }
  }
}

module magnetCalibrationCube(size, mS, mT=0.3, cH=0) {
  l = size[0];
  w = size[1];
  h = size[2];

  mH = mS[len(mS)-1];

  difference() {
    cube(size);

    translate([l/2, w/2, h-mH]) magnetHole(mS, tolerance=mT, channel_height=cH);
    translate([1, 1, h-1]) linear_extrude(3) text("Z" , font="DejaVu Sans", size=3);

    translate([0, 0, h]) rotate([0, 90, 0]) {
      translate([l/2, w/2, h-mH]) magnetHole(mS, tolerance=mT, channel_height=cH);
      translate([1, 1, h-1]) linear_extrude(3) text("X" , font="DejaVu Sans", size=3);
    }

    translate([0, w, 0]) rotate([90, 0, 0]) {
      translate([l/2, w/2, h-mH]) magnetHole(mS, tolerance=mT, channel_height=cH);
      translate([1, 1, h-1]) linear_extrude(3) text("Y" , font="DejaVu Sans", size=3);
    }
  }
}

// magnetCalibrationCube([30, 30, 30], [6, 3]);
magnetCalibrationCube([20, 20, 20], [2, 4, 5], cH=10);
```

In principle, if you have a physical object in your mind, you can express it in OpenSCAD using its [primitives, methods of composition and methods of abstraction](https://openscad.org/cheatsheet/). In practice, if you want to do anything even remotely smooth or organic-ish of any complexity, you need to reach for complex `polygon` arrays and possibly pull in something like [`RoundAnything`](https://learn.cadhub.xyz/docs/round-anything/api-reference/). Maybe _you_ can do this in your head, but I personally can't meaningfully visualize or internally transform things like

```
...
step = [[0, 10, 5],
	[0, 30, 5],
	[20, 40, 15],
	[60, 30, 10],
	[80, 40, 5],
	[100, 30, 5],
	[100, 10, 5],
	[80, 00, 5],
	[20, 0, 5]];

profile = [[100, 0, 3],
	   [100, 100, 3],
	   [0, 100, 10],
	   [0, 75, 3],
	   [30, 80, 50],
	   [80, 60, 100],];
...
```

I can't easily look at those things and visualize the corresponding lines or shapes. This is a problem if I want to create any physical item not derived from simple math. The one part of [`FreeCAD`](TODO) that I really like is the idea of a "Sketch" interface that lets you draw various 2D shapes that you can then `extrude`/`difference` or transform in various ways in order to get the 3D shapes out of your head and into your computer[^generally-as-a-brief]. So, in the grand tradition of Emacs users and Lispers in general, I'm going to steal it.

[^generally-as-a-brief]: Generally, as a brief layover on the way to the real world outside of your computer

## `scad-sketch-mode`

Here's the first cut of [`scad-sketch-mode`](https://github.com/inaimathi/scad-sketch-mode/). For now, you can clone it from that repo and then add

```
(require 'scad-sketch)
(add-hook 'scad-mode-hook #'scad-sketch-mode)
```

to your `.emacs` file. Then you'll be able to walk up to an array like the ones above and hit `C-c C-.` to get this:

![Image of the basic `scad-sketch` editor interface](/static/img/skad-sketch/editing.png)
![Video demo of `scad-sketch` editor interaction](/static/img/skad-sketch/editing.webm)

It's not point-and-click. This is an implementation of a visual editor that lets you navigate around with keyboard shortcuts, and it lets you edit 2D shapes. The interaction flow is 

1. go to a `polygon` array (or a place where you want one to be)
2. hit `C-c C-.` (if you imported everything properly, this should be an available binding in your `scad-mode`)
3. use the arrows, `i`, `p`, `m`, `k` and a bunch of other editor keys to add/remove points or move them around
4. when you're happy with the look of what you've got (and the accompanying preview array), hit `w` to write the array back to the origin region, and `q` to quit the sketch editor.

## The Byline

The first, kind of janky, cut of this editing mode was put together in concert with ChatGPT. It took around two hours to go from "I'm really annoyed by this particular part of OpenSCAD editing" to "Looks bad, and occasionally explodes, but most keys do what I want it to". Then I pulled in Claude to do refactoring. It took another hour and a half to go from that to "This is a serious but minimal editing mode for OpenSCAD polygons". It's _not_ a full substitute for FreeCAD sketch mode yet, because it _only_ handles polygons with curves.

Conceptually, I understood what I wanted, explained it to ChatGPT which made some suggestions[^such-as-using-svg], and then had it generate a pretty-close-to-fully-usable minor and editor mode. Then I took that output and ran it through Claude to make some generalizations, fix some bugs, and make certain things more stylistically in line with ways I like to write `elisp`. The _intent_ was for this to quickly get to a fully generalized 2D editing sketch editor for the emacs `scad-mode`, but this was unfortunately too complicated a task. It took me an hour or two to figure out that I was working at the edge of Claude 4.8s' competency in `elisp` libraries. Mildly disappointing, but I guess this is technically what I expected when I said it would take 6 months for models to replace programmers around a month ago. The _actually_ more disappointing fact is that in failing, Claude consumed all of my usage credits for the day, which means I had to regroup.

[^such-as-using-svg]: Such as using SVG rendering utilities that are appparently incluedd in stock Emacs as of version 27. This is a fact I didn't know. It probably would have occurred to me to check, but I didn't need to. I think that contribution on its' own saved me an hour or two of work minimum.

The next cut, still in progress, is moving everything into a ChatGPT session, then using it to surgically refactor the existing mode and add features as we go. As I type this, I think I'm around 4 hours deep and getting pretty close to shipping it. You'll see the results over at [my `github`](https://github.com/inaimathi/scad-sketch-mode/). Honestly, it's hard to know how to feel about apportioning productivity between me and the robots. On the one hand, my disappointment at Claude's failure to straight up write the expanded major mode is palpable; it felt real and more importantly, it felt like I had wasted some time in the implementation. This implies that there's [money left on the table](https://www.lesswrong.com/posts/rYq6joCrZ8m62m7ej/how-could-i-have-thought-that-faster), but I'm not sure this cashes out as a net negative for the technique. In particular, I strongly suspect that absent any AI involvement at all, this would have been a multi-day, possibly multi-week project, and I probably would have been happy with that level of progress. What's happening here is that there's certain fairly large chunks of the implementation process that are now beneath my level of abstraction.

I think I _still_ [endorse](https://inaimathi.ca/posts/ai-multipliers) my [previous views](https://inaimathi.ca/posts/dont-use-aidev-mode) on the status of programming as a pursuit absent of AI involvement.
