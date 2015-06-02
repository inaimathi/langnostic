I know I posted earlier, but this has to come out of my head. And I don't think I'll have time to do much more writing this week, despite the fact that there are at *least* three other things on my mind that I need to idea peel.

So...

![](/static/img/square-zones.png)

That's what I'm thinking.

And here's an attempt at forcing this primitive tool called "English" to explain what it means in my head.

First off, [this](http://langnostic.blogspot.ca/2013/04/hardware-and-squares.html) is what I was thinking [previously](http://langnostic.blogspot.ca/2013/04/conduits-and-more-squares.html). Which is to say, not much in particular; just working my way up to meaningfully tearing into the problem. The basic problem is that there are lots of ways to [dissect a square into smaller, integer-sized squares](http://oeis.org/A045846), and brute-forcing it isn't going to be viable for very high `n`s. In fact, from what I've observed, it took multiple days of compute time to solve for 16, and that's not encouraging.

Thing is, there are lots of little ways to cut out a *lot* of the brute-force work required for the calculation. An approach that starts from the universe of possibilities and filters isn't going to get very far, but we can constrain that universe pretty significantly if we pick our model carefully, and I think I have.

## <a name="characteristics-of-the-optimal-dissection-model"></a>Characteristics of the Optimal Dissection Model

### <a name="cut-at-the-base"></a>Cut at the base

A huge contribution to the final tally of work is going to be figuring out where `1x1` squares can fit. If you look at each of them as a `Place`ment, you'll be overwhelmed pretty quickly. An easy way out of that is picking a model that lets you ignore 1x1 squares, and the easiest obvious way to do *that* is using a sparse array with the understanding that any unrecorded placements actually represent 1x1 squares. This lets you ignore a bunch of `StartingPoint`s too (more on that in a moment).

### <a name="take-symmetry-into-account"></a>Take symmetry into account

A second huge contribution to the bottom line of work for this problem is that you need to recompute a lot of dissections which you must then de-duplicate later. I don't think we can lick this one entirely; we'll still need to do `Set` insertions at some point in the process just to make sure we're not counting anything twice. *But*. We can cut out a lot of them. Specifically, since the problem already mentions rotations and reflections, we don't need to `Place` any squares that we *know* will be picked up by some other permutations' reflections or rotations. That can greatly reduce the number potential starting nodes these placement trees. It also means we'll want a representation where `rotate` and `reflect` are really, *really* fast operations. The sparse array approach seems to be better than using an explicit grid, but I'm not quite convinced it's optimal.

### <a name="bring-it-all-together"></a>Bring it all together

So what we've got as a 10000-foot-view for the process is this:


1.   find all `StartingPoint`s
1.   `unfold` each starting point, starting with squares of size `nxn` and working your way down to `2x2`
1.   `record` the null dissection (a grid full of 1x1 squares)
1.   return the size of the set of recorded dissections

-   A `StartingPoint` is any point on our grid from `'(0 0)` to ``(,(limit n) ,(limit n))`.
-   The `limit` of `n` is `(- (ceiling (/ n 2)) 1)`
-   To `unfold` a `StartingPoint`, `insert` the solitary placement as a dissection (remember, any empty spaces are treated as "placed 1x1 squares"), then find all `free` `SecondPoint`s and `unfold` them.
-   A `SecondPoint` is any `free` `StartingPoint`, or any point between ``(,(limit n) ,y)` and ``(,(- n 2) ,y)`. Again, any others should be represented among `reflect`ions/`rotate`ions of other dissections.
-   To `unfold` a `SecondPoint`, `insert` it as a dissection, then `unfold` all remaining `free` points on the grid (there's probably a way to cut this step down, but I can't see it yet).
-   To `unfold` any other point, `insert` it as a dissection, and `unfold` all remaining `free` points until there's no more room.
-   To `insert` a dissection, perform `Set` insertion on each of its `rotate`ions and `reflect`ions into the set of all insertions for this particular grid.
-   A `free` point is one where there's enough room to put a square larger than 1x1 *(this implies that `place` could probably remove some additional squares from the potential starting pool to make calculating this easier)*.


Granted, that's easier said than done. I get the feeling that when I actually go to implement this fully, a whole bunch of problems are going to crop up, but at least I have a half-way decent starting point.
