I was going to say "this'll just be a quick update", but on reflection, I'm not sure that's true. I want to discuss two specific things I recently implemented, as well as what's been going on lately project-wise.

First things first.

### <a name="aosa-chapter"></a>AOSA Chapter

I'm doing a chapter for the upcoming book called *[500 Lines or Less](https://github.com/aosabook/500lines)*. It's the fourth installment in the [Architecture of Open Source Applications series](http://aosabook.org/en/index.html). To, I should hope, no ones' surprise, I'm doing a write-up of the [House](https://github.com/Inaimathi/house) asynchronous server currently serving as the back-end for the [Deal](https://github.com/Inaimathi/deal) project. I've seen fit to simplify the server a bit for entry, since the real live one masses slightly more than 500 lines. You can see the result [here](https://github.com/Inaimathi/500lines/tree/master/async-web-server); that's the House server with the session and static file mechanisms stripped out, which happens to weigh in at just over 400 lines of fairly readable Common Lisp. The first 1/2 draft of the prose write-up is [here](https://github.com/Inaimathi/500lines/blob/master/async-web-server/writeup.md).

This process is new to me. Granted, I write a fuck-ton, but I'm usually also the editor and proofreader. Frankly, I don't envy anyone that job. It may not look like it, but I end up chopping a good half of my output before it gets posted anywhere, and then tweak about a third of the result after the fact. Hopefully, involving other eyes will mean a more refined exposition than I usually manage.

### <a name="dmacs"></a>2dmacs

I'm still working on that visual editor at work. Internally, we've taken to calling the project "`2dmacs`", pronounced "two-dee-macs". Hopefully, I can live up to the name. We're going to do some user trials tomorrow, just to see what the target audience of 5 thinks. It's still missing one or two big features, mostly history related and mostly pretty straight-forward to implement. But based on my own experience, it's already more productive than the alternatives for the sorts of things we'll be doing. I'll let you know how it goes.

Now then, moving on to a couple of implementation details. Except that I still can't show you code. Sorry.

### <a name="quasimodes"></a>Quasimodes

Quasimodes are another thing I picked up from [that Raskin book](http://www.amazon.com/The-Humane-Interface-Directions-Interactive/dp/0201379376)<a name="note-Tue-Jan-28-214701EST-2014"></a>[|1|](#foot-Tue-Jan-28-214701EST-2014). The idea is to let holding a particular key kick off a different interaction mode and cancel out of it on release. Raskin's example is of the `Caps Lock` and `Shift` keys. Basically, `Caps Lock` switches the user into a mode where standard keyboard keys do something different, whereas `Shift` activates a quasimode to the same ends. The argument is that the second is better because there's no confusion about what state the system is currently in. If you're holding shift, you're in shout mode. If you're not, you're not. Comparatively, as long as your keyboard has a Caps Lock key, you might be dumped into Shout mode by an accidental keypress. If you've ever tried to type in a password, you know the problems this can cause. Quasimodes neatly sidestep the issue by keeping a mode active only as long as the user deliberately holds a particular key.

Implementing this in Javascript turns out to be non-trivial. Even after you've built up your own little event system around the various input actions a user can take. What you naively need to do is capture the `keydown` event, activate a given quasimode<a name="note-Tue-Jan-28-214704EST-2014"></a>[|2|](#foot-Tue-Jan-28-214704EST-2014), and clean up when you get a `keyup` corresponding to the initiating `keydown`.

That has some implications, though.

First, it requires the ability to *optionally* route events through particular functions. Which means that you have to have a layer of indirection between the default HTML event handlers and your systems' commands. Luckily, the system I'm building had that already for unrelated reasons, but if you're trying to do this properly in *your* system, it might mean some architectural changes.

Second, because we're dealing with browser events, we need to be able to cancel out of a quasimode with something other than the initiating keydown event<a name="note-Tue-Jan-28-214707EST-2014"></a>[|3|](#foot-Tue-Jan-28-214707EST-2014). In my case, I had already wired `&lt;esc>` as the generic cancel button. It already drops you out of half-completed keystrokes and resets some small pieces of internal state to their original positions. So it made perfect sense to just add "exit any quasimodes" to the list of things it does.

Finally, unless you want to let quasimodes monopolize the keys they'll be bound to, it means having some logic involved that distinguishes between a regular keypress and a quasimode invocation. That ended up being resolved a touch hackily; if you press a quasimode key, but don't trip any of that modes' internal bindings before releasing it, it gets interpreted as a vanilla keypress. Not sure that's the best approach long-term, but I can't think of a simpler one off the top of my head.

### <a name="selections"></a>Selections

A particular type of quasimode that gave me some pause is area selection. It turns out this is more complicated than you'd think at first glance. If you have a system that models various things as sets of points, and points as well as things are selectable and groupable, then there are a bunch of independent axes that you might be thinking about selections:

**additive/subtractive/replacing** You might reasonably be looking to add to your existing selection, remove elements from it, or replace your existing selection with the set of elements you're about to specify by area.

**intersecting/containing** You might want to select any thing whose bounding box *touches* your selection, or you might want to limit yourself to objects that are entirely *contained* inside said selection.

**element/point** You might want to select things, or you might want to select the points that compose them.

**top-level/deep** You might want to select the top-level groups of elements, or you might want to select the leaf nodes.

The point here is that these are all axes on which the user might choose independently, and most of them are reasonable choices. However, you don't want to have 24 different quasimodes, or even one quasimode with 23 different modifier keys to accomodate them all. After giving it some thought, I made the drag-select interaction default to an **additive, intersecting, element, top-level** selection. My intuition is that **point** and **subtractive** selections are going to be rare and simple enough that we won't need area support for them, that **replacing** selections are easy enough to counterfeit by hitting the `clear-selection` keystroke before making a selection, and that I can add modifier keys to distinguish between **intersecting/containing** and **top-level/deep** if needed. I'll let you know how that goes too.

* * *
##### Footnotes

1 - <a name="foot-Tue-Jan-28-214701EST-2014"></a>[|back|](#note-Tue-Jan-28-214701EST-2014) - Which I thoroughly recommend for anyone involved in interface design of any kind. I'm kind of surprised they never made me read it in college, alongside [Design of Everyday Things](http://www.amazon.com/The-Design-Everyday-Things-Expanded/dp/0465050654).

2 - <a name="foot-Tue-Jan-28-214704EST-2014"></a>[|back|](#note-Tue-Jan-28-214704EST-2014) - Which means running its bindings instead of the global ones for the duration.

3 - <a name="foot-Tue-Jan-28-214707EST-2014"></a>[|back|](#note-Tue-Jan-28-214707EST-2014) - An alert, or forced focus switch might take the user away from our window before we get the intended key released.
