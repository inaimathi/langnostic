Dragging SVG elements is harder than it might appear at first glance.

The underlying implementation of the SVG DOM and the HTML DOM is different in current browsers you see, so the standard HTML5 drag event doesn't apply to SVG nodes. Luckily, `mousedown`, `mousemove` and `mouseup` *are* supported, so you'd think it would be a straight-forward task to implement the fucker yourself. You probably imagine, as I did initially, something that takes a selector and a list of callbacks, and implements something similar to jQuery's `.draggable()` in ~30 lines of code by


-   binding a callback to the targets' `mousedown`, `mousemove`, `mouseup` events
-   storing the initial position of the element, and its delta from the mouse cursor
-   preventing default on `mousedown`
-   in addition to firing the callback, moving the target element by manipulating `x` and `y` coordinates using the current mouse position, initial delta and initial position


Maybe that's possible for the simple cases, but it's the edges that'll get you. And unless an implementation happens to dull all *your* edges, it's not really good enough.

**Minor speed-bump.** What if you need to drag multiple elements? If you were dealing with HTML elements, it wouldn't be such a big deal, but SVG elements have different properties to represent their coordinates. Rectangles and similar have an `x` and `y` that represents their top-left corner, circles and ellipses have `cx` and `cy` that represents their center, text elements have an `x` and `y`, but theirs represents the *bottom*-left coordinate, and I'm not even getting into the path elements. Bottom line, if you want to implement something that works, you're using `transform` settings. Also, you're not doing it naively through `setAttribute`, unless you're lucky enough to have a situation where you can guarantee that no other transformations will be applied to any draggable element<a name="note-Sat-Nov-30-122522EST-2013"></a>[|1|](#foot-Sat-Nov-30-122522EST-2013). The snippet that handles that particular piece looks like this in my codebase

```lisp
(defpsmacro =translate (elem/s dx dy)
  `(try 
    (=>> ,elem transform base-val (get-item 0) (set-translate ,dx ,dy))
    (:catch (error)
      (=set-attribute ,elem :transform (+ "translate(" ,dx "," ,dy ")")))))
```

If you're one of the sad bastards who don't have macros at their disposal, I guess you're doing that or something fairly similar manually<a name="note-Sat-Nov-30-122530EST-2013"></a>[|2|](#foot-Sat-Nov-30-122530EST-2013). Like I said though, no big deal either way.

**Medium sized speedbump.** If you want to do this on an arbitrarily sized element, specifically a very small one, you'll discover that moving your cursor at even moderate speeds is enough to escape from the `mousemove` event and leave your draggable behind. One possible solution here is to also bind the `mouseleave` event and hope you never need to move fast enough to escape that too. Another approach is to have your chosen `mousedown` set up a *global* `mousemove` event, on `body` or `html`, use that to drag your element around, and have a global `mouseup` waiting to cut it off as soon as you're done<a name="note-Sat-Nov-30-122532EST-2013"></a>[|3|](#foot-Sat-Nov-30-122532EST-2013). A bit hacky, but doable.

**Slightly larger speedbump.** If you want to make these bindings switchable, you're in for a bit of a harder time. Not switchable as in "different objects should be able to do mildly  different things", that's a given. I mean like "it should be possible to jump into a separate interaction mode where the same object does something mildly or wildly different under certain circumstances". If you want that, you need a level of indirection in your listener tree that you can swap out with other functions, and that level of indirection is going to be calling an externally specified function on each event trigger. Basically, you'll want to be working with hooks rather than listeners at this point<a name="note-Sat-Nov-30-122536EST-2013"></a>[|4|](#foot-Sat-Nov-30-122536EST-2013). I'll keep you posted on how this one goes in real life.

**Large speedbump.** Suppose you want to be able to use your dragging events, *and* a `mouseup` event on the same element. Better yet, suppose you wanted to implement `drag`/`mousedown` interactions, but let the user decide what layer to apply them on at any given time. Imagine a situation where you had the elements `foo`, overlapping `bar`, overlapping `baz`, and when a `drag` or `mousedown` hits, you want to let the user decide whether they want to be `click`/`drag`ging `foo` and/or `bar` and/or `baz`. Near I can tell, there is no way of implementing this elegantly in terms of listeners on individual elements. What you need if you want this is a central listener that delegates particular events out to some intermediary functions, or eats them<a name="note-Sat-Nov-30-122539EST-2013"></a>[|5|](#foot-Sat-Nov-30-122539EST-2013) as appropriate.

Keep in mind that the last two speedbumps I hit here probably won't be felt by most people going in the same direction. Still, I went into this figuring it'd take me a half hour at the outside to implement something workable. It ended up taking me the rest of the day, and will probably cost me another hour or two when I get back in on Monday.

Such is development sometimes, I suppose.

* * *
##### Footnotes

1 - <a name="foot-Sat-Nov-30-122522EST-2013"></a>[|back|](#note-Sat-Nov-30-122522EST-2013) - If you *are* going to have other active transformations, using the `setAttribute` method would overwrite those, which is why it's a bad idea.

2 - <a name="foot-Sat-Nov-30-122530EST-2013"></a>[|back|](#note-Sat-Nov-30-122530EST-2013) - if you *are* doing that, I should point out that the only reason I went the `try`/`catch` route here is that both `=>>` and `=set-attribute` take either an element or a set of elements as their first argument, and I wanted `=translate` to do the same. Since you probably won't have the same situation, you're likely better off with `if`/`else`.

3 - <a name="foot-Sat-Nov-30-122532EST-2013"></a>[|back|](#note-Sat-Nov-30-122532EST-2013) - You wouldn't want to do this naively either, unless you knew there'd be no other `mousemove` events on that top-level element. If you *did* have that, you'd want to set up a hook that you could change out rather than messing with event listeners every time you dragged something.

4 - <a name="foot-Sat-Nov-30-122536EST-2013"></a>[|back|](#note-Sat-Nov-30-122536EST-2013) - It just occurred to me that you might have no idea what I mean by "hook" in this context. Basically, something like this:

```javascript
var mouseMoveHook = null;
document.querySelector("body").addEventListener("mousemove",
  function (event){
    //do other stuff
    if (mouseMoveHook) { mouseMoveHook(event) };
  });
```

If you have something that looks like that, you can change some of the behavior of your global `mousemove` event by assigning a new callback to the `mouseMoveHook` variable. I'm sure it's been used elsewhere, but I learned the term "hook" from Emacs, which provides standard event hooks in a bunch of different situations, and does it more or less this way, modulo some syntactic sugar.

5 - <a name="foot-Sat-Nov-30-122539EST-2013"></a>[|back|](#note-Sat-Nov-30-122539EST-2013) - In the case of the trailing `mouseup` event after a drag concludes.
