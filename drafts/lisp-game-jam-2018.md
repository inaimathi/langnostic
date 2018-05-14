Holy fuck, I guess I'm doing this shit.

[This](https://inaimathi.itch.io/clobble) is a client-side implementation of Boggle implemented entirely using [`cl-notebook`](https://github.com/Inaimathi/cl-notebook). It's called `Clobble`, becasue honestly, why wouldn't it be? I used the official die distribution cribbed from [this SE question](https://boardgames.stackexchange.com/questions/29264/boggle-what-is-the-dice-configuration-for-boggle-in-various-languages) and the challenge-mode scoring rules from [this WikiHow post](https://www.wikihow.com/Play-Boggle#/Image:Play-Boggle-Step-22.jpg)[^that-challenge-block]. Also, I used a static dictionary from [this project](https://github.com/insightcoder/boggle-dictionary), released under an MIT license.

[^that-challenge-block]: That `Qu` block was a bitch to implement, by the way. I haven't generalized it completely, but it alone accounted for an extra half-hour or so of coding.

Why Boggle? Er... Clobble? Because it's dead simple, so I could focus on getting something minimal together quickly and leave plenty of time for polishing up issues that come up in [`cl-notebook`](https://github.com/Inaimathi/cl-notebook) itself. In all honesty, I was hoping to get much further than I did. I had plans for alternate game modes, leaderboards and a player-specific progression system, in addition to better visuals, but all that got kicked in the head when I realized how little time I'd _actually_ have this week. So what I ended up putting together was the dumbest thing that would work. There's two modes; single-level and zen (which just gives you infinite levels until you give up and hit the `Quit` button).

The challenges were many, and the [`cl-notebook` issues list](https://github.com/inaimathi/cl-notebook/issues) has already grown somewhat as a result of this experience. So let me do a post-mortem brain dump.

## `cl-notebook` needs to deal with external files

So the dictionary itself is contained in a ~6MB external `js` file downloadable from the [itch page](https://inaimathi.itch.io/clobble) that declares a global variable with the appropriate `object`[^could-have-cut-that]. I could have cut that down to ~1MB by dropping the definitions and just keeping a word list, since the current implementatoin only really checks for the _presence_ of a word in the dictionary, but I had some plans that might have involved definitions. I ended up hacking around this limitation, as described later in this article, but it tells me that `cl-notebook` should _probably_ deal with static file bundles somehow.

Exactly _how_ opens up a bunch of worm cans. Or rather, at least one, my choice.

Simply adding a cell-type to pull in local dependencies gives project exporters something to work with. It means that you can reasonably something like an `HTML`, or specific `itch.io` exporter, to slurp up local files and do something sensible with them for deployment purposes. The downside of this approach is that notebook files are suddenly not self-contained. If you want to send a notebook file to someone else, you now need to make sure to also send the local static files and expect the target user to unpack them properly[^you-could-also-depend-on-importers].

[^you-could-also-depend-on-importers]: Alternately, we could define a general serialized form for book files, then define importers. This would let you send a notebook file by exporting it to said serialized form, and let some consumer use it by importing it appropriately. I'm not entirely sure how I feel about this.

Slurping local files in and effectively "storing" them as cell results lets exporters work, _and_ keeps notebooks self-contained, but is kind of a disk hog. In particular, pulling this trick with a `6MB` dictionary file would mean that said dictionary would be copied in book history how many ever times the appropriate cell was re-evaluated. This seems like something we might be able to get around by making the evaluation step of a book more intelligent about repeated values, but I'm not sure how effective that would ultimately be at the end of the day. There's also the possibility of slurping in files that mess with a book files' appropriate encoding, and which might therefore get us into trouble when we go to read the damn thing off disk later.

Finally, defining functions that do the appropriate thing in context without messing with the `book` format or `exporter` code directly is marginally plausible, but seems _extra_ hacky. In the `Clobble` situation, I'd have to define a `parenscript` function like `inline-script` or something that slurped a local file, and dumped it into the evaluation result. This would have the same problem as slurping files directly at the cell level, but doesn't define additional cell-level machinery. There's the additional downside that users need to be taught about this specifically, rather than being able to discover the functionality by exploring the `cl-notebook` UI.

I'm not yet sold on the concept, but it'd be nice to have some sort of solution, given what I'm expecting to do with this project.

## itch.io has some odd behavior for external files in web games

Feeding the external file debate in my head is the fact that external files aren't exactly reliable on `itch`.

This might have been a problem with the loading process of the external dictionary file, or it might have been an uploading error, or it might be a bug on the server end. Whatever the case is, the initial upload of the `words.js` file ended up being shown on my front-end as an HTTP `400` error. Since I wanted to actually get this thing running for the contest, I ended up just `cat`ting the dictionary into the appropriate place in my games' `index.html` file after exporting and calling it a day. The real solution is probably to externalize it. Or, possibly, write a `words` API endpoint that I keep elsewhere so that the initial page load doesn't have to involve a `6MB` download[^this-opens-the-debate].

[^this-opens-the-debate]: To be fair, this opens the debate about whether requiring internet connectivity to play the game is fair. If I made `word?` an API call instead of a local dictionary comparison, you couldn't just load this game into a browser on whatever device and then go play it offline. How common that use-case would be is not _entirely_ clear to me, but it also seems like the sort of thing I shouldn't arbitrarily disallow if it can be avoided.

## The `Qu` die face introduces a bunch of special cases

As far as crafting the actual game itself goes, the main non-dictionary-related challenge I ran into was the stupid `Qu` challenge-mode die face. Having a particular face with multiple letters required a bunch of its own special-case in the games' global `keydown` event and some display logic. Firstly, it's not really good enough to allow typing `Q` to highlight `Qu`, because that behavior gets weird if you _also_ have a `U` present in the same game. What you really want is for `Q` to put the game into a different state that expects to consume an additional `U` keypress in order to highlight `Qu`. Which means that we want the additional `semi-selected` state for letter faces. This consequently means that a `U` keypress needs to trip a check for a `semi-selected` `Qu` square and do something different if one is found. The `clear` and `claim` actions also each have to consider `semi-selected` squares in addition to `selected` ones, and appropriately reset them to the ground state.

I _didn't_ end up fully generalizing this code, so it only deals with `Qu` squares, and not multi-letter squares, but I called it Good Enough For Now.

## Intervals and Timeouts

A common thing you want to do in stupid HTML games is [set `timeout`s and `interval`s](https://www.w3schools.com/js/js_timing.asp). The trouble is that this runs up against `cl-notebook`s display model. Javascript default delay behavior isn't to override existing delays, but to merely declare new ones. Which means that doing iterative development on a cell that calls `setInterval` directly runs into some odd edge cases. To fight this, I added a few new functions to [`base.lisp`](https://github.com/inaimathi/cl-notebook/blob/master/src/ui/http-front-end/base.lisp). Specifically, we've got `interval!`, `timeout!`, `clear-delay!` and `clear-all-delays!`, all of which deal with _named_ delays that automatically destroy previous versions of themselves rather than naively defining separate intervals/timeouts. This is one little corner that I managed to sand out fairly satisfactorily.

## `dom-ready`

There's a convenience function in [`base.lisp`](https://github.com/inaimathi/cl-notebook/blob/master/src/ui/http-front-end/base.lisp) named `dom-ready`, which does something when the HTML DOM is loaded. This lets you set up a mechanism to run one-off initialization hooks for your web thing that behave correctly in the exported HTML.

However.

This has the unsatisfactory behavior that opening the relevant book in `cl-notebook` for the first time gives you some odd errors. In particular, if you've got a `dom-ready` call set up that targets a game screen defined in one of your notebooks' cells, which seems like a reasonable thing to do, you'll get `element-not-found` errors when loading the notebook. This is because `dom-ready` works with the browsers' default `DOMContentLoaded` event.

The way I ended up solving this tension was to define a `book-ready` function that lets you set hooks that run after the `notebook` is fully loaded, rather than merely at DOM loaded time. I'm not _entirely_ sure this was the right decision. The only reasonable alternative I can think of is to have `dom-ready` have the behavior of tripping hooks at `notebook`-ready time by default, and leave out the additional construct. I'm not sure if there's a situation where I might legitimately want to differentiate between the two, so I'm leaving it in for now, but I might mildly regret this later.

## UI Inconveniences

The biggest remaining annoyance in using `cl-notebook` for game development remains the detail of UI definition. Specifically, at the moment, the UI cell is treated as a regular cell, which is problematic for two reasons:

1. If you're working with another cell that affects your display somehow, you suddenly need to scroll between the target cell and the display cell fairly frequently during the development process
2. If your display cell does things with `position: absolute` or similar, you might get into the sitation that adjacent cells occlude part of your display

In order to mitigate #2, we could just say that the `focused` cell has a much higher `z-order` than `unfocused` cells. Woo. That doesn't really solve #1 though. What I've got in mind that might kill both issues with the same stone is adding a different kind of cell. Specifically, maybe it should be possible for the user to designate a cell as "floating". It would be off to the side with a `position: fixed` behavior so that you'd never need to scroll back up to it, and it could act as a canvas for presentation displays. This would make it easier to use `cl-notebook` for giving demos, _and_ it would deal with both of the above pain points.

## `itch.io` exporter

I don't have it quite finished yet, because I'm still thinking about the structure `exporter`s should take in general, but the start of it is

```common-lisp
;;;;;;;;;; itch.io exporter
(defmethod filename-of ((format (eql :itch.io)) book)
  "index.html")

(defmethod mimetype-of ((format (eql :itch.io))) "text/html")
(defmethod export-as ((format (eql :itch.io)) (book notebook))
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:title (str (notebook-name book)))
      (:script :type "text/javascript" (str *base-js*)))
     (:body
      (:ul :class "cells"
           (str (format nil "~{~a~}" (export-cells format book))))))))

(defmethod export-cell ((format (eql :itch.io)) cell-language (cell-type (eql :markup)) cell)
  (html-to-str
   (-cell-comment cell)
   (str (-html-value cell))))
(defmethod export-cell ((format (eql :itch.io)) cell-language (cell-type (eql :parenscript)) cell)
  (html-to-str
   (-cell-comment cell)
   (:script :type "text/javascript" (str (-html-value cell)))))
```

And that's exactly what I used to generate the index file you can see played at [the `itch` page](https://inaimathi.itch.io/clobble)

## Next Steps for `Clobble`

As next steps, I kinda want to polish, and add the features I wanted there to begin with. Both in the sense of polishing `cl-notebook` into a better web-game development tool, and in the sense of polishing `Clobble` into a game that I wouldn't be ashamed to charge for. The game features I'm interested in adding are mainly different play languages, a scripting mode, and possibly some kind of level/progression system.

I have no idea how long this is going to take, given what my schedule looks like these days, but as always, I'll keep you posted.
