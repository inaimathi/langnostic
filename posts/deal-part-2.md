This is part two of the journal for the [Deal project](https://github.com/Inaimathi/deal). [Part one](http://langnostic.blogspot.ca/2013/08/deal-journal-part-one.html) can be found [here](http://langnostic.blogspot.ca/2013/08/deal-journal-part-one.html).

## <a name="day-" href="#day-"></a>Day 44

I've been pouring time into the thing, as you can see by taking a look at the code contribution graphs. I'm trying desperately to keep the JS as terse and understandable as possible, but I'm running into challenges all over the place. If I wasn't writing Lisp, I'd probably have given up by now. Which is not to say that its been *easy* in Lisp. Mistakes have been made and rolled back, rest assured, and I still need to take a second look at the SSE/chat system to make it as DRY as possible. I currently have a very similar structure repeating in three places, which means its time to factor that out.

Here are a couple of close-ups on things I already changed:

```lisp
(defpsmacro $keypress (target &rest key/body-pairs)
  `($ ,target
      (keypress
       (lambda (event)
         (let ((shift? (@ event shift-key))
               (alt? (@ event alt-key))
               (ctrl? (@ event ctrl-key))
               (meta? (@ event meta-key))
               (<ret> 13) (<esc> 27) (<space> 32) 
               (<up> 38) (<down> 40) (<left> 37) (<right> 39))
           (cond ,@(loop for (key body) on key/body-pairs by #'cddr
                      collect `((= (@ event which) ,(if (stringp key) `(chain ,key (char-code-at 0)) key)) ,body))))))))
```

That's a function for simplifying `$("foo").keypress()` use. It doesn't simplify the call itself much, but it handles looking up some common keycodes for me. This is the shittily performing version; if I really felt like it, I could have it substitute literals at macro-expansion time rather than doing a run-time let. Hm. In fact, Note To Self.

Next up:

```lisp
(defpsmacro $droppable (target (&key overlapping) &rest class/action-list)
  `($ ,target
      (droppable 
       (create 
        :drop (lambda (event ui)
                (let ((dropped (@ ui helper context))
                      (shift? (@ event shift-key)))
                  (cond ,@(loop for (class action) in class/action-list
                             collect `(($ dropped (has-class ,class)) ,action)))))
        ,@(when overlapping
                `(:over (fn ($ ,overlapping (droppable "disable")))
                  :out (fn ($ ,overlapping (droppable "enable")))))))))
```

`$droppable` got a bit of a face-lift due to the way overlapping jQuery droppables interact with one another. Specifically, they *all* trigger. If you don't want that, you have to write a couple of lines of code to disable potentially overlapping droppables so as not to trigger them. This is a "good-enough" solution, but I'm still wondering if [the HTML5 droppable interface](http://www.w3schools.com/html/html5_draganddrop.asp) handles this situation better.

```lisp
(defpsmacro $draggable (target (&key revert handle cancel) &body body)
  `($ ,target (draggable (create :stop (lambda (event ui) 
                                         (let ((shift? (@ event shift-key)))
                                           ,@body))
                                 ,@(when revert `(:revert ,revert))
                                 ,@(when handle `(:handle ,handle))
                                 ,@(when cancel `(:cancel ,cancel))))))
```

Finally, `$draggable` got the tiny addition of an injected binding of `shift?`, just because I use it in a few places. Really, I should probably be consistent with these and bind similar symbols for all the modifier keys. That's something I'll change for the versions of these functions that end up getting dropped into [cl-web-dev](https://github.com/Inaimathi/cl-web-dev)

The back-end isn't going to get any more complicated. In fact, now that I've gotten the chance to test drive [the result](http://deal.inaimathi.ca/static/index.html) a few times, I'm probably going to aggressively *cut* features from it. Every change made to [deal.lisp](https://github.com/Inaimathi/deal/blob/master/deal.lisp) since last time has been either a bug fix, a clearer and simpler way to say something, or a change to parameter bounds. Specifically, it came to my attention that rolling `4096` dice at once created a message big enough to choke the push-stream module, so I limited players to rolling only `128` dice at a time.

Tons of the features I wanted got put together in the last couple of days. The front-end is now good enough that I'm seriously considering starting in on the deck/board builder rather than adding more stuff. There's nice-to-haves, obviously, but nothing that'll outright prevent any game I can think of from being played.

## <a name="day-" href="#day-"></a>Day 45

Didn't get as much work done as I was hoping today. The lack of sleep is finally taking its toll, I think. Hopefully I can catch up on some rest this weekend, then move on to the deck/setup editor, then get to polishing the fuck out of all of it. I did a bit of that. Little things like


- the ability to shuffle
- changed the thought process behind stacks<a name="note-Sat-Sep-07-221201EDT-2013"></a>[|1|](#foot-Sat-Sep-07-221201EDT-2013)
- fixing some bugs that don't seem to have bitten yet, but would have in pretty short order


but nothing major.

One thing I wanted to point out explicitly, just to convince you that I'm serious about this "clarity" thing:

```
inaimathi@lambda:~$ cd projects/deal/
inaimathi@lambda:~/projects/deal$ wc -l *lisp model/*lisp deal-ui/*lisp *asd
  197 deal.lisp
  124 define-handler.lisp
   24 package.lisp
   32 start.lisp
   81 util.lisp
   68 model/server.lisp
  123 model/table.lisp
   96 deal-ui/css.lisp
  461 deal-ui/deal-ui.lisp
    4 deal-ui/package.lisp
  150 deal-ui/pQuery.lisp
   46 deal-ui/util.lisp
   14 deal.asd
   15 deal-ui.asd
 1435 total
inaimathi@lambda:~/projects/deal$ 
```

We're at ~1500 lines, and not terribly likely to crack 2000, *and* we've got a pretty feature-full little play-testing tool to show for it.

## <a name="day-ive-lost-track-and-actually-these-have-been-bullshit-for-a-while-its-august-st-though-so-maybe-i-just-start-counting-from-the-end-now-yeah-ok-lets-do-that" href="#day-ive-lost-track-and-actually-these-have-been-bullshit-for-a-while-its-august-st-though-so-maybe-i-just-start-counting-from-the-end-now-yeah-ok-lets-do-that"></a>Day I've lost track. And actually, these have been bullshit for a while. It's August 31st though, so maybe I just start counting from the end now? Yeah, ok, lets do that.

## <a name="t-minus-days" href="#t-minus-days"></a>T minus 30 days

It's amazing what a little sleep will accomplish. I haven't written any more code, but I've got the shape of the deck/board editor in my head.

Basically, it'll be an interface where users can specify collections of cards in text or JSON format. Those collections will then be added to their decks pane, at which point they can add it to the table. Each deck will also get a download button, which will let the user save that particular deck in a JSON format suitable for uploading later.

Before a second player joins a game, there will be a save button somewhere on the toolbar, and clicking it will let the user download a chunklet of JSON that represents everything on the board. The "new table" window will also gain a new `upload` button which will let the user upload a start file, and start the game off with the specified items pre-arranged on the board. We want this dump to contain *everything*. That is, all the information about things on the board, including the `contents` slots of face-down cards, and the list of cards inside of all stacks in play. *That* means we can't let the user randomly save out state; they could use it to cheat at the game<a name="note-Sat-Sep-07-221208EDT-2013"></a>[|2|](#foot-Sat-Sep-07-221208EDT-2013).

Now, because a goal of this project is zero authentication, and because I want it to be inherently distributed, I can't just save all this stuff on the server side. The client actually needs to store their own games and start files, and upload them when they need to use them. This makes certain seemingly uncommon things very slightly clunkier, but it lets a given user freely switch browsers and servers whenever they like, rather than being bound to a single instance of Deal.

Also, because I'm not going to be serializing *everything* in these dumps<a name="note-Sat-Sep-07-221214EDT-2013"></a>[|3|](#foot-Sat-Sep-07-221214EDT-2013), the complete system will actually need three different ways of outputting its internal data structures:


- one that leaves out any private information, such as the contents of stacks, or the text on face-down cards. This one is already implemented as the `redact` method
- one that keeps private information, but strips anything game-specific, such as IDs and `belongs-to` slots. This is what I'm discussing for the save feature; it should be possible to use these to instantiate a particular object or board in a different game. Not sure what to call it, but it's named `serialize` for the moment. Maybe `save` or something?
- one that keeps everything. This is the one I'll be using to store logs and replays later. It needs *all* the information about a particular game, its result shouldn't be available until after a game is concluded, and the board should have an additional piece of data attached to describe its starting position.


## <a name="t-minus-days" href="#t-minus-days"></a>T minus 29 days

Something odd started going wrong recently, and I had to sit down to puzzle through it. It turned out to be an odd corner of how `cl-json` handles encoding `list`s. See, because a list might represent an `[alist](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node153.html)`, a `[plist](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node108.html)` or just a vanilla `[list](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node149.html)`, there's ambiguity in terms of how to encode it. The way that this seems to be handled is with an error/backtrack strategy that tries each option and sees what it comes up with. The trouble with *that* when you're serializing things that look something like

```lisp
`((TYPE . :TABLE) (ID . :G2444) (TABLECLOTH)
  (THINGS ((:G2523 (TYPE . :STACK) (ID . :G2523) (X . 677) (Y . 461) (Z . 0) (ROT . 0)
                   (BELONGS-TO . :G2443) (CARD-COUNT . 78) (CARD-TYPE . :OCCULT-TAROT))
           (:G2578 (TYPE . :STACK) (ID . :G2578) (X . 705) (Y . 259) (Z . 0) (ROT . 0)
                   (BELONGS-TO . :G2443) (CARD-COUNT . 54) (CARD-TYPE . :FRENCH))
           (:G2619 (TYPE . :STACK) (ID . :G2619) (X . 815) (Y . 358) (Z . 0) (ROT . 0)
                   (BELONGS-TO . :G2443) (CARD-COUNT . 40) (CARD-TYPE . :N-ITALIAN)))
          (PLAYERS ((ID . :G2443) (HAND . 0)))
          (HISTORY
           ((TIME . 3587036264) (TYPE . :NEW-DECK) (PLAYER . :G2443)
            (PLAYER-TAG . "Anonymous Coward") (NAME . "Northern Italian")
            (STACK (TYPE . :STACK) (ID . :G2619) (X . 815) (Y . 358) (Z . 0) (ROT . 0)
                   (BELONGS-TO . :G2443) (CARD-COUNT . 40) (CARD-TYPE . :N-ITALIAN)))
           ((TIME . 3587036262) (TYPE . :NEW-DECK) (PLAYER . :G2443)
            (PLAYER-TAG . "Anonymous Coward") (NAME . "54-Card Standard")
            (STACK (TYPE . :STACK) (ID . :G2578) (X . 705) (Y . 259) (Z . 0) (ROT . 0)
                   (BELONGS-TO . :G2443) (CARD-COUNT . 54) (CARD-TYPE . :FRENCH)))
           ((TIME . 3587036259) (TYPE . :NEW-DECK) (PLAYER . :G2443)
            (PLAYER-TAG . "Anonymous Coward") (NAME . "Occult Tarot")
            (STACK (TYPE . :STACK) (ID . :G2523) (X . 677) (Y . 461) (Z . 0) (ROT . 0)
                   (BELONGS-TO . :G2443) (CARD-COUNT . 78) (CARD-TYPE . :OCCULT-TAROT))))))
```

With, maybe, a few more levels in a couple of places, the error/backtrack strategy ends up bottoming out your error depth. Even if you use `encode-json-alist` instead of the standard `encode-json`, the library seems to guess what it needs to do separately for each element of each level. Also, I'm not sure whether it's how I have Hunchentoot configured or whatever, but for some reason, this error would immediately be ignored in REPL. I had to really be paying attention to catch the flicker of `error depth exceeded` pass by SLIME. The only real symptom I could detect was otherwise unexplained `502` errors on the client side, which is why it took me a few hours to figure out what was actually happening.

The solution ended up being fairly easy to implement: just use hashes everywhere. Because those are unambiguous to encode into JSON, they won't incur the same guessing damage. So even though I've got a feeling they're heavier than `alist`s, they'll avoid a whole lot of unnecessary work later. Of course, since Common Lisp doesn't have good built-in syntax for writing hashes, I had to write my own.

```lisp
(defmacro hash (&rest k/v-pairs)
  (with-gensyms (hash-table)
    `(let ((,hash-table (make-hash-table)))
       ,@(loop for (k v) on k/v-pairs by #'cddr
            collect `(setf (gethash ,k ,hash-table) ,v))
       ,hash-table)))
```

Which lets me redefine the `redact` methods to emit hash tables rather than alists, like so:

```lisp
(defmethod redact ((stack stack))
  (with-slots (id x y z rot belongs-to card-count card-type) stack
    (hash :type :stack :id id :x x :y y :z z :rot rot
          :belongs-to belongs-to
          :card-count card-count
          :card-type card-type)))
```

That's still begging for some better syntax, of course. Its already bitten me once; I wrote the `card` emitter and forgot to have it encode the `id` slot, which ended up preventing cards from being interacted with on the front-end. Anyhow, that's fixed. Hopefully.

The other thing I managed to do was put together a very minimal little system for creating custom decks. Which means that after I hack in the intentionally limited game-state save/load feature, this will officially be a capable playtesting **and** prototyping tool. Wish me luck.

## <a name="t-minus-days-wee-hours-of-the-morning" href="#t-minus-days-wee-hours-of-the-morning"></a>T minus 28 days: wee hours of the morning

It's about 1:20 right now, and I managed to put a few more hours in. The latest [codebase up at github](https://github.com/Inaimathi/deal) has a working, if ugly, upload system. That is, while you're the only one in your game, you can set stuff up, save a `game.json` file, and load it up again later to replicate your earlier setup. It looks like the web hasn't been sitting still after all, by the way. Whereas the server-push situation still sucks a bag of donkey dongs, file uploading now sucks only a single such dong. Two at the outside. You can do [asynchronous file uploads fairly simply](http://stackoverflow.com/a/8758614), provided you're willing to live with only supporting HTML5 browsers, and trust me, I am. Having put that together, this is technically a finished playtesting/prototyping tool for tabletop card games. There's still a [bunch of polish](https://github.com/Inaimathi/deal/issues) I want to put on it, patches welcome by the by, and I still want to make a few things make slightly more sense, *and* I really want to make some parts of the tool prettier before I start promoting it publicly, but even [Deal as it stands](http://deal.inaimathi.ca/static/index.html) is now enough to do the prototype work which was ostensibly the point of the project.

## <a name="t-minus-days" href="#t-minus-days"></a>T minus 23 days

I just finished the single biggest commit since the start of this project. It... well, here, I'll just read you the log message.

```
16fb9532 * origin/master 
         | Author: inaimathi <inaimathi@lambda>
         | Date:   Sat Sep 7 20:53:26 2013 -0400
         | 
         |     Easily the biggest commit yet
         |     
         |     -Added cl-fad to dependency list
         |     -minis are now implemented (still no custom minis; working on it)
         |     -tablecloths are now implemented (still no custom tableclblah blah blah)
         |     -fixed droppable bug related to window scrolling. Not pixel-precise, but I think this is as close as we're gonna get
         |     -fixed droppable bug which kept competing droppables disabled even after a droppable event was completed
         |     -added go-board tablecloth
         |     -added chess-board tablecloth
         |     -added chess/checkers/go/chinese-checkers minis
         |     -added all relevant images
         |     -dragging a card/mini/stack to your backpack now removes it from play (may want to keep this at just minis. Might get a bit confusing otherwise)
         |     -chat history is now tracked in-cookie for completion purposes
         |     -player tag is now tracked in-cookie for completion purposes
         |     -dice counts for rolling icons in backpack are now tracked in-cookie for ease of use (I still prefer rolling via the @roll command)
         |     -position of zoomed card and table-toolbar is now tracked in-cookie for ease of use
         |     -did I mention we had zoomed cards now? We have zoomed cards now.
```

Not mentioned is the addition of an actual license<a name="note-Sat-Sep-07-221225EDT-2013"></a>[|4|](#foot-Sat-Sep-07-221225EDT-2013), but I won't dwell on that.

Minis now actually work. Ditto tablecloths. Which means you can now effectively play [chess](http://en.wikipedia.org/wiki/Chess), [checkers](http://en.wikipedia.org/wiki/Draughts) and [go](http://en.wikipedia.org/wiki/Go_(game)) in deal, which was a not-entirely-unintended side-effect of building a general-purpose tabletop game simulator. That's all [up](http://deal.inaimathi.ca/static/index.html), by the way. You can play around with it now.

The big stuff that still needs to get done, as far as I'm concerned, is in order of priority


- some UI-tweaking regarding the chat, tablecloths, dice interface, stacks (which are still ugly as sin) and deck editing
- cookie-ing of custom decks
- the ability to specify custom tablecloths and minis, and cookie-fying those too
- a bunch of re-factoring and code contraction related to various wordy and common patterns that cropped up in deal-ui


that's basically it though. And all of those things deal with the front-end, which I've already mentioned, should really be thought of as "the reference UI implementation" rather than "the UI". The intent is for people to be able to put together their own specialized, possibly game-specific user interfaces, and run them against the same back-end server.

That's the elephant in the room at the moment, by the way. The server is currently [much, *much* harder to set up](https://github.com/Inaimathi/deal#installation) than I'd like. The main reason is the separation of SSEs from the rest of the application. Since that involves [a non-standard nginx module](https://github.com/wandenberg/nginx-push-stream-module), it means any potential user needs to compile their own `nginx`, then perform some non-trivial, and not-easily-automated setup before anything starts working. Ideally, that would all be handled by the application. Which means that, after the contest ends, I've got two options:


- write a custom, high-performance asynchronous web server for Common Lisp
- re-write the server-side of the application in a language that already has such a server. Most likely candidates being [Haskell](http://hackage.haskell.org/package/warp) and [Erlang](http://hyber.org/)


Neither will be too much of a challenge.

If I go the server route, there's no reason whatsoever for me to put together a general-purpose web server. All it'll have to do is handle stream publishing, and serve static files faster than it would take to type them manually<a name="note-Sat-Sep-07-221236EDT-2013"></a>[|5|](#foot-Sat-Sep-07-221236EDT-2013).

On the other hand, the complete back-end including `asd`s, config files and the entire [`define-handler` DSL](https://github.com/Inaimathi/deal/blob/master/define-handler.lisp) weighs in at 805 lines of code<a name="note-Sat-Sep-07-221240EDT-2013"></a>[|6|](#foot-Sat-Sep-07-221240EDT-2013), so I can't see it being exceedingly difficult to port over. I *could* see either Haskells' assorted type-system bullshittery or Erlangs' [built-in bureaucracy](http://langnostic.blogspot.ca/2012/06/not-building-erlang-apps.html) adding another couple hundred lines, but 1000 total is still nothing to worry about in the grand scheme of things. Especially since it would end up letting potential users run the fucker without compiling anything themselves.


* * *
##### Footnotes

1 - <a name="foot-Sat-Sep-07-221201EDT-2013"></a>[|back|](#note-Sat-Sep-07-221201EDT-2013) - That'll necessitate interacting with multiple cards in the future, but it's the better alternative, I think.

2 - <a name="foot-Sat-Sep-07-221208EDT-2013"></a>[|back|](#note-Sat-Sep-07-221208EDT-2013) - Of course, it is possible to just log this event to history to make everyone aware of the possibility, so it might work either way.

3 - <a name="foot-Sat-Sep-07-221214EDT-2013"></a>[|back|](#note-Sat-Sep-07-221214EDT-2013) - In particular, they'll be leaving out any `id` and `belongs-to` slots.

4 - <a name="foot-Sat-Sep-07-221225EDT-2013"></a>[|back|](#note-Sat-Sep-07-221225EDT-2013) - The [AGPL](http://www.gnu.org/licenses/agpl-3.0.html), surprise, surprise.

5 - <a name="foot-Sat-Sep-07-221236EDT-2013"></a>[|back|](#note-Sat-Sep-07-221236EDT-2013) - The assumption being that it should be able to support small custom servers, and that if someone wants to serve several thousand people, they should use a reverse proxy and serve static files that way anyhow.

6 - <a name="foot-Sat-Sep-07-221240EDT-2013"></a>[|back|](#note-Sat-Sep-07-221240EDT-2013) - In case you were wondering, yes, I take the Yegge quote to heart.

> Large systems suck. This rule is 100% transitive. If you build one, *you* suck.   
> -[SteveY](http://steve-yegge.blogspot.ca/2007/06/rich-programmer-food.html).  
