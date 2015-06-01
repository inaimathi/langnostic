Part three of my journal for the [Deal Project](https://github.com/Inaimathi/deal). Part [one](http://langnostic.blogspot.ca/2013/08/deal-journal-part-one.html) and [two](http://langnostic.blogspot.ca/2013/09/deal-part-2.html) are already up, of course.

## <a name="t-minus-days"></a>T minus 22 days

There's a couple of small patterns that I've come across. One:

```lisp
($ [foo]
   (button (create :icons (create :primary "ui-icon-[icon-name]") :text nil))
   (click (lambda (event) ($ [parent-or-child-of-foo] ([do-something])))))
```

and two

```lisp
($ foo
   (on [event] [child]
       (lambda (event)
         ($ [parent-or-child-of-foo] ([do-something])))))
```

Or, if you're more comfortable with JavaScript, I guess,

```lisp
$("foo")
    .button({"icons": { "primary" : "ui-icon-[icon-name]"}, "text": false})
    .click(function (event) {
        $("parent-or-child-of-foo").do-something();
    })

$("foo")
    .on("event", "child",
        function (event) {
            $("parent-or-child-of-foo").do-something();
        })
```

Now, you could argue about whether the JS-style hash-literal syntax is more elegant, or about how I might save a word or two somewhere if I was writing jQuery directly, but neither the JS nor the Lisp version of these are particularly fun to type. So.

```lisp
(defpsmacro $button (selector (icon-name &key text? (class "control-button")) &body on-click)
  `($ ,selector
      (button (create :icons (create :primary ,(format nil "ui-icon-~(~a~)" icon-name)) :text ,text?))
      (click (lambda (event) ,@on-click))
      ,@(when class `((add-class ,class)))))

(defpsmacro $on (context-selector &rest event/selector/behavior-list)
  `($ ,context-selector
      ,@(loop for (ev sel . behav) in event/selector/behavior-list
           collect `(on ,ev ,sel (lambda (event) ,@behav)))))
```

Tadaah. I also took the liberty of having the `$button` macro add a custom body class, for my own ease-of-use in the CSS. These let me write greatly simplified versions of both constructs like this:

```lisp
($button foo (:icon-name) ($ parent-or-child-of-foo (do-something)))

($on foo
     (:event "child"
             ($ parent-or-child-of-foo (do-something)))
     ...)
```

Much better. I'll be adding both of these to [cl-web-dev](https://github.com/Inaimathi/cl-web-dev) later on. While we're on the subject by the way, [that project](https://github.com/Inaimathi/cl-web-dev) has been very helpful for me, even though I've yet to actually use it anywhere. Porting code from `:deal` to `:cl-web-dev` forced me to think about how to generalize certain constructs, and its made me pay a lot more attention to the small assumptions I've been making while writing front-end code. I thoroughly recommend it. In fact, I may just get into the habit of writing a general version along with whatever project-specific code I'm writing in the future.

## <a name="later-that-day"></a>Later That Day...

Two things. First off, I updated the `$on` macro to give me the same kind of sugar as `$keydown`

```lisp
(defpsmacro $on (context-selector &rest event/selector/behavior-list)
  `($ ,context-selector
      ,@(loop for (ev sel . behav) in event/selector/behavior-list
           collect 
             `(on ,ev ,sel (lambda (event) 
                             ,@(if (eq ev :keydown)
                                   `((let (,@mod-keys ,@key-codes
                                           (key-code (or (@ event key-code) (@ event which))))
                                       (cond ,@(loop for (key body) on behav by #'cddr
                                                  collect `((= key-code ,(if (stringp key) `(chain ,key (char-code-at 0)) key)) ,body)))))
                                   behav))))))
```

That complicates the definition, but it lets me do things like

```lisp
...
($on "#deck-editor"
     (:keydown ".new-card"
               &lt;ret> (unless shift?
                       (chain event (prevent-default))
                       ($ "#deck-editor button.add-card" (click)))
               &lt;esc> ($ "#deck-editor .new-card" (val ""))))
...
```

Instead of putting together that keychecking manually, so fair enough. In this particular case, I could have gotten away with just using `$keydown` diretly, but I have some nefarious plans that involve front-end changes to the way this editor works, so it will *shortly* become important that the key event gets delegated rather than attached to a particular DOM node.

Second, it turns out file uploading isn't the only way JS file operations suck absolute balls in the browser.

File *downloading* as well as loading local files are both giant shit sandwiches that'll hopefully get fixed up at some point fairly soon. In the meantime, I've chosen to follow [Dave](http://stackoverflow.com/users/652308/dave)'s [advice for the file saving](http://stackoverflow.com/a/18690830/190887), which is basically "Use [FileSaver.js](https://github.com/eligrey/FileSaver.js)", and use a stupid handler workaround for loading.

As in, when the client needs to load a part of their local filesystem, they'll upload it to the server, which will echo it back so that it can be saved on the client side.

```lisp
; deal.lisp

...
;;; TODO: figure out a way to do this entirely on the client side. 
;;; Why should I have to give a rats' ass what they want to load?
(define-handler (load-deck) ((deck :json-file))
  deck)
...
```


```lisp
; deal-ui.lisp

...
($ "#load-deck-form" 
   (change 
    (fn ($upload "#load-deck-form" "/load-deck"
                 (load-deck-for-editing res)))))
...
```

I tell you, it's a good thing I don't drink.

There has got to be a better way of doing that. If all else fails, I'll just unwrap it into the naked `define-easy-handler` call. Because, just in case you didn't catch that, that wonderful `define-handler` mini-language works hard against me here. Because all of its output is JSON-serialized, and the incoming argument is of type `:json-file`, what's actually happening in `load-deck` is


1.   the server receives a file
1.   it parses said file into an s-exp representation of the JSON tree<a name="note-Sun-Sep-29-203106EDT-2013"></a>[|1|](#foot-Sun-Sep-29-203106EDT-2013)
1.   it encodes the result into JSON and writes it out as a string<a name="note-Sun-Sep-29-203108EDT-2013"></a>[|2|](#foot-Sun-Sep-29-203108EDT-2013)


What you're looking at is an `On` implementation of `identity`, with some really, *really* bad, network-dependent constant factors tacked on. And the entire thing is completely, aggravatingly unnecessary anyway because as the comment says: why do I care what my users are loading in the first place? I guess it doesn't really matter for the short term. It will *begin* to matter the instant people start actually using the thing, because I really won't be able to afford the extra traffic at that point.

## <a name="t-minus-days"></a>T minus 21 days

Today, I showed Deal off to the [first bunch of humans who aren't programmers](https://bentomiso.com/events/40-games-with-friends). Reaction was by and large positive, so that's good I guess. Also, someone called me an "idealistic craftsman", in what I'm sure he thought was a condescending way, but, well... I resemble that remark.

What I found out is that I'm pretty much on target for what people want, and my observations told me I really ought to fast-track some features I was planning for later on


-   custom backgrounds on cards/decks
-   custom tablecloths/minis
-   adding notes<a name="note-Sun-Sep-29-203112EDT-2013"></a>[|3|](#foot-Sun-Sep-29-203112EDT-2013)
-   rotation


I figured I could wait on these, but having seen some games people actually play, they'll will become painfully necessary soon. Also, I got the idea for a feature I wasn't even thinking of, but that's entirely obvious in retrospect. The ability to ping a particular location on the board, or perhaps an existing placeable. This wouldn't change the board in any way, but it would highlight a particular thing on it to draw everyones' attention.

The server-side components for all of those have been implemented, by the by. By the time this is published, they'll have been in production for about two weeks. The *client* side will take a little longer. Mostly because I have to figure out intuitive ways to trigger all of this without cluttering the UI.

## <a name="t-minus-days"></a>T minus 14 days

I've been spending most of my time figuring out a new server arrangement for the project. Which I did, more or less, and the incomplete<a name="note-Sun-Sep-29-203116EDT-2013"></a>[|4|](#foot-Sun-Sep-29-203116EDT-2013) results have already been published [here](http://langnostic.blogspot.ca/2013/09/deal-journal-interlude-one-treatise-on.html). I don't expect the session is going to give me any trouble at all, but I'm still leaving it for a bit. I've already got a running server with the shitty deployment method, after all, and work on the UI will benefit my current as well as my future deployments, so I figure those are higher priority targets. In particular, the more I think about rotation the more I think that's going to be the really annoying feature to add. We'll see I guess. I hope to have a further update on this project for you later today.

## <a name="t-minus-days"></a>T minus 13 days

Got a bit done yesterday, and a bit today. It turns out that neither implementing [element rotation](http://vremenno.net/js/jquery-ui-rotation-using-css-transform/) nor implementing [HTML5 localStorage](http://www.w3schools.com/html/html5_webstorage.asp) were particularly difficult. It did give me a couple of ideas that I'll need to look into though


-   resizing minis/tablecloths<a name="note-Sun-Sep-29-203119EDT-2013"></a>[|5|](#foot-Sun-Sep-29-203119EDT-2013)
-   storing custom mini/tablecloth URIs in local storage<a name="note-Sun-Sep-29-203122EDT-2013"></a>[|6|](#foot-Sun-Sep-29-203122EDT-2013)


The project's really winding down though. Apart from that server rewrite/tweak I'll need to make at some point, the work's pretty much done. And even that won't really change the UI much. Depending on the approach I end up taking, I may need to switch from EventSource to WebSockets on the front-end, and that's about as intense a project as I've got left. We've got a fairly nice tool in just under 2500 lines of Common Lisp.

Lets see what it'll take to make it beautiful instead of just nice...

## <a name="t-minus-days"></a>T minus 9 days

The deck editor has finally been overhauled into something approaching beautiful. You no longer need to know JSON to use it at all<a name="note-Sun-Sep-29-203128EDT-2013"></a>[|7|](#foot-Sun-Sep-29-203128EDT-2013), and it's reasonably intuitive. That's it. That was the goal. I've amended the note in README.md to reflect that we now, in fact, have the prototyping down cold.

At this point I'm giving serious thought to plowing ahead and putting together that special-purpose server I was on about last time. In a separate branch obviously, so that I still have something to show if it passes "pencils down" before I get done with it, but still.

## <a name="t-minus-days"></a>T minus 8 days

I've re-considered that again. Looking at the [issue tracker](https://github.com/Inaimathi/deal/issues), there's still a few things I want to put together *before* I start thinking about re-engineering the back end. It still needs to happen; putting together a stripped-down, async server with SSE and Session capabilities out of the box will make game writing in Lisp much easier. To the point that I might finally go back and finish up [Strifebarge](http://langnostic.blogspot.ca/2012/02/strifebarge-turn-based-web-games-in.html) once it's done. For this specific project though, some stuff is slightly more important than re-working the back end. Stuff like card images, custom notes, peek/show/play-from, setting the number of seats and starting private games. Normally, I'd say that's a short list. And I'd probably try to get all of the above done. Unfortunately, my next week is going to be busy as hell even without factoring in the additional work I intend to pour into Deal, so I'm picking my battles at the moment.

On another note, I was pointed to two "competitors" to this project way back when I showed this off to other humans. The reason the word "competitors" has quotes around it is that, as I said to the other human, I'm not competing. Deal is an entirely free-time project that I intend to release under the AGPL and use for some personal projects with a friend, but I'm not selling it<a name="note-Sun-Sep-29-203131EDT-2013"></a>[|8|](#foot-Sun-Sep-29-203131EDT-2013) or pushing it, I just want feedback. So it feels really bizarre to refer to anything as a competing product.

Anyhow, the two that got pointed out are [Vassal](http://www.vassalengine.org/) and [Roll20](http://roll20.net/), both of which I'd have trouble thinking of as "competitors" even if not for the previous points. Vassal is, bizarrely, a Java-based desktop app that requires you to connect to a central server to play. They can probably bend the UI to their will further than I can, but there are disadvantages to making something like this a piece of desktop software. Not the least of which is that connectivity is the point, which means you still need to worry about network connections, and run a [server](http://www.vassalengine.org/wiki/Server), but you get to build it all from the ground up<a name="note-Sun-Sep-29-203147EDT-2013"></a>[|9|](#foot-Sun-Sep-29-203147EDT-2013). So, to summarize: similar goals, wildly different architecture and approach.

Roll20 takes a similar *approach*, but has much different *goals*. It's a web application people connect to<a name="note-Sun-Sep-29-203150EDT-2013"></a>[|10|](#foot-Sun-Sep-29-203150EDT-2013), and they connect in order to have a shared space they can manipulate. That seems to be that though. The focus of Roll20 is to *play* tabletop games, and I'm not sure, but it looks like their card game support isn't very extensive. They focus on [tabletop games in the vein of D&D](https://wiki.roll20.net/Main_Page), and they don't seem to bother providing facilities to mock up your own minis/boards/cards/what-have-you, which is kind of key if you're looking for a prototyping tool. So again, executive summary: similar architecture and approach, wildly different goal.

I guess you could look at Deal as an attempt to unify these two applications, but that would imply I was aware of either when I started, and I wasn't.

## <a name="t-minus-days"></a>T minus 4 days

I seriously wanted to do a countdown from 5, but life gets in the way of that. Today I had an idea for a cooperative card game heavily riffing off of [ShadowRift](http://boardgamegeek.com/boardgame/112092/shadowrift), so I figured I'd put Deal through its paces. It was slow going at first, I'll admit, but I added two or three minor features that made it easier. Most notably, the card editor now deals with copy count rather than naively treating each individual card as a separate record, even if they're copies. It still doesn't go deep; the JSON representation I save still does the stupid thing and it shouldn't in the long run, but the interface has been greatly simplified. I'm going to release the game files so you guys can try it once I get it to version 1.0.

This weekend is the last couple of full days I'll be able to throw at the thing before the contest is up, which means I want to add a last couple of features to the front end, then concentrate on cleanup. There aren't any particularly gnarly bits I can think of off the top of my head, but its been a while since I got the chance to just sit down and read the UI code, so maybe I'll see rot accumulating where I wasn't expecting it.

## <a name="t-minus-days"></a>T minus 3 days

Today saw some minor UI changes, inspired by my attempt to actually build a game, as well as the removal of the last piece of stupid JS hack code on the server. There used to be a thing, which I already bitched about earlier, that looked like this

```lisp
;;;;;;;;;; Stupid hacks around in-browser JS limitations
;;; TODO: figure out a way to do this entirely on the client side. 
;;; Why should I have to give a rats' ass what they want to load?
(define-handler (load-deck) ((deck :json-file))
  deck)
```

in `deal.lisp`. The only reason it was there is that there didn't seem to be a good way of loading a local file into a JS application. Which a player would want to do if they saved their deck file, but were away from their machine or main browser. The result was that in order to accomplish this relatively straight-forward task, I had to let users upload their deck files, send them back (After parsing unfortunately; `:json-file` does a parse of its contents and I didn't feel like putting in further pseudo-types just for something I saw as a hack anyhow), where they would then be loaded locally.

The way I ended up solving that was with a bit of HTML5 tech (which unfortunately means that only HTML5-compatible browsers can load decks, but on the plus side, the sever is finally out of a loop it doesn't belong in anyhow)

```lisp
;;; in pQuery.lisp
...
(defpsmacro $load (elem-id &body body)
  (with-ps-gensyms (f-list)
    `(let ((reader (new -file-reader))
           (,f-list (chain document (get-element-by-id ,elem-id) files)))
       (setf (@ reader onloadend)
             (lambda (event)
               (let ((res (string->obj (@ event target result)))) 
                 ,@body)))
       (when ,f-list
         (chain reader (read-as-text (@ ,f-list 0)))))))
...
```

```lisp
;;; in deal-ui.lisp
...
($ "#load-deck-form" 
   (change (fn 
            ($load "load-deck-file" (load-deck-for-editing res))
            ($ "#load-deck-dialog" (dialog :close)))))
...
```

The macro takes an element id<a name="note-Sun-Sep-29-203159EDT-2013"></a>[|11|](#foot-Sun-Sep-29-203159EDT-2013) and a callback `body`. It sets up a `FileReader` to read the first file specified by the named element if any, then evaluates `body` in an environment where `event` is bound to the reader event and `res` is bound to the JSON-parsed content of the read file.

The block in `deal-ui.lisp` just calls that macro in a `change` event for `#load-deck-form`, and loads the resulting JSON for editing as a deck.

Not as pretty as it could possibly be, but it does the job.

## <a name="t-minus-days"></a>T Minus 2 days

I'm in the middle of throwing the second biggest chunk of time I've had at this project. For an opener, I put together a peek system which lets players look at and manipulate the cards in stacks. You can just peek, and you can reorder the cards you're looking at, *and* you can take arbitrary ones into your hand. Of course, as usual, all of this is logged in the games public broadcast, so you can't exactly cheat inconspicuously using these techniques.

That killed a good three or four feature requests with one stone. Before the day's up, I'm still hoping to do something about


-   the ability to play a card directly from a stack<a name="note-Sun-Sep-29-203202EDT-2013"></a>[|12|](#foot-Sun-Sep-29-203202EDT-2013)
-   the ability to flip over the top card of a stack
-   card/deck background images
-   leavers hands
-   in-game player status


and that's all. Once those are put together, I'm just doing a final few cleanup runs before the deadline is up. At that point, I'll consider this One Point Ohed, and I'll move on to the larger project chunklets I still want to put together, including a custom server and multi-select functionality.

## <a name="later-that-day"></a>Later That Day...

So that's done.

Seriously.

All of it.

The only thing I've got left to do tomorrow is cleanup, which is non-trivial but certainly possible. I don't even have enough mental energy to witter about technique at this point, so I know damn well that I don't want to bite off any more features on a schedule this tight. This has been a pretty draining three months, all things considered, but I'm almost done here. A break will be quite welcome after that.

## <a name="dawn-of-the-final-day"></a>Dawn of the Final day

Satori.

So that's it. It's actually **T Minus 1 day**, but it's highly doubtful I'll be able to do much work tomorrow. Off to tuning I go. If you're used to "refactoring" in the Java/C# sense, this is probably going to be the weirdest round of tweaks you've seen. Because I'll be trying to maximize front-end readability as opposed to performance. In other words, as a result of the effort I'm about to put forth, the application will probably get *slower* and *smaller*<a name="note-Sun-Sep-29-203205EDT-2013"></a>[|13|](#foot-Sun-Sep-29-203205EDT-2013).

Preliminary triage on **The Server Side** turned up one thing, but it was *very* preliminary so there may be more. There's a few handlers like this one

```lisp
(define-player-handler (table/stack/play) ((table :table) (stack :stack) (card-id :keyword) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (let ((card (find card-id (cards stack) :key #'id)))
    (assert card)
    (set-props card face x y z rot)
    (publish! table :played-from-stack `((stack . ,(id stack)) (card . ,(redact card))))
    (move! card stack table)
    :ok))
```

the relevant parts are

```lisp
(define-player-handler (table/stack/play) (... (stack :stack) (card-id :keyword) ...)
  (let ((card (find card-id (cards stack) :key #'id)))
    (assert card)
    ...))
```

What I'm doing is expecting a card. But it won't be in a hand, or on the table, it'll be in a stack. My psuedo-type-system doesn't cover this possibility, so what you see there is me getting a keyword from the client, looking it up manually and `assert`ing that it's actually in the `stack` I care about. This is a change I'm not doing right now, because I think the front-end will need much more work, but what I really want to be able to say in this situation is something like

```lisp
(define-player-handler (table/stack/play) (... (stack :stack) (card (:card :in-stack stack) ...)
  ...)
```

and have the lookups/assertions happen automatically on my behalf. There's a couple more places this pattern emerges, so I'll come back to put the fix together, but not right now. At the moment, this is a Note To Self.

There's a fuckton of work on **The Client Side**, as I suspected. There's places where I need to add `define-thing` declarations, there's places where `thing`s overlap more than I'd like, and there's a few places where small, counter-intuitive patterns are emerging. So, we'll begin at the beginning, get through as much of the middle as possible, hopefully come to the end and then stop.

**Incrementing**

There's a bunch of places that exhibit this sort of pattern

```lisp
...
(let ((trg ($ this (siblings ".num-dice"))))
  ($ trg (text (max 1 (- ($int trg) 1))))
  (store-dice))
...
```

The `(store-dice)` call there is actually just noise, but the rest of that expression is relevant. What I'm trying to express is "Decrement the number contained in this DOM element, and make sure the result doesn't go below 1", but because the number is stored textually, there's a bunch of overhead that needs to happen for that fairly simple concept to be executed. I need to get the element, get an integer from its contents, increment it, make sure it's not below 1 and then store it in place. You'll note that there's already a macro there that isn't helping much. `$int` just takes an element and returns its contents after a `parse-int` call. What I really want to be able to write in this situation is

```lisp
...
($decf ($ this (siblings ".num-dice")) -1 :min 1)
(store-dice)
...
```

Which is going to mean finally fixing the `$int` plumbing properly, like I've been meaning to. I already more-or-less solved this problem in [`cl-web-dev`](https://github.com/Inaimathi/cl-web-dev), so I'll be borrowing a slightly modified copy of that solution.

```lisp
(defpsmacro $val (selector &optional new-value)
  (with-ps-gensyms (sel type elem)
    (let* ((!exp (when new-value `(,new-value)))
           (val-exp `(chain ,elem (val ,@!exp))))
      `(let* ((,sel ,selector)
              (,elem ($ ,sel))
              (,type (chain ,elem (get 0) tag-name)))
         (case ,type
           ("INPUT" ,val-exp) ("BUTTON" ,val-exp) ("TEXTAREA" ,val-exp)
           (t (chain ,elem (text ,@!exp))))))))
```

That's going to do a better job than the current `$int` of getting/setting the `val` or `text` of a particular target<a name="note-Sun-Sep-29-203220EDT-2013"></a>[|14|](#foot-Sun-Sep-29-203220EDT-2013). It takes a `selector`, and optionally a `new-value`, checks what kind of element the `selector` specifies, and returns or modifies either its `text` or `val` as appropriate. The `!exp`<a name="note-Sun-Sep-29-203226EDT-2013"></a>[|15|](#foot-Sun-Sep-29-203226EDT-2013) shows up wherever a value is changed/looked up. It's written oddly, because I want it to expand into nothing as opposed to `nil` if a `new-value` wasn't passed in, which means stitching with `,@` rather than just `,`. The `val-exp` is what I run if the element is of a type that has a `(val)`, otherwise we need to return its `(text)`. This is one of those things that might go easier *without* jQuery.

Anyhow, now that we have a generic value getter/setter, we can define a new, shorter `$int` in terms of it.

```lisp
(defpsmacro $int (selector)
  `(parse-int ($val ,selector)))
```

Which should be self-explanatory. Next, `$incf` *would* be trivial to define if not for the bounding requirements. If we wanted a naive increment, we could just say

```lisp
(defpsmacro $incf (selector &optional (delta +1))
  (with-ps-gensyms (elem)
    `(let ((,elem ,selector))
       ($val ,elem (+ 1 ($int ,elem))))))
```

with just one minor complication we need so that `selector` isn't evaluated twice. however, we want to make sure this is a bounded `$incf`, so we need to account for that. Luckily, unlike with the `$val` macro, we can do that at macro-expansion time

```lisp
(defpsmacro $incf (selector &optional (delta +1) &key min max)
  (with-ps-gensyms (elem)
    (let* ((val-exp `(+ ,delta ($int ,elem)))
           (new-val (cond ((and min max)
                           `(max (min ,max ,val-exp) ,min))
                          (max `(min ,max ,val-exp))
                          (min `(max ,val-exp ,min))
                          (t val-exp))))
      `(let ((,elem ,selector))
         ($val ,elem ,new-val)))))
```

Now that we've got that, we can define `$decf` in terms of `$incf` for a bit of syntactic sugar<a name="note-Sun-Sep-29-203237EDT-2013"></a>[|16|](#foot-Sun-Sep-29-203237EDT-2013). That kills this pattern. Boom. Headshot.

**Self-Replacing `component`s and `aif`-friendly `$exists?`**

Next up, not so much a pattern as an annoying thing I've had to write

```lisp
...
(let ((sel (+ "#game-" (@ ev id))))
  (if ($exists? sel)
      ($ sel (replace (render-table-entry (@ ev table))))
      (render-table-entry (@ ev table))))
...
```

What I want there is to replace an existing game record with an update. It's not entirely obvious from that expression, but the reason I do this is that I want recently updated games to be at the top of the games list. `render-table-entry` `prepend`s, you see. Now that I think back on it actually, the only reason I didn't use `define-thing`<a name="note-Sun-Sep-29-203245EDT-2013"></a>[|17|](#foot-Sun-Sep-29-203245EDT-2013) here *is* that prepending issue. So...

```lisp
(defpsmacro define-thing ((name &key prepend? replace?) markup &body behavior)
  (with-ps-gensyms (container)
    `(defun ,(intern (format nil "create-~a" name)) (,container thing)
       ,@(when replace?
               `(when (@ thing id)
                  (aif ($exists? (+ "#" (@ thing id)))
                       ($ it (remove)))))
       ($ ,container (,(if prepend? 'prepend 'append) (who-ps-html ,(expand-self-expression markup 'thing))))
       (let (($self (aif (@ thing id) ($ (+ "#" it)) ($ ,container (children) ,(if prepend? '(first) '(last))))))
         (flet (($child (selector) (chain $self (children selector)))
                ($find (selector) (chain $self (find selector))))
           ,@(loop for clause in behavior
                collect (expand-self-expression clause 'thing)))))))
```

There. That adds optional `prepend?` and `replace?` options to `define-thing`. Note that I make the choice about replacement and prepending in the macro call, rather than the resulting function definition. That means that I can't `prepend` a thing I've declared as `append`ing<a name="note-Sun-Sep-29-203250EDT-2013"></a>[|18|](#foot-Sun-Sep-29-203250EDT-2013), but it means more work can be done at macro-expansion time, which is usually a good thing. 

As an aside, in the course of this edit, I noticed that there's another call pattern surrounding `$exists?`. Specifically

```lisp
...
                        (if ($exists? (+ "#deck-editor .cards .card[title='" (@ res name) "']"))
                            ($incf (+ "#deck-editor .cards .card[title='" (@ res name) "'] .count"))
                            (create-card-record "#deck-editor .cards" res))
...
```

the relevant idea here is that if a particular element `$exists?`, I tend to do something with it. Which means that I'd really like to use `aif` here, except I can't because `$exists?` returns `true` or `false`, rather than something interesting in the positive case. That's a pretty quick edit:

```lisp
(defpsmacro $exists? (selector)
  `(when (> (@ ($ ,selector) length) 0)
     ,selector))
```

It now returns the original selector, which means that I can re-write that `if` above as a very slightly cleaner

```lisp
...
                        (aif ($exists? (+ "#deck-editor .cards .card[title='" (@ res name) "']"))
                             ($incf ($ it (children ".count")))
                             (create-card-record "#deck-editor .cards" res))
...
```

Ahem. Now, then<a name="note-Sun-Sep-29-203259EDT-2013"></a>[|19|](#foot-Sun-Sep-29-203259EDT-2013).

```lisp
...
           (define-thing (table-entry :prepend? t :replace? t)
               (:li :id (self id)
                    (:span :class "tag" (self tag))
                    (:span :class "id" (self id))
                    (:span :class "players" (:span :class "count" (self player-count)) "/" (self max-players))
                    (:button :class "join" "Join"))
             ($highlight $self)
             ($button ($child ".join") (:arrowthick-1-ne) 
                      (let ((passphrase ""))
                        (lobby/join-table (self id) passphrase))))
...
```

Now that I've got that definition, I can go back and replace things like this

```lisp
...
(let ((sel (+ "#game-" (@ ev id))))
  (if ($exists? sel)
      ($ sel (replace (render-table-entry (@ ev table))))
      (render-table-entry (@ ev table))))
...
```

with things like this

```lisp
...
(create-table-entry "#open-tables" (@ ev table))
...
```

**Hidden Components**

Next up, there's a few elements on the front end that *are* components, but aren't *defined* as components. Specifically, there are mini-windows for specifying new games, custom minis and such, and they have their own behaviors, but they're specified ad-hoc and in-line.

```lisp
...
(define-component (new-table-form :empty? nil)
    (:div :id "new-table-setup" :class "overlay"
          (:h3 "New Table")
          (:div :class "content"
                (:input :class "game-tag")
                (:button :class "ok" "Ok")
                (:button :class "cancel" "Cancel")))
  ($button "#new-table-setup .ok" (:check :text? t)
           (lobby/new-table ($ "#new-table-setup .game-tag" (val)) ""))
  ($button "#new-table-setup .cancel" (:cancel :text? t) ($ "#new-table-setup" (hide)))
  
  ($keydown "#new-table-setup .game-tag" 
            &lt;ret> ($ "#new-table-setup .ok" (click))
            &lt;esc> ($ "#new-table-setup .cancel" (click))))
...

(define-component (custom-tablecloth-form :empty? nil)
    (:div :id "custom-tablecloth-form" :class "overlay" 
          (:h3 "Custom Tablecloth")
          (:div :class "content"
                (:input :class "url-input" :placeholder "Tablecloth image URL")
                (:input :class "name-input" :placeholder "Tablecloth Name")
                (:button :class "ok" "Ok")
                (:button :class "cancel" "Cancel")))
  ($button "#custom-tablecloth-form .ok" (:check :text? t)
           (let* ((name ($val "#custom-tablecloth-form .name-input"))
                  (tblc (create :name name :uri ($val "#custom-tablecloth-form .url-input"))))
             (create-custom-tablecloth "#tablecloth-tab .content" tblc)
             (setf (aref *session* :custom-tablecloths name) tblc)
             (store-custom-tablecloths)
             ($val "#custom-tablecloth-form input" "")
             ($ "#custom-tablecloth-form" (hide))))
  ($button "#custom-tablecloth-form .cancel" (:cancel :text? t) ($ "#custom-tablecloth-form" (hide))))
...

(define-component (custom-mini-form :empty? nil)
    (:div :id "custom-mini-form" :class "overlay"
          (:h3 "Custom Mini")
          (:div :class "content"
                (:input :class "url-input" :placeholder "Mini image URL")
                (:button :class "ok" "Ok")
                (:button :class "cancel" "Cancel")))
  ($button "#custom-mini-form .ok" (:check :text? t)
           (let ((uri ($ "#custom-mini-form .url-input" (val))))
             (create-custom-mini "#minis-tab .content" (create :uri uri))
             (aif (aref *session* :custom-minis)
                  (chain it (push uri))
                  (setf (aref *session* :custom-minis) (list uri)))
             (store-custom-minis)
             ($val "#custom-mini-form .url-input" "")
             ($ "#custom-mini-form" (hide))))
  ($button "#custom-mini-form .cancel" (:cancel :text? t) ($ "#custom-mini-form" (hide))))
...

(define-component (load-deck-form :empty? nil)
    (:div :id "load-deck-form" :class "overlay"
          (:h3 "Load Deck")
          (:div :class "content"
                (:form :id "load-deck-inputs" :enctype "multipart/form-data"
                       (:input :id "load-deck-file" :name "deck" :type "file")))
          (:button :class "cancel" "Cancel"))
  ($ "#load-deck-inputs" 
     (change (fn 
              ($load "load-deck-file" (load-deck-for-editing res))
              ($ "#load-deck-form" (hide)))))
  ($button "#load-deck-form .cancel" (:cancel :text? t) ($ "#load-deck-form" (hide))))
...
```

So that's done; they're all pulled together now. Except, that tweak makes it clear that I really mean something else. These are all overlays. They're meant to go in the middle of the screen in place of a `prompt`, they have a title, a bit of markup and a bit of behavior, and they all have `:cancel` buttons that work exactly the same way. This looks like something we could easily abstract.

**Defining Overlays**

```lisp
(defpsmacro define-overlay ((name &key (ok-button? t)) content &body behavior)
  (let* ((elem-name (format nil "~(~a~)-overlay" name))
         (display-name (string-capitalize (cl-ppcre:regex-replace-all "-" "load-deck" " ")))
         (id (concatenate 'string "#" elem-name)))
    `(define-component (,(intern (string-upcase elem-name)) :empty? nil)
         (:div :id ,elem-name :class "overlay"
               (:h3 ,display-name)
               (:div :class "content"
                     ,content
                     (:button :class "cancel" "Cancel")
                     ,@(when ok-button? `((:button :class "ok" "Ok")))))
       (let (($self ,id))
         (flet (($find (selector) ($ $self (find selector))))
           ,@behavior
           ($button (+ ,id " .cancel") (:cancel :text? t) ($ ".overlay" (hide))))))))
```

If you haven't seen it before, you may want to go through this one slowly. It's a macro that expands into a definition macro to abstract away the pieces this particular set have in common, leaving more or less just their differences in the resulting `define-overlay` calls. In this particular case, that's common extra arguments to `define-component`, a few lines of markup and a line of behavior. I could probably have boiled more out if I wanted to, but as you'll see below, this is already a pretty good result.

```lisp
...
(define-overlay (new-table)
    (:input :class "game-tag")
  ($button ($find ".ok") (:check :text? t)
           (lobby/new-table ($ "#new-table-setup .game-tag" (val)) ""))
  ($keydown ($find ".game-tag")
            &lt;ret> ($ ($find ".ok") (click))
            &lt;esc> ($ ($find ".cancel") (click))))
...

(define-overlay (custom-tablecloth)
    (:span (:input :class "url-input" :placeholder "Tablecloth image URL")
           (:input :class "name-input" :placeholder "Tablecloth Name"))
  ($button ($find ".ok") (:check :text? t)
           (let* ((name ($val "#custom-tablecloth-form .name-input"))
                  (tblc (create :name name :uri ($val "#custom-tablecloth-form .url-input"))))
             (create-custom-tablecloth "#tablecloth-tab .content" tblc)
             (setf (aref *session* :custom-tablecloths name) tblc)
             (store-custom-tablecloths)
             ($val ($find "input") "")
             ($ $self (hide)))))
...

(define-overlay (custom-mini)
    (:input :class "url-input" :placeholder "Mini image URL")
  ($button ($find ".ok") (:check :text? t)
           (let ((uri ($val "#custom-mini-form .url-input")))
             (create-custom-mini "#minis-tab .content" (create :uri uri))
             (aif (aref *session* :custom-minis)
                  (chain it (push uri))
                  (setf (aref *session* :custom-minis) (list uri)))
             (store-custom-minis)
             ($val ($find ".url-input") "")
             ($ $self (hide)))))
...

(define-overlay (load-deck :ok-button? nil)
    (:form :id "load-deck-inputs" :enctype "multipart/form-data"
           (:input :id "load-deck-file" :name "deck" :type "file"))
  ($change "#load-deck-inputs"
           ($load "load-deck-file" (load-deck-for-editing res))
           ($ $self (hide))))
...
```

And that's going to be the mod I end off on for now. There's more refactoring to do, of course.


-   The server-side still needs those additions to `define-handler`
-   There's a lot of boilerplate on the client-side relating to storing things in `localStorage` which I'm pretty sure I could abstract away
-   The initial `localStorage` loads happen all over the place, as opposed to just where I set `*session*`
-   There's still implicit components hiding in a couple of places
-   I've yet to unify the front-end SSE handlers with the chat message interface
-   *and* I'm calling `$button`/`$droppable`/`$draggable`/`$keydown` a lot more than I thought I would, and each of them has their own local bindings for the modifier and special key values. the specials, at least, probably ought to be global bindings instead of being inlined each time.


But all of that, and probably a lot more besides, will have to wait until I've had a bit of rest.


* * *
##### Footnotes

1 - <a name="foot-Sun-Sep-29-203106EDT-2013"></a>[|back|](#note-Sun-Sep-29-203106EDT-2013) - Possibly failing for encoding reasons.

2 - <a name="foot-Sun-Sep-29-203108EDT-2013"></a>[|back|](#note-Sun-Sep-29-203108EDT-2013) - Possibly failing due to that encoding-depth, `alist`-related bug related to s we discussed [last time](http://langnostic.blogspot.ca/2013/09/deal-part-2.html).

3 - <a name="foot-Sun-Sep-29-203112EDT-2013"></a>[|back|](#note-Sun-Sep-29-203112EDT-2013) - Either standalone to the table, or attached to an existing placeable.

4 - <a name="foot-Sun-Sep-29-203116EDT-2013"></a>[|back|](#note-Sun-Sep-29-203116EDT-2013) - Sans session.

5 - <a name="foot-Sun-Sep-29-203119EDT-2013"></a>[|back|](#note-Sun-Sep-29-203119EDT-2013) - Which will require server-side changes because I don't currently track the size of anything.

6 - <a name="foot-Sun-Sep-29-203122EDT-2013"></a>[|back|](#note-Sun-Sep-29-203122EDT-2013) - That way you don't need to keep pasting from imgur or whatever every time you start a game.

7 - <a name="foot-Sun-Sep-29-203128EDT-2013"></a>[|back|](#note-Sun-Sep-29-203128EDT-2013) - Entry happens in explicit k/v inputs and preview takes the form of actual, zoomable preview cards you can poke at instead of just an object dump.

8 - <a name="foot-Sun-Sep-29-203131EDT-2013"></a>[|back|](#note-Sun-Sep-29-203131EDT-2013) - Though, because it's AGPL, *you* should feel perfectly free to, as long as you respect your users' freedom by pointing them at a repo that includes any changes you've made.

9 - <a name="foot-Sun-Sep-29-203147EDT-2013"></a>[|back|](#note-Sun-Sep-29-203147EDT-2013) - Rather than getting the vast majority of it for free, as you would if you took the web application approach.

10 - <a name="foot-Sun-Sep-29-203150EDT-2013"></a>[|back|](#note-Sun-Sep-29-203150EDT-2013) - Though it's not AGPL, which I guess technically means that [I should have disregarded it](http://www.gnu.org/philosophy/free-software-for-freedom.html) even had I known about it when starting this project.

11 - <a name="foot-Sun-Sep-29-203159EDT-2013"></a>[|back|](#note-Sun-Sep-29-203159EDT-2013) - This is the *only* construct that does so, but I'm not going to hack leading "#" support into it quite yet because I have a sneaking suspicion that I could have put Deal together purely using HTML5 constructs. That's something else I'll look into after the contest; porting the back-end away from jQuery, hopefully just by re-defining the pQuery macros.

12 - <a name="foot-Sun-Sep-29-203202EDT-2013"></a>[|back|](#note-Sun-Sep-29-203202EDT-2013) - Which will also be helped by that peek interface.

13 - <a name="foot-Sun-Sep-29-203205EDT-2013"></a>[|back|](#note-Sun-Sep-29-203205EDT-2013) - Both conceptually and in terms of line-count, but that first one is the more important of the two.

14 - <a name="foot-Sun-Sep-29-203220EDT-2013"></a>[|back|](#note-Sun-Sep-29-203220EDT-2013) - For the record, I re-wrote that macro three times as I was writing this paragraph. Someone asked me why I blogged a while ago, and my answer was basically that this was an attempt at rubber-duck debugging with the internet instead of the duck. Don't laugh; it works.

15 - <a name="foot-Sun-Sep-29-203226EDT-2013"></a>[|back|](#note-Sun-Sep-29-203226EDT-2013) - For the non-schemers, that's pronounced "set expression"; `!` denotes a side-effect, not logical negation.

16 - <a name="foot-Sun-Sep-29-203237EDT-2013"></a>[|back|](#note-Sun-Sep-29-203237EDT-2013) - Specifically so that we don't have to say correct, but counter-intuitive things like `($incf -4)`.

17 - <a name="foot-Sun-Sep-29-203245EDT-2013"></a>[|back|](#note-Sun-Sep-29-203245EDT-2013) - That's a basic macro I defined earlier to let me group markup with behavior in various places. It defines some `who-ps-html` and some initialization to run each time I create a `thing`, and its gotten slightly more elaborate since I started.

18 - <a name="foot-Sun-Sep-29-203250EDT-2013"></a>[|back|](#note-Sun-Sep-29-203250EDT-2013) - Which I don't need to do anywhere.

19 - <a name="foot-Sun-Sep-29-203259EDT-2013"></a>[|back|](#note-Sun-Sep-29-203259EDT-2013) - Actually, as an aside to the aside, this change ended up giving me the expression 

```lisp
(aif ($exists? (+ ".new-deck.new-custom-deck[title='" deck-name "']"))
     ($ it (remove)))
```

in one particular place. The `($ it (remove))` just looks odd to me. And it's one of the reasons I've been defining `$foo` macros everywhere; my eyebrow wouldn't be raised at `($remove it)` in the same way. I'm not going to go down the rabbit-hole of implementing *that* right now, but it would be fairly easiy to.
