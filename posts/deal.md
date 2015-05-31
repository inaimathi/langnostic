I already mentioned [Lisp In Summer Projects](http://lispinsummerprojects.org/) time, and called it

>...like a multi-month [NaNoWriMo](http://www.nanowrimo.org/) with parentheses instead of character development and sleep.
> --Inaimathi

The project I picked out wasn't something I'd ever talked about here before. It was something [a friend](http://chillier17.deviantart.com/) of mine and I were thinking of putting together to make his hobby/job slightly easier. He makes tabletop games you see. Mostly card stuff, but potentially other tabletop stuff too, from what I gather. This is to be the running journal of my project as I'm writing it. I'm not too worried about it denting my motivation, because I don't intend to publish it until I'm done.

To be perfectly honest with you, Lisp wouldn't be my first choice for this. I mean, the language is always a good thing to have in your corner, but there's a notable lack of battle-tested, feature-full, asynchronous web-servers for it. There's [Antiweb](http://hoytech.com/antiweb/), if you want to get down into the nuts and bolts of the system and serve things at the most efficient possible rate, at the expense of more complex installation and configuration. There's [Wookie](http://wookie.beeets.com/), if you don't particularly care about speed or security. There's [sw-http](http://www.cliki.net/sw-http), which I've been [warned off of directly](http://stackoverflow.com/questions/9388893/details-about-application-finder-fn-in-sw-http). And there's [Hunchentoot](http://weitz.de/hunchentoot/) if you don't care about being asynchronous. There's no Common Lisp equivalent to [Warp](http://hackage.haskell.org/package/warp) or [Yaws](http://hyber.org/) or [Tornado](http://www.tornadoweb.org/en/stable/), and I'm fairly comfortable with each of them, so if not for this contest, this probably would have been a [Haskell](http://www.haskell.org/haskellwiki/Haskell)/[Elm](http://elm-lang.org/), or perhaps [Erlang](http://www.erlang.org/)/[Angular.js](http://angularjs.org/) project rather than a [CL](http://www.cliki.net/)/[Parenscript](http://common-lisp.net/project/parenscript/) one.

I'm not *too* worried. The only part of this system that capital N *needs* to be asynchronous is the SSE handler I'll be using for browser pushes, and I'm fairly confident I'll be able to tweak Hunchentoot slightly to offload those onto a single, dedicated thread rather than keeping each one running in its own.

### The Approach

I want to battle-test some of my own ideas. Starting with the front-end/back-end separation I've been on about for a while, and continuing with some notions I've had about self-documenting APIs. To that end, `deal` is going to be a pair of projects. A game server implementation which will huddle behind [nginx](http://wiki.nginx.org/Main), deal with the application requests, and whose handler definitions are going to be simple enough to read that you'll be able to. And a reference-implementation of a web UI that will communicate with that server and do useful things in a browser.

Now then, without further ado.

### The Journal

## Day One

So here's the minimal amount of stuff we need to model in order to be a useful play-testing tool:


-   cards
-   collections of cards *(I'm going with "stack" for the moment)*
-   hands *(different from stacks in that they're not on the table, but being held by players)*
-   players
-   die-rolls/coin-flips
-   counters/notes


And we need to be able to interact with each one in a variety of ways.


-   rotate/move/flip cards and collections *("rotate" as in "on an axis", "flip" as in "from face-up to face-down or vice-versa")*
-   play *(either face up or face down)*
-   play to *(onto a stack rather than onto the board directly)*
-   pick up
-   shuffle *(this one just applies to a stack)*
-   peek *(a player looks at n cards of a given stack)*
-   show *(all players see n cards of a given stack)*
-   re-arrange *(peek at `n` cards and put them back in an order you specify)*


Each line of that second group needs to be a handler. Each line of the first group needs to be represented somewhere. Despite my confidence, I'm not *entirely* sure I won't be porting away from Hunchentoot if hacking SSE support into it turns out to be too difficult, so I'd rather define a little sub-language for handler definitions than call `define-easy-handler`s manually. While I'm at it, let that mini-language take type-hints so I don't have to deal with chucking strings around myself. The initial version of `define-handler` does simple type conversion, and thinly wraps `define-easy-handler`

```lisp
(defmacro define-handler ((name &key (default-type :integer)) (&rest args) &body body)
  (let ((opts `(,name :uri (concatenate 'string "/" (string-downcase (symbol-name name))))))
    (if (not args)
        `(define-easy-handler ,opts nil (encode-json (progn ,@body)))
        (flet ((type-exp (arg type)
                 (case type
                   (:integer `(parse-integer ,arg))
                   (:string arg)
                   (:keyword `(intern (string-upcase ,arg) :keyword)))))
          (let ((type-conversion (mapcar (lambda (a) 
                                           (if (atom a) 
                                               (list a (type-exp a default-type))
                                               (list (car a) (type-exp (first a) (second a)))))
                                         args))
                (final-args (mapcar (lambda (a) (if (atom a) a (car a))) args)))
            `(define-easy-handler ,opts ,final-args
               (let ,type-conversion
                 (encode-json (progn ,@body)))))))))

(defmacro define-game-handler ((name &key (default-type :integer)) (&rest args) &body body)
  `(define-handler (,name :default-type ,default-type) (game-id ,@args) ,@body))
```

And it lets me write things like

```lisp
(define-handler (play/move) (thing-id x y z rot)
  (list :moving thing-id :to x y z rot))
```

and have it mean `"give me the Integers thing-id, x, y, z and rot, and I'll give you a JSON-encoded response of the list ':moving thing-id :to x y z rot'"`.

That's it for day one.

## Day 3

I skipped one. In truth, this is a few days later, and I *have* been throwing hours/half-hours at the problem in the meantime, but getting no dedicated time down.

The type annotations are a good idea here, I think. Even in a dynamically typed language, you want to surround any kind of outside input with `[assert](http://www.lispworks.com/documentation/HyperSpec/Body/m_assert.htm#assert)`ions or similar, and being able to read the types is going to help people trying to interact with your API. The separate handler definition macro for tables was a misstep though. All it actually does at this point is provide a `with-lock` around the body, and add an invisible argument of `(table :table)` to whatever you define with it.

```lisp
(defmacro define-server-handler ((name) (&rest args) &body body)
  "Specifically defines handlers dealing with global server state.
Shares similarity with define-table-handler (if another one comes along, we'll abstract this)"
  `(define-handler (,name) ,args
     (with-lock-held ((lock *server*))
       ,@body)))
```

The first is bad because you don't always want a lock with a `table`. For instance, when you're serving up an `EventSource`, it would be a phenomenally bad idea to keep a lock on the related table. The second is bad because we're trying to make this self-documenting. Which means that, while invisible arguments are going to save some typing, they'll be just a little bit more annoying to any front-end developers who try to develop against our server. So, this has to go.

There's also the point that my existing type annotations aren't saving me as much work as they could be. Specifically, whenever I ask for a `foo-id`, I end up looking it up in the appropriate place; either `(things table)`, or possibly `(hand *player*)`<a name="note-Sun-Aug-25-220244EDT-2013"></a>[|1|](#foot-Sun-Aug-25-220244EDT-2013), then assert that the thing coming out of the lookup is the sort of `thing` I'm expecting, then I do something to that `thing`. The "type" system really should be able to do this for me.

```lisp
(defun type-exp (arg type)
  "Given a symbol name and a type, returns the expression to read that type from a string"
  (match type
    (:string nil)
    (:int `(parse-integer ,arg))
    (:json `(decode-json-from-string ,arg))
    ((or :keyword :facing)
     `(intern (string-upcase ,arg) :keyword))
    (:table 
     (lookup-exp arg '(private-tables *server*) '(public-tables *server*)))
    ((or :stack :flippable :placeable
         (list :card :from-table))
     (lookup-exp arg '(things table)))
    ((list :card :from-hand)
     (lookup-exp arg '(hand *player*)))
    (_ (error "Invalid type label: '~a'" type))))

(defun lookup-exp (arg &rest places)
  (with-gensyms (sym)
    `(let ((,sym (intern ,arg :keyword)))
       (or ,@(loop for p in places
                collect `(gethash ,sym ,p))))))

(defun lookup-assn (arg type)
  (match type
    (:table `(assert ,arg))
    (:stack `(assert (typep ,arg 'stack)))
    (:facing `(assert (or (eq ,arg :up) (eq ,arg :down))))
    (:placeable `(assert (typep ,arg 'placeable)))
    (:flippable `(assert (typep ,arg 'flippable)))
    ((list :card _) `(assert (typep ,arg 'card)))
    (_ nil)))

(defun type-pieces (args)
  "Takes a list of arguments and returns three values:
- The conversion expressions
- The names (for use as final args)
- The lookup assertions"
  (loop for (name type) in args
     when (aif (type-exp name type) (list name it)) collect it into convs
     collect name into as 
     when (lookup-assn name type) collect it into assrs
     finally (return (values convs as assrs))))

(defmacro define-handler ((name) (&rest args) &body body)
  "Defines handlers with an eye for self-documentation, DRY and portability"
  (let ((opts `(,name :uri ,(concatenate 'string "/" (string-downcase (symbol-name name))))))
    (if (not args)
        `(define-easy-handler ,opts nil (encode-json (progn ,@body)))
        (multiple-value-bind (type-conversion final-args lookup-assertions) (type-pieces args)
          `(define-easy-handler ,opts ,final-args
             (assert (and ,@final-args))
             ,(if type-conversion
                  `(let* ,type-conversion
                     ,@lookup-assertions
                     (encode-json ,@body))
                  `(progn ,@lookup-assertions
                          (encode-json ,@body))))))))
```

And it can.

The other thing I'm finalizing is the `id` system. An earlier crack just had each component keep count of its contents and assign that as the next id. There are a few obvious problems with this. Firstly that it would result in duplicate `id`s sometimes. Secondly, unless I wanted to update the item `id` every time I moved the item, this would mean a global counter in `*server*`, which would mean a lock on the whole server any time anything changed play zones. The change I ended up making is just using `[gensym](http://www.lispworks.com/documentation/HyperSpec/Body/f_gensym.htm#gensym)`. Ordinarily, I wouldn't *but*: these `id`s don't need to be cryptographically random, they just need to be unique with respect to all other active `id`s. Of course, doing it this way is going to run me up against potential problems when I get to loading games from disk storage, but that's a pretty long way off. Anyhow, as a result, all the `foo-id` and `id` fields are now `keyword`s rather than `integer`s.

## Day 4

First stab at the interface. And by "first stab", I mean "stupid basic interface that quote renders end-quote things by echoing them to console". It's nowhere near complete, but it's already enough to iron out a wrinkle or two. Specifically, I've had to go back through the model and change every `belongs-to` slot to expect an ID rather than a pointer to a `player`. It became obvious that this was necessary when I got memory-use warnings followed by a crash when I tried to "render" a card. `encode-json-to-string` doesn't like circular references, you see.

Now that everything uses IDs, there's one semi-obvious good thing about it: it'll make putting together the front-end much easier. Because the IDs are now globally unique, I can use them as a `class` tag in the `DOM` to identify objects on the board. That'll let me update the state of a lot of things in the UI without having to re-render very much at all.

## Day 6

I've been refining the model a bit to take into account some of the games I'll want to model for this project. There's also a slightly revised `define-handler` macro that stores information about any `handler`s it `define`s, which then gets served through the `list-handlers` handler. That'll make certain parts of the front-end easier to put together.

Not much work other than that, sadly. I'm still moving forward in increments of an hour or half-hour at the outside. What I *have* been able to do is read through pieces of the [Hunchentoot](http://weitz.de/hunchentoot/) code to try figuring out how, exactly, to hack conditional SSE support to it. Near as I can tell, I'll need to define a `:before` method for [`handle-request`](https://github.com/edicl/hunchentoot/blob/master/acceptor.lisp#L563-L582) and then figure out how to let its call chain know not to terminate the appropriate socket stream. Something else has occurred to me though. Because there's really only one handler I'm going to need to be served asynchronously, *and* that handler will *only* serve up public information, a reasonably simple approach here might be to just off-load SSE serving to [something](http://hackage.haskell.org/package/warp) better [suited](http://hyber.org/server_sent_events.yaws) for [it](http://nic.ferrier.me.uk/blog/2012_08/elnode-nears-1-point-0), [specifically](https://github.com/ztellman/aleph). Yet another approach, since I'm considering [aleph](https://github.com/ztellman/aleph), is to just write the whole thing in Clojure to begin with...

## Fatherly Interlude

My son is at a stage where everything he gets his hands on automatically goes in his mouth. Food, toys, cats, carpet, the computer I got him to paw at. Everything. He's also gotten to teething, which seems to be a very painful experience judging from his vocal emissions.

## Day 9

The past few days have been mostly prospective development and a little thought about secrecy. The end result is going to be some minor mechanical changes to how `id`s function, and they won't be shown for cards inside stacks.

Let me try to take you through it. What I was thinking earlier is that I can just assign a canonical ID to each `thing` that needs to go on the table. The trouble with that approach is that it canonically identifies a `thing`. So, for example, if you take a card from the table, put it into a stack, shuffle that stack, and then play a card face-down, it will be possible for each player to tell whether it's the same card. If it has the same `id` as the starting card, everyone knows what it is, otherwise, no one knows what it is but they can at least knock one option out of the possibility space.

This is not what you want.

The default for that situation is that no one should know what the card is, or have any additional information about it. There are two ways to solve this:


1.   We could create canonical `id`s for everything, but display a salted+hashed version to the front end, changing out the salt whenever the zone of play changes. That would let us keep a single `id` in the back-end, but it would keep everything reasonably anonymous to the front end. It seems kind of expensive, and complicated, and not particularly useful in any case.
1.   We could assign a new `id` to a `thing` when it crosses play zones. So, for example, when you `play` a card, it gets a new `id` while in play. If you then put it into a stack, it gets a new `id` while there. If you play it face-down out of the stack again, that's a third in-play `id`.


We don't actually need a central way of addressing a given `thing`. Or, at least, we don't yet, so I'm inclined to go for this second option. Remember, we generate `id`s through `gensym`, which is a pretty cheap computation as far as I know. We could, of course, keep our own global counter as part of `*server*`, but I'll see if that's necessary later. What I might want to do at the moment is name the function `make-id` just to make it a bit simpler to change if we end up needing to.

## Day 10

I've been thinking about the SSE situation, and it occurred to me that since


-   I'd only need one SSE channel per game
-   It would contain only public data
-   I would want it to support spectators *(and therefore wouldn't want to restrict access to it)*
-   I plan to deploy Deal by running a reverse-proxy from [nginx](http://wiki.nginx.org/Main)


it wouldn't be a bad idea to off-load that particular handler onto nginx itself. The ideal situation would be one where I could just serve up a file per game as the "stream", then keep appending to it from within Deal. That *doesn't* seem to be trivially possible, but nginx *does* have an optional, production-ready [push_stream_module](https://github.com/wandenberg/nginx-push-stream-module) licensed under [GPL3](http://gplv3.fsf.org/). That's something to consider, since it would really only take a bit of configuration twiddling as opposed to actual code to get this up-and-running.

## Day 12

Ok, I'm ignoring the SSE question for now; we don't really have any call for it until I get enough of a front-end together to support more than one player in any case. That's proceeding apace. I've been thinking about how to approach this task; should I abstract as much and as aggressively as possible, or should I keep it plain, straightforward and stupid? Typically, I go for the second option if I can help it at all, but I decided to go the opposite way this time. Here's a list of utilities I defined. Mostly thin wrappers around existing [jQuery](http://jquery.com/) constructs, and two *very* tasty pieces of syntactic sugar to help me define things.

```lisp
(in-package #:deal-ui)

(defparameter *debugging* t)

(defpsmacro log (&body body)
  (when *debugging*
    `(chain console (log ,@body))))

;;;;;;;;;; JS Basics
(defpsmacro obj->string (thing)
  `(chain -j-s-o-n (stringify ,thing)))

(defpsmacro string->obj (thing)
  `(chain j-query (parse-j-s-o-n ,thing)))

(defpsmacro fn (&body body) `(lambda () ,@body))

;;;;;;;;;; jQuery Basics
(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro doc-ready (&body body) 
  `($ document (ready (fn ,@body))))

(defpsmacro $map (lst &body body)
  `(chain j-query (map ,lst (lambda (elem i) ,@body))))

(defpsmacro $post (uri arg-plist &body body)
  `(chain j-query 
          (post ,uri (create ,@arg-plist)
                (lambda (data status jqXHR)
                  (let ((res (string->obj (@ jqXHR response-text))))
                    ,@body)))))

(defpsmacro $droppable (target &rest class/action-list)
  `($ ,target (droppable 
               (create 
                :drop (lambda (event ui)
                        (let ((dropped (@ ui helper context)))
                          ;; not sure if this should be a cond or a list of independent whens
                          (cond ,@(loop for (class action) in class/action-list
                                     collect `(($ dropped (has-class ,class)) ,action)))))))))

(defpsmacro $draggable (target (&key revert) &body body)
  `($ ,target (draggable (create :stop (lambda (event ui) ,@body) :revert ,revert))))

;;;;;;;;;; Define client-side ajax handlers
(defpsmacro define-ajax (name uri arg-list &body body)
  `(defun ,name ,arg-list
     (log *current-table-id* ,@arg-list)
     ($post ,uri (:table *current-table-id* ,@(args->plist arg-list))
            ,@body)))

;;;;;;;;;; Defining markup/behavior hybrids made easier
(defun expand-self-expression (form self-elem)
  (flet ((recur (frm) (expand-self-expression frm self-elem)))
    (cond ((null form) nil)
          ((atom form) form)
          ((and (eq 'self (car form)) (eq (second form) 'position))
           (recur '(+ "top:" (self y) "px;" "left:" (self x) "px;" "z-index:" (self z) ";" "transform:rotate(" (self rot) "deg)")))
          ((eq 'self (car form)) 
           `(@ ,self-elem ,@(cdr form)))
          ((atom (car form)) 
           (cons (car form) (recur (cdr form))))
          ((listp (car form)) 
           (cons (recur (car form))
                 (recur (cdr form)))))))

(defpsmacro define-thing (name markup &body behavior)
  (deal::with-gensyms (thing container)
    `(defun ,(intern (format nil "create-~a" name)) (container thing)
       (let* ((,thing thing)
              (,container container)
              (css-id (+ "#" (@ ,thing id))))
         ($ ,container (append (who-ps-html ,(expand-self-expression markup thing))))
         ,@(loop for clause in behavior
              collect (expand-self-expression clause thing))))))

&lt;p>The first bunch already kind of got addressed &lt;a href="http://langnostic.blogspot.ca/2011/03/javascript-with-lisp.html">last time I talked about parenscript&lt;/a>. Some newcomers include sugar for using map, draggables and droppables in a simpler way than the default jQuery UI package allows for&lt;/p>

(defpsmacro $map (lst &body body)
  `(chain j-query (map ,lst (lambda (elem i) ,@body))))

(defpsmacro $droppable (target &rest class/action-list)
  `($ ,target (droppable 
               (create 
                :drop (lambda (event ui)
                        (let ((dropped (@ ui helper context)))
                          ;; not sure if this should be a cond or a list of independent whens
                          (cond ,@(loop for (class action) in class/action-list
                                     collect `(($ dropped (has-class ,class)) ,action)))))))))

(defpsmacro $draggable (target (&key revert) &body body)
  `($ ,target (draggable (create :stop (lambda (event ui) ,@body) :revert ,revert))))
```

All of the correspondingly wrapped structures suffer from the same syntactic problem; they want you to pass them a function, but that function will always get the same arguments passed to it. In plain JS, you can't really bust out of this pattern without using `eval`. Which you shouldn't do. If you're dealing with JS through a language like Lisp though, you can just define macros like these to take the appropriate `body` arguments and then drop the appropriate `lambda`s around them. As long as you remember what the arguments *are*, that frees you from having to check documentation on their order every goddamn time I write any serious front-end JavaScript.

`define-thing` and `define-ajax` are more complex constructs. The second one is a way for me to define connecting functions between the front-end and the back end. Specifically, it lets me say things like

	     (define-ajax show-table "/show-table" ()
			  (render-board res))

That'll do exactly what you think it should; send an ajax request to the uri `/show-table`, then pass the JSON-parsed result to `render-board`. `define-thing` buys me more of the same, except it's good for defining local UI components rather than asynchronous handlers. Here's an example

```lisp
             (define-thing stack
                 (:div :id (self id) 
                       :class (+ "stack" (when (= (self face) "down") " face-down"))
                       :style (self position)
                       :title (self id)
                       (:button :class "draw" "Draw")
                       (:div :class "card-count" (+ "x" (self card-count))))
               ($draggable css-id () 
                           (move (self id) (@ ui offset left) (@ ui offset top) 0 0))
               ($ (+ css-id " .draw") (click (fn (draw (self id) 1)))))
```

Note that this makes use of the `$` and `$draggable` macros as well. What this does is sugar-coat the definition of a function called `create-stack`, which will take a container selector and a JSON object and


1.   slot the object into that markup specification
1.   append the result to the given container
1.   run the behavior applying code on the newly formed element


I'm still considering having the macro itself add the declaration of `:id (self id)`, because I do that literally everywhere. The only other interesting part is that this macro goes through the trees you pass it and expands anything that looks like `(self foo)` into something that looks like `(@ self foo)`, which is how you're supposed to index thing in Parenscript. It also special-cases `(self position)` into the complete CSS style rule, making sure that the `x`, `y`, `z` and `rot` slots are reflected in the relevant CSS properties.

That's that for now. Hopefully, I can 0.1 this thing fairly soon, and finally publish part one of this journal. I *was* going to wait 'till the end, but it looks like the complete document will be far too long to publish at once.

## Day 37

Kind of a big jump this time. Haven't really had the chance to do stuff related to this project lately. My time's been getting filled with extremely interesting, lispy things that I'm unfortunately not allowed to tell you about. Yet. Hopefully, I can convince the correct humans to let me publish some or all of it in the near future.

I've implemented the [session](http://weitz.de/hunchentoot/#sessions) system, which actually lets multiple people sit down at a single table and play together. That's basically it. I've been thinking about what I want the join/new-game interface to look like, but at this point that's all it'll have to be. An interface. The hard part is more or less done. There's one big architectural question I have to answer, and one big feature I need to properly implement, and then I can move on to the task of making the UI pretty, and maybe build some basic tools for deck construction as well as playing.


#### The Big Architectural Decision

Is whether to explicitly represent stacks in the final model. It *kind* of makes sense, given that you don't want anyone to know what cards actually get shuffled to, so it's possible to conceptualize "in a stack" as a state change for the card on the table. It still doesn't work that way in real life. You can take a bunch of cards and stack them, but you never lose the ability to interact with each of them individually. There might be one or two things that either view of the world enables or prohibits, but it also seems that it'd be pretty straight-forward to switch between them later if I wanted to. Maybe this is one I hold off on until I see a direct need.

#### The Big Feature

Is data pushing.

Fuck, I had vaguely hoped that in the year 2013, [this](http://langnostic.blogspot.ca/2012/02/client-communication.html) would be a solved problem, but none of the options provided natively as part of the http/js/html stack are both simple and compatible with the thread-per-request model of serving up data. I'm still heavily leaning to just using the [nginx](https://github.com/wandenberg/nginx-push-stream-module) stream module given that this projects' published data fits some specific criteria that would make a full public solution possible.

That's that. Once those are ironed out, I can finally post a one point oh and get people playing it. Oh, and get this piece published already so I can get on with the next one: taking it from "working" to "beautiful".

## Day 38

So the trivial part of the feature is done. It seems that the nginx stream module is easy to set up and get running properly. I haven't restricted publishing rights to `localhost` yet, but I can't imagine that'll be much more difficult to configure. Now comes the slightly harder part: defining the infrastructure inside of `deal` to publish to these streams and get new arrivals up and running. The basics will look *something* like

```lisp
(defmethod publish-move! ((table table) move &optional (stream-server *stream-server*))
  (push move (history table))
  (http-request
   (format nil "~apub?id=~a" stream-server (id table))
   :method :post :content move))
```

Except that I think I'm going to make `move` itself a JSON object just to make it easier to work with on the other end. The `*stream-server*` variable will then be set to the location of the nginx instance that handles stream serving for me. Note two things about this setup, incidentally:


-   the nginx stream module natively handles multiple protocols. By default it uses a forever-frame, but it can be configured to expose *the same stream* as an EventSource<a name="note-Sun-Aug-25-220313EDT-2013"></a>[|2|](#foot-Sun-Aug-25-220313EDT-2013), and a web-socket, and a long-poll handler.
-   the server handling my stream publishing doesn't have to be on the same machine as the rest of the application, which opens up some interesting hosting possibilities if scaling up ever gets to be *the* problem I'm staring down


It also really, truly looks like it'll be both more performant and much easier than trying to re-write pieces of Hunchentoot to support asynchronous requests in certain contexts.

## Day 41

I have no idea what happened, but I finally ended up getting a solid day to put stuff together for this project. As a result, I've got a pretty-much-playable edition sitting up on my server, waiting for a couple more edits before I unveil it, and this massive `Journal: Part One` I've had going. Right now, I'm in the guts of the `define-handler` mini-language, trying to get my pseudo-type-system to automatically solve the problems of argument bounding for me. That is, I want to be able to specify the min and max for various argument types and have it do the right thing. Specifically, I'd like to be able to specify minimum/maximum *values* for `:int`s, and minimum/maximum *lengths* for `:string`s.

The `:int` changes only come into play in the `new-table` handlers, and the dice-rolling system. I don't want people to start tables that seat fewer than 2 or more than 12. Also, I don't want people to be able to roll 2>-sided dice, or fewer than one of them. A `d2` is a coin-flip, which I have a separate handler defined for, and any less than that would be entirely too predictable. That's pretty obvious: assert that the incoming parameter fits within the specified range, and we're done.

The `:string` changes are going to be used as part of chatting and tagging. Chat messages are going to be delivered from the user to the named table/lobby, and tags are user-specified strings that will be applied either to themselves or games they start. Tags can be empty strings<a name="note-Sun-Aug-25-220319EDT-2013"></a>[|3|](#foot-Sun-Aug-25-220319EDT-2013), but chat messages need to be at least two characters. And *neither* thing should ever be longer than 255 characters<a name="note-Sun-Aug-25-220323EDT-2013"></a>[|4|](#foot-Sun-Aug-25-220323EDT-2013). Now, if I get a chat message shorter than I want it, that's obvious: just throw an error and do nothing.

But.

What do I do with a string *longer* than I want? There's two reasonable-sounding ways to handle that situation


1.   **Error out**; after all, interface this user is piloting doesn't conforming to my API, so it should come as no surprise to anyone
1.   **Truncat**; take a chunklet of whatever they sent small enough for my purposes, and proceed to fulfill the request with only the relevant data


Erring means chat messages get dropped, truncating means something goes out over the wire, even if it wasn't exactly what the user intended. Now that I think about it, it seems obvious that what you'd really want, as a user, is for the server to be hard-assed about it, but the front-end to tell you what's going on. In the interests of loose coupling, this means I actually want to specify that limitation in both places. Which works perfectly, because my server already emits the specifications for its handlers through [`/server-info` requests](https://github.com/Inaimathi/deal/blob/master/deal.lisp#L6-L12), and that will automatically include any mins/maxes I define in the relevant argument lines.

## Day 42

Basically, finished a bunch of the UI changes I needed to make in order to get this into a playable state. Not quasi-playable, not semi-playable, just plain playable. You can actually go [here](http://deal.inaimathi.ca/static/index.html) and use it for realzies. I mean, it's not *enjoyable* yet, and there's a lot of basic functionality still missing<a name="note-Sun-Aug-25-220327EDT-2013"></a>[|5|](#foot-Sun-Aug-25-220327EDT-2013), but you can actually go there with a couple of friends, start a game of [crazy eights](http://en.wikipedia.org/wiki/Crazy_Eights) or [something](http://www.pagat.com/climbing/asshole.html), and have an excellent chance of finishing it before anything crashes. If anything crashes, incidentally, [do report that](https://github.com/Inaimathi/deal/issues?state=open). Or patch it and send me a pull request.

I've got a bunch of things still in my head concerning where this project ought to go. Some of them involve more degrees of freedom in terms of the reference UI I've been putting together, one of them is a deck builder (which it'll need to fulfill the "prototyping" promise of this project), and another is a board position editor (which it'll need to fulfill the "prototyping" promise of this project for anything other than card games). Now that I think of it, those last to could possibly be combined elegantly. Hmm.

Anyhow, so concludes part one of my journal: `zero` to `playable`. Now I'll try to take it from `playable` to as close to `beautiful` as I can before the contest deadline is up.

Wish me luck.


* * *
##### Footnotes
1 - <a name="foot-Sun-Aug-25-220244EDT-2013"></a>[|back|](#note-Sun-Aug-25-220244EDT-2013) - This second one will change, incidentally. In the real system, this will be referencing a player record from the current users' session rather than the global one, so it's an even better idea to handle that in a macro rather than manually as part of each handler.

2 - <a name="foot-Sun-Aug-25-220313EDT-2013"></a>[|back|](#note-Sun-Aug-25-220313EDT-2013) - Which is basically a formally-specified forever-frame with direct JavaScript support in modern browsers.

3 - <a name="foot-Sun-Aug-25-220319EDT-2013"></a>[|back|](#note-Sun-Aug-25-220319EDT-2013) - The appropriate `id` gets used in that case so that there's an unambiguous way to refer to a player or game, if you're wondering, tags are just supposed to provide something human-readable.

4 - <a name="foot-Sun-Aug-25-220323EDT-2013"></a>[|back|](#note-Sun-Aug-25-220323EDT-2013) - Arbitrarily chosen. It's what all the cool kids were doing, and it serves my purposes well enough, so I went with it.

5 - <a name="foot-Sun-Aug-25-220327EDT-2013"></a>[|back|](#note-Sun-Aug-25-220327EDT-2013) - Such as changing your screen name, playing things face down, picking things up, coin-flips, dice rolls, and pretty much anything other than playing the standard 54-card deck.
