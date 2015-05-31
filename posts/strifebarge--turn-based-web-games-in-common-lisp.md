I've been in kind of a blah mood lately. To the point that I didn't really feel like struggling for huge new insights at all this weekend. Hopefully, that passes, because I really don't want to get mired in mediocrity any time soon. Anyway, you didn't come here to hear me being a whiny little bitch, so let me share the small insights I *have* had the energy to pursue.

First off, [quickproject](https://github.com/xach/quickproject) is fairly useful. It's missing some stuff I obsess over (I'm specifically thinking license boilerplate generators and an automatic `git init`+`.gitignore` call), and it does one or two small details in a way I don't like (mainly to do with the README file), but it still beats writing the `package` and `asd` file by hand. Next time I don't particularly feel like hunting down large insights, I'll probably fork this little utility and add the stuff I want. Between this and [quicklisp](http://www.quicklisp.org/), it's about high time I get a reasonably-sized pile of money together and [send it to Zach](http://www.quicklisp.org/donations.html), because that fucker **earned** it if anyone has.

With that out of the way, here's what I ended up using a chunk of my weekend to do. Unlike my previous piece on Hunchentoot development, this is meant to be less a lesson and more of an open code review by the invisible peanut gallery. Pot shots and [patches](https://github.com/Inaimathi/strifebarge) are welcome. I've had this idea of putting together a turn-based web-game for a while now, and that's the sort of thing that doesn't really require any kind of deep learning. Just some straightforward thinking from first principles, and some light iteration. So, I whipped *out* `quickproject` and whipped *up* an `0.01`. Lets start with the `asd` and `package`

```lisp
;;;; strifebarge.asd

(asdf:defsystem #:strifebarge
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
               #:ironclad
               #:parenscript
               #:cl-css
               #:swank
               #:clsql)
  :components ((:file "package")
               (:file "util")
               (:file "model") (:file "space") (:file "board") (:file "game")
               (:file "strifebarge")))
```

```lisp
;;;; package.lisp

(defpackage #:strifebarge
  (:use #:cl #:cl-who #:clsql #:hunchentoot #:parenscript)
  (:import-from #:swank #:find-definition-for-thing)
  (:import-from #:ironclad 
                #:encrypt-in-place #:decrypt-in-place #:make-cipher #:digest-sequence 
                #:octets-to-integer #:integer-to-octets
                #:ascii-string-to-byte-array #:byte-array-to-hex-string)
  (:shadow #:get-time))

(in-package #:strifebarge)

(defparameter *web-server* (start (make-instance 'hunchentoot:easy-acceptor :port 5050)))
```

And actually, now that I look at them, those clearly include things I haven't used yet, and may not for a while yet. I'll keep them around for the moment, but I'm leaving a mental note here that I really don't need anything past `:hunchentoot` and `:cl-who` just yet.

By the way, a significant chunk of this was `quickproject`-generated. I added the `:import-from` clauses, and some of the `:file` declarations, but that's pretty much it. The rest of it was created by running `quickproject:make-project` with the appropriate inputs. Moving right along, lets start with the meat of this thing

```lisp
;;;; strifebarge.lisp

(in-package #:strifebarge)

(defparameter *game* nil)

(define-easy-handler (index :uri "/") ()
  (let ((players (list (make-player 'carrier 'cruiser 'destroyer)
                       (make-player 'carrier 'cruiser 'destroyer))))
    (echo (apply #'make-game players) (car players))))

(define-easy-handler (new-game :uri "/new-game") (player-count)
  (let* ((p-count (if player-count (parse-integer player-count) 2)) 
         (players (loop for i from 1 to p-count
                        collect (make-player 'carrier 'cruiser 'destroyer))))
    (setf *game* (apply #'make-game players))
    (redirect "/join-game")))

(define-easy-handler (join-game :uri "/join-game") ()
  (assert (and (not (null (waiting-for *game*)))
               (null (session-value :player))))
  (setf (session-value :player) (pop (waiting-for *game*)))
  (redirect "/show-game"))

(define-easy-handler (show-game :uri "/show-game") ()
  (assert (not (null (session-value :player))))
  (echo *game* (session-value :player)))

(define-easy-handler (quit-game :uri "/quit-game") ()
  (assert (not (null (session-value :player))))
  (push (waiting-for *game*) (session-value :player))
  (setf (session-value :player) nil)
  "You have quit the game")

(define-easy-handler (turn :uri "/turn") (x y)
  (assert (and (eq (car (turn-stack *game*)) (session-value :player))
               (stringp x) (stringp y)))
  (advance-turn *game*)
  (fire *game* (session-value :player) (parse-integer x) (parse-integer y))
  (redirect "/show-game"))
```

`strifebarge` contains all the HTTP handlers this project uses. I've implemented a test handler (`index`), which does nothing now that I'm past working up the `echo` methods. It's also possible to create a `new-game`, `join` or `quit` a game, `show` the current state of a game board, and take a `turn`<a name="note-Mon-Feb-20-223207EST-2012"></a>[|1|](#foot-Mon-Feb-20-223207EST-2012).

I did mention that this was an `0.01`, so the intense lack of usability should come as no surprise to you. Firstly, there is only one `*game*`, stored in the global variable of the same name. For the moment, if anyone starts a new game, the old one gets clobbered. Secondly, note that turn order is maintained through an error mechanism. In the final game, those should actually display a little note along the lines of "It's not your turn yet", rather than vomiting a stack dump<a name="note-Mon-Feb-20-223214EST-2012"></a>[|2|](#foot-Mon-Feb-20-223214EST-2012).

Lets take a closer look at how the turn mechanism is approached. It actually starts at the `join-game` handler.

```lisp
(define-easy-handler (join-game :uri "/join-game") ()
  (assert (and (not (null (waiting-for *game*)))
               (null (session-value :player))))
  (setf (session-value :player) (pop (waiting-for *game*)))
  (redirect "/show-game"))
```

The `assert` here makes sure of two things:


-   The game is waiting for at least one more player to join
-   You have not already joined a game


As noted, if an `assert` statement fails, you get an error. If they both succeed, you are assigned a `player` record, stored in your `session`, to track who you are for the duration of the game<a name="note-Mon-Feb-20-223319EST-2012"></a>[|3|](#foot-Mon-Feb-20-223319EST-2012). This is relevant in two ways. Firstly, the `board` is displayed differently based on which `player` is looking;  we'll see more about this later, the only hint you get from this file is the call to `echo` in `show-game`.

```lisp
(define-easy-handler (show-game :uri "/show-game") ()
  (assert (not (null (session-value :player))))
  (echo *game* (session-value :player)))
```

and secondly the `player` record in your session determines when it's your turn.

```lisp
(define-easy-handler (turn :uri "/turn") (x y)
  (assert (and (eq (car (turn-stack *game*)) (session-value :player))
               (stringp x) (stringp y)))
  (advance-turn *game*)
  (fire *game* (session-value :player) (parse-integer x) (parse-integer y))
  (redirect "/show-game"))
```

Notice both that the `assert` in this handler makes sure that the top player on the `turn-stack` is the same as the player in your `session`, *and* that part of the handler body calls the method `advance-turn` on the current game before calling `fire` and re-displaying the game board. That segues nicely into

```lisp
;;;; game.lisp

(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; game creation and setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-player (&rest ship-types)
  (let ((p (make-instance 'player)))
    (setf (ships p)
          (mapcar (lambda (s) (make-instance s :player p)) ship-types))
    p))

(defun make-game (&rest players)
  (let ((board (make-board (mapcan #'ships players))))
    (make-instance 'game :board board 
                         :players players 
                         :waiting-for players 
                         :turn-stack players)))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo ((g game) (p player))
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html (:body (echo (board g) p)))))

;;;;;;;;;;;;;;;;;;;; actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod advance-turn ((g game))
  (if (cdr (turn-stack g))
      (pop (turn-stack g))
      (setf (turn-stack g) (players g))))

(defmethod fire ((g game) (p player) x y)
  (let ((result (make-instance 
                 (if (empty-space-at? (board g) x y) 'miss 'hit)
                 :player p :x x :y y)))
    (push result (history g))
    (setf (move (space-at (board g) x y)) result)
    result))
```

The creation and setup functions give you a pretty good idea of how `player`s and `games` are represented. For now, a `player` is just an object that has one or more `ship`s<a name="note-Mon-Feb-20-224338EST-2012"></a>[|4|](#foot-Mon-Feb-20-224338EST-2012). A `game` is a slightly more complex construct; it has a `board` as well as a collection of `players`, a `turn-stack` and list of players that haven't shown up yet<a name="note-Mon-Feb-20-224349EST-2012"></a>[|5|](#foot-Mon-Feb-20-224349EST-2012). We'll discuss the `board` a bit later, lets get into how `player`s and the `game` `function`. Um. I mean: function.

For the moment, `echo`ing a `game` just passes the buck to `echo`ing a `board` for the current player. There will eventually be things other than the board, such as various stat displays, and a turn counter. The interesting stuff here is `advance-turn` and `fire`.

```lisp
(defmethod advance-turn ((g game))
  (if (cdr (turn-stack g))
      (pop (turn-stack g))
      (setf (turn-stack g) (players g))))
```

After reading this, it should be perfectly obvious what the turn stack is, and how it enforces actions. It just starts off as a copy of the list of `players` participating in the game, and we `pop` the top record off each time a turn is passed. Once we get down to the last player in the stack, we copy out the list of `players` instead of `pop`ing again. That keeps the game circular.

```lisp
(defmethod fire ((g game) (p player) x y)
  (let ((result (make-instance 
                 (if (empty-space-at? (board g) x y) 'miss 'hit)
                 :player p :x x :y y)))
    (push result (history g))
    (setf (move (space-at (board g) x y)) result)
    result))
```

`fire` makes a new `hit` or `miss` marker<a name="note-Mon-Feb-20-224637EST-2012"></a>[|6|](#foot-Mon-Feb-20-224637EST-2012) and attaches it to the space ... I mean, `space`... at the given coordinates. It also records the `move` in the `game`s `history`.

Again, `0.01`, so neither of these functions actually deal damage to a given `ship`, or end the game if a player has been eliminated. The turn sequence just goes on until all the players stop playing. Note one very intentional effect of this architecture though; the game supports `n` `player`s by default. It's not a two-player affair, but rather, as many as you like<a name="note-Mon-Feb-20-224841EST-2012"></a>[|7|](#foot-Mon-Feb-20-224841EST-2012), as hinted at by the `new-game` handler<a name="note-Mon-Feb-20-224855EST-2012"></a>[|8|](#foot-Mon-Feb-20-224855EST-2012).

Before we deal with the `space` and `board` files, we should probably take a look at the `model`. There are some non-obvious interactions, and I want to lay them bare before getting into how I put together the actual front end and hit tracking.

```lisp
;;;; model.lisp

(in-package :strifebarge)

(defclass ship ()
  ((space-count :reader space-count :initarg :space-count)
   (player :reader player :initarg :player)
   (damage :accessor damage :initform 0)
   (coords :accessor coords :initarg :coords)
   (direction :accessor direction :initarg :direction)))

(defclass carrier (ship) ((space-count :initform 5)))
(defclass cruiser (ship) ((space-count :initform 3)))
(defclass destroyer (ship) ((space-count :initform 2)))

(defclass move ()
  ((player :reader player :initarg :player)
   (x :reader x :initarg :x)
   (y :reader y :initarg :y)))

(defclass hit (move) ())
(defclass miss (move) ())

(defclass player ()
  ((score :accessor score :initform 0)
   (sunken :accessor sunken :initarg :sunken)
   (ships :accessor ships :initarg :ships)))

(defclass board-space ()
  ((x :reader x :initarg :x)
   (y :reader y :initarg :y)
   (contents :accessor contents :initform nil)
   (move :accessor move :initform nil)))

(defclass board ()
  ((width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (spaces :accessor spaces :initarg :spaces)))

(defclass game ()
  ((board :accessor board :initarg :board)
   (players :accessor players :initarg :players)
   (waiting-for :accessor waiting-for :initarg :waiting-for)
   (turn-stack :accessor turn-stack :initarg :turn-stack)
   (history :accessor history :initform nil)))
```

You probably inferred the shape of the `game`, `player` and `move` classes based on stuff I've already shown you. The reason that `move`, `hit` and `miss` are implemented like this is twofold. First, it makes `echo`ing simple<a name="note-Mon-Feb-20-224942EST-2012"></a>[|9|](#foot-Mon-Feb-20-224942EST-2012), and second, it will eventually let me do clever things like color-coding shot markers per player.

The new stuff here is the `ship` and associated classes. I've only implemented 3; a 5-space, a 3-space and a 2-space vessel, each of which just inherits from `ship` and sets its `space-count`. As you can see, they're already prepared to take damage, in addition to tracking their position, orientation and owner. Now that I really think about it, I'm not sure why I have a ship track its coordinates after being placed; it becomes completely irrelevant to the ship at that point. The space-count matters<a name="note-Mon-Feb-20-225043EST-2012"></a>[|10|](#foot-Mon-Feb-20-225043EST-2012), but it makes no difference what specific spaces a given ship occupies and won't for a rather long while. That's definitely something I'll be removing after I finish this write-up.

The other new bits, which may help understand the rest of the files so I'll dwell on them a moment, are the `board` and `space` classes.

```lisp
(defclass board-space ()
  ((x :reader x :initarg :x)
   (y :reader y :initarg :y)
   (contents :accessor contents :initform nil)
   (move :accessor move :initform nil)))
```

A board-space has an `x` and `y` coordinate, as well as initially empty `contents` and `move` slots. You already saw what `move` does; when a `space` is fired upon, it's marked as either a hit or a miss using a shot flag<a name="note-Mon-Feb-20-225148EST-2012"></a>[|11|](#foot-Mon-Feb-20-225148EST-2012). The `contents` are exactly what you'd expect; each occupied `space` carries a pointer to the `ship` it contains.

```lisp
(defclass board ()
  ((width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (spaces :accessor spaces :initarg :spaces)))
```

Last one, and then we can round out the methods. A `board` caches its `width` and `height`, as well as keeping the full `spaces` grid. What a grid looks like is non-obvious from just the class declaration, so this is actually the perfect segue into

```lisp
;;;; board.lisp

(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; board creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun empty-grid (width height)
  (loop for y from 0 to height
        collect (loop for x from 0 to width collect (make-space x y))))

(defun empty-board (width height)
  (make-instance 'board 
                 :spaces (empty-grid width height)
                 :width width
                 :height height))

;;;;;;;;;;;;;;;;;;;; board setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod space-at ((b board) x y) (nth x (nth y (spaces b))))

(defmethod assign-ship-spaces ((s ship) direction x y)
  (loop for i from 0 to (- (space-count s) 1)
        if (eq :vertical direction)
          collect (cons x (+ i y))
        else
          collect (cons (+ i x) y)))

(defmethod position-ship ((s ship) (b board))
  (let* ((x (random (- (width b) (space-count s))))
         (y (random (- (height b) (space-count s))))
         (direction (pick '(:vertical :horizontal)))
         (ship-spaces (assign-ship-spaces s direction x y)))
    (if (every (lambda (p) (empty-space-at? b (car p) (cdr p))) ship-spaces)
        (progn 
          (setf (coords s) ship-spaces
                (direction s) direction)
          (loop for (x . y) in ship-spaces
                do (setf (contents (space-at b x y)) s)))
        (position-ship s b))))

(defun make-board (list-of-ships)
  (let* ((width (+ 5 (* 2 (length list-of-ships))))
         (height (+ 5 (* 2 (length list-of-ships))))
         (board (empty-board width height)))
    (dolist (s list-of-ships) (position-ship s board))
    board))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo ((b board) (p player))
  (with-html-output (*standard-output* nil :indent t)
    (:table :id "game-board"
            (mapc (lambda (row) 
                    (htm (:tr (mapc (lambda (s) (echo s p)) row)))) 
                  (spaces b)))))
```

As you can see, that's the chunkiest single file in the package, and that's because it implements creating a `board` as well as placing `ship`s<a name="note-Mon-Feb-20-225410EST-2012"></a>[|12|](#foot-Mon-Feb-20-225410EST-2012). Firstly, looking at `empty-board` and `space-at` should clear up what a `board` looks like. It's a list of lists of spaces<a name="note-Mon-Feb-20-225424EST-2012"></a>[|13|](#foot-Mon-Feb-20-225424EST-2012).

The `ship` placement methods are worth a slightly closer look

```lisp
(defmethod assign-ship-spaces ((s ship) direction x y)
  (loop for i from 0 to (- (space-count s) 1)
        if (eq :vertical direction)
          collect (cons x (+ i y))
        else
          collect (cons (+ i x) y)))

(defmethod position-ship ((s ship) (b board))
  (let* ((x (random (- (width b) (space-count s))))
         (y (random (- (height b) (space-count s))))
         (direction (pick '(:vertical :horizontal)))
         (ship-spaces (assign-ship-spaces s direction x y)))
    (if (every (lambda (p) (empty-space-at? b (car p) (cdr p))) ship-spaces)
        (progn 
          (setf (coords s) ship-spaces
                (direction s) direction)
          (loop for (space-x . space-y) in ship-spaces
                do (setf (contents (space-at b space-x space-y)) s)))
        (position-ship s b))))
```

The `position-ship` method takes a ship and a board and positions the ship on the board. It does this by randomly picking a starting `x`/`y` coordinate and `direction`. Those are fed into `assign-ship-spaces` which returns a list of `(x . y)` corresponding to the `space`s this `ship` will take up<a name="note-Mon-Feb-20-225545EST-2012"></a>[|14|](#foot-Mon-Feb-20-225545EST-2012). Once we have that, we check whether all of the generated spaces are currently empty, and if they aren't<a name="note-Mon-Feb-20-225552EST-2012"></a>[|15|](#foot-Mon-Feb-20-225552EST-2012), we try again. If the given spaces are clear, we<a name="note-Mon-Feb-20-225559EST-2012"></a>[|16|](#foot-Mon-Feb-20-225559EST-2012) store those spaces in the ships' `coords` and the direction in `direction`<a name="note-Mon-Feb-20-225609EST-2012"></a>[|17|](#foot-Mon-Feb-20-225609EST-2012) before assigning ship pointers to the appropriate `space`s on the `board`. Tadaah! That was the most complicated piece of this game.

`make-board` is fairly self-explanatory; it takes a list of `ship`s and determines `width`/`height` of the map based on how many there are, then places each `ship` and returns the resulting `board` instance. The `board`s' `echo` method should make perfect sense now that you've seen what a `board` is; in order to `echo` one, we start an HTML table and map `echo` over each `space` in each row of the `board`. Before we look at `space`, lets just zoom in on one part of `position-ship`. Specifically, the part that reads 

```lisp
...
(direction (pick '(:vertical :horizontal)))
...
```

`pick` isn't actually a Lisp primitive, but it's fairly simple to define. Here's

```lisp
;;;; util.lisp

(in-package :strifebarge)

(defun pick (a-list)
  "Randomly selects an element from the given list with equal probability."
  (nth (random (length a-list)) a-list))

(defun range (a b)
  "Returns a list of numbers starting with a and ending with b inclusive."
  (loop for i from a to b collect i))
```


Both are fairly self-explanatory. `range` is a second utility function I defined for an earlier iteration of the codebase, but ended up refactoring out all calls to it. I'm still keeping it, probably more out of superstition than anything else. In fact, never mind, I'm adding one to the list of things I need to trim once I finish writing this.

Ok, all that out of the way, lets finally take a look at

```lisp
;;;; space.lisp

(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; creation and setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-space (x y) 
  (make-instance 'board-space :x x :y y))

;;;;;;;;;;;;;;;;;;;; predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod empty-space? ((s board-space)) (null (contents s)))
(defmethod empty-space-at? ((b board) x y) (null (contents (space-at b x y))))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod echo ((s board-space) (p player))
  (with-html-output (*standard-output* nil :indent t)
    (:td (cond ((move s) (echo (move s) p))
               ((and (contents s) (eq (player (contents s)) p)) (str "#"))
               (t (htm (:a :href (format nil "/turn?x=~a&y=~a" (x s) (y s)) "~")))))))

(defmethod echo ((m hit) (p player))
  (with-html-output (*standard-output* nil :indent t)
    "X"))

(defmethod echo ((m miss) (p player))
  (with-html-output (*standard-output* nil :indent t)
    "O"))
```

I told you the hard part concluded with `make-board` earlier. `make-space` is a self-explanatory shortcut to using the raw `make-instance`<a name="note-Mon-Feb-20-225847EST-2012"></a>[|18|](#foot-Mon-Feb-20-225847EST-2012). The `empty` predicates are shorthand for checking whether a given `space` (or a `space` given by specified coordinates on a `board`) is empty.

The last mystery is solved with the `echo` methods here. A `space` is `echo`ed as a `td` tag, but its contents depends on certain properties of the `space`. First, if this `space` has been fired upon, we `echo` its shot marker<a name="note-Mon-Feb-20-230313EST-2012"></a>[|19|](#foot-Mon-Feb-20-230313EST-2012). Second, if the space hasn't been fired upon, but contains a ship belonging to the current player, we echo a marker for a `ship`<a name="note-Mon-Feb-20-230324EST-2012"></a>[|20|](#foot-Mon-Feb-20-230324EST-2012). Finally, if all else fails, we output a shot link with the coordinates of the current space, and wrap it around "~" which looks sufficiently wave-like for this stage of development.

As an architectural aside, that last one is why we needed the more complex representation of `space`s. I initially toyed with just having a simple 2-dimensional list of `'([move] [contents])`, but that would have been both more difficult to abstract from other parts of the program<a name="note-Mon-Feb-20-230403EST-2012"></a>[|21|](#foot-Mon-Feb-20-230403EST-2012), and it would have complicated emitting the coordinate link to `/turn`.

So there, putting it all together, we've got a very simple<a name="note-Mon-Feb-20-230423EST-2012"></a>[|22|](#foot-Mon-Feb-20-230423EST-2012) implementation of an HTTP-using multiplayer, turn-based, guessing/strategy game in Common Lisp with a grand total of 220 lines including comments<a name="note-Mon-Feb-20-230529EST-2012"></a>[|23|](#foot-Mon-Feb-20-230529EST-2012). Hopefully this step-by step has been useful to someone. If nothing else, it helped me figure out where I'm going next in a much more concrete way. I need to trim a few things, add some re-direction constructs to use in place of the assertions, get cracking on a sprite-set<a name="note-Mon-Feb-20-230600EST-2012"></a>[|24|](#foot-Mon-Feb-20-230600EST-2012), and figure out a good way to periodically notify clients about new developments in the game<a name="note-Mon-Feb-20-230609EST-2012"></a>[|25|](#foot-Mon-Feb-20-230609EST-2012). That's it for the short term, once that's all done, I'll do another one of these little reflection/code-review articles. 

If you feel like poking around the codebase for your own education, or for the purposes of patching, check out the [github repo](https://github.com/Inaimathi/strifebarge). I haven't actually decided what license I'm using yet, so maybe hold off on hacking on it until I get that cleared up.<a name="note-Mon-Feb-20-231049EST-2012"></a>[|26|](#foot-Mon-Feb-20-231049EST-2012)


* * *
##### Footnotes
1 - <a name="foot-Mon-Feb-20-223207EST-2012"></a>[|back|](#note-Mon-Feb-20-223207EST-2012) - Which fires a single shot on the specified space and passes the turn.

2 - <a name="foot-Mon-Feb-20-223214EST-2012"></a>[|back|](#note-Mon-Feb-20-223214EST-2012) - Which is what failed `assert`ions do.

3 - <a name="foot-Mon-Feb-20-223319EST-2012"></a>[|back|](#note-Mon-Feb-20-223319EST-2012) - Incidentally, this is why I wanted to include [:ironclad](http://method-combination.net/lisp/ironclad/) right out of the gate; as far as I know, Hunchentoot sessions [aren't particularly spoof-resistant](http://lists.common-lisp.net/pipermail/tbnl-devel/2007-December/003795.html), so in a real game I'd want better player verification than this approach gives me. I'm assuming the final solution will take the form of IP and user-agent recording combined with a [Diffie-Hellman handshake](http://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange).

4 - <a name="foot-Mon-Feb-20-224338EST-2012"></a>[|back|](#note-Mon-Feb-20-224338EST-2012) - It also has some other tracking slots, like score and how many ships they sank, but those don't get tracked quite yet.

5 - <a name="foot-Mon-Feb-20-224349EST-2012"></a>[|back|](#note-Mon-Feb-20-224349EST-2012) - That'd be `waiting-for`.

6 - <a name="foot-Mon-Feb-20-224637EST-2012"></a>[|back|](#note-Mon-Feb-20-224637EST-2012) - Depending on whether the space being fired into is `empty` or not, obviously.

7 - <a name="foot-Mon-Feb-20-224841EST-2012"></a>[|back|](#note-Mon-Feb-20-224841EST-2012) - Though I probably should have the option of limiting the count through a config variable somewhere in the final.

8 - <a name="foot-Mon-Feb-20-224855EST-2012"></a>[|back|](#note-Mon-Feb-20-224855EST-2012) - Which actually takes `player-count` as an input, and defaults to 2.

9 - <a name="foot-Mon-Feb-20-224942EST-2012"></a>[|back|](#note-Mon-Feb-20-224942EST-2012) - As you'll see when we get to the `space` file.

10 - <a name="foot-Mon-Feb-20-225043EST-2012"></a>[|back|](#note-Mon-Feb-20-225043EST-2012) - Or rather, will matter, once I start tracking ship damage.

11 - <a name="foot-Mon-Feb-20-225148EST-2012"></a>[|back|](#note-Mon-Feb-20-225148EST-2012) - An instance of the `move` class.

12 - <a name="foot-Mon-Feb-20-225410EST-2012"></a>[|back|](#note-Mon-Feb-20-225410EST-2012) - Which is only non-trivial because we're breaking tradition by placing all ships on the same board, necessitating both random placing and preventing ship collisions.

13 - <a name="foot-Mon-Feb-20-225424EST-2012"></a>[|back|](#note-Mon-Feb-20-225424EST-2012) - Subject to change to arrays in the final, but I can't be bothered to optimize at this point. On the upside, defining `space-at` explicitly means that when I change the representation of a board, I'll only have to change that single function and the `empty-` functions rather than tracking down every call to `nth`.

14 - <a name="foot-Mon-Feb-20-225545EST-2012"></a>[|back|](#note-Mon-Feb-20-225545EST-2012) - Taking into account this particular ships' length.

15 - <a name="foot-Mon-Feb-20-225552EST-2012"></a>[|back|](#note-Mon-Feb-20-225552EST-2012) - Which would signal a collision with another ship.

16 - <a name="foot-Mon-Feb-20-225559EST-2012"></a>[|back|](#note-Mon-Feb-20-225559EST-2012) - Uselessly

17 - <a name="foot-Mon-Feb-20-225609EST-2012"></a>[|back|](#note-Mon-Feb-20-225609EST-2012) - The direction will actually be useful sooner rather than later; it will help figure out how to render a ship once I start using sprites instead of the plain grid display going on currently.

18 - <a name="foot-Mon-Feb-20-225847EST-2012"></a>[|back|](#note-Mon-Feb-20-225847EST-2012) - This technique both saves some typing, and lets you be flexible about re-defining the representation of the object in question later. In this case, I could completely change how the game thinks of `board-space`s, and all I'd really need to change is the code in this file.

19 - <a name="foot-Mon-Feb-20-230313EST-2012"></a>[|back|](#note-Mon-Feb-20-230313EST-2012) - Currently, a `hit` is represented as "X" and a `miss` is "O".

20 - <a name="foot-Mon-Feb-20-230324EST-2012"></a>[|back|](#note-Mon-Feb-20-230324EST-2012) - The current representation is "#" for all ships, this will eventually get complicated enough to call for another `echo` method specializing on `ship`, but that can wait until I actually get some graphics up ins.

21 - <a name="foot-Mon-Feb-20-230403EST-2012"></a>[|back|](#note-Mon-Feb-20-230403EST-2012) - In the sense that changing a particular `space`s' `move` or `contents` would have necessitated at least a little grubbing around with `car` and `cdr`.

22 - <a name="foot-Mon-Feb-20-230423EST-2012"></a>[|back|](#note-Mon-Feb-20-230423EST-2012) - And still unplayable.

23 - <a name="foot-Mon-Feb-20-230529EST-2012"></a>[|back|](#note-Mon-Feb-20-230529EST-2012) - And that's even before the cuts trims I said I'd make.

24 - <a name="foot-Mon-Feb-20-230600EST-2012"></a>[|back|](#note-Mon-Feb-20-230600EST-2012) - Or try to find one.

25 - <a name="foot-Mon-Feb-20-230609EST-2012"></a>[|back|](#note-Mon-Feb-20-230609EST-2012) - My intuition tells me that long-poll/comet won't be a very good fit for Hunchentoot's view of the world, so I'll need to figure something out.

26 - <a name="foot-Mon-Feb-20-231049EST-2012"></a>[|back|](#note-Mon-Feb-20-231049EST-2012) -  It'll definitely be an open one, I'm just not sure which, though currently leaning towards [the AGPL](http://www.gnu.org/licenses/agpl.html) since the point of this is a hobby-horse/educational project. In other words, definitely hold off if you're a GNU hater.
