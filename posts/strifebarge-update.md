Before I get to the StrifeBarge update, I've been thinking about a problem. It's one I vaguely assumed had been definitively solved, but it turns out that's only a matter of perspective. Now that I really sit down to think about how I'd implement a "solution", it's quite obvious that there is no good way of countering a sufficiently long-running and elaborate [man-in-the-middle attack](http://en.wikipedia.org/wiki/Man-in-the-middle_attack).


1.   You can thwart mere eavesdroppers by using [DH](http://mathworld.wolfram.com/Diffie-HellmanProtocol.html) or similar
1.   You can counter short-term MitM attacks by relying on keys that you exchange with Alice before the attack started
1.   You can counter long-term MitM attacks by relying on some additional authentication channel like a phonecall, video conference or SMS chain
1.   You can counter elaborate, long-term MitM attacks by relying on a web of trust to confirm Alice's key
1.   You can counter elaborate, network-saturating, long-term MitM attacks by flying out and actually meeting Alice, exchanging keys in [both](http://en.wikipedia.org/wiki/Public-key_cryptography) [senses](http://www.tigerdirect.ca/applications/category/category_tlc.asp?CatId=379&name=USB%20Flash%20Drive)


I'm not entirely sure how you can counter supremely elaborate, long-term MitM attacks, but it probably involves several independent notaries that hate each other as well as Alice<a name="note-Sat-Mar-03-111456EST-2012"></a>[|1|](#foot-Sat-Mar-03-111456EST-2012), or possibly a DNA sample in addition to the key exchange. That list was cumulative, by the way, not independent.

Handshakes or public-key crypto get you past regular old surveillance. Both together can get you past an active attacker on the current line, but that's basically it. After that, any technique you try only buys you a bit of extra confidence that your messages aren't being tampered with, and that confidence approaches 100% without ever getting there. Certificate authorities can be compromised, networks of trust can be saturated by an attacker's agents<a name="note-Sat-Mar-03-111752EST-2012"></a>[|2|](#foot-Sat-Mar-03-111752EST-2012), and any other authentication mechanism I can think of can be faked by a sufficiently motivated attacker.

Now, the good news is that StrifeBarge isn't anything like a juicy enough target to tempt the Chinese government into trying something. The bad news is that it's never really possible to fully trust a given user. The best you can hope to do is maximize the chance that they're really who they say they are. From another perspective, the best you can do is make the resource expenditure required to fool your authentication greater than the potential payoff in succeeding. It's relevant, because you don't want an opponent to be able to peek over at your map during a game for some hopefully obvious reasons<a name="note-Sat-Mar-03-111908EST-2012"></a>[|3|](#foot-Sat-Mar-03-111908EST-2012). This really, truly looked like it should have been solved already, until I plucked it from the periphery of my thoughts and focused on it for a couple of days. Upon closer inspection, I'm not sure why I ever thought that.

With that, let's move on to the highlights.

The big change is that I've settled on a license. The intention is that this game should be out in public for educational purposes in case anyone actually cares, so the AGPL seems most appropriate<a name="note-Sat-Mar-03-201833EST-2012"></a>[|4|](#foot-Sat-Mar-03-201833EST-2012). I may also pull out the basic stuff into a separate turn-based-HTTP-game framework that I will release under MIT/BSD for people to more easily mess with. That's a long way off<a name="note-Sat-Mar-03-201845EST-2012"></a>[|5|](#foot-Sat-Mar-03-201845EST-2012), but I thought I'd mention it.

The big, *noticeable* change is that styles, images and js have been incorporated semi-well into the program. It's gone into space rather than the traditional naval setting. Collecting the sprites was easier than it should have been, by the way, and that's entirely thanks to [OpenGameArt](http://opengameart.org/). The star background is going to have to be replaced by something snazzier at some point, but the rest of the graphics are actually quite nice.

Part of integrating CSS included using [`cl-css`](https://github.com/Inaimathi/cl-css). I tried to avoid it, I really did, because I wanted you to be able to install StrifeBarge just by `ql:quickload`ing the single appropriate package name, but doing CSS3 directives manually sucks some of the biggest donkey testicles that exist on this earth. I decided that the ability to define functions like

```lisp
;;; css.lisp

(defun css-rotate (degrees)
  (let ((d (format nil "rotate(~adeg)" degrees)))
    `(:transform ,d
      :-ms-transform ,d
      :-webkit-transform ,d
      :-o-transform ,d
      :-moz-transform ,d)))
```

instead of manually writing this shit every time just about trumps ease of installation. Once the web stabilizes enough that the big browser writers don't feel the need to implement their own cutely named directives and ignore CSS3 equivalents, I'll ditch my CSS generator. Until then, I will happily debase myself for the ability to define higher level constructs which will deal with the equine testicle sucking on my behalf. I guess I could submit the library for consideration in [quicklisp](http://www.quicklisp.org/), but I really, really don't want to until I've ironed out that case-insensitivity issue, and possibly defined CSS3-abstracting-functions as part of the default library (though I'm still pretty sure cl-css will ever attempt to validate your input).

Finally, the default `mapcan` bit me, so I needed a replacement.

```lisp
;;; a/util.lisp
... 

(defun mapcan-f (fn a-list)
  "Functional unary mapcan"
  (loop for i in a-list append (funcall fn i)))

...
```

I estimate having spent a good twenty minutes or so puzzling over why `(ships a-player)` was suddenly returning a very odd list of elements once a game had started. The reason is that `mapcan` uses `nconc` to put its results together, which means that the arguments are going to be modified destructively. So I guess this is another place where [State Is Hard](http://langnostic.blogspot.com/2011/04/game-jam.html). I ended up defining the functional, unary `mapcan` seen above, which suffices for my purposes.

And on we go to the `diff`s. I don't actually expect anyone to read past this, by the way, but thinking about code I wrote well enough to explain it prosaically has proven to be a very effective technique<a name="note-Sat-Mar-03-202120EST-2012"></a>[|6|](#foot-Sat-Mar-03-202120EST-2012). Oh, also, before I forget, we've gone from 220 lines to about 550 (and that new count *doesn't* include the license info, generated css/js, or any of the images being tracked as binary files).

```diff
diff --git a/board.lisp b/board.lisp
index b18c662..b25c106 100644
--- a/board.lisp
+++ b/board.lisp
@@ -16,22 +16,39 @@
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmethod space-at ((b board) x y) (nth x (nth y (spaces b))))
 
+(defun create-point (direction x y i j)
+  (if (eq :vertical direction)
+      (list (+ i x) (+ j y))
+      (list (+ j x) (+ i y))))
+
 (defmethod assign-ship-spaces ((s ship) direction x y)
-  (loop for i from 0 to (- (space-count s) 1)
-       if (eq :vertical direction)
-         collect (cons x (+ i y))
-       else
-         collect (cons (+ i x) y)))
+  "Given a ship, a direction and an initial x/y, 
+returns a list of spaces that the ship will occupy."
+  (mapcan (lambda (i) 
+           (mapcar (lambda (j)
+                     (create-point direction x y i j)) 
+                   (range 0 (- (len s) 1))))
+         (range 0 (- (wid s) 1))))
 
 (defmethod position-ship ((s ship) (b board))
-  (let* ((x (random (- (width b) (space-count s))))
-        (y (random (- (height b) (space-count s))))
-        (direction (pick '(:vertical :horizontal)))
+  "Given a ship and a board, positions a ship on the board,
+ensuring there are no collisions."
+  (let* ((direction (pick '(:vertical :horizontal)))
+        (ship-v-padding (+ 1 (if (eq :vertical direction) (len s) (wid s))))
+        (ship-h-padding (+ 1 (if (eq :vertical direction) (wid s) (len s)))) 
+        (x (random (- (width b) ship-h-padding))) 
+        (y (random (- (height b) ship-v-padding)))
         (ship-spaces (assign-ship-spaces s direction x y)))
-    (if (every (lambda (p) (empty-space-at? b (car p) (cdr p))) ship-spaces)
-       (progn (setf (direction s) direction)
-              (loop for (space-x . space-y) in ship-spaces
-                    do (setf (contents (space-at b space-x space-y)) s)))
+    (if (every (lambda (p) (empty-space-at? b (car p) (cadr p))) ship-spaces)
+       (progn (setf (direction s) direction 
+                    (x s) x 
+                    (y s) y)
+              (loop for (space-x space-y) in ship-spaces
+                    do (let ((current-space (space-at b space-x space-y))) 
+                         (setf (contents current-space) s))))
        (position-ship s b))))
 
 (defun make-board (list-of-ships)
```

I didn't do a *complete* overhaul of the ship placement routines, but I got pretty close. I abstracted point creation a bit and replaced `loop` with a nested `mapcan`/`mapcar` on the results of the `range` utility function I mentioned last time. That's back by the way. `mapcan` doesn't cause any trouble here because `range` creates an entirely new list, which means that there are no outside references that might get tripped up by `nconc`. Also, the reason for nesting iterations in the `assign-ship-spaces` function is that I'm now generating ships wider than one square<a name="note-Sat-Mar-03-202239EST-2012"></a>[|7|](#foot-Sat-Mar-03-202239EST-2012).

```diff
@@ -44,8 +61,21 @@
 ;;;;;;;;;;;;;;;;;;;; display
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defmethod echo ((b board) (p player))
-  (with-html-output (*standard-output* nil :indent t)
-    (:table :id "game-board"
-           (mapc (lambda (row) 
-                   (htm (:tr (mapc (lambda (s) (echo s p)) row)))) 
-                 (spaces b)))))

+  (html-to-stout
+    (:div :id "board-wrapper"
+         (loop for s in (ships p)
+               do (str (echo s p)))
+         (:table :id "game-board"
+                 (mapc (lambda (row) 
+                         (htm (:tr (mapc (lambda (s) (echo s p)) row)))) 
+                       (spaces b))))))
+
+(defmethod image-file ((s ship)) (format nil "/img/ships/~(~a~).png" (type-of s)))
+
+(defmethod echo ((s ship) (p player))
+  (let ((direction (direction s)))
+    (html-to-str (:img :class "ship" 
+                      :style (inline-css `(:left ,(css-left s) :top ,(px (board-scale (y s)))
+                                           :width ,(board-scale (len s)) :height ,(board-scale (wid s))
+                                           ,@(when (eq :vertical direction) (css-rotate 90))))
+                      :src (image-file s)))))
```

The rest of the additions here just deal with outputting the appropriate html and styles for `ship`s and `board`s; that's the price you pay for choosing a more pleasing visual representation than just ascii characters in a table. There's enough `ship`-specific code at this point for me to consider pulling it out into its own file, but not really enough to actually force me to do it yet. Also, I get the feeling I should be referring to these as "barges" instead. In fact, that's two mental notes for after the write-up.

```lisp
;;; css.lisp
(in-package #:strifebarge)

(defun css-space-size ()
    (let ((d (format nil "~apx" *board-square-size*)))
      `(:width ,d :height ,d)))

(defmacro css-transform-origin (x y)
  (let ((d (format nil "~a ~a" x y)))
    `'(:transform-origin ,d
       :-ms-transform-origin ,d
       :-webkit-transform-origin ,d
       :-moz-transform-origin ,d
       :-o-transform-origin ,d)))

(defun css-rotate (degrees)
  (let ((d (format nil "rotate(~adeg)" degrees)))
    `(:transform ,d
      :-ms-transform ,d
      :-webkit-transform ,d
      :-o-transform ,d
      :-moz-transform ,d)))

(defun css-scale (scale-factor)
  (let ((d (format nil "scale(~a,~a)" scale-factor scale-factor)))
    `(:transform ,d
      :-ms-transform ,d
      :-webkit-transform ,d
      :-o-transform ,d
      :-moz-transform ,d)))

(defun px (num) (format nil "~apx" num))

(defmethod css-left ((s ship))
  (px (if (eq :vertical (direction s))
          (board-scale (+ (x s) (wid s)))
          (board-scale (x s)))))

(compile-css "css/strifebarge.css"
             `((body :background-color \#000 :background-image "url(/img/galaxy.png)" :padding 0px :margin 0px :font-family sans-serif)

               (.menu-item :padding 10px :background-color \#eee :font-size large :font-weight bolder)
               ("#player-console .menu-item" :background-color transparent)

               (\#board-wrapper :position absolute)
               (\#game-board :border-spacing 0px :color \#fff)
               ("#game-board .miss" :font-family courier :font-size x-small)
               ("#game-board .hit" ,@(css-space-size))
               ("#game-board td" ,@(css-space-size) :padding 0px)
               ("#game-board .shot-link" :height 100% :width 100% :display block)
               ("#game-board .shot-link:hover" :background-position center :border none :background-image "url(/img/crosshairs/crosshair9.png)")

               (\#player-console :float right :width 110px :margin ,(px *board-square-size*) :padding 5px :background-color \#eee)
               (.ship-stats :margin-bottom 15px)
               (".ship-stats img" :width 60px :margin "5px 20px")
               (".ship-stats .total-hp" :height 25px :background-color red :width 100px :border "2px solid black")
               (".ship-stats .hp-remaining" :height 25px :background-color green :font-weight bold)

               (.ship ,@(css-transform-origin 0 0) :position absolute :z-index -10000)))
```

We'll dispense with the `diff` here because `css.lisp` is an entirely new file. This is basically just how I like to write CSS in Lisp projects. The library I'm using is cl-css; a lightweight, non-validating CSS generator of my own devising. Use it if you like, but remember that it does downcase everything<a name="note-Sat-Mar-03-202411EST-2012"></a>[|8|](#foot-Sat-Mar-03-202411EST-2012) you pass it at the moment, so you may have a rough time using it with certain JS libraries that insist on using `CamelCase` or `snakeCase` rather than `lisp-case` for their DOM classes. As mentioned, the main reason I resorted to this can be seen in the `css-scale`, `css-rotate` and `css-transform-origin` macros, which just paper over the fact that browser developers don't quite seem to be cooperating yet.

```diff
diff --git a/game.lisp b/game.lisp
index 476baa5..c4c82e6 100644
--- a/game.lisp
+++ b/game.lisp
@@ -9,15 +9,48 @@
     p))
 
 (defun make-game (&rest players)
-  (let ((board (make-board (mapcan #'ships players))))
+  (let ((board (make-board (mapcan-f #'ships players))))
     (make-instance 'game :board board :players players :waiting-for players :turn-stack players)))
```

This is where that `mapcan` bug bit. As you can see, it's been replaced by `mapcan-f` which we've already gone over.
 
```diff
+;;;;;;;;;;;;;;;;;;;; predicates
+;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
+(defmethod turn-p ((g game) &optional (player (session-value :player))) 
+  (eq (car (turn-stack g)) player))
 
 ;;;;;;;;;;;;;;;;;;;; display
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
-(defmethod echo ((g game) (p player))
-  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
-    (:html (:body (echo (board g) p)))))
+(defmethod echo ((g game) (p player)) (echo (board g) p))
+
+(defmethod echo-console ((g game) (p player))
+  (html-to-stout (:div :id "player-console" 
+                      (:div :id "turn-marker" (str (if (turn-p *game*) "Your turn" "Their turn")))
+                      (echo-stats p)
+                      (:a :class "menu-item" :href "/quit-game" "Quit Game"))))
+
+(defmethod echo-stats ((p player))
+  (html-to-stout 
+    (:div :class "player-ships"
+         (loop for s in (ships p)
+               do (str (echo-stats s))))))
+
+(defmethod echo-stats ((s ship))
+  (html-to-str
+    (:div :id (instance-to-id s) :class "ship-stats" (:img :src (image-file s))
+         (htm (:div :class "total-hp" 
+                    (:div :class "hp-remaining" 
+                          :style (inline-css `(:width ,(format nil "~a%" (hp-% s))))
+                          (:span :class "num-hp" (str (remaining-hp s))) "/" (:span :class "num-total-hp" (str (hp s)))))))))
+
+(defmethod remaining-hp ((s ship))
+  (- (space-count s) (damage s)))
+
+(defmethod hp ((s ship)) (space-count s))
+
+(defmethod hp-% ((s ship))
+  (round (* 100 (/ (remaining-hp s) (space-count s)))))
+
+(defmethod to-json ((s ship))
+  (encode-json-to-string `((ship-id . ,(instance-to-id s)) (hp . ,(remaining-hp s)) (percent . ,(hp-% s)))))

``` 

Again, most of the additions here have to do with generating html/json representations of classes we've already gone over. The semi-interesting bits are the `ship` `hp` calculations (which would be good candidates for moving out into that `ship`-specific file I mentioned thinking about). `instance-to-id` is something we'll go over later because I'm unsure of its implementation. The point is to take an instance and return a string suitable for use as a CSS id while being ambiguous enough that a player can't get too much information from it<a name="note-Sat-Mar-03-202709EST-2012"></a>[|9|](#foot-Sat-Mar-03-202709EST-2012).

```diff
@@ -27,9 +60,15 @@
       (setf (turn-stack g) (players g))))
 
 (defmethod fire ((g game) (p player) x y)
-  (let ((result (make-instance 
-                (if (empty-space-at? (board g) x y) 'miss 'hit)
-                :player p :x x :y y)))
-    (push result (history g))
-    (setf (move (space-at (board g) x y)) result)
+  (let* ((space (space-at (board g) x y))
+        (result (make-instance 
+                 (if (empty-space? space) 'miss 'hit)
+                 :player p :x x :y y)))
+    (push-record g "shot" (to-json result))
+    (unless (empty-space? space)
+      (let ((ship (contents space)))
+       (setf (ship result) ship)
+       (incf (damage ship))
+       (push-record g "ship-damage" (to-json ship))))
+    (setf (move space) result)
     result))
```

`fire` has changed pretty substantially. First off, it's now making calls to `push-record` (which we'll go over in the next file), and it's also applying damage to the stricken `ship` in addition to placing the shot marker on the `board` as before. The new stuff is all happening in that `unless` block.

```lisp
;;; history-event.lisp
(in-package :strifebarge)

;;;;;;;;;;;;;;;;;;;; creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod to-json ((m move))
  (encode-json-to-string `((x . ,(x m)) (y . ,(y m)) 
                           (text . ,(echo m (session-value :player))))))

(defmethod push-record ((g game) event-type message)
  (push (make-instance 'history-event
                       :id (length (history g))
                       :event-type event-type
                       :message message)
        (history g)))

;;;;;;;;;;;;;;;;;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod emit-record ((g game) (p player))
  (apply #'concatenate 
         (cons 'string 
               (mapcar (lambda (r) (emit-record r p)) 
                       (reverse (take 10 (history g)))))))

(defmethod emit-record ((e history-event) (p player))
  (format nil "id: ~a~%event: ~a~%data: ~a~%~%"
          (id e) (event-type e) (message e)))
```

This is another new file. The interactions involving `game` history were getting complex enough that I wanted them on their own, but I ended up refactoring a lot of it out, leaving this rather sparse file. This stuff deals with the information we'll be passing back to the clients in-flight. To that end, I've defined `history-event` as its own class, and made sure that it handles `echo`s properly. `push-record` is also a shortcut method you've already seen used. All of this could fit comfortable in the `game` file, and it probably will be moved before the next check-in.

```lisp
;;; js.lisp
(in-package :strifebarge)

(compile-js "js/strifebarge.js" "strifebarge-js.lisp"
            (ps 
              (define-event-source source "update-map")

              (define-event-listener source "turn"
                (lambda (e) ($ "#turn-marker" (text (chain e data)))))

              (define-event-listener source "ship-damage"
                (lambda (e)
                  (let* ((d (parse-json (chain e data))) 
                         (ship-id (+ "#" (@ d "shipId"))))
                    ($ ship-id (find ".num-hp") (text (@ d "hp")))
                    ($ ship-id (find ".hp-remaining") (width (+ (@ d "percent") "%"))))))

              (define-event-listener source "shot"
                (lambda (e) 
                  (let ((d (parse-json (chain e data))))
                    ($-space-at ((@ d "x") (@ d "y")) (html (@ d "text"))))))

              (defun send-shot (x y)
                (post-to "/turn" 
                         (create :x x :y y) 
                         (lambda (data)
                           ($-space-at (x y) (html data))
                           ($ "#turn-marker" (text "Their Turn")))))))
```

Another entirely new file. This is what [`parenscript`](http://common-lisp.net/project/parenscript/) tends to look like if you use it with [jQuery](http://jquery.com/). Most of this deals with the server-sent-event stuff we need to send incremental, on-the-fly updates to the client. Particularly note the `define-event-listener`s and `define-event-source`. Neither are primitives, and we'll discuss implications in a moment. The only other thing we've got here is `send-shot`, which is actually an ajax call to `/turn`. It doesn't bother doing anything if it's not your turn (defined implicitly by the `assert`s we saw last time), but places the shot flag and updates the turn marker when needed.


```lisp
;;; js-macros.lisp
(in-package :strifebarge)

(defun compile-js (file-name origin js)
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "//////////~%// This is a generated file. ~%// If you want to edit this javascript, tweak '~a' and re-evaluate it instead.~%//////////~%~%" origin)
    (format stream js)))

;;;;;;;;;;;;;;; basic shortcuts
(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro fn (&body body) `(lambda () ,@body))

(defpsmacro doc-ready (&body body) 
  `($ document (ready (fn ,@body))))

(defpsmacro parse-json (target)
  `(chain j-query (parse-j-s-o-n ,target)))

(defpsmacro post-to (target-page data-hash on-success)
  "target-page is a page url.
data-hash is the data sent along as the post request; declared as (create :k v ...)
on-success is a function to run on a successful response; 
it should expect a single argument (the data returned by the target handler)"
  `(chain $ (post ,target-page
                  ,data-hash
                  ,on-success)))

;;;;;;;;;;;;;;; SSE specific
(defpsmacro define-event-source (name source-url)
  `(defvar ,name (new (-event-source ,source-url))))

(defpsmacro define-event-listener (event-source-name message-type on-receive)
  "event-source-name must be a defined event source. 
message-type is the event label sent by the server for this action (the default label is 'message').
on-receive is a function called when a satisfying message is received. It should take one argument (the event)."
  `(chain ,event-source-name (add-event-listener ,message-type ,on-receive false)))

;;;;;;;;;;;;;;; StrifeBarge specific
(defpsmacro $-space-at ((x y) &rest chains)
  `($ "#game-board tr" (eq ,y) (children "td") (eq ,x) ,@chains))
```

This one's... a bit complicated. I went a bit heavy on the comments for exactly that reason. `compile-js` is just responsible for generating `js` files from `parenscript` code. The next few macros are jQuery-oriented shortcuts that I've [already discussed](http://langnostic.blogspot.com/2011/03/javascript-with-lisp.html) here<a name="note-Sat-Mar-03-203059EST-2012"></a>[|10|](#foot-Sat-Mar-03-203059EST-2012). The `define` pair let me specify SSE feed sources and event handlers much more simply than I could in vanilla JS. `$-space-at` just lets me shortcut the selection of a map square.

```diff
diff --git a/model.lisp b/model.lisp
index 0ee1685..f6bb48c 100644
--- a/model.lisp
+++ b/model.lisp
@@ -1,23 +1,32 @@
 (in-package :strifebarge)
 
 (defclass ship ()
-  ((space-count :reader space-count :initarg :space-count)
+  ((wid :reader wid :initarg :wid :initform 1)
+   (len :reader len :initarg :len)
+   (x :accessor x :initarg :x)
+   (y :accessor y :initarg :y)
+   (space-count :accessor space-count :initarg :space-count)
    (player :reader player :initarg :player)
    (damage :accessor damage :initform 0)
    (direction :accessor direction :initarg :direction)))
 
-(defclass carrier (ship) ((space-count :initform 5)))
-(defclass cruiser (ship) ((space-count :initform 3)))
-(defclass destroyer (ship) ((space-count :initform 2)))
+(define-ship carrier 5 2)
+(define-ship cruiser 3)
+(define-ship destroyer 2)
 
 (defclass move ()
   ((player :reader player :initarg :player)
    (x :reader x :initarg :x)
    (y :reader y :initarg :y)))
 
-(defclass hit (move) ())
+(defclass hit (move) ((ship :accessor ship :initarg :ship)))
 (defclass miss (move) ())
 
+(defclass history-event ()
+  ((id :reader id :initarg :id)
+   (event-type :reader event-type :initarg :event-type)
+   (message :reader message :initarg :message)))
+
 (defclass player ()
   ((score :accessor score :initform 0)
    (sunken :accessor sunken :initarg :sunken)
```

Back into merely modified files. As you can see, ship definitions got shorter (we'll see the macro behind that in the `util` file), `history-event`s became their own explicitly defined objects, and `ship` got a few new slots to make wider pieces possible. Incidentally, if you look at those new `ship` slots, you'll see one of the CLOS speedbumps that [Yegge whinged about quite a while ago](http://steve-yegge.blogspot.com/2006/04/lisp-is-not-acceptable-lisp.html). `length` is a polymorphic function and not a method. Meaning that if you have a non-sequence `foo` that would benefit from having a `length` method, you either need to name it `foo-length` or `len` or some other annoyingly minute variation of the word. I went with `len` and have no particular regrets there (other than not actually being able to [use `length` as a method](http://langnostic.blogspot.com/2011/11/objective-lisp.html)).

```diff
diff --git a/package.lisp b/package.lisp
index 94e808f..57ab4e9 100644
--- a/package.lisp
+++ b/package.lisp
@@ -1,8 +1,9 @@
 ;;;; package.lisp
 
 (defpackage #:strifebarge
-  (:use #:cl #:cl-who #:clsql #:hunchentoot #:parenscript)
-  (:import-from #:swank #:find-definition-for-thing)
+  (:use #:cl #:cl-who #:cl-css #:clsql #:hunchentoot #:parenscript)
+  (:import-from #:json #:encode-json-to-string #:decode-json-from-string)
+  (:import-from #:cl-ppcre #:scan-to-strings)
   (:import-from #:ironclad 
                #:encrypt-in-place #:decrypt-in-place #:make-cipher #:digest-sequence 
                #:octets-to-integer #:integer-to-octets
@@ -11,4 +12,8 @@
 
 (in-package #:strifebarge)
 
-(defparameter *web-server* (start (make-instance 'hunchentoot:easy-acceptor :port 5050)))
\ No newline at end of file
+;;;;;;;;;;;;;;;;;;;; config variable
+
+(defparameter *server-port* 5050)
+(defparameter *board-square-size* 35)
+;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```

Not much new to see here. I've imported `cl-css`, as well as choice functions from `cl-ppcre` and `:json`. Still importing `:ironclad` chunklets, and still haven't put them to use. I did pull out the server port number and board square size into their own variables though. Thanks to my use of `:parenscript` and `:cl-css`, that single parameter should be all you need to change in order to mess with square size. The server startup, as well as a couple of related statements were moved out to a separate `start` file.

```lisp
;;; start.lisp
(in-package :strifebarge)

(defparameter *web-server* (start (make-instance 'hunchentoot:easy-acceptor :port *server-port*)))
(web-folders "js" "img" "css")
```

And here it is. The `defparameter` line starts a new `hunchentoot` instance listening on `*server-port*`, and `web-folders` does exactly what you'd expect (we'll discuss it in very slightly more detail in the `util` file).
 
```diff
diff --git a/strifebarge.lisp b/strifebarge.lisp
index d651ba9..2630341 100644
--- a/strifebarge.lisp
+++ b/strifebarge.lisp
@@ -4,37 +4,57 @@
 
 (defparameter *game* nil)
 
+;;;;;;;;;;;;;;;;;;;; full handlers
+;;; (all of these either directly return, or redirect to complete pages)
+
 (define-easy-handler (index :uri "/") ()
-  (let ((players (list (make-player 'carrier 'cruiser 'destroyer)
-                      (make-player 'carrier 'cruiser 'destroyer))))
-    (echo (apply #'make-game players) (car players))))
+  (html-to-str
+    (:html (:head (:title "StrifeBarge")
+                 (:link :rel "stylesheet" :type "text/css" :href "/css/strifebarge.css"))
+          (:body (:a :class "menu-item" :href "/new-game" "New Game")
+                 (:a :class "menu-item" :href "/join-game" "Join Game")))))
 
 (define-easy-handler (new-game :uri "/new-game") (player-count)
   (let* ((p-count (if player-count (parse-integer player-count) 2)) 
-        (players (loop for i from 1 to p-count
-                       collect (make-player 'carrier 'cruiser 'destroyer))))
+        (players (loop repeat p-count collect (make-player 'carrier 'cruiser 'destroyer))))
     (setf *game* (apply #'make-game players))
     (redirect "/join-game")))
 
 (define-easy-handler (join-game :uri "/join-game") ()
-  (assert (and (not (null (waiting-for *game*)))
-              (null (session-value :player))))
+  (redirect-unless (and (not (null (waiting-for *game*)))
+                   (null (session-value :player))))
   (setf (session-value :player) (pop (waiting-for *game*)))
   (redirect "/show-game"))
 
 (define-easy-handler (show-game :uri "/show-game") ()
-  (assert (not (null (session-value :player))))
-  (echo *game* (session-value :player)))
+  (redirect-unless (not (null (session-value :player))))
+  (html-to-str
+    (:html (:head
+           (:title "StrifeBarge")
+           (:script :type "text/javascript" :src "/js/jquery-1.7.1.min.js")
+           (:script :type "text/javascript" :src "/js/strifebarge.js")
+           (:link :rel "stylesheet" :type "text/css" :href "/css/strifebarge.css"))
+          (:body (echo-console *game* (session-value :player)) 
+                 (echo *game* (session-value :player))))))
 
 (define-easy-handler (quit-game :uri "/quit-game") ()
-  (assert (not (null (session-value :player))))
+  (redirect-unless (not (null (session-value :player))))
   (push (waiting-for *game*) (session-value :player))
   (setf (session-value :player) nil)
   "You have quit the game")
 
+;;;;;;;;;;;;;;;;;;;; ajax handlers
+;;; these return either errors or partial json/html. 
+;;; The caller is expected to transform their output before final display
+
+(define-easy-handler (update-map :uri "/update-map") ()
+  (assert (not (null (session-value :player))))
+  (setf (header-out :cache-control) "no-cache"
+       (content-type*) "text/event-stream")
+  (emit-record *game* (session-value :player)))
+
 (define-easy-handler (turn :uri "/turn") (x y)
-  (assert (and (eq (car (turn-stack *game*)) (session-value :player))
-              (stringp x) (stringp y)))
+  (assert (and (turn-p *game*) (stringp x) (stringp y)))
   (advance-turn *game*)
-  (fire *game* (session-value :player) (parse-integer x) (parse-integer y))
-  (redirect "/show-game"))

+  (echo (fire *game* (session-value :player) (parse-integer x) (parse-integer y)) 
+       (session-value :player)))
```

This is possibly the most changed file at the moment. Firstly, we've added an ajax handler for player actions. Second, `update-map` shows you exactly what you need to do on the server to set up a Server-Sent-Event source that the client will actually support. `emit-record` was shown earlier, but I didn't really explain it. It still takes a `player` as an argument, because the record digest used to be subjective<a name="note-Sat-Mar-03-203328EST-2012"></a>[|11|](#foot-Sat-Mar-03-203328EST-2012). In the non-ajax handlers, note that the various `assert`s have all been replaced with equivalent `redirect-unless` calls. That's a new utility function that does exactly what you'd think. It may seem to be more complex than necessary at this point, but I will eventually shunt the player off into different pages depending on where they came from (you know, improving signage and all that).

```diff
diff --git a/util.lisp b/util.lisp
index 76f62b4..1de23d8 100644
--- a/util.lisp
+++ b/util.lisp
@@ -2,4 +2,44 @@
 
 (defun pick (a-list)
   "Randomly selects an element from the given list with equal probability."
-  (nth (random (length a-list)) a-list))
\ No newline at end of file
+  (nth (random (length a-list)) a-list))
+
+(defun range (a b)
+  (loop for i from a to b collect i))
+
+(defun board-scale (num) (* *board-square-size* num))
+
+(defun mapcan-f (fn a-list)
+  "Functional implementation of unary mapcan"
+  (loop for i in a-list append (funcall fn i)))
+
+(defmacro web-folders (&body body)
+  "Sets up folder dispatchers for the given folders"
+  `(progn ,@(mapcar #'(lambda (f) 
+                       `(push (create-folder-dispatcher-and-handler ,(format nil "/~a/" f) ,(format nil "~a/" f)) *dispatch-table*))
+                   body)))
+
+(defmacro redirect-unless (predicate &optional (target "/"))
+  `(unless ,predicate (redirect ,target)))
+
+(defmacro html-to-stout (&body body)
+  "Outputs HTML to standard out."
+  `(with-html-output (*standard-output* nil :indent t) ,@body))
+
+(defmacro html-to-str (&body body)
+  "Returns HTML as a string, as well as printing to standard-out"
+  `(with-html-output-to-string (*standard-output*) ,@body))
+
+(defun instance-to-id (instance)
+  (aref (nth-value 1 (scan-to-strings "{\(.*?\)}" (format nil "~a" instance))) 0))
+
+(defun take (num a-list)
+  (if (> (length a-list) num)
+      (subseq a-list 0 num)
+      a-list))
+
+(defmacro define-ship (name length &optional (width 1))
+  `(defclass ,name (ship) 
+     ((len :initform ,length)
+      (wid :initform ,width)
+      (space-count :initform ,(* length width)))))
\ No newline at end of file
```

Finally, `util` has grown quite a bit. Range has returned after being removed temporarily. We've already gone through `mapcan-f` and `redirect-unless`. `take` is a naive definition of the [equivalent function](http://docs.racket-lang.org/reference/pairs.html?q=take#(def._((lib._racket/list..rkt)._take))) from Scheme, `define-ship` is a minimal shortcut for cutting out ship definition boilerplate now that ships are two-dimensional, and the `html-to-` twins are the thinnest possible useful wrappers around the equivalent [`with-html-output`](http://weitz.de/cl-who/#with-html-output) from `:cl-who`. `web-folders` is a shortcut that makes it easier to have `hunchentoot` serve folders. Its operation should be obvious if you know how [this normally works](http://weitz.de/hunchentoot/#create-folder-dispatcher-and-handler); the only note I'll make is that if you were to actually run StrifeBarge in production for some bizarre reason, you'd actually want to set up `nginx` or some other lightweight HTTP server out front to serve these static directories instead of having your Lisp process handle everything.

The only really interesting piece here is `instance-to-id`, which takes a CLOS instance and returns a sufficiently-ambiguous string mostly useful as a DOM id. I initially had this implemented as

```lisp
(regex-replace-all "[#&lt;>{} ]" (format nil "~a" instance))
```

instead, but found that a player can get too much information from that (though it did perform marginally better in profiling reports). Running it on a carrier would produce an id like `CARRIER100AABD943`, from which it's trivial to find out what kind of ship just got hit. The actual definition above instead returns `100AABD943` in the same situation. This is specific enough to unambiguously identify the DOM element for any JS function we need invoked, but it's general enough that a player probably can't get any useful, new information just by pulling down `update-map` and reading the output manually. I may change my mind on this later, by the by, since I'm already taking quite a few liberties with the rules, but it stays for now.

So that's *almost* that for the game itself. I still need to make `ship`s send out a "You sunk my..." message when they get taken down, and there's a few usability-related things I'd like to add to the client side (ok, yes, and there needs to be support for multiple games at once, as well as actually winning), but most of the remaining development actually needs to happen outside the game itself. I hinted at it earlier, but just to make it explicit, I still need to implement


-   A chat-room/lobby for people to start games up from (in-game chat might be nice too)
-   A leaderboard system to show off high scores (possibly with replays too)
-   A formalized way of automating games


Sounds like this'll actually keep me busier than I expected.

* * *
##### Footnotes
1 - <a name="foot-Sat-Mar-03-111456EST-2012"></a>[|back|](#note-Sat-Mar-03-111456EST-2012) - That'd be a meatspace, belligerent network of trust. The idea being that if you can get n people who certainly aren't cooperating to vouch for Alice face-to-face, your odds are better than if you merely had n of Alice's random online social contacts do the same.

2 - <a name="foot-Sat-Mar-03-111752EST-2012"></a>[|back|](#note-Sat-Mar-03-111752EST-2012) - And don't really help you much in the general case, unless you're assuming that you'll always be able to trace a line from your direct contacts to the other party, or you're assuming belligerence.

3 - <a name="foot-Sat-Mar-03-111908EST-2012"></a>[|back|](#note-Sat-Mar-03-111908EST-2012) - I have to be honest though; preventing people from cheating in a hobby-horse project of mine isn't why I was researching this. I'm doing some development at work that will need me to build at least reasonably secure components, so I'm beefing up on the basics of [crypto](http://crypto.stackexchange.com/) and computer security.

4 - <a name="foot-Sat-Mar-03-201833EST-2012"></a>[|back|](#note-Sat-Mar-03-201833EST-2012) - It also lets me use various GPL licensed sprites.

5 - <a name="foot-Sat-Mar-03-201845EST-2012"></a>[|back|](#note-Sat-Mar-03-201845EST-2012) - I still need to implement multiple `game`s per server, some sort of lobby/leaderboard system, and some way of actually winning before I think about implying that I've solved most game-related problems. Even for something as simple as this.

6 - <a name="foot-Sat-Mar-03-202120EST-2012"></a>[|back|](#note-Sat-Mar-03-202120EST-2012) - I'm actually giving more and more thought to the [literate programming](http://en.wikipedia.org/wiki/Literate_programming) idea for specifically this reason.

7 - <a name="foot-Sat-Mar-03-202239EST-2012"></a>[|back|](#note-Sat-Mar-03-202239EST-2012) - There really isn't a technical reason for that; I took a look at the [spaceship sprite set](http://opengameart.org/content/spaceships-top-down)s at OpenGameArt and thought it would be a shame not to be able to use some of the oddly-shaped ones.

8 - <a name="foot-Sat-Mar-03-202411EST-2012"></a>[|back|](#note-Sat-Mar-03-202411EST-2012) - No longer actually true; [the version up at github](https://github.com/Inaimathi/cl-css) currently lets you preserver case on selectors by passing them in as strings, and it incorporates the CSS3 transformation abstractions from this project. I'll probably be adding animation and transition to the pile shortly.

9 - <a name="foot-Sat-Mar-03-202709EST-2012"></a>[|back|](#note-Sat-Mar-03-202709EST-2012) - Specifically, to stick to the classic game as closely as possible, it shouldn't be possible for a player to identify what kind of ship they hit until they either sink it, or deduce it from the number of squares it occupies.

10 - <a name="foot-Sat-Mar-03-203059EST-2012"></a>[|back|](#note-Sat-Mar-03-203059EST-2012) - Though I did start calling the anonymous function shortcut `fn` rather than `\` for ease of exporting.

11 - <a name="foot-Sat-Mar-03-203328EST-2012"></a>[|back|](#note-Sat-Mar-03-203328EST-2012) - It's not at the moment, but this is something I won't be changing in the short term, since it will become true again once I implement a chat client.
