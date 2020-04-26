```lisp
(in-package #:cl-pestilence)

;;   This is _not_ a simulation. It's just a game. And any resemblance
;; to any world, real or imaginary, is entirely coincidental.
;;   I've been thinking a lot about asymmetric multiplayer games and
;; <gestures wildly to world at large> all this.
;; I'm not actively _trying_ to model it accurately, but it's probably
;; obvious what's been consuming my thoughts lately.

;;   Lets get right into this. I'll explain as I go, and tie a few things
;; together neatly at the end. I hope. Regardless, there will absolutely
;; be a repo sometime fairly soon.

;; A place can be tagged arbitrarily, and can contain occupants.
;; They also collect points.

(defclass place ()
  ((tags :initarg :tags :initform nil :accessor tags)
   (occupants :initarg :occupants :initform nil :accessor occupants)
   (points :initform 0 :accessor points)))

(defun place? (thing)
  (eq (find-class 'place) (class-of thing)))

(defun place (&key tags occupants)
  (make-instance 'place :tags tags :occupants occupants))

(defun gen-place ()
  (let ((tag (pick '(:apartment-building :house :cottage
		     :office-building :factory :store
		     :cafe :lounge :theater))))
    (place :tags (list tag))))

(defmethod details ((place place))
  (format nil "====================~%~a {~{~a~}}~%~{  ~a~^~%~}~%"
	  (first (tags place))
	  (rest (tags place))
	  (mapcar #'details (occupants place))))

(defmethod show ((place place))
  (format nil "~20@a ~5a [~{~a~}]~%"
	  (first (tags place)) (points place)
	  (mapcar #'show (occupants place))))

;; A peep goes places.
;; They have
;;  - their daily routine (a list of places to visit)
;;  - their todo (the part of their routine they still need to do;
;;                they are currently at the first place in this list)
;;  - their health (a number from 0 to 100)
;;  - a list of plagues
;; Finally, they _also_ collect points.
(defclass peep ()
  ((routine :initarg :routine :initform (list) :accessor routine)
   (todo :initarg :todo :initform nil :accessor todo)
   (health :initarg :health :initform 100 :accessor health)
   (plagues :initform nil :accessor plagues)
   (points :initform 0 :accessor points)))

(defun peep? (thing)
  (eq (find-class 'peep) (class-of thing)))

(defun peep (&key places)
  (make-instance 'peep :routine places :todo places))

(defun health->string (health)
  (cond ((>= health 90) "@")
	((>= health 80) "0")
	((>= health 70) "O")
	((>= health 50) "o")
	((>= health 30) ":")
	((>= health 1)  ".")
	(t "☠")))

(defmethod details ((peep peep))
  (format nil "[~a ~3d [~{ ~a~^ ->~}]]"
	  (health->string (health peep)) (health peep)
	  (mapcar
	   (lambda (place) (first (tags place)))
	   (routine peep))))

(defmethod show ((peep peep)) (health->string (health peep)))

;; A world is a list of places, occupied by peeps. This worl
(defun gen-world (&key (num-places 20) (num-peeps 100))
  (let ((places (loop repeat num-places collect (gen-place))))
    (loop repeat num-peeps
       do (let* ((routine (loop repeat 5 collect (pick places)))
		 (peep (peep :places routine)))
	    (push peep (occupants (first routine)))))
    places))

(defmethod details ((world list))
  (format nil "~%~{~a~}~%" (mapcar #'details world)))

(defmethod show ((world list))
  (format nil "~%~{~a~}~%" (mapcar #'show world)))

(defmethod all-peeps ((world list))
  (loop for place in world append (all-peeps place)))

(defmethod all-peeps ((place place))
  (loop for o in (occupants place) if (peep? o) collect o))

;; `tick!`ing a world means moving every peep through their routine once.
;;   We `tick!` each peep, then `tick!` each place until all the peeps are
;; done. Then we reset their routines.
;; You can think of this as a turn in the game.

(defmethod tick! ((world list))
  (let ((peeps (all-peeps world)))
    (loop while peeps
       do (setf peeps
		(loop for p = (pop peeps) while p
		   for res = (tick! p)
		   if res collect res))
       do (mapc #'tick! world)
       do (format t "~a" (show world)))
    (loop for p in (all-peeps world)
       do (setf (todo p) (routine p))))
  world)

;; Don't worry about the details of how to `tick!` peeps or places yet

;;   Ok, here's where it gets a bit darker. Although, we _did_ foreshadow this in
;; the definition of `peep`. And also, sort of in the title of the accompanying
;; blog post.

;; A plague is another living thing.
;; It has
;;  - a host (a peep that it's infecting)
;;  - a signature (a token representing its' lineage and strain)
;;  - health (how well it's doing inside its host)
;;  - virulence (how likely it is to spread to another host)
;;  - efficiency (how efficient they are at feeding)
;;  - reproduce (a function that returns a new instance to push into a new host)
;;  - and a strategy (a function, possibly closed, that takes
;;    itself and its host peep and mutates
;; Plagues do not collect points; they score differently.

(defclass plague ()
  ((host :initarg :host :initform nil :accessor host)
   (signature :initarg :host :initform "SIG" :accessor signature)
   (health :initarg :health :initform 10 :accessor health)
   (virulence :initarg :virulence :initform 10 :accessor virulence)
   (efficiency :initarg :efficiency :initform 0.2 :accessor efficiency)
   (reproduce
    :initarg :reproduce
    :initform
    #'plague
    :reader reproduce)
   (strategy
    :initarg :strategy
    :initform
    (lambda (plague peep)
      (feed! plague peep 30))
    :reader strategy)))

(defun plague ()
  (make-instance 'plague))

;; Plagues can `feed!` on peeps or plagues. To feed means to
;; take away some of the targets' health and add some to your own.
(defmethod feed! ((self plague) (peep peep) (amount integer))
  (decf (health peep) amount)
  (incf (health self) (* (efficiency self) amount)))

(defmethod feed! ((self plague) (plague plague) (amount integer))
  (decf (health plague) amount)
  (incf (health self) (* (efficiency self) amount)))

;; Plagues can also `infect!` peeps by `reproduce`ing into them.
(defmethod infect! ((self plague) (peep peep))
  (unless (infected-by? self peep)
    (let ((child (funcall (reproduce self))))
      (setf (host child) peep)
      (push child (plagues peep)))))

(defmethod infected-by? ((self plague) (peep peep))
  (member (signature self) (mapcar #'signature (plagues peep))
	  :test #'string=))

;;  `tick!`ing a plague causes it to weaken and also carry out its strategy.
;; This models the background effect of the immune system of its' host.
(defmethod tick! ((plague plague))
  (decf (health plague) 1)
  (funcall (strategy plague) plague (host plague))
  plague)

;;  Ticking a peep means moving them to their next place, and also ticking
;; any plagues they may have contracted. Also, peeps are resilient; they heal
;; a small amount each time they tick (to a maximum of 100)
;;  If a peep dies, they no longer move. And their plagues probably won't do well.
;; Peeps like to go places. They score points for each place they go to.
(defun dead? (thing) (>= 0 (health thing)))

(defmethod tick! ((peep peep))
  (unless (dead? peep)
    (let ((location (pop (todo peep))))
      (incf (points peep))
      (setf (occupants location) (remove peep (occupants location)))
      (push peep (occupants (or (first (todo peep)) (first (routine peep)))))
      (setf (health peep) (min 100 (+ 5 (health peep))))
      (mapc #'tick! (plagues peep))
      (unless (empty? (todo peep))
	peep))))

;; `tick!`ing a place causes it to score for each `peep` present. And it causes
;; any `plague`s on present `peep`s to try to infect! other nearby peeps.
;; Places also lose points for each dead peep they contain.
(defmethod tick! ((place place))
  (incf (points place) (length (occupants place)))
  (loop for peep in (all-peeps place)
     if (dead? peep)
     do (decf (points place) 2)
     else do (loop for plague in (plagues peep)
		do (loop for victim in (remove peep (all-peeps place))
		      if (>= (virulence plague) (random 100))
		      do (infect! plague victim))))
  place)

;;  So, now we've got the basic framework of the game in place. There are three
;; players in this game: places, peeps and plagues.
;;   A plague player automatically loses if they are completely cured, and
;; automatically wins if they manage to kill everyone. That's fairly simple.
;;   A place player wins if they manage to cure the plague. They automatically
;; lose if all the peeps die. Also, fairly simple.
;;   A peep player is trying to survive. If they manage to make it some numer
;; of turns before dying, then we have to score the game instead of declaring
;; an outright winner regardless of game state.

;;   A peep player's score is the total number of points plus remaining health
;; on all of their peeps, minus the number of active plagues on said peeps. A plague
;; player's score is the total number of health of their plagues, with a multiplier
;; equal to the number of places fully infected by their plague. A place player's score is
;; the total number of points in their places

(defun score (world)
  (list :peep (let ((score 0))
		(loop for p in (all-peeps world)
		   unless (dead? p) do (incf score (+ (health p) (points p)))
		   do (decf score (length (plagues p))))
		score)
	:place (let ((score 0))
		 (loop for p in world
		    do (incf score (points p)))
		 score)
	:plague (let ((score 0))
		  (loop for victim in (all-peeps world)
		     do (loop for p in (plaguesvictim)
			   do (incf score (max 0 (health p)))))
		  (loop for target in world
		     if (every (lambda (victim) (not (empty? (plagues victim))))
			       (all-peeps target))
		     do (setf score (* 2  score)))
		  score)))

;;   I think that's all I've got for now. This is definitely an idea I want to run with.
;; At the moment, it's just a tiny, in-repl proof-of-concept, and not particularly fun,
;; but I'm going to try developing it further with an eye towards turning it into an actual
;; web game playable from this site.

;; As always, I'll let you know how it goes.

(defun pick (lst)
  (nth (random (length lst)) lst))

(defun empty? (lst)
  (null lst))
```
