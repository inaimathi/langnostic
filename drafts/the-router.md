Ok, so a little while ago, I gave [a PW<3 talk](https://www.meetup.com/Papers-We-Love-Toronto/events/244292363/) on one of [Ackley's pieces](http://www.cs.unm.edu/~ackley/papers/hotos-11.pdf)[PDF]. In order to properly give that talk, and point out interesting ideas and approaches in this kind of distributed computation, I had to write a [Router](https://github.com/inaimathi/trurl/blob/62f9c429710331f0d27b7ea00ede3ad489307245/machines.lisp#L23-L116).

![the-router.gif](An animation of the router being spawned, unfolding and beginning to shunt messages)

This is the first involved program I've written in this style, and I learned a bunch of things doing it that weren't obvious to me even after all the time I spent thinking about it, so I thought I'd share.

## How it works

```
;;; trurl.lisp
...

(lem:define-machine lemon nil)

(lem:define-machine router-message
  (let ((dest (pick (loop for (x y) in lem:n*extended
		       for c = (lem:neighbor x y)
		       when (and c (typep (lem:occupant c) 'router-fluid)) collect c))))
    (when dest (lem:move-to! dest lem:here))))

(lem:define-machine router-input
  (let ((messages (loop for (x y) in lem:n*extended
		     for c = (lem:neighbor x y)
		     when (and c (typep (lem:occupant c) 'router-message)) collect c)))
    (when (> 3 (length messages))
      (let ((dest (apply #'lem:neighbor (pick lem:n*extended))))
	(when (and dest (typep (lem:occupant dest) 'router-fluid))
	  (lem:spawn-in! dest (router-message) :message (format nil "~r" (random 10000000)) :x (random 10) :y (random 10)))))))

(lem:define-machine router-endpoint
  (let ((x (lem:get-state lem:self :x 0))
	(y (lem:get-state lem:self :y 0))
	(left (lem:neighbor -3 0))
	(up (lem:neighbor 0 -3))
	(message-cell (pick (loop for (x y) in lem:n*extended
			  for c = (lem:neighbor x y)
			  when (and c (typep (lem:occupant c) 'router-message)) collect c))))
    (when (and left (typep (lem:occupant left) 'router-fluid))
      (lem:spawn-in! left lem:self :x (+ x 1) :y y))
    (when (and up (typep (lem:occupant up) 'router-fluid))
      (lem:spawn-in! up lem:self :x x :y (+ y 1)))

    (unless (null message-cell)
      (let* ((m (lem:occupant message-cell))
	     (mx (lem:get-state m :x))
	     (my (lem:get-state m :y))
	     (name (format nil "ENDPOINT[~a::~a]" x y))
	     (up (lem:neighbor 0 -2))
	     (left (lem:neighbor -2 0)))
	(cond ((and (= x mx)
		    (= y my))
	       (lem:empty! message-cell)
	       (log! name (lem:get-state m :message)))
	      ((and (not (typep (lem:occupant up) 'router-message)) (= x mx))
	       (lem:move-to! up message-cell))
	      ((and (not (typep (lem:occupant left) 'router-message)) (= y my))
	       (lem:move-to! left message-cell)))))))

(lem:define-machine router-fluid
  (let ((w (lem:get-state lem:self :w 10))
	(h (lem:get-state lem:self :h 10))
	(x (lem:get-state lem:self :x 0))
	(y (lem:get-state lem:self :y 0)))
    (labels ((any-of (cell &rest types)
	       (let ((o (lem:occupant cell)))
		 (some (lambda (tp) (typep o tp)) types)))
	     (spawn! (neigh &key (x x) (y y))
	       (when neigh
		 (unless (any-of neigh 'router-endpoint 'router-fluid 'router-input 'router-message)
		   (lem:spawn-in! neigh lem:self :x x :y y :h h :w w)))))
      (when (> w x) (spawn! (lem:neighbor -1 0) :x (+ x 1)))
      (when (> h y) (spawn! (lem:neighbor 0 -1) :y (+ y 1)))
      (unless (zerop x) (spawn! (lem:neighbor 1 0) :x (- x 1)))
      (unless (zerop y) (spawn! (lem:neighbor 0 1) :y (- y 1))))))

(lem:define-machine router
  (flet ((neighbors-at (fn)
	   (loop for (x y) in lem:n*extended
	      for c = (lem:neighbor x y)
	      when (and c (funcall fn x y)) collect c)))
    (let* ((w 25) (h 25)
	   (x (lem:get-state lem:self :x 0))
	   (y (lem:get-state lem:self :y 0))
	   (outsides (concatenate
		      'list
		      (when (zerop x) (neighbors-at (lambda (x y) (> x 0))))
		      (when (zerop y) (neighbors-at (lambda (x y) (> y 0))))
		      (when (= x w) (neighbors-at (lambda (x y) (> 0 x))))
		      (when (= y h) (neighbors-at (lambda (x y) (> 0 y)))))))
      (when (and (zerop y) (> (- w 1) x 1))
	(lem:spawn-in! (lem:neighbor 0 -1) (router-input)))
      (when (and (zerop y) (zerop x))
	(lem:spawn-in! (lem:neighbor -1 -1) (router-fluid) :h (- h 2) :w (- w 2))
	(lem:spawn-in! (lem:neighbor -2 -2) (router-endpoint)))
      (loop for c in outsides do (lem:spawn-in! c (lemon)))

      (when (and (> w x) (or (= y 0) (= y h)))
	(lem:spawn-in! (lem:neighbor -1 0) lem:self :x (+ x 1) :y y))
      (unless (= 0 x)
	(when (or (= y 0) (= y h))
	  (lem:spawn-in! (lem:neighbor 1 0) lem:self :x (- x 1) :y y)))
      (when (and (> h y) (or (= x 0) (= x w)))
	(lem:spawn-in! (lem:neighbor 0 -1) lem:self :x x :y (+ y 1)))
      (unless (= 0 y)
	(when (or (= x 0) (= x w))
	  (lem:spawn-in! (lem:neighbor 0 1) lem:self :x x :y (- y 1)))))))

...
```

![drink-from-the-firehose.jpg]()

This is what the code looks like. It's fugly, because I haven't had the time to really dive into what kinds of ideas we want to express in systems like this. As a consequence, we've got what should probably be abstracted logic threaded throughout these programs. And that's sort of to be expected from the first time I do anything in this context. But it might be instructive to go through it anyway. This style of programming involves defining `machine`s that work on the basis of a grid. The idea is that a `machine` is a prototype that defines a behavior that will be executed when an instance of the machine gets to take a turn.


## The environmental concerns

One interesting thing that happened initially is that I defined `router-fluid` as

```
(lem:define-machine router-fluid
    (labels ((any-of (cell &rest types)
	       (let ((o (lem:occupant cell)))
		 (some (lambda (tp) (typep o tp)) types)))
	     (spawn! (neigh &key (x x) (y y))
	       (when neigh
		 (unless (any-of neigh 'router-endpoint 'router-fluid 'router-input 'router-message)
		   (lem:spawn-in! neigh lem:self)))))
      (loop for (x y) in lem:n*extended
	 do (lem:spawn-in! (lem:neighbor x y) lem:self))))
```

Which is much simpler than the version you saw earlier, but also much more interesting. This is basically gray goo with a whitelist. What's gray goo?

```
(lem:define-machine gg
    (loop for (x y) in lem:n*extended
       do (lem:spawn-in! (lem:neighbor x y) lem:self)))
```

It's the machine that eats everything. The idea behind the simpler `router-fluid` is that when fully formed, a `router`s protective shell is as wide as the event-window radius, so whitelisting that will be enough to keep the fluid contained inside the router. This causes two problems though. Firstly, you need to make sure that the `router` wall is sealed before you spawn the first drop of `router-fluid` because it would otherwise escape and eat the world. Second, you'd damn well better hope that nothing breaches that `router` wall, because the consequence is the same. This original version was titled `router-bleach`, for perhaps obvious resons. You probably _do_ want it to eat things that aren't part of the router, because you might otherwise get hostile particles jumping around that interfere with the operations of the router. But you absolutely _don't_ want it eating things outside the `router`.

This is one of those places where it would help to have different conceptual structures than just grid-based accessors. It might help to have an idea of what it means to be inside of a particular space that isn't quite as low-level as the current `x`/`y` co-ordinate system.

- Talk about grey goo in general and explain the concerns (as well as some mitigating strategies such as imposing a cost on `spawn!`, introducing a high error rate or introducing substrate-level antagonism (the neighborhood surrounding some new move intercommunicates and comes to a consensus about how to treat instructions from a particular program))

## The metastasis problem

- There's a bunch of situations where you want to introduce self-stabilizing populations of certain types of cells. For instance, that router wants to limit messages in some way if its to avoid saturating its output ports. In service to that goal, messages only spawn on and move around in router bleach, which might go through dry spells if enough messages get plonked into the system at the same time
- That's one approach; make sure that there's some finitely available resource cell floating around and make sure that your actual program only reproduces by consuming/highjacking those resource cells
- Another approach is to introduce predation. Make your population get controlled externally; there's some network of higher level cells that spawn/eat them according to some randomized metric
- Another one is conducting a census of your neighborhood on your turn and dispatching on that. This is problematic when you're expected to deal with partial neighborhoods (as you would in a lock-free system)
- You might also go with a stateful system a-la anthills (link to Deborah Gordon's TED talk). That is, each cell conducts a census each turn, and applies some temporal decay of that state. This is problematic in the situation where a cell is wiped out (because that loses a bunch of collected memory rather than just an extra computational unit)
