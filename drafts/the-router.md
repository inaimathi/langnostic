Ok, so a little while ago, I gave [a PW<3 talk](https://www.meetup.com/Papers-We-Love-Toronto/events/244292363/) on one of [Ackley's pieces](http://www.cs.unm.edu/~ackley/papers/hotos-11.pdf)[PDF]. In order to properly give that talk, and point out interesting ideas and approaches in this kind of distributed computation, I had to write a [Router](https://github.com/inaimathi/trurl/blob/62f9c429710331f0d27b7ea00ede3ad489307245/machines.lisp#L23-L116).

![/static/img/the-router/the-router.gif](An animation of the router being spawned, unfolding and beginning to shunt messages)

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

This is what the code looks like. It's fugly, because I haven't had the time to really dive into what kinds of ideas we want to express in systems like this. As a consequence, we've got what should probably be abstracted logic threaded throughout these programs. And that's sort of to be expected given my inexperience in this context. But it might be instructive to go through it anyway. This style of programming involves defining `machine`s that work on the basis of a grid. The idea is that a `machine` is a prototype that defines a behavior that will be executed when an instance of the machine gets to take a turn. On a `machine`s turn, it can consider its surrounding environment, including all exposed state, terrain and population.

## The environmental concerns
### Simple Router Fluid

One interesting thing that happened initially is that I defined `router-fluid` as

```
(lem:define-machine router-bleach
    (labels ((any-of (cell &rest types)
	       (let ((o (lem:occupant cell)))
		 (some (lambda (tp) (typep o tp)) types)))
	     (spawn! (neigh &key (x x) (y y))
	       (when neigh
		 (unless (any-of neigh 'router-endpoint 'router-bleach 'router-input 'router-message)
		   (lem:spawn-in! neigh lem:self)))))
      (loop for (x y) in lem:n*extended
	 do (lem:spawn-in! (lem:neighbor x y) lem:self))))
```

Which is much simpler than the version you saw earlier, but also a bit more interesting at the macro scale. It's basically gray goo with a whitelist. The idea is that when fully formed, a `router`s protective shell is as wide as the event-window radius, so whitelisting those cells will be enough to keep the fluid contained through inter-cell rather than intra-cell interactions. This causes two problems. Firstly, you need to make sure that the `router` wall is sealed before you spawn the first drop of `router-fluid` because it would otherwise escape and eat the world. Second, you'd damn well better hope that nothing breaches that `router` wall for exactly the same reason. This original version was titled `router-bleach`, for perhaps obvious resons. You probably _do_ want it to eat things that aren't part of the router, because you might otherwise get hostile particles jumping around that interfere with its internal operations. But you absolutely _don't_ want it eating anything past the router border.

This is one of those places where it would help to have different conceptual structures than just grid-based accessors. It might help to have an idea of what it means to be inside of a particular space that isn't quite as low-level as the current `x`/`y` co-ordinate system. Because otherwise you have the choice between bounding the fluid explicitly[^which-seems-like], and bounding it implicitly[^which-has-the-problem].

[^which-seems-like]: Which seems like it would make it harder to scale the `router` out to encompass more space later.

[^which-has-the-problem]: Which has the problem we just discussed of posing a serious threat to the well-being of the world if it ever escapes. Although it seems like this might make for an interesting and dramatic story arc in some piece of science fiction, we realistically want to limit that risk.

### Gray Goo

While we're on the topic, in case it's not obvious from the above, what's gray goo?

```
(lem:define-machine gg
    (loop for (x y) in lem:n*extended
       do (lem:spawn-in! (lem:neighbor x y) lem:self)))
```

It's the machine that eats everything. If `box` is ["the machine that knows what it is to be a box"](https://youtu.be/Dmlm6mtnSZs?t=4m30s), then `gg` is "the machine that knows what it is to hunger without end". In a massively distributed system with no central ownership, it's possible that someone intentionally introduces `gg` to the world, but as we've seen above, it's also entirely possible to effectively do it by accident.

One way of fighting this involves restricting the set of possibly spawnable machines. The idea is that we do some amount of research into what sorts of machines make a good enough set of primitives, agree on them, and burn their definitions into the substrates of all of our cellular networks. That makes sure we've got a known-good-actor set of machines to choose from, but something tells me that the expressivity of such a system is going to suffer pretty fatally. My instinct is that what we want is a set of composition primitives; a language from which to craft machines rather than a pre-defined set.

A second way involves imposing a cost on the `spawn!` operation in some way. So that a machine that does nothing _but_ spawn in all directions will run out of resources in short order.

A third is introducing a high error-rate or some substrate-level antagonism towards ill-behaved machines. As in, the cells themselves do some kind of tracking, inter-communication and decision procedure to decide whether to honor orders coming from a particular machine class or instance. I'm not sure exactly what shape this would take, but it's at least marginally plausible.

Also, lets be honest here, I have no idea which or any of these might be the correct solution in general, if there even is one. This is something I'll have to figure out by prototyping like a madman for the next little while.

## The metastasis problem

There's a bunch of situations in which you want to introduce self-stabilizing populations of cells into the world. For instance, the above router wants to limit messages in some way if it's to avoid saturating its output ports. In service to that goal, messages only spawn on and move around in router fluid. That fluid might go through dry spells if enough messages get plonked into the system at the same time, and the rate of message spawning is therefore implicitly capped through the behavior of the cells. That's one approach; make sure that incoming spawns require some non-empty substrate and make sure that the substrae is not infinitely available in some way.

Another approach is introducing predation. In our router example, that would mean introducing a `rate-limiter` cell that would amble around inside of the router and ocassionally kill off message cells with some low probability. This approach is shown in action by Ackley in his `DReg`/`Res` example. The end result is slow to become stable, and might kill off too much through random chance, but fundamentally works and is driven primarily by intra-cell forces.

Yet another approach is to have each spawning cell conduct a census of its neighborhood and only spawning if it finds a population below some threshold. In a router, that would mean adding code to `router-input` to check how many `router-message` cells are visible before spawning new `router-message`s. This differs from the predation approach in two ways. Firstly, it's not probability based, so some equilibrium is reached relatively quickly, and second, it relies mostly in inter-cell behavior rather than intra-cell behavior[^there-is-no-explicit].

[^there-is-no-explicit]: There is no explicit die-roll here. We check if there's "enough" messages running around to keep us busy, and if there are, we deterministically refrain from spawning new messages. The overall system behavior might still be very chaotic because of the shunting behavior of `router-endpoint`s or `router-message`s themselves, but that chaos is introduced elsewhere and not exacerbated by the census-conducting `router-input` cells.

## The synchronization problem

- Synchronizing a specific turn gets odd in the global context. While we're running single-threaded simulators, it's easy to imagine a turn being taken somewhere in a way that minimizes friction. In a distributed, decentralized system, it's much harder to understand how some field of 41 cells[^or-whatever-number] ends up deciding wh

[^or-whatever-number]: Or whatever number you ultimately ended up picking for your thing, it doesn't matter.


- Talk about interactions between synchronization problems and census taking. "This is problematic when you're expected to deal with partial neighborhoods (as you would in a lock-free system)"
- You might also go with a stateful system a-la anthills (link to Deborah Gordon's TED talk). That is, each cell conducts a census each turn, and applies some temporal decay of that state. This is problematic in the situation where a cell is wiped out (because that loses a bunch of collected memory rather than just an extra computational unit)
