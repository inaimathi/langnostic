Two things this time. First...

### <a name="an-admission"></a>...An Admission

![](http://2.bp.blogspot.com/-FYMRLJ4dw-c/UU0D-CbvKxI/AAAAAAAAAPU/Qlo2W9wmc6I/s1600/vrms-output.png)

I'm weak.

It turns out there are exactly two things I'm willing to run non-free software for, and one is wifi access<a name="note-Fri-Mar-22-212851EDT-2013"></a>[|1|](#foot-Fri-Mar-22-212851EDT-2013). Another other option is of course, buying an [Atheros wifi card](https://www.thinkpenguin.com/gnu-linux/penguin-wireless-n-usb-req-antennas), which I intend to do eventually but don't have the spare $100 right at this very moment. Lets move on and say no more about this.

### <a name="actors"></a>Actors

I've been on vacation for a little while now, which finally gave me the chance to get back into some Common Lisp<a name="note-Fri-Mar-22-212856EDT-2013"></a>[|2|](#foot-Fri-Mar-22-212856EDT-2013). You know, since I've mostly been hacking Python at work for the past five months or so. Specifically, I got to do some long-overdue thinking on [that Actors library I forked](https://github.com/Inaimathi/Common-Lisp-Actors) forever and a fucking day ago.

The big problem with actors as they're implemented here is that, while they don't care where their messages come from, they very much *do* care where their messages go. To be fair, this seems to be a very common implementation, and not limited to cl-actors, so I don't think it's worth holding against the author. What it does is force you to choose between three fairly shitty alternatives for composeability:

## <a name="message-targets"></a>1, Message Targets

Define a communication convention whereby a piece of the message is going to specify the actor that it needs to be passed to next.

```lisp
(define-actor greeter () (target name)
  (send target (format nil "Hello, ~a!" name))
  next)

(define-actor printer (stream) (msg)
  (format stream "~a~%" msg)
  next)

(defparameter *greeter* (greeter))
(defparameter *printer* (printer :stream *standard-output*))

(send *greeter* *printer* "whoeverthefuck")
```

The problem here is that you're setting up a situation where each sender is going to have to know the entire call chain its message will be going through. That's not good because changing any node suddenly becomes an exercise in frustration if you've got a non-trivial actor network set up, and it only gets worse if you want to do anything other than straight chaining actors. For instance, think about how you would implement an imbalanced tree here; a situation where you have actors `A` through `F`, and what needs to happen is

```
    actor-A
    ├──> actor-B
    ├──> actor-C
    └──> actor-D
         └──> actor-E ──> actor-F
```

## <a name="globals"></a>2. Globals

The Erlang equivalent is "[registered processes](http://www.erlang.org/doc/reference_manual/processes.html#id82815)"; you define a global name which will refer to your actor instance, and any other actors that need to interact with it use that global name.

```lisp
(define-actor greeter () (name)
  (send *printer* (format nil "Hello, ~a!" name))
  next)

(define-actor printer (stream) (msg)
  (format stream "~a~%" msg)
  next)

(defparameter *greeter* (greeter))
(defparameter *printer* (printer :stream *standard-output*))

(send *greeter* "whoeverthefuck")
```

The problem has moved from the last line to the second line. This approach requires you to re-write pieces of every non-leaf actor if you want to use them in a new context. Ideally, an actor wouldn't have care where its messages go, or at least it wouldn't have to care about it until after it's instantiated. That would let you increase the isolation of your components, thereby giving you more and easier opportunities for code reuse.

## <a name="local-state"></a>3. Local State

Instead of manually specifying targets, make the actor track its targets with a piece of local state. You'd then have to pass targets in along with the other initialization parameters.

```lisp
(define-actor greeter (targets) (name)
  (let ((msg (format nil "Hello, ~a!" name)))
    (mapcar (lambda (trg) ;; blast you, canonical truth value T!
              (send trg msg))
            targets))
  next)

(define-actor printer (stream) (msg)
  (format stream "~a~%" msg)
  next)

(defparameter *printer* (printer :stream *standard-output*))
(defparameter *greeter* (greeter :targets (list *printer*)))

(send *greeter* "whoeverthefuck")
```

The two problems with this are complexity and definition dependencies. Complexity because, as you can see from that new `greeter` definition, *most* of the body code is now dealing with where the message is meant to go next, rather than with the business logic of what this actor is supposed to be doing. I'm tempted to call this the Yak Shaving Anti-pattern, except that someone else has certainly identified and named it already.

The other problem is apparent in the change among those two `defparameter` lines. Note that `*greeter*` is now defined second, and that this isn't an accident. If you did it the other way around, you'd discover that `*printer*` must be defined in order for it to be specified as a message target.It may be a minor annoyance, but I prefer to avoid those where I can.

### <a name="the-solution"></a>The Solution?

As far as I can see, oh and thanks to [Paul Tarvydas](https://github.com/guitarvydas) for pointing me in this direction, it's to separate the actors from their call chains. That is, define an actor as essentially a queue, a thread and a function that returns some value given some message, then introduce an external mechanism by which to get that return value to the next node in the network. What we really want to be able to do is something like

```lisp
(define-actor greeter () (name)
  (format nil "Hello, ~a!" name))

(define-actor printer (stream) (msg)
  (format stream "~a~%" msg))

(defparameter *greeter* (greeter))
(defparameter *printer* (printer :stream *standard-output*))

(link *greeter* *printer*)

(send *greeter* "whoeverthefuck")
```

which concentrates the links entirely into that call to `link`, and leaves the actors themselves cheerfully oblivious to what they're interacting with at the time. It also separates out the general patterns of communication<a name="note-Fri-Mar-22-212912EDT-2013"></a>[|3|](#foot-Fri-Mar-22-212912EDT-2013) from the business logic of an actor body, so your `define-actor`s are *only* dealing with the stuff they want to do, rather than the minutia of who needs to do the next bit. So, here's how we do it. Firstly, we'll want to change the definition of an `actor` to take into account the fact that others may be watching.

```lisp
(defclass actor ()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor name)
   (behavior :initarg :behavior
             :initform (error ":behavior must be specified")
             :accessor behavior
             :documentation "Behavior")
   (watched-by :initarg :watched-by :initform nil 
             :accessor watched-by)
   (in :initform (make-queue) :accessor in
       :documentation "Queue of incoming messages")
   thread))
```

`watched-by` is the addition there; it'll hold a list of all `actor`s and/or `queue`s that might need to be notified about this actors' output. Next, we'll want to simplify `define-actor` slightly, because we want to collect the return value from its behavior rather than assuming it sends the message on itself

```lisp
(defmacro define-actor (name state vars &body body)
  "Macro for creating actors with the behavior specified by body"
  `(defun ,name (&key (self) ,@state)
     (declare (ignorable self)) ;; You might not care about referencing self, and that's ok
     (setf self (make-actor (lambda ,vars (progn ,@body)) ,(string name)))
     self))
```

I also took the opportunity to do away with the need for an explicit `next`. Near as I can tell, this is just going to prevent me from changing out behaviors at runtime and creating one-cycle actors. My intuition about the first is that it'd be easier to define a new actor and insert it into the network than it would be to reliably and correctly rip out the behavior function of one we already have in place, so I don't mind losing that, though I reserve the right to change my mind if experience teaches me the contrary. The second one is a situation where I'd really want to use a `thread` with an embedded `lambda` anyway, so not being able to use an actor there doesn't sound particularly disastrous.

Finally, we'll need to change what each cycle through the message queue does.

```lisp
(defmethod initialize-instance :after ((self actor) &key)
  "Uses the main function name to create a thread"
  (with-slots (behavior in name thread) self
    (setf thread 
          (bt:make-thread 
           (lambda () 
             (loop 
                for res = (apply behavior (dequeue in))
                when (watched-by self)
                  ;; TODO -- Add customization to protocol, rather than always send-all
                do (loop for target in (watched-by self)
                      do (enqueue (list res) target))))
           :name name))))
```

so, instead of just `apply`ing `behavior` to each message, we get the result and send it on to any watchers. The `TODO` is there because, as written, an `actor` *always* notifies *all* watchers, and we might want to do something like round-robin scheduling instead. The main reason I'm thinking along those lines is that I'm planning to use this library in the construction of a non-blocking web-server, where I'd want a single listener but multiple, parallel parsers/response-generators picking up some percentage of total load. Doing something other than "send one to everyone" is an integral part of that strategy. We'll see how it goes.

I should note that you don't have to decide to use only one of `send`/`link` here; even with the connection system working<a name="note-Fri-Mar-22-212925EDT-2013"></a>[|4|](#foot-Fri-Mar-22-212925EDT-2013) there are use cases where you really do want a manual `send` in an actor body. To be fair, most of those use cases seem to be places where you wouldn't really want to use actors in the first place, but I've reserved judgment and left in both options in the interests of flexibility.

### <a name="still-todo"></a>Still ToDo

I've already mentioned separating out the send pattern for an actor so that you can have more flexibility in deciding targets. Although, to be fair, I'm not *entirely* sure whether that's the best approach; it might be possible to implement different behaviors by just specifying different network shapes rather than by complicating actors further. I'll think on it, and probably solicit some advice from people smarter than I am.

Some additional network-oriented constructs would be nice. We've already got `link` and `chain`, but it seems like `splice`, `prune` and `nip` might be useful too. `splice` would take two linked actors and a third actor, and insert the third one between the first two. `prune` would take an actor, kill it and remove it from any `watched-by` lists it might be on. `nip` would basically do the opposite of `splice`; take three linked actors, remove the middle one and connect the first to the last.

While I'm at it, it would be nice if all these functions, real and notional, played by Actor Model rules rather than doing hard edits. For instance, instead of `link` doing `(push target (watched-by self))`, it would send a message to `self` which would get processed when it came up in the queue. This has a bit more background complexity than the straight-up side-effect, but it prevents the actor from dropping any messages that might be getting processed while the change is taking place.

While I'm at *that*, it would be nice if actors automatically responded to certain messages without being specified explicitly in the `define-actor` body. Off the top of my head, `:link` (for creating connections), `:drop` (for breaking them), `:set` (for changing actor state) and `:ping` (to allow for supervisor-style constructs later).

The reason I'm just listing these rather than building them *right now* is that some of them would require a fundamental change to the way the system works. For one thing, accepting default messages implies that we're taking a message which is only one object that we then [pattern](https://github.com/arielnetworks/cl-pattern)-[match](https://github.com/m2ym/optima) on. For another, things like `prune` imply either a centralized storage method for all actors, or imply two-way links between nodes, neither of which I'm sure is a good idea. It might be better to assume that connections are only going to be created at startup.

Anyhow, in the meanwhile, what I've got here is a trivially composeable actor system, which lets you re-use any of them at will in any context that applies. That by itself makes the effort worth it as far as I'm concerned. I'll see what I can do for the next pass.


<hr />
##### Footnotes

1 - <a name="foot-Fri-Mar-22-212851EDT-2013"></a>[|back|](#note-Fri-Mar-22-212851EDT-2013) - The other is vintage gaming. Which doesn't pollute my main machine, but I do have a desktop set up at home which has a virtual Win XP machine where I installed a bunch of games from the golden age of fantasy gaming; copies of [Icewind Dale 2](http://www.planetbaldursgate.com/iwd2/), [Planescape Torment](http://www.amazon.com/Planescape-Torment-Pc/dp/B00002EPZ2), [Baldur's Gate 2](http://web.archive.org/web/20000815213945/http://www.interplay.com/bgate2/), and [Dungeon Keeper](http://en.wikipedia.org/wiki/Dungeon_Keeper).

2 - <a name="foot-Fri-Mar-22-212856EDT-2013"></a>[|back|](#note-Fri-Mar-22-212856EDT-2013) - And a bunch of sketching, but this isn't the place for that. If you're interested, go to [my deviantart](http://inaimathi.deviantart.com/) instead, I'll be uploading a new batch shortly.

3 - <a name="foot-Fri-Mar-22-212912EDT-2013"></a>[|back|](#note-Fri-Mar-22-212912EDT-2013) - Though, as you'll see later, those could probably be separated further still. I'll be working on it for the next little while.
4 - <a name="foot-Fri-Mar-22-212925EDT-2013"></a>[|back|](#note-Fri-Mar-22-212925EDT-2013) - And in most cases, producing much more elegant and flexible code, I might add.
