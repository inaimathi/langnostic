So a little while ago, the [Comp Sci Cabal](http://cscabal.com/) read [a paper](https://infoscience.epfl.ch/record/169879/files/RMTrees.pdf) entitled ["RRB Trees: Efficient Immutable Vectors"](https://infoscience.epfl.ch/record/169879/files/RMTrees.pdf). It wasn't entirely obvious to me how you'd go about implementing this scheme in practice. In particular, because their illustrations of concatenation on page 4 only shows the fortuitous case, it's kind of difficult to figure out what the procedure would look like for sub-optimal cases. As it happens, I was also talking to someone about implementing some [persistent datastructures for Common Lisp](https://github.com/inaimathi/cl-fds) so that I'd have less desire to reach for Clojure when prototyping certain things for my personal projects.

So, lets fucking [do this](https://github.com/inaimathi/ribbit)[^also-be-sure-to].

[^also-be-sure-to]: Also, be sure to read or at least skim [the paper](https://infoscience.epfl.ch/record/169879/files/RMTrees.pdf), because the rest of this article assumes you at least got the gist of what we're trying to do here. Some reading on [purely functional data structures](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf) might also be in order depending on how much experience you have with this sort of thing.

## Naive `cat`

So, the goal of this datastructure is to get slightly better concatenation behavior than other options. With that in mind, the interesting operation
is `cat`[^similarly-interesting-ops].

[^similarly-interesting-ops]: And similarly interesting operations are going to be `split` and `insert-at`, but we'll leave those for later. Partly because I suspect they won't have much to do with `cat`, and partly because I've also started [a separate project](https://github.com/inaimathi/cl-fds) to implement a more general interface for functional datastructures in Common Lisp. Patches are, of course, welcome on both.

```lisp
;;;; package.lisp
(defpackage #:ribbit
  (:use #:cl)
  (:export
   #:ribbit #:cat #:len #:ix #:split #:insert-at))
```

That's just package setup. This is going to be a data structure, so we don't need any external libraries, just `:cl`. We'll be exporting `:ribbit` (the literal notation for this data structure), `:cat` (the subject of this article), `:len` (a way to calculate the number of elements in a `ribbit`), `:ix` (a way to index into a `ribbit`), `:split` (a way to partition a ribbit into two trees) and `:insert-at` (a way to insert a new element between two existing elements of a `ribbit` at a given index).

Lets start with that literal notation.

```lisp
;;;; ribbit.lisp
(in-package #:ribbit)

;; Basics and internals
(defconstant +size+ 4)
```

The structure outlined by the paper works for size `m` leaf chunks. The actual `m` is apparently `32`, but `4` still works on the same principles and is a lot easier to think about and write up. Testing is going to require going above `2`, because some stages allow for `m-1` sized chunks to be emitted and preserved and it would be mildly unsatisfactory to only see `empty` or `reusable` vectors later down the line. It'll make sense in a bit, trust me.

```
(defun take-across (n vecs)
  (let ((ct n)
	(head nil)
	(tail nil))
    (loop for vs on vecs while (> ct 0)
       for v = (first vs)
       do (cond ((> (length v) ct)
		 (push (subseq v 0 ct) head)
		 (setf tail (cons (subseq v ct) (rest vs))
		       ct 0))
		(t (push v head)
		   (decf ct (length v))))
       finally (unless tail (setf tail vs)))
    (values (apply #'concatenate 'vector (reverse head)) tail)))

(defun repartition (ct vecs)
  (let ((rest vecs))
    (loop while rest
       collect (multiple-value-bind (next rst) (take-across ct rest)
		 (setf rest rst)
		 next))))
```

This is the conceptual core of the operation. `take-across` is like the classic [`take`](https://clojuredocs.org/clojure.core/take), but operating on multiple input sequences instead of just one[^note-for-haskellers]. In this case, we're working with `vector`s because that's our first cut internal representation of `ribbit`s.

[^note-for-haskellers]: If you're a Haskeller or MLer, you can think of it as approximately `take-across n = take n . concat`, except that this version also returns its tail, and not just the head.

```
RIBBIT> (take-across 5 (list #(1) #(2 3) #(4 5 6 7 8 9 10 11) #(12 13) #(14 15 16)))
#(1 2 3 4 5)
(#(6 7 8 9 10 11) #(12 13) #(14 15 16))
RIBBIT> (take-across 1 (list #(1) #(2 3) #(4 5 6 7 8 9 10 11) #(12 13) #(14 15 16)))
#(1)
(#(2 3) #(4 5 6 7 8 9 10 11) #(12 13) #(14 15 16))
RIBBIT> (take-across 3 (list #(1) #(2 3) #(4 5 6 7 8 9 10 11) #(12 13) #(14 15 16)))
#(1 2 3)
(#(4 5 6 7 8 9 10 11) #(12 13) #(14 15 16))
RIBBIT>
```

As you can see, it also returns the unconsumed `tail` of its input in order to make it easier to define `repartition`. `repartition` takes a length and bunch of sequences, and uses `take-across` to separate the sequences' elements into vectors of at most the given length[^at-most-because].

[^at-most-because]: "At most" because, as you cans see by the example of `(repartition 3 ...)` above, the total number of elements in the incoming sequence might not divide evenly into the given length. In that case, the last `repartition`ed vector will actually be shorter than specified.

```
RIBBIT> (repartition 2 (list #(1) #(2 3) #(4 5 6 7 8 9 10 11) #(12 13) #(14 15 16)))
(#(1 2) #(3 4) #(5 6) #(7 8) #(9 10) #(11 12) #(13 14) #(15 16))
RIBBIT> (repartition 3 (list #(1) #(2 3) #(4 5 6 7 8 9 10 11) #(12 13) #(14 15 16)))
(#(1 2 3) #(4 5 6) #(7 8 9) #(10 11 12) #(13 14 15) #(16))
RIBBIT> (repartition 4 (list #(1) #(2 3) #(4 5 6 7 8 9 10 11) #(12 13) #(14 15 16)))
(#(1 2 3 4) #(5 6 7 8) #(9 10 11 12) #(13 14 15 16))
RIBBIT>
```

Now that we've got those two basic utilities, we can write our `ribbit` literal.

```
;; Literal ribbit notation
(defun ribbit (&rest elems)
  (let ((rb (list (coerce elems 'vector))))
    (loop for next = (repartition +size+ rb)
       if (not (cdr next)) return (first next)
       else if (>= +size+ (length next)) return (coerce next 'vector)
       else do (setf rb (list (coerce next 'vector))))))
...
```

It's a function that takes some number of elements (the leaf nodes of our intended `ribbit`) and returns an appropriately `partition`ed `vector` tree.

```
RIBBIT> (ribbit 1 2)
#(1 2)
RIBBIT> (ribbit 1 2 3 4)
#(1 2 3 4)
RIBBIT> (ribbit 1 2 3 4 5 6)
#(#(1 2 3 4) #(5 6))
RIBBIT> (ribbit 1 2 3 4 5 6 7 8 9 10 11 12)
#(#(1 2 3 4) #(5 6 7 8) #(9 10 11 12))
RIBBIT> (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
#(#(#(1 2 3 4) #(5 6 7 8) #(9 10 11 12) #(13 14 15 16)) #(#(17 18 19 20)))
RIBBIT> (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
#(#(#(1 2 3 4) #(5 6 7 8) #(9 10 11 12) #(13 14 15 16)) #(#(17 18 19 20) #(21)))
RIBBIT>
```

Ok, so now we've got basic `REPL`-level interaciton primitives ready so we can start building the interface for this persistent data structure. Here's a first cut of `cat`.

```
;;;; ribbit.lisp
...
;; External interface
(defun stupid-cat (a b)
  (let ((zeros nil))
    (labels ((find-zeros (rb)
	       (if (not (vectorp (aref rb 0)))
		   (push rb zeros)
		   (loop for v across rb do (find-zeros v)))))
      (mapc #'find-zeros (list a b))
      (loop for next = (repartition +size+ (reverse zeros))
	 if (not (cdr next)) return (first next)
	 else if (>= +size+ (length next)) return (coerce next 'vector)
	 else do (setf zeros (list (coerce next 'vector)))))))
...
```

It... kinda works.

```
RIBBIT> (stupid-cat (ribbit 1 2) (ribbit 3 4 5 6))
#(#(1 2 3 4) #(5 6))
RIBBIT> (stupid-cat (ribbit 1 2 3 4 5 6 7 8) (ribbit 9 10 11 12 13 14 15 16))
#(#(1 2 3 4) #(5 6 7 8) #(9 10 11 12) #(13 14 15 16))
RIBBIT> (stupid-cat (ribbit 1 2 3 4) (ribbit 5 6 7 8))
#(#(1 2 3 4) #(5 6 7 8))
RIBBIT> (stupid-cat (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18) (ribbit 19 20))
#(#(#(1 2 3 4) #(5 6 7 8) #(9 10 11 12) #(13 14 15 16)) #(#(17 18 19 20)))
RIBBIT>
```

So, why "`stupid-cat`"?

```
RIBBIT> (let ((r (ribbit 1 2 3 4)))
	  (eq r (aref (stupid-cat r (ribbit 5 6 7 8)) 0)))
NIL
RIBBIT>
```

Because it doesn't pointer share.

As you can see, even though the first element of that resulting `ribbit` is the same as part of the input, `eq` reveals that the element pointers have actually been copied. Half the point of using persistent data structures is that, because mutation is heavily restricted, you can share structures with previous "versions" of a particular data instance. In this case, it would be entirely reasonable to simply point the first element of the result at the input vector that it's going to be exactly equivalent to.

So. How do we do that?

## Being _slightly_ less naive

One potential answer (the one I think is the simplest) isn't even in `cat`, but actually in `take-across`.

```
(defun take-across (n vecs)
  (let ((ct n)
	(head nil)
	(tail nil))
    (loop for vs on vecs while (> ct 0)
       for v = (first vs)
       do (cond ((> (length v) ct)
		 (push (subseq v 0 ct) head)
		 (setf tail (cons (subseq v ct) (rest vs))
		       ct 0))
		(t (push v head)
		   (decf ct (length v))))
       finally (unless tail (setf tail vs)))
    (values (apply #'concatenate 'vector (reverse head)) tail)))
```

Note that this thing returns a fresh sequence as a header (by application of `concatenate`), _even if the current `head` is composed of only one element_. We can do better by optimizing for a special case.

```
(defun take-across (n vecs)
  (let ((ct n)
	(head nil)
	(tail nil))
    (loop for vs on vecs while (> ct 0)
       for v = (first vs)
       do (cond ((> (length v) ct)
		 (push (subseq v 0 ct) head)
		 (setf tail (cons (subseq v ct) (rest vs))
		       ct 0))
		(t (push v head)
		   (decf ct (length v))))
       finally (unless tail (setf tail vs)))
    (values
     (if (cdr head)
	 (apply #'concatenate 'vector (reverse head))
	 (first head))
     tail)))
```

If the collected `head` sequence is one element long, we just return that first element rather than doing anything with `concatenate`. This lets us reuse 0-level `ribbit` pointers transparently.

```
RIBBIT> (let ((r (ribbit 1 2 3 4)))
	  (eq r (aref (stupid-cat r (ribbit 5 6 7 8)) 0)))
T
RIBBIT>
```

Sadly, however, this is still a `stupid` cat. Because special-casing `take-across` as shown really only gets you pointer sharing at the lowest level of tree.

```
RIBBIT> (stupid-cat (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) (ribbit 1 2 3 4))

#(#(#(1 2 3 4) #(5 6 7 8) #(9 10 11 12) #(13 14 15 16)) #(#(1 2 3 4)))
RIBBIT> (let ((r (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
	  (eq r (aref (stupid-cat r (ribbit 1 2 3 4)) 0)))
NIL
RIBBIT> (let ((r (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
	  (eq (aref r 0) (aref (aref (stupid-cat r (ribbit 1 2 3 4)) 0) 0)))
T
RIBBIT>
```

In order to do any better than this, we kind of have to step up our game.

## Being less naive

First off, we're currently checking for depth 0 `ribbit`s by checking if their first element is not a vector. If you want to use a `ribbit` of `vector`s for some reason, this will fuck you over pretty hard. So, we're changing that strategy by introducing an explicit `ribbit` struct.

```
...
(defstruct ribbit (size-table nil) (depth 0) (vec #()))
...
```

Now that we've got that, we'll specialize `take-across` and `repartition` so that they work on `ribbit`s rather than general sequences.

```
...
(defun take-across (depth rbs)
  (if (not (cdr rbs))
      (values (first rbs) nil)
      (let ((ct *m*)
	    (head nil)
	    (tail nil))
	(loop for rs on rbs while (> ct 1)
	   for r = (first rs)
	   for v = (ribbit-vec r)
	   do (cond ((> (length v) ct)
		     (push (subseq v 0 ct) head)
		     (setf tail (cons (mk-ribbit depth (subseq v ct)) (rest rs))
			   ct 0))
		    (t (push v head)
		       (decf ct (length v))))
	   finally (unless tail (setf tail rs)))
	(values
	 (mk-ribbit depth (apply #'concatenate 'vector (reverse head)))
	 tail))))

(defun repartition (depth rbs)
  (let ((rest rbs))
    (loop while rest for r = (pop rest)
       if (reusable? r) collect r
       else collect (multiple-value-bind (next rst) (take-across depth (cons r rest))
		      (setf rest rst)
		      next))))
...
```

This new representation has some complications that we need to abstract away from the `ribbit` literal function.

```
...
(defun full-level? (ribbit) (= *m* (length (ribbit-vec ribbit))))

...

(defun compute-size-table (depth vec)
  (unless (or (zerop depth) (and (= *m* (length vec)) (every #'full-level? vec)))
    (coerce
     (let ((s 0))
       (loop for e across vec
	  do (incf s (len e)) collect s))
     'vector)))

(defun mk-ribbit (depth vec)
  (make-ribbit :size-table (compute-size-table depth vec) :depth depth :vec vec))

(defun ribbit-level (depth elems)
  (let ((es elems))
    (loop while es
       collect (mk-ribbit
		depth (coerce
		       (loop repeat *m* while es
			  for e = (pop es) collect e)
		       'vector)))))

(defun ribbit-from (depth elems)
  (let ((rbs (ribbit-level depth elems))
	(d depth))
    (loop while (cdr rbs)
       do (setf rbs (ribbit-level (incf d) rbs)))
    (first rbs)))

(defun ribbit (&rest elems)
  (if elems
      (ribbit-from 0 elems)
      (make-ribbit)))
...
```

Also, you might notice the `len` call in the definition of `compute-size-table`, which means we'll need to define that ahead of completing our `cat` implementation.

```
...
(defun len (rb)
  (cond ((zerop (ribbit-depth rb))
	 (length (ribbit-vec rb)))
	((null (ribbit-size-table rb))
	 (expt *m* (+ 1 (ribbit-depth rb))))
	(t (let ((szs (ribbit-size-table rb)))
	     (aref szs (- (length szs) 1))))))
...
```

The good news is that, as a result of the above, `stupid-cat` gets a bit simpler.

```
...
(defun stupid-cat (a b)
  (ribbit-from 1 (repartition 0 (append (prune-to 0 a) (prune-to 0 b)))))
...
```

The bad news is that building up to `smart-cat` is a bit tricky.

Because this routine is still so raw in my mind, lets jump into the hard part first and I'll paint in the surroundings for you as I go. In fact, lets go through what we actually want before writing the code. Here's a notation I've been toying with.

![](/static/img/ribbit-001--fully-reusable-left-tree-right-tree-of-same-depth.jpg)

If we've got a reusable tree on the left, and a tree of the same depth on the right, we've got the easy case. It's basically just a matter of consing the two together.

![](/static/img/ribbit-002--fully-reusable-left-tree-right-tree-of-greater-depth.jpg)

If we've got a reusable tree on the left and a deeper tree on the right, we want to build a ribbit from the thing on the left and the equivalently deep levels on the right[^there-may-in-fact]

[^there-may-in-fact]: There may in fact be more edge cases opening up here when we consider splitting trees, since that can yield "invalid" `ribbits`. I'll discuss this more in a future article.

![](/static/img/ribbit-003a--non-zero-left-tree.jpg)
![](/static/img/ribbit-003b--non-zero-left-tree.jpg)

This is a more complicated case. If we've got a non-reusable, non-zero-level left tree, we need to

1. find its maximum reusable level
2. get the corresponding level from the right tree
3. make a new `ribbit` with the reusable elements from the beginning of that level, along with the `ribbpartition`ing of the remainder (there's no point in `ribbpartition`ing reusable elements, and we can't assume that the rest can be used as-is in a new tree)

Oh, also, if we don't find a reusable level, we'll default to the `stupid-cat` approach, because nothing will save us from copying at least some of the right tree at that point.

![](/static/img/ribbit-004--non-reusable-zero-left-tree.jpg)

Finally, if the left tree is both zero-level _and_ non-reusable, we've hit our screw case. At this point, we may as well just apply the strategy from `stupid-cat` because reuse is unlikely.

In code, that looks like

```
...
(defun smartish-cat (a b)
  (let* ((d (ribbit-depth a))
	 (max-d (or (max-reusable-level a) 0)))
    (cond ((and (reusable? a) (>= d (ribbit-depth b)))
	   (mk-ribbit (+ d 1) (vector a (raise-to d b))))
	  ((reusable? a)
	   (ribbit-from (+ d 1) (cons a (prune-to d b))))
	  (t
	   (ribbit-from
	    (+ max-d 1)
	    (repartition
	     max-d (append
		    (prune-to max-d a)
		    (prune-to max-d (raise-to max-d b)))))))))
...
```

The question that should be borderline annoying you by this point is "what does it mean to be `reusable?`?".

```
...
(defun reusable? (ribbit)
  (let* ((d (ribbit-depth ribbit))
	 (ct (length (ribbit-vec ribbit)))
	 (reusable-lv (or (= *m* ct) (= *n* ct))))
    (or (and (zerop d) reusable-lv)
	(let ((max (expt *m* (+ 1 d)))
	      (l (len ribbit)))
	  (or (= l max) (= l (- max 1))))
	(and reusable-lv
	     (every #'reusable? (ribbit-vec ribbit))))))
...
```

As per the paper, we're dealing with `m/n` trees here, which means that an individual node is allowed to be either `+size+` or `(- +size+ 1)` in length. So a `reusable?` node is one of

1. Fully saturated
2. One off from being fully saturated
3. One smaller than `+size+`, and has only `reusable?` children
4. A 0-depth node that is one smaller than `+size+` or as large as `+size+`

The last bits of this procedure that you haven't seen yet are `raise-to`, `prune-to` and `max-reusable-level`, none of which should be surprising in their definitions.

```
...
(defun raise-to (depth ribbit)
  (if (>= (ribbit-depth ribbit) depth)
      ribbit
      (let ((r ribbit)
	    (d (ribbit-depth ribbit)))
	(loop until (= d depth)
	   do (setf r (mk-ribbit (incf d) (vector r))))
	r)))

(defun prune-to (depth ribbit)
  (if (= depth (ribbit-depth ribbit))
      (list ribbit)
      (let ((rbs (list ribbit)))
	(loop until (= depth (ribbit-depth (first rbs)))
	   do (setf rbs (loop for r in rbs append (coerce (ribbit-vec ribbit) 'list))))
	rbs)))

(defun max-reusable-level (ribbit)
  (cond ((reusable? ribbit) (ribbit-depth ribbit))
	((zerop (ribbit-depth ribbit)) nil)
	(t (loop for r across (ribbit-vec ribbit)
	      for d = (max-reusable-level r)
	      if d do (return d)))))
...
```

`raise-to` just wraps its input in singleton `ribbit`s until we get to a desired depth. `prune-to` returns the list of `ribbits` at the given level of the input, and `max-reusable-level` does a depth-first traversal to find the first `reusable?` node of the given `ribbit`.

## Being smart

The problem with `smartish-cat` is that it still doesn't reuse nodes in all cases where it might. Consider the situation

![](/static/img/ribbit-005--consing-onto-reusable-first-block.jpg)

Here, `smartish-cat` would throw away the vast majority of the right tree (because we default to calling `stupid-cat` in the case of non-reusable, zero-level left trees), even though it would optimally only have to copy out 4 nodes, and one zero-level chunk. What we'd really want to do here is figure out which zero-level nodes we need to fuck with, and make sure to only copy out affected parents rather than the full tree going upwards.

This was a harder problem for me to think about than I was sort of expecting. It required backing off a few times, and taking another run up the hill after a break. Including re-writing most of the underlying plumbing into something mildly more elegant and less bug-ridden. Which is why (sorry) we kind of need to go through some of it again in fine grained detail.

### Another implementation

Ok, so first up, there's a problem that I didn't even mention last time, and that's the type conflation issue we introduce by doing the stupid depth auto-detection thing. I'm talking about

```
...
(defun mk-ribbit (vec)
  (let* ((depth (if (ribbit-p (aref vec 0))   ;; < THIS
		    (+ 1 (ribbit-depth (aref vec 0))) ;; < THIS
		    0))                               ;; < THIS
	 ...)
    ...))
...
```

that specifically. The problem here is that we're tacitly assuming that we'll never want to use a `ribbit` of `ribbit`s anywhere. And with the current implementation, if we ever do that, things will go silently but definitely wrong in various subtle ways. What we need to do in order to fight that is introduce explicit depth specification in all functions that deal with creating new `ribbit`s. This way we can have something like `(ribbit (ribbit 1) (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18) (ribbit :a :b :c :d))` without shit getting all weird on you. You'll even be able to `(cat (ribbit "a" "b") (ribbit (ribbit 1) (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18) (ribbit :a :b :c :d)))` and get the right result back. This is a fairly pervasive change. It touches `mk-ribbit`, `vecs->ribbit` (which suddenly has an inappropriate name), `take-across`, `repartition`, `cat` and `ribbit` itself (because it needs to call into the others). In fact, the only pieces that don't get touched are `prune-to`, `raise-to` and `max-reusable-level`. So, here's the revised source

```
;;;; ribbit.lisp
...
(defun take-across (depth rbs)
  (if (not (cdr rbs))
      (values (first rbs) nil)
      (let ((ct *m*)
	    (head nil)
	    (tail nil))
	(loop for rs on rbs while (> ct 1)
	   for r = (first rs)
	   for v = (ribbit-vec r)
	   do (cond ((> (length v) ct)
		     (push (subseq v 0 ct) head)
		     (setf tail (cons (mk-ribbit depth (subseq v ct)) (rest rs))
			   ct 0))
		    (t (push v head)
		       (decf ct (length v))))
	   finally (unless tail (setf tail rs)))
	(values
	 (mk-ribbit depth (apply #'concatenate 'vector (reverse head)))
	 tail))))

(defun repartition (depth rbs)
  (let ((rest rbs))
    (loop while rest for r = (pop rest)
       if (reusable? r) collect r
       else collect (multiple-value-bind (next rst) (take-across depth (cons r rest))
		      (setf rest rst)
		      next))))

(defun full-level? (ribbit) (= *m* (length (ribbit-vec ribbit))))

(defun reusable? (ribbit)
  (let* ((d (ribbit-depth ribbit))
	 (ct (length (ribbit-vec ribbit)))
	 (reusable-lv (or (= *m* ct) (= *n* ct))))
    (or (and (zerop d) reusable-lv)
	(let ((max (expt *m* (+ 1 d)))
	      (l (len ribbit)))
	  (or (= l max) (= l (- max 1))))
	(and reusable-lv
	     (every #'reusable? (ribbit-vec ribbit))))))

(defun compute-size-table (depth vec)
  (unless (or (zerop depth) (and (= *m* (length vec)) (every #'full-level? vec)))
    (coerce
     (let ((s 0))
       (loop for e across vec
	  do (incf s (len e)) collect s))
     'vector)))

(defun mk-ribbit (depth vec)
  (make-ribbit :size-table (compute-size-table depth vec) :depth depth :vec vec))

(defun ribbit-level (depth elems)
  (let ((es elems))
    (loop while es
       collect (mk-ribbit
		depth (coerce
		       (loop repeat *m* while es
			  for e = (pop es) collect e)
		       'vector)))))

(defun ribbit-from (depth elems)
  (let ((rbs (ribbit-level depth elems))
	(d depth))
    (loop while (cdr rbs)
       do (setf rbs (ribbit-level (incf d) rbs)))
    (first rbs)))

(defun ribbit (&rest elems)
  (if elems
      (ribbit-from 0 elems)
      (make-ribbit)))

...

(defun cat (a b)
  (let* ((d (ribbit-depth a))
	 (max-d (or (max-reusable-level a) 0)))
    (cond ((and (reusable? a) (>= d (ribbit-depth b)))
	   (mk-ribbit (+ d 1) (vector a (raise-to d b))))
	  ((reusable? a)
	   (ribbit-from (+ d 1) (cons a (prune-to d b))))
	  (t
	   (ribbit-from
	    (+ max-d 1)
	    (repartition
	     max-d (append
		    (prune-to max-d a)
		    (prune-to max-d (raise-to max-d b)))))))))
...
```


### `take-across` and `repartition`

Most of that's subtle, but irrelevant to the problem we actually care about. The relevant stuff is in `take-across` and `repartition`. It's either really easy to subtly fuck up your implementation of these internal primitives, or I'm a fucking idiot and did it wrong twice in a row. Lets zoom in on them, so at least I'll have a better understanding of where the mistakes crept in for next time.

```
...
(defun take-across (depth rbs)
  (if (not (cdr rbs))
      (values (first rbs) nil)
      (let ((ct *m*)
	    (head nil)
	    (tail nil))
	(loop for rs on rbs while (> ct 1)
	   for r = (first rs)
	   for v = (ribbit-vec r)
	   do (cond ((> (length v) ct)
		     (push (subseq v 0 ct) head)
		     (setf tail (cons (mk-ribbit depth (subseq v ct)) (rest rs))
			   ct 0))
		    (t (push v head)
		       (decf ct (length v))))
	   finally (unless tail (setf tail rs)))
	(values
	 (mk-ribbit depth (apply #'concatenate 'vector (reverse head)))
	 tail))))

(defun repartition (depth rbs)
  (let ((rest rbs))
    (loop while rest for r = (pop rest)
       if (reusable? r) collect r
       else collect (multiple-value-bind (next rst) (take-across depth (cons r rest))
		      (setf rest rst)
		      next))))
...
```

These versions are both mildly simplified by assuming the special variable `*m*` instead of accepting it as an argument and assuming they're dealing with `ribbit` sequences rather than polymorphic `ribbit`/`vector` sequences. That second one is due to the new implementations of `ribbit-level` and `ribbit-from` dealing with leaf vectors directly. `repartition` is also slightly more complex in that it deals with `reusable?` elements of its input directly, but `take-across` is _much_ simpler not having to deal with the distinction so I count that as a win.

The specific behavior we want out of this thing, which we apparently weren't getting from the earlier implementations, is

1. Directly collect any `reusable?` input `ribbit`s (thus preventing their copying in the output)
2. At any `ribbit` seams, re-jig some number of contiguous `ribbit`s to form new, `reusable?`-sized `ribbit`s for the next level (this implies copying pointers to their children)
3. Only re-jig as many `ribbit`s as you need to in order to get to freshly reusable sequences, and if you get to further reusables, collect them to avoid copying again
4. Don't bother re-jigging the last `ribbit` in an input sequence, even if it isn't `reusable?`, because there's no chance of doing better with no remaining material.

I'm honestly still mildly perplexed why previous versions didn't exhibit this, but it's fairly obvious that the above revisions do the right thing.

### Close-Up of the revised `cat`

```
...
(defun cat (a b)
  (let* ((d (ribbit-depth a))
	 (max-d (or (max-reusable-level a) 0)))
    (cond ((and (reusable? a) (>= d (ribbit-depth b)))
	   (mk-ribbit (+ d 1) (vector a (raise-to d b))))
	  ((reusable? a)
	   (ribbit-from (+ d 1) (cons a (prune-to d b))))
	  (t
	   (ribbit-from
	    (+ max-d 1)
	    (repartition
	     max-d (append
		    (prune-to max-d a)
		    (prune-to max-d (raise-to max-d b)))))))))
...
```

Which has also been pretty significantly simplified. We're down to three clauses, and none of them do anything perplexing at this point.

```
...
    (cond ((and (reusable? a) (>= d (ribbit-depth b)))
	   (mk-ribbit (+ d 1) (vector a (raise-to d b))))
...
```

When the left `ribbit` is reusable and the right `ribbit` is as deep or shallower, just make a new binary `ribbit` of the inputs. Easy.

```
...
	  ((reusable? a)
	   (ribbit-from (+ d 1) (cons a (prune-to d b))))
...
```

When the left `ribbit` is reusable and the right `ribbit` is deeper, prune the right `ribbit` down and make a new ribbit from the components that are merely as deep as the left `ribbit`. Again, easy.

```
...
	  (t
	   (ribbit-from
	    (+ max-d 1)
	    (repartition
	     max-d (append
		    (prune-to max-d a)
		    (prune-to max-d (raise-to max-d b))))))
...
```

Finally, the tough one. If the thing on the left is not reusable, find its maximum reusable depth (which might be zero), `repartition` the pruning of both `a` and `b` down to that depth and build up a new ribbit from that point.

That's not as good as you can do. There's still a couple screw-cases that this approach falls into, and it's sometimes possible to reuse more of the right tree than the maximum reusable depth of the left tree. The problem is that doing so involves getting a lot more elaborate in the handling of concatenation inputs, which may or may not be worth it depending on how much actual performance/memory it ends up saving, but it doesn't seem like it'll change the external interface at all. Which means I'm prepared to call it for now.

## Next Steps

So, that's the state of play. I was considering not posting this for a little while longer to finish up what I was thinking about, but this post has been brewing for about two/three weeks, and I've got other shit I want to write about. So around this point in the lifecycle, it's either time to [get my FILDI out](https://www.youtube.com/watch?v=RYlCVwxoL_g), or accept that it'll never be posted.

The code written up to this point can be found in two places:

1. [The `ribbit` repo](https://github.com/inaimathi/ribbit), which contains a partial implementation of just this data structure
2. [The `cl-fds` repo](https://github.com/inaimathi/cl-fds), which contains this along with a couple other useful functional data structures for Common Lisp

I'm going to finish out the `ribbit` implementation in both places, but my mid/long-term plan is to do more work on `cl-fds` so that it provides a pretty good, consistent interface to some new functional datastructures as well as functional interfaces to the built-in mutation-capable set pieces. I'll also very probably be putting together a follow-up article when one or both of those projects get to a place I can call "complete".
