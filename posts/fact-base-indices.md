Got through a late-night programming binge, followed by more of the same on my lunch break, followed by a couple more days of thought and work during downtime. It's up at my [fact-base implementation](https://github.com/Inaimathi/fact-base). Basically, we have indices now. Fairly naive, simple indices, but they should suffice for what I'm up to<a name="note-Tue-Mar-25-150513EDT-2014"></a>[|1|](#foot-Tue-Mar-25-150513EDT-2014).

Before we get to discussing any code, lets back up and discuss the idea of an `index` for a moment.

### The Idea of an `index`

An `index` in this context is an extra layer on top of our fact storage engine that keeps track of what we've put in/taken out in a way that makes certain things easier to look up. It's easier for fact-bases than it is for relational databases. Since every fact is made up of three components<a name="note-Tue-Mar-25-150529EDT-2014"></a>[|3|](#foot-Tue-Mar-25-150529EDT-2014), all we have to do is keep an index by one or two slots. What we're basically looking to do is a really fast lookup of some subset of all facts in a base based on one or two of those keys<a name="note-Tue-Mar-25-150535EDT-2014"></a>[|4|](#foot-Tue-Mar-25-150535EDT-2014). The way I've chosen to do it, after some advice from friends who've used systems something like this, is by maintaining hash tables<a name="note-Tue-Mar-25-150538EDT-2014"></a>[|5|](#foot-Tue-Mar-25-150538EDT-2014) in memory that give you shortcuts to some specified indices. We're trading space<a name="note-Tue-Mar-25-150541EDT-2014"></a>[|6|](#foot-Tue-Mar-25-150541EDT-2014) for time<a name="note-Tue-Mar-25-150544EDT-2014"></a>[|7|](#foot-Tue-Mar-25-150544EDT-2014).

### The Post-Explanation Version

This was a much different article initially; I was going to discuss some bone-headed intermediate states for this code before getting to the "final"<a name="note-Tue-Mar-25-150547EDT-2014"></a>[|8|](#foot-Tue-Mar-25-150547EDT-2014). It ended up looking like a stupid idea in this particular instance because the previous versions weren't things I'd consider running after having thought through it a bit more. 

I try to do that a fair amount these days. 

Not the stupid implementations thing, though that's also true. I mean sit down with another actual human being and talk them through the code I just wrote. It's not *quite* as good as blogging about it, but it's faster and turns up almost as many issues. It also has an added benefit that blogging doesn't seem to give me, which is showing me when I'm completely off my nut. Anyhow, here's what I had after I explained the thing to someone, poured some shower time into thinking about it, and drafted the first version of this post:

```lisp
(in-package #:fact-base)

(defclass index ()
  ((table :reader table :initform (make-hash-table :test 'equal))))

(defun make-index (indices)
  (let ((index (make-instance 'index)))
    (loop for ix in indices
       do (setf (gethash ix (table index))
                (make-hash-table :test 'equal)))
    index))

(defmethod indexed? ((state index) (ix-type symbol))
  (gethash ix-type (table state)))

(defun decide-index (&optional a b c)
  (cond ((and a b c) (list :abc a b c))
        ((and a b) (list :ab a b))
        ((and a c) (list :ac a c))
        ((and b c) (list :bc b c))
        ((and a) (list :a a))
        ((and b) (list :b b))
        ((and c) (list :c c))))

(defmethod format-index ((ix-type symbol) (fact list))
  (destructuring-bind (a b c) fact
    (case ix-type
      (:abc (list a b c))
      (:ab (list a b))
      (:ac (list a c))
      (:bc (list b c))
      (:a (list a))
      (:b (list b))
      (:c (list c)))))

(defmethod map-insert! ((facts list) (state index))
  (dolist (f facts) (insert! f state)))

(defmethod insert! ((fact list) (state index))
  (loop for ix being the hash-keys of (table state)
     for ix-table being the hash-values of (table state)
     do (push fact (gethash (format-index ix fact) ix-table))))

(defmethod delete! ((fact list) (state index))
  (loop for ix being the hash-keys of (table state)
     for ix-table being the hash-values of (table state)
     for formatted = (format-index ix fact)
     do (setf (gethash formatted ix-table) 
              (remove fact (gethash formatted ix-table) :test #'equal :count 1))
     unless (gethash formatted ix-table) do (remhash formatted ix-table)))

;;;;; Show methods
;; Entirely for debugging purposes. 
;; Do not use in production. 
;; Seriously.
(defmethod show (thing &optional (depth 0))
  (format t "~a~a" (make-string depth :initial-element #\space) thing))

(defmethod show ((tbl hash-table) &optional (depth 0))
  (loop for k being the hash-keys of tbl
     for v being the hash-values of tbl
     do (format t "~a~5@a ->~%" 
                (make-string depth :initial-element #\space) k)
     do (show v (+ depth 8))
     do (format t "~%")))

(defmethod show ((ix index) &optional (depth 0))
  (show (table ix) depth))
```

There, that's not so intimidating, is it? The actual interface to these indices is in [`fact-base.lisp`](https://github.com/Inaimathi/fact-base/blob/master/fact-base.lisp#L45-L58), but the above contains most of the functionality. First, ignore the `show` methods at the bottom there. That was just a piece of hackery to give me a usable visual representation of an index while I was debugging this beast. Lets start at the top.

```lisp
(defclass index ()
  ((table :reader table :initform (make-hash-table :test 'equal))))

(defun make-index (indices)
  (let ((index (make-instance 'index)))
    (loop for ix in indices
       do (setf (gethash ix (table index))
                (make-hash-table :test 'equal)))
    index))

(defmethod indexed? ((state index) (ix-type symbol))
  (gethash ix-type (table state)))
```

An `index` has a table of bindings. You'd call `make-index` in a way resembling `(make-index '(:a :b :bc :ac))`, which would give you back an `index` with room to dissect a fact base into chunklets keyed off of


1.   the first element
1.   the second element
1.   the second then the third element
1.   the first then the third element


No, I have no idea if I'll ever actually need a setup like this. The `indexed?` method takes an `index` and an `ix-type` symbol and tells you whether the given `index` is tracking that particular type of lookup.

```lisp
(defun decide-index (&optional a b c)
  (cond ((and a b c) (list :abc a b c))
        ((and a b) (list :ab a b))
        ((and a c) (list :ac a c))
        ((and b c) (list :bc b c))
        ((and a) (list :a a))
        ((and b) (list :b b))
        ((and c) (list :c c))))

(defmethod format-index ((ix-type symbol) (fact list))
  (destructuring-bind (a b c) fact
    `(,ix-type
      ,@ (case ix-type
           (:abc (list a b c))
           (:ab (list a b))
           (:ac (list a c))
           (:bc (list b c))
           (:a (list a))
           (:b (list b))
           (:c (list c))))))
```

Those both map an index type to the components they'll need for insertion/lookup. I've thought about factoring out the obvious pattern, and even wrote some prototype code to do it, but it turns out that for 6 indices, the macrology involved is more complicated than the trivial lookup-table thing. If `fact-base` were an arbitrary storage system, this would probably be complicated enough to `macro` away, but the whole point is that I'm only ever storing triples. Which means I'm never going to need more `index` types than this, and usually much less.

```lisp
(defmethod map-insert! ((facts list) (state index))
  (dolist (f facts) (insert! f state)))

(defmethod insert! ((fact list) (state index))
  (loop for ix being the hash-keys of (table state)
     for ix-table being the hash-values of (table state)
     do (push fact (gethash (format-index ix fact) ix-table))))

(defmethod delete! ((fact list) (state index))
  (loop for ix being the hash-keys of (table state)
     for ix-table being the hash-values of (table state)
     for formatted = (format-index ix fact)
     do (setf (gethash formatted ix-table) 
              (remove fact (gethash formatted ix-table) :test #'equal :count 1))
     unless (gethash formatted ix-table) do (remhash formatted ix-table)))
```

These three utility methods at the end are exactly what you'd expect. `insert!` takes a fact and an `index` and inserts one into the other, into how-many-ever particular lookups that `index` is tracking. `map-insert!` is a shorthand for inserting a bunch of facts at once into the same `index`. And finally, `delete!` takes a fact and removes it from all `index` lookups, then cleans up empty lookup lists.

And that's that. You've got a quick run-through of how this works out in practice [here](https://github.com/Inaimathi/fact-base#example-usage). I get the feeling I'll be talking about deltas, forking and the applications of fact bases before long, but we shall see.


* * *
##### Footnotes
1 - <a name="foot-Tue-Mar-25-150513EDT-2014"></a>[|back|](#note-Tue-Mar-25-150513EDT-2014) - Granted, because "what I'm up to" at this point "an almost trivial semi-anonymous forum for a local meetup group", and "an almost trivial notebook-style REPL for common lisp", that's true of almost any data storage technique ever, but still. The naive, index-less storage was giving [cl-kanren](https://github.com/Inaimathi/cl-kanren-trs)<a name="note-Tue-Mar-25-150519EDT-2014"></a>[|2|](#foot-Tue-Mar-25-150519EDT-2014) a bit more trouble than I wanted it to when I pushed the stored entry count past 10000 or so. Which is not satisfactory. So yeah, this index is basically a search-space optimization for my database traversals. I'll let you know how it goes.

2 - <a name="foot-Tue-Mar-25-150519EDT-2014"></a>[|back|](#note-Tue-Mar-25-150519EDT-2014) - Which I'll also have to talk about before long, if for no reason other than to get some ideas out of my head temporarily.

3 - <a name="foot-Tue-Mar-25-150529EDT-2014"></a>[|back|](#note-Tue-Mar-25-150529EDT-2014) - The second two might be compound structures, but it's still only three top-level elements.

4 - <a name="foot-Tue-Mar-25-150535EDT-2014"></a>[|back|](#note-Tue-Mar-25-150535EDT-2014) - If you have none of the three components of the fact you're looking for, you can't do better than "all facts"; if you have all of them, you already have the fact you're looking for. So we're only interested in the other two cases.

5 - <a name="foot-Tue-Mar-25-150538EDT-2014"></a>[|back|](#note-Tue-Mar-25-150538EDT-2014) - There's no particular reason you couldn't use some tree structure if you like, but hashes are easy, and they come with Common lisp, so...

6 - <a name="foot-Tue-Mar-25-150541EDT-2014"></a>[|back|](#note-Tue-Mar-25-150541EDT-2014) - Which is in ample supply these days.

7 - <a name="foot-Tue-Mar-25-150544EDT-2014"></a>[|back|](#note-Tue-Mar-25-150544EDT-2014) - Which is always a vanishingly scarce resource.

8 - <a name="foot-Tue-Mar-25-150547EDT-2014"></a>[|back|](#note-Tue-Mar-25-150547EDT-2014) - Which isn't really final any more; I've updated it several times since the stuff here.
