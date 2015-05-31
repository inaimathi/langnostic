You can't implement a lazy sort.

Not really.

I mean, ok, yes, you can implement a sort that defers as much work as possible via that destructured approach I touched on last time and which I will expand on in a minute, but that's **not a lazy sort**.

It has certain characteristics which seem intrinsic to sorts that prevent it from behaving the way you'd expect an actual lazy function to behave. For starters, it doesn't save you any memory. In fact, you need to pull some fairly fancy footwork to write a functional, deferring sort that doesn't waste much more memory than the in-place version<a name="note-Thu-May-02-214518EDT-2013"></a>[|1|](#foot-Thu-May-02-214518EDT-2013). Next, it can't consume infinite sequences. That's to do with the definition of a sort; I can't see a good way of sorting an infinite sequence without backtracking, which isn't practical in the wild. Finally, it doesn't seem to save you as much time in composition as a regular lazy function would. It has to consume the entire relevant input stream, do some preliminary processing on it<a name="note-Thu-May-02-214521EDT-2013"></a>[|2|](#foot-Thu-May-02-214521EDT-2013), then hand you a stream of outputs.

The end result is a sort function that defers a lot of its work, and does save you a lot of time if you only happen to want the first bit of a list sorted, *and* it even saves you a little bit of time if you want to run several lazy functions in succession on its output, but it doesn't quite do enough to get the label "lazy" without a massive asterisk next to it.

### How to implement a lazy* sort

First up, here's the Python code.

```python
from copy import copy

def eager(aList, pred=lambda a, b: a > b):
    arr = copy(aList)
    count = len(aList)
    heapify(arr, count, pred)
    end = count - 1
    while end > 0:
        __swap(arr, end, 0)
        end -= 1
        sift(arr, 0, end, pred)
    arr.reverse()
    return arr

def lazy(aList, pred=lambda a, b: a > b):
    arr = copy(aList)
    count = len(arr)
    heapify(arr, count, pred)
    end = count - 1
    while end > 0:
        yield arr[0]
        __swap(arr, end, 0)
        end -= 1
        sift(arr, 0, end, pred)
    yield arr[0]

def heapify(arr, count, pred=lambda a, b: a > b):
    start = (count - 1)/2
    while start >= 0:
        sift(arr, start, count-1, pred)
        start -= 1
    return arr

def sift(arr, start, end, pred=lambda a, b: a > b):
    root = start
    while root * 2 + 1 &lt;= end:
        child = root * 2 + 1
        target = root
        if pred(arr[target], arr[child]):
            target = child
        if child+1 &lt;= end and pred(arr[target], arr[child+1]):
            target = child + 1
        if not target == root:
            __swap(arr, root, target)
            root = target
        else:
            return arr

def __swap(arr, a, b):
    arr[a], arr[b] = arr[b], arr[a]
```

This is a direct translation of the [Wikipedia Heapsort pseudo-code](http://en.wikipedia.org/wiki/Heapsort#Pseudocode). And yes, I used Python because it's close enough to pseudo-code that I could do a working, line-by-line translation. It was really, *really* tempting to just implement `eager` as `list(lazy(aList, pred))`, but that wouldn't have told me what I wanted to know for the next bit.


```python
>>> import heapsort, cProfile
>>> from random import Random
>>> sample = [Random().randint(0, 1000) for i in xrange(0, 5000)]
>>> cProfile.run("heapsort.eager(sample)")
         172098 function calls in 0.103 seconds

   Ordered by: standard name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000    0.103    0.103 &lt;string>:1(&lt;module>)
        1    0.000    0.000    0.000    0.000 copy.py:113(_copy_with_constructor)
        1    0.000    0.000    0.000    0.000 copy.py:66(copy)
        1    0.002    0.002    0.013    0.013 heapsort.py:27(heapify)
   107567    0.014    0.000    0.014    0.000 heapsort.py:3(&lt;lambda>)
        1    0.004    0.004    0.103    0.103 heapsort.py:3(eager)
     7499    0.069    0.000    0.097    0.000 heapsort.py:34(sift)
    57023    0.015    0.000    0.015    0.000 heapsort.py:49(__swap)
        1    0.000    0.000    0.000    0.000 {len}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
        1    0.000    0.000    0.000    0.000 {method 'get' of 'dict' objects}
        1    0.000    0.000    0.000    0.000 {method 'reverse' of 'list' objects}


>>> cProfile.run("heapsort.lazy(sample)")
         3 function calls in 0.000 seconds

   Ordered by: standard name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000    0.000    0.000 &lt;string>:1(&lt;module>)
        1    0.000    0.000    0.000    0.000 heapsort.py:15(lazy)
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}


>>> cProfile.run("list(heapsort.lazy(sample))")
         177097 function calls in 0.107 seconds

   Ordered by: standard name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.001    0.001    0.107    0.107 &lt;string>:1(&lt;module>)
        1    0.000    0.000    0.000    0.000 copy.py:113(_copy_with_constructor)
        1    0.000    0.000    0.000    0.000 copy.py:66(copy)
   107567    0.015    0.000    0.015    0.000 heapsort.py:15(&lt;lambda>)
     5001    0.004    0.000    0.106    0.000 heapsort.py:15(lazy)
        1    0.002    0.002    0.014    0.014 heapsort.py:27(heapify)
     7499    0.070    0.000    0.098    0.000 heapsort.py:34(sift)
    57023    0.015    0.000    0.015    0.000 heapsort.py:49(__swap)
        1    0.000    0.000    0.000    0.000 {len}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
        1    0.000    0.000    0.000    0.000 {method 'get' of 'dict' objects}


>>> 
```

This is that work deferring thing happening. The call to `heapsort.lazy` doesn't give you a sorted list; it gives you a generator you can traverse to get that list. The call to `heapsort.eager` does give you the whole sorted list, and takes very slightly less time than the lazy version if you happen to need the whole list. As I said before though; if you're only after the first 10% or so elements, there's no contest in terms of execution time, *even if* you're trying to be semi-functional by copying out the input instead of destructively modifying it.

Oh, before anyone gets the wrong idea

```python
>>> cProfile.run("sorted(sample)")
         3 function calls in 0.002 seconds

   Ordered by: standard name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000    0.002    0.002 &lt;string>:1(&lt;module>)
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
        1    0.002    0.002    0.002    0.002 {sorted}


>>> 
```

My stupid little heapsort implementation isn't meant to showcase how slow Python is or anything like that. It's a learning exercise to show how you'd go about implementing a deferred sort in principle, not go into the nuts-and-bolts tuning process that comes once you've got your algorithm and data structures down. In other words, see it as a direct comparison of a shitty sort to the equivalent-except-lazy shitty sort.

### Second verse, same as the first

Except with more parentheses. And I actually try to think through the problem rather than mindlessly parroting back an algorithm outline pilfered from Wikipedia.

```lisp
(defpackage :heapsort (:use :cl))
(in-package :heapsort)

(defun heapsort-lazy (seq &optional (predicate #'>))
  (let* ((len (length seq))
         (heap-vector (heapify seq len predicate)))
    (lambda ()
      (decf len)
      (heappop! heap-vector len predicate))))

(defun heapsort-eager (seq &optional (predicate #'>))
  (let* ((len (length seq))
         (heap-vector (heapify seq len predicate)))
    (loop for i from (- len 1) downto 0
       unless (= 0 i) 
       collect (heappop! heap-vector i predicate))))

(defun heapify (seq len predicate)
  (loop with buf = (make-array len :adjustable t :fill-pointer len)
     for ix from 0
     for elem in seq do (setf (aref buf ix) elem)
     do (loop with i = ix
           while (> i 0)
           for parent = (heap-parent i)
           until (compare buf parent i predicate)
           do (swap! buf parent i)
           do (setf i parent))
     finally (return buf)))

(defun sift! (heap-vector start end predicate)
  (loop with ix = start
     until (> (+ 1 (* 2 ix)) end)
     while (loop for child-ix in (heap-children-descending heap-vector ix end predicate)
              when child-ix
              do (unless (or (compare heap-vector ix child-ix predicate))
                   (swap! heap-vector ix child-ix)
                   (setf ix child-ix)
                   (return t))
              finally (return nil))))

(defun heappop! (heap-vector last predicate)
  (swap! heap-vector 0 last)
  (sift! heap-vector 0 (- last 1) predicate)
  (vector-pop heap-vector))

(defun heap-children-descending (heap-vector ix bounds predicate)
  (let ((child-l (+ 1 (* 2 ix)))
        (child-r (+ 2 (* 2 ix))))
    (cond ((> child-l bounds) nil)
          ((> child-r bounds) (list child-l nil))
          (t (if (compare heap-vector child-l child-r predicate) 
                 (list child-l child-r)
                 (list child-r child-l))))))

(defun heap-parent (n)
  (- (ceiling (/ n 2)) 1))

(defun compare (arr ix-a ix-b &optional (predicate #'>))
  (funcall predicate (aref arr ix-a) (aref arr ix-b)))

(defun swap! (arr ix-a ix-b)
  (rotatef (aref arr ix-a) (aref arr ix-b)))
```

I'm not going to bother showing you the profiling on this one. Rest assured that the results on this and the Python version were very similar; the eager version is marginally faster than the lazy version at sorting the entire list and handing it to you, but has a *massive* disadvantage if you only want some small chunklet of the complete list. Also, the built-in sort beats both by several orders of magnitude.

For those of you who, like me, have never worked with heaps before<a name="note-Thu-May-02-214622EDT-2013"></a>[|3|](#foot-Thu-May-02-214622EDT-2013), here's some basic theory. A heap is actually two things:


-   **A tree-based data structure in which each parent node is ordered with respect to its children.** This is the easier-than-sorted-but-still-useful property mentioned earlier; children aren't ordered with respect to each other, and if you're watching the [wiki illustration](http://en.wikipedia.org/wiki/File:Sorting_heapsort_anim.gif) for the first time ever, you might be forgiven for thinking that step 1 involves randomly re-arranging your input. It's very easy to pull out the next element; it's the root. However, every time you pop the root, you need to do some re-juggling to maintain the heap property.
-   **A way of packing said tree-based data-structure into a 1-d array.** It's not painfully obvious, so I figured I'd make this part explicit: you pack a heap into a vector by designating `(aref vector (+ 1 (* i 2)))` and `(aref vector (+ 2 (* i 2)))` to be the children of `(aref vector i)`. This is faster than navigating an actual pointer tree, but it makes the structure of the code a bit counter-intuitive to the uninitiated, since it's talking about indices in non-obvious ways rather than talking about parents and children.


Now then, most of the actual `heapsort.lisp` code is implementing a heap. Again, just for educational purposes, I'm sure there's a variable-predicate heap implementation floating around somewhere even though I haven't looked for it<a name="note-Thu-May-02-214631EDT-2013"></a>[|4|](#foot-Thu-May-02-214631EDT-2013). In fact, lets take a look at the top-level functions before diving into that code, just to get it out of the way.

```lisp
(defun heapsort-eager (seq &optional (predicate #'>))
  (let* ((len (length seq))
         (heap-vector (heapify seq len predicate)))
    (loop for i from (- len 1) downto 0
       unless (= 0 i) 
       collect (heappop! heap-vector i predicate))))
```

We take a list, `heapify` it, then collect `heappop!`ed elements and return the result. Nothing to see here, it's exactly what you'd expect from a sort.

```lisp
(defun heapsort-lazy (seq &optional (predicate #'>))
  (labels ((next (heap-vector)
             (lambda ()
               (cons (heappop! heap-vector (- (length heap-vector) 1) predicate)
                     (next heap-vector)))))
    (funcall (next (heapify seq (length seq) predicate)))))
```

The lazy version is mildly more interesting 

```lisp
(defun heapsort-lazy (seq &optional (predicate #'>))
  (let* ((len (length seq))
         (heap-vector (heapify seq len predicate)))
    (lambda ()
      (decf len)
      (heappop! heap-vector len predicate))))
```

Common Lisp doesn't have the notion of a generator in the same sense as Python, but a lambda with a closure around it does just as well for our purposes. You keep calling it to get at the `next` element, and it eventually throws an `invalid-array-index-error` that you need to deal with in some way. This actually seems like the most reasonable solution here; the alternative is something like

```lisp
(defun heapsort-lazy (seq &optional (predicate #'>))
  (let* ((len (length seq))
         (heap-vector (heapify seq len predicate)))
    (lambda ()
      (if (>= 0 len)
          (values nil nil)
          (progn (decf len)
                 (values (heappop! heap-vector len predicate) t))))))
```

It's tempting to *just* return `NIL`, but then there's no way for a caller to disambiguate between "The next element in your sequence is `NIL`" and "There are no more elements in the sequence. So `NIL`". My kingdom for a `Maybe` monad, as annoying as most people seem to consider them.

Anyhow, onward.

```lisp
(defun heapify (seq len predicate)
  (loop with buf = (make-array len :adjustable t :fill-pointer len)
     for ix from 0
     for elem in seq do (setf (aref buf ix) elem)
     do (loop with i = ix
           while (> i 0)
           for parent = (heap-parent i)
           until (compare buf parent i predicate)
           do (swap! buf parent i)
           do (setf i parent))
     finally (return buf)))
```

My definition of `heapify` doesn't use a call to `sift!` anywhere, in blatant defiance of the standard implementation. Really, I should have factored that middle bit out into `heappush!`, because that's what it amounts to. You start with an empty heap, insert new elements, and compare each new element to its parent, calling `swap!` until you have something that respects the Heap Property.

```lisp
(defun swap! (arr ix-a ix-b)
  (rotatef (aref arr ix-a) (aref arr ix-b)))
```

`swap!` is implemented in terms of `rotatef`; it takes an array and two indices, and swaps the appropriate array cells. `heap-parent` shouldn't surprise you at all if you were paying attention when I explained what a heap actually is

```lisp
(defun heap-parent (n)
  (- (ceiling (/ n 2)) 1))
```

And `heappop!` swaps the first element with the last, calls `sift!` on everything but the last element, then runs `vector-pop` to return the last element and shorten the vector.

```lisp
(defun heappop! (heap-vector last predicate)
  (swap! heap-vector 0 last)
  (sift! heap-vector 0 (- last 1) predicate)
  (vector-pop heap-vector))
```

Which just leaves the `sift!` procedure, and its utility functions.

```lisp
(defun sift! (heap-vector start end predicate)
  (loop with ix = start
     until (> (+ 1 (* 2 ix)) end)
     while (loop for child-ix in (heap-children-descending heap-vector ix end predicate)
              when child-ix
              do (unless (or (compare heap-vector ix child-ix predicate))
                   (swap! heap-vector ix child-ix)
                   (setf ix child-ix)
                   (return t))
              finally (return nil))))
```

It takes a `start` parameter, since the pseudo-code did the same, but I didn't find myself calling it with anything other than `0`, so maybe that was a bit of a waste. To be fair, that pseudo also uses `sift!` as part of insertion, rather than doing the more straight-forward parent comparison, which might explain the difference. This is literally the most impenetrable part of this program, and it's crucial, because unless you understand this you won't get how the entire system produces sorted output. I'll take it slow, just in case. Feel free to stop reading here if you know this already.

```lisp
  ...
  (loop with ix = start
  ...
```

We're starting at the beginning

```lisp
     ...
     until (> (+ 1 (* 2 ix)) end)
     while [something-huge]
     ...
```

and going either until we get to the end of the heap, or until we get to the point where no more calls to `swap!` are needed. That makes sense because if we don't need to `swap!` further, and we started with a heap, we know that the rest of it already satisfies the Heap Property and therefore doesn't need to be heaped again. If we've gotten to the end, then we know that we just tried to `sift!` the smallest element in the heap, which is why it's at the bottom.

```lisp
     while (loop for child-ix in (heap-children-descending heap-vector ix end predicate)
     ...
```

That's going to do something to each `child-ix` in the result of `heap-children-descending` function.

```lisp
(defun heap-children-descending (heap-vector ix bounds predicate)
  (let ((child-l (+ 1 (* 2 ix)))
        (child-r (+ 2 (* 2 ix))))
    (cond ((> child-l bounds) nil)
          ((> child-r bounds) (list child-l nil))
          (t (if (compare heap-vector child-l child-r predicate) 
                 (list child-l child-r)
                 (list child-r child-l))))))
```

This isn't in the pseudocode either; I ended up deciding to `compare` the children for size so that the parent `compare`s against the greatest first so that the Heap Property is more easily preserved. The return value is a list of children in descending order, and we also handle the case where this particular parent only has one child. Oh, also, `compare` is just a utility function that helps me compare two `array` elements by index

```lisp
(defun compare (arr ix-a ix-b &optional (predicate #'>))
  (funcall predicate (aref arr ix-a) (aref arr ix-b)))
```

Now then.

```lisp
              ...
              when child-ix
              do (unless (or (compare heap-vector ix child-ix predicate))
                   (swap! heap-vector ix child-ix)
                   (setf ix child-ix)
                   (return t))
              finally (return nil))))
              ...
```

If there are any children, `compare` the parent to them in descending order. If one of the children is bigger than the parent, `swap!` them, set `ix` to the index of that child (which now contains our new parent), and `return` `t` (this tells the upper `loop` to keep comparing, since we now need to compare that new `ix` to *its* children). Otherwise, `return` `nil` to signal that we've achieved a new heap.

And that's that. As long as the Heap Property is respected, the next item by whatever predicate was passed is always one quasi-efficient `heappop!` away, and as a result, a call to `heapsort-lazy` never does any more work than it absolutely needs to while still providing flexible sorted output.

*I* learned a lot implementing this. Hopefully the write-up does something for someone else out there too.


* * *
##### Footnotes
1 - <a name="foot-Thu-May-02-214518EDT-2013"></a>[|back|](#note-Thu-May-02-214518EDT-2013) - I don't incidentally, both the Python and Common Lisp code I'm going to show you use [generators](http://wiki.python.org/moin/Generators). That is, side-effect-dependent lazy sequences with the restriction that you can only traverse them once, mostly as a result of those side-effects.
2 - <a name="foot-Thu-May-02-214521EDT-2013"></a>[|back|](#note-Thu-May-02-214521EDT-2013) - I've been saying "heapsort", though that's strictly speaking not the case. What you need for a lazy* sort is an intermediate structure that


-   is easier create than a full sort in the general case
-   has some property which makes it useful than an arbitrary collection when it comes to finding `next`


A heap does both of those, and it's fairly easy to understand, so I picked it, but it's not necessarily the only or best approach. I just don't know any other ones.

3 - <a name="foot-Thu-May-02-214622EDT-2013"></a>[|back|](#note-Thu-May-02-214622EDT-2013) - Graphic Design degree, remember? They didn't offer Data Structures 101, as much as I would have enjoyed that much more than Art History for Consumers.

4 - <a name="foot-Thu-May-02-214631EDT-2013"></a>[|back|](#note-Thu-May-02-214631EDT-2013) - As a note, Python *doesn't* seem to. The standard `[heapq](http://docs.python.org/2/library/heapq.html)` doesn't provide [a way of pulling out a `key` from the sorted structures, *or* a way change out predicates](http://code.activestate.com/lists/python-list/162387/). The [standard Python wisdom](http://stackoverflow.com/a/14189741/190887) seems to be pulling keys out yourself, storing your actual values in a way indexable by said keys, then sorting the keys instead.
