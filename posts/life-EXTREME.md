I'm sick today.

I fucking *hate* being sick.

So, to make myself feel better, I'm profiling things. Specifically, the Common Lisp version of Life I wrote last time. I'll be using [Emacs](http://www.gnu.org/software/emacs/) and [SLIME](http://common-lisp.net/project/slime/), but I'm pretty sure you can do at least some of this using [`time`](http://www.gnu.org/software/emacs/) in whatever `REPL` you've got lying around.

```lisp
(defpackage :life (:use :cl))
(in-package :life)

(defun moore-neighborhood (cell)
  (let ((r '(-1 0 1)))
    (mapcan
         (lambda (delta-x)
           (loop for delta-y in r
              unless (and (= delta-x 0) (= delta-y 0))
              collect (cons (+ (car cell) delta-x) (+ (cdr cell) delta-y))))
         r)))

(defun frequencies (cells)
  (let ((h (make-hash-table :test #'equal)))
    (loop for c in cells
       do (incf (gethash c h 0)))
    h))

(defun life-step (cells)
  (let ((f (frequencies (mapcan #'moore-neighborhood cells))))
    (loop for k being the hash-keys in f
       when (or 
             (= (gethash k f) 3) 
             (and (= (gethash k f) 2) (member k cells :test #'equal)))
         collect k)))

(defun print-world (live-cells &optional (world-size 10))
  (dotimes (y world-size)
    (dotimes (x world-size)
      (if (member (cons x y) live-cells :test #'equal)
          (format t "X")
          (format t ".")))
    (format t "~%")))

(defun run-life (world-size steps cells)
  (when (&lt; 0 steps)
    (run-life world-size (- steps 1) (life-step cells))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; data related
(defun .cells->list (filename)
  (with-open-file (stream filename)
    (apply #'append
           (loop with y = 0
              for line = (read-line stream nil 'eof) until (eq line 'eof)
              unless (char= (aref line 0) #\!)
              collect (let ((line (loop for x from 0
                                     for char being the elements of line
                                     when (char= char #\O) collect (cons x y)))) 
                        (incf y)
                        line)))))

(defparameter *blinker* '((1 . 2) (2 . 2) (3 . 2)))
(defparameter *glider* '((1 . 0) (2 . 1) (0 . 2) (1 . 2) (2 . 2)))
(defparameter *gosper-glider-gun* 
  '((24 . 0) (22 . 1) (24 . 1) (12 . 2) (13 . 2) (20 . 2) (21 . 2) (34 . 2)
    (35 . 2) (11 . 3) (15 . 3) (20 . 3) (21 . 3) (34 . 3) (35 . 3) (0 . 4) (1 . 4)
    (10 . 4) (16 . 4) (20 . 4) (21 . 4) (0 . 5) (1 . 5) (10 . 5) (14 . 5) (16 . 5)
    (17 . 5) (22 . 5) (24 . 5) (10 . 6) (16 . 6) (24 . 6) (11 . 7) (15 . 7)
    (12 . 8) (13 . 8)))
```

  [Gosper's gun](http://www.conwaylife.com/wiki/Gosper_glider_gun) is the simplest emitter I could find, and I need to test that sort of thing to convince myself of the performance of this abstract machine. The `.cells->list` function exists purely to convert files like [this](http://www.conwaylife.com/patterns/gosperglidergun.cells) into inputs suitable for our peculiar model of the Life world. You'll also notice that I stripped all printing code from `run-life`; I'm not interested in how inefficient the conversion between sparse-array and grid is, and I imagine that it would have been the main cost-center had I kept it. Lets hop into the `REPL`

```lisp
CL-USER> (load "life.lisp")
T
CL-USER> (in-package :life)
#&lt;PACKAGE "LIFE">
```

Remember to turn on profiling with `M-x slime-profile-package life`, and answer yes to the options it asks about.


```lisp
LIFE> (run-life 50 5000 *gosper-glider-gun*)
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
Control stack guard page temporarily disabled: proceed with caution
; Evaluation aborted on #&lt;SB-KERNEL::CONTROL-STACK-EXHAUSTED {100F0230D3}>.
```

Ok, I guess that's not entirely unexpected. After all, `run-life` is still recursive, and Common Lisp doesn't guarantee tail-call optimization. Still, we probably got some pretty decent data, even from a failed attempt. `M-x slime-profile-report` says

```
  seconds  |     gc     |    consed   |   calls   |  sec/call  |  name  
-------------------------------------------------------------
    11.820 |      0.000 |     590,592 |     3,322 |   0.003558 | LIFE::LIFE-STEP
     4.082 |      1.796 | 534,176,080 |     3,322 |   0.001229 | LIFE::FREQUENCIES
     0.887 |      0.428 | 378,046,784 | 1,073,904 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |  12,360,624 |     3,322 |   0.000000 | LIFE::RUN-LIFE
-------------------------------------------------------------
    16.790 |      2.224 | 925,174,080 | 1,083,870 |            | Total

estimated total profiling overhead: 1.99 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.84e-6s total profiling, 8.24e-7s internal profiling
```

`frequencies` and `life-step` are obviously the culprits here, and since we now know what the cost-centers are, we can mitigate them. Discounting micro-optimization<a name="note-Thu-Dec-13-133118EST-2012"></a>[|1|](#foot-Thu-Dec-13-133118EST-2012), there are essentially three ways to optimize a piece of code for time<a name="note-Thu-Dec-13-133125EST-2012"></a>[|2|](#foot-Thu-Dec-13-133125EST-2012)


1.   reduce the number of traversals of your corpus
1.   reduce the time taken per traversal
1.   eliminate sequential data dependencies and do more traversals at once through parallelism


We won't be doing the third because the Game of Life problem doesn't inherently lend itself to it; you need to compute step N before you can compute step N+1, and that can't really be helped. We might be able to take advantage of parallelism in a couple of places *during* each step, but that tends to have its own costs associated and typically doesn't pay off except on very large data sets.

There are a bunch of ways to do one and two. We can re-write pieces of our code with tight loops; reducing readability somewhat but removing traversals where we can. We can change the representation of our corpus to something more easily searchable, or we can be more aggressive up-front about throwing out elements we know we won't need later. We'll probably end up doing *all* of that.

But first...

```lisp
(defun run-life (world-size steps cells)
  (declare (ignore world-size))
  (let ((world (copy-list cells)))
    (loop 
       repeat steps
       do (setf world (life-step world)))
    world))
```

That should get around our snippy little stack warning.

```lisp
LIFE> (run-life 50 5000 *gosper-glider-gun*)
```

now returns with a list of 884 living cells. Which makes perfect sense, since this is a generator we're testing. The profiler says "moo".

```
  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name  
---------------------------------------------------------------
    37.257 |      0.000 |       622,592 |     5,000 |   0.007451 | LIFE::LIFE-STEP
     5.156 |      0.588 | 1,058,382,096 |     5,000 |   0.001031 | LIFE::FREQUENCIES
     1.692 |      0.564 |   821,550,624 | 2,315,504 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |             0 |         1 |   0.000000 | LIFE::RUN-LIFE
---------------------------------------------------------------
    44.105 |      1.152 | 1,880,555,312 | 2,325,505 |            | Total

estimated total profiling overhead: 4.28 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.84e-6s total profiling, 8.24e-7s internal profiling
```

So frequencies takes up a fuckton of `cons`es, and the second most execution time, right behind `life-step`. This preliminary survey probably wasn't worth doing on a program this size; just looking at our defined functions would probably have convinced you who the culprits are and aren't.

```lisp
(defun life-step (cells)
  (let ((f (frequencies (mapcan #'moore-neighborhood cells))))
    (loop for k being the hash-keys in f
       when (or 
             (= (gethash k f) 3) 
             (and (= (gethash k f) 2) (member k cells :test #'equal)))
         collect k)))
```

First off, `(mapcan #'moore-neighborhood cells)` is one traversal of the input. Ok, not too much we can do about that, we need to do it at least once. Calling `frequencies` on that is a second traversal, and we can probably tweak our code enough that those two happen at once. The subsequent `loop` call is another traversal of `(* ~8 cells)`. We do actually need to traverse `f`, but it's currently longer than it needs to be because it's a `hash-table` that contains *all* cells in any living cells' Moore neighborhood. Fixing that would mean tweaking `frequencies` so that it automatically threw out cells with fewer than two or more than three neighbors, since those couldn't possibly be alive next time. Finally, it might not be entirely obvious, but `member` is a linked-list operation that traverses its list argument each time its called. I put it in the tail end of an `and`, which means it should only be getting called for cells with two neighbors, but each time it *does* get called, it traverses some part of `cells`; all of it, if its argument wasn't alive last time. We'll fix *that* by using a data type that has a more efficient membership check than a linked list.

Ok, firstly, do it in one traversal

```lisp
(defun compute-frequencies (cells)
  (let ((h (make-hash-table :test #'equal)))
    (loop for a-cell in cells
       do (loop for c in (moore-neighborhood a-cell)
             do (incf (gethash c h 0))))
    h))
```

Oh, by the by, I have to apologize for the poor `frequencies` implementation last time. It turns out that Common Lisp has something like Python's `defaultdict` built-in; `gethash` takes an optional third argument which it returns as the default value. Which is nice because `(incf (gethash [key] [table] 0))` will do exactly what you think it should. Now then, one traversal eliminated, lets hook the new thing into `life-step`

```lisp
(defun life-step (cells)
  (let ((f (compute-frequencies cells)))
    (loop for k being the hash-keys in f
       when (or 
             (= (gethash k f) 3) 
             (and (= (gethash k f) 2) (member k cells :test #'equal)))
         collect k)))
```

How did we do?

```
  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name  
---------------------------------------------------------------
    33.800 |      0.012 |    42,812,560 |     5,000 |   0.006760 | LIFE::LIFE-STEP
     4.404 |      0.172 | 1,098,791,040 |     5,000 |   0.000881 | LIFE::COMPUTE-FREQUENCIES
     1.276 |      0.072 |   738,782,288 | 2,315,504 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |             0 |         1 |   0.000000 | LIFE::RUN-LIFE
---------------------------------------------------------------
    39.481 |      0.256 | 1,880,385,888 | 2,325,505 |            | Total

estimated total profiling overhead: 4.28 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.84e-6s total profiling, 8.24e-7s internal profiling
```

Not bad, actually. `compute-frequencies` conses more, but saves us about half a second over the programs' running time. A direct result of this is a ~5 second drop in computing time for a 5000 step Gosper gun. Not too shabby for five minutes' worth of work. Next up, lets try to ignore irrelevant cells. That means not adding them to the result hash unless they've got at least two neighbors, and it means knocking them out if they have more than three. In other words, we'll be wanting more `hash-table`.

```lisp
(defun compute-frequencies (cells)
  (let ((lonely (make-hash-table :test #'equal)) 
        (h (make-hash-table :test #'equal)))
    (loop for a-cell in cells
       do (loop for c in (moore-neighborhood a-cell)
             do (let ((res (incf (gethash c lonely 0))))
                  (cond 
                    ((or (= res 2) (= res 3)) (setf (gethash c h) res))
                    ((= res 4) (remhash c h))))))
    h))
```

My memory is gonna cry, but the processor will have a slightly easier time of this, because it will only need to deal with cells that have a decent shot of being alive in the next iteration.

```
  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name  
---------------------------------------------------------------
    32.981 |      0.000 |    37,683,200 |     5,000 |   0.006596 | LIFE::LIFE-STEP
     5.931 |      0.288 | 1,718,346,016 |     5,000 |   0.001186 | LIFE::COMPUTE-FREQUENCIES
     1.220 |      0.080 |   638,848,352 | 2,315,504 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |             0 |         1 |   0.000000 | LIFE::RUN-LIFE
---------------------------------------------------------------
    40.133 |      0.368 | 2,394,877,568 | 2,325,505 |            | Total

estimated total profiling overhead: 4.28 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.84e-6s total profiling, 8.24e-7s internal profiling
```

Hm. Very slightly easier, it turns out. All the time we buy in reducing the number of cells we need to traverse seems to get eaten by the more complex check. I'm not entirely sure it was worth it, but lets keep that optimization where it is for now. We've got one trick left up our sleeves, and it's changing the representation of `cells`. At the moment, it's represented by that Lisp mainstay, the Linked List. In Clojure, we'd use a set, but we don't have ready access to those here. So, we'll need to use the next best thing; a data structure with quick insertion and constant-time lookup.

```lisp
(defun compute-frequencies (cells)
  (let ((lonely (make-hash-table :test #'equal)) 
        (h (make-hash-table :test #'equal)))
    (loop for a-cell being the hash-keys of cells
       do (loop for c in (moore-neighborhood a-cell)
             do (let ((res (incf (gethash c lonely 0))))
                  (cond 
                    ((or (= res 2) (= res 3)) (setf (gethash c h) res))
                    ((= res 4) (remhash c h))))))
    h))

(defun cells->hash (cells)
  (let ((h (make-hash-table :test #'equal :size 800)))
    (loop for c in cells
         do (setf (gethash c h) 0))
    h))

(defun life-step (cells)
  (let ((f (compute-frequencies cells)))
    (loop for k being the hash-keys in f
       when (and (= (gethash k f) 2) (not (gethash k cells)))
       do (remhash k f))
    f))

(defun run-life (world-size steps cells)
  (declare (ignore world-size))
  (let ((world (cells->hash cells)))
    (loop 
       repeat steps
       do (setf world (life-step world)))
    world))
```

Subtle changes happen to each of those functions to support the overarching change, which is that we're using `hash-table`s everywhere now. Because `member` has to traverse the entire list of `cells`, while `gethash` is constant time, this should knock the shit out of our performance problems.

```
  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name  
---------------------------------------------------------------
     5.871 |      0.268 | 1,716,630,608 |     5,000 |   0.001174 | LIFE::COMPUTE-FREQUENCIES
     1.252 |      0.108 |   641,132,304 | 2,315,504 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |             0 |     5,000 |   0.000000 | LIFE::LIFE-STEP
     0.000 |      0.000 |             0 |         1 |   0.000000 | LIFE::RUN-LIFE
     0.000 |      0.000 |        30,080 |         1 |   0.000000 | LIFE::CELLS->HASH
---------------------------------------------------------------
     7.123 |      0.376 | 2,357,792,992 | 2,325,506 |            | Total

estimated total profiling overhead: 4.28 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.84e-6s total profiling, 8.24e-7s internal profiling
```

Boom. Headshot.

Granted, we're still `cons`ing like crazy, but removing that `member` check has pushed `life-step` down so low that it actually takes up significantly fewer resources than friggin `moore-neighborhood`. We've cut our total running time from ~40 seconds to under 10. In fact, lets crank this fucker up to eleven.

```lisp
LIFE> (run-life 50 50000 *gosper-glider-gun*)

  seconds  |     gc     |      consed     |    calls    |  sec/call  |  name  
-------------------------------------------------------------------
   371.779 |     23.251 |  96,030,964,864 |      40,182 |   0.009252 | LIFE::COMPUTE-FREQUENCIES
    74.690 |      8.977 |  44,912,583,584 | 136,410,636 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |          77,920 |      40,182 |   0.000000 | LIFE::LIFE-STEP
     0.000 |      0.000 |               0 |           1 |   0.000000 | LIFE::RUN-LIFE
     0.000 |      0.000 |          21,840 |           1 |   0.000000 | LIFE::CELLS->HASH
-------------------------------------------------------------------
   446.468 |     32.228 | 140,943,648,208 | 136,491,002 |            | Total

estimated total profiling overhead: 251.14 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.84e-6s total profiling, 8.24e-7s internal profiling
```

Aaaaand I got bored. Yeah, that took a while. What were you expecting? We get plenty of new cells each iteration, and we don't actually throw any away unless they die naturally. Which doesn't happen often when you're dealing with a generator. That's the last "optimization" we can make; instead of `(declare (ignore`ing the `world-size`, we can use it to forget cells that lie outside of our target area. It won't help all patterns, but the `*gosper-glider-gun*` won't create a Malthusian disaster for our computing resources.

```lisp
(defun run-life (world-size steps cells)
  (let ((world (cells->hash cells)))
    (loop 
       repeat steps
       do (setf world (life-step world world-size)))
    world))

(defun life-step (cells world-size)
  (let ((f (compute-frequencies cells)))
    (loop for k being the hash-keys in f
       when (or 
             (> (car k) world-size)
             (> (cdr k) world-size)
             (and (= (gethash k f) 2) (not (gethash k cells))))
       do (remhash k f))
    f))
```

There. Now we just chuck all the cells that run off the edge of the world. Provided the world is small enough, that keeps the population from exploding tribble style.

```lisp
LIFE> (run-life 50 50000 *gosper-glider-gun*)

  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name  
---------------------------------------------------------------
     8.938 |      0.500 | 2,520,410,944 |    50,000 |   0.000179 | LIFE::COMPUTE-FREQUENCIES
     1.801 |      0.100 | 1,100,839,008 | 3,352,906 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |             0 |    50,000 |   0.000000 | LIFE::LIFE-STEP
     0.000 |      0.000 |             0 |         1 |   0.000000 | LIFE::RUN-LIFE
     0.000 |      0.000 |        52,144 |         1 |   0.000000 | LIFE::CELLS->HASH
---------------------------------------------------------------
    10.739 |      0.600 | 3,621,302,096 | 3,452,908 |            | Total

estimated total profiling overhead: 6.35 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.84e-6s total profiling, 8.24e-7s internal profiling
```

Pretty good right? All things considered? Before we go, lets take a look at how this approach compares to the traditional grid Life technique. Here's [the code pulled from Rosetta Code](http://rosettacode.org/wiki/Conway%27s_Game_of_Life#Common_Lisp), using a two-dimensional array instead of a list of the living. Oh, I've commented out printing of intermediate steps, and included a 50x50 field with the Gosper Gun, just to make sure this is as even as possible. Oh, I also have to reset the starting world for `:life-grid` each time, since its process is destructive.

```lisp
(defpackage :life-grid (:use :cl))
(in-package :life-grid)

(defun next-life (array &optional results)
  (let* ((dimensions (array-dimensions array))
         (results (or results (make-array dimensions :element-type 'bit))))
    (destructuring-bind (rows columns) dimensions
      (labels ((entry (row col)
                 "Return array(row,col) for valid (row,col) else 0."
                 (if (or (not (&lt; -1 row rows))
                         (not (&lt; -1 col columns)))
                   0
                   (aref array row col)))
               (neighbor-count (row col &aux (count 0))
                 "Return the sum of the neighbors of (row,col)."
                 (dolist (r (list (1- row) row (1+ row)) count)
                   (dolist (c (list (1- col) col (1+ col)))
                     (unless (and (eql r row) (eql c col))
                       (incf count (entry r c))))))
               (live-or-die? (current-state neighbor-count)
                 (if (or (and (eql current-state 1)
                              (&lt;=  2 neighbor-count 3))
                         (and (eql current-state 0)
                              (eql neighbor-count 3)))
                   1
                   0)))
        (dotimes (row rows results)
          (dotimes (column columns)
            (setf (aref results row column)
                  (live-or-die? (aref array row column)
                                (neighbor-count row column)))))))))
 
(defun print-grid (grid &optional (out *standard-output*))
  (destructuring-bind (rows columns) (array-dimensions grid)
    (dotimes (r rows grid)
      (dotimes (c columns (terpri out))
        (write-char (if (zerop (aref grid r c)) #\+ #\#) out)))))
 
(defun run-life (&optional world (iterations 10) (out *standard-output*))
  (let* ((world (or world (make-array '(10 10) :element-type 'bit)))
         (result (make-array (array-dimensions world) :element-type 'bit)))
    (do ((i 0 (1+ i))) ((eql i iterations) world)
;;      (terpri out) (print-grid world out)
      (psetq world (next-life world result)
             result world))))

(defparameter *gosper-glider-gun*
  (let ((w (make-array '(50 50) :element-type 'bit)))
    (loop for (x . y) in '((24 . 0) (22 . 1) (24 . 1) (12 . 2) (13 . 2) (20 . 2) (21 . 2) (34 . 2)
                           (35 . 2) (11 . 3) (15 . 3) (20 . 3) (21 . 3) (34 . 3) (35 . 3) (0 . 4) (1 . 4)
                           (10 . 4) (16 . 4) (20 . 4) (21 . 4) (0 . 5) (1 . 5) (10 . 5) (14 . 5) (16 . 5)
                           (17 . 5) (22 . 5) (24 . 5) (10 . 6) (16 . 6) (24 . 6) (11 . 7) (15 . 7)
                           (12 . 8) (13 . 8))
       do (setf (aref w y x) 1))
    w))
```


Lets start small

```
  seconds  |     gc     |   consed  | calls |  sec/call  |  name  
-------------------------------------------------------
     0.005 |      0.000 | 1,588,176 |    50 |   0.000103 | LIFE::COMPUTE-FREQUENCIES
     0.002 |      0.000 |   786,432 | 2,534 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |    20,768 |     1 |   0.000000 | LIFE::CELLS->HASH
     0.000 |      0.000 |         0 |    50 |   0.000000 | LIFE::LIFE-STEP
     0.000 |      0.000 |         0 |     1 |   0.000000 | LIFE::RUN-LIFE
-------------------------------------------------------
     0.007 |      0.000 | 2,395,376 | 2,636 |            | Total

estimated total profiling overhead: 0.00 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.808e-6s total profiling, 7.04e-7s internal profiling
```
<br />
```
  seconds  |     gc     |   consed   | calls |  sec/call  |  name  
--------------------------------------------------------
     0.048 |      0.000 | 23,997,056 |    50 |   0.000959 | LIFE-GRID::NEXT-LIFE
     0.000 |      0.000 |          0 |     1 |   0.000000 | LIFE-GRID::RUN-LIFE
--------------------------------------------------------
     0.048 |      0.000 | 23,997,056 |    51 |            | Total

estimated total profiling overhead: 0.00 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.808e-6s total profiling, 7.04e-7s internal profiling
```

* * *

```lisp
LIFE> (run-life 50 5000 *gosper-glider-gun*)
  seconds  |     gc     |    consed   |  calls  |  sec/call  |  name  
-----------------------------------------------------------
     0.828 |      0.016 | 241,614,336 |   5,000 |   0.000166 | LIFE::COMPUTE-FREQUENCIES
     0.237 |      0.032 | 118,408,160 | 334,156 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |      57,248 |       1 |   0.000000 | LIFE::CELLS->HASH
     0.000 |      0.000 |           0 |   5,000 |   0.000000 | LIFE::LIFE-STEP
     0.000 |      0.000 |           0 |       1 |   0.000000 | LIFE::RUN-LIFE
-----------------------------------------------------------
     1.064 |      0.048 | 360,079,744 | 344,158 |            | Total

estimated total profiling overhead: 0.62 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.808e-6s total profiling, 7.04e-7s internal profiling
```
<br />
```lisp
LIFE-GRID> (run-life *gosper-glider-gun* 5000)
  seconds  |     gc     |     consed    | calls |  sec/call  |  name  
-----------------------------------------------------------
     4.760 |      0.288 | 2,400,164,160 | 5,000 |   0.000952 | LIFE-GRID::NEXT-LIFE
     0.014 |      0.000 |             0 |     1 |   0.014479 | LIFE-GRID::RUN-LIFE
-----------------------------------------------------------
     4.775 |      0.288 | 2,400,164,160 | 5,001 |            | Total

estimated total profiling overhead: 0.01 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.808e-6s total profiling, 7.04e-7s internal profiling
```

Hm. Honestly wasn't expecting to be cleaning the grids' clock yet, but we're using about a quarter of the time and about a sixth of the memory. [Remember](http://science.slc.edu/~jmarshall/courses/2002/spring/cs50/BigO/index.html), at the low-end of the spectrum, the difference between a poor algorithm and a good one isn't very big. If you've got a corpus of length 20, it really doesn't matter whether you pick [bubble-sort](http://en.wikipedia.org/wiki/Bubble_sort), [quicksort](http://en.wikipedia.org/wiki/Quicksort) or [timsort](http://en.wikipedia.org/wiki/Timsort). In fact, you'd expect the better algorithms to do mildly worse on smaller data sets, since their optimizations don't have as much opportunity to pay for themselves.

Lets crank it up a bit to figure out how these numbers diverge.

```lisp
LIFE> (run-life 50 50000 *gosper-glider-gun*)
  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name  
---------------------------------------------------------------
     8.812 |      0.456 | 2,418,859,120 |    50,000 |   0.000176 | LIFE::COMPUTE-FREQUENCIES
     2.265 |      0.092 | 1,202,364,272 | 3,352,906 |   0.000001 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |        58,240 |         1 |   0.000000 | LIFE::CELLS->HASH
     0.000 |      0.000 |             0 |    50,000 |   0.000000 | LIFE::LIFE-STEP
     0.000 |      0.000 |             0 |         1 |   0.000000 | LIFE::RUN-LIFE
---------------------------------------------------------------
    11.077 |      0.548 | 3,621,281,632 | 3,452,908 |            | Total

estimated total profiling overhead: 6.24 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.808e-6s total profiling, 7.04e-7s internal profiling
```
<br />
```lisp
LIFE-GRID> (run-life *gosper-glider-gun* 50000)
  seconds  |     gc     |     consed     |  calls |  sec/call  |  name  
-------------------------------------------------------------
    48.140 |      3.116 | 24,001,592,224 | 50,000 |   0.000963 | LIFE-GRID::NEXT-LIFE
     0.025 |      0.000 |              0 |      1 |   0.024799 | LIFE-GRID::RUN-LIFE
-------------------------------------------------------------
    48.165 |      3.116 | 24,001,592,224 | 50,001 |            | Total

estimated total profiling overhead: 0.09 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.808e-6s total profiling, 7.04e-7s internal profiling
```

The optimized gridless approach is holding steady at about 1/4 time taken and about 1/6 memory used. Again, because this is a garbage collected language, those affect each other. Each trip of the collector adds precious seconds to the tally of consumed resources, so being a memory *hog* does come back to bite you in the ass even if you're not directly optimizing for space. Last one. Don't try this at home, unless you have something to do for a little while.

```lisp
LIFE> (run-life 50 5000000 *gosper-glider-gun*)
  seconds  |     gc     |      consed     |    calls    |  sec/call  |  name  
-------------------------------------------------------------------
   924.769 |     45.233 | 252,714,140,464 |   5,000,000 |   0.000185 | LIFE::COMPUTE-FREQUENCIES
   145.698 |     10.865 | 109,648,457,760 | 335,415,406 |   0.000000 | LIFE::MOORE-NEIGHBORHOOD
     0.000 |      0.000 |              64 |   5,000,000 |   0.000000 | LIFE::LIFE-STEP
     0.000 |      0.000 |               0 |           1 |   0.000000 | LIFE::RUN-LIFE
     0.000 |      0.000 |          20,320 |           1 |   0.000000 | LIFE::CELLS->HASH
-------------------------------------------------------------------
  1070.467 |     56.098 | 362,362,618,608 | 345,415,408 |            | Total

estimated total profiling overhead: 635.56 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.84e-6s total profiling, 8.24e-7s internal profiling
```
<br />
```lisp
LIFE-GRID> (run-life *gosper-glider-gun* 5000000)
  seconds  |     gc     |       consed      |   calls   |  sec/call  |  name  
-------------------------------------------------------------------
  4818.453 |    340.823 | 2,400,161,918,448 | 5,000,000 |   0.000964 | LIFE-GRID::NEXT-LIFE
     5.769 |      0.000 |             3,008 |         1 |   5.768999 | LIFE-GRID::RUN-LIFE
-------------------------------------------------------------------
  4824.222 |    340.823 | 2,400,161,921,456 | 5,000,001 |            | Total

estimated total profiling overhead: 9.04 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.808e-6s total profiling, 7.04e-7s internal profiling
```

We're still the same fraction better, but the numbers have increased pretty drastically. I know which one I'd rather rely on for crunching large life patterns.

These aren't all the optimizations we could pull, by the way. If we wanted to do better, we could inline `moore-neighborhood` within `compute-frequencies`, and we could prevent it from consing nearly as much by using its integers directly rather than allocating a fresh list of `cons`es every time. A particular optimization we could do that would be relatively difficult with the grid approach would be to check for a barren world before each step; if we ever get an empty set as a result, we can return immediately rather than spinning wheels until we reach the end of our step counter. It would be easy for us to do, since we just need to check `(= 0 (hash-table-count cells))`, whereas doing it the obvious way would add another traversal of the corpus per step for the already much slower traditional approach.

Ok. I'm going to sleep. I was *going* to do a similar writeup using the [Haskell profiler](http://www.haskell.org/ghc/docs/7.0.1/html/users_guide/profiling.html), but that took a lot out of me. Hopefully, you've learned something from all this. Fresh code up at [my Life github](https://github.com/Inaimathi/life). Feel free to beat those numbers. I'd be particularly interested if someone wanted to do some micro-optimization on the same problem and put forth an explanatory article.

* * *
##### Footnotes
1 - <a name="foot-Thu-Dec-13-133118EST-2012"></a>[|back|](#note-Thu-Dec-13-133118EST-2012) - Which is a huge topic in its own right, and involves things like hand-optimizing memory cache interactions for minimum fall-through and various other low-level, machine oriented optimizations.

2 - <a name="foot-Thu-Dec-13-133125EST-2012"></a>[|back|](#note-Thu-Dec-13-133125EST-2012) - We're optimizing for time because space tends to be cheap, and running things fast is fun.
