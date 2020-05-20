I mentioned at the [tail end of last piece](/posts/more-on-clj#polymorphic-operators) that a set of polymorphic data operators is what we want in the correctness sense, but is pretty poor from the performance standpoint. It turns out that there's a bunch of options aimed at improving the performance of generic functions. There's [`static-dispatch`](https://github.com/alex-gutev/static-dispatch), [`fast-generic-functions`](https://github.com/marcoheisig/fast-generic-functions), and the archived but still available [`inlined-generic-function`](https://github.com/guicho271828/inlined-generic-function).

### The operators

So, we want `==`, that's obvious. But the end usability goal also _probably_ demands `lookup`, `insert` and `len`. I'm leaving out some things we'll realistically _want_ but that would be more complicated to implement[^for-instance-things-like]. I want a minimal, benchmarkeabla set for my current purposes.

[^for-instance-things-like]: Things like polymorphic `map`, `reduce`, `mapcat`, `concat`, possibly `->list` and `conj`. First, these might be more complicated to implement, but second, I'm not entirely sure I want to or how specifically to do so. Most of the functions I list in this footnote could be implemented using an underlying `next`. Part of the thought for these systems is putting them together in a way that lets the end users define as little as they can to get the full benefit.

The naive implementations of these look like

```
(defgeneric == (a b))
(defgeneric lookup (container key))
(defgeneric insert (container elem))
(defgeneric len (container))

(defmethod == (a b) (equalp a b))
(defmethod == ((a number) (b number)) (= a b))
(defmethod == ((a string) (b string)) (string= a b))
(defmethod == ((a character) (b character)) (char= a b))
(defmethod == ((a symbol) (b symbol)) (eq a b))
(defmethod == ((a list) (b list)) (equal a b))
(defmethod == ((a cl-hamt:hash-dict) (b cl-hamt:hash-dict))
  (cl-hamt:dict-eq a b :value-test #'==))
(defmethod == ((a cl-hamt:hash-set) (b cl-hamt:hash-set))
  (cl-hamt:set-eq a b))

(defmethod lookup ((container list) key)
  (nth key container))
(defmethod lookup ((container hash-table) key)
  (gethash key container))
(defmethod lookup ((container cl-hamt:hash-set) key)
  (cl-hamt:set-lookup container key))
(defmethod lookup ((container cl-hamt:hash-dict) key)
  (cl-hamt:dict-lookup container key))

(defmethod insert ((container list) elem) (cons elem container))
(defmethod insert ((container hash-table) k/v)
  ;; NOTE - strictly, this should copy the hash-table in order to be functional
  ;;        Not right now.
  (setf (gethash (car k/v) container) (cdr k/v)))
(defmethod insert ((container cl-hamt:hash-dict) k/v)
  (cl-hamt:dict-insert container (car k/v) (cdr k/v)))
(defmethod insert ((container cl-hamt:hash-set) elem)
  (cl-hamt:set-insert container elem))

(defmethod len ((container list)) (length container))
(defmethod len ((container hash-table)) (hash-table-count container))
(defmethod len ((container cl-hamt:hash-set)) (cl-hamt:set-size container))
(defmethod len ((container cl-hamt:hash-dict)) (cl-hamt:dict-size container))
```

We can debate about what the arglist of `insert` _should_ look like or what `len` _should_ be called later, ut this is a decent start.

The `static-dispatch` version basically just involves using `static-dispatch:defmethod` rather than the built-in `cl:defmethod`.

```
(static-dispatch:defmethod static-dispatch-== (a b) (equalp a b))
(static-dispatch:defmethod static-dispatch-== ((a number) (b number)) (= a b))
(static-dispatch:defmethod static-dispatch-== ((a string) (b string)) (string= a b))
(static-dispatch:defmethod static-dispatch-== ((a character) (b character)) (char= a b))
(static-dispatch:defmethod static-dispatch-== ((a symbol) (b symbol)) (eq a b))
(static-dispatch:defmethod static-dispatch-== ((a list) (b list)) (equal a b))
(static-dispatch:defmethod static-dispatch-== ((a cl-hamt:hash-dict) (b cl-hamt:hash-dict))
  (cl-hamt:dict-eq a b :value-test #'==))
(static-dispatch:defmethod static-dispatch-== ((a cl-hamt:hash-set) (b cl-hamt:hash-set))
  (cl-hamt:set-eq a b))

(static-dispatch:defmethod static-dispatch-lookup ((container list) key)
  (nth key container))
(static-dispatch:defmethod static-dispatch-lookup ((container hash-table) key)
  (gethash key container))
(static-dispatch:defmethod static-dispatch-lookup ((container cl-hamt:hash-set) key)
  (cl-hamt:set-lookup container key))
(static-dispatch:defmethod static-dispatch-lookup ((container cl-hamt:hash-dict) key)
  (cl-hamt:dict-lookup container key))

(static-dispatch:defmethod static-dispatch-insert ((container list) elem) (cons elem container))
(static-dispatch:defmethod static-dispatch-insert ((container hash-table) k/v)
  ;; NOTE - strictly, this should copy the hash-table in order to be functional
  ;;        Not right now.
  (setf (gethash (car k/v) container) (cdr k/v)))
(static-dispatch:defmethod static-dispatch-insert ((container cl-hamt:hash-dict) k/v)
  (cl-hamt:dict-insert container (car k/v) (cdr k/v)))
(static-dispatch:defmethod static-dispatch-insert ((container cl-hamt:hash-set) elem)
  (cl-hamt:set-insert container elem))

(static-dispatch:defmethod static-dispatch-len ((container list)) (length container))
(static-dispatch:defmethod static-dispatch-len ((container hash-table)) (hash-table-count container))
(static-dispatch:defmethod static-dispatch-len ((container cl-hamt:hash-set)) (cl-hamt:set-size container))
(static-dispatch:defmethod static-dispatch-len ((container cl-hamt:hash-dict)) (cl-hamt:dict-size container))
```

If we wanted to allow users to define their own types, we'd also have to make sure that they use `static-dispatch:defmethod`, because there would otherwise be odd interoperability issues.

For [`fast-generic-functions`](https://github.com/marcoheisig/fast-generic-functions), we've got some options. First, we can make `inline` declarations as part of each method, and second, we need to `seal-domain` each method that we want to run faster. There's a couple considerations here. First off, we have to be more stringent about the domains of our methods. In particular, and this really only affects the internal implementation of `==`, we can't have partially overlapping domains. So, if we have something that specializes on `(number number)`, we can't _also_ have a `(number t)` or `(t t)` specializer. Second, once a `domain` is `seal`ed, no touching it again. Third, some domains can't be sealed. In particular

```
CLJ> (defmethod fgf-== ((a cl-hamt:hash-dict) (b cl-hamt:hash-dict))
  (cl-hamt:dict-eq a b :value-test #'==))
#<STANDARD-METHOD CLJ::FGF-== (CL-HAMT:HASH-DICT CL-HAMT:HASH-DICT) {100332D7E3}>
CLJ> (fast-generic-functions:seal-domain #'fgf-== '(cl-hamt:hash-dict cl-hamt:hash-dict))

The assertion SEALABLE-METAOBJECTS::SUCCESS failed.
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Retry assertion.
 1: [RETRY] Retry SLIME REPL evaluation request.
 2: [*ABORT] Return to SLIME's top level.
 3: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1004F19B63}>)

Backtrace:
  0: (SB-KERNEL:ASSERT-ERROR SEALABLE-METAOBJECTS::SUCCESS NIL NIL NIL)
  1: ((:METHOD SEALABLE-METAOBJECTS:SPECIALIZER-INTERSECTIONP (CLASS CLASS)) #<STANDARD-CLASS CL-HAMT:HASH-SET> #<STANDARD-CLASS CL-HAMT:HASH-DICT>) [fast-method]
  2: ((FLET "WRAPPER9" :IN SEALABLE-METAOBJECTS:DOMAIN-INTERSECTIONP) #<STANDARD-CLASS CL-HAMT:HASH-SET> #<STANDARD-CLASS CL-HAMT:HASH-DICT>)
  3: (SB-IMPL::%MAP-FOR-EFFECT #<CLOSURE (FLET "WRAPPER9" :IN SEALABLE-METAOBJECTS:DOMAIN-INTERSECTIONP) {7FFFEFDADB8B}> ((#1=#<STANDARD-CLASS CL-HAMT:HASH-SET> #1#) (#2=#<STANDARD-CLASS CL-HAMT:HASH-DICT>..
  4: (SB-KERNEL:%MAP NIL #<CLOSURE (FLET "WRAPPER9" :IN SEALABLE-METAOBJECTS:DOMAIN-INTERSECTIONP) {7FFFEFDADB8B}> (#1=#<STANDARD-CLASS CL-HAMT:HASH-SET> #1#) (#1=#<STANDARD-CLASS CL-HAMT:HASH-DICT> #1#)) ..
  5: ((:METHOD SEALABLE-METAOBJECTS:DOMAIN-INTERSECTIONP (SEALABLE-METAOBJECTS:DOMAIN SEALABLE-METAOBJECTS:DOMAIN)) #<SEALABLE-METAOBJECTS:DOMAIN CL-HAMT:HASH-SET CL-HAMT:HASH-SET> #<SEALABLE-METAOBJECTS:D..
  6: ((:METHOD SEALABLE-METAOBJECTS:SEAL-DOMAIN (SEALABLE-METAOBJECTS:SEALABLE-GENERIC-FUNCTION SEALABLE-METAOBJECTS:DOMAIN)) #<FAST-GENERIC-FUNCTIONS:FAST-GENERIC-FUNCTION CLJ::FGF-== (7)> #<SEALABLE-META..
  7: ((SB-PCL::EMF SEALABLE-METAOBJECTS:SEAL-DOMAIN) #<unused argument> #<unused argument> #<FAST-GENERIC-FUNCTIONS:FAST-GENERIC-FUNCTION CLJ::FGF-== (7)> #<SEALABLE-METAOBJECTS:DOMAIN CL-HAMT:HASH-DICT CL..
  8: ((:METHOD SEALABLE-METAOBJECTS:SEAL-DOMAIN :AROUND (SEALABLE-METAOBJECTS:SEALABLE-GENERIC-FUNCTION SEALABLE-METAOBJECTS:DOMAIN)) #<FAST-GENERIC-FUNCTIONS:FAST-GENERIC-FUNCTION CLJ::FGF-== (7)> #<SEALA..
  9: (SB-INT:SIMPLE-EVAL-IN-LEXENV (SEALABLE-METAOBJECTS:SEAL-DOMAIN (FUNCTION FGF-==) (QUOTE (CL-HAMT:HASH-DICT CL-HAMT:HASH-DICT))) #<NULL-LEXENV>)
 10: (EVAL (SEALABLE-METAOBJECTS:SEAL-DOMAIN (FUNCTION FGF-==) (QUOTE (CL-HAMT:HASH-DICT CL-HAMT:HASH-DICT))))
 --more--
```

This... might not be as horrible as it seems. I'm not prepared to write it off yet. In any case, I'm not about to do any of this manually. Not even for the benchmark.


```
(defun seal-all-domains (generic-function)
  (loop for m in (closer-mop:generic-function-methods generic-function)
     do (format t "SEALING ~s~%" (mapcar #'class-name (closer-mop:method-specializers m)))
     do (ignore-errors
	  (progn (fast-generic-functions:seal-domain
		  generic-function
		  (mapcar #'class-name (closer-mop:method-specializers m)))
		 (format t "  Sealed...~%"))) ))

(defmacro -definlineable (name (&rest args) &body body)
  `(defmethod ,name ,args
       (declare (fast-generic-functions:method-properties fast-generic-functions:inlineable))
       ,@body))

(defgeneric fgf-== (a b)
  (:generic-function-class fast-generic-functions:fast-generic-function))
(defgeneric fgf-lookup (container key)
  (:generic-function-class fast-generic-functions:fast-generic-function))
(defgeneric fgf-insert (container elem)
  (:generic-function-class fast-generic-functions:fast-generic-function))
(defgeneric fgf-len (container)
  (:generic-function-class fast-generic-functions:fast-generic-function))

(-definlineable fgf-== (a b) (equalp a b))
(-definlineable fgf-== ((a number) (b number)) (= a b))
(-definlineable fgf-== ((a string) (b string)) (string= a b))
(-definlineable fgf-== ((a character) (b character)) (char= a b))
(-definlineable fgf-== ((a symbol) (b symbol)) (eq a b))
(-definlineable fgf-== ((a list) (b list)) (equal a b))
(-definlineable fgf-== ((a cl-hamt:hash-dict) (b cl-hamt:hash-dict))
  (cl-hamt:dict-eq a b :value-test #'==))
(-definlineable fgf-== ((a cl-hamt:hash-set) (b cl-hamt:hash-set))
  (cl-hamt:set-eq a b))
(seal-all-domains #'fgf-==)

(-definlineable fgf-lookup ((container list) key)
  (nth key container))
(-definlineable fgf-lookup ((container hash-table) key)
  (gethash key container))
(-definlineable fgf-lookup ((container cl-hamt:hash-set) key)
  (cl-hamt:set-lookup container key))
(-definlineable fgf-lookup ((container cl-hamt:hash-dict) key)
  (cl-hamt:dict-lookup container key))
(seal-all-domains #'fgf-lookup)

(-definlineable fgf-insert ((container list) elem) (cons elem container))
(-definlineable fgf-insert ((container hash-table) k/v)
  ;; NOTE - strictly, this should copy the hash-table in order to be functional
  ;;        Not right now.
  (setf (gethash (car k/v) container) (cdr k/v)))
(-definlineable fgf-insert ((container cl-hamt:hash-dict) k/v)
  (cl-hamt:dict-insert container (car k/v) (cdr k/v)))
(-definlineable fgf-insert ((container cl-hamt:hash-set) elem)
  (cl-hamt:set-insert container elem))
(seal-all-domains #'fgf-insert)

(-definlineable fgf-len ((container list)) (length container))
(-definlineable fgf-len ((container hash-table)) (hash-table-count container))
(-definlineable fgf-len ((container cl-hamt:hash-set)) (cl-hamt:set-size container))
(-definlineable fgf-len ((container cl-hamt:hash-dict)) (cl-hamt:dict-size container))
(seal-all-domains #'fgf-insert)
```

The benchmark suite is going to be two parts. First, one hitting only equality, and second, one hitting a bunch of round-trip `lookup`/`insert`/`len` operations, each of which will call equality under the hood.

### The Benchmark Setup

Now that we've got our operators, we need to test between four and six approaches to optimizing them, depending on how you count. So lets set this up.

```
(defun fgn-fn (a b)
  (declare (inline fgf-==))
  (fgf-== a b))
(defun static-fn (a b)
  (declare (inline static-dispatch-==))
  (static-dispatch-== a b))
(defun naive-fn (a b)
  (declare (inline ==))
  (== a b))
(defun built-in-fn (a b)
  (declare (inline =))
  (= a b))

(defun equality-benchmark ()
  (loop for f in (list #'naive-fn #'built-in-fn #'fgn-fn #'static-fn)
     do (loop repeat 1000000
	   do (funcall f (random 256) (random 256)))))

(defun lookup/insert/len-benchmark (equality insert lookup len &key (times 10000))
  (let* ((elem-gen (test-utils:a-member (test-utils:a-member
					 test-utils:a-keyword
					 test-utils:a-number
					 test-utils:a-string)))
	 (pair-gen (test-utils:a-pair elem-gen elem-gen)))
    (loop repeat times
       do (let* ((map (alist->map
		       (test-utils:generate (test-utils:a-list pair-gen))
		       :equality equality))
		 (inserted (funcall insert map (cons :test-key :test-value))))
	    (list (funcall len map)
		  (funcall lookup inserted :test-key)
		  (funcall len inserted))))))
```

Let me highlight a few things, just to make them explicit.

First, the `equality-benchmark` only compares numbers. I wanted a single type so that we could have a general comparison benchmark that includes `fgf-==`. I had to play some tricks to achieve this; in particular, I had to comment out the `fgf-== (a b)` method, because generalizing a method like this makes it unsealable.

Second, the latter benchmark does not run against a sealed `fgf-==` method. Because there can't be a default fall-through case, I can't use a sealed `fgf-==` to compare keys on a polymorphic `cl-hamt:hash-dict`; it would throw errors.

Third, that second benchmark makes use of `generator`s exposed through `test-utils`. If you don't want to set this up yourself, check out the [`benchmark.lisp` file in the main `clj` repo](https://github.com/inaimathi/clj/blob/master/src/benchmark.lisp). You should be able to just

```
CL-USER> (ql:quickload :clj) (in-package :clj) (load "benchmark.lisp")
```

to get in the right place. From there, since I'm a [`SLIME`](https://common-lisp.net/project/slime/) user, I evaluated `slime-profile-package CLJ`, followed by a `slime-profile-reset` for good measure, then ran

```
CLJ> (equality-benchmark)
```
followed by `slime-profile-report`, followed by `slime-profile-reset`, followed by

```
CLJ> (lookup/insert/len-benchmark #'static-dispatch-== #'static-dispatch-insert #'static-dispatch-lookup #'static-dispatch-len :times 10000)
NIL
CLJ> (lookup/insert/len-benchmark #'fgf-== #'fgf-insert #'fgf-lookup #'fgf-len :times 10000)
NIL
CLJ> (lookup/insert/len-benchmark #'== #'insert #'lookup #'len :times 10000)
NIL
```

and a final `slime-profile-report`.

Here are the results.

### The Benchmark

```
CLJ> (equality-benchmark)
NIL
  seconds  |     gc     |   consed   |   calls   |  sec/call  |  name
------------------------------------------------------------
     0.134 |      0.004 | 81,380,672 | 1,000,000 |   0.000000 | CLJ::FGF-==
     0.058 |      0.000 |     32,768 | 1,000,000 |   0.000000 | CLJ::STATIC-DISPATCH-==
     0.031 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ::BUILT-IN-FN
     0.027 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ::FGN-FN
     0.025 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ::STATIC-FN
     0.024 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ:==
     0.000 |      0.000 |          0 |         1 |   0.000000 | CLJ::EQUALITY-BENCHMARK
     0.000 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ::NAIVE-FN
------------------------------------------------------------
     0.299 |      0.004 | 81,413,440 | 7,000,001 |            | Total

estimated total profiling overhead: 10.01 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.4299999e-6s total profiling, 6.44e-7s internal profiling
```

Seems a bit suspect. I'm going to juggle the call order around, just to make sure I'm not priviledging any given function.


```
CLJ> (equality-benchmark)

NIL
  seconds  |     gc     |   consed   |   calls   |  sec/call  |  name
------------------------------------------------------------
     0.141 |      0.028 | 81,382,992 | 1,000,000 |   0.000000 | CLJ::FGF-==
     0.058 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ:==
     0.032 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ::BUILT-IN-FN
     0.016 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ::STATIC-FN
     0.013 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ::FGN-FN
     0.004 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ::NAIVE-FN
     0.000 |      0.000 |          0 |         1 |   0.000000 | CLJ::EQUALITY-BENCHMARK
     0.000 |      0.000 |          0 | 1,000,000 |   0.000000 | CLJ::STATIC-DISPATCH-==
------------------------------------------------------------
     0.264 |      0.028 | 81,382,992 | 7,000,001 |            | Total

estimated total profiling overhead: 10.01 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.4299999e-6s total profiling, 6.44e-7s internal profiling

CLJ>
```

Not... that big a difference, it looks like. To a first approximation, in this specific case, `fast-generic-function` is much worse than the alternatives, taking more than double the time and more than 1000x the space of the next most performant option. This is pretty damning, given that the test was pared down specifically to conform to the limitations of `seal-domain` here.

Moving on.

```
CLJ> (lookup/insert/len-benchmark #'static-dispatch-== #'static-dispatch-insert #'static-dispatch-lookup #'static-dispatch-len :times 10000)
NIL
CLJ> (lookup/insert/len-benchmark #'fgf-== #'fgf-insert #'fgf-lookup #'fgf-len :times 10000)
NIL
CLJ> (lookup/insert/len-benchmark #'== #'insert #'lookup #'len :times 10000)
NIL
measuring PROFILE overhead..done
  seconds  |     gc     |    consed   |  calls  |  sec/call  |  name
-----------------------------------------------------------
     0.589 |      0.024 | 207,097,712 |       3 |   0.196231 | CLJ::LOOKUP/INSERT/LEN-BENCHMARK
     0.455 |      0.030 | 335,245,040 |  30,000 |   0.000015 | CLJ:ALIST->MAP
     0.082 |      0.048 |  15,686,608 |  10,000 |   0.000008 | CLJ::FGF-INSERT
     0.038 |      0.018 |  11,400,528 |  10,000 |   0.000004 | CLJ::INSERT
     0.026 |      0.000 |      98,288 |  20,000 |   0.000001 | CLJ::STATIC-DISPATCH-LEN
     0.025 |      0.000 |   1,738,336 |  10,000 |   0.000002 | CLJ::STATIC-DISPATCH-LOOKUP
     0.022 |      0.000 |   1,570,448 |  20,000 |   0.000001 | CLJ::FGF-LEN
     0.021 |      0.000 |           0 |  20,000 |   0.000001 | CLJ::LEN
     0.021 |      0.000 |           0 |  10,000 |   0.000002 | CLJ::LOOKUP
     0.019 |      0.000 |   7,848,400 |  10,528 |   0.000002 | CLJ::FGF-==
     0.018 |      0.000 |   1,766,656 |  10,000 |   0.000002 | CLJ::FGF-LOOKUP
     0.011 |      0.000 |  10,319,072 |  10,000 |   0.000001 | CLJ::STATIC-DISPATCH-INSERT
     0.001 |      0.000 |           0 |  10,515 |   0.000000 | CLJ:==
     0.000 |      0.000 |           0 |  10,579 |   0.000000 | CLJ::STATIC-DISPATCH-==
-----------------------------------------------------------
     1.327 |      0.120 | 592,771,088 | 181,625 |            | Total

estimated total profiling overhead: 0.26 seconds
overhead estimation parameters:
  6.0e-9s/call, 1.4459999e-6s total profiling, 6.9e-7s internal profiling
```

So, weirdly, for our specific use-case, it looks like the `fast-generic-function` versions of these are slightly _worse_ than just built-in generic functions, while the `static-dispatch` implementations are slightly faster. I do emphasize _slightly_ in both of these situations. To the point that I'm seriously wondering whether the performance improvement made available by [`static-dispatch`](https://github.com/alex-gutev/static-dispatch) is actually worth [giving up `before`/`after`/`around` methods the way their documentation implies](https://github.com/alex-gutev/static-dispatch#usage). I don't specifically want any of those for `clj` internals, but `clj` _would_ have to expose `static-dispatch:defmethod` in order to avoid some weird interface incompatibility edge cases.

To be sure, there's a pretty serious improvement to `static-dispatch:insert` when compared to `insert` or `fgf-insert`, but the picture for `len`, `lookup` and `==` is less rosy. The naive `cl:defmethod` implementation beats both the others solidly for `len` and puts in a strong showing for both `==` and `lookup`. It's sort of hard for me to argue that `insert` is a more important operation than `lookup` or `==`. It probably depends on your use case, but this is not really a ringing endorsement either way.


### Conclusions

The tentative conclusion is that all this performance grubbing is a relative boondoggle. For the moment, given a bout this undecisive, I have to give victory to the incumbent.

So it goes sometimes.

I'll keep the `benchmark` file around for fun and possibly future profiling purposes. I was going to talk a bit about the approaches I'm thinking about to save cycles at low-effort where it matters, but I think this piece is already long and dense enough. If you want a sneak peek, check out [`types.lisp` in the main repo](https://github.com/inaimathi/clj/blob/master/src/types.lisp), but I'm not going to talk about it quite yet.
