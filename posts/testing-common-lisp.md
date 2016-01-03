Given that I'm on vacation, I'm obviously back to writing mostly Common Lisp rather than mostly Ruby. Although, admittedly, *mostly* for [tiny puzzle-style things](https://github.com/Inaimathi/advent-code). I wanted to use [`prove`](https://github.com/fukamachi/prove) and [`cl-quickcheck`](https://github.com/mcandre/cl-quickcheck) to do some light automated testing, and found the experience a bit clunkier than expected. So, I [did something about it](https://github.com/Inaimathi/test-utils).

## The Aim

What I'd like to be able to write is something like

```common-lisp
(a-suite
 (is (+ 1 2) 3 "Addition works")
 (is (+ 3 3 3 6) 15 "Addition works on more than two numbers")
 (is (+ -1 -1 -1) -3 "Addition works on negative numbers")
 (is (+ -1 2) 1 "Addition works on negative and positive numbers together")

 (qchecks
  (for-all ((a a-number) (b a-number))
    (is= (+ a b) (+ b a)))
  "Addition is idempotent"))
```

and have it represent a proper `prove` suite that checks some manually specified properties, *along with* some fuzzed proerties via `quickcheck`. And that't exactly what you can do with `test-utils`.

```common-lisp
TEST-UTILS> (a-suite
 (is (+ 1 2) 3 "Addition works")
 (is (+ 3 3 3 6) 15 "Addition works on more than two numbers")
 (is (+ -1 -1 -1) -3 "Addition works on negative numbers")
 (is (+ -1 2) 1 "Addition works on negative and positive numbers together")

 (qchecks
  (for-all ((a a-number) (b a-number))
    (is= (+ a b) (+ b a)))
  "Addition is idempotent"))
1..5

  ✓ Addition works
  ✓ Addition works on more than two numbers
  ✓ Addition works on negative numbers
  ✓ Addition works on negative and positive numbers together
  ✓ Addition is idempotent

✓ 5 tests completed (4ms)
T
TEST-UTILS>
```

Of course, it's important to have decent error reporting too.

```common-lisp
TEST-UTILS> (a-suite
 (is (- 1 2) -1 "Subtraction works")
 (is (- 1) -1 "Unary subtraction is negation")
 (is (- -1) 1 "Negation works on negative numbers")
 (is (- -1 -2) 1 "Subtraction works on negative numbers")

 (qchecks
  (for-all ((a a-number) (b a-number))
    (is= (- a b) (- b a)))
  "Subtraction is not idempotent, so this should fail"))
1..5

  ✓ Subtraction works
  ✓ Unary subtraction is negation
  ✓ Negation works on negative numbers
  ✓ Subtraction works on negative numbers
X
FAIL (IS= (- A B) (- B A))
  with values -0.88533974 0.88533974
  for ((A 14297/2081) (B 7.7555943))
1 test submitted; 1 FAILED.
  × Subtraction is not idempotent, so this should fail
    NIL is expected to be T

× 1 of 5 tests failed (4ms)
NIL
TEST-UTILS>
```

I could finesse the whitespace a little bit *(in fact, note to self: finesse the whitespace a little bit)* but that's basically what I'd want as output on a failure. Inform me of failure, show me the specific values that triggered it, and that's it.

Lets take a somewhat closer look at the obstacles and code here.


## The Obstacles

### Symbol Conflicts

The first big obstacle is that both `prove` and `cl-quickcheck` bind the symbols `is` and `isnt`. Which is less than convenient for callers, because it means this name collision needs to be resolved in any projects that want to include both of them. The *ideal* solution would be to use `prove:is`/`prove:isnt` by default, but switch to using `cl-quickcheck:is`/`cl-quickcheck:isnt` in the context of the `qchecks` macro. There are enough edge-cases that I don't particularly want to do that. The next best thing is to just use `is` and `isnt` from prove, and expect the user to use `is=` in the `qchecks` context. Less than ideal, but we can do that fairly easily.

```common-lisp
;;;; package.lisp

(defpackage #:test-utils
  (:use #:cl #:prove #:cl-quickcheck)
  (:shadowing-import-from #:prove #:is #:isnt)
  (:export
...
   ;; re-export :quickcheck symbols
   #:quickcheck #:is= #:isnt= #:should-signal
...

   ;; re-export :prove symbols
...
   #:is
   #:isnt
...
```

by doing a `shadowing-import` and re-exporting some symbols from both packages.

### Noisy Reporting

Next, `cl-quickcheck` is unfortunately noisier than I'd like it to be. It insists on reporting all successes and failures, *and* always dumps its random seed as the initial couple hundred lines of output to `*standard-output*`. There doesn't seem to be a way of turning that off via config variable or similar. This is ok behavior if you're *only* running a `quickcheck` suite, but if you're actually running a `prove` suite, some small part of which you'd like to enforce `quickcheck` properties, what you'd really want is

1. that random seed block to not be printed
2. `quickheck` to stay silent unless it fails (in which case it should show the failures, and specific generated values that triggered them)

```common-lisp
(defmacro quiet-check (&body body)
  "Like :quickcheck, but squelches the initial seed reporting. Useful for running quickcheck properties in the middle of :prove suites."
  (let ((res (gensym))
	(msg (gensym))
	(short-msg (gensym)))
    `(let ((,res nil)
	   (,msg nil))
       (setf ,msg (with-output-to-string (*standard-output*)
		    (setf ,res (quickcheck ,@body))))
       (let ((,short-msg (subseq ,msg (nth-value 1 (read-from-string ,msg nil nil :start 25)))))
	 (unless ,res (format t "~a" ,short-msg))
	 ,res))))

(defmacro qchecks (quickcheck-test &optional message)
  "Form for calling quickcheck tests from a prove test (this lets you easily execute :quickcheck properties as part of a prove:run)"
  `(is (quiet-check ,quickcheck-test) t ,message))
```

It's ... ugly. Really, *really* ugly. But it works. That macro sets up a `quickcheck` call that re-routes output to a string, chops off the `seed` reporting, and only dumps it to `*standard-output*` if the test fails. This is another point to clear up for later actually; make sure that we take format flag configuration from `:prove` and send it to the right stream, rather than always `*standard-output*`. That way we can make use of external tools for automated testing.

The second part, the `qchecks` macro, just wraps a call to `quiet-check` in a `prove:is` clause, and checks that the output of the suite is `T`.

### Missing Generators

Quickcheck comes with some built-in random value generators. Most of the basics are covered, and you have basic sum/product composition in the form of `a-list`, `a-tuple` and `a-member`, each of which does nice things. However, the primitives are oddly arranged. Some of them are in the function namespace, and some of them are `lambda` terms in the variable namespace. Concretely, this means that in order to use a higher order combinator, you sometimes need to pass generators as `#'foo` and sometimes as `foo`. There doesn't seem to be a reason for this, so lets not bother ever again.

```common-lisp
...

;;;;; Make namespacing consistent amongst primitive quickcheck generators
(defparameter a-string #'cl-quickcheck:a-string)
(defparameter a-symbol #'cl-quickcheck:a-symbol)
(defparameter a-char #'cl-quickcheck:a-char)

...
```

Next, some primitive generators are missing, and so are some combinators. The primitives I was expecting to be present that aren't are `ratio`, `number` (an arbitrary member of any of Common Lisp's `number` type, rather than a specific one) and `atom` (an arbitrary non-compound value rather than one of a specific type). The first of these seems oddly absent, given that there's syntactic support for it in Lisp, while the other two are sometimes useful for testing type-agnostic functions and macros. No big deal; none of them are hard to define, but I don't want to have to define them individually in the projects that call for them.

```common-lisp
...

(defparameter a-ratio
  (lambda () (rationalize (funcall a-real))))

(defparameter a-number
  (a-member an-integer a-real a-ratio))

(defparameter an-atom
  (a-member a-number a-boolean a-string a-symbol a-char))

...
```

Also, `list`s aren't the only kind of compound structure available in Common Lisp. We've also got `vector`s and `hash`es.

```common-lisp
...

(defun a-vector (generator)
  (lambda ()
    (coerce (funcall (a-list generator)) 'vector)))

(defun a-hash (key-generator value-generator)
  (lambda ()
    (let ((res (make-hash-table :test 'equalp)))
      (loop repeat (funcall an-index)
	 for k = (funcall key-generator)
	 for v = (funcall value-generator)
	 do (setf (gethash k res) v))
      res)))

...
```

This still leaves multi-dimensional arrays out of the picture, but those at least seem like they have different domain-specific requirements on appropriate size and dimension, so I feel more comfortable leaving them out of the general picture.

### Prove Boilerplate

Nitpick, really, but while I'm already here...

For some reason I'm not very clear on, `:prove` requires that a particular volley of tests be preceded by a `prove:plan` call specifying how many tests you're about to run, and followed by a call to `prove:finalize`. Given that this can be automatically computed based on the raw test tree, I've decided that I don't want to do it myself. So...

```common-lisp
(defmacro a-suite (&rest forms)
  "This hacks around :prove's requirement that a number of forms be provided, and #'prove:finalze be called around each set of :prove tests.
Pointed out at https://github.com/fukamachi/prove/issues/14, but not yet addressed."
  `(progn
     (prove:plan ,(length forms))
     ,@forms
     (prove:finalize)))
```

...I won't. [That `github` link](https://github.com/fukamachi/prove/issues/14) goes to an issue on the `prove` project in which some people have pointed this out to seemingly no avail. Hopefully, that changes at some point in the future, but until then, at least you don't need to manually count the number of tests you intend to run.
