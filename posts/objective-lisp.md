Stand back! I have _dramatic pause_ an idea!

```lisp
(defpackage :objective-lisp
    (:nicknames :ol)
    (:use :cl)
  (:shadow #:+ #:- #:* #:/
           #:member #:null
           #:= #:string= #:eq #:eql #:equal #:equalp
           #:length #:map #:mapcar #:concatenate)
  (:export #:+ #:- #:* #:/
           #:member #:null
           #:= #:string= #:eq #:eql #:equal #:equalp
           #:length #:map #:mapcar #:concatenate))
(in-package :objective-lisp)

;;; not, strictly speaking, relevant to what I want to discuss,
;;; but I figure I've already gone off the deep end just by
;;; writing this, so I may as well make it worth my while

(defun nullp (object) (cl:null object))

;;; or is it...
(defun memberp (item list &key key)
  (cl:member item list :key key :test #'=))

;; imagine other functions here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod + ((num number) &rest nums)
  (assert (every #'cl:numberp nums))
  (apply cl:+ (cons num nums)))

(defmethod - ((num number) &rest nums)
  (assert (every #'cl:numberp nums))
  (apply cl:- (cons num nums)))

(defmethod * ((num number) &rest nums)
  (assert (every #'cl:numberp nums))
  (apply cl:* (cons num nums)))

(defmethod / ((num number) &rest nums)
  (assert (every #'cl:numberp nums))
  (apply cl:/ (cons num nums)))

(defmethod length (seq) (cl:length seq))

(defmethod map ((fn function) (l list) &rest lists)
  (assert (every #'listp lists))
  (apply #'cl:mapcar fn (cons l lists)))

;; and defined for other types here

(defmethod concatenate ((str string) &rest strings)
  (assert (every #'stringp strings))
  (apply #'cl:concatenate 'string (cons str strings)))

;; and again

(defmethod = (a b) false)
(defmethod = ((a number) (b number)) (cl:= a b))
(defmethod = ((a string) (b string)) (cl:string= a b))
(defmethod = ((a character) (b character)) (cl:char= a b))
(defmethod = ((a symbol) (b symbol)) (cl:eq a b))
(defmethod = ((a cons) (b cons)) (cl:equalp a b))
(defmethod = ((a array) (b array)) (cl:equalp a b))
(defmethod = ((a structure-object) (b structure-object)) (cl:equalp a b))

;;; really, I should do the same for all the various
;;; comparison functions (>, >=, <=, <), but this is
;;; already longer than I'd like
```

Now, why would we want to do this terrible thing?

Well, we probably wouldn't.

I wouldn't ever straight-facedly recommend someone does this in any sort of production environment[^especially-since], but it does sand down some rather annoying corners[^in-case-you-care].

[^especially-since]: Especially since you get a lot of the benefit, with none of the weirdness or unfortunate performance implications, by using [Clojure](http://clojure.org/) instead.
[^in-case-you-care]: In case you care, I stubbed my metaphorical toe on them a little while ago by trying to implement [matrix operations](http://en.wikipedia.org/wiki/Matrix_(mathematics)#Basic_operations) in CL as a learning exercise only to find that I couldn't actually give them a `+` operation. It would have to be something like `add` or `matrix-add`. No world-ending obstruction, but slightly less than satisfying. At the time I implemented it as `matrix-add`.

What looking at CLOS this way gets you is the easing of a few syntactic binds at the cost of some performance.

- For starters, we now have a generic `=` that can be called in pretty-much any situation[^leaving-aside-identity]. That lets us define `memberp` and other functions without having to pass in a test function, since `=` handles dispatch itself. It also lets us define `map`/`concatenate` and similar functions without specifying what the output type is expected to be[^edge-case].
- As I note, it saves us from having a separate `=`, `char=` and `string=` and similar comparison operations.
- Finally, if we define new types (such as matrix), we are not prevented from giving them a `+` or `*` method (and we can specifically define how to go about `map`ping, `concatenate`ing or `length`ing them, if it makes sense, as well as defining the correct new equality test without having to name it `matrix=`).

[^leaving-aside-identity]: Leaving aside the structural identity vs. pointer identity question that may actually need to be addressed differently in some cases.
[^edge-case]: Although, to be fair, we do lose the ability to do something like `(concatenate 'string "Hello " (list #\t #\h #\e #\r #\e #\!))`.

You could also do things like override `+` so that it performs concatenation for strings[^widely-accepted], though I'd shrink from going further and applying it to sequences in general, just because it's somewhat ambiguous.

[^widely-accepted]: Since that seems to be well accepted in most languages by this point.

Because all these calls are handled by the type system, you'd also get compile-time warnings about operations that aren't defined[^from-the-future].

[^from-the-future]: Hello from 2016. The big "but..." I left out here was that you'd get significantly fewer compile-time warnings about type-mismatches. Like, if you accidentally tried to `+` a number to a string. The error messages in general also tend to get a lot more opaque with this approach; they'll all mention missing methods rather than the more reasonable type error documentation you'd get out of the box. Luckily, this was never proposed as a serious Thing You Should Do.

Incidentally, I know I'm not even remotely the first person to think of doing something like this on a lark. As usual, I just wanted to get an idea far enough out of my head to take a good look at it.

This has been a completely random thought about Lisp. Thank you for your time.
