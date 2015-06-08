Stand back! I have dramatic pause an idea!

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

Now, why would we want to do this terrible thing? Well, we probably wouldn't. I wouldn't ever straight-facedly recommend someone does this in any sort of production environment, but it does sand down some rather annoying corners<a name="note-Fri-Nov-04-235357EDT-2011"></a>[|1|](#foot-Fri-Nov-04-235357EDT-2011).

What looking at CLOS this way gets you is the easing of a few syntactic binds at the cost of some performance.



- For starters, we now have a generic `=` that can be called in pretty-much any situation<a name="note-Sat-Nov-05-000146EDT-2011"></a>[|2|](#foot-Sat-Nov-05-000146EDT-2011). That lets us define `memberp` and other functions without having to pass in a test function (`=` handles dispatch itself). It also lets us define `map`/`concatenate` and similar functions without specifying what the output type is expected to be<a name="note-Sat-Nov-05-000434EDT-2011"></a>[|3|](#foot-Sat-Nov-05-000434EDT-2011). 
- As I note, it saves us from having a separate `=`, `char=` and `string=` and similar comparison operations. 
- Finally, if we define new types (such as matrix), we are not prevented from giving them a `+` or `*` method (and we can specifically define how to go about `map`ping, `concatenate`ing or `length`ing them, if it makes sense, as well as defining the correct new equality test without having to name it `matrix=`).



You could also do things like override `+` so that it performs concatenation for strings<a name="note-Sat-Nov-05-001019EDT-2011"></a>[|4|](#foot-Sat-Nov-05-001019EDT-2011), though I'd shrink from going further and applying it to sequences in general, just because it's somewhat ambiguous. 

Because all these calls are handled by the type system, you'd also get compile-time warnings about operations that aren't defined.

Incidentally, I know I'm not even remotely the first person to think of doing something like this on a lark. As usual, I just wanted to get an idea far enough out of my head to take a good look at it. 

This has been a completely random thought about Lisp. Thank you for your time.

* * *
##### Footnotes

1 - <a name="foot-Fri-Nov-04-235357EDT-2011"></a>[|back|](#note-Fri-Nov-04-235357EDT-2011) - In case you care, I stubbed my metaphorical toe on them a little while ago by trying to implement [matrix operations](http://en.wikipedia.org/wiki/Matrix_(mathematics)#Basic_operations) in CL as a learning exercise only to find that I couldn't actually give them a `+` operation. It would have to be something like `add` or `matrix-add`. No world-ending obstruction, but slightly less than satisfying. At the time I implemented it as `matrix-add`.

2 - <a name="foot-Sat-Nov-05-000146EDT-2011"></a>[|back|](#note-Sat-Nov-05-000146EDT-2011) - leaving aside the identity vs. pointer question that may actually need to be addressed differently in some cases

3 - <a name="foot-Sat-Nov-05-000434EDT-2011"></a>[|back|](#note-Sat-Nov-05-000434EDT-2011) - Although, to be fair, we do lose the ability to do something like `(concatenate 'string "Hello " (list #\t #\h #\e #\r #\e #\!))`

4 - <a name="foot-Sat-Nov-05-001019EDT-2011"></a>[|back|](#note-Sat-Nov-05-001019EDT-2011) - Since that seems to be well accepted in most languages by this point.
