Have you ever defined a custom class with hash components? Something like, say, a session?

```lisp
(defclass session ()
  ((started :reader started :initform (get-universal-time))
   (last-poked :accessor last-poked :initform (get-universal-time))
   (token :reader token :initarg :token)
   (session-values :reader session-values :initform (make-hash-table :test 'equal))))
```

If you have, you probably also figured it would be better to define some sugar for look-ups rather than doing `(gethash foo (session-values bar))` every damn time. Something like

```lisp
(defmethod lookup (key (session session))
  (gethash key (session-values session)))
```

**And** if you've gotten that far, you probably noticed that you can't just go ahead and say `(setf (lookup :test foo) new-val)`, because if you tried, you'd get this

```
The function (COMMON-LISP:SETF COMMON-LISP-USER::LOOKUP) is undefined.
   [Condition of type UNDEFINED-FUNCTION]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] Abort thread (#&lt;THREAD "repl-thread" RUNNING {10047D0113}>)
```

There are two ways of fixing this.

### If You Don't Need CLOS Support

For instance, if your `lookup` is a function rather than a method, you can get away with doing something like this

```lisp
(defsetf lookup (key session) (new-value)
  `(setf (gethash ,key (session-values ,session)) ,new-value))
```

or, annotated

```lisp
(defsetf [name-of-lookup-function] (&rest [lookip-function-args]) (new-value)
  [macro-body])
```

Once you've defined that as appropriate, you *can* just

```lisp
CL-USER> (lookup :test foo)
NIL
NIL
CL-USER> (setf (lookup :test foo) 'new-val)
NEW-VAL
CL-USER> (lookup :test foo)
NEW-VAL
T
CL-USER> 
```

This isn't satisfying if the lookup abstraction you've defined is a method though. Because then you get into this problem

```lisp
CL-USER> (defmethod lookup (key (session session))
  (gethash key (session-values session)))

(defmethod lookup (key (hash hash-table))
  (gethash key hash))
STYLE-WARNING: Implicitly creating new generic function LOOKUP.
#&lt;STANDARD-METHOD LOOKUP (T HASH-TABLE) {1005048E03}>
CL-USER> (defsetf lookup (key session) (new-value)
  `(setf (gethash ,key (session-values ,session)) ,new-value))
LOOKUP
CL-USER> (defparameter foo (make-instance 'session))
FOO
CL-USER> (defparameter bar (make-hash-table))
BAR
CL-USER> (lookup :test foo)
NIL
NIL
CL-USER> (lookup :test bar)
NIL
NIL
CL-USER> (setf (lookup :test foo) 'one)
ONE
CL-USER> (setf (lookup :test bar) 'two)

There is no applicable method for the generic function
  #&lt;STANDARD-GENERIC-FUNCTION SESSION-VALUES (1)>
when called with arguments
  (#&lt;HASH-TABLE :TEST EQL :COUNT 0 {10051DC633}>).
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [RETRY] Retry calling the generic function.
 1: [RETRY] Retry SLIME REPL evaluation request.
 2: [*ABORT] Return to SLIME's top level.
 3: [ABORT] Abort thread (#&lt;THREAD "repl-thread" RUNNING {10047D0113}>)

Backtrace:
  0: ((SB-PCL::FAST-METHOD NO-APPLICABLE-METHOD (T)) #&lt;unused argument> #&lt;unused argument> #&lt;STANDARD-GENERIC-FUNCTION SESSION-VALUES (1)> #&lt;HASH-TABLE :TEST EQL :COUNT 0 {10051DC633}>)
  1: (SB-PCL::CALL-NO-APPLICABLE-METHOD #&lt;STANDARD-GENERIC-FUNCTION SESSION-VALUES (1)> (#&lt;HASH-TABLE :TEST EQL :COUNT 0 {10051DC633}>))
  2: (#:EVAL-THUNK)
  ...
```

### If You Need CLOS Support

Then you'll need to define a `setf` generic. In the example we've been using, you could do the following:

```lisp
(defgeneric (setf lookup) (new-value key session)
  (:documentation "Setter for lookup methods"))

(defmethod (setf lookup) (new-value key (session session))
  (setf (gethash key (session-values session)) new-value))

(defmethod (setf lookup) (new-value key (hash hash-table))
  (setf (gethash key hash) new-value))
```

Which would then let you polymorphically use `setf` exactly the way you'd expect.

```lisp
CL-USER> (defparameter foo (make-instance 'session))
FOO
CL-USER> (defparameter foo (make-hash-table))
FOO
CL-USER> (defparameter foo (make-instance 'session))
FOO
CL-USER> (defparameter bar (make-hash-table))
BAR
CL-USER> (list (lookup :test foo) (lookup :test bar))
(NIL NIL)
CL-USER> (setf (lookup :test foo) 'session (lookup :test bar) 'hash)
HASH
CL-USER> (list (lookup :test foo) (lookup :test bar))
(SESSION HASH)
CL-USER>
```

There. Hopefully the next person who searches for "`defsetf` examples" will find something more useful than I did.
