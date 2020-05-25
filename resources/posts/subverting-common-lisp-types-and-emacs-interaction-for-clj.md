Ok, so [profiling the extra-low-hanging-fruit](http://inaimathi.ca/posts/profiling-polymorphic-options) in terms of generic function performance revealed that it didn't do much in our situation. My next idea was to subvert the Common Lisp type system to give our `set` and `map` primitives some hints about what kind of equality operations to use.

I'm once again starting this piece before having written the code it'll be explaining, so this is less a thoughtful tour and more a stream-of-consciousness account of the writing.

### Defining Types

Assuming the thing you're defining fits into the pre-existing Common Lisp types, you're fine. As soon as you want to do something like define polymorphic key/value structures you are, near as I can tell, on your fucking own bucko.

So I guess I'm rolling my own here?

Ok, the good news is that I'm in just enough of a hacky mood that I don't give a flying fuck how shitty this is going to be. That... might come back to bite me later, but we'll burn that bridge and salt it as we pass.

Here's how I have to define type `map`.

```
(deftype map (&optional keys vals)
  (let ((sym (intern (format nil "MAP-TYPE-~a-~a" keys vals) :clj)))

    (unless (fboundp sym)
      (setf (fdefinition sym) (kv-types keys vals)))

    `(and (satisfies map?) (satisfies ,sym))))
```

This feels batshit insane. In order to properly define a polymorphic key/value type, I have to _manually intern predicates that deal with the specific types in question at declaration time_. The problem is that `satisfies` specifically only accepts a `symbol` that must refer to a `function` of one argument that's meant to return a `boolean`. If it could take `lambda` terms, I could do something like

```
(defun kv-type (k-type v-type)
  (lambda (thing)
    (and (map? thing)
	 (every (lambda (pair)
		  (and (typep (car pair) k-type)
		       (typep (cdr pair) v-type)))
		(values thing)))))
...
(satisfies (kv-type 'keyword 'integer))
```

This is, unfortunately, off the table. Oh well. The complete definitions for both `map` and `set` types is

```
(defun map? (thing)
  (typep thing 'cl-hamt:hash-dict))

(defun map-type? (type)
  (and type
       (listp type)
       (eq (car type) 'map)))

(defun kv-types (k-type v-type)
  (lambda (map)
    (cl-hamt:dict-reduce
     (lambda (memo k v)
       (and memo (typep k k-type) (typep v v-type)))
     map t)))

(deftype map (&optional keys vals)
  (let ((sym (intern (format nil "MAP-TYPE-~a-~a" keys vals) :clj)))

    (unless (fboundp sym)
      (setf (fdefinition sym) (kv-types keys vals)))

    `(and (satisfies map?) (satisfies ,sym))))

(defun set? (thing)
  (typep thing 'cl-hamt:hash-set))

(defun set-type? (type)
  (and type
       (listp type)
       (eq (car type) 'set)))

(defun seq-types (v-type)
  (lambda (set)
    (cl-hamt:set-reduce
     (lambda (memo elem)
       (and memo (typep elem v-type)))
     set t)))

(deftype set (&optional vals)
  (let ((sym (intern (format nil "SET-TYPE-~a" vals) :clj)))

    (unless (fboundp sym)
      (setf (fdefinition sym) (seq-types vals)))

    `(and (satisfies set?) (satisfies ,sym))))
```

Once I've got that, I can declare things. Like,

```
CLJ> (let ((a {:a 1 :b 2}))
  (declare (type (map keyword t) a))
  a)
{:A 1 :B 2}
CLJ>
```

### Checking for equalities

There's some more work to do. The whole point of this exercise is Once I've got a type declared, I need to do the work I actually care about. Which is: figure out which of the built-in structural equality operations is the most efficient I can use while _also_ being as correct as possible.

```
(defun fullest-equality (equalities)
  (find-if
   (lambda (e) (member e equalities :test #'eq))
   '(clj:== cl:equalp cl:equal cl:eql cl:eq cl:string= cl:=)))

(defun equality-function (name) (fdefinition name))

(defun equality-of (type)
  (cond
    ((member type '(integer number float ratio rational bignum bit complex long-float short-float signed-byte unsigned-byte single-float double-float fixnum))
     'cl:=)
    ((member type '(string simple-string))
     'cl:string=)
    ((member type '(atom symbol keyword package readtable null stream random-state))
     'cl:eq)
    ((member type '(standard-char character pathname))
     'cl:eql)
    ((member type '(cons list))
     'cl:equal)
    ((and (listp type) (eq 'or (first type)))
     (fullest-equality (mapcar #'equality-of (rest type))))
    ((member type '(hash-table sequence array bit-vector simple-array simple-bit-vector simple-vector vector))
     'cl:equalp)
    ((and (listp type) (member (car type) '(array simple-array simple-bit-vector simple-vector vector)))
     'cl:equalp)
    ((member type '(compiled-function function))
     nil)
    (t 'clj:==)))
```

It's a fairly naive binding table, completely inextensible for the moment, that maps a `type` to the name of an equality operation that will accurately compare them. Hopefully, I mean. As long as I didn't fuck something up.

```
CLJ> (equality-of '(map keyword t))
==
CLJ> (equality-of 'keyword)
EQ
CLJ> (equality-of 'list)
EQUAL
CLJ> (equality-of 'hash-table)
EQUALP
CLJ> (equality-of 'string)
STRING=
CLJ>
```

Seems legit.

### Putting it all together

The next step is, we want to use this equality selection procedure to make our `map` and `set` constructors pick a better one than `==` if it can.

```
(defparameter *type* nil)
...
(defun alist->map (alist &key equality)
  (let ((equality (or equality
		      (if (map-type? *type*)
			  (equality-function (equality-of (second *type*)))
			  #'==))))
    (loop with dict = (cl-hamt:empty-dict :test equality)
       for (k . v) in alist do (setf dict (cl-hamt:dict-insert dict k v))
       finally (return dict))))

(defun list->map (lst &key equality)
  (assert (evenp (length lst)) nil "Map literal must have an even number of elements")
  (let ((equality (or equality
		      (if (map-type? *type*)
			  (equality-function (equality-of (second *type*)))
			  #'==))))
    (loop with dict = (cl-hamt:empty-dict :test equality)
       for (k v) on lst by #'cddr
       do (setf dict (cl-hamt:dict-insert dict k v))
       finally (return dict))))
...
(defun list->set (lst)
  (let ((equality (if (set-type? *type*)
		      (equality-function (equality-of (second *type*)))
		      #'==)))
    (reduce
     (lambda (set elem)
       (cl-hamt:set-insert set elem))
     lst :initial-value (cl-hamt:empty-set :test equality))))
```

So, we've got a `*type*` [special var](http://clhs.lisp.se/Body/d_specia.htm) that we can use to declare the type of the `map`/`set` we're defining, and if it's set, we use it to pick an appropriate equality. Otherwise, we just go with `#'==`, because that's as general as it gets.

```
CLJ> (list->set (list 1 2 3 4))
#{3 2 1 4}
CLJ> (cl-hamt::hamt-test (list->set (list 1 2 3 4)))
#<STANDARD-GENERIC-FUNCTION CLJ:== (8)>
CLJ> (let ((*type* '(set integer))) (list->set (list 1 2 3 4)))
#{3 2 1 4}
CLJ> (let ((*type* '(set integer))) (cl-hamt::hamt-test (list->set (list 1 2 3 4))))
#<FUNCTION =>
CLJ> (list->map (list :a 1 :b 2 :c 3))
{:A 1 :C 3 :B 2}
CLJ> (cl-hamt::hamt-test (list->map (list :a 1 :b 2 :c 3)))
#<STANDARD-GENERIC-FUNCTION CLJ:== (8)>
CLJ> (let ((*type* '(map keyword t))) (cl-hamt::hamt-test (list->map (list :a 1 :b 2 :c 3))))
#<FUNCTION EQ>
CLJ>
```
Nice.

It doesn't fit _all_ of our use cases though.

```
CLJ> {:a 1 :b 2 :c 3}
{:A 1 :C 3 :B 2}
CLJ> (let ((*type* '(map keyword t))) {:a 1 :b 2 :c 3})
{:A 1 :C 3 :B 2}
CLJ> (let ((*type* '(map keyword t))) (cl-hamt::hamt-test {:a 1 :b 2 :c 3}))
#<STANDARD-GENERIC-FUNCTION CLJ:== (8)>
CLJ>
```

The problem is that, because we have reader syntax for our `map`s and `set`s, this decision kicks in too late to deal with them. We unfortunately also need a reader macro to handle type declarations.

### Reader Macro for Type Declaration

The naive solution here is

```
...
(defun type-literal-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((*type* (read stream))
	 (form (read stream))
	 (val (eval form)))
    (assert (typep val *type*) nil "Type checking failure ~s ~s" *type* form)
    val))

...
  (:dispatch-macro-char #\# #\# #'type-literal-reader))

```

I don't really want to define this as using `::` because of the headache-inducing implications of doing `(make-dispatch-macro-character #\:)`. I'm trying to avoid those for the moment. Same story with `#:`, because [uninterned symbols](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node110.html) are common and I don't want to stomp them here. So, I had to pick something else, and arbitrarily accepted `##` even though `#t` or `#T` would have been equally reasonable choices.

This technically works.

```
CLJ> {:a 1 :b 2 :c 3}
{:A 1 :C 3 :B 2}
CLJ> ## (map keyword t) {:a 1 :b 2 :c 3}
{:A 1 :C 3 :B 2}
CLJ> (cl-hamt::hamt-test ## (map keyword t) {:a 1 :b 2 :c 3})
#<FUNCTION EQ>
CLJ>
```

But I want to avoid calling `eval` as part of it. The more macro-like version would look something more like

```
(defun type-literal-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((*type* (read stream))
	 (form (read stream))
	 (res (gensym)))
    (if *type*
	`(let ((,res ,form))
	   (check-type ,res ,*type*)
	   ,res)
	res)))
```

It still works...

```
CLJ> ## (map keyword t) {:a 1 :b 2}
{:A 1 :B 2}
CLJ> (cl-hamt::hamt-test ## (map keyword t) {:a 1 :b 2})
#<FUNCTION EQ>
CLJ>
```

... but has the added bonuses of not calling `eval` and also making use of `check-type`, which we couldn't do if we wanted to do that check inline at read time.

I don't really like the syntax[^ideal-type-annotation-syntax], but that's good enough for now[^future-improvements-include].

[^ideal-type-annotation-syntax]: Ideally, the type annotation would be declared like `(:: type form)`, `:: type form`, or possibly `type :: form`. However, infix operators are more complicated to deal with, and `:` already has various meanings in Common Lisp that would make using it as a `read-macro-char` more complicated than I'd like.

[^future-improvements-include]: Possible future improvements include inferring the type of a `map` literal based on its initial values, and storing the type annotation somehow so that it can be checked against by `insert` later. I'm not sure any of this is worth the time, and once we pick an appropriate interface, it'll be easy to change internals later.

### Performance implications

```
(defun untyped-benchmark (&key (times 10000))
  (loop repeat times
     do (let* ((m {:a 1 :b "two" :c :three :d 44})
	       (inserted (insert m (cons :test-key :test-value))))
	  (list (len m)
		(lookup inserted :test-key)
		(len inserted)))))

(defun typed-benchmark (&key (times 10000))
  (loop repeat times
     do (let* ((m ## (map keyword t) {:a 1 :b "two" :c :three :d 44})
	       (inserted (insert m (cons :test-key :test-value))))
	  (list (len m)
		(lookup inserted :test-key)
		(len inserted)))))
```

With the above defined in [`benchmark.lisp`](https://github.com/inaimathi/clj/blob/master/src/benchmark.lisp), running the benchmarks and reporting them with `M-x slime-profile-report slime-profile-reset` gives us...

```
CLJ> (untyped-benchmark :times 1000000)
NIL
measuring PROFILE overhead..done
  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name
---------------------------------------------------------------
     1.230 |      0.068 | 1,076,698,880 | 1,000,000 |   0.000001 | CLJ::INSERT
     0.931 |      0.000 |        32,768 | 2,000,000 |   0.000000 | CLJ::LEN
     0.617 |      0.000 |     1,679,216 | 1,000,000 |   0.000001 | CLJ::LOOKUP
     0.000 |      0.018 |    59,768,832 |         1 |   0.000000 | CLJ::UNTYPED-BENCHMARK
     0.000 |      0.000 |             0 | 1,000,000 |   0.000000 | CLJ:==
---------------------------------------------------------------
     2.778 |      0.086 | 1,138,179,696 | 5,000,001 |            | Total

estimated total profiling overhead: 9.09 seconds
overhead estimation parameters:
  1.8e-8s/call, 1.8179999e-6s total profiling, 8.8e-7s internal profiling

These functions were not called:
 CLJ:ALIST->MAP CLJ::EQUALITY-FUNCTION CLJ::EQUALITY-OF
 CLJ::FULLEST-EQUALITY CLJ::KV-TYPES CLJ::LIST->MAP CLJ:LIST->SET
 CLJ::MAP-LITERAL-READER CLJ::MAP-TYPE-KEYWORD-T CLJ::MAP-TYPE?
 CLJ::MAP? CLJ::SEQ-TYPES CLJ::SET-LITERAL-READER CLJ::SET-TYPE?
 CLJ::SET? CLJ::TYPE-LITERAL-READER CLJ::TYPED-BENCHMARK

CLJ> (typed-benchmark :times 1000000)
NIL
  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name
---------------------------------------------------------------
     1.195 |      0.040 | 1,076,307,616 | 1,000,000 |   0.000001 | CLJ::INSERT
     0.768 |      0.000 |             0 | 2,000,000 |   0.000000 | CLJ::LEN
     0.605 |      0.000 |             0 | 1,000,000 |   0.000001 | CLJ::LOOKUP
     0.000 |      0.004 |    59,703,296 |         1 |   0.000000 | CLJ::TYPED-BENCHMARK
---------------------------------------------------------------
     2.568 |      0.044 | 1,136,010,912 | 4,000,001 |            | Total

estimated total profiling overhead: 7.27 seconds
overhead estimation parameters:
  1.8e-8s/call, 1.8179999e-6s total profiling, 8.8e-7s internal profiling

These functions were not called:
 CLJ:== CLJ:ALIST->MAP CLJ::EQUALITY-FUNCTION CLJ::EQUALITY-OF
 CLJ::FULLEST-EQUALITY CLJ::KV-TYPES CLJ::LIST->MAP CLJ:LIST->SET
 CLJ::MAP-LITERAL-READER CLJ::MAP-TYPE-KEYWORD-T CLJ::MAP-TYPE?
 CLJ::MAP? CLJ::SEQ-TYPES CLJ::SET-LITERAL-READER CLJ::SET-TYPE?
 CLJ::SET? CLJ::TYPE-LITERAL-READER CLJ::UNTYPED-BENCHMARK
CLJ>
```

A pretty goddamn tiny difference. I'm not sure this approach is worth much more effort, but I'll plug away for a bit longer to see how elegant I can make it. In the meantime,

### Emacs Interaction Improvements

So, the sad thing about all of this is that I've been lying to you. Whenever I show you those nice readouts from the SLIME `repl` that says something like `{:a 1 :b 2}`, that's a result of me correcting it. Because, by default, when I type `{`, what I get is `{$`. Which I then have to manually backspace and correct. Using this shiny new syntax in Common Lisp mode is also less than ideal, because the default `paredit` doesn't provide `s-exp` support for curly braces. It's not as simple as adding

```
"{" 'paredit-open-curly
"}" 'paredit-close-curly
```

to a mode-map somewhere, because that _does_ pair them, but _doesn't_ help with navigation.

After messing around with modifying existing `syntax-table`s, redefining `matching-paren`, and poking around in [`paredit`](https://www.emacswiki.org/emacs/ParEdit) internals, the solution I settled on was just adding a `mode-hook` to a bunch of `lisp` modes and `slime-repl` modes that activates the `clojure-mode-syntax-table`.

_You_ can do this in _your_ `.emacs` file by doing something like

```
(defun use-clojure-syntax-table () (set-syntax-table (set-syntax-table clojure-mode-syntax-table)))
(add-hook 'common-lisp-mode-hook 'use-clojure-syntax-table)
(add-hook 'slime-mode-hook 'use-clojure-syntax-table)
(add-hook 'slime-repl-mode-hook 'use-clojure-syntax-table)
```

_I_ added it to _my_ `.emacs` by doing

```
(hooks (common-lisp lisp emacs-lisp scheme lisp-interaction slime clojure slime-repl)
       (lambda ()
	 (setq autopair-dont-activate t)
	 (autopair-mode -1))
       'enable-paredit-mode
       (lambda () (set-syntax-table (set-syntax-table clojure-mode-syntax-table))))
```

Which is both more thorough and more extensive, but requires me to define [some conveniences](https://github.com/inaimathi/machine-setup/blob/master/convenience.el#L61-L81) first.

The next time I write about `CLJ`, the SLIME `repl` snippets will _not_ be a lie.
