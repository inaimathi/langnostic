I mentioned the [`clj`](https://github.com/inaimathi/clj) repo last time. And mentioned off-handedly that there's a bunch of things I _really_ like about Clojure, and also a few reason that, for my personal use at least, I'm definitely going to continue developing things in Common Lisp.

I intend to do something about this. If you like, you can [follow along](https://github.com/inaimathi/clj/tree/master/src). Who knows, we may both learn something. We'll go from the trivial, to the difficult but hopefully possible. I'm not writing this piece after finishing development, the intent is to journal as I go.

### Readable Anaphora
_Implementation Complexity: Trivial_

There's a chapter in [On Lisp](http://www.paulgraham.com/onlisp.html) that defines some [anaphoric macros](https://letoverlambda.com/index.cl/guest/chap6.html). Things that bind the results of intermediate computations and expose them as symbols in their result bodies. The archetypal ones are `aif` and `awhen`, roughly defined as

```
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body then)
  `(aif ,test (progn ,@then)))
```

The idea is that you then have the symbol `it` bound in the body of the `then` form. This is useful, but the Clojure approach of conditional binding forms makes the resulting code easier to read because it lacks symbols defined outside of the visible scope. Granted, the implementation is a bit more complex

```
(defmacro if-let ((name test) then &optional else)
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,test))
       (if ,tmp
	   (let ((,name ,tmp)) then)
	   ,else))))

(defmacro when-let ((name test) &body then)
  `(if-let (,name ,test) (progn ,@then)))
```

...but not by much. And the calls are going to be more obvious to someone reading it.

```
(aif (expensive-operation)
     (some-interesting-transformation it))
```

vs

```
(if-let (it (expensive-operation))
  (some-interesting-transformation it))
```

The only finer point here is the use of the intermediate variable `tmp`, because we don't want to evaluate any of the inputs more than once.

### Arrow macros
_Implementation Complexity: Simple_

The basic arrow macros are `->` and `->>`. There's a bunch more that you can see over at the [`arrow-macros` project](https://github.com/hipeta/arrow-macros/blob/master/arrow-macros.lisp), but I'm going to stick to those two for now. The only non-trivial part of this is compensating for `lambda` expressions and `#'` symbols.

That is, it's not enough to

```
(defmacro -> (exp &rest ops)
  (reduce
   (lambda (memo op)
     (if (atom op)
	 `(,op ,memo)
	 `(,(first op) ,memo ,@(rest op))))
   ops :initial-value exp))
```

because that fails on inputs like `(-> foo #'bar (lambda (n) (baz n)))`. Macroexpanding it will show you why:

```
(LAMBDA (FUNCTION FOO BAR) (N) (BAZ N))
```

Ew.

You need to account for at least those two edge cases.

```
(defmacro -> (exp &rest ops)
  (reduce
   (lambda (memo op)
     (cond ((atom op) `(,op ,memo))
	   ((and (consp op)
		 (or (eq 'cl:function (car op))
		     (eq 'cl:lambda (car op))))
	    `(funcall ,op ,memo))
	   (t `(,(first op) ,memo ,@(rest op)))))
   ops :initial-value exp))
```

That should do the correct thing here.

```
CLJ> (macroexpand '(-> foo #'bar (lambda (n) (baz n)) mumble))
(MUMBLE (FUNCALL (LAMBDA (N) (BAZ N)) (FUNCALL #'BAR FOO)))
T
CLJ>
```

Testing these by example is probably a thing we want. The particular two arrows we have above have some common ground:

```
(subtest "Common threads"
  (is-expand (-> a foo) (foo a)
	     "Thread applies a function to an argument")
  (is-expand (-> a foo bar) (bar (foo a))
	     "Thread can compose multiple functions")
  (is-expand (->> a foo) (foo a)
	     "Rthread applies a function to an argument")
  (is-expand (->> a foo bar) (bar (foo a))
	     "Rthread can compose multiple functions"))
```

Function application and trivial composition ends up coming out the same way regardless of which you pick. That's where the similarities end though.

```
 (subtest "Thread"
   (is-expand (-> a (foo 1)) (foo a 1)
	      "If called with a partially applied multi-argument function, thread plants the target in the first slot")
   (is-expand (-> a (foo 1) (foo 2)) (foo (foo a 1) 2)
	      "Thread can nest multi-arity function calls")
   (is-expand (-> a foo (bar 1) (bar 2)) (bar (bar (foo a) 1) 2)
	      "Thread can nest single and multi-arity function calls")
   (is-expand (-> a foo (bar 1) (bar 2) baz) (baz (bar (bar (foo a) 1) 2))
	      "Thread can nest single and multi-arity function calls. Again.")
   (is-expand (-> a #'foo (lambda (b) (bar b)) baz)
	      (BAZ (FUNCALL (LAMBDA (B) (BAR B)) (FUNCALL #'FOO A)))
	      "Thread handles #' terms and lambda forms properly"))
```

Using `->` stitches each sub-expression in as the first argument of the next, while using `->>` slots it into the _last_ argument.

```
(subtest "Rthread"
   (is-expand (->> a (foo 1)) (foo 1 a)
	      "If called with a partially applied multi-argument function, rthread plants the target in the last slot")
   (is-expand (->> a (foo 1) (foo 2)) (foo 2 (foo 1 a))
	      "Rthread can nest multi-arity function calls")
   (is-expand (->> a foo (bar 1) (bar 2)) (bar 2 (bar 1 (foo a)))
	      "Rthread can nest single and multi-arity function calls")
   (is-expand (->> a foo (bar 1) (bar 2) baz) (baz (bar 2 (bar 1 (foo a))))
	      "Rthread can nest single and multi-arity function calls. Again.")
   (is-expand (->> a #'foo (lambda (b) (bar b)) baz)
	      (BAZ (FUNCALL (LAMBDA (B) (BAR B)) (FUNCALL #'FOO A)))
	      "Rthread handles #' terms and lambda forms properly"))
```

Realistically, the only worthwhile code here is that test section. I should just use and re-export symbols from `arrow-macros` and make sure they check out. But it's still a good idea to think about what must be underneath the immediate interface we see, and why it might be there.

### Anaphoric lambda
_Implementation Complexity: Tricky_

Anaphoric `lambda`, as opposed to `if-let` and `when-let` takes some matching to get working properly. So, we need to pull in [`optima`](https://github.com/m2ym/optima) for this one. I _don't_ think some of the things that the Clojure dudes are up to in terms of argument lists are worth the complexity. In particular, the way they do optional arguments, while it is slightly more flexible, doesn't end up being especially useful given the complexity it introduces. I prefer the Common Lisp approach to `rest`/`body`/`keyword` args. Their `fn` has one trick up its' sleeve that I definitely want though, which is the ability to refer to itself.

```clojure
user=> (fn a [b c] (if (even? c) (+ b c) (a b (inc c))))
#object[user$eval2021$a__2022 0x52b57247 "user$eval2021$a__2022@52b57247"]
user=> ((fn a [b c] (if (even? c) (+ b c) (a b (inc c)))) 3 5)
9
user=>
```

It's not useful often, but is sometimes. And I friggin' want it. The unfortunate part of this one, is that, even _with_ `optima`, I can't figure out how to evaluate the function name only once. Also, ironically, this is one of the maybe handful of situations where Clojure-style optional arguments would be useful.

```
(defmacro fn (&rest args)
  (optima:match args
    ((optima:guard
      (cons name (cons params body))
      (and (symbolp name) (listp params)))
     `(labels ((,name ,params ,@body))
	#',name))
    ((optima:guard
      (cons params body)
      (and (listp params)))
     `(lambda ,params ,@body))))
```

The only comfort I have is that it's guaranteed to be a symbol if it's there, so I figure that's not the worst thing that could happen. The named lambda case is about the only place I can think of where it seems you literally can't get around the requirement to evaluate the argument twice.

This is because

1. The `body` needs to be able to refer to that `name`, which means it has to be present in the `labels` binding
2. We want to return that function which means we need to `name` it in the return value.

It looks like no amount of aliasing is going to get us out of this one, but feel free to correct me if I'm wrong.

### Map and Set literals with functional underpinnings
_Implementation Complexity: Difficult_

The data structures on their own are already implemented in the form of [`cl-hamt`](https://quickref.common-lisp.net/cl-hamt.html). We don't necessarily want to commit to it, but it's available and we can probably make the interface agnostic to some degree. The syntax stuff involves defining some simple reader macros. Ideally, they'd be namespace-bounded which means we're pulling out `named-readtables`.

So, reader macros are things you can do in Lisp. You can read more about them [here](https://gist.github.com/chaitanyagupta/9324402). The short version is that in order to get _namespaced_ reader macros, which we want so that we don't have to stomp on everyone elses' tightly wound DSLs, we need to use [`named-readtables`](https://common-lisp.net/project/named-readtables/).

In order to use it, you need to define some `macro-char`s and some parsing functions. Luckily for me, Common Lisp happens to already have [`read-delimited-list`](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_del.htm), which does exactly what I want when defining `map` and `set` literals.

```
(defun map-literal-reader (stream char)
  (declare (ignore char))
  (loop with dict = (cl-hamt:empty-dict)
     for (k v) on (read-delimited-list #\} stream t) by #'cddr
     do (setf dict (cl-hamt:dict-insert dict k v))
     finally (return dict))
```

This is going to do exactly what you think it is. We `read-delimited-list` from the incoming stream, with `#\}` as the stop chararcter.

```
CLJ> (with-input-from-string (stream ":a 1 :b 2}") (read-delimited-list #\} stream nil))
(:A 1 :B 2)
CLJ>
```

The function definition has a `t` in place of that trailing `nil` parameter because we'll want to read recursively from `standard-output`, but can't from a `string-stream`.

The trailing `t` of that call means we do this recursively if we have-sub-forms. Once we have the form `read`, we walk it pairwise, adding each pair of characters to a`cl-hamt:hash-dict`. Note to self; add some error handling here if we read an odd set of elements. The `set-literal-reader` is about the same thing but simpler because we don't need to worry about pairing elements off.

```
(defun list->set (lst)
  (reduce
   (lambda (set elem)
     (cl-hamt:set-insert set elem))
   lst :initial-value (cl-hamt:empty-set)))

(defun set-literal-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (list->set (read-delimited-list #\} stream t))))
```

Once that's all done, we need to define a local `read-table`

```
(named-readtables:defreadtable syntax
  (:merge :standard)
  (:macro-char #\{ #'map-literal-reader nil)
  (:macro-char #\} (get-macro-character #\)) nil)
  (:dispatch-macro-char #\# #\{ #'set-literal-reader))
```

This is such a table that uses the `:standard` Common Lisp readers, but adds support for forms beginning with  `{`/`#{` and ending with `}` to be handled as part of the read step. In some other file of the project, we need to add a call to `(named-readtables:in-readtable syntax)`. Externally, we can use `(named-readtables:in-readtable clj:syntax)` to get access to these reader macros.

Once, we get all that evaluated, we've got `map` and `set` literals.

```
CLJ> {:a 1 :b 2}
#<CL-HAMT:HASH-DICT {1003068E13}>
CLJ> #{:a :b :c}
#<CL-HAMT:HASH-SET {10034E20B3}>
CLJ>
```
And that's the very next thing I want to fix.

### Readable datastructure representations
_Implementation Complexity: Tricky_

One thing Clojure does nicely and consistently is emit both its maps and sets in a `read`-able format. Lisp does this for `list`s and `simple-vector`s, but not `hash-tables`es or `hash-dict`s. And that kind of sucks.

To get this for our representations, we need to define `print-object` methods for them. That's the long and the short of it. It's _tricky_, not _difficult_. All we need to do is make sure that the `print-object` method for our datatypes emits the syntax we defined in our macros earlier.

```
(defmethod print-object ((object cl-hamt:hash-dict) stream)
  "The printable representation for clj:maps"
  (format
       stream
       "{~{~s ~s~^ ~}}"
       (reverse (cl-hamt:dict-reduce (lambda (memo k v) (cons v (cons k memo))) object nil))))
```

We need to iterate through the `hamt-dict`, collecting keys and values along the way, then `format` them into the given stream surrounded by `{` and `}`. And with that, we've left the world of opaque data.

```
CLJ> {:a 1 :b 2}
{:A 1 :B 2}
CLJ> (prin1-to-string {:a 1 :b 2})
"{:A 1 :B 2}"
CLJ> (read-from-string (prin1-to-string {:a 1 :b 2}))
{:A 1 :B 2}
11
CLJ>
```

The only real problem left here is that it won't work unless we're in the `clj:syntax` `read-table`. As in

```
CLJ> (defparameter *D* {:a 1 :b 2 :c 3})
*D*
CLJ> *d*
{:A 1 :C 3 :B 2}
CLJ> (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> clj::*D*
{:A 1 :C 3 :B 2}
CL-USER> (prin1-to-string clj::*D*)
"{:A 1 :C 3 :B 2}"
CL-USER> (read-from-string (prin1-to-string clj::*D*))

Package { does not exist.

  Line: 1, Column: 2, File-Position: 2

  Stream: #<SB-IMPL::STRING-INPUT-STREAM {1008382BB3}>
   [Condition of type SB-INT:SIMPLE-READER-PACKAGE-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1004F29B63}>)

Backtrace:
  0: (SB-IMPL::READER-FIND-PACKAGE "{" #<SB-IMPL::STRING-INPUT-STREAM {1008382BB3}>)
  1: (SB-IMPL::READ-TOKEN #<SB-IMPL::STRING-INPUT-STREAM {1008382BB3}> #\{)
  2: (SB-IMPL::READ-MAYBE-NOTHING #<SB-IMPL::STRING-INPUT-STREAM {1008382BB3}> #\{)
  3: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {1008382BB3}> T (NIL) T)
  4: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {1008382BB3}> T (NIL) NIL)
  5: (READ #<SB-IMPL::STRING-INPUT-STREAM {1008382BB3}> T NIL NIL)
  6: (SB-IMPL::%READ-FROM-STRING "{:A 1 :C 3 :B 2}" T NIL 0 NIL NIL)
  7: (SB-INT:SIMPLE-EVAL-IN-LEXENV (READ-FROM-STRING (PRIN1-TO-STRING CLJ::*D*)) #<NULL-LEXENV>)
  8: (EVAL (READ-FROM-STRING (PRIN1-TO-STRING CLJ::*D*)))
 --more--
 ```

Not a deal-breaker exactly, but it's not a great thing for interoperability. If we define our `print-object` as

```
(defmethod print-object ((object cl-hamt:hash-dict) stream)
  "The printable representation for clj:maps"
  (if (eq 'clj:syntax (named-readtables:readtable-name *readtable*))
      (format
       stream
       "{~{~s ~s~^ ~}}"
       (reverse (cl-hamt:dict-reduce (lambda (memo k v) (cons v (cons k memo))) object nil)))
      (format stream "(CLJ:ALIST->MAP (LIST ~{~S~^ ~}))" (cl-hamt:dict->alist object))))
```

instead, and additionally define

```
(defun alist->map (alist)
  (loop with dict = (cl-hamt:empty-dict :test #'==)
     for (k . v) in alist do (setf dict (cl-hamt:dict-insert dict k v))
     finally (return dict)))
```

we get what we want, although the representation _is_ uglier.

```
CL-USER> clj::*D*
(CLJ:ALIST->MAP (LIST (:B . 2) (:C . 3) (:A . 1)))
CL-USER> (prin1-to-string clj::*D*)
"(CLJ:ALIST->MAP (LIST (:B . 2) (:C . 3) (:A . 1)))"
CL-USER> (read-from-string (prin1-to-string clj::*D*))
(CLJ:ALIST->MAP (LIST (:B . 2) (:C . 3) (:A . 1)))
50
CL-USER>
```

The corresponding `set` method is

```
(defmethod print-object ((object cl-hamt:hash-set) stream)
  (if (eq 'clj:syntax (named-readtables:readtable-name *readtable*))
      (format stream "#{~{~s~^ ~}}" (cl-hamt:set->list object))
      (format stream "(CLJ:LIST->SET (LIST ~{~S~^ ~}))" (cl-hamt:set->list object))))
```

Next, we do in fact want these to be recursive. And, currently, they're not.

```
CLJ> {:a 1 :b 2}
{:A 1 :B 2}
CLJ> {:a 1 :b {:c 3}}
{:A 1 :B {:C 3}}
CLJ> {{:a 1} {:b 2}}
Don't know how to hash {:A 1}
   [Condition of type CL-MURMURHASH:UNHASHABLE-OBJECT-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1004F29B63}>)

Backtrace:
  0: ((:METHOD CL-MURMURHASH:MURMURHASH (T)) {:A 1}) [fast-method]
  1: (CL-HAMT:DICT-INSERT {} {:A 1} {:B 2})
  2: (MAP-LITERAL-READER #<SB-IMPL::STRING-INPUT-STREAM {1002A726D3}> #<unused argument>)
  3: (SB-IMPL::READ-MAYBE-NOTHING #<SB-IMPL::STRING-INPUT-STREAM {1002A726D3}> #\{)
  4: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {1002A726D3}> NIL (NIL) T)
  5: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {1002A726D3}> NIL (NIL) NIL)
  6: (READ #<SB-IMPL::STRING-INPUT-STREAM {1002A726D3}> NIL #<SB-IMPL::STRING-INPUT-STREAM {1002A726D3}> NIL)
 --more--
CLJ> #{1 #{:a} #{"a"}}

Don't know how to hash #{:A}
   [Condition of type CL-MURMURHASH:UNHASHABLE-OBJECT-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1004F29B63}>)

Backtrace:
  0: ((:METHOD CL-MURMURHASH:MURMURHASH (T)) #{:A}) [fast-method]
  1: ((FLET CL-HAMT::%INSERT :IN CL-HAMT:SET-INSERT) #<CL-HAMT::SET-TABLE {10028D25D3}> #{:A})
  2: (REDUCE #<CLOSURE (FLET CL-HAMT::%INSERT :IN CL-HAMT:SET-INSERT) {10028D265B}> (#{:A}) :INITIAL-VALUE #<CL-HAMT::SET-TABLE {10028D25D3}>)
  3: (CL-HAMT:SET-INSERT #{1} #{:A})
  4: (REDUCE #<FUNCTION (LAMBDA (SET ELEM) :IN LIST->SET) {228B851B}> (1 #{:A} #{"a"}) :INITIAL-VALUE #{})
  5: (SB-IMPL::READ-MAYBE-NOTHING #<SB-IMPL::STRING-INPUT-STREAM {100247DD93}> #\#)
  6: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {100247DD93}> NIL (NIL) T)
  7: (SB-IMPL::%READ-PRESERVING-WHITESPACE #<SB-IMPL::STRING-INPUT-STREAM {100247DD93}> NIL (NIL) NIL)
  8: (READ #<SB-IMPL::STRING-INPUT-STREAM {100247DD93}> NIL #<SB-IMPL::STRING-INPUT-STREAM {100247DD93}> NIL)
 --more--
```
- Show the methods, go over the tests
- Show off how this works outside of the `clj` readtable

### Recursive dicts and sets
_Implementation Complexity: Tricky_

We need to define a hashing method that is `cl-hamt` compatible and works with [`cl-murmurhash`](https://github.com/ruricolist/cl-murmurhash). All we really need are specializers on `hash-set` and `hash-dict`. Again, _tricky_, not _difficult_. It looks like

```
(defmethod cl-murmurhash:murmurhash ((object cl-hamt:hash-dict) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (cl-hamt:dict->alist object) :seed seed :mix-only mix-only))

(defmethod cl-murmurhash:murmurhash ((object cl-hamt:hash-set) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (cl-hamt:set->list object) :seed seed :mix-only mix-only))
```

With that, `set`s can contain `set`s, `map`s can contain `map`s, even as keys, and they can both contain each other. Sort of.

```
CLJ> {:a 1}
{:A 1}
CLJ> {:a {:b 1}}
{:A {:B 1}}
CLJ> {{:a 1} {:b 1}}
{{:A 1} {:B 1}}
CLJ> (cl-hamt:dict-lookup {{:a 1} {:b 1}} {:a 1})
NIL
NIL
CLJ> (cl-hamt:set-lookup #{1 2 {:a 1}} {:a 1})
NIL
```

The problem is that two `set`s/`dict`s are not `equal` or even `equalp` to each other.

```
CLJ> (equal {:a 1} {:a 1})
NIL
CLJ> (equalp {:a 1} {:a 1})
NIL
CLJ> (equal #{:a 1} #{:a 1})
NIL
CLJ> (equalp #{:a 1} #{:a 1})
NIL
CLJ>
```

So to get any real use out of this, we actually also need...

### Polymorphic operators
_Implementation Complexity: Fiendish_

Clojure doesn't have a bunch of equality operators. It has one; [`=`](https://clojure.org/guides/equality). [Equality is fraught](http://people.math.harvard.edu/~mazur/preprints/when_is_one.pdf), and if you don't believe me, give that a read. The basics are actually easy.

```
(defmethod == (a b) (equalp a b))
(defmethod == ((a cl-hamt:hash-dict) (b cl-hamt:hash-dict))
  (cl-hamt:dict-eq a b :value-test #'==))
(defmethod == ((a cl-hamt:hash-set) (b cl-hamt:hash-set))
  (cl-hamt:set-eq a b))
```

With that,

```
CLJ> (== {:a 1} {:a 1})
READING :A -> 1
READING :A -> 1
T
CLJ> (== {:a 1} {:a 2})
READING :A -> 1
READING :A -> 2
NIL
CLJ> (== #{:a 1} #{:a 1})
T
CLJ> (== #{:a 1} #{:a 2})
NIL
CLJ>
```

And, once we wire up our literal definition to account for it,

```
...
(defun alist->map (alist)
  (loop with dict = (cl-hamt:empty-dict :test #'==)
     for (k . v) in alist do (setf dict (cl-hamt:dict-insert dict k v))
     finally (return dict)))

(defun map-literal-reader (stream char)
  (declare (ignore char))
  ;; TODO - check for an even number of elements here and throw an error if there isn't
  (loop with dict = (cl-hamt:empty-dict :test #'==)
     for (k v) on (read-delimited-list #\} stream t) by #'cddr
     do (setf dict (cl-hamt:dict-insert dict k v))
     finally (return dict))))
...
(defun list->set (lst)
  (reduce
   (lambda (set elem)
     (cl-hamt:set-insert set elem))
   lst :initial-value (cl-hamt:empty-set :test #'==))))
...
```

we get the behavior we want out of our hashes.

```
CLJ> (cl-hamt:dict-lookup {:A 1 {:B 2} :C} {:b 2})
:C
T
CLJ> (cl-hamt:dict-lookup {{:a 1} {:b 2}} {:a 1})
{:B 2}
T
CLJ>
```

In the correctness sense, at least.

The problem is, we've entered generic function land. And that's good in terms of interface, but if the day comes when we want to squeeze extra performance out of these structures, it might be annoying to pay the cost of performing a fully polymorphic equality. What we'd really want is a way to declare the types of our `dict`s/`set`s and use the tightest applicable sense of equality for those types so that the inliner has a chance of doing us some good for those cases where performance matters.

I _think_ the best way to do this is to introduce type hints. Something like

```
(:: (map keyword integer) {:a 1 :b 2})
```

which would put together a map that assumes keyword keys and integer values. I'm going to do some testing, research and/or profiling before embarking on this journey. To be fair, [Clojure equality performance](https://clojureverse.org/t/is-fast-in-clojure/2182/2) is also kind of up in the air, so this might not be the biggest deal. I'd still like to give it some serious thought?

That's all I've got the energy for in one go. I'll keep you up to date on further developments.
