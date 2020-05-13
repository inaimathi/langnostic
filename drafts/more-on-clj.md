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

The only finer point here is the use of the intermediate variable `tmp`, because we don't want to evaluate either `name` or `test` more than once.

### Arrow macros
_Implementation Complexity: Simple_

The basic arrow macros are `->` and `->>`. There's a few more that you can see over at the [`arrow-macros` project](https://github.com/hipeta/arrow-macros/blob/master/arrow-macros.lisp), but I'm interested, as usual, in keeping this library minimal. So for the moment, I'm only implementing the two above. The only non-trivial part of this is compensating for `lambda` expressions and `#'` symbols.

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

because that fails on inputs like `(-> foo #'bar (lambda (n) (baz n)))`. Macroexpanding it will show you why: `(LAMBDA (FUNCTION FOO BAR) (N) (BAZ N))`. Ew.

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

- TODO - show some tests to verify the above properties and go through some testing stuff

Realistically, I should just use and re-export symbols from `arrow-macros` and make sure they test out under the same properties, but it's still a good idea to think about what must be underneath the immediate interface we see.

### Anaphoric lambda
_Implementation Complexity: Tricky_

Anaphoric `lambda`, as opposed to `if-let` and `when-let` takes some matching to get working properly. So, we need to pull in [`optima`](TODO) for this one. I _don't_ think some of the things that the Clojure dudes are up to in terms of argument lists are worth the complexity. In particular, the way they do optional arguments, while it does sound more flexible, doesn't seem especially useful given the complexity it introduces. I prefer the Common Lisp approach to `rest`/`body`/`keyword` args. Their `fn` has one trick up its' sleeve that I definitely want though, which is the ability to refer to itself.

```clojure
user=> (fn a [b c] (if (even? c) (+ b c) (a b (inc c))))
#object[user$eval2021$a__2022 0x52b57247 "user$eval2021$a__2022@52b57247"]
user=> ((fn a [b c] (if (even? c) (+ b c) (a b (inc c)))) 3 5)
9
user=>
```

It's not useful often, but is sometimes. And I friggin' want it. The unfortunate part of this one, is that, even _with_ `optima`, I can't figure out how to evaluate the function name only once.

```
(defmacro fn (&rest args)
  (optima:match args
    ((optima:guard
      (cons name (cons params (cons docstring body)))
      (and (symbolp name) (listp params) (stringp docstring)))
     `(labels ((,name ,params ,docstring ,@body))
	#',name))
    ((optima:guard
      (cons name (cons params body))
      (and (symbolp name) (listp params)))
     `(labels ((,name ,params ,@body))
	#',name))
    ((optima:guard
      (cons params (cons docstring body))
      (and (listp params) (stringp docstring)))
     `(lambda ,params ,docstring ,@body))
    ((optima:guard
      (cons params body)
      (and (listp params)))
     `(lambda ,params ,@body))))
```

The only comfort I have is that it's guaranteed to be a symbol, so I figure that's not the worst thing that could happen.

### Hash and Set literals with functional underpinnings
_Implementation Complexity: Difficult_

The data structures on their own are already implemented in the form of [`cl-hamt`](TODO). We don't necessarily want to commit to it, but it's available and we can probably make the interface agnostic to some degree. The syntax stuff involves defining some simple reader macros. Ideally, they'd be namespace-bounded which means we're pulling out `named-readtables`.

- Summarize `named-readtables` use, and how to go about writing simple readers using the built-in `read-delimited-list`. Show and explain the initial implementation.

### Readable datastructure representations
_Implementation Complexity: Tricky_

This one's really only difficult because of the testing involved. One thing Clojure does nicely and consistently is emit both its hashes and sets in a `read`-able format. Lisp does this for `list`s and `simple-vector`s, but not `hash`es. And that kind of sucks.

To get this for our representations, we need to define `print-object` methods for them.

- Show the methods, go over the tests

### Recursive dicts and sets
_Implementation Complexity: Tricky_

We need to define a hashing method that is `cl-hamt` compatible and works with [`cl-murmurhash`](TODO). That's relatively simple, because `murmurhash` is a [generic function](TODO), so all we really need are specializers on `hash-set` and `hash-dict`. That looks something like

```
(defmethod cl-murmurhash:murmurhash ((object cl-hamt:hash-dict) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (cl-hamt:dict->alist object) :seed seed :mix-only mix-only))

(defmethod cl-murmurhash:murmurhash ((object cl-hamt:hash-set) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (cl-hamt:set->list object) :seed seed :mix-only mix-only))
```

With that, `set`s can contain `set`s, `dict`s can contain `dict`s, even as keys, and they can both contain each other. Sort of.

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
(defun hash-literal-reader (stream char)
  (declare (ignore char))
  (loop with dict = (cl-hamt:empty-dict :test #'==)
     for (k v) on (read-delimited-list #\} stream t) by #'cddr
     do (setf dict (cl-hamt:dict-insert dict k v))
     finally (return dict)))
...
(defun set-literal-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (reduce
   (lambda (set elem)
     (cl-hamt:set-insert set elem))
   (read-delimited-list #\} stream t)
   :initial-value (cl-hamt:empty-set :test #'==)))
...
```

we get the behavior we want out of our hashes.

```
CLJ> (cl-hamt:dict-lookup {:A 1 {:B 2} :C} {:b 2})
READING :B -> 2
:C
T
CLJ>
```

In the correctness sense, at least. The problem is, we've entered generic function land. And that's good in terms of interface, but if the day comes when we want to squeeze extra performance out of these structures, it might be annoying to pay the cost of performing a polymorphic equality. What we'd really want is a way to declare the types of our `dict`s/`set`s and use the tightest applicable sense of equality for those types so that the inliner has a chance of doing us some good for those cases where performance matters.

I _think_ the best way to do this is to introduce type hints for the datatypes I'll be putting together. I'm thinking a syntax that looks something like

```
(:: (-> keyword integer) {:a 1 :b 2})
```

which would put together a map that assumes keyword keys and integer values. I'm going to do some testing, research and/or profiling before embarking on this journey. And, to be fair, [Clojure equality performance](https://clojureverse.org/t/is-fast-in-clojure/2182/2) is also kind of up in the air, so shrug I guess?

That's all I've got the energy for in one go. I'll keep you up to date on further developments.
