I mentioned the [`clj`](TODO) repo last time. And mentioned off-handedly that there's a bunch of things I _really_ like about Clojure, and also a few reason that, for my personal use at least, I'm definitely going to continue developing things in Common Lisp.

I intend to do something about this. If you like, you can [follow along](TODO -- link to clj repo). Who knows, we may both learn something. We'll go from the trivial, to the difficult but hopefully possible. I'm not writing this piece after finishing development, the intent is to journal as I go.

### Readable Anaphora
_Implementation Complexity: Trivial_

There's a chapter in [On Lisp](TODO) that defines some [anaphoric macros](TODO). Things that bind the results of intermediate computations and expose them as symbols in their result bodies. The archetypal ones are `aif` and `awhen`, roughly defined as

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

- we need to define a hashing method that is `cl-hamt` compatible and works with [`cl-murmurhash`](TODO).
- we need a polymorphic equality. It needs to be designed to be simple to use to start with, but bypassable if the user wants to optimize for performance and have things inlined.
