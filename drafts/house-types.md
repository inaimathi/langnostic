So I've started thinking about that extension to the [`house` type system](https://github.com/inaimathi/house/blob/c4344c075b3fda0780a164338b365e8316abb9e1/define-handler.lisp#L3-L50) I mentioned last time, and I need to think about it out loud for a bit.

Firstly, regardless of whatever else I do, this `type-priority` bullshit has to go. It seems saner and more sensical to just process parameters left-to-right, and allow each parameter to see all previously processed parameters. It seems like I'd have to very deliberately try to prevent it, and it adds some weird levels of complexity to boot.

Secondly, the goal of this exercise is to be able to do something like

```lisp
(define-handler (foo) ((bar (list integer)) (baz integer))
   (mapcar (lambda (i) (+ baz i)) bar))
```

or

```lisp
(define-handler (bar) ((baz (alist string (list integer))) (mumble (list keyword)) (flarp (optional string)))
   ...)
```

That is, I'd like to be able to specify container types, rather than doing the stupid current thing of defining separate monomorphic types for different use-cases. Specifically, see [this chunk](https://github.com/inaimathi/house/blob/c4344c075b3fda0780a164338b365e8316abb9e1/define-handler.lisp#L43-L50), where I've declared `list-of-keyword` and `list-of-integer` parsers separately from `integer` and `keyword` parsers. It annoys me to no end when type systems force that busywork on me, so I don't want to inflict it on my users either.

## First Cut

So here's something that might work.

```lisp
(define-condition parameter-parse-error (error)
  ((parameter-value :initarg :parameter-value :initform nil :reader parameter-value)
   (expected-type :initarg :expected-type :initform nil :reader expected-type))
  (:report (lambda (condition stream)
	     (format stream "Failed to parse ~s to ~a"
		     (parameter-value condition)
		     (expected-type condition)))))

(defmacro define-http-type (type-name (&rest args) &body body)
  `(defun ,(intern (format nil "HTTP-TYPE-~a" type-name) *package*) ,(butlast args)
     (lambda ,(last args)
       (handler-case
	   (progn ,@body)
	 (parameter-parse-error (e) (error e))
	 (error ()
	   (error
	    (make-instance
	     'parameter-parse-error
	     :expected-type ',type-name
	     :parameter-value ,(car (last args)))))))))
```

We have to do the ridiculous symbol-renaming thing, because we couldn't otherwise have an `http-type` named `integer`[^actually-its-more-complex].

[^actually-its-more-complex]: Actually, it's more complex than that. I think we could theoretically define our own package named something like `http-types`, which doesn't include `cl` or `cl-user`, then define arbitrarily named symbols into it as part of this procedure. The problem with that is i _think_ I want users to have the ability to easily define their own version of the container types like `list`. The easiest way of doing that which I can see is to use the package system, which means _not_ using it for our own convenience here. I still need to _thoroughly_ think through the implications though. It may be that the separate-package approach wins out in the end, or it may be that I end up rolling my own symbol table as a hash or something similarly quasi-ridiculous.

With that, we can do things like

```lisp
(define-http-type list (v x)
  (loop for elem in (json:decode-json-from-string x)
     collect (funcall v elem)))

(define-http-type json (x)
  (json:decode-json-from-string x))

(define-http-type integer (x)
  (integer x))
```

And with _that_, we can do

```lisp
HOUSE> (http-type-list (http-type-integer))
#<CLOSURE (LAMBDA (X) :IN HTTP-TYPE-LIST) {1005A53C4B}>
HOUSE> (funcall (http-type-list (http-type-integer)) "[\"1\", \"2\", \"3\"]")
(1 2 3)
HOUSE>
```

Which is _sort of_ what we want.

## Why "Sort Of"?

You'll notice that in order for these definitions to work, we actually need to try parsing an ecoded list of strings that encode numbers. This is unsatisfactory somehow, but given the infrastructure we've defined, you can't actually write

```lisp
HOUSE> (funcall (http-type-list (http-type-integer)) "[1, 2, 3]")

Failed to parse 1 to INTEGER
   [Condition of type PARAMETER-PARSE-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {10031B0033}>)

Backtrace:
  0: ((LAMBDA (X) :IN HTTP-TYPE-LIST) "[1, 2, 3]")
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (FUNCALL (HTTP-TYPE-LIST (HTTP-TYPE-INTEGER)) "[1, 2, 3]") #<NULL-LEXENV>)
  2: (EVAL (FUNCALL (HTTP-TYPE-LIST (HTTP-TYPE-INTEGER)) "[1, 2, 3]"))
```

The problem being that if our `integer` parser gets something that's _already_ an integer, it has no fucking idea what to do...

## More Thought Required

I'm not entirely sure what the mistake here is. It's possible that I've defined `list` poorly, and that it shouldn't rely on a json-parse in order to functino. It's possible that I've defined `integer` poorly, and that it should consider what to do in the event that it gets an Integer _as input_. Or, as a friend pointed out to me, it's possible that I'm taking the type metaphor too seriously here.

Specifically, what we're defining as part of house type handlers aren't really type annotations; they're transformers of some kind, which start with a chain from `string`s and end up in whatever the appropriate data-structure is for some piece of business logic. An entirely valid approach is to say that we should accept functions in the "type annotation" slots of a `define-handler` form, and run the fucker on whatever our parameter happened to be, with additional error-handling logic that propagates HTTP errors back to the client somehow. This means that the situation is potentially a lot more general than type-checking systems a-la [Hindley Milner](http://www.sciencedirect.com/science/article/pii/0022000078900144) or descendants. So maybe something to consider is just writing a set of parser functions and composition utilities that make defining new ones easy, and letting the user pass those in as desired. This does raise a couple new concerns, of course. Specifically, we do need to be more careful about the error-handling surrounding these primitives, since our new set of assumptions now allows them to error in new and exciting ways that aren't necessarily the clients' fault. This kind of validates my suspicion that I should provide a debugging mode for `house` of the same style present in `hunchentoot`, which drops you into a local debugger on error for ease of error trapping.

It doesn't feel like I'm going to get much further on this, so I'm backburnering it for now. The next step is moving over to one of the other things I've meant to do for the longest time, and leaving this particular use case for when I get enough concrete use-cases in mind to make a proper run at it. So it goes sometimes.
