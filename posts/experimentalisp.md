So [here's](https://github.com/Inaimathi/experimentalisp) something I poured a few days into so far. It's because I'm curious about a few things, and it seemed like a good idea at the time. Here's what I've found out so far:

## <a name="manual-memory-management-sucks-balls"></a>Manual Memory Management Sucks Balls

In [a previous piece](/article?name=write-myself-a-scheme.html), I mentioned that a C implementation of a simple LISP ran something like 1600 lines of code. Firstly, that 1600 lines still has some odd corners; the finished implementation is going to be closer to 2k. Secondly, the vast majority of it concerns itself with memory layout. It turns out that just cutting out memory management, and relying on the underlying platforms' ability to optimize tail calls gets rid of most of the complexity.

```
~/projects/experimentalisp $ wc -l *rkt
  74 evaluator.rkt
  45 experimentalisp.rkt
 105 model.rkt
 224 total
```

and lest you say "Well of course it's more concise to implement a LISP *in* a LISP; you get the reader for free!", keep the following in mind

```
~/projects/experimentalisp $ wc -l *hs
   65 Evaluator.hs
   31 Experimentalisp.hs
   91 Model.hs
   81 Reader.hs
  268 total
```

Granted, the [Racket](racket-lang.org/) version currently has a slight edge on features, but when I get around to experimenting heavily with reader macros, I'll have to re-implement most of the built-in reader to support them properly. At that point we'll be in a situation where a LISP-in-Racket is about as verbose as a LISP-in-Haskell, so it's not just that my substrate has s-expressions this time. The situation supports an off-handed comment I heard that most of the hard work of a Scheme-like is in the memory manager, and in making sure tail calls are optimized properly.

## <a name="partials-and-rest-args-might-get-along"></a>Partials And Rest Args Might Get Along

Still speculative, since I haven't gotten around to mixing them yet, but here's what we've got in the Racket implementation

```scheme
;; model.rkt
...
(define (make-partial thing new-args)
  (letrec ((argl (arglist-of thing))
           (rec (lambda (ks vs)
                  (cond ((and (null? ks) (null? vs)) '())
                        ((null? vs) ks)
                        ((null? ks) (error "Too many arguments"))
                        (else (rec (cdr ks) (cdr vs)))))))
    (if (partial? thing)
        (partial (rec argl new-args) (append (partial-values thing) new-args) (body-of thing))
        (partial (rec argl new-args) new-args thing))))

(define (collapse a-partial)
  (arglist-env!
   (extend-env (environment-of a-partial))
   (arglist-of (body-of a-partial))
   (partial-values a-partial)))

(define (arglist-env! env arglist args)
  (if (and (null? arglist) (null? args))
      env
      (arglist-env! 
       (bind! env (car arglist) (car args)) 
       (cdr arglist) (cdr args))))
...
```

There's few more utility functions whose operations should be obvious from their names, but I'll talk about them if I get questions. In addition to the above model chunk, the `apply` function had to change mildly

```scheme
;; evaluator.rkt
...
(define (exp-apply op args env)
  (let* ((fn (exp-eval op env))
         (final-args
          (if (or (fexpr? fn) (fexpr-partial? fn))
              args
              (eval-args args env)))
         (p (make-partial fn final-args)))
    (if (complete? p)
        (let ((f (body-of p))
              (new-env (collapse p)))
          (cond ((primitive? f)
                 ((body-of f) new-env))
                ((procedure? f)
                 (eval-sequence (body-of f) new-env))
                ((fexpr? f)
                 (exp-eval 
                  (eval-sequence (body-of f) new-env)
                  env))))
        p)))
```

So. In order to do a function application, we


1.   evaluate the operator
1.   evaluate the arguments, unless we're dealing with a `fexpr` or `fexpr partial`
1.   make a `partial` out of the evaluated function and arguments
1.   if the `partial` is complete, apply its operation to its collected arguments, otherwise return the `partial`


The way we make a partial is

```scheme
(define (make-partial thing new-args)
  (letrec ((argl (arglist-of thing))
           (rec (lambda (ks vs)
                  (cond ((and (null? ks) (null? vs)) '())
                        ((null? vs) ks)
                        ((null? ks) (error "Too many arguments"))
                        (else (rec (cdr ks) (cdr vs)))))))
    (if (partial? thing)
        (partial (rec argl new-args) (append (partial-values thing) new-args) (body-of thing))
        (partial (rec argl new-args) new-args thing))))
```

We figure out how many arguments we're still expecting, save that partial list and save the values so far. If we're making a `partial` *out of* a `partial`, we just extend that partials' existing information rather than nesting partials, or mutating the original. Finally, once we get a complete partial, we need to collapse it into an environment where its stored values are bound to the appropriate names.

```scheme
(define (collapse a-partial)
  (arglist-env!
   (extend-env (environment-of a-partial))
   (arglist-of (body-of a-partial))
   (partial-values a-partial)))

(define (arglist-env! env arglist args)
  (if (and (null? arglist) (null? args))
      env
      (arglist-env! 
       (bind! env (car arglist) (car args)) 
       (cdr arglist) (cdr args))))
```

To do that, we extend the environment of this partials' operator and bind the arguments to their names in the resulting environment. This lets us do things like

```
~/projects/experimentalisp $ racket -t experimentalisp.rkt -m
experimentalisp 0.001
 Base env:
 (#hash((cdr . #&lt;primitive>) (+ . #&lt;primitive>) (print . #&lt;primitive>) (/ . #&lt;primitive>) (- . #&lt;primitive>) (* . #&lt;primitive>) (the-env . #&lt;primitive>) (cons . #&lt;primitive>) (= . #&lt;primitive>) (car . #&lt;primitive>)))
EXP>> (+ 2)
#&lt;partial>

EXP>> ((+ 2) 4)
6

EXP>> (def map 
  (fn (f lst) 
    (if (= lst '())
      '()
      (cons (f (car lst))
            (map f (cdr lst))))))
'()

EXP>> (map (+ 2) '(1 2 3 4 5))
'(3 4 5 6 7)

EXP>> 
```

Now. I'm not sure this is the correct model for partials. That's sort of the point of this project; finding out these little truths through experimentation so that I can understand what it is that you'd actually want. But, the implementation you see above would co-exist just fine with functions that have `&rest` or `&key`. The cost would be that you can't partially apply more arguments after you've applied the mandatory ones. So for instance, if you had a function like

```
(def foo (fn (a b &rest r) ...))
```

you *could* do `((foo 5) 6)`, or `((foo 5) 6 7 8 9 10)` or even `(foo 5 6 7 8 9 10)`. You just couldn't do `((foo 5 6) 7 8 9 10)` because `(foo 5 6)` is already a complete application, and will therefore be evaluated to its result before getting the rest of the arguments passed<a name="note-Fri-Dec-12-094216EST-2014"></a>[|1|](#foot-Fri-Dec-12-094216EST-2014).

## <a name="fexprs-are-surprisingly-easy"></a>Fexprs Are Surprisingly Easy

It's about a four-line change to a `fexpr`-less scheme interpreter to add in `fexpr` support. You already saw them in scheme up above, in the exp-apply function. So lets take a look at the Haskell version, just to keep you on your toes

```haskell
apply :: LispVal -> LispVal -> Environment -> (LispVal, Environment)
apply exp args env = 
    case eval exp env of
      (Primitive fn arglist, env') -> 
          fn $ arglist_env (extend env') arglist $ eval_args args env'
      (Procedure local_env arglist body, env') -> 
          eval_sequence body $ arglist_env (extend local_env) arglist $ eval_args args env'
      (Fexpr local_env arglist body, env') -> 
          eval res env'
                where (res, _) = eval_sequence body $ arglist_env (extend local_env) arglist args
      _ -> error $ "Undefined function '" ++ show exp ++ "'"
```

The main difference between this one and the Racket version is that this one needs to return both a `LispVal`(expression) and an `Environment`, since I'm trying to have no side-effects here. We'll come back to that in a minute. The lines we're concerned with right now are

```haskell
...
(Fexpr local_env arglist body, env') -> 
    eval res env'
          where (res, _) = eval_sequence body $ arglist_env (extend local_env) arglist args
...
```

If you're applying a `Fexpr` to some arguments, you're going to pass it the **unevaluated** arguments, evaluate its body just as you would with a `Procedure`, and then you're going to call `eval` on the result. The idea is that a `fexpr` is a function that returns a new syntax tree to evaluate in place of its original call. It just so happens that in Lisp, you have access to the underlying data structure it uses to form its own AST, which means you get this some very powerful syntactic abstraction facilities for free. This is what [McCarthy means](http://www.infoq.com/interviews/mccarthy-elephant-2000) when he asks whether Ruby uses "list structures as data".

This *isn't* [what macros are](http://axisofeval.blogspot.ca/2010/07/whats-phase-separation-and-when-do-you.html). The main difference being that macros literally don't exist at run time. Which is why they're overall more efficient, and why you have problems when you try to `(apply and {args})` in most LISPs.

## <a name="lisp-interpreters-are-fundamentally-effectful"></a>LISP Interpreters are Fundamentally Effectful

Lets take a closer look at that Haskell implementation of `apply`

```haskell
apply :: LispVal -> LispVal -> Environment -> (LispVal, Environment)
apply exp args env = 
    case eval exp env of
      (Primitive fn arglist, env') -> 
          fn $ arglist_env (extend env') arglist $ eval_args args env'
      (Procedure local_env arglist body, env') -> 
          eval_sequence body $ arglist_env (extend local_env) arglist $ eval_args args env'
      (Fexpr local_env arglist body, env') -> 
          eval res env'
                where (res, _) = eval_sequence body $ arglist_env (extend local_env) arglist args
      _ -> error $ "Undefined function '" ++ show exp ++ "'"
```

For contrast, compare this to the Racket version from earlier

```scheme
(define (exp-apply op args env)
  (let* ((fn (exp-eval op env))
         (final-args
          (if (or (fexpr? fn) (fexpr-partial? fn))
              args
              (eval-args args env)))
         (p (make-partial fn final-args)))
    (if (complete? p)
        (let ((f (body-of p))
              (new-env (collapse p)))
          (cond ((primitive? f)
                 ((body-of f) new-env))
                ((procedure? f)
                 (eval-sequence (body-of f) new-env))
                ((fexpr? f)
                 (exp-eval 
                  (eval-sequence (body-of f) new-env)
                  env))))
        p)))
```

Based just on reading the Racket version, you might come to the conclusion that `apply`ing an operator to arguments involves exactly two environments: the initial one passed into `apply`, and the one you create by extending the callables' environment with its new arguments. This is not true. And it becomes obvious when you lose the ability to define things by mutating your environment. Zoom in on the `procedure` clauses specifically

```haskell
-- Evaluator.hs
...
(Procedure local_env arglist body, env') -> 
    eval_sequence body $ arglist_env (extend local_env) arglist $ eval_args args env'
...
```

```scheme
;; evaluator.rkt
...
 (final-args
  (if (or (fexpr? fn) (fexpr-partial? fn))
      args
      (eval-args args env)))
...
       (let ((f (body-of p))
             (new-env (collapse p)))
...
               ((procedure? f)
                (eval-sequence (body-of f) new-env))
...
```

At that level, especially in the Haskell version, it should be obvious that the environment you're finally evaluating this call in is not the same one you're getting passed in *or* the one resulting from a trivial binding of arguments. In fact, if you look closely enough, you'll see that there's a bug in the Haskell version that mishandles `set!` and `def` calls passed as args. Here, imagine this hypothetical. You've got a function named foo that takes two arguments, and makes reference to some global symbol.

```
(def mumble "A label")
(def foo (fn (bar baz) (print (list mumble bar baz))))
```

And you pass it a tricky set of arguments like

```
(foo (do (set! mumble "A changed label")
         27)
     (do (set! bar "a string")
         "and another"))
```

That's going to do one of at least three things, depending on how exactly your environment model works. You might think that the problem is mutation, and simply disallow `set!`

```
(def mumble "A label")
(def foo (fn (bar baz) (list mumble bar baz)))

(foo (do (def mumble "A changed label")
         27)
     (do (def bar "a string")
         "and another"))
```

Granted, `def` is sort of a side-effect, but this'll give you similar problems. There's at least one environment implementation you could pick<a name="note-Fri-Dec-12-094240EST-2014"></a>[|2|](#foot-Fri-Dec-12-094240EST-2014) that can support a "pure" `def` operation, and I don't think it has the same sorts of problems. Under that model, `def` basically acts as a single-term `let` statement that encompasses the rest of its block, which means that you're basically trading problems with mutation for problems with defining mutually recursive functions. Open recursion and [`Y`](http://en.wikipedia.org/wiki/Fixed-point_combinator#Derivation_of_the_Y_combinator)s aren't the worst thing in the world, but I sort of don't want that to be the *only* way to program.

I've got no answers yet; I'll need to read a thing or four, build a thing or two, then let you know how it goes. And actually, the next post I do will probably be dealing with implementing a multi-line-accepting REPL in Haskell using [Pipes](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Tutorial.html) rather than extending the fundamental insights.

Stay tuned I guess?

* * *
##### Footnotes

1 - <a name="foot-Fri-Dec-12-094216EST-2014"></a>[|back|](#note-Fri-Dec-12-094216EST-2014) - It might be possible to do even better by doing a type inference pass first, actually. At that point, you'd know that in the expression `((foo 5 6) 7 8 9 10)`, the `(foo 5 6)` has to be callable, since it's being called with more arguments, so you might be able to finangle that as well. I'm just not at all sure you could get that level of insight in anything but the simplest examples.

2 - <a name="foot-Fri-Dec-12-094240EST-2014"></a>[|back|](#note-Fri-Dec-12-094240EST-2014) - The [Forth](https://github.com/chengchangwu/jonesforth/blob/master/jonesforth.S#L166-L225) one, in case you were curious.
