This post started out as a reply to a lisper-in-training on [codereview.SE](http://codereview.stackexchange.com/). It got pretty long, so I ended up posting the final solution [there](http://codereview.stackexchange.com/questions/1227/common-lisp-solve-a-cryptoarithmetic-problem), and the story here. For those too lazy to click, the problem was given as

```
;; Cryptoarithmetic. In cryptoarithmetic problems, we are given a problem wherein the digits are replaced
;; with characters representing digits. A solution to such a problem is a set of digits that, when substituted
;; in the problem, gives a true numerical interpretation. Example:
;;   IS
;;   IT
;;   __
;;   OK
;;
;;   Has a solution { I = 1; K = 1; O = 3; S = 5; T = 6}.  For each of the below cryptoarithmetic problems,
;;   write a program that finds all the solutions in the shortest possible time.
;;
;;   IS     I
;;   IT    AM
;;   __    __
;;   OK    OK

```

The original poster put up a brute force solution, so I figured I'd follow suit (I'm sure there's a way to reduce the problem space significantly, but I've spent quite enough time on this puzzle. If you'd like to upstage me, [feel more than free](http://codereview.stackexchange.com/questions/1227/common-lisp-solve-a-cryptoarithmetic-problem/)).

* * *

Ok, so I sat down for a while (probably longer than I should have) and thought how I would actually put together a brute-force solution to this. Basically, the ideal scenario is: for the input `IS IT OK`, I would have a function to compute `(= (+ IS IT) OK)`.

```lisp
(lambda (s i |t| o k) ;; T is a reserved symbol, so I must escape the t
  (= (+ (digits->number i s) (digits->number i |t|)) (digits->number o k)))

```

If I had this function, I could simply 

```lisp
(loop for i from 0 to 99999
      when (apply [that function] (number->digits i)) 
      collect (number->bindings i '(s i |t| o k)))

```

EDIT: I was severely under-optimistic here; the **ideal** situation would be to have a function that does
```lisp
(lambda (s i |t| o k)
  (when (= (+ (digits->number i s) (digits->number i |t|)) (digits->number o k))
    (list "s" s "i" i "t" |t| "o" o "k" k)))

```

which I could then use by doing `(loop for i from 0 to 99999 when (apply [that function] (number->digits i)) collect it)` Let this be a lesson to you; always apply the maximum amount of wishful thinking (the [response over at CR](http://codereview.stackexchange.com/questions/1227/common-lisp-solve-a-cryptoarithmetic-problem/1251#1251) has `solution-fn` generate this function rather than the original).


That would give me the list of all single-digit bindings for the letters "S I T O K" that satisfy the problem. Ok, so what do I need for that? First off, 

```lisp
(defun digits->number (&rest digits) 
   (parse-integer (coerce digits 'string)))

(defun number->digits (num &optional (pad-to 5)) 
   (coerce (format nil (concatenate 'string"~" (write-to-string pad-to) ",'0d") num) 'list))

(defun numbers->bindings (num bindings)
   (let ((digits (number->digits num)))
      (mapcar (lambda (b d) `(,b . ,(parse-integer (format nil "~a" d)))) bindings digits)))

```

those helpers were mentioned, and seem simple enough (I'm defining them as naively as possible, with no eye to performance right now, so don't bother pointing out that strings are slow). The last thing I need is something that takes a problem string (like `"it is ok"`) and returns that `lambda` above. So, here's the magic trick.

```lisp
(defmacro solution-fn (problem-string)
  (let* ((terms (string->terms problem-string))
     (args (remove-duplicates (apply #'append terms))))
    `(lambda ,args
       (= (+ ,@(loop for term in (cdr terms) collect `(digits->number ,@term)))
      (digits->number ,@(car terms))))))

```

That introduces one more helper function, `string->terms`, which should return `'((|o| |k|) (|i| |t|) (|i| |s|))` given "is it ok". It actually doesn't matter what order the rest are in, as long as the "answer" ("ok" in this case) is the first element. Here's what the function looks like:

```lisp
(defun string->terms (problem-string)
  (reverse
   (mapcar (lambda (s) (mapcar (lambda (i) (intern (format nil "~a" i))) 
                   (coerce s 'list)))
       (cl-ppcre:split " " (string-downcase problem-string)))))

```

you'll have to install and include `cl-ppcre` to use `cl-ppcre:split`. I recommend [quicklisp](http://www.quicklisp.org/beta/) for your installation needs. I'm downcasing to avoid that problem with `T` being a reserved symbol. If you macroexpand `(solution-fn "is it ok")`, you get

```lisp
(LAMBDA (|o| |k| |t| |i| |s|)
  (= (+ (DIGITS->NUMBER |i| |t|) (DIGITS->NUMBER |i| |s|))
     (DIGITS->NUMBER |o| |k|)))

```

Tadaaah!

```lisp
(loop for i from 0 to 99999 ;;careful to enter these correctly
      when (apply (solution-fn "is it ok") (number->digits i)) 
      collect (numbers->bindings i '(|o| |k| |t| |i| |s|))) ;;note the re-ordered args to match output from `solution-fn`

```

Gives you the solution in reasonable time (assuming you don't want to solve for more than 5 digits).

...

That's not very satisfying though, is it. With this solution, we'd need to manually figure out the order of arguments in the final function, and we'd have to enter the correct number of `9`s for those arguments. Whenever there's an easy-to-miss detail in the code somewhere, I like to make sure it's as hard to miss as possible. The best way to make it hard to miss is to tell the program to take care of these details itself.

I object to doing things computers can do.
--Olin Shivers

```lisp
(defmacro solve-for (problem-string)
  (let* ((terms (string->terms problem-string))
     (args (remove-duplicates (apply #'append terms)))
     (nines (parse-integer (coerce (make-list (length args) :initial-element #\9) 'string))))
    `(loop for i from 0 to ,nines
       when (apply (solution-fn ,problem-string) (number->digits i))
       collect (numbers->bindings i ',args))))

```

Now, `(solve-for "it is ok")` will give you the answer you need. And it'll work for similar problems. That's not really the solution yet though; the problem asks for "shortest possible time", and so far I've been paying for simplicity with increased runtime. I don't trust myself here though; a priori reasoning about efficiency is fine for a first stab at the problem, but when I want to optimize, the profiler is my friend. After turning profiling on for my helper functions in `SLIME` and running `solve-for` for `"it is ok"`, `"its not ok"`, `"i am ok"`, `"i am not ok"` and (accidentally) `"it no ok"`, `M-x slime-profile-report` spits out...

```
seconds  |     gc     |     consed     |    calls   |  sec/call  |  name

  62.983 |      1.732 | 23,736,407,072 | 11,203,305 |   0.000006 | NUMBER->DIGITS
  14.780 |      0.092 |  3,777,455,536 | 43,600,000 |  0.0000003 | DIGITS->NUMBER
   0.037 |      0.004 |     11,936,688 |      3,305 |   0.000011 | NUMBERS->BINDINGS
   0.000 |      0.000 |         37,008 |          8 |   0.000000 | STRING->TERMS

  77.800 |      1.828 | 27,525,836,304 | 54,806,618 |            | Total

```

So the bottleneck is clearly `number->digits` by a pretty wide margin and this really shouldn't come as a shock given how I've been representing digits. Time to change that. *This* is where I'll resort to some light side-effect. When I get a big, *necessary* performance boost in return.

```lisp
(defun number->digits (num &optional (pad-to 5))
  (let ((temp num)
    (digits nil))
    (loop do (multiple-value-call 
         (lambda (rest d) (setf temp rest digits (cons d digits)))
           (floor temp 10))
          until (= pad-to (length digits)))
    digits))

```

And, of course, I need to go back and make sure that the appropriate functions are now expecting a list of integers rather than a list of chars. That ends up very slightly simplifying `numbers->bindings`

```lisp
(defun numbers->bindings (num bindings) 
  (let ((digits (number->digits num))) (mapcar (lambda (b d) `(,b . ,d)) bindings digits)))
</code>
```

and complicating `digits->number`

```lisp
(defun digits->number (&rest digits)
  (apply #'+ (loop for d in (reverse digits) for i from 0
           collect (* d (expt 10 i)))))

```

Lets see. After `solve` ing `for` `"it is ok"`, `"its not ok"`, `"i am ok"`, `"i am not ok"` and (not accidentally this time) `"it no ok"` again:

```
seconds  |     gc     |     consed    |    calls   |  sec/call  |  name

  10.993 |      0.752 | 7,199,265,488 | 43,900,000 |  0.0000003 | DIGITS->NUMBER
   5.009 |      0.224 | 1,069,467,616 | 11,304,260 |  0.0000004 | NUMBER->DIGITS
  0.0003 |      0.000 |       634,880 |      4,260 |  0.0000001 | NUMBERS->BINDINGS
   0.000 |      0.000 |        48,224 |         10 |   0.000000 | STRING->TERMS

  16.002 |      0.976 | 8,269,416,208 | 55,208,530 |            | Total

```

Which is a pretty drastic improvement to `number->digits`. `digits->number` is doing worse now though. I'll try out something else. Intuitively, this should be worse, but you never know 'till you try a few million times.

```lisp
(defun digits->number (&rest digits) (parse-integer (format nil "~{~a~}" digits)))

```

```
seconds  |     gc     |     consed     |    calls   |  sec/call  |  name 

  61.560 |      2.489 | 27,440,408,672 | 43,900,000 |   0.000001 | DIGITS->NUMBER
   3.973 |      0.004 |         96,832 | 11,304,260 |  0.0000004 | NUMBER->DIGITS
   0.004 |      0.000 |        593,920 |      4,260 |   0.000001 | NUMBERS->BINDINGS
   0.000 |      0.000 |         47,232 |         10 |   0.000000 | STRING->TERMS

  65.537 |      2.493 | 27,441,146,656 | 55,208,530 |            | Total

```

Ok, looks like I didn't need the profiler to figure that one out; intuition is sometimes right. I'm reverting to the numeric version, and the only other things I'll test is inlining `solution-fn` and changing a `reverse` out for `nreverse` in `digits->number`. It doesn't seem like these will make a *huge* difference, but it should improve the memory situation by a bit (and `digits->number` is hurting on conses). 

```
seconds  |     gc     |     consed    |    calls   |  sec/call  |  name  

   7.627 |      0.400 | 4,363,098,528 | 43,900,000 |  0.0000002 | DIGITS->NUMBER
   4.388 |      0.160 |   776,123,744 | 11,304,260 |  0.0000004 | NUMBER->DIGITS
   0.004 |      0.000 |       876,544 |      4,260 |   0.000001 | NUMBERS->BINDINGS
   0.000 |      0.000 |        21,600 |          5 |   0.000000 | STRING->TERMS

  12.019 |      0.560 | 5,140,120,416 | 55,208,525 |            | Total

```

Not bad. 3 fewer seconds of runtime in exchange for a single lambda inlining and one additional character in a function name. It's actually more drastic, because upon inspection, about 90% of that boost comes from `nreverse`, so it's not really even worth the in-lining (which costs a lot of readability). At this point, 

```lisp
(progn (solve-for "it is ok") 
       (solve-for "its not ok") 
       (solve-for "i am ok") 
       (solve-for "i am not ok") 
       (solve-for "it no ok"))

```

runs in just under 15 seconds with profiling off (and about a minute with the whole package profiled), which is pretty good by my estimation. If you *really* wanted to tune performance 

- Firstly, `digits->number` can do some of its calculations at macroexpansion time (since by that point, we know what number of digits we're expecting in each term, we could create a custom function with the specific arity we need instead of using a `&rest` argument).

- Secondly, `numbers->bindings` can be folded into `solution-fn` (so that it generates a function that returns either a set of bindings, or `nil`, instead of `t` or `nil`). That should save another couple of seconds.

- Thirdly you could spend some time figuring out an algorithmic solution

- Finally you could parallelize this operation. Remember, we're just applying a function to each element of the list of integers between 0 and 99999... Since there are no sequential data dependencies, there's no reason it couldn't be broken up into equal pieces across a server cluster if you wanted to solve "the quick brown fox jumps over the lazy dog". 

I think I'll cut it here though. I've already spent about two hours on this article, and that feels like it's more than enough.

By the by, I'm experimenting with a markdown converter this time (instead of using my usual blog-mode). Some assembly has been required in a couple of places (mainly that paste of the problem, and the profiler reports), but I might try again next time, with this in mind. I'm finding the lack of code highlighting and proper formatting annoying, frankly, so I'm not sure if I'll be giving it another whirl.
