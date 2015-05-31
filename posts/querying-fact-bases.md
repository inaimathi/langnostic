So it seems that a lot of people are into this logic programming thing I've been reading about lately. There's the already mentioned Reasoned Schemer trio of Friedman/Byrd/Kiselyov behind the beautiful but arcane [miniKanren language](http://minikanren.org/), a prolog-like contained in Peter Norvig's [Paradigms of Artificial Intelligence Programming](http://norvig.com/paip.html) chapters 11, 12 and 14, another one in Graham's [On Lisp](http://www.paulgraham.com/onlisp.html) chapters 19, 22, 23, 24 and yet another in [chapter 4.4](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-29.html#%_sec_4.4) of [Abelson and Sussman's SICP](http://mitpress.mit.edu/sicp/full-text/book/book.html). So there's a lot of literature around dealing with how you go about building a unifier or pattern-matcher<a name="note-Thu-Apr-10-000043EDT-2014"></a>[|1|](#foot-Thu-Apr-10-000043EDT-2014).

Anyway, I've been consuming this literature for a while, and the part I want to zoom in on is searching the database. The other stuff is easy; a unifier can be straight-forwardly built in about ten lines<a name="note-Thu-Apr-10-000046EDT-2014"></a>[|2|](#foot-Thu-Apr-10-000046EDT-2014) and handling variables is the same tree-traversal stuff you've seen a hundred times before, but the actual search never seems to be the focus of these things. And I'm looking for a particular type of search for [`fact-base`](https://github.com/Inaimathi/fact-base). I showed it off recently at a [Toronto Lisp Group](http://lispwiki.inaimathi.ca/) meeting along with the app it's supposed to enable and mentioned that querying is mildly annoying when you get to compound queries. Specifically, I took as an example the very simple database

```lisp
(defparameter base (make-fact-base))
(insert! base (list 0 :message "This is a sample message"))
(insert! base (list 1 :message "This is another one"))
(insert! base (list 1 :author "Inaimathi"))
(insert! base (list 2 :message "That second one was written by me. This one is a meta-message (also by me)."))
(insert! base (list 2 :author "Inaimathi"))
(insert! base (list 2 :type :meta))
```

and pointed out that in order to find `All the message bodies authored by Inaimathi`, you'd have to take two steps


1.   traverse the database looking for facts that have `'(:author "Inaimathi")` for a `rest`
1.   get all the facts that have the same `first` as one of the above and the `second` `:message`, and collect their `third`s


In actual lisp, that looks like

```lisp
(loop for (a b c) in (lookup base :b :author :c "Inaimathi") 
      collect (caddar (lookup base :a a :b :message)))
```

That's even passably fast, thanks to [our `index` system](http://langnostic.blogspot.ca/2014/03/fact-base-indices.html), but it's annoying to write, and it forces me to do a `fact-base->objects` conversion in some places rather than write out these multi-stage iterations myself. What I'd like to be able to do in the above is something like

```lisp
(for-all (and (?id :author "Inaimathi") (?id :message ?message)) :in my-fact-base :get ?message)
```

and have the system figure it out for me. Granted in this situation, you don't gain very much, but it would be a compounding gain for more complex queries. For instance, if I suddenly decided I want to select `All the message bodies authored by Inaimathi pertaining to other messages`, the query language version handles it very simply:

```lisp
(for-all (and (?id :author "Inaimathi") (?id :message ?message) (?id :type :meta)) :in my-fact-base :get ?message)
```

whereas the manual version would add another level of iteration I'd need to work through. Oh, and have fun with the situation where you only want the first 5 or so hits. The easiest solution with the manual approach is searching the entire space and throwing away all but the first `n` results. You *could* do better, but you're suddenly in the *supremely* annoying situation where your queries all look mildly different, but perform the same basic task.

What I figure I'd want is a lazy or lazy-ish way of getting the results. The lazy solution can easily be converted to the eager solution later, but it's really painful to take the eager approach and *then* find out that you only needed to do about 4% of the work done. I'll be using [generators](http://en.wikipedia.org/wiki/Generator_%28computer_programming%29), rather than outright lazy sequences just because they're mildly easier to put together. For a single goal, that's trivial.

```lisp
(for-all (?id :author "Inaimathi") :in my-fact-base)
```

All you have to do here is have a generator that runs over the facts in `my-fact-base` and returns the next matching one it finds. Something like

```lisp
(defun match-single (goal bindings facts)
  (let ((fs facts))
    (lambda ()
      (loop for res = (unify goal (pop fs) bindings)
         unless (fail? res) do (return res)
         while fs
         finally (return (fail))))))
```

would do fine. I'm pointedly refusing to commit to an implementation of `(fail)`, `unify` and `bindings` at each point for the purposes of this post, but am using the stuff out of [Norvig's PAIP source code](http://norvig.com/paip/README.html). For the uninitiated: A `goal` is the thing you're trying to match; it's an expression that may contain some `variable`s. A `variable` is a thing that you can substitute; it can either be unbound or assigned a value in a particular set of `bindings`. If a `unification` fails, it returns `(fail)`, and if it's successful it returns the set of `bindings` that would make that unification expression true. For instance, if you unified `?a` with `5`, starting with empty bindings, `unify` would return the set of `bindings` in which `?a` is bound to `5`.

So the above `match-single` definition would return a generator which, when called, would either `(fail)`, *or* return the environment resulting from `unify`ing the next element of `facts` with `goal`. Hopefully, straight-forward, though you may need to do a bit of reading up on it if you've never seen the terms before.

The next easiest thing to do would be handling a set of `or`ed goals. That is

```lisp
(for-all (or (?id :author "Aaron") (?id :author "Bradley")) :in my-fact-base)
```

It's basically the same thing, except that instead of applying a single goal and checking if it sticks, we're applying several goals in sequence and seeing if any of them stick. Something like

```lisp
(defun match-ors (goals bindings facts)
  (let ((fs facts))
    (flet ((try-goals (f)
             (loop for g in goals 
                when (unify g f bindings) (return it)
                finally (return (fail)))))
      (lambda ()
        (loop for res = (try-goals (pop fs))
           unless (fail? res) do (return res)
           while fs
           finally (return (fail)))))))
```

which is by my estimation only marginally more complicated. The tricky part is traversing a fact-base in order to satisfy `and`ed goals. Like in that example I mentioned near the beginning:

```lisp
(for-all (and (?id :author "Inaimathi") (?id :message ?message) (?id :type :meta)) :in my-fact-base :get ?message)
```

Think about it.

What you want here is fairly complicated to express in English. I'm still trying to return a generator from the whole thing, but expressing its behavior is a complex.

If you only get one goal, you want to fall through to a call to `match-single`; that's still fairly straight-forward. The magic happens at more than one goal. And I just deleted about four paragraphs of prose that would have thoroughly confused you. It's not a very easy set of concepts to express in English because it refers to pieces of itself fairly often.

Lets try it this way:

```lisp
(defun match-ands (goals bindings facts)
  (let ((generator (match-single (first goals) bindings facts))
        (rest-generator))
    (if (null (cdr goals))
        generator
        (labels ((next-gen ()
                   (let ((res (funcall generator)))
                     (if (fail? res)
                         (fail)
                         (setf rest-generator (match-ands (rest goals) res facts)))))
                 (backtrack! ()
                   (if (fail? (next-gen))
                       (fail)
                       (next)))
                 (next ()
                   (if (null rest-generator)
                       (backtrack!)
                       (let ((res (funcall rest-generator)))
                         (if (fail? res)
                             (backtrack!)
                             res)))))
          #'next))))
```

Now, chunk by chunk

```lisp
(defun match-ands (goals bindings facts)
  (let ((generator (match-single (first goals) bindings facts))
        (rest-generator))
    (if (null (cdr goals))
        generator
        ...
```

By the time we're calling this function, I assume that it'll be handed at least one goal. You always want the generator of your first goal, and if you only get the one goal, you just return said generator and you're done. Multiple goals are where you need to pull fancy footwork. Again, one chunk at a time:

```lisp
        ...
        (labels ((next-gen ()
                   (let ((res (funcall generator)))
                     (if (fail? res)
                         (fail)
                         (setf rest-generator (match-ands (rest goals) res facts)))))
                 ...
```

This is where we set the `rest-generator` from earlier. It's just the procedure that will return the next result from proving the rest of the goals given the set of `bindings` built from proving the first goal into the starting set of `bindings` given to `match-ands` initially. If calling the first goals' generator fails, we likewise fail; otherwise we set `rest-generator` to the generator we create by passing the result back up to `match-ands`.

```lisp
                 ...
                 (backtrack! ()
                   (if (fail? (next-gen))
                       (fail)
                       (next)))
                 ...
```

Occasionally, we have to backtrack. Which in this context means we try to call `next-gen`. If that fails, we likewise fail, otherwise we invoke `next`. Which...

```lisp
                 ...
                 (next ()
                   (if (null rest-generator)
                       (backtrack!)
                       (let ((res (funcall rest-generator)))
                         (if (fail? res)
                             (backtrack!)
                             res)))))
                       ...
```

...sets up an initial `rest-generator` if there isn't one, then tries to call it. If that fails, we `backtrack!`<a name="note-Thu-Apr-10-000130EDT-2014"></a>[|3|](#foot-Thu-Apr-10-000130EDT-2014), otherwise we return the result.

```lisp
          ...
          #'next))))
```

That `next` function I just described is the generator we want for a multi-goal proof, which means that it's the final return value from `match-ands`.

The only required piece of infrastructure left is `for-all` itself. We want it to be able to provide results, or do something with them lazily. Which means it'll look something like

```lisp
(defmacro for-all (goal-term &key in get apply)
  (assert in nil "Need a database to query...")
  (when (and get apply)
    (format t ":apply and :get arguments passed in; ignoring :get"))
  (with-gensyms (template gen res facts)
    `(let* ((,facts ,in)
            (,gen ,(cond ((eq 'and (car goal-term))
                          `(match-ands ',(replace-anonymous (rest goal-term)) +succeed+ ,facts))
                         ((eq 'or (car goal-term))
                          `(match-ors ',(replace-anonymous (rest goal-term)) +succeed+ ,facts))
                         (t
                          `(match-single ',goal-term +succeed+ ,facts))))
            ,@(unless apply
              `((,template ',(replace-anonymous (or get goal-term))))))
       (loop for ,res = (funcall ,gen)
          while ,res collect ,(if apply
                                  `(apply (lambda ,(variables-in apply) ,apply)
                                          (subst-bindings ,res ',(variables-in apply)))
                                  `(subst-bindings ,res ,template))))))
```

Which isn't nearly as complicated as it seems at first glance. Lets go through that too.

```lisp
(defmacro for-all (goal-term &key in get apply)
  (assert in nil "Need a database to query...")
  (when (and get apply)
    (format t ":apply and :get arguments passed in; ignoring :get"))
    ...
```

arguments, and trying to be helpful with invocation errors. We want the thing to be *readable* too, which is why I use "mandatory" keyword arguments in this one.

```lisp
  ...
  (with-gensyms (template gen res facts)
    `(let* ((,facts ,in)
            (,gen ,(cond ((eq 'and (car goal-term))
                          `(match-ands ',(replace-anonymous (rest goal-term)) +succeed+ ,facts))
                         ((eq 'or (car goal-term))
                          `(match-ors ',(replace-anonymous (rest goal-term)) +succeed+ ,facts))
                         (t
                          `(match-single ',goal-term +succeed+ ,facts))))
   ...
```

we're setting up some name sanitation for certain words we'd like to use in the definition that should still be usable by the callers of `for-all`. Note the use of `replace-anonymous`, the definition can be found in Norvig's prolog implementation. The entirety of that `cond` decides which of our matchers we're going to use to traverse our corpus.

```lisp
            ...
            ,@(unless apply
              `((,template ',(replace-anonymous (or get goal-term))))))
            ...
```

If we get passed the `apply` argument, we'll be doing something special later. Otherwise, we'll want to slot our results into the template in `gen`, and failing *that*, just slot it back into the querying goal form.

```lisp
       ...
       (loop for ,res = (funcall ,gen)
          while ,res collect ,(if apply
                                  `(apply (lambda ,(variables-in apply) ,apply)
                                          (subst-bindings ,res ',(variables-in apply)))
                                  `(subst-bindings ,res ,template))))))
```

And that's the meat of it. We're going to be grabbing results out of our generator. As you can see, the special thing we're doing with the `apply` argument is stitching up a function to `apply` to a substituted list of our results. If we didn't get an `apply`, we're just slotting said result back into the `template` we defined earlier. I find that seeing some macroexpansions really helps understanding at this stage. So, here are the basics:

**Plain single-goal:**
```lisp
CL-USER> (macroexpand '(for-all (?id :author "Inaimathi") :in my-fact-base))
(LET* ((#:FACTS1073 MY-FACT-BASE)
       (#:GEN1071
        (MATCH-SINGLE '(?ID :AUTHOR "Inaimathi") +SUCCEED+ #:FACTS1073))
       (#:TEMPLATE1070 '(?ID :AUTHOR "Inaimathi")))
  (LOOP FOR #:RES1072 = (FUNCALL #:GEN1071)
        WHILE #:RES1072
        COLLECT (SUBST-BINDINGS #:RES1072 #:TEMPLATE1070)))
T
```

**`or`-goals:**
```lisp
CL-USER> (macroexpand '(for-all (or (?id :author "Inaimathi") (?id :message ?message)) :in my-fact-base))
(LET* ((#:FACTS1077 MY-FACT-BASE)
       (#:GEN1075
        (MATCH-ORS '((?ID :AUTHOR "Inaimathi") (?ID :MESSAGE ?MESSAGE))
                   +SUCCEED+ #:FACTS1077))
       (#:TEMPLATE1074 '(OR (?ID :AUTHOR "Inaimathi") (?ID :MESSAGE ?MESSAGE))))
  (LOOP FOR #:RES1076 = (FUNCALL #:GEN1075)
        WHILE #:RES1076
        COLLECT (SUBST-BINDINGS #:RES1076 #:TEMPLATE1074)))
T
```

**`and`-goals:**
```lisp
CL-USER> (macroexpand '(for-all (and (?id :author "Inaimathi") (?id :message ?message)) :in my-fact-base))
(LET* ((#:FACTS1081 MY-FACT-BASE)
       (#:GEN1079
        (MATCH-ANDS '((?ID :AUTHOR "Inaimathi") (?ID :MESSAGE ?MESSAGE))
                    +SUCCEED+ #:FACTS1081))
       (#:TEMPLATE1078
        '(AND (?ID :AUTHOR "Inaimathi") (?ID :MESSAGE ?MESSAGE))))
  (LOOP FOR #:RES1080 = (FUNCALL #:GEN1079)
        WHILE #:RES1080
        COLLECT (SUBST-BINDINGS #:RES1080 #:TEMPLATE1078)))
T
```

**Using the `:get` option**
```lisp
CL-USER> (macroexpand '(for-all (and (?id :author "Inaimathi") (?id :message ?message)) :in my-fact-base :get ?message))
(LET* ((#:FACTS1085 MY-FACT-BASE)
       (#:GEN1083
        (MATCH-ANDS '((?ID :AUTHOR "Inaimathi") (?ID :MESSAGE ?MESSAGE))
                    +SUCCEED+ #:FACTS1085))
       (#:TEMPLATE1082 '?MESSAGE))
  (LOOP FOR #:RES1084 = (FUNCALL #:GEN1083)
        WHILE #:RES1084
        COLLECT (SUBST-BINDINGS #:RES1084 #:TEMPLATE1082)))
T
```

**Using the `:apply` option**
```lisp
CL-USER> (macroexpand '(for-all (and (?id :author "Inaimathi") (?id :message ?message)) :in my-fact-base :apply (format t "~s~%   -Inaimathi" ?message)))
(LET* ((#:FACTS1089 MY-FACT-BASE)
       (#:GEN1087
        (MATCH-ANDS '((?ID :AUTHOR "Inaimathi") (?ID :MESSAGE ?MESSAGE))
                    +SUCCEED+ #:FACTS1089)))
  (LOOP FOR #:RES1088 = (FUNCALL #:GEN1087)
        WHILE #:RES1088
        COLLECT (APPLY
                 (LAMBDA (?MESSAGE) (FORMAT T "~s~%   -Inaimathi" ?MESSAGE))
                 (SUBST-BINDINGS #:RES1088 '(?MESSAGE)))))
T
CL-USER> 
```

And that's that. Granted, the *implementation* is a bit more complicated than just writing manual loops, but I'm convinced there are a couple wins here. Firstly, the invocation is simpler, which means that the above definitions will eventually "pay for themselves" in terms of complexity. Secondly, it seems like I could fairly easily mod this into [`parenscript`](http://common-lisp.net/project/parenscript/)-friendly forms, which means this'll save me from having to convert fact-bases to object lists on the client side. But that's something I'll tell you about next time.

* * *
##### Footnotes
1 - <a name="foot-Thu-Apr-10-000043EDT-2014"></a>[|back|](#note-Thu-Apr-10-000043EDT-2014) - Almost always using the question-mark-prefix notation for logic variables for some reason. I'm not sure what the approach gains or loses you yet. I guess in the case of miniKanren, it gains you the ability to unify on vectors since there's no ambiguity, and it might make it easier to read the resulting programs, but I'm not banking on that.

2 - <a name="foot-Thu-Apr-10-000046EDT-2014"></a>[|back|](#note-Thu-Apr-10-000046EDT-2014) - Though do go over Norvig's version to see a dissection of the common bugs.

3 - <a name="foot-Thu-Apr-10-000130EDT-2014"></a>[|back|](#note-Thu-Apr-10-000130EDT-2014) - And remember, `backtrack!` itself fails if it runs out of search space.
