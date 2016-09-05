So here's how unification works.

```lisp
(defun unify (x y &optional bindings)
  (cond ((fail? bindings) +fail+)
        ((eql x y) bindings)
        ((variable? x) (unify-variable x y bindings))
        ((variable? y) (unify-variable y x bindings))
        ((and (stringp x) (stringp y))
         (if (string= x y) bindings +fail+))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))
        (t +fail+)))

(defun unify-variable (var x bindings)
  (cond ((get-binding var bindings)
         (unify (lookup-binding var bindings) x bindings))
        ((and (variable? x) (get-binding x bindings))
         (unify var (lookup-binding x bindings) bindings))
        ((occurs-check var x bindings)
         +fail+)
        (t (extend-bindings var x bindings))))
```

That's a joke.

The above is an implementation of unification in Common Lisp. It won't, by itself, tell you how unification works, or the resulting implications. It's a very mildly modified version of the unifier that [Norvig](http://norvig.com/) built in [PAIP](http://norvig.com/paip/README.html). What a lot of people don't grasp intuitively<a name="note-Tue-Jun-17-201209EDT-2014"></a>[|1|](#foot-Tue-Jun-17-201209EDT-2014) is that unifiers don't return expressions, they return environments.

```lisp
FACT-BASE> (unify '(?a :test 1) '(:blah ?b 1))
((?B . :TEST) (?A . :BLAH))
FACT-BASE>
```

This particular implementation represents environments as [association list](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node153.html), but that's not a requirement. An `environment` is a key/value structure that tells you what variables are bound to which values. The three possible outputs of `unify` are:

**Failure**, which is the unsuccessful empty environment. Here's an example:

```lisp
FACT-BASE> (unify '(?a :test 1) '(:blah ?b 2))
#:FAIL973
FACT-BASE>
```

The problem there is that `1` and `2` are different constant values, so these two terms can't be `unify`ied. This is fundamentally different from

**Success**, which is the *successful* empty environment. You get this by successfully `unify`ing terms with no variables. For example,

```lisp
FACT-BASE> (unify '(:blah :test 2) '(:blah :test 2))
NIL
FACT-BASE>
```

This particular implementation of `unify` uses the empty list (`NIL`) as the empty success. And finally, `unify` might return

**An Environment**, which is a set of bindings under which the given unification is true. Once again,

```lisp
FACT-BASE> (unify '(?a :test 1) '(:blah ?b 1))
((?B . :TEST) (?A . :BLAH))
FACT-BASE>
```

What this is saying is

If `?a` were bound to `:BLAH` and `?b` were bound to `:TEST`, these two terms would be equal

Now, by default, `unify` starts with the empty environment, but it doesn't have to.

```lisp
FACT-BASE> (unify '(?a :test 1) '(:blah ?b 1) '((?a . :FOO)))
#:FAIL973
FACT-BASE>
```

If `unify` tries to work on these same terms, but `?a` is already bound to `:foo`, it can do nothing but fail. Yes, you could unbind a particular variable, but that's skipping ahead a bit. Backtracking is dealt with at a different level than straight-up unification. In fact...

## How `for-all` Works

Here's how `for-all` works.

```lisp
(defmacro for-all (goal-term &key in collect do)
  (with-gensyms (gen res)
    `(let ((,gen ,(if (and (symbolp (first goal-term))
                           (member (->key (first goal-term))
                                   (list :quote :backq-list :backq-list*)))
                      `(make-goal ,in ,goal-term)
                      `(make-goal ,in ',goal-term))))
       (loop for ,res = (funcall ,gen)
          until (fail? ,res)
            ,(if do 'do 'collect)
            ,(if (or collect do)
                 `(apply (lambda ,(variables-in goal-term)
                           ,(or collect do))
                         (subst-bindings ,res ',(variables-in goal-term)))
                 `(subst-bindings ,res ',(variables-in goal-term)))))))
```

Hah!

This'll get funny eventually, I swear. In all seriousness, understanding this implementation of `for-all` is entirely optional to understanding how you use `unify` to query a fact-base. Here's a simple query:

```lisp
(for-all (?id :rectangle nil) :in base :do (push (list ?id :sicp-constraint nil) base))
```

This pushes a new fact into the fact-base `base` for each individual fact that `unify`ies with the pattern `(?id :rectangle nil)`. And that's more or less how you read it:

For all environments which makes this query true in the knowledge-base `base`, do `(push (list ?id :sicp-constraint nil) base)`.

Here's a more complicated query:

```lisp
(for-all (and (?id :line-segment nil)
                (?id :start ?start) (?id :end ?end)
                (?id2 :line-segment nil)
                (lisp (not (equal ?id ?id2)))
                (or (?id2 :start ?end) (?id2 :end ?start)
                    (?id2 :start ?start) (?id2 :end ?end)))
           :in base :do (push (list ?id :line-connects-to-line ?id2) base))
```

In fact, this is about the upper bound of complexity you're likely to run into in a single query. You can read the full statement more or less the same way as above, but the "this query" part is much more complicated. In this case, rather than just going through each fact in `base` and `unify`ing it, `for-all` needs to run a deeper query. The top-level `and` there means that these sub-clauses are dependent on each other. So what it does is go through `base` looking for a fact that `unify`ies with `(?id :line-segment nil)`. If it finds such a fact, lets say it looks like `(27 :line-segment nil)`, it then tries to satisfy the next `and` clause under the resulting set of bindings. That is, it tries to find another `fact` in `base` that satisfies

```lisp
(unify fact (?id :start ?start) '((?id . 27)))
```

If *that* works, it keeps going. Either until it gets to the end of its clauses, in which case it returns the presumably fairly large `environment` which satisfies all of them, or until it gets to a sub-clause that fails. If that first sub-clause fails, `for-all` is out of answers in the given `base`, and is therefore done. If it were to fail on some other clause, it backtracks. That is, if `(unify fact (?id :start ?start) '((?id . 27)))` fails above, `for-all` will go back to the first goal and start searching where it left off until it finds another satisfying fact, then try to move up the tower again.

Hopefully, that clarified more than it confused.

* * *
##### Footnotes

1 - <a name="foot-Tue-Jun-17-201209EDT-2014"></a>[|back|](#note-Tue-Jun-17-201209EDT-2014) - And don't worry if this set includes you; it included me too, until I spent three weeks or so hitting my head against it repeatedly.
