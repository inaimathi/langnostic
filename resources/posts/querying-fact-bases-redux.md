So I [put together](https://github.com/Inaimathi/fact-base/blob/b2d1dd7e9ecf0e62d8a10d30dfec7570cdfdae04/unify.lisp) that thing I talked about [last time](http://langnostic.blogspot.ca/2014/04/querying-fact-bases.html), only to discover three things.

**Firstly**, the only places I actually needed lazy operation could be handled by passing a body directly to the query macro.

**Secondly**, when I loaded the thing up, worked up a 40k fact corpus<a name="note-Sat-Apr-12-205143EDT-2014"></a>[|1|](#foot-Sat-Apr-12-205143EDT-2014) and ran

```lisp
(for-all (and (?id :user ?name) (?id :time ?time) (?id :number 62))
         :in *base* :get (list ?id ?time ?name))
```

I'd get this:

![](/static/img/slime-eval-screen.png)

You may have noticed that this *isn't* an animated gif. It hangs there for something on the order of thirty seconds, more if profiling is on, and then returns the expected result. So that won't really do. There's some interesting points I'll talk about later, that have to do with clause order and the underlying operations. But, even though this is probably the worst way to write this particular query, it should return in under a second.

**Thirdly**, that I had exactly zero use cases for `or` goals. This might change, but until then, it looks like I don't even need unification<a name="note-Sat-Apr-12-205435EDT-2014"></a>[|2|](#foot-Sat-Apr-12-205435EDT-2014).

So as a result, I sat down and took the precise opposite approach to traversal that I tried last time. Instead of trying to keep it elegant and lazy, lets make it hacky and eager. Here's our problem, once again:

```lisp
(for-all (?id :user ?name)
         :in *base* :get (list ?id ?name))
```

should basically be the same as

```lisp
(loop for (a b c) in (current *base*) when (eq b :user) collect (list a c))
```

and

```lisp
(for-all (and (?id :user ?name) (?id :time ?time) (?id :number 62)
         :in *base* :get ?time))
```

should more or less be equivalent to

```lisp
(loop for (a b c) in (current *base*)
   when (eq b :user)
   append (loop for (d e f) in (current *base*)
             when (and (eq d a) (eq e :time)))
             append (loop for (g h i) in (current *base*)
                       when (and (eq g d) (eq h :number) (= i 62))
                       collect f))
```

Except, you know, it should be smarter about using indices where it can. But that's a pretty straight-forward specification.

### `lookup` and `decide-index` changes - take 1

The first thing I had to do was change `lookup` and `decide-index` a bit, because I wanted them to be mildly less naive. And yeah, I broke down and added some macrology to pull out all the repetition in the index-related functions. Turns out that was a good thing.

```lisp
(defmacro lookup-index (state &rest indices)
  (with-gensyms (st)
    `(let ((,st ,state))
       (cond ,@(loop for i in indices
                  for syms = (key->symbols i)
                  collect `((and (indexed? (index ,st) ,i)
                                 ,@syms)
                            (list ,i ,@syms)))))))

(defmethod decide-index ((state fact-base) &optional a b c)
  (lookup-index state :abc :ab :ac :bc :a :b :c))
```

Short version is, the function now takes a `fact-base` in addition to an `a`, `b` and `c`, and checks whether a particular type of index is kept for a fact base before otherwise seeing whether it would be appropriate for the current query.

```lisp
(defmethod lookup ((state fact-base) &key a b c)
  (if (every #'not (list a b c))
      (current state)
      (let ((ix (aif (decide-index state a b c)
                     (gethash (rest it) (gethash (first it) (table (index state))))
                     (current state))))
        (loop for f in ix
           when (and (or (not a) (equal a (first f)))
                     (or (not b) (equal b (second f)))
                     (or (not c) (equal c (third f))))
           collect f))))
```

`lookup` now has to be mindful of this, and has to check that the indexed facts match the incoming query. Because we're now potentially using a more general index than the query calls for. My gut tells me this is still a net increase in performance since last time, even though our best case is now `On` with the size of the result rather than `01`. If it comes to it, I'll go back and make that more efficient.

Actually, lets fix it right now.

### `lookup` and `decide-index` changes - take 2

```lisp
(defmethod lookup ((state fact-base) &key a b c)
  (if (every #'not (list a b c))
      (current state)
      (multiple-value-bind (index ideal-index) (decide-index state a b c)
        (let ((ix (if index
                      (gethash (rest index) (gethash (first index) (table (index state))))
                      (current state))))
          (if (and index (eq (first index) ideal-index))
              ix
              (loop for f in ix
                 when (and (or (not a) (equal a (first f)))
                           (or (not b) (equal b (second f)))
                           (or (not c) (equal c (third f))))
                 collect f))))))
```

That more complicated version of lookup expects two values instead of one; which `index` we're using, and which `index` we'd ideally use. If the two are the same, we just return the results of our lookup, otherwise we have to do the narrowing traversal. That's about as efficient as it's going to get without making it lazy. Which I guess I could, but not right now. However, we also need a modified `decide-index` to pull this little trick off. And that's going to be fugly.

```lisp
(defmacro lookup-index (state &rest indices)
  (with-gensyms (ix ideal applicable?)
    `(let ((,ix (index ,state))
           (,ideal))
       ,@(loop for i in indices
            for syms = (key->symbols i)
            collect `(let ((,applicable? (and ,@syms)))
                       (when (and (null ,ideal) ,applicable?) (setf ,ideal ,i))
                       (when (and (indexed? ,ix ,i) ,applicable?)
                         (return-from decide-index
                           (values (list ,i ,@syms) ,ideal)))))
       (values nil ,ideal))))

(defmethod decide-index ((state fact-base) &optional a b c)
  (lookup-index state :abc :ab :ac :bc :a :b :c))
```

Say what you will about imperative programming; it's efficient. That's a single pass over the relevant indices that returns both the least general applicable index, and the ideal index for a given query. Which means we can now profitably compare the two in `lookup`, which means that our best case is back up to `O1`, since we don't need to traverse queries for things we've indexed.

With those modifications, I can pull some fancier crap in translating `for-all` calls into `loop`s. Specifically, I can do this:

### This

```lisp
(defun goal->destructuring-form (goal &key (bindings (make-hash-table)))
  (labels ((rec (elem)
             (cond ((listp elem)
                    (mapcar #'rec elem))
                   ((or (eq '? elem) (not (variable? elem)))
                    (gensym))
                   ((and (variable? elem) (gethash elem bindings))
                    (gensym))
                   ((variable? elem)
                    (setf (gethash elem bindings) t)
                    elem)
                   (t (error "Somethings' up. goal->destructuring-form~%     ~s~%     ~s~%     ~s"
                             bindings goal elem)))))
    (mapcar #'rec goal)))

(defun goal->lookup (base goal &key (bindings (make-hash-table)))
  (flet ((->ix (elem)
           (cond ((and (variable? elem) (gethash elem bindings))
                  elem)
                 ((any-variables? elem)
                  nil)
                 (t elem))))
    (destructuring-bind (a b c) goal
      `(lookup ,base
               :a ,(->ix a)
               :b ,(->ix b)
               :c ,(->ix c)))))

(defun goal->or-expression (a b c goal)
  (flet ((test (term elem) `(equal ,term ,elem)))
    `(and ,(test a (first goal))
          ,(test b (second goal))
          ,(test c (third goal)))))

(defmethod handle-goals ((goal-type (eql 'and)) base goals collecting)
  (let ((bindings (make-hash-table)))
    (labels ((single-goal (destruct lookup tail)
               `(loop for ,destruct in ,lookup ,@tail))
             (rec (goals)
               ;; We want to generate the lookups first,
               ;; because the bindings are going to be generated
               ;; from the result of the lookup. Meaning, if the bindings
               ;; are established in a given destruct clause,
               ;; they won't be usable until the NEXT lookup.
               ;; Therefore, even though it isn't immediately obvious,
               ;; order matters in this let* form

               (let* ((lookup (goal->lookup base (first goals) :bindings bindings))
                      (destruct (goal->destructuring-form (first goals) :bindings bindings)))
                 (if (null (cdr goals))
                     (single-goal destruct lookup `(collect ,collecting))
                     (single-goal destruct lookup `(append ,(rec (rest goals))))))))
      (rec (rest goals)))))

(defmethod handle-goals (goal-type base goals collecting)
  ;; Same story here as in handle-goals (eql 'and) method
  (let* ((bindings (make-hash-table))
         (lookup (goal->lookup base goals :bindings bindings))
         (destruct (goal->destructuring-form goals :bindings bindings)))
    `(loop for ,destruct in ,lookup collect ,collecting)))

(defmacro for-all (goal-term &key in get)
  (with-gensyms (base)
    (let ((template (replace-anonymous (or get `(list ,@(variables-in goal-term))))))
      `(let ((,base ,in))
         ,(handle-goals (first goal-term) base goal-term template)))))
```

We'll go through it in a minute, but the point of these changes is that writing

```lisp
(for-all (and (?id :user ?name) (?id :time ?time) (?id :number 62))
         :in *base* :get (list ?id ?time ?name))
```

should expand directly into something like

```lisp
(LET ((#:BASE1122 *BASE*))
  (LOOP FOR (?ID #:G1123 ?NAME)
     IN (LOOKUP #:BASE1122 :A NIL :B :USER :C NIL)
     APPEND (LOOP FOR (#:G1124 #:G1125 ?TIME)
               IN (LOOKUP #:BASE1122 :A ?ID :B :TIME :C NIL)
               APPEND (LOOP FOR (#:G1126 #:G1127 #:G1128)
                         IN (LOOKUP #:BASE1122 :A ?ID :B :NUMBER :C 62)
                         COLLECT (LIST ?ID ?TIME ?NAME)))))
```

rather than the lazy-ish generator tree from last time. Thanks to our re-structuring of `lookup`, this is about as efficient as it's going to get without re-jigging goal order. The only edge case we have is what happens if the entire goal is perfectly indexable, except it seems that the programmer would use `lookup` directly in those situations<a name="note-Sat-Apr-12-205517EDT-2014"></a>[|3|](#foot-Sat-Apr-12-205517EDT-2014).

On to the code review. Reading. Whatever.

```lisp
(defun goal->destructuring-form (goal &key (bindings (make-hash-table)))
  (labels ((rec (elem)
             (cond ((listp elem)
                    (mapcar #'rec elem))
                   ((or (eq '? elem) (not (variable? elem)))
                    (gensym))
                   ((and (variable? elem) (gethash elem bindings))
                    (gensym))
                   ((variable? elem)
                    (setf (gethash elem bindings) t)
                    elem)
                   (t (error "Somethings' up. goal->destructuring-form~%     ~s~%     ~s~%     ~s"
                             bindings goal elem)))))
    (mapcar #'rec goal)))
```

step one of the transformation is to put together the `destructuring-form` for a particular goal

```lisp
;;             this thing
;;          vvvvvvvvvvvvvvvvvvv
  (LOOP FOR (?ID #:G1123 ?NAME) IN (LOOKUP #:BASE1122 :A NIL :B :USER :C NIL)
...
```

In order to do that, we have to replace everything *other* than variables with `gensym` calls, but keep the same tree structure. `loop` does deep destructuring, so we can get away with using this as a pattern-matching strategy. We also need to replace already bound variables from previous `destructuring-form`s with the same `gensym` calls so they don't get re-assigned unnecessarily.

```lisp
(defun goal->lookup (base goal &key (bindings (make-hash-table)))
  (flet ((->ix (elem)
           (cond ((and (variable? elem) (gethash elem bindings))
                  elem)
                 ((any-variables? elem)
                  nil)
                 (t elem))))
    (destructuring-bind (a b c) goal
      `(lookup ,base
               :a ,(->ix a)
               :b ,(->ix b)
               :c ,(->ix c)))))
```

The next thing we need to put together is a given goals' `lookup` clause

```lisp
;;                                       this thing
;;                                 vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  (LOOP FOR (?ID #:G1123 ?NAME) IN (LOOKUP #:BASE1122 :A NIL :B :USER :C NIL)
...
```

We're being conservative at the moment, but there's an optimization or two I could still make here. The way we're dealing with these is:


1.   if a given goal-component is a variable, then look it up by its value in the current bindings<a name="note-Sat-Apr-12-205533EDT-2014"></a>[|4|](#foot-Sat-Apr-12-205533EDT-2014)
1.   if a given goal-component is a compound form which *contains* any variables, don't index by it<a name="note-Sat-Apr-12-205537EDT-2014"></a>[|5|](#foot-Sat-Apr-12-205537EDT-2014)
1.   otherwise, use it as an index


Onwards to `handle-goal`; the real meat of this approach. Lets take a look at how we deal with singleton goals first

```lisp
(defmethod handle-goals (goal-type base goals collecting)
  ;; Same story here as in handle-goals (eql 'and) method
  (let* ((bindings (make-hash-table))
         (lookup (goal->lookup base goals :bindings bindings))
         (destruct (goal->destructuring-form goals :bindings bindings)))
    `(loop for ,destruct in ,lookup collect ,collecting)))
```

Easy, right? grab the results of `goal->lookup` and `goal->destructuring-form` and stitch them into a `loop` along with the `collecting` clause. Nothing fancy here, except for that cryptic note about a different method definition.

```lisp
(defmethod handle-goals ((goal-type (eql 'and)) base goals collecting)
  (let ((bindings (make-hash-table)))
    (labels ((single-goal (destruct lookup tail)
               `(loop for ,destruct in ,lookup ,@tail))
             (rec (goals)
               ;; We want to generate the lookups first,
               ;; because the bindings are going to be generated
               ;; from the result of the lookup. Meaning, if the bindings
               ;; are established in a given destruct clause,
               ;; they won't be usable until the NEXT lookup.
               ;; Therefore, even though it isn't immediately obvious,
               ;; order matters in this let* form
               (let* ((lookup (goal->lookup base (first goals) :bindings bindings))
                      (destruct (goal->destructuring-form (first goals) :bindings bindings)))
                 (if (null (cdr goals))
                     (single-goal destruct lookup `(collect ,collecting))
                     (single-goal destruct lookup `(append ,(rec (rest goals))))))))
      (rec (rest goals)))))
```

And this is the full story<a name="note-Sat-Apr-12-205604EDT-2014"></a>[|6|](#foot-Sat-Apr-12-205604EDT-2014). Because of the specific way we want `lookup` and `destruct` to interact with their containing `bindings`, their order matters quite a bit. Play around with the macroexpander if you don't quite see it from just the definition.

Anyhow, the way we deal with `and` goals is by building up a chain of `loop` forms, each one dealing with a single goal while taking the previous goals into account. All but the last one need to `append` their results, while the last needs to `collect` them. The only part we've got left is the now trivial step of putting together the `for-all` macro interface to the rest of this compilation pipeline<a name="note-Sat-Apr-12-205609EDT-2014"></a>[|7|](#foot-Sat-Apr-12-205609EDT-2014).

```lisp
(defmacro for-all (goal-term &key in collecting)
  (with-gensyms (base)
    (let ((template (replace-anonymous (or get `(list ,@(variables-in goal-term))))))
      `(let ((,base ,in))
         ,(handle-goals (first goal-term) base goal-term template)))))
```

Tadaah!

I haven't yet put together an equivalent facility for the old `apply` keyword arg, but because of how we've changed up the underlying code processors, `collecting` can now trivially handle things like

```lisp
(for-all (and (?id :user ?name) (?id :time ?time) (?id :number 62))
         :in *base* :collecting (list ?name (+ ?id ?time 62)))
```

This concludes the part of this post wherein I talk about implementation details. The rest is just one or two interesting notes about traversals. If you're getting bored, or tired, this is a pretty good break-point for you.

### Traversal Notes

Near the beginning of this piece, I said

> ...this is probably the worst way to write this particular query...
> --Inaimathi

referring to

```lisp
(for-all (and (?id :user ?name) (?id :time ?time) (?id :number 62))
         :in *base* :get (list ?id ?time ?name))
```

and the reason should be fairly obvious now that we know exactly how we go about finding these answers. Remember, the expansion for this form, after compensating for the different keyword argument in our new `for-all`, is

```lisp
(LET ((#:BASE1262 *BASE*))
  (LOOP FOR (?ID #:G1263 ?NAME)
    IN (LOOKUP #:BASE1262 :A NIL :B :USER :C NIL)
    APPEND (LOOP FOR (#:G1264 #:G1265 ?TIME)
              IN (LOOKUP #:BASE1262 :A ?ID :B :TIME :C NIL)
              APPEND (LOOP FOR (#:G1266 #:G1267 #:G1268)
                        IN (LOOKUP #:BASE1262 :A ?ID :B :NUMBER :C 62)
                        COLLECT (LIST ?ID ?TIME ?NAME)))))
```

and just so that we're perfectly clear on what that means, here's the Lisp-esque pseudo-code

```lisp
(for-each goal-1
    append (for-each goal-2
               append (for-each goal-3
                          collect [some list of components])))
```

Now granted, we're aggressively using indices where we can, so we can slice a lot of the constant time out of this equation depending on how often such an operation happens, but *no matter* how efficiently we slice it, we're going to take a number of steps equal to `goal-3 * (goal-2 * goal-1)`. That is, we're going `On` over the candidates for the last goal, for each candidate of the previous goal, for each candidate of the previous goal and so on.

This is why the indices help us a lot. If we couldn't effectively discount swathes of our initial corpus, the performance characteristic would be `On^m` where `n` is the size of our fact base and `m` is the number of goals. Meaning that it behooves us to cut as many candidates as early as possible, since early reductions in our problem space will give us much better returns.

In other words, to paraphrasingly re-iterate Norvig, even though

```lisp
(for-all (and (?id :user ?name) (?id :time ?time) (?id :number 62))
         :in *base* :get (list ?id ?time ?name))
```

and

```lisp
(for-all (and (?id :number 62) (?id :time ?time) (?id :user ?name))
         :in *base* :get (list ?id ?time ?name))
```

are logically equivalent, the latter is going to perform noticeably better, because `(?id :number 62)` has a much smaller set of candidate facts than `(?id :user ?name)` in our particular corpus. One interesting exercise, which I'll leave for next time, would be to have `for-all` try to optimally sort its `and` goals by putting the smallest candidate lists at the beginning so as to reduce the search-space with no thought required from the user. The above is a trivial example; there's one goal that has more indexable terms in it than the others, so in general<a name="note-Sat-Apr-12-205638EDT-2014"></a>[|8|](#foot-Sat-Apr-12-205638EDT-2014) it will probably yield a smaller candidate list. The real way about this feels like it would be to aggressively index goals at the start of a query and sample their corpus size, then sort on that. Not sure if that would cost more than it buys me though, since it feels like that would get complex fast.

Anyway, like I said, I'll leave it for next time.

If I end up seeing performance issues in the things I'm building out of [`fact-base`](https://github.com/Inaimathi/fact-base).

And I get bored.


* * *
##### Footnotes

1 - <a name="foot-Sat-Apr-12-205143EDT-2014"></a>[|back|](#note-Sat-Apr-12-205143EDT-2014) - Like this, if you're interested:

```lisp
(defparameter *base* (make-fact-base :indices '(:a :ab :abc)))

(defmethod test-generate! (n)
  (loop repeat n
     do (multi-insert!
         *base* `((:number ,(random 100)) (:type :digit)
                  (:time ,(get-universal-time))
                  (:user ,(nth (random 7) '("Inaimathi" "Anon" "Someone Else" "Albert" "Beatrice" "Charles" "Daria")))))))

(test-generate! 10000)
```

2 - <a name="foot-Sat-Apr-12-205435EDT-2014"></a>[|back|](#note-Sat-Apr-12-205435EDT-2014) - Which makes things much simpler for this approach. Hopefully, you'll see why as we go.

3 - <a name="foot-Sat-Apr-12-205517EDT-2014"></a>[|back|](#note-Sat-Apr-12-205517EDT-2014) - and they can, since it's still an `:export`ed symbol itself.

4 - <a name="foot-Sat-Apr-12-205533EDT-2014"></a>[|back|](#note-Sat-Apr-12-205533EDT-2014) - if it has been bound by a previous `destructuring-form`, it'll be assigned by this point, which means we'll be able to index by it. Otherwise, `gethash` will return `nil`, which is exactly what we want.

5 - <a name="foot-Sat-Apr-12-205537EDT-2014"></a>[|back|](#note-Sat-Apr-12-205537EDT-2014) - This is where we could be a bit more efficient, in case you're interested. If we wanted to be very precise about it, we'd say that we *could* use a compound form with variables as an index, provided that all of its variables have been bound prior to this point in the traversal. I'm leaving it out for now because


- it would further complicate an already tricky chunk of code
- I'm not sure how often this edge case would happen in practice and
- if it *does* happen, the current result will be a slightly less efficient traversal, which doesn't sound too bad. If the consequence were incorrect results instead, I'd have reconsidered



6 - <a name="foot-Sat-Apr-12-205604EDT-2014"></a>[|back|](#note-Sat-Apr-12-205604EDT-2014) - As an aside, this is the first place I've seen in something like 8 years where a comment is appropriate. It doesn't mirror the code to which it pertains *and* it explains a non-obvious but necessary facet of the implementation. Usually, I'd either work out some naming scheme that would make the point obvious, or just factor out the chunk of code that needs explanation. There doesn't need to be a simple way of doing either here<a name="note-Sat-Apr-12-205721EDT-2014"></a>[|9|](#foot-Sat-Apr-12-205721EDT-2014).

7 - <a name="foot-Sat-Apr-12-205609EDT-2014"></a>[|back|](#note-Sat-Apr-12-205609EDT-2014) - And just to highlight this, it *is* a compilation pipeline. I mentioned this at a semi-Lisp-related meet-up lately, and it's true enough to repeat to the internets: a good way of conceptualizing a Common Lisp macro is as a compiler that takes some Lisp code and emits different Lisp code. Because of the way Lisp is structured, we get the first chunk of an actual compilation pipeline for free, and essentially start with a tokenized input. It's a pretty powerful technique once you get your head around it.

8 - <a name="foot-Sat-Apr-12-205638EDT-2014"></a>[|back|](#note-Sat-Apr-12-205638EDT-2014) - Though not necessarily in plenty of specific cases.

9 - <a name="foot-Sat-Apr-12-205721EDT-2014"></a>[|back|](#note-Sat-Apr-12-205721EDT-2014) - Though I guess I could factor that let out into a `with-for-all-forms` if it turned out I had to repeat it in many places.
