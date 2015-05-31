---
Testing testing
...

Something I've been thinking about lately is parsing HTTP requests. Particularly because that's [the chunklet](https://github.com/Inaimathi/house/blob/937ee9404b497ca8a576d80c34e96190bf1a6b05/house.lisp#L30-L114) of [`house`](https://github.com/Inaimathi/house) that ended up evolving most over the past little while. It turns out that there's really no way around it: in order to finish *reading* a request properly, you need to parse the first piece of it. This is because certain requests with the method `POST` might contain additional bytes past the content headers. And the claimed number of bytes they send is contained in the [`Content-Length` header](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13). In other words, in order to properly read such a request from a TCP stream, you need, at minimum, to parse the request line (to check whether this is a `POST` or not) and the `Content-Length`/`Content-Type` headers, if they exist<a name="note-Fri-May-15-155703EDT-2015"></a>[|1|](#foot-Fri-May-15-155703EDT-2015). You then have enough information to know whether you should be expecting additional data past the headers, figure out the format of that data, and establish an upper bound on its size.

Being the optimist that I am, I figured I'd try doing an [incremental parse](https://github.com/Inaimathi/cl-lazy-parse). I've done some light use of [`esrap`](https://github.com/nikodemus/esrap) and [`esrap-liquid`](https://github.com/mabragor/esrap-liquid), except that the former doesn't deal with streams while the latter doesn't work with [SBCL classic](/article?name=sbcl-quasiquoting.html), which tells me it's doing some relatively complicated syntax tree traversals.

So, here's my attempt:

```lisp
;; lazy.lisp
(in-package :cl-lazy-parse)

;;;;;;;;;; Basic lazy computation stuff 
;;; (avoiding calling them delay/force, because I suspect they ultimately won't be thunks)
(defstruct paused fn)
(defmacro pause (&body body)
  `(make-paused :fn (lambda () ,@body)))
(defmethod resume ((p paused))
  (funcall (paused-fn p)))
```


```lisp
;; rapid.lisp
(in-package #:cl-lazy-parse)

;;;;;;;;;; Buffer structure
(defstruct buffer arr (end-ix 0) (read-ix 0))
(defun buffer (&key (initial-size 512))
  (make-buffer :arr (make-string initial-size)))

(defmethod more? ((b buffer)) 
  (and (> (buffer-end-ix b) (buffer-read-ix b)) (> (buffer-end-ix b) 0)))

(defmethod place! ((b buffer) (c character))
  (let ((arr (buffer-arr b)))
    (when (= (buffer-end-ix b) (length arr))
      (setf (buffer-arr b) 
            (concatenate 'string arr (make-string (length arr))))))
  (setf (aref (buffer-arr b) (buffer-end-ix b)) c)
  (incf (buffer-end-ix b))
  c)

(defmethod read! ((b buffer))
  (when (more? b)
    (let ((c (aref (buffer-arr b) (buffer-read-ix b))))
      (incf (buffer-read-ix b))
      c)))

(defmethod unread! ((b buffer) &key (count 1))
  (setf (buffer-read-ix b)
        (max 0 (- (buffer-read-ix b) count))))

(defmethod clear! ((b buffer))
  (setf (buffer-read-ix b) 0
        (buffer-end-ix b) 0))

;;;;;;;;;; Rapids are streams that don't block or char! operations, and might pause them instead
(defclass rapid ()
  ((stream-of :reader stream-of :initarg :stream-of)
   (cached :reader cached :initform (buffer) :initarg :cached)))

(defmethod rapid ((s stream) &key (buffer-size 256))
  (make-instance 'rapid :stream-of s :cached (buffer :initial-size buffer-size)))

(defmethod getc! ((r rapid))
  (let ((res (read-char-no-hang (stream-of r))))
    (if res
        (place! (cached r) res)
        (pause (getc! r)))))

;;;;;;;;;; External interface
;;; Basic calls
(defmethod char! ((r rapid))
  (unless (more? (cached r))
    (let ((res (getc! r)))
      (when (paused-p res)
        (pause (char! r)))))
  (read! (cached r)))

(defmethod unchar! ((r rapid) (c character))
  (declare (ignore c))
  (unread! (cached r))
  nil)

(defmethod unread! ((r rapid) &key (count 1))
  (unread! (cached r) :count count))
```


```lisp
;; cl-lazy-parse.lisp
(in-package #:cl-lazy-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Parsers
;;; A parser is a function that might return a result, a +fail+ or a paused state.
(defmethod run! ((r rapid) (n null)) nil)
(defmethod run! ((r rapid) (parser function))
  (funcall parser r))
(defmethod run! ((r rapid) (chr character))
  (funcall (char>> chr) r))
(defmethod run! ((r rapid) (str string))
  (funcall (apply #'and>> (map 'list #'char>> str)) r))

(defmethod run! ((s stream) parser)
  (run! (rapid s) parser))
(defmethod run! ((path pathname) parser)
  (with-open-file (s path)
    (run! s parser)))
(defmethod run! ((str string) parser)
  (with-input-from-string (s str)
    (run! s parser)))

(defparameter +fail+ (gensym "FAIL"))
(defun failed? (thing) (eq thing +fail+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Now then, basic composition
(defun and>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
If any of them fail, the entire expression fails."
  (lambda (r)
    (let ((acc nil)
          (total 0)
          (rest parsers))
      (labels ((next! () (multiple-value-call #'cont (run! r (pop rest))))
               (cont (v &optional ct)
                 (cond ((paused-p v)
                        (pause (multiple-value-call #'cont (resume v))))
                       ((failed? v)
                        (unread! r :count total)
                        +fail+)
                       (rest
                        (push v acc)
                        (incf total ct)
                        (next!))
                       (t
                        (values (reverse (cons v acc)) (+ total ct))))))
        (next!)))))

(defun or>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
Returns the first successful one.
If they all fail, the entire expression fails."
  (lambda (r)
    (let ((rest parsers))
      (labels ((next! () (multiple-value-call #'cont (run! r (pop rest))))
               (cont (v &optional ct)
                 (cond ((paused-p v)
                        (pause (multiple-value-call #'cont (resume v))))
                       ((and rest (failed? v))
                        (next!))
                       ((failed? v)
                        +fail+)
                       (t (values v ct)))))
        (next!)))))

(defun many>> (parser)
  "Takes a parser and runs it until it fails.
Returns the accumulated successes (the empty list, if there were none)."
  (lambda (r)
    (let ((acc nil)
          (total 0))
      (labels ((next! () (multiple-value-call #'cont (run! r parser)))
               (cont (v &optional ct)
                 (cond ((paused-p v)
                        (pause (multiple-value-call #'cont (resume v))))
                       ((failed? v)
                        (values (reverse acc) total))
                       (t
                        (push v acc)
                        (incf total ct)
                        (next!)))))
        (next!)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Basic transformation
(defun with (parser fn)
  (lambda (r)
    (labels ((cont (v &optional ct)
               (cond ((paused-p v)
                      (pause (multiple-value-call #'cont (resume v))))
                     ((failed? v) +fail+)
                     ((listp v)
                      (values (apply fn v) ct))
                     (t (values (funcall fn v) ct)))))
      (multiple-value-call #'cont (run! r parser)))))

(defmacro _fn ((&rest args) &body body)
  (multiple-value-bind (final-args ignored)
      (loop for a in args
         for s = (gensym "IGNORED")
         if (eq a '_) 
         collect s into res and collect s into vars
         else collect a into res
         finally (return (values res vars)))
    `(lambda ,final-args
       (declare (ignore ,@ignored))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Basic parsers
(defmethod char>> ((pred function))
  (lambda (r)
    (labels ((cont (v)
               (cond ((paused-p v) 
                      (pause (cont (resume v))))
                     ((funcall pred v)
                      (values v 1))
                     (t 
                      (unchar! r v)
                      +fail+))))
      (cont (char! r)))))

(defmethod char>> ((pred character))
  (char>> (lambda (c) (eql c pred))))
(defmethod char>> ((pred string))
  (let ((lst (coerce pred 'list)))
    (char>> (lambda (c) (member c lst)))))

(defun not-char>> (char)
  (char>> (lambda (c) (not (eql c char)))))
```

This isn't done yet. Nowhere near. The *ultimate* goal is to replace the current two-stage buffer/parse chunks of `house`, but there's a few things I still need to take care of for that to work, and of course I'd like to make sure that the result performs at least *as well as* the naive approach. Additionally, I need to be able to gracefully handle the idea of a maximum request-size/request-age/request-retries, there are a few naming issues still waiting to be worked out, I'm not sure about the shape of the intermediate parser return value, and it would be nice to build in some support for very large data streams. Like, larger than will fit in memory. I've got some thoughts on it, but lets go through the code before we get to those.

Obvious stuff out of the way first

```lisp
;; lazy.lisp
(in-package :cl-lazy-parse)

;;;;;;;;;; Basic lazy computation stuff 
;;; (avoiding calling them delay/force, because I suspect they ultimately won't be thunks)
(defstruct paused fn)
(defmacro pause (&body body)
  `(make-paused :fn (lambda () ,@body)))
(defmethod resume ((p paused))
  (funcall (paused-fn p)))
```

`lazy.lisp` has exactly three definitions. A `paused` struct to disambiguate paused functions from other functions with the same arity. At the moment, these are exactly identical to the classic `delay` and `force` constructs. As the note says, it might eventually be the case that paused functions take some arguments. Possibly a fresh `stream` to read from, for instance. If I go through enough revisions in the meantime *without* adding those options, I'll just rename these.

Next up, our "`stream`" construct.

```lisp
;; rapid.lisp
(in-package #:cl-lazy-parse)

;;;;;;;;;; Buffer structure
(defstruct buffer arr (end-ix 0) (read-ix 0))
(defun buffer (&key (initial-size 512))
  (make-buffer :arr (make-string initial-size)))

(defmethod more? ((b buffer)) 
  (and (> (buffer-end-ix b) (buffer-read-ix b)) (> (buffer-end-ix b) 0)))

(defmethod place! ((b buffer) (c character))
  (let ((arr (buffer-arr b)))
    (when (= (buffer-end-ix b) (length arr))
      (setf (buffer-arr b) 
            (concatenate 'string arr (make-string (length arr))))))
  (setf (aref (buffer-arr b) (buffer-end-ix b)) c)
  (incf (buffer-end-ix b))
  c)

(defmethod read! ((b buffer))
  (when (more? b)
    (let ((c (aref (buffer-arr b) (buffer-read-ix b))))
      (incf (buffer-read-ix b))
      c)))

(defmethod unread! ((b buffer) &key (count 1))
  (setf (buffer-read-ix b)
        (max 0 (- (buffer-read-ix b) count))))

(defmethod clear! ((b buffer))
  (setf (buffer-read-ix b) 0
        (buffer-end-ix b) 0))

;;;;;;;;;; Rapids are streams that don't block or char! operations, and might pause them instead
(defclass rapid ()
  ((stream-of :reader stream-of :initarg :stream-of)
   (cached :reader cached :initform (buffer) :initarg :cached)))

(defmethod rapid ((s stream) &key (buffer-size 256))
  (make-instance 'rapid :stream-of s :cached (buffer :initial-size buffer-size)))

(defmethod getc! ((r rapid))
  (let ((res (read-char-no-hang (stream-of r))))
    (if res
        (place! (cached r) res)
        (pause (getc! r)))))

;;;;;;;;;; External interface
;;; Basic calls
(defmethod char! ((r rapid))
  (unless (more? (cached r))
    (let ((res (getc! r)))
      (when (paused-p res)
        (pause (char! r)))))
  (read! (cached r)))

(defmethod unchar! ((r rapid) (c character))
  (declare (ignore c))
  (unread! (cached r))
  nil)

(defmethod unread! ((r rapid) &key (count 1))
  (unread! (cached r) :count count))
```

A `rapid` is a stream that doesn't block on reads. Whenever it *would* block, it returns a `paused` state. If that was all, we could just implement this as a couple of methods on top of regular `stream`s and be done with it. As you can see by the `buffer` struct, we also want to remember everything we read. This is because we sometimes want to back out of a parse. For instance, if we have the parser `(and>> #\a #\b #\c #\d)`, and we try parsing on a stream that contains "abdc", the parse fails when we hit that `d` (since we're expecting a `c` instead). But if we want to retry the parse somehow, we need to undo the parse of `a` and `b`. Consider the example

```lisp
(or>> (and>> #\a #\b #\c #\d)
      (and>> #\a #\b #\d #\c))
```

In that case, we'd expect the second option in the `or>>` to succeed, but it will only do so if we back out and pretend we never read out the leading `ab`<a name="note-Fri-May-15-155729EDT-2015"></a>[|2|](#foot-Fri-May-15-155729EDT-2015). The way I'm dealing with that at the moment is by keeping a string buffer of a `rapid`s' output, so that "backtracking" involves moving the `read` pointer of said buffer back to the point a given parser started from. This may or may not be the final implementation, but I think the interface is at least pretty close. That being `char!`, `unchar!` and `unread!`. I may add a `reset!` at some point, to zero out the `read` and `fill` indices on a `rapid`s' buffer, but that's about it.

The real meat of this implementation is in the last file.

```lisp
;; cl-lazy-parse.lisp
(in-package #:cl-lazy-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Parsers
;;; A parser is a function that might return a result, a +fail+ or a paused state.
(defmethod run! ((r rapid) (n null)) nil)
(defmethod run! ((r rapid) (parser function))
  (funcall parser r))
(defmethod run! ((r rapid) (chr character))
  (funcall (char>> chr) r))
(defmethod run! ((r rapid) (str string))
  (funcall (apply #'and>> (map 'list #'char>> str)) r))

(defmethod run! ((s stream) parser)
  (run! (rapid s) parser))
(defmethod run! ((path pathname) parser)
  (with-open-file (s path)
    (run! s parser)))
(defmethod run! ((str string) parser)
  (with-input-from-string (s str)
    (run! s parser)))

(defparameter +fail+ (gensym "FAIL"))
(defun failed? (thing) (eq thing +fail+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Now then, basic composition
(defun and>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
If any of them fail, the entire expression fails."
  (lambda (r)
    (let ((acc nil)
          (total 0)
          (rest parsers))
      (labels ((next! () (multiple-value-call #'cont (run! r (pop rest))))
               (cont (v &optional ct)
                 (cond ((paused-p v)
                        (pause (multiple-value-call #'cont (resume v))))
                       ((failed? v)
                        (unread! r :count total)
                        +fail+)
                       (rest
                        (push v acc)
                        (incf total ct)
                        (next!))
                       (t
                        (values (reverse (cons v acc)) (+ total ct))))))
        (next!)))))

(defun or>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
Returns the first successful one.
If they all fail, the entire expression fails."
  (lambda (r)
    (let ((rest parsers))
      (labels ((next! () (multiple-value-call #'cont (run! r (pop rest))))
               (cont (v &optional ct)
                 (cond ((paused-p v)
                        (pause (multiple-value-call #'cont (resume v))))
                       ((and rest (failed? v))
                        (next!))
                       ((failed? v)
                        +fail+)
                       (t (values v ct)))))
        (next!)))))

(defun many>> (parser)
  "Takes a parser and runs it until it fails.
Returns the accumulated successes (the empty list, if there were none)."
  (lambda (r)
    (let ((acc nil)
          (total 0))
      (labels ((next! () (multiple-value-call #'cont (run! r parser)))
               (cont (v &optional ct)
                 (cond ((paused-p v)
                        (pause (multiple-value-call #'cont (resume v))))
                       ((failed? v)
                        (values (reverse acc) total))
                       (t
                        (push v acc)
                        (incf total ct)
                        (next!)))))
        (next!)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Basic transformation
(defun with (parser fn)
  (lambda (r)
    (labels ((cont (v &optional ct)
               (cond ((paused-p v)
                      (pause (multiple-value-call #'cont (resume v))))
                     ((failed? v) +fail+)
                     (t (values (apply fn v) ct)))))
      (multiple-value-call #'cont (run! r parser)))))

(defmacro _fn ((&rest args) &body body)
  (multiple-value-bind (final-args ignored)
      (loop for a in args
         for s = (gensym "IGNORED")
         if (eq a '_) 
         collect s into res and collect s into vars
         else collect a into res
         finally (return (values res vars)))
    `(lambda ,final-args
       (declare (ignore ,@ignored))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Basic parsers
(defmethod char>> ((pred function))
  (lambda (r)
    (labels ((cont (v)
               (cond ((paused-p v) 
                      (pause (cont (resume v))))
                     ((funcall pred v)
                      (values v 1))
                     (t 
                      (unchar! r v)
                      +fail+))))
      (cont (char! r)))))

(defmethod char>> ((pred character))
  (char>> (lambda (c) (eql c pred))))
(defmethod char>> ((pred string))
  (let ((lst (coerce pred 'list)))
    (char>> (lambda (c) (member c lst)))))

(defun not-char>> (char)
  (char>> (lambda (c) (not (eql c char)))))
```

That set of `run!` methods at the beginning is extensive because we want to be able to accept various things in the first argument and treat them as `rapid`s, as well as various things in the second argument and treat them as `parser`s.

```lisp
...
(defmethod run! ((s stream) parser)
  (run! (rapid s) parser))
(defmethod run! ((path pathname) parser)
  (with-open-file (s path)
    (run! s parser)))
(defmethod run! ((str string) parser)
  (with-input-from-string (s str)
    (run! s parser)))
...
```

Specifically, we want to be able to do the appropriate thing when given a `rapid`, `stream`, `pathname` or `string` (And the thing we do with strings can be much more efficient than we've got at the moment. We're currently pretending that it's a stream, when we could be setting it up as the contents of a `buffer`) as a source.

```lisp
...
(defmethod run! ((r rapid) (n null)) nil)
(defmethod run! ((r rapid) (parser function))
  (funcall parser r))
(defmethod run! ((r rapid) (chr character))
  (funcall (char>> chr) r))
(defmethod run! ((r rapid) (str string))
  (funcall (apply #'and>> (map 'list #'char>> str)) r))
...
```

*And* we want to do the appropriate thing with a `function`, `character`, `string` or `nil` as the parser. `nil` is the null parser, a `character` parses itself once, a string is just shorthand for parsing every component `character` in sequence, and a `function` could be an arbitrarily complicated parser operating on the character stream.

```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Now then, basic composition
(defun and>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
If any of them fail, the entire expression fails."
  (lambda (r)
    (let ((acc nil)
          (total 0)
          (rest parsers))
      (labels ((next! () (multiple-value-call #'cont (run! r (pop rest))))
               (cont (v &optional ct)
                 (cond ((paused-p v)
                        (pause (multiple-value-call #'cont (resume v))))
                       ((failed? v)
                        (unread! r :count total)
                        +fail+)
                       (rest
                        (push v acc)
                        (incf total ct)
                        (next!))
                       (t
                        (values (reverse (cons v acc)) (+ total ct))))))
        (next!)))))

(defun or>> (&rest parsers)
  "Takes a list of parsers and matches them in sequence.
Returns the first successful one.
If they all fail, the entire expression fails."
  (lambda (r)
    (let ((rest parsers))
      (labels ((next! () (multiple-value-call #'cont (run! r (pop rest))))
               (cont (v &optional ct)
                 (cond ((paused-p v)
                        (pause (multiple-value-call #'cont (resume v))))
                       ((and rest (failed? v))
                        (next!))
                       ((failed? v)
                        +fail+)
                       (t (values v ct)))))
        (next!)))))

(defun many>> (parser)
  "Takes a parser and runs it until it fails.
Returns the accumulated successes (the empty list, if there were none)."
  (lambda (r)
    (let ((acc nil)
          (total 0))
      (labels ((next! () (multiple-value-call #'cont (run! r parser)))
               (cont (v &optional ct)
                 (cond ((paused-p v)
                        (pause (multiple-value-call #'cont (resume v))))
                       ((failed? v)
                        (values (reverse acc) total))
                       (t
                        (push v acc)
                        (incf total ct)
                        (next!)))))
        (next!)))))
```

The basic combination primitives are `and>>`, which runs a number of parsers in series, `or>>`, which runs a number of parsers and returns the result of the first successful one or failure if they all fail, and `many>>`, which runs a given parser many times until it fails then returns the accumulated successes. I could see myself maybe wanting `n>>`, `between>>` and `optionally>>`, but that's about it. This is another section of the code where I'm considering renaming stuff. Specifically, I'm thinking of taking the [esrap](https://github.com/nikodemus/esrap) approach of naming them after the appropriate `regex` modifiers<a name="note-Fri-May-15-155742EDT-2015"></a>[|3|](#foot-Fri-May-15-155742EDT-2015). We'll see how it goes.

```lisp
...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Basic parsers
(defmethod char>> ((pred function))
  (lambda (r)
    (labels ((cont (v)
               (cond ((paused-p v) 
                      (pause (cont (resume v))))
                     ((funcall pred v)
                      (values v 1))
                     (t 
                      (unchar! r v)
                      +fail+))))
      (cont (char! r)))))

(defmethod char>> ((pred character))
  (char>> (lambda (c) (eql c pred))))
(defmethod char>> ((pred string))
  (let ((lst (coerce pred 'list)))
    (char>> (lambda (c) (member c lst)))))

(defun not-char>> (char)
  (char>> (lambda (c) (not (eql c char)))))
```

In addition to our combination constructs, we need at least one primitive. I've chosen `char>>`, which takes a predicate function and returns a parser that succeeds or fails based on the given predicate. I've also defined three pieces of sugar; one to pick out a specific character, one to pick out a character from a set, and one to pick out anything *but* a specific character. Arguably, I should just define a general `not>>` combination function rather than defining `not-char>>` specifically, but I haven't had the need to negate a parser in general. Similarly, I could see defining `any>>` instead of using `(char>> (constantly t))`, but haven't seen the need yet.

The `char>>` function is also a good place to talk about what `parser`s really *are*, since it's the most basic demonstration. A parser takes a `rapid` as an argument, and returns one of three things as its result:


-   If a parser runs out of characters to read in the given `rapid`, but hasn't hit `eof`, and hasn't succeeded or failed yet, it returns **a paused state**. This means that the caller should wait until the appropriate stream has new available input before trying to resume that state<a name="note-Fri-May-15-155748EDT-2015"></a>[|4|](#foot-Fri-May-15-155748EDT-2015). This is here specifically for the case of TCP streams, which don't always have all input available at the time we begin taking characters out; the client might be adding them to the other end as we go.
-   If a parser encounters a character that completes its desired input, it returns two values: the result of the parse in the first position, and the number of characters that result consumed in the second
-   If a parser encounters a character that doesn't satisfy its next step, it returns **a failure** and reverses its work up to that point.


Those steps a bit obscured in `or>>`, `and>>` and `many>>`, because those parsers each have to do some slightly odd bookkeeping, but you can see them perfectly in the `char>>` definition.

```lisp
...
(cond ((paused-p v) 
       (pause (cont (resume v))))
      ((funcall pred v)
       (values v 1))
      (t 
       (unchar! r v)
       +fail+))))
...
```

If the underlying call to `char!` pauses, then `char>>` propagates the pause. If the given predicate is satisfied, it returns the character it just read, and `1` as the total affected. Otherwise, it unreads the character back onto its `rapid` and returns `+fail+`.

The final thing we'll need, if we want to convert incoming character streams into symbol trees, is a transformer primitive. That's the `with` thing you saw earlier.

```lisp
...
(defun with (parser fn)
  (lambda (r)
    (labels ((cont (v &optional ct)
               (cond ((paused-p v)
                      (pause (multiple-value-call #'cont (resume v))))
                     ((failed? v) +fail+)
                     ((listp v)
                      (values (apply fn v) ct))
                     (t (values (funcall fn v) ct)))))
      (multiple-value-call #'cont (run! r parser)))))
...
```

The idea here is that `with` takes a parser and a function that expects input in the shape of the parsers' output. It applies the function to the value, and propagates the total number of characters consumed. This lets us return arbitrary values from a parse, while keeping them "reversible". The accompanying piece of syntax is just something to make it easier to ignore certain arguments to a function, since `with` will require a lot of that.

```lisp
...
(defmacro _fn ((&rest args) &body body)
  (multiple-value-bind (final-args ignored)
      (loop for a in args
         for s = (gensym "IGNORED")
         if (eq a '_) 
         collect s into res and collect s into vars
         else collect a into res
         finally (return (values res vars)))
    `(lambda ,final-args
       (declare (ignore ,@ignored))
       ,@body)))
...
```

The idea is that you can the write something like

```lisp
(_fn (a _ _ d) 
  (list a d))
```

and have it mean something like

```lisp
(lambda (a _0 _1 d)
  (declare (ignore _0 _1))
  (list a d))
```

just to make the intent a bit clearer. This is entirely optional, and you should feel free to manually write out `ignore` declarations if you like; they mean exactly the same thing. Anyhow, with all of that defined, we can now write

```lisp
;;;; Predicates and utility
(defun to-string (seq)
  (coerce seq 'string))

(defun space? (c) (eql c #\space))
(defun non-space? (c) (not (space? c)))
(defun floating? (c) 
  (let ((code (char-code c)))
    (or (= code 46) (>= 57 code 48))))

(defun header-char? (c)
  (let ((code (char-code c)))
    (or (= code 45) (>= 122 code 65))))
(defun header-val-char? (c) 
  (> (char-code c) 13))

;;;; Parsers
(defparameter crlf>>
  (to-string (list #\return #\linefeed)))

(defparameter http-method>> 
  (or>> "GET" "DELETE" "POST" "PUT"))

(defparameter request-line>>
  (with (and>> http-method>> " " (many>> (char>> #'non-space?)) " HTTP/1.1" crlf>>)
        (_fn (method _ uri _ _)
          (cons (to-string method) (to-string uri)))))

(defparameter header>>
  (with (and>> (many>> (char>> #'header-char?)) ": " (many>> (char>> #'header-val-char?)) crlf>>)
        (_fn (k _ v _)
          (cons (intern (string-upcase (to-string k)) :keyword)
                (to-string v)))))

(defparameter request>>
  (with (and>> request-line>>
               (many>> header>>))
        (lambda (req headers)
          (format t "~a~%" req)
          (format t "~{   ~a~%~}" headers))))
```

and get a basic, non-blocking, stream parser for HTTP requests. We can then call `(run! [stream] request>>)` and get an incremental parse of the thing coming in. This example just prints results to demonstrate that effectful operations are allowed, and doesn't really *do* anything with the result. It also doesn't parse parameters *or* the request body, but the framework for defining those things is present, and they shouldn't take more than a few lines. If you want to take a look at the [`example.lisp`](https://github.com/Inaimathi/cl-lazy-parse/example.lisp) file, you've also got a tiny little dummy server put together that calls this parser on incoming TCP streams and prints results. If you manually send HTTP requests at a very slow speed, you'll see that the request line and individual headers get parsed incrementally, rather than waiting for the `CRLFCRLF` that designates the boundary between headers and a potential request body.

I mentioned that this wasn't done yet. Granted, the example doesn't do everything I set out to do yet, but it could fairly easily. I've mentioned the naming issues as we encountered them in the code reading. The big outstanding issue is handling request limits. A server could easily handle maximum-retries and maximum request-age with the current setup. But maximum *request-size* is also important, and that can't be handled elegantly right now. The exploit is something like opening a TCP request and sending a rapid, infinite sequence of the character `a` in the resource slot. There's currently no mechanism to cut that off, and until I make a change or two, there can't be one that goes into effect before the `request>>` parser returns a final value. Which it won't, if the attacker knows what they're doing. Anyway, my point is, I need to figure out how to resolve that before I bother with the exercise of putting together any complete parsers. At this point, I'm thinking I'll probably resolve it by making modifications to the `rapid` definition and methods. Add slots for `created`, `tries` and `char-allowance`, and make the `getc!` do some additional book-keeping and maybe throw `error`s in certain circumstances.

As always, I'll keep you posted.

* * *
##### Footnotes

1 - <a name="foot-Fri-May-15-155703EDT-2015"></a>[|back|](#note-Fri-May-15-155703EDT-2015) - And, consequently, enough of *each* header to find out whether it's either `Content-Length` or `Content-Type`.

2 - <a name="foot-Fri-May-15-155729EDT-2015"></a>[|back|](#note-Fri-May-15-155729EDT-2015) - Strictly speaking, in this case it's not true. We could simultaneously advance both branches for overlapping pieces. I'm guessing I'll eventually want to do something like that, but I'm trying to be a bit naive at this point. Of course, there is a completely alternate way of looking at the situation. Instead of thinking of parsers as "things that operate on streams and return results", we could think of them as "things that operate on characters and return parsers or results". Then we could imagine an `or` operation as something that feeds characters simultaneously to a series of parsers, and just returning the first result it finds, or a failure if all its child parsers fail. This approach involves no backtracking whatsoever, but does involve taking a completely different approach at the problem. I'm noting it here because I want to follow up on it, *but* I also want to run my current approach to completion in an effort to learn as much as possible about the complete problem before backtracking through the solution space myself. On a cursory consideration, it looks like it would also require making some of the combinator primitives stateful. I'm sort of trying to take the view that a parser, including compound parsers, are pure constructs that operate on stateful streams.

3 - <a name="foot-Fri-May-15-155742EDT-2015"></a>[|back|](#note-Fri-May-15-155742EDT-2015) - So, like `?` instead of `optionally>>`, `*` instead of `many>>` and so on.

4 - <a name="foot-Fri-May-15-155748EDT-2015"></a>[|back|](#note-Fri-May-15-155748EDT-2015) - That caller doesn't *have* to, but until more input is available or the stream terminates, a parser will always return another paused state.
