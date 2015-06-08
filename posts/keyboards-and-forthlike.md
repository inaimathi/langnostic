So I got a [new keyboard](http://elitekeyboards.com/products.php?sub=topre_keyboards,rftenkeyless&pid=rf_se1700).

All told, it ran me something in the area of $250. Not sure I consider it worth it, but I've only really typed on it for a solid day so far, so that's probably not enough experience to make a call about it. So far, my impression is that it *is* an improvement over the Cherry MX blues I've tried, but not quite ~$150 worth of improvement. If you've got some cash burning a hole in your pocket, and a need for more comfortable typing, I can heartily recommend it to you. It's very comfy, the Topre keys feel great, there's dip-switch support for swapping the Caps Lock and left Ctrl keys<a name="note-Sat-Aug-10-172026EDT-2013"></a>[|1|](#foot-Sat-Aug-10-172026EDT-2013). It also costs quite a bit less than the [HHK2](http://elitekeyboards.com/products.php?sub=pfu_keyboards,hhkbpro2&pid=pdkb400wns), which I would totally order if someone dumped a million dollars on me tomorrow, but as it stands, $400 is out of my price-range for a keyboard.

### <a name="forthlike" href="#forthlike"></a>Forthlike

Nothing serious here, just some thinking and playing I've been doing.

At the [last meeting](http://lispwiki.inaimathi.ca/2013%20August%20Meeting), a fellow Toronto Common Lisper presented an idea of *his* for a language titled FNFAL. The FuNctional Fixed Arity Language. Its primary goal is to be extremely simple to implement, while also not being quite as alien as [Forth](http://home.iae.nl/users/mhx/sf.html) or [Scheme](http://www.call-cc.org/) to the uninitiated. Someone made a comment to the effect that Forth was extremely easy to implement, which sent me off thinking about how easy it actually is.

I pointedly *didn't* read the spec, but I have read a few forth [tutorials](http://bernd-paysan.de/httpd-en.html) and I have vigorously poked at [GForth](http://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Tutorial.html) once or twice.

Anyhow, the answer is "not very hard", assuming you start with a high-level language and don't adhere to the standard<a name="note-Sat-Aug-10-172030EDT-2013"></a>[|2|](#foot-Sat-Aug-10-172030EDT-2013). The entire exercise took me one sitting of about three hours, and ended up weighing in at just under 100 lines of Common Lisp, not including the package and system definitions.

Utility first:

```lisp
;; util.lisp
(in-package :forthlike)

(defun println (thing) (format t "~a~%" thing))

(defmacro aif (test if-true if-false)
  `(let ((it ,test))
     (if it ,if-true ,if-false)))

(defmacro bif (test) `(if ,test "true" "false"))

(defmacro with-pop! ((&rest symbols) &body body)
  `(let ,(loop for s in symbols collect `(,s (pop!)))
     ,@body))

(defun parse-num (str)
  (multiple-value-bind (int end) (parse-integer str :junk-allowed t)
    (if (and int (/= end (length str)) (eq #\. (aref str end)))
        (ignore-errors
          (multiple-value-bind (float f-end) (parse-integer str :start (+ end 1))
            (+ int (float (/ float (expt 10 (- f-end end 1)))))))
        int)))
```

`println` is a shortcut for printing something with a newline at the end, `aif` is one of [Graham's anaphors](http://dunsmor.com/lisp/onlisp/onlisp_18.html), `bif` is a function to let me deal with the target languages' booleans<a name="note-Sat-Aug-10-172038EDT-2013"></a>[|3|](#foot-Sat-Aug-10-172038EDT-2013), and `with-pop!` is a piece of utility to make it easier to work with multiple values from the stack. You'll see how that comes together in a second.

`parse-num` looks like it's the scariest thing here, but all it actually does is use `parse-integer` to parse integers *or* floats.

Moving on...

```lisp
;; forthlike
(in-package #:forthlike)

(defparameter *stack* nil)
(defparameter *words* (make-hash-table :test #'equal))
(defparameter *input* "")

(defun pull! (&optional (looking-for #\ ))
  (multiple-value-bind (word len) (split-sequence looking-for *input* :count 1)
    (setf *input* (subseq *input* len))
    (first word)))

(defun pop! () (pop *stack*))

(defun push! (thing) (push thing *stack*))

(defun ev (word)
  (if (or (string= "true" word) (string= "false" word))
      (push! word)
      (aif (parse-num word)
           (push! it)
           (aif (gethash word *words*)
                (funcall it)
                (format t "Unknown word: ~s~%" word)))))

(defmacro def (name &body body)
  `(setf (gethash ,name *words*) (lambda () ,@body)))

(def "." (println (pop!)))
(def ".s"
  (println "")
  (if *stack*
      (loop for i from 0 for elem in *stack*
         do (format t "< ~a > :: ~a~%" i elem))
      (format t "< Empty stack >~%"))
  (println ""))

(def "`" (push! (pull!)))
(def "," (funcall (gethash (pop!) *words*)))
(def "\"" (push! (format nil "~s" (pull! "\""))))

(def "dup" (push! (first *stack*)))
(def "swap" (rotatef (first *stack*) (second *stack*)))

(def "+" (push! (+ (pop!) (pop!))))
(def "*" (push! (* (pop!) (pop!))))
(def "/" (with-pop! (b) (push! (/ (pop!) b))))
(def "-" (with-pop! (b) (push! (- (pop!) b))))

(def "=" (push! (bif (equal (pop!) (pop!)))))
(def ">" (push! (bif (with-pop (b) (> (pop!) b)))))
(def "<" (push! (bif (with-pop (b) (< (pop!) b)))))
(def "not" (push! (if (string= (pop!) "false") "true" "false")))
(def "and" (push! (with-pop! (a b) (bif (and (string= "true" a) (string= "true" b))))))
(def "or" (push! (with-pop! (a b) (bif (or (string= "true" a) (string= "true" b))))))
(def "if" (if (string= (pop!) "true")
              (with-pop! (a) (pop!) (ev a))
              (progn (pop!) (ev (pop!)))))

(def ":" (let ((name (pull!))
               (words (loop for wd = (pull!) until (string= wd ";") collect wd)))
           (def name (mapc #'ev words))))

(defun forthlike-eval (str)
  (setf *input* str)
  (loop until (string= *input* "") do (ev (pull!))))

(defun forthlike-load (file-path)
  (with-open-file (s file-path)
    (loop for res = (read-line s nil :eof)
       until (eq res :eof)
         do (forthlike-eval res))))

(defun repl ()
  (loop for line = (progn (format t "~~4th >> ") (read-line)) 
     until (string= line "bye") do (forthlike-eval line)))
```

And that's all.

```lisp
(defparameter *stack* nil)
(defparameter *words* (make-hash-table :test #'equal))
(defparameter *input* "")
```

`*stack*`, `*words*` and `*input*` are storage places for, respectively, the stack, the dictionary, and the current input.

```lisp
(defun pop! () (pop *stack*))

(defun push! (thing) (push thing *stack*))

(def "dup" (push! (first *stack*)))
(def "swap" (rotatef (first *stack*) (second *stack*)))

(def "+" (push! (+ (pop!) (pop!))))
(def "*" (push! (* (pop!) (pop!))))
(def "/" (with-pop! (b) (push! (/ (pop!) b))))
(def "-" (with-pop! (b) (push! (- (pop!) b))))
```

`pop!` and `push!` are both the obvious stack operations, `def` is a utility macro to make definitions simpler, and it's immediately used to put together some primitives. The obvious ones are `+`, `*`, `/` and `-`. `.` `pop!`s one thing and prints it, while `.s` prints the entire stack. There should probably also be a way to print the current dictionary, now that I think about it. `dup` and `swap` do exactly what you'd expect, knowing their Forth equivalents.

```lisp
(def "=" (push! (bif (equal (pop!) (pop!)))))
(def ">" (push! (bif (with-pop (b) (> (pop!) b)))))
(def "<" (push! (bif (with-pop (b) (< (pop!) b)))))
(def "not" (push! (if (string= (pop!) "false") "true" "false")))
(def "and" (push! (with-pop! (a b) (bif (and (string= "true" a) (string= "true" b))))))
(def "or" (push! (with-pop! (a b) (bif (or (string= "true" a) (string= "true" b))))))
(def "if" (if (string= (pop!) "true")
              (with-pop! (a) (pop!) (ev a))
              (progn (pop!) (ev (pop!)))))
```

The boolean functions all operate on the symbols `true` and `false`, represented here as strings. Those symbols include `=`, `>`, `<`, `not`, `and`, `or` and `if`. The reason I'm not sure this is the best way to handle it is that it makes `true` and `false` pretty glaring exceptions to the basic rules of the language. Ideally, I'd have some way of designating a class of words that self-evaluate, and let the user play with them too. This is something I'm thinking about for an upcoming article, though I guess it's possible that I'm just taking it too seriously. Anyhow.

Ok, here's where this gets interesting.

```lisp
(defun pull! (&optional (looking-for #\ ))
  (multiple-value-bind (word len) (split-sequence looking-for *input* :count 1)
    (setf *input* (subseq *input* len))
    (first word)))
```

`pull!` grabs the next "word" from `*input*`. By default, that's the next space-delimited chunklet, but it's possible to change the terminating character. This turns out to be fairly useful for things like `"`, which looks for the next quote<a name="note-Sat-Aug-10-172051EDT-2013"></a>[|4|](#foot-Sat-Aug-10-172051EDT-2013).

```lisp
(def "`" (push! (pull!)))
(def "," (funcall (gethash (pop!) *words*)))
(def "\"" (push! (format nil "~s" (pull! "\""))))

...

(def ":" (let ((name (pull!))
               (words (loop for wd = (pull!) until (string= wd ";") collect wd)))
           (def name (mapc #'ev words))))
```

`pull!` is used in places where we care about upcoming input, and it lets Forthlike define `:` as merely looking for the word `;`, rather than something more complicated. It also lets me define two words that I don't think exist in traditional Forths, but that seem obvious if you have any lisp experience. ``` calls `pull!`, and pushes the result onto the stack *without* calling `ev` on it, while `,` calls `pop!` and tries to treat the result as a function to call. I was tempted to just make it call `ev`, but I'm not too sure that would be the best idea since numbers and symbols would just stay on the stack.

The evaluation conditions and evaluation time could both use a pretty thorough thinking session; would it be ok to tokenize things as they're pulled? That is, parse numbers, `intern` symbols in the `keywords` package, and intern/lookup the functions? That would mean that it would be easier to tell what to do with the thing on top of the stack; strings, keywords and numbers get left there and functions get called. Do I want to look up functions right away? Or should I keep their name on the stack? What are the implications of that? Do I want to re-think the `*words*` table entirely? For example, by making `:` create a `lambda` rather interning a function, and then letting a different function designate names in the `*words*` table? That would make it pretty easy to define variables too, which the system currently lacks.

```lisp
(defun forthlike-eval (str)
  (setf *input* str)
  (loop until (string= *input* "") do (ev (pull!))))

(defun forthlike-load (file-path)
  (with-open-file (s file-path)
    (loop for res = (read-line s nil :eof)
       until (eq res :eof)
         do (forthlike-eval res))))

(defun repl ()
  (loop for line = (progn (format t "~~4th >> ") (read-line)) 
     until (string= line "bye") do (forthlike-eval line)))
```

The last three functions are what actually let you use the language. `forthlike-eval` takes a string and evaluates it one word at a time, `forthlike-load` uses `forthlike-eval` to load named files, and `repl` uses `forthlike-eval` in conjunction with `read-line` to make a Forthlike [REPL](http://www.redbubble.com/shop/repl+t-shirts) available.

It's a pretty featureful little toy for three hours of work. I may come back to this at some point in the future.


* * *
##### Footnotes

1 - <a name="foot-Sat-Aug-10-172026EDT-2013"></a>[|back|](#note-Sat-Aug-10-172026EDT-2013) - As well as a replacement keycap for your Caps Lock, if you go that route.

2 - <a name="foot-Sat-Aug-10-172030EDT-2013"></a>[|back|](#note-Sat-Aug-10-172030EDT-2013) - Though using Lisp to implement something not unlike Forth feels a little bit like cheating.

3 - <a name="foot-Sat-Aug-10-172038EDT-2013"></a>[|back|](#note-Sat-Aug-10-172038EDT-2013) - Though I'm not convinced I took the right approach quite yet.

4 - <a name="foot-Sat-Aug-10-172051EDT-2013"></a>[|back|](#note-Sat-Aug-10-172051EDT-2013) - It doesn't take escapes into account yet, though I don't suspect that would be very difficult to add. Likewise, it would be nice if I could designate a character to nest on, so that we could grab balanced expressions. Things for the future, I'm sure.
