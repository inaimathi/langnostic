Extremely short post. Seriously, not so much as a self-referential footnote this time.

I've been hard at work on [`deal`](https://github.com/Inaimathi/deal), when it occurred to me that I've written at least some of that stuff before. Pretty much every web development project I've started in Common Lisp in the past few years has had certain pieces of low-level helper code baked in. And I got sick of it.

So, [here's `cl-web-dev`](https://github.com/Inaimathi/cl-web-dev) a small collection of functions and macros to make it marginally more pleasant to deal with `hunchentoot`, `cl-who` and `parenscript`. The bare-bones "Hello World!" for it is

```lisp
(ql:quickload :cl-web-dev)
(defpackage :your-package (:use :cl :cl-web-dev :parenscript))
(in-package :your-package)

(define-handler test ()
  (html-str
    (:html
      (:body
       (:h1 "Hello World!")
       (:p "From" (:code "cl-web-dev"))
       (:script (str (ps (alert "And also, parenscript"))))))))

(defparameter server (easy-start 4242))
```

Which starts a server on local port `4242`, and sets up that handler at `/test`. I could probably hack a pretty big chunk out of `deal`, but I think I'll wait on that until after [the contest](http://lispinsummerprojects.org/) has ended.
