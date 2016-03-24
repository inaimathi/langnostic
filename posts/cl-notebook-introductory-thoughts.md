So it's about time I talked about [this thing](https://github.com/Inaimathi/cl-notebook), and what the hell exactly I'm thinking. Because I've been working on it for a while, and while it's still kind of buggy, I've already found myself wanting some of the features it has when working with Emacs, or other text editors I've had to work with.
Notebooks

Actually, before I get to that, a little exposition. As far as I know, notebook-style editors already exist for [Python](http://ipython.org/notebook.html), [R](https://github.com/ramnathv/rNotebook), and [Clojure](http://gorilla-repl.org/index.html). And a second one for [Clojure](https://github.com/kovasb/session). The general idea is to have a web-based interface, with code being divided into small, re-arrangable chunks called `cells`, each of which are associated with their evluation results. Some cells are code in whichever language the notebook supports, others are just prose, usually in `markdown`. The idea is that you get a dynamic environment that lets you selectively evaluate small chunklets of code, and intersperse relevant documentation in the form of prose and tests.

As someone who's been eyeing [literate programming techniques](http://literateprogramming.com/) for a while, this predictably appeals to me. So I've built my own, for my language of choice.

## `cl-notebook`

You can find it at the other end of [that github link I start out with](https://github.com/Inaimathi/cl-notebook). Last time I [mentioned this project](/posts/housekeeping) in passing, I noted that the ultimate goal was replacing Emacs as my Common Lisp IDE of choice, and that's no small task. Despite the existence of `subpar`, I don't have proper s-expression navigation yet, and I haven't wired up proper auto-completion or argument hinting yet, and there's a bunch of other stuff I still want to build, ranging from the necessary to the frivolous. On the whole, I think I'm on the right track, because certain things are somewhat easier here, and because there are some features that I find myself missing when I hop back into Emacs.

Lets just get those out of the way right now, actually. Firstly, I get to program in my browser, which is surprisingly elegant once I hop into full-screen mode. It lets me tab over to search for relevant links to talk about, and since my browser can be set to start up with previously open tabs, I get to resume editing exactly where I was in a later session. Secondly, because of the [back-end storage system I'm using](https://github.com/Inaimathi/fact-base), I get to have a running history of all the edits I've ever made, which is updated every time I evaluate a cell (I'm working on having it implicitly updated every so often between evaluations, but don't have that part checked in). Thirdly, I've got exporters wired up that let me put together a book, then export it as an HTML page, or as a `.lisp` file. And I'm planning to add two more, one to just extract tests and a second to just hand me an executable from the given book.

The first one is minor, and makes it all the easier to randomly check my email or github notifications, so pros and cons. The third could concievably be wired together in Emacs. The second one is huge. I don't know about you, but I've been programmed to hit `save` every few seconds in whatever editor I've got open just because crashes happen, and I don't want them to be too painful. I guess I could have wired up `emacs` to do that every so often, but it sounds fiddly as hell. You don't particularly want a standard editor saving every three seconds or so; you might be in the middle of an edit the currently keyed-in part of which doesn't make sense by itself, and most editors 'save' by overwriting your existing file. Which is exactly what you don't want when you've got an unfinished code edit. Hopefully, adding total-history retention to the equation softens the blow.

## Core Concepts

Code is organized into `book`s. Each `book` is the complete history of a bunch of `cell`s. A `cell` can contain `code`, `tests`, or `markup` in a particular language (currently just Common Lisp, but given how many languages I blog about, it'll probably need at least highlighting support for a few more). The `cell`s' language and type impacts the evaluation approach we take on the back end, as well as which exports it appears in, and in what form. Specifically, `common-lisp/markup` cells are evaluated as `:cl-who` forms, don't appear in `.lisp` exports and only contribute their results to an `.html` export. By contrast `common-lisp/code` is straight up evaluated (capturing warnings, errors and standard-output), contribute their contents to `.lisp` exports, and both their contents and results to `.html` exports.

In addition to a type, language and id, a cell has a `contents`, `result`, and a `noise`. The `contents` is what the user has typed in, the `result` is what that contents evaluates to and the `noise` dictates how the results are displayed. This is a `normal` cell:

```lisp
(+ 1 2)
(+ 2 3)
(format t "Testing ")
(+ 3 4)
(format t "testing")
(format t ". One two.~%")
(format t "Testing.")
(+ 4 5)
```

> ```standard-output
> Testing testing. One two.
> Testing.
> ```
>
> ```return-value
> 9 :: integer
> ```

This is the same cell with a `noise` of `verbose`:

```lisp
(+ 1 2)
(+ 2 3)
(format t "Testing ")
(+ 3 4)
(format t "testing")
(format t ". One two.~%")
(format t "Testing.")
(+ 4 5)
```

>
> ```return-value
> 3 :: integer
> ```
> ```return-value
> 5 :: integer
> ```
> ```standard-output
> Testing
> ```
> ```return-value
> NIL :: null
> ```
> ```return-value
> 7 :: integer
> ```
> ```standard-output
> testing
> ```
> ```return-value
> NIL :: null
> ```
> ```standard-output
> . One two.
> ```
> ```return-value
> NIL :: null
> ```
> ```standard-output
> Testing.
> ```
> ```return-value
> NIL :: null
> ```
> ```return-value
> 9 :: integer
> ```

And again with terse

```
(+ 1 2)
(+ 2 3)
(format t "Testing ")
(+ 3 4)
(format t "testing")
(format t ". One two.~%")
(format t "Testing.")
(+ 4 5)
```
> ```return-value
> 9 :: integer
> ```

There's also a silent setting which lets you ignore the evaluation result entirely.

You can edit a cell *(changing its contents)*, evaluate it *(changing its result)*, delete it, change any of its mentioned properties, or change the order of cells in a notebook. Each of these is an event that gets initiated by a `POST` request and gets completed with an `event-stream` message to any listening front-ends (which means I'll relatively easily be able to make this a multi-user editor when I get to that point). Enough low level stuff, here's an example.

## Example

This is a piece of code I actually wrote using `cl-notebook`.

* * *

A parameter is a thing that starts with `-`. It might be nullary or unary. A parameter followed by a parameter or an empty list is interpreted as nullary. A parameter followed by a non-parameter is unary. Command line args are more complicated in the general case, but not in cl-notebook

```lisp
(defun parse-args! (raw)
   (pop raw)
   (flet ((param? (str) (eql #\- (first-char str)))
          (->param (str) (intern (string-upcase (string-left-trim "-" str)) :keyword))
          (->arg (str) (or (parse-integer str :junk-allowed t) str)))
      (loop for next = (pop raw) while next
            if (and (param? next) (or (not raw) (param? (car raw))))
              collect (cons (->param next) t) into params
            else if (param? next)
              collect (cons (->param next) (->arg (pop raw))) into params
            else collect next into args
            finally (return (values params args)))))

(defun get-param (names params)
  (loop for (a . b) in params
     if (member a names) do (return b)))
```

> ```return-value
> GET-PARAM :: symbol
> ```

```lisp
(parse-args! '("./prog-name" "-f" "-h" "something"))
(parse-args! '("./prog-name" "a" "b" "c" "d"))
(parse-args! '("./prog-name" "-p" "4040"))

(get-param '(:f :force) (parse-args! '("./prog-name" "-f" "-h" "something")))
(get-param '(:p :port) (parse-args! '("./prog-name" "-p" "4040")))
(get-param '(:p :port) (parse-args! '("./prog-name" "--port" "4040")))
(get-param '(:p :port) (parse-args! '("./prog-name" "--frob" "4040")))
```

> ```return-value
> ((:F . T) (:H . "something")) :: cons
> NIL :: null
> ```
> ```return-value
> NIL :: null
> ("a" "b" "c" "d") :: cons
> ```
> ```return-value
> ((:P . 4040)) :: cons
> NIL :: null
> ```
> ```return-value
> T :: boolean
> ```
> ```return-value
> 4040 :: integer
> ```
> ```return-value
> 4040 :: integer
> ```
> ```return-value
> NIL :: null
> ```

It's a small utility function for parsing command line arguments in `:cl-notebook`. You can see all the relevant features on display there; it starts with some documentation prose in a `markup` cell, has definitions in a `code` cell, and finally a bunch of example invocations of each thing in a `tests` cell. They're not *really* tests, because they don't encode my assumptions about the return values of those calls, but you could imagine them doing so. The point is, they won't be part of a `.lisp` export, but *will* show up in an `.html` export like this one.

That's it for the introductory thoughts. I'll try to gather some insights into such editors into the next piece I put together. And I'll continue dogfooding until it gets good enough to call "delicious".
