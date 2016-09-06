So the Reddit/Y Combinator spike seems to have died down, which means I can return to blissful obscurity. Not that arguing with [Jay freaking McCarthy](http://faculty.cs.byu.edu/~jay/home/) of [PLT Racket](http://racket-lang.org/) and getting to thank [Xah Lee](http://www.blogger.com/profile/11896508961236679878) for his [Emacs](http://xahlee.org/emacs/emacs.html) [tutorials](http://xahlee.org/emacs/elisp.html) wasn't the high-point of my day yesterday, but I sort of write these posts in order to get stuff out of my head rather than to have them read.

I got to the point of needing some CSS in a lisp app a little while ago, and while I was typing it up, I thought "Hang on, self, I'm sure there's a way to get this done in lisp instead of repeating yourself this much in CSS". Checking online, sure enough there's a library for it; [`css-lite`](http://www.cliki.net/css-lite), which is available through `asdf`. The `asdf` version seems to have some bugs in it, sadly.

```lisp

* (asdf-install 'css-lite)

[snip installation trace...]

* (require 'css-lite)

NIL

* (css-lite:css (("body") (:height "50px" :width "100px")))

"
body {
height, '50px', width, '100px':nil;
}"
```


That's not *exactly* what I meant.

I'm sure the [git-hub version](http://github.com/paddymul/css-lite) has this stuff resolved, but by this point I was already on the "How hard could this possibly be?" train of thought.

Inputs and outputs are strings by the `css-lite` convention, so it seems like it should be pretty simple to output. Well, it is.

```lisp

(defun css (directives)
  (apply #'concatenate
         (cons 'string
               (mapcar #'(lambda (dir) (format nil "~(~a { ~{~a: ~a; ~}}~)~%" (car dir) (cdr dir)))
                       directives))))

* (defvar test `((body :margin 5px :padding 0px :font-family sans-serif :font-size medium :text-align center)
             (\#page-box :width 1100px)
             (".box-one, .box-two" :width 200px :float left :overflow hidden :margin "0px 5px 5px 0px" :padding 0px)))

TEST

* (css test)

"body { margin: 5px; padding: 0px; font-family: sans-serif; font-size: medium; text-align: center; }
#page-box { width: 1100px; }
.box-one, .box-two { width: 200px; float: left; overflow: hidden; margin: 0px 5px 5px 0px; padding: 0px; }
"
```

Tadaah!

It could be more efficient if I used reduce instead of having `mapcar` and `concatenate` as separate steps.

```lisp

(defun css (directives)
  (flet ((format-directive (d) (format nil "~(~a { ~{~a: ~a; ~}}~)~%" (car d) (cdr d))))
    (reduce (lambda (a b)
              (let ((final-a (if (listp a) (format-directive a) a)))
                (concatenate 'string final-a  (format-directive b))))
            directives)))

* STYLE-WARNING: redefining CSS in DEFUN

CSS

* (css test)

"body { margin: 5px; padding: 0px; font-family: sans-serif; font-size: medium; text-align: center; }
#page-box { width: 1100px; }
.box-one, .box-two { width: 200px; float: left; overflow: hidden; margin: 0px 5px 5px 0px; padding: 0px; }
"

* (defvar box '(:margin "32px 10px 10px 5px" :padding 10px))

BOX

* (css `((body ,@box :font-family sans-serif :font-size medium :text-align center)))

"body { margin: 32px 10px 10px 5px; padding: 10px; font-family: sans-serif; font-size: medium; text-align: center; }
"
```

That should do it. So yeah, there's a quick and dirty non-validating CSS generator. It took about 10 minutes to write[^and-most], which is probably less time than it would take to go online, download a fix for `css-lite`, install it and try it again. I would submit it to github or something, but 6 lines of code seems somehow unworthy of its own module.

[^and-most]: And most of that was trying to figure out why it wasn't working, then realizing that I'm no longer using Scheme and that `foldl` therefore doesn't exist.

I feel this also validates my statements about the `format` function in the last post. In scheme, this css transformer would have to resort to another couple of function calls. It's that short in part because I was able to take advantage of CL's embedded formatting DSL.
