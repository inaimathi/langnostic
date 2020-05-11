Minor diversion into Emacs Lisp.

So, at work I'm one of the few dinosaurs who still uses [Emacs](https://www.gnu.org/software/emacs/) for basically everything. You can see evidence of this in [my machine setup routine](https://github.com/inaimathi/machine-setup). I one hundred percent recommend you do the same, especially if you have any interest in any Lisps at all.

My work _mostly_ isn't Lisp, byt Python. And one of the things I end up doing is running `emacs -nw` in a bunch of different [`virtualenv`](https://docs.python.org/3/tutorial/venv.html)-enabled directories[^this-is-mainly-because]. This has been annoying in one noticeable way; it's hard to juggle so many instances of the editor mentally, and I've found myself getting confused about which one is getting called where.

[^this-is-mainly-because]: This is mainly because, despite my attempts at doing so, I have not _yet_ read up on and practiced enough with [`guix`](https://guix.gnu.org/) to seriously recommend it as the one package manager to rule them all. I'm getting dangerously close. What I've read and seen so far has had me replacing my `nix` installation with a `guix` one even though I likely won't have much time to pour in it.

So, [`elisp`](https://www.gnu.org/software/emacs/manual/html_node/elisp/) to the rescue...

### Recoloring Emacs Backgrounds by `virtualenv` Source

What I'd like to do is make sure that my editor window is colored differently, depending on which virtual environment I come from. For the setup I'm using, it's enough to check the environment variable `VIRTUAL_ENV`. I generally use Emacs from outside any `venv`s, because most of my personal hacking still happens outside of Python, so I still want that to be a working use case.

First off, getting the name is trivial. We want to check that environment variable, and take its last path element.

```
(defun virtual-env-name ()
  (if-let venv (getenv "VIRTUAL_ENV")
    (file-name-nondirectory venv)))
```

If that var is unset, we want to return `nil`. If it isn't, we want to get its `file-name-nondirectory`. Of course, in order to express that elegantly, we need to define a utility macro.

```
(defmacro if-let (name test then &optional else)
  (let ((tmp (gensym)))
    `(let ((,tmp ,test))
       (if ,tmp
	   (let ((,name ,tmp)) ,then)
	 ,else))))
```

Or you could probably include it from somewhere. I don't know. Still fairly straight-forward.

Next up, we need to make a color out of an incoming string. Also, straightforward.

```
(defun color-of (input)
  (concat "#" (substring (secure-hash 'md5 input) 0 6)))
```

Ok, last bit. I want to set the background color of my editor to that color. But also, if the color is dark enough, I might need to reset the _foreground_ color so that things don't get hard to read. This turns out to be non-trivial, but still fairly easy. There's a stock package called `color` that lets you convert between `RGB` and `HSL` color models. It takes input as float-tuples rather than strings though, and we need to ultimately feed this color to `set-*-color` in CSS format though. There doesn't seem to be an obvious parsing function around, so that's something we need too.

Parsing first.

```
(defun hex->rgb (hex-string)
  (mapcar
   (lambda (h)
     (/ (float (string-to-number h 16)) (float 255)))
   (list
    (substring hex-string 1 3)
    (substring hex-string 3 5)
    (substring hex-string 5))))
```

I make the assumption that input is going to be in the format `#rrggbb` rather than `rrggbb` or any of the other options, and that simplifies things quite a bit here. Ok, now we're ready for the magic trick...

```
(defun set-colors-by (input)
  (when input
    (let* ((color (color-of input))
	   (lightness (third (apply #'color-rgb-to-hsl (hex->rgb (color-of input))))))
      (set-background-color color)
      (set-foreground-color
       (if (>= lightness 0.5) "white" "black"))

      (list color lightness))))
```

With that in place, and having added `(set-colors-by (virtual-env-name))` to [my `.emacs`](https://github.com/inaimathi/machine-setup/blob/master/dot-emacs), I now get

1. The default color scheme when I'm outside of a `virtualenv`
2. A different background color for each env when I launch in one
3. Readable text no matter what color my background is set to

Which is basically everything I was hoping to get out of this.
