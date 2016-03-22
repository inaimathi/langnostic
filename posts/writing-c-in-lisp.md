I'm sick today, so I figured I'd write something so that I can at least retain the impression of productivity.

Lately, I've been working on a little pet project of mine which has to do with the codebase for [Elite for Emacs](http://members.fortunecity.com/salkosuo/elite-for-emacs/0.10.0/index.html) (a port of Ian Bell and David Braben's original [Elite](http://en.wikipedia.org/wiki/Elite_(video_game)) game for my favorite editor). I liked the original, and while I didn't play it in 1984 (too busy being born that year), I did enjoy it quite a bit. The Emacs version is text driven, of course, but that's not all bad. I'll come back to why in a later post, but first I want to heap some scorn on this code. It's not that I hate it particularly. It works, for the most part, and it doesn't have to be fast (performance requirements are unsurprisingly low in a single-player game in Emacs). It's just that whenever I try to read any piece of elite-for-emacs, I come away with the impression that someone sat down and carefully thought of the worst possible way to do something, then implemented it. The result is an extremely verbose transliteration of a C program into Emacs Lisp. I'm going to dissect it here so that I can learn something, and so that hopefully, if there are hardcore C programmers out there learning Lisp, they won't pull things like this again.

This didn't start out as a re-write, just so you know.

I just wanted to fix a couple of bugs with weird control characters showing up in planet descriptions, change the behavior of a couple of commands for ease of use, and remove one or two components. It won't be a rewrite in the "throw it out and start over sense", but after peeking under the hood, it looks like I'll replace very close to all of the 4205 lines of code that make up this "port" of the original (either C or Assembly) codebase before I'm satisfied. There are some mistakes. Not in the sense that they don't produce working code, but in the sense that there are much simpler, easier and more accurate ways of doing the same thing in Elisp. Here are some before shots of common idioms I've found in no particular order:

```lisp
(if foo
    (progn bar
           baz))

(defun foo (a)
  (let ()
    (progn
      (baz)
      (mumble))))

(if foo
    (progn (setq b bar))
    (progn (setq b baz)))

(defun foo ()
  (let ((a)
        (b)
        (c))
    (setq a bar)
    (setq b baz)
    (setq c mumble)
    (progn
      ...)))

(let ((a)
      (i 0)
      (b ()))
  (while (< i (length foo))
    (progn
      (setq a (car foo))
      (setq b (append b (list (mumble a))))
      (setq i (+1 i))))
  b))

```

![](/static/img/progn-progn-everywhere.jpg)

If you want to see the complete, non-elided code, check out [the project page]((elite-for-emacs-commander-fuel cmdr)). It's pretty much more of the same, with some odd byte/numeric indexing operations thrown in where they probably don't belong.

As far as I can tell, the snippets above should have been respectively

```lisp
(when foo bar baz)

(defun foo (a)
  (baz)
  (mumble))

(setq b (if foo bar baz));; I'm leaving the setq in for demonstration purposes, but the actual task could have been done functionally

(defun foo ()
  (let ((a bar)
        (b baz)
        (c mumble))
      ...))

(mapcar 'mumble foo)

```

I'm not pointing this out to be mean. This code was written way back in 2003 (I'm using an earlier version that doesn't include combat/missions/GUI/windowed interface because my purposes demand simplicity, but the later code still makes use of the above idioms from what I saw). It's possible that some of this stuff was necessary at the time because of bugs in Emacs, or the peculiarities of Elisp. Not terribly likely, but possible. Anyway, here's why I don't like the above.


- The `when` macro exists. `(when foo bar baz)` does `bar` and `baz` if `foo` is true (unless is a similar construct that does exactly what you'd expect given when). Keep in mind that in Elisp, `()`, `'()` and `nil` are "false" and anything else is "true" for boolean purposes.
- The last value in a lisp block is returned implicitly. you can use this property to chain calls on objects instead of explicitly setfing an intermediate variable then copying. This applies to `if` too, which is why you can do `(setq b (if foo bar baz))` instead of having to put setq in both branches of the conditional.
- You don't need to declare variables in lisp. If you want to establish local bindings `(let ((a 1) (b 2) ...) [body])` is the way to do it. You can also use `let*` if you want the temporary variables to refer to each other, for example `(let* ((a 1) (b (+ 3 a))) b)` would return `4`. You do need to keep the two straight in your head, because `(let ((a 1) (b (+ 3 a))) b)` would return an error (specifically, it would complain that the variable `a` is unbound). This is because `let` doesn't guarantee that its bindings will be done in the order they are presented. `let*` does this, but it's considered good style to use `let` where you can. If you need to define temporary functions, use `flet`.
- `progn` isn't necessary everywhere. Use it if you need to do multiple things in one branch of an if statement. Keep in mind that when, unless and cond have implicit progn for their blocks, so you don't need to type it out. New Lisp coders might think this is analogous to the missing curlies problem in C-like languages. I've been chewed out for doing things like

```javascript
if(foo) bar();
else baz();
```

in javascript code. The argument is always that if someone later adds mumble to the else block, but forgets to add curly braces, they'll get a fairly hard-to-find bug.

```javascript
if(foo) bar();
else mumble();
     baz();
```

In case you didn't catch it, that makes `baz()` unconditional, which is presumably not what you want. The correct way of doing it, I'm told, is

```javascript
if(foo){
    bar();
} else {
    baz();
}
```

or

```javascript
if(foo)
{
    bar();
}
else
{
    baz();
}
```

depending on who's talking. In an imperative language with optional curlies/parentheses/brackets, this matters, so I'm not arguing that you should all stop using curly braces except where explicitly required. However, the fact that Lisp is fully parenthesized almost makes this a non-issue, a functional style mitigates it further, and in any case, adding progn all over the place isn't the right way to address it.

- It's very common in Lisp to want to iterate over a sequence, do something to each member of that sequence, and return the resulting list. The name given to this oddly specific idea is "mapping". The specific function you use is called different things (`map`, `mapcar` or similar) depending on which language you're in, and there are one or two subtleties (for instance, Elisp's `mapcar` only takes a unary function and a single sequence, Scheme's `map` can only take lists and not any sequence, etc.), but odds are that if you search the docs of a given functional language for "map", you'll find a function that does the above. When you're dealing with a sequence of things, it's a mistake to use `while` with an explicit, manual counter. I'd use `mapcar` or similar, and fall back to recursion (with tail calls where applicable) for more general purposes.

There are more things I could gripe about, and my sinister purpose in toying with this code is unrevealed, and I feel like I've only started off a much larger conversation about what it actually means to learn to program in a given language, but I think I need to go lie down now. This got enough stuff off my chest that I can continue to wade through the code for a while without bitching internally.
