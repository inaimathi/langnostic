Aside from [the obvious](http://c2.com/cgi/wiki?OpenGenera), I mean.

[Here's](http://www.clisp.org/clash.html) what I've been playing with for the past little while.

Those instructions still work surprisingly well, given that they were published all of 11 years ago. Here's what I did to replicate them

```
apt-get install clisp x-window-system
wget http://beta.quicklisp.org/quicklisp.lisp
echo "/usr/bin/clisp" >> /etc/shells
```

I then installed `quicklisp` and ran `(ql:add-to-init-file)`, then manually added the following to `.clisprc`:

```lisp
(ql:quickload (list :cl-fad :cl-ppcre :trivial-shell))
(defun startx () (execute "/usr/bin/X11/xinit"))
```

and the following to my .xinitrc

`clisp -x "(progn (ql:quickload (list :clx :cl-ppcre :stumpwm)) (funcall (intern \"STUMPWM\" :stumpwm)))"`

After poking around for a little while and making sure everything worked approximately correctly, I ran `chsh` and set my shell to `/usr/bin/clisp`.

Performance-wise, it's surprisingly snappy given

a) [what it's running on](http://www.pcmag.com/article2/0,2817,1625692,00.asp)
b) that there are at least 3 instances of `clisp` at work at any given time. It's a toy, but quite a quick and fun toy, actually.

Now, granted, the title is supposed to be taken with a grain of salt[^grain-of-salt], but this still feels like it's approaching the target. What I've got running is a fully open system[^except] that implements most of its components in Lisp (the shell is Clisp, the WM is Stump and the editor is Emacs). I suppose I could also throw in [Closure](http://common-lisp.net/project/closure/)[^an-aside] and [Climacs](http://common-lisp.net/project/climacs/) as well, but I'm done playing for today.


[^grain-of-salt]: Since I've never used an [actual LISP Machine](http://en.wikipedia.org/wiki/Lisp_machine) or even the [Open](http://www.advogato.org/person/johnw/diary/12.html) Genera [System](http://collison.ie/blog/2008/04/lisp-machines). Incidentally, these links are here to remind me to look into it when I have a spare moment, so I'm not sure how much longer I'll be able to say "never used 'em".

[^except]: Except that it uses b43-fwcutter for the wireless card.

[^an-aside]: As an aside, that meme-space is getting pretty crowded. To the point that I have to disambiguate in conversation. There's [Clojure](http://clojure.org/) (the language), [Clozure](http://ccl.clozure.com/) (the Common Lisp implementation) and Closure (the common-lisp based [browser](http://common-lisp.net/project/closure/)/[html-parser](http://www.cliki.net/closure-html))
