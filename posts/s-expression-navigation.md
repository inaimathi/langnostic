This week's been going well, in case you [hadn't noticed](https://github.com/Inaimathi/cl-notebook/commits?author=Inaimathi). I got a bigger chunk of time to devote to [`cl-notebook`](https://github.com/Inaimathi/cl-notebook) than I thought I would, and the result has been a mostly-working s-expression navigation system that looks like this

```lisp
(defun go-sexp (direction mirror)
  (destructuring-bind (paren til)
      (case direction
        (:right (list "(" #'at-end?))
        (:left (list ")" #'at-beginning?)))
    (let ((ls (lines (mirror-contents mirror)))
          (other-paren (matching-brace paren)))
      (skip-whitespace direction mirror :ls ls)
      (cond ((and (string-at-cursor? direction mirror) (not (char-at-cursor? direction mirror "\"" :ls ls)))
             (skip-to direction mirror (list " " "\"" undefined) :ls ls))
            ((string-at-cursor? direction mirror)
             (skip-until 
              (lambda (c) (not (string-at-cursor? direction mirror)))
              mirror direction :ls ls))
            ((token-type-at-cursor? direction mirror :comment)
             (skip-to direction mirror (list " " undefined) :ls ls))
            ((and (bracket-at-cursor? direction mirror)
                  (char-at-cursor? direction mirror other-paren :ls ls))
             (go-char direction mirror))
            ((and (bracket-at-cursor? direction mirror)
                  (char-at-cursor? direction mirror paren :ls ls))
             (loop with tally = 1 until (til mirror :ls ls)
                do (go-char direction mirror)
                when (and (char-at-cursor? direction mirror paren :ls ls) 
                          (not (string-at-cursor? direction mirror))) 
                do (incf tally)
                when (and (char-at-cursor? direction mirror other-paren :ls ls)
                          (not (string-at-cursor? direction mirror)))
                do (decf tally)
                until (and (char-at-cursor? direction mirror other-paren :ls ls) (= 0 tally)))
             (go-char direction mirror))
            (t 
             (skip-to direction mirror (+ " " other-paren) :ls ls))))))
```

It's "mostly working" as opposed to "working" because I haven't figured out how to make it play nice with the `CodeMirror` selection system. That, along with the `slurp`/`barf`-s-exp combo is the next thing on my plate<a name="note-Thu-Aug-14-173707EDT-2014"></a>[|1|](#foot-Thu-Aug-14-173707EDT-2014). Not sure if I'll get the time I need to, but I've been fucking lucky so far, so I may as well push it a bit.

The s-exp stuff up at [the github repo](https://github.com/Inaimathi/cl-notebook/blob/master/front-end.lisp#L619-L788) was odd to get running. One thing I thought would be annoying turned out to be quite helpful, and several things I thought would be trivial turned out to be absent entirely from the `CodeMirror` model. For starters, that `go-sexp` function above is fucking atrocious, if you think about it, because we're dealing with `s-expressions`. And I sort of expected that any parse sufficient for generating highlighting and indentation data would *also* be sufficient for figuring out how to jump around the syntax tree with a minimum of fuss. The problem is that


-   `CodeMirror`s parser "`stream`" [pointedly and intentionally doesn't tell you how far along it is](https://groups.google.com/forum/#!topic/codemirror/Uv41kb0GII8)
-   it equally pointedly [skips newlines](http://codemirror.net/doc/manual.html#modeapi), so figuring it out for yourself is possible but non-trivial
-   *and* doesn't guarantee that the parsed version is going to match the latest edits to a particular mirror, so even if I got one of the above working, there would be some odd jumps between re-parses of the mirror contents


Ultimately, I decided that getting enough information out of the internal parser would be more effort than it was worth, and just put together an s-experession jumper that works on a raw string. Less elegant than the ideal, but it works. Finally, and second-most annoyingly<a name="note-Thu-Aug-14-173713EDT-2014"></a>[|2|](#foot-Thu-Aug-14-173713EDT-2014), there seems to be no built-in way to test whether the cursor is currently at the beginning or end of an editor, so that's something else I've [had to put together](https://github.com/Inaimathi/cl-notebook/blob/master/front-end.lisp#L634-L642) from whole cloth. So yeah. At the very least, I've knocked a few more line items out of the [`cl-notebook` TODOs](https://github.com/Inaimathi/cl-notebook#todo).

The thing that ended up surprisingly helping is the fact that `CodeMirror`s cursor is between characters instead of *at* one. Thinking about every cursor operation as looking either to the left or to the right forced me to write symmetrically all the way down to the `get-cur` function. Which in turn meant that I could relatively easily write a symmetric `go-sexp`<a name="note-Thu-Aug-14-173716EDT-2014"></a>[|3|](#foot-Thu-Aug-14-173716EDT-2014), and I'd sort of written that off from the get-go. I like it much better this way because it means I can centralize related operations, and I really like having only one place to change for a particular feature/bug.


* * *
##### Footnotes
1 - <a name="foot-Thu-Aug-14-173707EDT-2014"></a>[|back|](#note-Thu-Aug-14-173707EDT-2014) - Well, the next `cl-notebook`-related thing on my plate. I'm quite likely to cut over to [AOSA work](https://github.com/Inaimathi/500lines/blob/master/event-driven-web-server/writeup.md) for the next while, but you know what I mean dammit.

2 - <a name="foot-Thu-Aug-14-173713EDT-2014"></a>[|back|](#note-Thu-Aug-14-173713EDT-2014) - This ended up hanging Firefox a few times in testing before I figured out what to do about it. If you're wondering, the `stream` thing holds "most annoying".

3 - <a name="foot-Thu-Aug-14-173716EDT-2014"></a>[|back|](#note-Thu-Aug-14-173716EDT-2014) - Which is to say, one whose direction can easily be parameterized.
