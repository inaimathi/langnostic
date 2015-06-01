I need a short break here. It's nowhere near done yet, not even the pieces I wanted to put together. But, at the risk of ending up with another piece of detritus littering my blog, I need to let off some steam and talk about where I'm going with this.

For the past couple of days, I've been busy working on writing a replacement for [rebol.el](http://www.rebol.com/tools/rebol.el) and running into some not altogether unexpected headaches. The file I just linked you hasn't been updated since about 2001, and doesn't include a license block. After attempting to contact its current host and getting no response, I just went ahead and started from scratch. I had a few goals from the outset:

## <a name="proper-highlighting"></a>Proper Highlighting

The current `rebol.el` was put together for REBOL2, and thus lacks highlighting for certain symbols that have been introduced since. The one that sticks out most in my mind is the symbol `funct`, which you saw semi-humorously higlighted as `funct` in the third section [last time](http://langnostic.blogspot.ca/2013/07/rebol-without-cause.html).

## <a name="jumptodefinition"></a>Jump-To-Definition

Some kind of binding in a mode that lets you jump to the file and line of the definition of a given symbol. Not entirely sure what the interaction there is going to be, probably just a key-binding that jumps for the `thing-at-point`. This one looks like it would be pretty simple to pull off actually; when a file is loaded, record the position of any assignments in it. Assignments are simple to find, since there's exactly one way to do it, and it involves adding a single-character suffix to whatever word you're assigning.

## <A NAME="REPL"></A>REPL

There isn't a `run-rebol`, in the style of `run-python` or `run-lisp`, and I'd like one.

## <a name="send-region"></a>Send Region

Fairly self-explanatory. Or maybe not? In Lisp modes, there's typically a binding to evaluate the current s-expression, either `C-c C-c` or `C-M-x`. When you hit it, the effect is to evaluate the surrounding block into the REPL. There isn't a `send-region` command in the current `rebol-mode`, probably because they don't directly implement an in-Emacs-REPL, but since I want the second, I'd also like the first.

## <a name="documentation-display"></a>Documentation Display

Just a simple, straight-forward way to display help about a particular symbol in a separate buffer. Nothing fancy, move along. Ok, in a future edition, it would be nice if the return text was all [linkified](http://www.gnu.org/software/emacs/manual/html_node/elisp/Clickable-Text.html) and [highlighted](https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Properties.html#Text-Properties) so that you could click/`&lt;Ret>` through docs, but that can wait.

## <a name="source-display"></a>Source Display

This one might be a bit more functional. You see, you can use the `[source](http://www.rebol.com/r3/docs/functions/source.html)` function to get a source dump of almost<a name="note-Fri-Aug-02-000908EDT-2013"></a>[|1|](#foot-Fri-Aug-02-000908EDT-2013) any REBOL3 word. It may or may not be useful at all, but it would be pretty cool to build a buffer that would let you load such `source` output, change it, then send it back to the REPL when you save.

## <a name="argument-hints"></a>Argument Hints

If you've used things like [SLIME](http://common-lisp.net/project/slime/), you'll appreciate this one. As you're typing, a summary of the arguments to the thing you're typing shows up in the minibuffer. This is trivial in Lisp, because of the way everything is parenthesized pretty consistently. It turns out to be quite a headache in REBOL3, and basically necessitates interacting with some sort of running runtime system. Here's an example, pretend these are all actual REBOL words:

foo bar baz mumble |

Assuming that pipe represents my cursor, what should the mode display in your minibuffer, and how would you find out? Near as I can tell, you'd need to ask a running r3 interpreter with the words `foo`, `bar`, `baz` and `mumble` defined. What it would do is:


-   check if `mumble` is a function, and if so, print the arg-hint for `mumble`, otherwise
-   check if `baz` is a function of more than one argument, and if so print the arg-hint for `baz`, otherwise
-   check if `bar` is a function of more than two arguments, and if so print the blah blah blah


until you run out of words to check. Another open question is: how far back should the mode look for relevant symbols? For the moment, I've settled on "To the beginning of the previous block, or the first assignment, whichever comes first", but that's probably not the best approach to take.

### <a name="where-ive-got-so-far"></a>Where I've Got So Far

At the moment, I'm about a quarter of the way there by my reckoning. And I've run into some issues, both expected and unexpected. The mode as currently posted [here](https://github.com/Inaimathi/r3-mode), implements proper highlighting, a basic REPL, a basic documentation display, a basic source display, and a hacked-together `send-region`. I've already gone through the problems with argument hinting in an almost-purely whitespace-delimited language; there was only one completely unexpected problem and two little gotchas I ran into. Lets start small:

[Gotcha the first](https://github.com/Inaimathi/r3-mode/blob/master/r3-mode.el#L93-L96) is that [REBOL path strings](http://www.rebol.com/docs/core23/rebolcore-12.html#section-2.2) aren't fully cross-platform. They do the standard Unix thing, and auto-convert for Windows, but if you include a drive letter, which the Windows version of Emacs does by default, your string won't be recognized as a file. As a result, I need that extra snippet to sanitize Windows paths.

[Gotcha the second](https://github.com/Inaimathi/r3-mode/blob/master/r3-mode.el#L51-L53) is another cross-platform, or possibly cross-version, issue. For some reason the Linux edition of the r3 `comint` buffer prints its input before its return value. That is

```lisp
(defun proc-filter (process msg)
  (message "%s" msg))

(set-process-filter proc #'proc-filter)

;; on Linux

(process-send-string proc "source source")

;; Output to *Message* is
;;
;;    source source
;;    source: make function ! [[ 
;;    ..
;;    >>

;; on Windows

(process-send-string proc "source source")

;; Output to *Message* is
;;
;;    source: make function ! [[ 
;;    ..
;;    >>
```

Not a *huge* deal, except that I have to deal with it if I want to succeed in my master plan of running an r3 interpreter behind the scenes. Which brings me to the big, unexpected piece of code I had to write. This actually took a few tries, as I came to grips with the situation.

```lisp
(defun r3-process-filter (proc msg)
  "Receives messages from the r3 background process.
Processes might send responses in 'bunches', rather than one complete response,
which is why we need to collect them, then split on an ending flag of some sort.
Currently, that's the REPL prompt '^>> '"
  (let ((buf ""))
    (setf buf (concat buf msg))
    (when (string-match ">> $" msg)
      (mapc #'r3-ide-directive 
            (split-string buf "^>> "))
      (setf buf ""))))

(defun r3-send! (string)
  "Shortcut function to send a message to the background r3 interpreter process"
  (process-send-string r3-rebol-process (concat string "\n")))

(set-process-filter r3-rebol-process #'r3-process-filter)

(defun r3-ide-directive (msg)
  (let* ((raw-lines (butlast (split-string msg "\r?\n")))
         ;; the linux edition seems to return the function call before its output. Might also be an Emacs version issue.
         (lines (if (eq system-type 'gnu/linux) (rest raw-lines) raw-lines)))
    (when lines
      (cond ((string-match "NEW-KEYWORDS: \\(.*\\)" (first lines))
             (let ((type (intern (match-string 1 (first lines)))))
               (setf (gethash type r3-highlight-symbols) (rest lines))
               (r3-set-fonts)))
            ((string-match "HELP: \\(.*\\)" (first lines))
             (get-buffer-create "*r3-help*")
             (with-current-buffer "*r3-help*"
               (kill-region (point-min) (point-max))
               (insert ";;; " (match-string 1 (first lines)) " ;;;\n\n")
               (mapc (lambda (l) (insert l) (insert "\n")) (rest lines)))
             (pop-to-buffer "*r3-help*"))
            ((string-match "SOURCE" (first lines))
             (ignore-errors (kill-buffer "*r3-source*"))
             (get-buffer-create "*r3-source*")
             (with-current-buffer "*r3-source*"
               (mapc (lambda (l) (insert l) (insert "\n")) (rest lines))
               (r3-mode))
             (pop-to-buffer "*r3-source*"))))))
```

The third piece there isn't terribly important<a name="note-Fri-Aug-02-000919EDT-2013"></a>[|2|](#foot-Fri-Aug-02-000919EDT-2013). The gist of it is that I want to run a separate `r3` process, and communicate it for certain things. In order to do that, I have to attach a listener to the process. Then, whenever I send a string to the process, it will respond with a process id and message to that listener.

The catch I wasn't counting on was that output arrives in "bunches". Which is to say, if you send three or four commands, you're going to get back strings of ~400 characters, each containing either a partial response, a full response or multiple full/partial responses. Because I'm not expecting responses much larger than a couple thousand characters, I can get away with just buffering until output lets up, but that might not be the best general strategy.

I'll talk about this a bit more after I've had some more time to work on it. Right now?

I. Need. Sleep.

Cheers.


* * *
##### Footnotes

1 - <a name="foot-Fri-Aug-02-000908EDT-2013"></a>[|back|](#note-Fri-Aug-02-000908EDT-2013) - Before you ask, yes, it's entirely possible to get the `source` of `source`. The only words you can't introspect on in this way are `native!`s, which are implemented in C rather than REBOL.

2 - <a name="foot-Fri-Aug-02-000919EDT-2013"></a>[|back|](#note-Fri-Aug-02-000919EDT-2013) - As a note to self, I'm going to have to re-write pieces of it. Both for speed, and because I'm repeating blocks for each condition. That needs to be a mini-dsl instead of manual code.
