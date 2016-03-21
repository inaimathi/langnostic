I've had too much Common Lisp coding at work this week. I basically did two 12 hour sessions across Wednesday and Thursday, then a 4 hour on Friday with a little time off for fighting PHP-related fires and a bit of sleep. So today, I took a break. And what did I do on my break, you might ask?

I hacked Emacs Lisp.

I tell ya, my fiancee loves me. That's right. I took a break from my Common Lisp day-job by getting up on Saturday and dusting off some old Elisp code I had lying around. I touched up some `git-mode` customizations, an [etags library](http://codereview.stackexchange.com/questions/45/emacs-etags-shortcut-functions") I sometimes use, and my `.emacs` itself, but my main target was the `blog-mode` module (which I've actually been using to write these articles, except for [one awkward brush with a markdown converter](http://langnostic.blogspot.com/2011/03/puzzling-with-lisp.html")). It has served, but the code was far from elegant, and there were a couple of features I've been meaning to add, but never quite got around to, always telling myself to just get through the blog post instead. The code is <b>still</b> far from elegant, so I won't talk about that, but the features are there[^note-0].

[^note-0]: **Note from the future - March 20th of 2016:** The actual content of this article is now woefully oboslescent, by the way. I've since moved on to using a [client-side highlighter](TODO - js highlight) and [`pandoc`](TODO) for post transformation. The Markdown extensions `pandoc` provides include non-recursive footnotes, so everything I talk about semi-automating in this article gets handled fully-automatically now.

First thing, and probably the most pressing, is that those nice highlighted code-blocks were getting annoying. It would work fine for plain gray text (which I use sometimes, in small inline snippets), but to do it properly, I had to paste code into a separate buffer, turn on the correct highighting mode, run htmlize-buffer on it, then paste it back into the blog post and maybe tweak it for good measure. I figured that my ideal interaction would be the code auto-detecting what language I'm using and highighting correctly, but one step back would be asking for a highlighting mode and applying it to the code I wanted to htmlize. So here's how that looks

```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; <pre> and <code> definitions
(definsert code-block "<pre>" "</pre>")
(definsert inline-code "<code>" "</code>")

;; region versions are more complicated to accomodate htmlize
(defun region-to-inline-code (code-mode)
  "HTMLize just the current region and wrap it in a <code> block"
  (interactive "CMode name: ")
  (let* ((start (region-beginning))
         (end (region-end))
         (htmlified (get-htmlified-region start end code-mode)))
    (delete-region start end)
    (insert-inline-code)
    (insert htmlified)))

(defun region-to-code-block (code-mode)
  "HTMLize the current region and wrap it in a <pre> block"
  (interactive "CMode name: ")
  (let* ((start (region-beginning))
         (end (region-end))
         (result (get-htmlified-region start end code-mode)))
    (delete-region start end)
    (insert-code-block)
    (insert result)))

(defun get-htmlified-region (start end code-mode)
  "Returns a string of the current region HTMLized with highlighting according to code-mode"
  (let ((htmlified nil))
    (clipboard-kill-ring-save start end)
    (get-buffer-create "*blog-mode-temp*") ;;using 'with-temp-buffer here doesn't apply correct higlighting
    (with-current-buffer "*blog-mode-temp*"
      (funcall code-mode)
      (clipboard-yank)
      (setq htmlified (substring (htmlize-region-for-paste (point-min) (point-max)) 6 -6)))
    (kill-buffer "*blog-mode-temp*")
    htmlified))
```

I pasted that block in from my code file, highlighted it, then typed `C-c C-p emacs-lisp-mode [ret]`, in case you were wondering. The result was that pretty block above. `region-to-code-block` and `region-to-inline-code` are actually the same function except for which insert they use, and I <b>would</b> factor that out if it ever got to the point that there needed to be a third function doing the same, but it doesn't seem worth it for just two functions.

> EDIT:
> Ok, ok goddammit. Here. They're simplified now.
>
> ```lisp
> (defun region-to-inline-code (code-mode)
>   "HTMLize just the current region and wrap it in a <code> block"
>   (interactive "CMode name: ")
>   (htmlized-region code-mode #'insert-inline-code))
>
> (defun region-to-code-block (code-mode)
>   "HTMLize the current region and wrap it in a <pre> block"
>   (interactive "CMode name: ")
>   (htmlized-region code-mode #'insert-code-block))
>
> (defun htmlized-region (code-mode insert-fn)
>   (let* ((start (region-beginning))
>          (end (region-end))
>          (result (get-htmlified-region start end code-mode)))
>     (delete-region start end)
>     (funcall insert-fn)
>     (insert result)))
> ```
>
> Sun, 27 Mar, 2011

I uh, also put in an edit block function and a footnote manager[^note-1]. The edit blocks are pretty self-explanatory; just a block with a date at the bottom to indicate when I did the thing. After a couple of definition macros[^note-2], it's actually a one-liner.

[^note-1]: And yes, since you ask, I am basically using this post as a way to test the editing mode I'm talking about.
[^note-2]: If you want to see the definition macros, check out [the github page](https://github.com/Inaimathi/emacs-utils") I started for my little utility files. The documentation is extremely light, but it's only because I fully expect to be the only one using these.


```lisp
(deftag edit "<span class=\"edit\">EDIT:\n\n" (concat "\n" (format-time-string "%a, %d %b, %Y" (current-time)) "</span>"))
```

The footnote manager is a bit more complex. I've actually been doing them manually for the last little while, which started to get frustrating[^note-3]. The process was to put a numbered tag down with `<a name="somethingHopefullyUnique">`, and hook it up to a correspondingly numbered [back] link at the bottom of the page, then write the footnote, then find my way back. The linking turns out to be the hardest part there, because these posts potentially get displayed together on my blog, so I had to be very careful to make the name unique across the entire blogs' history, not just within that article[^note-4]. With this new function, instead it's `C-c f` to insert a fresh footnote, or `C-c C-f` to convert the selected region to a footnote. The links are generated and numbered automatically, so all I have to do is actually write the footnote[^note-5].

[^note-3]: To the point that I would frequently include a long, rambling paranthetical comment instead of putting the damned thought in a footnote, where it belongs. Interface difficulties really do lead to a lot of shoddy work, it seems.
[^note-4]: The way I'd been doing that was by using the article name and a number in the `href` and `name` parameters. The mode is actually better, using a date and timestamp.
[^note-5]: I still haven't found a way to automate writing these columns, but that's not the same as saying it can't be done.

```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; footnote definitions
(defun insert-footnote ()
  "Inserts footnote, and a return link at the bottom of the file.
   Moves point to footnote location."
  (interactive)
  (progn (footnotes-header)
         (let ((footnote-name (format-time-string "%a-%b-%d-%H%M%S%Z-%Y" (current-time)))
               (num (number-to-string (+ 1 (count-footnotes)))))
           (insert "<a href=\"#foot-" footnote-name "\" name=\"note-" footnote-name "\">[" num "]</a>")
           (goto-char (point-max))
           (insert "\n\n" num " - <a href=\"#note-" footnote-name "\" name=\"foot-" footnote-name "\">[back]</a> - "))))

(defun region-to-footnote ()
  "Inserts a footnote at point and return link at the bottom. Moves the current region to the end of the file.
   Leaves point where it is."
  (interactive)
  (save-excursion (kill-region (region-beginning) (region-end))
         (insert-footnote)
         (yank)))

(defun footnotes-header ()
  "Inserts footnote header if not already present"
  (unless (save-excursion (search-forward blog-footnote-header nil t))
    (save-excursion
      (goto-char (point-max))
      (insert "\n\n" blog-footnote-header))))

(defun count-footnotes ()
  "Returns the number of footnotes in the current file. Used for human-readable note labels"
  (interactive)
  (save-excursion
    (if (not (search-forward blog-footnote-header nil t))
        0
      (let ((count -1))
        (while (progn (setq count (1+ count))
                      (search-forward "<a href=\"#note-" nil t)))
        count))))
```

Boy, that's playing hell with the highlighting right now. It's fairly self-explanatory; `count-footnotes` counts up how many footnotes I have left, `footnotes-header` checks if there's a footnote header in the post already[^note-6], `insert-footnote` just creates a new footnote/backlink and takes me to the bottom of the page to write it, and finally, `region-to-footnote` takes the current region and converts it to a new footnote (leaving the point where it is).

[^note-6]: And adds one if it doesn't exist yet.

Even though it's a simple, and specific[^note-7] piece of code, I still learned a lot by testing it out like this. Specifically, the code formatting functions need to accept `nil` as an argument<a name="note-Sun-Mar-27-015300EDT-2011"></a>[|8|](#foot-Sun-Mar-27-015300EDT-2011) (which should take 5 minutes), and the footnote section needs a way to re-number footnotes and jump between corresponding note/back links (which seems like it could take a while).

[^note-7]: Which is to say, it had a very specific goal in mind.
[^note-8]: _(and default to `fundamental-mode` in that case)_

I'm going to sleep now though; I'll leave those features for the next time I need a break from Common Lisp.

> EDIT:
>
> Ok, so it was actually slightly less than 5 minutes to get the code argument done; one line change did it (see if you can guess which one)
>
> ```lisp
> (when (fboundp code-mode) (funcall code-mode))
> ```
>
> The latest is now up [at github](https://github.com/Inaimathi/emacs-utils").
> Sun, 27 Mar, 2011
