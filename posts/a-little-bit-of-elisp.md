<p>I've had too much Common Lisp coding at work this week. I basically did two 12 hour sessions across Wednesday and Thursday, then a 4 hour on Friday with a little time off for fighting PHP-related fires and a bit of sleep. So today, I took a break. And what did I do on my break, you might ask? </p>

<p>I hacked Emacs Lisp.</p>

<p>I tell ya, my fiancee loves me. That's right. I took a break from my Common Lisp day-job by getting up on Saturday and dusting off some old Elisp code I had lying around. I touched up some <code>git-mode</code> customizations, an <a href="http://codereview.stackexchange.com/questions/45/emacs-etags-shortcut-functions">etags library</a> I sometimes use, and my <code>.emacs</code> itself, but my main target was the <code>blog-mode</code> module (which I've actually been using to write these articles, except for <a href="http://langnostic.blogspot.com/2011/03/puzzling-with-lisp.html">one awkward brush with a markdown converter</a>). It has served, but the code was far from elegant, and there were a couple of features I've been meaning to add, but never quite got around to, always telling myself to just get through the blog post instead. The code is <b>still</b> far from elegant, so I won't talk about that, but the features are there. </p>

<p>First thing, and probably the most pressing, is that those nice highlighted code-blocks were getting annoying. It would work fine for plain gray text (which I use sometimes, in small inline snippets), but to do it properly, I had to paste code into a separate buffer, turn on the correct highighting mode, run htmlize-buffer on it, then paste it back into the blog post and maybe tweak it for good measure. I figured that my ideal interaction would be the code auto-detecting what language I'm using and highighting correctly, but one step back would be asking for a highlighting mode and applying it to the code I wanted to htmlize. So here's how that looks</p>

<pre><span style="color: #b22222;">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; </span><span style="color: #b22222;">&lt;pre&gt; and &lt;code&gt; definitions
</span>(definsert code-block <span style="color: #8b2252;">"&lt;pre&gt;"</span> <span style="color: #8b2252;">"&lt;/pre&gt;"</span>)
(definsert inline-code <span style="color: #8b2252;">"&lt;code&gt;"</span> <span style="color: #8b2252;">"&lt;/code&gt;"</span>)

<span style="color: #b22222;">;; </span><span style="color: #b22222;">region versions are more complicated to accomodate htmlize
</span>(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">region-to-inline-code</span> (code-mode)
  <span style="color: #8b2252;">"HTMLize just the current region and wrap it in a &lt;code&gt; block"</span>
  (interactive <span style="color: #8b2252;">"CMode name: "</span>)
  (<span style="color: #a020f0;">let*</span> ((start (region-beginning))
         (end (region-end))
         (htmlified (get-htmlified-region start end code-mode)))
    (delete-region start end)
    (insert-inline-code)
    (insert htmlified)))

(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">region-to-code-block</span> (code-mode)
  <span style="color: #8b2252;">"HTMLize the current region and wrap it in a &lt;pre&gt; block"</span>
  (interactive <span style="color: #8b2252;">"CMode name: "</span>)
  (<span style="color: #a020f0;">let*</span> ((start (region-beginning))
         (end (region-end))
         (result (get-htmlified-region start end code-mode)))
    (delete-region start end)
    (insert-code-block)
    (insert result)))

(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">get-htmlified-region</span> (start end code-mode)
  <span style="color: #8b2252;">"Returns a string of the current region HTMLized with highlighting according to code-mode"</span>
  (<span style="color: #a020f0;">let</span> ((htmlified nil))
    (clipboard-kill-ring-save start end)
    (get-buffer-create <span style="color: #8b2252;">"*blog-mode-temp*"</span>) <span style="color: #b22222;">;;</span><span style="color: #b22222;">using 'with-temp-buffer here doesn't apply correct higlighting
</span>    (<span style="color: #a020f0;">with-current-buffer</span> <span style="color: #8b2252;">"*blog-mode-temp*"</span>
      (funcall code-mode)
      (clipboard-yank)
      (setq htmlified (substring (htmlize-region-for-paste (point-min) (point-max)) 6 -6)))
    (kill-buffer <span style="color: #8b2252;">"*blog-mode-temp*"</span>)
    htmlified))</pre>

<p>I pasted that block in from my code file, highlighted it, then typed <code>C-c C-p emacs-lisp-mode [ret]</code>, in case you were wondering. The result was that pretty block above. <code>region-to-code-block</code> and <code>region-to-inline-code</code> are actually the same function except for which insert they use, and I <b>would</b> factor that out if it ever got to the point that there needed to be a third function doing the same, but it doesn't seem worth it for just two functions.</p>

<span class="edit">EDIT:

<p>Ok, ok goddammit. Here. They're simplified now.</p>

<pre>(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">region-to-inline-code</span> (code-mode)
  <span style="color: #8b2252;">"HTMLize just the current region and wrap it in a &lt;code&gt; block"</span>
  (interactive <span style="color: #8b2252;">"CMode name: "</span>)
  (htmlized-region code-mode #'insert-inline-code))

(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">region-to-code-block</span> (code-mode)
  <span style="color: #8b2252;">"HTMLize the current region and wrap it in a &lt;pre&gt; block"</span>
  (interactive <span style="color: #8b2252;">"CMode name: "</span>)
  (htmlized-region code-mode #'insert-code-block))

(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">htmlized-region</span> (code-mode insert-fn)
  (<span style="color: #a020f0;">let*</span> ((start (region-beginning))
         (end (region-end))
         (result (get-htmlified-region start end code-mode)))
    (delete-region start end)
    (funcall insert-fn)
    (insert result)))</pre>
Sun, 27 Mar, 2011</span>

<p>I uh, also put in an edit block function and a footnote manager<a href="#foot-Sun-Mar-27-013129EDT-2011" name="note-Sun-Mar-27-013129EDT-2011">[1]</a>. The edit blocks are pretty self-explanatory; just a block with a date at the bottom to indicate when I did the thing. After a couple of definition macros<a href="#foot-Sun-Mar-27-013353EDT-2011" name="note-Sun-Mar-27-013353EDT-2011">[2]</a>, it's actually a one-liner.</p>

<pre>(deftag edit <span style="color: #8b2252;">"&lt;span class=\"edit\"&gt;EDIT:\n\n"</span> (concat <span style="color: #8b2252;">"\n"</span> (format-time-string <span style="color: #8b2252;">"%a, %d %b, %Y"</span> (current-time)) <span style="color: #8b2252;">"&lt;/span&gt;"</span>))</pre>

<p>The footnote manager is a bit more complex. I've actually been doing them manually for the last little while, which started to get frustrating<a href="#foot-Sun-Mar-27-013612EDT-2011" name="note-Sun-Mar-27-013612EDT-2011">[3]</a>. The process was to put a numbered tag down with <code>&lt;<span style="color: #0000ff;">a</span> <span style="color: #a0522d;">name</span>=<span style="color: #8b2252;">"somethingHopefullyUnique"</span>&gt;</code>, and hook it up to a correspondingly numbered [back] link at the bottom of the page, then write the footnote, then find my way back. The linking turns out to be the hardest part there, because these posts potentially get displayed together on my blog, so I had to be very careful to make the name unique across the entire blogs' history, not just within that article<a href="#foot-Sun-Mar-27-014303EDT-2011" name="note-Sun-Mar-27-014303EDT-2011">[4]</a>. With this new function, instead it's <code>C-c f</code> to insert a fresh footnote, or <code>C-c C-f</code> to convert the selected region to a footnote. The links are generated and numbered automatically, so all I have to do is actually write the footnote<a href="#foot-Sun-Mar-27-014111EDT-2011" name="note-Sun-Mar-27-014111EDT-2011">[5]</a>.</p>

<pre><span style="color: #b22222;">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; </span><span style="color: #b22222;">footnote definitions
</span>(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">insert-footnote</span> ()
  <span style="color: #8b2252;">"Inserts footnote, and a return link at the bottom of the file. 
   Moves point to footnote location."</span>
  (interactive)
  (<span style="color: #a020f0;">progn</span> (footnotes-header)
         (<span style="color: #a020f0;">let</span> ((footnote-name (format-time-string <span style="color: #8b2252;">"%a-%b-%d-%H%M%S%Z-%Y"</span> (current-time)))
               (num (number-to-string (+ 1 (count-footnotes)))))
           (insert <span style="color: #8b2252;">"&lt;a href=\"#foot-"</span> footnote-name <span style="color: #8b2252;">"\" name=\"note-"</span> footnote-name <span style="color: #8b2252;">"\"&gt;["</span> num <span style="color: #8b2252;">"]&lt;/a&gt;"</span>)
           (goto-char (point-max))
           (insert <span style="color: #8b2252;">"\n\n"</span> num <span style="color: #8b2252;">" - &lt;a href=\"#note-"</span> footnote-name <span style="color: #8b2252;">"\" name=\"foot-"</span> footnote-name <span style="color: #8b2252;">"\"&gt;[back]&lt;/a&gt; - "</span>))))

(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">region-to-footnote</span> ()
  <span style="color: #8b2252;">"Inserts a footnote at point and return link at the bottom. Moves the current region to the end of the file. 
   Leaves point where it is."</span>
  (interactive)
  (<span style="color: #a020f0;">save-excursion</span> (kill-region (region-beginning) (region-end))
         (insert-footnote)
         (yank)))

(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">footnotes-header</span> ()
  <span style="color: #8b2252;">"Inserts footnote header if not already present"</span>
  (<span style="color: #a020f0;">unless</span> (<span style="color: #a020f0;">save-excursion</span> (search-forward blog-footnote-header nil t))
    (<span style="color: #a020f0;">save-excursion</span> 
      (goto-char (point-max))
      (insert <span style="color: #8b2252;">"\n\n"</span> blog-footnote-header))))

(<span style="color: #a020f0;">defun</span> <span style="color: #0000ff;">count-footnotes</span> ()
  <span style="color: #8b2252;">"Returns the number of footnotes in the current file. Used for human-readable note labels"</span>
  (interactive)
  (<span style="color: #a020f0;">save-excursion</span>
    (<span style="color: #a020f0;">if</span> (not (search-forward blog-footnote-header nil t))
        0
      (<span style="color: #a020f0;">let</span> ((count -1))
        (<span style="color: #a020f0;">while</span> (<span style="color: #a020f0;">progn</span> (setq count (1+ count))
                      (search-forward <span style="color: #8b2252;">"&lt;a href=\"#note-"</span> nil t)))
        count))))</pre>

<p>Boy, that's playing hell with the highlighting right now. It's fairly self-explanatory; <code>count-footnotes</code> counts up how many footnotes I have left, <code>footnotes-header</code> checks if there's a footnote header in the post already<a href="#foot-Sun-Mar-27-014807EDT-2011" name="note-Sun-Mar-27-014807EDT-2011">[6]</a>, <code>insert-footnote</code> just creates a new footnote/backlink and takes me to the bottom of the page to write it, and finally, <code>region-to-footnote</code> takes the current region and converts it to a new footnote (leaving the point where it is).</p>

<p>Even though it's a simple, and specific<a href="#foot-Sun-Mar-27-015109EDT-2011" name="note-Sun-Mar-27-015109EDT-2011">[7]</a> piece of code, I still learned a lot by testing it out like this. Specifically, the code formatting functions need to accept <code>nil</code> as an argument<a href="#foot-Sun-Mar-27-015300EDT-2011" name="note-Sun-Mar-27-015300EDT-2011">[8]</a> (which should take 5 minutes), and the footnote section needs a way to re-number footnotes and jump between corresponding note/back links (which seems like it could take a while).</p>

<p>I'm going to sleep now though; I'll leave those features for the next time I need a break from Common Lisp.</p>

<span class="edit">EDIT:

<p>Ok, so it was actually slightly less than 5 minutes to get the code argument done; one line change did it (see if you can guess which one)</p>

<pre>(<span style="color: #a020f0;">when</span> (fboundp code-mode) (funcall code-mode))</pre>

<p>The latest is now up <a href="https://github.com/Inaimathi/emacs-utils">at github</a>.</p>
Sun, 27 Mar, 2011</span>

<hr />
<h5>Footnotes</h5>

<p>1 - <a href="#note-Sun-Mar-27-013129EDT-2011" name="foot-Sun-Mar-27-013129EDT-2011">[back]</a> - And yes, since you ask, I am basically using this post as a way to test the editing mode I'm talking about.</p>
<p>2 - <a href="#note-Sun-Mar-27-013353EDT-2011" name="foot-Sun-Mar-27-013353EDT-2011">[back]</a> - If you want to see the definition macros, check out <a href="https://github.com/Inaimathi/emacs-utils">the github page</a> I started for my little utility files. The documentation is extremely light, but it's only because I fully expect to be the only one using these.</p>
<p>3 - <a href="#note-Sun-Mar-27-013612EDT-2011" name="foot-Sun-Mar-27-013612EDT-2011">[back]</a> - To the point that I would frequently include a long, rambling paranthetical comment instead of putting the damned thought in a footnote, where it belongs. Interface difficulties really do lead to a lot of shoddy work, it seems.</p>
<p>4 - <a href="#note-Sun-Mar-27-014303EDT-2011" name="foot-Sun-Mar-27-014303EDT-2011">[back]</a> - The way I'd been doing that was by using the article name and a number in the <code>href</code> and <code>name</code> parameters. The mode is actually better, using a date and timestamp.</p>
<p>5 - <a href="#note-Sun-Mar-27-014111EDT-2011" name="foot-Sun-Mar-27-014111EDT-2011">[back]</a> - I still haven't found a way to automate writing these columns, but that's not the same as saying it can't be done.</p>
<p>6 - <a href="#note-Sun-Mar-27-014807EDT-2011" name="foot-Sun-Mar-27-014807EDT-2011">[back]</a> - And adds one if it doesn't exist yet.</p>
<p>7 - <a href="#note-Sun-Mar-27-015109EDT-2011" name="foot-Sun-Mar-27-015109EDT-2011">[back]</a> - Which is to say, it had a very specific goal in mind.</p>
<p>8 - <a href="#note-Sun-Mar-27-015300EDT-2011" name="foot-Sun-Mar-27-015300EDT-2011">[back]</a> - (and default to <code>fundamental-mode</code> in that case)</p>
