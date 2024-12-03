Given [my recent `trivialai` work](https://github.com/inaimathi/trivialai?tab=readme-ov-file#trivialai), I thought I'd update my local [AI development integration](https://github.com/inaimathi/machine-setup/blob/master/emacs/aidev.el). And I have. And it's pretty cool, because now I can basically trivially switch between OpenAI and Claude, both of which are implemented [in my `shell-ui` repo](https://github.com/inaimathi/shell-ui/blob/master/python/claude).

Nothing big, in fact I _mostly_ took stuff out.

As of this writing, the new `aidev.el` file looks like

```
(require 'request)

(defun aidev--chat (system messages)
  (let* ((cmd (format
	       "gpt -s %s %s"
	       (shell-quote-argument system)
	       (string-join
		(mapcar
		 (lambda (m) (shell-quote-argument (json-encode m)))
		 messages)
		" ")))
         (result (shell-command-to-string cmd)))
    (string-trim
     (replace-regexp-in-string
      "\\(?:^```[a-zA-Z-]*\\s-*\n\\|\\n?```\\s-*$\\)"
      ""
      result))))

(defun aidev-insert-chat (prompt)
  (interactive "sPrompt: ")
  (let ((system
	 (string-join
	  (list
	   "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."
	   "The likeliest requests involve generating code. If you are asked to generate code, only return code, and no commentary. If you must, provide minor points and/or testing examples in the form of code comments (commented in the appropriate syntax) but no longer prose unless explicitly requested."
	   (format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode))
	  "\n"))
	(prompt
	 `(,@(when (region-active-p)
	       `((("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))
	    (("role" . "user") ("content" . ,prompt)))))
    (insert (aidev--chat system prompt))))

(defun aidev-refactor-region-with-chat (prompt)
  "Refactors the current region using `aidev--chat` function and a prompt."
  (interactive "sPrompt: ")
  (when (use-region-p)
    (let* ((system (string-join
		    (list
		     "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."
		     (format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)
		     "The user wants you to help them refactor a piece of code they've already written. Unless specified by their prompt, you should output code in the same language as the input code. Output absolutely nothing but code; the message you return should be a drop-in replacement for the code the user needs help with.")
		    "\n"))
	   (prompt `((("role" . "user")
		      ("content" . ,prompt))
		     (("role" . "user")
		      ("content" . ,(buffer-substring-no-properties
				     (region-beginning) (region-end))))))
	   (data (aidev--chat system prompt))
	   (reg-start (region-beginning))
	   (reg-end (region-end)))
      (goto-char reg-start)
      (delete-region reg-start reg-end)
      (insert data))))

(provide 'aidev)
```
Which is similar to the old one except that:

1. I've removed `aidev-document-python-region`, `aidev-explain-reason` and `aidev-explain-reason-in-particular` because I've used each exactly once (to test whether they work) and then promptly let them gather dust
2. I've changed the interface of `aidev--chat` to make it easier to switch out between `claude` and `chatgpt` on the back-end

Basically everything else is downstream of the second change, so it's the only one I need to really explain.

If you check out the `python` section of [`shell-ui`](https://github.com/inaimathi/shell-ui/tree/master), you'll notice that I have two separate shell scripts there to facilitate this dev environment hookup; [`gpt`](https://github.com/inaimathi/shell-ui/blob/master/python/gpt) and [`claude`](https://github.com/inaimathi/shell-ui/blob/master/python/claude). The _actual_ difference between them is the way that they handle `system` prompts. `claude` expects a top-level `system` argument, whereas `gpt` expects you to add some number of system messages to your `messages` list with the `role` of `system`. This means that the old setup here wouldn't have been the most straightforward approach.

```
(defun aidev--chat (system messages)
  (let* ((cmd (format
	       "gpt -s %s %s"
	       (shell-quote-argument system)
	       (string-join
		(mapcar
		 (lambda (m) (shell-quote-argument (json-encode m)))
		 messages)
		" ")))
         (result (shell-command-to-string cmd)))
    (string-trim
     (replace-regexp-in-string
      "\\(?:^```[a-zA-Z-]*\\s-*\n\\|\\n?```\\s-*$\\)"
      ""
      result))))
```

This new implementation takes `system` and `messages` as separate parameters and uses a command line option to pass the former instead of throwing both in as arguments. The only other difference is that `string-trim` change at the bottom. For some reason, even when you specifically tell them to _only_ return runnable source code and no other content, all modern models seem to return code in [`markdown`](https://daringfireball.net/projects/markdown/)-style blocks that look like

~~~
```<language name>
(defun you-code-actually (here) (list here))
```
~~~

Given what I'm using these functions for, I'll just take the raw code, thank you. So that extra `replace-regexp-in-string` removes the markdown code-block boundaries under the assumption that they happen at the beginning and end of the `string-trim`med response. I never would have sat down and written this monstrosity by hand, by the way, the old version of `aidev-insert-chat` provided the definition, and I tested it on a few representative inputs. 

If it happens to leave some weird edge case untouched... shrug I guess?

### The Bigger Picture

I'm doing this in preparation for some serious code automation I'm trying out. It's going to involve the next level of workflow automation for me. Which, given that I'm a lisp-slinging, `bash`-toting Emacs user, almost certainly means something different to me than it does to you. I'm not too clear on what the end result is going to look like, and I don't want to set the expectations too high. But, as always, I'll let you know how it goes.
