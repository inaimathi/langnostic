So apparently people are [mad about this](https://steve-yegge.medium.com/the-death-of-the-stubborn-developer-b5e8f78d326b)? Or were about a month ago and I missed it because I was too busy wiring up LLMs to do as much work as I could possibly wring out of them?

Man, I dunno, all of this seems entirely uncontroversial. The only disagreement I've got with stevey on his take is how likely and/or soon fully autonomous (or close enough to fully autonomous) agent systems are to reality. As far as I'm concerned, [this](https://sourcegraph.com/blog/the-death-of-the-junior-developer) and [this](https://steve-yegge.medium.com/the-death-of-the-stubborn-developer-b5e8f78d326b) are required reading for people with any kind of development ambition.

Starting this year, or possibly negative one year from now, if you have any ambition to be a developer on the cutting edge, your output is going to be majorly affected by how willing you are to make use of generative tools in your workflow. You don't have to like it, but this isn't a sea change anyone is likely to be able to stand against at this point. Are you currently at or above my level? Then if you reject LLM-based tools, you have a real shot of being this industry's [John Henry](https://en.wikipedia.org/wiki/John_Henry_(folklore)).

Good luck with that.

## `aidev`

I've been [wiring various LLMs](https://github.com/inaimathi/machine-setup/blob/master/emacs/aidev.el) into my workflow for a while now. The accumulated result of that effort is

```
(require 'request)

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

(defun aidev-refactor-buffer-with-chat (prompt)
  "Refactors the current buffer using `aidev--chat` function and a prompt."
  (interactive "sPrompt: ")
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
                                   (point-min) (point-max))))))
         (data (aidev--chat system prompt)))
    (delete-region (point-min) (point-max))
    (insert data)))

(defun aidev-new-buffer-from-chat (prompt)
  "Creates a new buffer with the result of a chat request."
  (interactive "sPrompt: ")
  (let* ((system
          (string-join
           (list
            "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."
            "The likeliest requests involve generating code. If you are asked to generate code, only return code, and no commentary. If you must, provide minor points and/or testing examples in the form of code comments (commented in the appropriate syntax) but no longer prose unless explicitly requested."
            (format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode))
           "\n"))
         (messages
          `(,@(when (use-region-p)
                `((("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))
            (("role" . "user") ("content" . ,prompt))))
         (result (aidev--chat system messages))
         (new-buffer (generate-new-buffer "*AI Generated Code*")))
    (with-current-buffer new-buffer
      (insert result)
      (funcall major-mode))
    (switch-to-buffer new-buffer)))

(defun aidev--chat (system messages)
  (string-trim (aidev---ollama messages system)))

(defun aidev---ollama-check-connectivity (url)
  "Check if there's a listening Ollama server at URL."
  (let* ((parsed-url (url-generic-parse-url url))
         (host (url-host parsed-url))
         (port (url-port parsed-url))
         (connected nil))
    (and host port
         (condition-case nil
             (let ((proc (make-network-process
                          :name "ollama-test"
                          :host host
                          :service port
                          :nowait t)))
               (set-process-sentinel
                proc
                (lambda (proc event)
                  (when (string-match "open" event)
                    (setq connected t))))
               (sleep-for 0.2)
               (delete-process proc)
               connected)
           (error nil)))))

(defvar aidev---ollama-default-url
  (let ((env-address (getenv "AIDEV_OLLAMA_ADDRESS")))
    (or env-address
	(and (aidev---ollama-check-connectivity "http://192.168.0.12:11434/")
             "http://192.168.0.12:11434/")
	(and (aidev---ollama-check-connectivity "http://localhost:11434/")
             "http://localhost:11434/"))))

(defun aidev---ollama (messages &optional system model)
  "Send MESSAGES to Ollama API using the generate endpoint.
MODEL defaults to \"deepseek-coder-v2:latest\".
SYSTEM is an optional system prompt."
  (unless aidev---ollama-default-url
    (signal 'ollama-url-unset nil))
  (let* ((model (or model "deepseek-coder-v2:latest"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (prompt (format "SYSTEM PROMPT: %s MESSAGES: %s"
                         (or system "")
                         (json-encode messages)))
         (url-request-data
          (json-encode
           `((prompt . ,prompt)
	     (stream . :json-false)
             (model . ,model))))
         (response-buffer
          (url-retrieve-synchronously
           (concat aidev---ollama-default-url "/api/generate")))
         response)
    (unwind-protect
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (forward-char)
          (setq response (json-read)))
      (kill-buffer response-buffer))
    (cdr (assoc 'response response))))

(defun aidev---openai (messages &optional system model)
  "Send MESSAGES to OpenAI API.
MODEL defaults to \"gpt-4-0-latest\".
SYSTEM is an optional system prompt."
  (let* ((model (or model "chatgpt-4o-latest"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (getenv "OPENAI_API_KEY")))))
         (url-request-data
          (json-encode
           `((messages . ,(if system
                              (cons `((role . "system")
                                      (content . ,system))
                                    messages)
                            messages))
             (model . ,model))))
         (response-buffer
          (url-retrieve-synchronously
           "https://api.openai.com/v1/chat/completions"))
         response)
    (message "GOT RESPONSE")
    (unwind-protect
	(with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (forward-char)
          (setq response (json-read)))
      (kill-buffer response-buffer))
    (message "RESPONSE: %s" response)
    (cdr (assoc 'content (cdr (assoc 'message (aref (cdr (assoc 'choices response)) 0)))))))

(defun aidev---claude (messages &optional system model)
  "Send MESSAGES to Claude API.
MODEL defaults to \"claude-3-5-sonnet-20240620\".
SYSTEM is an optional system prompt."
  (let* ((model (or model "claude-3-5-sonnet-20240620"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("X-Api-Key" . ,(getenv "ANTHROPIC_API_KEY"))
            ("anthropic-version" . "2023-06-01")))
         (url-request-data
          (json-encode
           (append
            `((messages . ,messages)
              (model . ,model)
              (max_tokens . 4096))
            (when system
              `((system . ,system))))))
         (response-buffer
          (url-retrieve-synchronously
           "https://api.anthropic.com/v1/messages"))
         response)
    (unwind-protect
	(with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (forward-char)
          (setq response (json-read)))
      (kill-buffer response-buffer))
    (cdr (assoc 'text (aref (cdr (assoc 'content response)) 0)))))

(defun aidev--invert-markdown-code (md-block &optional comment-start comment-end)
  (let* ((lines (split-string md-block "\n"))
         (in-code-block nil)
         (c-start (or comment-start ";; "))
         (c-end (or comment-end ""))
         result)
    (dolist (line lines)
      (if (string-match-p "^[ \t]*```" line)
          (setq in-code-block (not in-code-block))
        (push (if in-code-block
                  line
                (concat c-start line c-end))
              result)))
    (string-join (nreverse result) "\n")))

(defun aidev--strip-markdown-code (md-block)
  (replace-regexp-in-string
   "\\(?:^```[a-zA-Z-]*\\s-*\n\\|\\n?```\\s-*$\\)"
   ""
   md-block))



(provide 'aidev)
```

Yeah, yeah, it's hilarious that flattering LLMs still gives you better results for some reason. It works, and I'll keep doing it while it does. We've seen some of this code [before](/posts/making-llms-do-what-you-want-interlude). The new changes are

- There's now a `aidev-refactor-buffer-with-chat` function which is a minor shortcut to selecting the full buffer and calling `aidev-refactor-region-with-chat`.
- There's now a `aidev-new-buffer-from-chat` function which is similar to the others, except that it dumps the results into a fresh `*AI Generated Code*` buffer instead of the source of the region and prompt
- All the web requests now happen in-module rather than calling out to an external shell command

Oddly, that last one was the hardest to actually implement. Instead of calling up to whatever cloud and consuming compute that'd be metered against my subscriptions, I've been using `ollama` more and more. In particular, `deepseek-coder-v2:latest` seems like it's the equal of `claude` and `4o` for code generation purposes, and it's _blazingly_ fast running on my local GPU rig.

## The current shortfalls

My guess is that coding up the above changes took on the order of 30 minutes.

_Except_ for this part:

```
(defun aidev---ollama-check-connectivity (url)
  "Check if there's a listening Ollama server at URL."
  (let* ((parsed-url (url-generic-parse-url url))
         (host (url-host parsed-url))
         (port (url-port parsed-url))
         (connected nil))
    (and host port
         (condition-case nil
             (let ((proc (make-network-process
                          :name "ollama-test"
                          :host host
                          :service port
                          :nowait t)))
               (set-process-sentinel
                proc
                (lambda (proc event)
                  (when (string-match "open" event)
                    (setq connected t))))
               (sleep-for 0.2)
               (delete-process proc)
               connected)
           (error nil)))))
```

That is an incredibly hairy function. The problem I'm trying to solve there is that my local setup depends on a particular `ollama` target, and I want to go through a list of `ollama` servers, picking the first one that works or falling through to the next otherwise. The order is

1. check the `AIDEV_OLLAMA_ADDRESS` env var. If that's present, just trust it and don't bother doing a connection check. Possibly, this level of trust is going to come back to bite me in the ass. I'll let you know how it goes.
2. check `"http://192.168.0.12:11434/"`
3. check `"http://localhost:11434/"`
4. fail

As part of checking those URLs, I want to see if the target `ollama` server exists and is responding to requests on its port. You might think that you could do this with `url-retrieve-synchronously` naively, and you would be wrong. 

```
(url-retrieve-synchronously "http://localhost:11434/api/tags")
```

This will return if the server exists, but will hang forever if it doesn't. Which means it's not appropriate for checking which option I should go with for the value of a global variable on `emacs` startup for what are hopefully obvious reasons.

Using timeouts doesn't help either. I'm not sure why this is, but I'm guessing it has to do with how `emacs` TCP network code is wired with its' event loop.

```
(url-retrieve-synchronously "http://localhost:11434/api/tags" nil nil 1) ;; setting timeout
```

```
(with-timeout (1 nil)
  (url-retrieve-synchronously "http://localhost:11434/api/tags" nil nil 1))
```

Both of those also hang forever. Further, using any of the async equivalents, whether it's `url-retrieve` or starting our own naive manual network process, ends up either always returning a truthy value (the process/request/promise/whatever) or has the same error characteristics as the above naive approaches. So, `aidev---ollama-check-connectivity` ends up being chonky because we need to explicitly set up a `process-sentinel`, then start a `network-process`, then keep an eye on how it returns.

In practice, this point on its own ended up taking a good hour of combination prompting/doc-checking/tinkering because I had to keep restarting `emacs` in order to bail out of forever hangs. At the _current_ level of tooling, I think this is just the limit of what I can delegate to a machine. So it goes sometimes.

But you _could_ imagine the formal process that gets set up with its' own `bash` and `emacs` and get turned loose until it comes up with a function that meets the test of 

1. Returns `t` on an existing `ollama` server URL
2. Returns `nil` on a nonexistant `ollama` server URL
3. Does not crash `emacs` and return within one second

That's effectively what I was doing in concert with Claude in order to generate the above, but a robot that could do it without me is _so_ **tantalizingly** close.

I think the next portion of my personal programming time is going to be dedicated to exploring [feedback loops](https://www.lesswrong.com/posts/pZrvkZzL2JnbRgEBC/feedbackloop-first-rationality) in the learning context, and finding ways to have LLMs generate stable ones for software development. I don't expect this to be _easy_ exactly, but I expect it to be nontrivially possible and to have massive payoffs.

As always, I'll let you know how it goes.
