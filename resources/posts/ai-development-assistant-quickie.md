I'm still working on that drivethru module. Tentative progress up [here](https://github.com/inaimathi/clj-drivethru), but I won't be writing about it until it's at least theoretically runnable on a piece of hardware attached to a microphone. In the meantime though, I was thinking about what it would take to actually use these AI tools in the course of my everyday development workflow. And because I am who I am, my first reflex was an `emacs` mode. Which is why [this](https://github.com/inaimathi/machine-setup/blob/master/emacs/aidev.el) is now a thing.

It took me a bit of googling, installing the [`emacs-request`](https://github.com/tkf/emacs-request) library, and a fairly trivial amount of modification over [`trivial-openai`](https://github.com/inaimathi/trivial-openai) to come up with the first bit.

```emacs-lisp
(defun aidev-first-message-content (response)
  (cdr (assoc 'content (cdr (assoc 'message (aref (cdr (assoc 'choices response)) 0))))))

(defun aidev--chat (messages on-success)
  (request "https://api.openai.com/v1/chat/completions"
    :type "POST"
    :data (json-encode `(("messages" . ,messages) ("model" . "gpt-3.5-turbo")))
    :headers `(("Content-Type" . "application/json")
	       ("Authorization" . ,(concat "Bearer " (getenv "OPENAI_API_KEY"))))
    :parser 'json-read
    :error (cl-function
	    (lambda (&rest args &key error-thrown &allow-other-keys)
	      (message "Got error: %S" error-thrown)))
    :success on-success))
```

And an even smaller amount of googling[^specifically-about-interactive] to write the next part:

[^specifically-about-interactive]: Specifically, the googling was trying to remember what I had to pass to `interactive` in order to make a function I define a callable command that prompts the user for some input.

```
(defun aidev-insert-chat (prompt)
  (interactive "sPrompt: ")
  (aidev--chat
   `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
     (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
     (("role" . "system") ("content" . "Provide the requested runnable code and no other commentary. If you _absolutely must_ explain something about the code you return, please do so in block-comments preceding or following the code, and make sure they are in the appropriate comment syntax for the specified major mode."))
     (("role" . "user") ("content" . ,prompt)))
   (cl-function
    (lambda (&key data &allow-other-keys)
      (insert (aidev-first-message-content data))))))
```

Gotta love those English-language prompts. Now, the next thing I'd probably want to have access to, is the ability to request a refactoring of an existing code block. And I _was_ going to take a few minutes to remember how to deal with `emacs` regions when a proverbial lightbulb went off over my head.

```
M-x aidev-insert-chat
RET
A function that takes the current region, and calls a function named `aidev--chat` in order to get a refactored version and then replace the region with the result. You don't need to implement the aidev--chat function; I already have that.
```

After a moment passes for the requisite network hit, I see

```
Sure! Here's a function that takes the current region, calls the `aidev--chat` function to get a refactored version, and replaces the region with the result:

'''emacs-lisp
(defun refactor-region-with-aidev-chat ()
  "Refactors the current region using `aidev--chat` function."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (when (use-region-p)
      (let ((refactored (aidev--chat (buffer-substring-no-properties start end))))
        (delete-region start end)
        (goto-char start)
        (insert refactored)))))
'''

To use this function, you can bind it to a key combination by adding the following line to your Emacs configuration file (`~/.emacs` or `~/.emacs.d/init.el`):

'''emacs-lisp
(global-set-key (kbd "C-c r") #'refactor-region-with-aidev-chat)
'''

This example binds the `refactor-region-with-aidev-chat` function to the key combination `C-c r`. You can customize the key combination to your preference.

Now, when you have a region selected in a buffer, you can invoke the function by pressing the configured key combination. It will call the `aidev--chat` function with the selected region as input, and replace the region with the resulting refactored code.

Note: Replace `aidev--chat` with the actual name of your function.
```

drop into my buffer.

Which is... cute. I see that the system completely ignored the prompt to make the code runnable, which is fine I guess, and it also included some basic instructions on how to bind the resulting function to a keystroke[^and-also-possibly-made-sure]. I vaguely recall there being some way of passing instructions to `interactive` to make it take `region` as an input, but I'm not about to knock points off for that. This way of writing the function took me all of two seconds (to type the prompt specifying what I want), as opposed to the between 10 and 30 minutes it would have taken searching and poring over various Emacs documentation, StackOverflow and [Xah Lee's ancient blog](http://xahlee.info/emacs/emacs/elisp_interactive_form.html) before I figured out what I needed.

[^and-also-possibly-made-sure]: And also possibly made sure that the target keystroke was unassigned in the default keymap, and made sure that it aligned mnemonicaly? Like `C-c r` is probably what I would have chosen as the keybinding for a function whose name starts with `r`, and it does happen to be free in my local map. I don't know if this is a coincidence or not.

Also, it doesn't _quite_ call `aidev--chat` with the correct input, but it's not exactly bad either. A minor refactoring, possibly the last one I'll ever have to write, brings it to:

```emacs-lisp
(defun aidev-refactor-region-with-chat (prompt)
  "Refactors the current region using `aidev--chat` function and a prompt."
  (interactive "sPrompt: ")
  (when (use-region-p)
    (aidev--chat
     `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
       (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
       (("role" . "system") ("content" . "The user wants you to help them refactor a piece of code they've already written. Unless specified by their prompt, you should output code in the same language as the input code. Output absolutely nothing but code; the message you return should be a drop-in replacement for the code the user needs help with."))
       (("role" . "user") ("content" . ,prompt))
       (("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end)))))
     (cl-function
      (lambda (&key data &allow-other-keys)
	(let ((reg-start (region-beginning))
	      (reg-end (region-end)))
	  (goto-char reg-start)
          (delete-region reg-start reg-end)
          (insert (aidev-first-message-content data))))))))
```

Also, I guess I might as well remove all the prompts from the previous piece trying to get it to emit comments[^although-I-do-have], since they seem to be doing fuck all to help the situation:

[^although-I-do-have]: Although I do have some ideas in terms of what to do about that which I'll try out a bit later.

```emacs-lisp
(defun aidev-insert-chat (prompt)
  (interactive "sPrompt: ")
  (aidev--chat
   `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
     (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
     (("role" . "user") ("content" . ,prompt)))
   (cl-function
    (lambda (&key data &allow-other-keys)
      (insert (aidev-first-message-content data))))))
```

Ok, I need a piece of test code.

```
M-x aidev-insert-chat
RET
The anaphoric if macro written in emacs lisp
```

```
Here's an implementation of the anaphoric if macro in Emacs Lisp:

'''elisp
(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if macro.
If TEST-FORM evaluates to non-nil, evaluates THEN-FORM with the value of TEST-FORM
bound to 'it'. If TEST-FORM evaluates to nil and ELSE-FORM is provided, evaluates
ELSE-FORM. If both THEN-FORM and ELSE-FORM are omitted, returns the value of TEST-FORM."
  (let ((it-symbol (make-symbol "it")))
    `(let ((,it-symbol ,test-form))
       (if ,it-symbol
           (let ((it ,it-symbol))
             ,then-form)
         ,else-form))))

;; Usage examples:
(aif 10 (* it 2)) ; Returns 20

(aif nil (* it 2) "not found") ; Returns "not found"

(aif (gethash "key" my-hash) it "key not found")
'''

Explanation:
The anaphoric if macro, `aif`, takes three arguments: `test-form`, `then-form`, and an optional `else-form`. Inside the macro, we create a symbol `it` and bind the value of `test-form` to it using `let`. If `test-form` evaluates to non-nil, we bind the value of `it` to `test-form` and evaluate `then-form`. If `test-form` evaluates to nil and `else-form` is provided, we evaluate `else-form`. If both `then-form` and `else-form` are omitted, the value of `test-form` is returned.

Please note that this implementation assumes that the user is working in Emacs Lisp.
```

Which is pretty good, all things considered. Selecting the appropriate subregion of that, and calling

```
M-x aidev-refactor-region-with-chat
RET
Use gensym instead of make-symbol
```

produces the result...

drumroll please...

```emacs-lisp
(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if macro.
If TEST-FORM evaluates to non-nil, evaluates THEN-FORM with the value of TEST-FORM
bound to 'it'. If TEST-FORM evaluates to nil and ELSE-FORM is provided, evaluates
ELSE-FORM. If both THEN-FORM and ELSE-FORM are omitted, returns the value of TEST-FORM."
  (let ((it-symbol (gensym "it")))
    `(let ((,it-symbol ,test-form))
       (if ,it-symbol
           (let ((it ,it-symbol))
             ,then-form)
         ,else-form))))
```

Which is exactly what I asked for. Granted, this is a minor refactoring, and I strongly suspect more complicated ones will increase the odds of weird effects, but still. It's not bad. Let me try one more, this time sort of quasi-realistic refactoring that I sometimes need to put together.

```emacs-lisp
(mapc (lambda (n) (message (format "Number: %d" (+ n 3)))) (list 1 2 3 4))
```

```
M-x aidev-refactor-region-with-chat
RET
Instead of just messaging the number, it should also message the index of the number in the incoming array
```

outputs

```
Here's a refactored version of your code that also includes the index of each number in the incoming array:

'''emacs-lisp
(mapc (lambda (pair) (message (format "Index: %d, Number: %d" (car pair) (+ (cdr pair) 3))))
      (cl-loop for n in (list 1 2 3 4)
               for i from 0
               collect (cons i n)))
'''

In this version, I've used `cl-loop` to iterate over the numbers in the incoming list while keeping track of the corresponding index. Each pair of index and number is then passed to the lambda function, which formats and messages them.
```

So, I mean, yeah. Between the fact that it ignored the prompt directing it to output _only_ code this time, and that it hilariously used `cl-loop` internally, but not at the top-level, replacing `mapc`, this isn't exactly the _most_ effective code, but it actually does what's asked, which is pretty nifty.

Ok, one more parting shot:

```
M-x aidev-refactor-region-with-chat
RET
Use cl-loop at the top-level instead of mapc with an inner cl-loop
```

```
(cl-loop for pair in (cl-loop for n in (list 1 2 3 4)
                               for i from 0
                               collect (cons i n))
         do (message (format "Index: %d, Number: %d" (car pair) (+ (cdr pair) 3))))
```

:|

Close enough.

I'm planning to do some more UI work on this, and possibly some more exploration regarding the prompt engineering involved in getting decent responses here. As always, I'll let you know how it goes.
