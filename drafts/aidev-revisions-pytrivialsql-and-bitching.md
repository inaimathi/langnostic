So I recently did some work [re-writing most of `aidev`](https://github.com/inaimathi/machine-setup/blob/master/emacs/aidev.el), as well as adding [a new mini utility to `shell-ui`](https://github.com/inaimathi/shell-ui/blob/master/python/gpt). And I did it in service of pushing a new minor library that I'll be using some upcoming [`catwalk`](https://github.com/inaimathi/catwalk) revisions. So, strap in, here's a quick tour of the changes.

## The Object Level

Ok, so the thing I'm actually working on here is [a set of SQL bindings](https://github.com/inaimathi/pytrivialsql) that I've found myself copy-pasting into three projects over the past while. It's [up at pypi](https://pypi.org/project/pytrivialsql/) after a baffling amount of security theatre[^for-some-perplexing-reason], but that's not the point.

[^for-some-perplexing-reason]: For some perplexing reason, you are now required to have 2FA set up in order to log into pypi.org with your username and password, _and_ are required to use API tokens in order to deploy your projects. The second part of this means using the username `__token__`, and your API token as the password. Also, for some reason, their accordion menus don't work on Firefox, so I have to inspect their link source tree and type URLs manually when I'm navigating around to my project pages. The net effect of all this is: it is annoying as balls to actually log in and do anything on the website, BUT, leaking your API key anywhere still automatically exposes all of your projects to hostile deployment. So like, it's much harder to use than it needs to be, and very marginally more secure than the alternative. Shrug, I guess. This is what current security trade-offs look like. I'm not fixating too hard on it because it's possible if unlikely that this _is_ the security pareto frontier, but I still don't like it.

The point is that I wanted to actually make this thing a proper, capitol P project. Which means proper linting, a test suite and some CD courtesy of `github` actions. The problem is that one of the big things the linter is telling me to fix is

```
C0116: Missing function or method docstring (missing-function-docstring)
```

I'm not a big fan of docstrings in general. They tend to get ignored and/or weirdly out of sync with the surrounding code, they're mildly annoying to write, and extremely annoying to read unless they're done remarkably well. A _much better_ strategy than depending on docstrings is keeping your functions/classes small and well named, and keeping any intent-level docs either in a README file, or possibly in module level docstrings. I recognize how crazy this position might sound coming from someone who

1. has an ["almost-literate-programming" tag](/archive/by-tag/almost-literate-programming) in his blogs' archives
2. has done pretty extensive work on [diagram compilation](/posts/the-big-problem-and-visual-compilers#constraint-propagation) and
3. has written a [notebook-style editor](https://github.com/inaimathi/cl-notebook)

I maintain that this is maximally consistent. What all of those documentation strategies have in common is that a - they're much harder to accidentally de-sync from the attached code than usual comments and docstrings, and b - they focus on a higher level of imparting insight than a specific function or class and try to cut to the _intent_ rather than _current implementation_ of the code you're reading.

But fucked if I'm gonna be dinged by my linter for disobeying the rules, and I do have [that thing I wrote a little while ago](https://github.com/inaimathi/machine-setup/blob/master/emacs/aidev.el), so why not let a robot do this for me?[^Just so we're clear]

[^Just so we're clear]: Just so we're clear, this is a stupid idea. If ChatGPT and similar are going to be standard tools in the programming world, and it looks like they are to some extent, then I'd much rather use them by feeding in code that I don't understand in order to get it explained, rather than relying on the original programmer to use them in order to generate an explanation to commit into the codebase. Mainly, this has to do with the desync issue again. If you have someone generate a docstring by saying "ChatGPT, document this for me", then you run the risk of that code changing later and the old docstring being kept around even after it's obsolete. This is especially bad because if someone else decides to do what I consider the right thing and say "ChatGPT, explain this code to me", and includes the docstring along with the code, it really seems like a stale docstring might cause an incorrect explanation rather than a more enlightening one. Ironically, I could totally see needing to _strip_ docstrings as part of this workflow.

### The Meta Level

It turns out that my `curl` SSL certs are fucked? And also, the Emacs `requests` library either bottoms out in a `curl` call or uses the same cert stack? Because I kept getting back [error 60](https://stackoverflow.com/questions/29822686/curl-error-60-ssl-certificate-unable-to-get-local-issuer-certificate) against `https://api.openai.com` when trying to call `aidev--chat`. This is complete bullshit, because I can call it just fine from Python's [`requests`](https://pypi.org/project/requests/), or by navigating there in Firefox. After spending around 30 minutes trying to diagnose this, I said "fuck it" and decided to route around the problem.

#### The Meta Meta Level

```
#! /usr/bin/python3

import requests
import json
import os
from optparse import OptionParser

API_KEY = os.environ.get("OPENAI_API_KEY")


def chat(model, messages):
    res = requests.post(
        "https://api.openai.com/v1/chat/completions",
        headers={
            "Content-Type": "application/json",
            "Authorization": f"Bearer {API_KEY}",
        },
        data=json.dumps({"messages": messages, "model": model}),
    )

    if res.status_code == 200:
        return res.json()["choices"][0]["message"]["content"]

    return res


if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option(
        "-m",
        "--model",
        dest="model",
        default="gpt-3.5-turbo",
        help="Specify the GPT model to use for chat results",
    )
    opts, args = parser.parse_args()

    print(chat(opts.model, [json.loads(msg) for msg in args]))
```

This is exactly what it looks like. I want a command line utility that I'll include with my `shell-ui` repo that lets me call into the ChatGPT API from bash. Once I `chmod +x` it and put it onto my path ...

```
inaimathi@eschaton:~$ gpt '{"role": "user", "content": "Hello!"}'
Hello! How can I assist you today?
inaimathi@eschaton:~$ gpt '{"role": "user", "content": "Hah! It totally worked! :D"}'
That's great to hear! What worked for you?
inaimathi@eschaton:~$ gpt '{"role": "user", "content": "Calling you from a python script so I can call you from Emacs so you can do my bullshit documentation work for me"}'
I'm sorry, but I'm unable to assist with your request.
```

I hope I didn't hurt its machine feelings. Anyway, with that done, I can re-write `aidev.el`

### Back to the Meta Level

```
(require 'request)

(defun aidev--chat (messages)
  (let ((cmd (format
	      "gpt %s"
	      (string-join
	       (mapcar
		(lambda (m) (shell-quote-argument (json-encode m)))
		messages)
	       " "))))
    (string-trim (shell-command-to-string cmd))))

(defun aidev-document-python-region ()
  (interactive)
  (let* ((prompt
	  `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
	    (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
	    (("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))
	    (("role" . "user") ("content" . "Write the docstring the above function. Return only the docstring and no other commentary."))))
	 (response (aidev--chat prompt)))
    (goto-char (region-beginning))
    (end-of-line)
    (newline)
    (insert response)))

(defun aidev-insert-chat (prompt)
  (interactive "sPrompt: ")
  (let ((prompt
	 `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
	   (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
	   ,@(when (region-active-p)
	       `((("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))
	   (("role" . "user") ("content" . ,prompt)))))
    (insert (aidev--chat prompt))))

(defun aidev-refactor-region-with-chat (prompt)
  "Refactors the current region using `aidev--chat` function and a prompt."
  (interactive "sPrompt: ")
  (when (use-region-p)
    (let ((data (aidev--chat
		 `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
		   (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
		   (("role" . "system") ("content" . "The user wants you to help them refactor a piece of code they've already written. Unless specified by their prompt, you should output code in the same language as the input code. Output absolutely nothing but code; the message you return should be a drop-in replacement for the code the user needs help with."))
		   (("role" . "user") ("content" . ,prompt))
		   (("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end)))))))
	  (reg-start (region-beginning))
	  (reg-end (region-end)))
      (goto-char reg-start)
      (delete-region reg-start reg-end)
      (insert (aidev-first-message-content data)))))

(defun aidev-explain-region ()
  (interactive)
  (insert
   (aidev--chat
    `((("role" . "system")
       ("content" . "You are a brilliant writer and veteran programmer, able to put concepts into a simple and straightforward context undestandeable to any reader. You also have a comprehensive understanding of all programming languages from prominent to obscure. The user is asking you to explain a block of code they are working with. Read over the code and provide the clearest explanation of what the code does, how to use it, and the natural ways in which it might be changed. Return the best answer you possibly can after thinking about it carefully."))
      (("role" . "system")
       ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
      (("role" . "user")
       ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))))

(defun aidev-explain-region-in-particular (prompt)
  (interactive "sPrompt: ")
  (insert
   (aidev--chat
    `((("role" . "system")
       ("content" . "You are a brilliant writer and veteran programmer, able to put concepts into a simple and straightforward context undestandeable to any reader. You also have a comprehensive understanding of all programming languages from prominent to obscure. The user is asking you to explain a block of code they are working with, but they have specific questions. Read over the code and provide the clearest explanation of what the code does, making sure to answer the users' specific question. Return the best answer you possibly can after thinking about it carefully."))
      (("role" . "system")
       ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
      (("role" . "user")
       ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))))

(provide 'aidev)
```

The important parts here are actually the second and third bit:

```
(defun aidev--chat (messages)
  (let ((cmd (format
	      "gpt %s"
	      (string-join
	       (mapcar
		(lambda (m) (shell-quote-argument (json-encode m)))
		messages)
	       " "))))
    (string-trim (shell-command-to-string cmd))))

(defun aidev-document-python-region ()
  (interactive)
  (let* ((prompt
	  `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
	    (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
	    (("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))
	    (("role" . "user") ("content" . "Write the docstring the above function. Return only the docstring and no other commentary."))))
	 (response (aidev--chat prompt)))
    (goto-char (region-beginning))
    (end-of-line)
    (newline)
    (insert response)))
```

The `aidev--chat` function now takes some message maps and uses the earlier shell script to actually hit OpenAI's API. This ended up unexpectedly simplifying the workflow here, because it _used_ to do the async callback thing, but can now just execute straight-line code. This is an improvement under the assumption that the API hit gets back on an interactive time scale. Which, because I'm using `gpt-3.5-turbo`, seems to actually happen.

Now that I've got this put together, I can hop back into `pytrivialsql` and grab a region like

```
...
def _where_dict_clause_to_string(k, v):
    if type(v) in {set, tuple, list}:
        val_list = ", ".join([f"'{val}'" for val in sorted(v)])
        return f"{k} IN ({val_list})", None
    if v is None:
        return f"{k} IS NULL", None
    return f"{k}=?", v
...
```

do `M-x aidev-document-python-region RET`, and see the resulting

```
def _where_dict_clause_to_string(k, v):
    """Converts a key-value pair representing a WHERE clause in a dictionary to its string representation.

    The function takes a key-value pair (k, v) and converts it to a string representation of a WHERE clause. If the value is a set, tuple, or list, it converts it to a comma-separated string of the values enclosed in single quotes, sorted in ascending order. If the value is None, it returns a string representation of "IS NULL". For any other value, it converts it to a string representation of "=". The function returns a tuple containing the string representation of the WHERE clause and a value placeholder if applicable.

    :param k: The key representing the column name in the WHERE clause.
    :param v: The value representing the value in the WHERE clause.
    :return: A tuple containing the string representation of the WHERE clause and a value placeholder if applicable.
    """
    if type(v) in {set, tuple, list}:
        val_list = ", ".join([f"'{val}'" for val in sorted(v)])
        return f"{k} IN ({val_list})", None
    if v is None:
        return f"{k} IS NULL", None
    return f"{k}=?", v
```

I've got `python-black` mode wired into my editor, so it got appropriately reformatted automatically. This is ... not an awful explanation. And, given the inputs here, I don't think someone who doesn't actually know the intent of this function could do a better job explaining it. But note that what we've got here is a technical, low level explanation that references parentheses and comma-separated lists, as opposed to a goal-level explanation that references set membership. It _does_ make correct note of the special case handling of the `None` case. I'm tempted to call this "much better than a half-assed attempt by a human programmer", and consider it a net utility gain. However, I still maintain that the right thing to do here is let the reader inspect/explain this code for themselves rather than, effectively, caching the explanation and risking stale docstrings.

You can see the results, including test suite and CD setup over at the [pytrivialsql](https://github.com/inaimathi/pytrivialsql) repo. I'll probably do a bit more work on that shortly.

As always, I'll let you know how it goes.
