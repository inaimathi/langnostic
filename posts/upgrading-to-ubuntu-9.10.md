So I've upgraded to [ubuntu 9.10](http://www.ubuntu.com/). They've put a lot of effort into making the UI more consistant, which is nice I guess. Also, it's shaved off a further four seconds from my startup routine; the system itself now starts up about 2 seconds faster, and they changed the login screen so that I can just hit `<ret>` instead of typing in my user name (which saved the other two seconds).

That's really not why I finally decided to ugrade though [this is](http://web.psung.name/emacs/setup.html). Ubuntu 9.20 has `apt-get` support for `emacs23`, and I really didn't feel like going through a manual install from their [ftp site](http://ftp.gnu.org/pub/gnu/emacs/). That seems like something I'd have had to do in 1998, not 12 years after the fact.

A few things were obsoleted, but nothing important. Though, you may wanna take that with a grain of salt. Like I said last time, I basically live in Emacs, Gimp/Inkscape and Chrome now. Ok, and every once in a while I'll drop into DrScheme for the macro-stepper, Klavaro for keyboard practice or into terminal because Emacs' GIT mode doesn't provide `git-init` or `git-pull` [^funny-story].

[^funny-story]: Funny story, I actually got Emacs 23 because it supports tab completion when executing shell commands, which would let me write my own little add-ons for those two `git` commands to obviate the terminal entirely.

Anyway, my point is: the list of applications that I use is short. None of the above have broken, but if you use other programs, check whether they're still supported. One I was surprised to see obsoleted was `gcc4.3`, so it doesn't hurt to check.

My `.emacs` is getting fatter, by the way. Here's the latest evolution:

```emacs-lisp
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq load-path (append load-path (list "~/emods" "~/emods/slime" "/usr/lib/erlang/lib/tools-2.6.2/emacs" "/usr/share/doc/git-core/contrib/emacs")))

(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

(setq x-select-enable-clipboard t)

(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

(require 'htmlize)
(require 'blog-mode)
(require 'quack)
(require 'erlang-start)
(require 'slime)
(require 'redo)
(require 'php-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode) '("\\.ss$" . scheme-mode))

(add-hook 'php-mode-hook
          (lambda () (define-key php-mode-map (kbd "<tab>") (lambda () (interactive) (insert-char 9 1)))))

(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode `right)

(global-set-key (kbd "<f5>") 'eval-buffer)
(global-set-key (kbd "<f7>") 'call-last-kbd-macro)

(global-set-key (kbd "C-w") (lambda () (interactive) (kill-buffer nil)))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-y") 'redo)

(defun other-window-backward (&optional n)
  (interactive "p")
  (other-window (- (or n 1))))

(global-set-key (kbd "C-n") 'other-window)
(global-set-key (kbd "C-S-n") 'other-window-backward)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

(tool-bar-mode nil)
(menu-bar-mode 0)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(htmlize-output-type (quote inline-css))
 '(iswitchb-mode t)
 '(show-paren-mode t)
 '(transient-mark-mode t))
(put 'downcase-region 'disabled nil)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
```

Changes from last time:


1.`(tool-bar-mode nil)` and `(menu-bar-mode 0)` are now in my default `.emacs` instead of just on the netbook. Yegge [suggests removing the GUI right away](http://steve.yegge.googlepages.com/effective-emacs), but it actually takes a while before you get used to using the keyboard for *everything*[^and-in-fact]. I'm far enough along that I don't need the training wheels anymore, and it gives me another 5-6 lines of editor space on screen, so they're gone.
2. `iswitchb-mode` is on by default. It adds one keystroke to the process of making a buffer not tied to a file, but it makes finding existing buffers easy enough that it's worth the trade.
3. Lots more mode-includes including `htmlize`, my own `blog-mode` (which I'm using *right now* to type this), language support for Erlang, Scheme, and Common Lisp (PHP and JavaScript have been there for quite a while), and hooks into `git-mode`.
4. `parentheses` are being `highlighted` by default. I get the feeling it was made for Lisp coding, but it's actually even more useful in JavaScript with jQuery, where a code block might look something like


```javascript
$('#something').click(function () {
   stuff = [$(this).attr('id'), $(this).attr('class'), $(this).attr('src')];
   for(i in stuff){
      clickFunction(stuff[i]);
   }});
```


In these cases, parenthesis highlighting (which also highlights [] and {}) is critical for making sure you close them all properly, and in the right order. You could probably do it by hand without help, but I wouldn't envy you that task.

[^and-in-fact]: And in fact, if you want inter-application copy/paste and you don't know enough elisp to change how the default kill/yank functions work, you *have* to use the corresponding menu items.

> EDIT: After upgrading my desktop, I went to upgrade my other machines to 9.10. The HP Mini 1035nr upgrade was not seamless. Almost, but not quite. I had to run
>
> ```shell
> sudo apt-get install --reinstall bcmwl-kernel-source
> ```
>
>to enable the wireless card.
