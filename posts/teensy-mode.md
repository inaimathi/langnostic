Ok, this isn't one of those bullshit times where I just say "I'm going to bed" and then end up spending three hours explaining various shit at the expense of wakefulness the following day. Seriously, just one thing, I added a `teensy-mode` module to my [`emacs-utils`](https://github.com/Inaimathi/emacs-utils) project over at github. It turns out that I can't focus to save my life, but it has been damn enjoyable jumping into code so thoroughly after the set of weeks I've just been through.

The relevant bits are really just

```lisp
(defun teensy-compile-current-file ()
  (interactive)
  (shell-command (format "%s %s" teensy-compile-file-command buffer-file-name)))

(defun teensy-compile-project ()
  (interactive)
  (shell-command teensy-compile-project-command))

(defun teensy-load-hex-file ()
  (interactive)
  (let ((hex-files (directory-files (file-name-directory buffer-file-name) nil "hex$"))
        (command (format "%s -mmcu=%s -wv " teensy-loader-program teensy-processor-type)))
    (cond ((not hex-files) (message "No hex file found"))
          ((= 1 (length hex-files)) (shell-command (concat command (car hex-files))))
          (t (shell-command (concat command (completing-read "Hex File: " hex-files)))))))

(defun teensy-compile-project-load-file ()
  (interactive)
  (progn (teensy-compile-project)
         (teensy-load-hex-file)))
```

the rest of it is various Emacs boilerplate like key and customization declarations. The one interesting thing I've learned from *this* experience is that the `:options` keyword does pretty much jack shit. In a `hook` customization, you can presumably at least *see* the list, but it still doesn't constrain your choices to what's there. It might be nice to have the option, since the customization I had in mind was the chip-type of your Teensy board (a required option that needs to be correct or it'll hang your chip, so it's sort of critical to get it correct).

I'm also flaking out on actual project compilation, opting to keep the `make` command involved, which has som seriiaaah fuck this, I'm going to sleep.
