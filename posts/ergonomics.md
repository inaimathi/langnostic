'(I got something new.

![](/static/img/tray.jpg)

Pretty good as far as keyboard trays go, but I still find myself wanting one of those squishy wrist-rest pads. The default wrist bit on this tray isn't *terribly* uncomfortable, but it doesn't quite compress right. And for all the awesome work PFU did on [this little beast](http://www.fentek-ind.com/happyhacking.htm) in terms of key placement/spacing/spring system, they didn't put any effort at all into making the wrist comfortable. That's the one thing I miss from my [old behemoth](http://www.microsoft.com/hardware/mouseandkeyboard/productdetails.aspx?pid=043) (which is now comfortably at the office). I still prefer the HHL2 for speed; the keys are actually spaced close enough together (and consistently sized enough) that I get a fairly significant boost just from using it (on average 5 wpm). 

It's a +5 Keyboard of Speed. Together with my +1 Trackball of Accuracy and +2 Monitors of Insight, as well as the Hard Drive of Premonition, my home machine is now *significantly* better, in all ways, than my work setup.

Ahem.

There's this little slide-out sub-tray for the mouse, but since I have a tiny keyboard and a trackball, both of my peripherals fit comfortably on one half of the main area. I could use it for drinks I guess.

The only complaint I have is that Fellowes are fucking liars. In fact, *anyone* that tells you their keyboard tray has "easy installation" is a fucking liar, and you can tell them I said so. It was a pain in the ass. Granted, in this case it was worth it for the extra space and long-term comfort I got out of it, but there was significant short-term *dis*comfort.))

### <a name="define-officefollowup"></a>(define (office-follow-up)

'(Almost done configuring my dev environment for work. It's been bearable for a few days actually, so I've managed to get some work done at least, but there's still kinks here and there. The current plan is to get everything I need together in one place (and for the record, it's about twice as much as the documentation would have me believe I need), then create a bootable flash-drive with a dev-environment shell script to take care of everything other than the OS installation. I should be able to announce a dev-environment-on-a-stick (with your choice of editor) next week.))

### <a name="define-updateemacs"></a>(define (update-emacs)

'(I'm still updating my .emacs and associated files. The other day, I finally got off my ass and installed [nXhtml mode](http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html). Which I'm not using right now, incidentally, I prefer the vanilla html-mode for my everyday editing, but nXhtml provides a lot of much-needed functionality for editing PHP, JSP and mixed HTML/JS/CSS files (I try to minimize that last group, but you still run across them sometimes).

I'm also adding little convenience functions. Things like git-svn-fetch and git-svn-commit which let me seamlessley interoperate with my office-mates without giving me the giant headaches associated with Subversion use. I'm sure it's very nice for people that are used to CVS and VSS, but I *grew up with* [GIT](http://git-scm.com/) and [Mercurial](http://mercurial.selenic.com/). If you think I'll go back to centralized source control without a fight, you've got another thing coming.

Other new stuff includes in no particular order ant-build-project, restart-tomcat, stop-tomcat, and deploy-static. That last one is actually a copy of a couple ant tasks we have, which essentially just copy a whole bunch of css, js, jpg and jsp static files from our local repo to our local tomcat deployment. The thing is, the build file copies in the naive sense, whereas my emacs function uses rsync, and is therefore about ten billion times faster.

Most of that last set wouldn't have been possible without a little help from [StackOveflow](http://stackoverflow.com/). I guess no-one's really thought about it, so Emacs doesn't have a built-in sudo-shell-command. Meaning I had to write my own (which is still better than waiting for the maintainer of my IDE to patch their oversight). Here's what I came up with after consulting the overflow boards:

```emacs-lisp
(defun sudo-shell-command (command)
  (shell-command (concat "echo " (read-passwd "Password: ") " | sudo -S " command)))

(defun chain-sudo-shell-command (command-list)
  (let ((prefix (concat "echo " (read-passwd "Password: ") " | sudo -S ")))
    (mapcar (lambda (a-command)
              (shell-command (concat prefix a-command)))
            command-list)))

```

I use the first one if I can because the second one is more dependant on side-effects, and it only shows the output from the last command it recieved (so if I misspelled something in command 3 of 6 that I pass to it, I'll never know until it screws up something I'd much rather not get screwed up).)))
