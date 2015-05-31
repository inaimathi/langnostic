It's been a while since I touched it, so I figured I'd download the Erlang reference and language implementation and play around with it. This is also the first time I've played with it while being an admitted Emacs user though (if you go far enough back in the my blog archive, you'll note that my initial reaction to Emacs was something along the lines of "My eyes! The goggles do NOTHING!" [runs away]).

Sounds like a good time to get Erlang mode running inside Emacs rather than having to muck around with erl in the terminal. A search for ["erlang mode emacs"](http://lmgtfy.com/?q=erlang+mode+emacs) links me to the Erlang/OTP site where I found the [documentation](http://ftp.sunet.se/pub/lang/erlang/doc/reference_manual/users_guide.html) earlier. Turns out that Erlang comes with [its own Emacs mode](http://ftp.sunet.se/pub/lang/erlang/doc/apps/tools/erlang_mode_chapter.html). I guess the guys and gals over at Ericsson labs are all Emacs hackers too. (As an aside, I also found one post from a user who highly recommended the Erlang emacs mode, even to users of other editors. His advice ran something like "What you'll want to do is type $> vim ~/.emacs and edit that file to include your Erlang path". It was chuckle-worthy, at least.)

Anyway, it turns out that all you really need to do is install Erlang, then add "/usr/lib/erlang/lib/tools-[version number]/emacs" to your load path, and 

```emacs-lisp
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)
```

elsewhere in your .emacs file. note that you'll need to change those directories based on where you installed Erlang (the above are what I had to do after running apt-get install erlang. If you did yours manually, it'll be different).

Once that's done, M-x erlang-mode gets you the right mode, and M-x erlang-shell gets the current buffer running erl.
