So I had enough theorizing last week, and am currently putting the setup through the Compaq Test [TM].

Preliminary observations are good; now that I've fixed the minor ui annoyances pertaining to `screen` and `blog-mode`, this is a very comfortable editing environment. I don't actually have the wireless drivers installed on this machine yet, so I'll have to plug into the router later in order to *post* this piece, but it's quite snappy considering the hardware I'm actually working with<a name="note-Mon-Oct-10-162114EDT-2011"></a>[|1|](#foot-Mon-Oct-10-162114EDT-2011).

Hell, `slime` works pretty well too. Except that `paredit`<a name="note-Mon-Oct-10-162255EDT-2011"></a>[|2|](#foot-Mon-Oct-10-162255EDT-2011) seems to have it in for me in various ways. It's nothing I can't work around with some judicious re-binding, but it's extensive enough that I don't want to attempt it today.

I started with a fresh install of Debian Squeeze<a name="note-Mon-Oct-10-162750EDT-2011"></a>[|3|](#foot-Mon-Oct-10-162750EDT-2011) and basically just ran the following

```shell
## Basic dev tools
apt-get install emacs slime git-core mplayer lynx screen openssh-server gnupg
apt-get install sbcl python-setuptools ruby-full erlang

## app configuration
wget http://beta.quicklisp.org/quicklisp.lisp
su inaimathi -c "sbcl --load install.lisp"

## I. Fucking. Hate. Caps. Lock.
sed -i 's/XKBOPTIONS=""/XKBOPTIONS="ctrl:nocaps"/g' /etc/default/keyboard
/etc/init.d/console-setup reload
```

in order to get everything running the way I like. `install.lisp` contains

```lisp
(load "quicklisp.lisp")

(quicklisp-quickstart:install)
(ql:add-to-init-file)

(ql:quickload :linedit)
(linedit:install-repl)

(with-open-file (s (merge-pathnames ".sbclrc") :direction :output :if-exists :append :if-does-not-exist :create)
  (format s ";;; Check for --no-linedit command-line option.
(if (member \"--no-linedit\" sb-ext:*posix-argv* :test 'equal)
  (setf sb-ext:*posix-argv* 
        (remove \"--no-linedit\" sb-ext:*posix-argv* :test 'equal))
  (when (interactive-stream-p *terminal-io*)
    (require :sb-aclrepl)
    (ql:quickload \"linedit\")
    (funcall (intern \"INSTALL-REPL\" :linedit)
             :wrap-current t)))"))

(ql:quickload (list :drakma :cl-who :cl-ppcre :cl-fad :hunchentoot :clsql :cl-smtp :cl-base64 :ironclad :trivial-shell))
(quit)
```

Which just configures `quicklisp` and `linedit` to run whenever I start `sbcl`. After that , it was just a matter of importing my Emacs settings<a name="note-Mon-Oct-10-163306EDT-2011"></a>[|4|](#foot-Mon-Oct-10-163306EDT-2011), and `.screenrc` file. I didn't end up keeping the fancy settings I was thinking about last week, by the way. It currently contains, in its entirety

```shell
screen -t emacs emacs -nw

startup_message off

bind S split -v
bind s split
bind R remove
bind ^e screen emacs -nw
bind ^w screen webjump

markkeys "h=^b:l=^f:$=^e"
```

which is as basic as it could possibly be, except for the line that calls a program named `webjump`. That's actually a convenience script of my own devising that simulates my `conkeror` webjumps from the desktop machine. It reads

```ruby
#!/usr/bin/ruby

require 'uri'

print "Webjump: "
input = gets.chomp.split(" ", 2)

def get_url(input)
  jump = input[0]
  query = URI.escape(input[1])
  jumps = {
    "youtube" => "http://www.youtube.com/results?search_query=#{query}\&aq=f",
    "stockxchange" => "http://www.sxc.hu/browse.phtml?f=search\&txt=#{query}\&w=1\&x=0\&y=0",
    "google" => "http://www.google.com/search?q=#{query}\&ie=utf-8\&oe=utf-8\&aq=t",
    "wikipedia" => "http://en.wikipedia.org/wiki/Special:Search?search=#{query}\&sourceid=Mozilla-search",
    "gmail" => "http://mail.google.com"  
  }
  jumps[jumps.keys.find{|k| k =~ /#{jump}/}]
end

url = get_url(input)
if url
  system("lynx", url)
else
  puts "Can't find webjump '#{input[0]}'"
end
```

which is quite useful when I need to search for something quickly. I'm thinking about changing it such that it just takes a command-line option for which webjump to use so that I could actually keybind `google-search` as opposed to `webjump` (I've observed that "go something something" is used far more commonly than any of the others).

Like I said, that's it. It's an extremely minimal system, and it doesn't have any kind of multi-monitor support, but it gives me the important little comforts I've been used to (like tabbing between applications and convenient, keyboard-based browsing) without the need to start up an instance of X<a name="note-Mon-Oct-10-164445EDT-2011"></a>[|5|](#foot-Mon-Oct-10-164445EDT-2011). That greatly increases the universe of useable machines for me.

The only things I'm still missing:

- a klavaro-analogue *I still have no way of practicing typing from the command line (which is kind of ironic)*
- more shell-friendly bindings for `paredit`
- multi-monitor support *which I have no idea where to even start on*

* * *
##### Footnotes

1 - <a name="foot-Mon-Oct-10-162114EDT-2011"></a>[|back|](#note-Mon-Oct-10-162114EDT-2011) - An old Presario R3000 with a 1.4ghz processor and 256MB ram.

2 - <a name="foot-Mon-Oct-10-162255EDT-2011"></a>[|back|](#note-Mon-Oct-10-162255EDT-2011) - Which I've installed, and actually gotten to like under X, at the recommendation of a friend from the Toronto Lisp User Group. It's actually fantastic, but there are various key that just barf when you try using it from terminal. The default bindings for `slurp`, `barf`, `forward` and `back` s-exp operations are outright ignored, and it does something funky to my home and end keys so that they insert odd square-bracket escape sequence instead of doing what they say on the key. It's `paredit` because, all of the above works just fine in other modes.

3 - <a name="foot-Mon-Oct-10-162750EDT-2011"></a>[|back|](#note-Mon-Oct-10-162750EDT-2011) - Since the Compaq still had a copy of Parabola running from last time.

4 - <a name="foot-Mon-Oct-10-163306EDT-2011"></a>[|back|](#note-Mon-Oct-10-163306EDT-2011) - Including the steadily-growing `blog-mode`, which I've added several functions to since I started writing this piece.

5 - <a name="foot-Mon-Oct-10-164445EDT-2011"></a>[|back|](#note-Mon-Oct-10-164445EDT-2011) - Also, conveniently, lynx doesn't let me waste any time on Reddit, since I can't actually post or upvote from it.
