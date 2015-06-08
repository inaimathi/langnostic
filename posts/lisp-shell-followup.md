So, I may have to backtrack on what I was saying earlier. Specifically, I called `clisp` a toy shell, and I called the machine I'm currently typing this on a toy machine. I did this because, having just installed it and spent a grand total of five minutes poking around, I assumed


- it wouldn't run some programs properly
- my scripts would now be useless
- cd wouldn't work
- I'd lose tab completion on files
- there would be no gains to offset all the losses
- it would be a pain in the ass to use a regular shell when I hit the limits of `clisp`


It turns out that most of those don't apply. I did actually lose tab-completion when working with files, but that's it. Pretty much every program that I want to run typically<a name="note-Sun-Jan-29-015617EST-2012"></a>[|1|](#foot-Sun-Jan-29-015617EST-2012) works just as well from `clisp` as it does in `bash`, scripts run exactly the same as under a standard shell when you use `run-shell-command`, `cd` is actually a function defined in `clisp`s' `cl-user`, and when I need to run a regular bash for whatever reason `eshell` can pickup the slack.

There's also a few non-obvious things I gain to offset losing filename tab completion.

First off, I get to define helper functions at my command line. One situation I've already found this useful in is copying files off my previous computer. It's a fairly specific situation, because I didn't want to sync a complete directory, but rather surgically copy over some 12 or 13 irregularly named files. That would have taken 12 or 13 separate scp calls. In regular shell, I'd have to do something like write a script for it. Having an actual language available let me pull out my first trick

```lisp
> (defun cp-file (file-name) 
    (run-shell-command (format nil "scp inaimathi@other-machine:.emacs.d/~a .emacs.d/")))

CP-FILE

> (cp-file "example.el")
```

This isn't specific to `clisp`, obviously. I assume that [any](http://rbjl.net/43-use-fresh-ruby-as-your-shell) language [shell](http://stackoverflow.com/questions/209470/can-i-use-python-as-a-bash-replacement) you use could pull the same trick. Still, having the ability to define helpers on the fly is something I occasionally wish I had<a name="note-Sun-Jan-29-015814EST-2012"></a>[|2|](#foot-Sun-Jan-29-015814EST-2012).

Another thing that I imagine would work in any language shell, is an easier way of defining shell scripts. I wrote a [little set of ui utilities](https://github.com/Inaimathi/shell-ui) a while ago, one of which is `pack`, a translator for various archive formats so that I can write `pack foo` rather than `tar -xyzomgwtfbbq foo.tar.gz foo`

```ruby
#!/usr/bin/ruby

require 'optparse'
require 'pp'
require 'fileutils'

archive_types = {
  "tar" => ["tar", "-cvf"],
  "tar.gz" => ["tar", "-zcvf"],
  "tgz" => ["tar", "-zcvf"],
  "tar.bz2" => ["tar", "-jcvf"],
  "zip" => ["zip"]
}

########## parsing inputs
options = { :type => "tar", :excluded => [".git", ".gitignore", "*~"] }
optparse = OptionParser.new do|opts|
  opts.on("-e", "--exclude a,b,c", Array,
          "Specify things to ignore. Defaults to [#{options[:excluded].join ", "}]") do |e|
    options[:excluded] = e
  end
  opts.on("-t", "--type FILE-TYPE",
          "Specify archive type to make. Defaults to '#{options[:type]}'. Supported types: #{archive_types.keys.join ", "}") do |t|
    options[:type] = t
  end
end
optparse.parse!
##########

ARGV.each do |target|
  if not archive_types[options[:type]]
    puts "Supported types are #{archive_types.keys.join ", "}"
    exit
  elsif options[:type] == "zip"
    exclude = options[:excluded].map{|d| ["-x", d]}.flatten
  else
    exclude = options[:excluded].map{|d| ["--exclude", d]}.flatten
  end
  fname = target.sub(/\\/$/, "")
  args = archive_types[options[:type]] +
    [fname + "." + options[:type], fname] +
    exclude
  system(*args)
end
```

So that was necessary in bash, and because shell scripts can't easily share data, the companion script, `unpack`, had to define almost the exact same set of file-extension-to-command/option mappings<a name="note-Sun-Jan-29-015954EST-2012"></a>[|3|](#foot-Sun-Jan-29-015954EST-2012). If I'm using `clisp`, I could instead write

```lisp
(defun pack (file-name &key (type tar) (exclude '(".git" ".gitignore" "*~"))) 
  (pack-file (make-instance type :file-name file-name :excluded exclude)))

(defmethod pack-file ((f tar.gz))
  (run-shell-command (format nil "tar -zcvf ~@[~{--exclude ~a~^~}~]~a" 
                             (excluded f) (file-name f))))
```

and be done with it<a name="note-Sun-Jan-29-020048EST-2012"></a>[|4|](#foot-Sun-Jan-29-020048EST-2012). This is a similar, but more extreme version of the previous point. Instead of writing shell-scripts, I can now write functions, macros or methods. These are smaller conceptual units and deal with inputs more easily, letting me focus on expressing what I want the script to do. In fact looking at language shells this way makes it obvious that things like `optparse` are just hacks to get around the way that scripts accept arguments.

The last cool thing is to do with the package management. I could be wrong about this, but I don't think the Lisp notion of `in-package` exists elsewhere. So I can define a package like

```lisp
(defpackage :apt-get (:use :cl))

(defun install (&rest packages)
  (su-cmd "apt-get install ~{~(~a~)~^ ~}" packages))

(defun update () 
  (su-cmd "apt-get update"))

(defun search (search-string) 
  (cmd "apt-cache search '~a'" search-string))
```

where the `cmd`s are defined as something like

```lisp
(defmacro cmd (command &rest args)
  `(run-shell-command 
    (if args `(format nil ,command ,@args) `command)))

(defmacro su-cmd (command &rest args)
  `(run-shell-command 
    (format nil "su -c \\"~a\\""
            (if args `(format nil ,command ,@args) `command))))
```

The issue I'd have with defining these in, for example a Python shell, is that I'd then have a choice. I could either `import` the file and put up with typing out the name of the module at every invocation, or I could `import install, update, search from` and then hope that I don't have to define conflicting functions<a name="note-Sun-Jan-29-020437EST-2012"></a>[|5|](#foot-Sun-Jan-29-020437EST-2012). In a Lisp shell, I can define it and load it and then do `(in-package :apt-get)` when I need to do a series of commands relating to installing new modules.

Now all of these, clisp-exclusive or not, are small syntactic fixes that work around basic shell annoyances. To the point that you're probably asking yourself what the big deal is. It's basically the same reason that macros are awesome; they get rid of inconsistencies at the most basic levels of your code, and the increased simplicity you get that way has noticeable impacts further up the abstraction ladder. The sorts of things that look like minor annoyances can add up to some pretty hairy code, and cutting it off at the root often saves you more trouble than you'd think.

I'll admit that tab completion on file names is a pretty big thing to lose<a name="note-Sun-Jan-29-020554EST-2012"></a>[|6|](#foot-Sun-Jan-29-020554EST-2012), but the things I outline above are mighty tempting productivity boosts to my shell. To the point that I'm fairly seriously debating switching over on my main machine. Between Emacs, StumpWM/Xmonad and Conkeror, it's not really as if someone else can productively use my laptop anyway. Adding an esoteric shell really doesn't seem like it would be a big negative at this point.


* * *
##### Footnotes

1 - <a name="foot-Sun-Jan-29-015617EST-2012"></a>[|back|](#note-Sun-Jan-29-015617EST-2012) - Including fairly complex CLI stuff like `wicd-curses`, `mplayer` and `rsync --progress`

2 - <a name="foot-Sun-Jan-29-015814EST-2012"></a>[|back|](#note-Sun-Jan-29-015814EST-2012) - And now, I do

3 - <a name="foot-Sun-Jan-29-015954EST-2012"></a>[|back|](#note-Sun-Jan-29-015954EST-2012) - Except for compression rather than expansion

4 - <a name="foot-Sun-Jan-29-020048EST-2012"></a>[|back|](#note-Sun-Jan-29-020048EST-2012) - Defining methods for each archive type, and the appropriate class, obviously

5 - <a name="foot-Sun-Jan-29-020437EST-2012"></a>[|back|](#note-Sun-Jan-29-020437EST-2012) - Or import another module that defines new ones with the same names

6 - <a name="foot-Sun-Jan-29-020554EST-2012"></a>[|back|](#note-Sun-Jan-29-020554EST-2012) - And I'm going to put a bit of research into not losing it
