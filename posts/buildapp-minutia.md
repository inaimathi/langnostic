Before I get to telling you [what I've been up to lately](https://github.com/Inaimathi/cl-notebook#cl-notebook), let me share some insight into what it takes to build Lisp executables on Windows and Linux. This is something I had to do recently at work. I wouldn't normally bother, but we still have Windows dev boxes, and it seemed like an interesting enough exercise. In no particular order, here are things you need to think about when you're putting things together with [`buildapp`](http://www.xach.com/lisp/buildapp/).

### <a name="be-careful-with-your-storage-folders"></a>Be careful with your storage folders

My usual tactic for this sort of thing is to store static files and stored files in the project directory. It's easy to do because I just get to use relative paths everywhere, and it works well enough if your distribution plan is basically `git clone https://github.com/you/whatever.git`. The only downside is that you have to run the program in the context of its project directory, or have random `static` folders pop up in random places.

For people who don't just randomly write Lisp code for fun, this isn't exactly acceptable. To do proper binary deployment for a more general audience, you need to make sure that there's a single consistent place for your program to store stuff. And if you want to be cross-platform about it, you can't just do `(defvar *storage* "~/project-name/")` and call it a day. Fortunately, [`user-homedir-pathname`](http://www.lispworks.com/documentation/HyperSpec/Body/f_user_h.htm#user-homedir-pathname) it a thing, so what I ended up doing is setting my storage directory to `(merge-pathnames ".project-name/" (user-homedir-pathname))`. However, it wasn't quite that simple.

My *other* usual tactic is to just put such config variable declarations in the appropriate `package.lisp` file, and leave it at that. Unfortunately, when I tried that with `buildapp`, the first teammate who tried it responded with

Why is this complaining that "C:\Users\Inaimathi\2dmacs\" doesn't exist?

The problem is that `buildapp` takes your image and binary and ships it, including anything you did at compile or config time. What I really wanted was for my binary to check *at runtime* what the users' home directory is and use that as the starting point for its own storage directory. The way I ended up doing that, though I'm sure others are possible, was to define the appropriate variables where I'd usually put them, then set them again in the entry-point function. Mine happened to be called `main`, because why not, so what you'd see if you looked into the appropriate files is<a name="note-Sat-Apr-05-175204EDT-2014"></a>[|1|](#foot-Sat-Apr-05-175204EDT-2014)

```lisp
;;; package.lisp

...

(defparameter *storage* nil)

...
```

```lisp
;;; start.lisp

...

(defun main (&optional argv)
  ...
  (setf *storage (merge-pathnames ".2dmacs/" (user-homedir-pathname)))
  ...
  )
```

That lets the appropriate functions access storage files, but makes sure that `*storage*` will ultimately be set to the appropriate directory on the users' machine rather than yours.

### <a name="native-sbcl-shell-sucks-balls-under-windows"></a>Native SBCL shell sucks balls under Windows

We had a couple problems deploying a specific part of the system that dealt with running external programs. And like the title says, that was balls on Windows. Not that I expected it to run perfectly across operating systems, but it turned out to be particularly thorny. First off, `external-program` doesn't do what we wanted. It kinda does, but doesn't seem to let you have a hook in to search the users' environment variables for program locations. Luckily, we're using SBCL, and its native `sb-ext:run-program` *does* give you this facility in the form of the `search` keyword argument. Also, for some reason SBCL handles streams really oddly in Windows? Not sure why, but the end result was that I couldn't capture `*standard-output*` in the way I wanted to in one part of the code. We ended up dealing with that by calling `sb-ext:run-program` with `:output :stream`, then collecting the result through `sb-ext:process-output` instead of direct stream capture. Cool, I guess. It *seemed* to work in testing. But the particular shell command we were using started seizing up once we hit a particular corpus size<a name="note-Sat-Apr-05-175223EDT-2014"></a>[|2|](#foot-Sat-Apr-05-175223EDT-2014). The solution we ended up taking was using `uiop:run-program` instead. It deals with streams the way I was expecting *and* handles all shell commands we've thrown at it so far very snappily.
 
### <a name="finding-dependencies-is-nontrivial"></a>Finding dependencies is ... non-trivial

`buildapp` doesn't load dependencies automatically. It expects you to pass it a bunch of `--load-system` command line arguments for things you want included in the final binary. The first thing I did was check the docs. There didn't seem to be a `dependency-tree` call anywhere, so I wrote [this one](http://stackoverflow.com/a/22732580/190887). On a whim though, I dropped by `#lisp`, where I got the opportunity to ask [">Zach](<a href="http://xach.com/) about it<a name="note-Sat-Apr-05-175227EDT-2014"></a>[|3|](#foot-Sat-Apr-05-175227EDT-2014). It turns out that what I wrote is really only going to get you the dependencies declared in `asdf` files, and there are apparently enough people out there who hook manual `load` or `ql:quickload` statements in through odd, ad-hoc ways that he doesn't go that route. Turns out that what he does is `asdf:load-system`/`ql:quickload` the thing he wants to build, then runs `ql:write-asdf-manifest-file` to find the list of all systems included in the image after everything relevant has been loaded. The `buildapp` call you make after that needs to look something like `buildapp --asdf-manifest wherever/you/had/ql:write-manifest-file/output.txt --load-system foo --load-system bar ... --entry your-entry-fn --output your-program-name`. Mildly annoying, but at least it'll get you what you need.

* * *
##### Footnotes

1 - <a name="foot-Sat-Apr-05-175204EDT-2014"></a>[|back|](#note-Sat-Apr-05-175204EDT-2014) - Ok, it's mildly more complicated than this too, because I wanted to deal with some command line arguments, and I wanted some static files to be *optionally* re-generated at each program load. You can still see the basic principle.

2 - <a name="foot-Sat-Apr-05-175223EDT-2014"></a>[|back|](#note-Sat-Apr-05-175223EDT-2014) - If you must know, it was `git ls-tree`, and the call started hanging around the time that we committed the ~third file into the target repository.

3 - <a name="foot-Sat-Apr-05-175227EDT-2014"></a>[|back|](#note-Sat-Apr-05-175227EDT-2014) - Here's the transcript, for those of you who want the details:

```
13:58 &lt;inaimathi> Anyone here who knows things about asdf
                  dependencies, and is willing to answer
                  questions/lend an eye?
13:58 &lt;Xach> inaimathi: I know a bit. What's up?
13:59 &lt;inaimathi> I'm trying to find the dependency tree
                  (preferably ordered) of a given asdf system.
13:59 &lt;inaimathi> Is there a build-in way of doing that?
13:59 &lt;inaimathi> *built
13:59 &lt;inaimathi> Hm
14:00 &lt;inaimathi> Actually, you might be able to help with the
                  larger problem too. The real problem is that I'm
                  trying to run buildapp for a project, and I want
                  to know what systems I need to load as part of
                  the shell command.
14:00 &lt;Xach> oh. the way i do that is to load the project once with
             dependencies downloaded automatically and then note
             what was loaded.
14:00 &lt;Xach> then i load it again with just those things.
14:01 &lt;Xach> it is not great but arbitrary things load during
             find-system time so i'm not sure if there's a nice way
             around it.
14:01 &lt;inaimathi> How do I go about doing that?
14:02 &lt;inaimathi> That is, loading a project while getting output
                  of what's being loaded. Is there a ql flag or
                  something?
14:03 &lt;Xach> inaimathi: i don't actually note the specifics. i just
             load it, and then have quicklisp dump out an index to
             the currently installed libraries via
             (ql:write-asdf-manifest-file
             "/path/to/my/project/system-index.txt")
14:03 &lt;inaimathi> Ah
14:03 &lt;Xach> then i use buildapp --asdf-manifest system-index.txt
             --&lt;the rest of the stuff>
14:04 &lt;Xach> Each time I make a new project I refine the makefile
             technique a little more
14:05 &lt;inaimathi> Ok then. Do you think something like
                  http://stackoverflow.com/a/22732580/190887 could
                  be a valid approach, or is that going to miss
                  things?
14:07 &lt;Xach> inaimathi: One difficulty arises from .asd files with
             things like (eval-when ... (asdf:load-system
             "some-prerequisite"))
14:07 &lt;inaimathi> Right; those wouldn't be noted by the asdf system
                  itself.
14:07 &lt;inaimathi> Dammit.
14:08 &lt;Xach> I also don't know if the slot you're looking at
             includes :defsystem-depends-on dependencies.
14:08 &lt;Xach> inaimathi: I once asked about how to do this on
             asdf-devel and I got an answer I didn't really
             understand (it was complicated) and I haven't
             revisited it. And I'm not sure there's an archive you
             can search for it.
14:09 &lt;rtoym> asdf-devel is on gmane.org
14:10 &lt;inaimathi> Hm. I'll take the asdf-manifest-file approach for
                  the moment, and probably ask around on asdf-devel
                  later.
14:10 &lt;inaimathi> Thanks!

```
