Ever since I was made [keenly aware of my shortcomings](/posts/game-jam), I've been reading up on a various things including [CLOS](http://cl-cookbook.sourceforge.net/clos-tutorial/index.html) and [the OO interface to CLSQL](http://clsql.b9.com/manual/ch02s02.html). Probably the best resource available right now is the [CLSQL documentation](http://clsql.b9.com/documentation.html) itself, and that's not a compliment. It's basically a (not particularly complete) reference piece that gives you a function listing and some minor explanation. As far as I can tell, it doesn't get you significantly more information than a few `describe` and `inspect` calls.

Searching for [clsql tutorial](http://www.google.ca/search?q=clsql+tutorial) brings up a Bill Clementson [column on CLSQL basics](http://bc.tech.coop/blog/040608.html) and a pretty long forum argument between Slobodan Blazeski and (among others) [Holger Schauer](http://hillview.1on.de/authors/2-Holger-Schauer), [Zach Beane](http://www.xach.com/) and [Geoff Wozniak](http://wozniak.ca/) about the seeming poor quality of a webapp tutorial by Slobodan to illustrate CLSQL in conjunction with Hunchentoot. I say "seemingly" because the original was apparently put up as a PDF on MegaUpload, then taken down in favor of being posted on Slobodan's personal site, and then taken down altogether (all of the links in that thread currently lead to 404 errors).

And that's it.

So, I figured I could put something together. At first it was going to be the standard "Hello World" of webapps (a [blog](http://docs.racket-lang.org/continue/index.html)), but then I figured, fuck it, lets make [4chan](http://www.4chan.org/).

<div class="note assumptions">
## Assumptions Note

### Things this tutorial assumes:

- **You know the very basics of Lisp, SQL and HTML** I won't bother teaching you what `(+ 1 2)` does or the basics of how the REPL works; there are [better](http://www.gigamonkeys.com/book/) [places](http://www.cs.sfu.ca/CC/310/pwfong/Lisp/1/tutorial1.html) to learn that. I also won't be teaching you how to [SELECT * FROM employees;](http://www.w3schools.com/sql/default.asp), that horse has been shot, bludgeoned, stomped on, kicked, salted and kicked again. Finally, I assume you know a thing or two about [HTML](http://www.google.ca/search?aq=f&sourceid=chrome&ie=UTF-8&q=html), if not web development proper.
- **You have a Lisp and environment installed and configured to your liking ([quicklisp](http://www.quicklisp.org/beta/) optional, but recommended) **
It can be as simple as Notepad with SBCL+[linedit](http://xach.com/lisp/linedit-screencast.gif) in a terminal, or [LispWorks](http://www.lispworks.com/downloads/index.html) or [Emacs](http://www.gnu.org/software/emacs/)+[SLIME](http://common-lisp.net/project/slime/) (I prefer the third, but won't be using any arcane keyboard shortcuts without explaining them here).
If you don't, [LispBox](http://common-lisp.net/project/lispbox/) is a very good starting point.
- **You have a database picked out and installed** I use MySQL, but [clsql supports many more](http://clsql.b9.com/manual/prerequisites.html#id430640) (and the syntax is the same no matter which you pick, so you'll get your money's worth here in any case)

### Things it does not assume (and that you therefore may want to skip through):

- **You are an advanced Lisper** I won't teach you about the REPL, or how to use Lisp as a calculator, but I will have some notes here about (for example) package basics.
- **You have used `clsql` or `CLOS` before** I'm aiming at `clsql`/`CLOS` newbs here, so I may cover some of the same ground as the mentioned documentation and tutorial. Skip those bits that you already know.
- **You have built a Lisp webapp before** There are a few tutorials running around, but I won't assume that you've read all or any of them. Again, skip the bits you know (though if you're familiar with CLOS, clsql and Lisp web-development, why are you reading this?)
- **You are using SBCL (though I am, so let me know if something here fails to work on your end)** I assume you're using a Common Lisp (and not a Scheme or one of the mongrels like [newLISP](http://www.reddit.com/r/lisp/comments/ghqxs/introduction_to_newlisp_wikibook/c1noc0d) or [Arc](http://www.paulgraham.com/arc.html)), and that said Common Lisp is compatible with both [Hunchentoot](http://weitz.de/hunchentoot/) and [CLSQL](http://clsql.b9.com/platforms.html). Other than that though, go nuts.
- **You are psychic** I'll try specifically to explain the things that were less than obvious to me while I was learning this material. Some stuff tends to get accidentally glossed over as trivial (it's also possible that I'm just thick, in which case, skip the obvious bits).
</div>

So lets get right to it.

Start a new file and get this into it, then save it as `cl-chan.lisp`

```lisp
(defpackage :cl-chan (:use :cl :cl-who :hunchentoot))
(in-package :cl-chan)

(defvar *web-server* (start (make-instance 'acceptor :port 4242)))
```

Then hop into your REPL and load :cl-who + :hunchentoot, followed by cl-chan

<div class="note beginner">
## Beginner Note
You can do that by typing

```emacs-lisp
(require 'cl-who)
(require 'hunchentoot)
(require 'clsql)
```

if you already have them installed. If you don't, then you might be able to install them (on a good day, in certain implementations, if you're lucky and if your last name begins and ends with "T") by typing

```emacs-lisp
(require 'asdf)
(require 'asdf-install)
(asdf-install:install 'cl-who)
(asdf-install:install 'hunchentoot)
(asdf-install:install 'clsql)
```

I've been told that doesn't work on all implementations (though it does work on the SBCL you can get out of the Debian repos). If you're smart, you will instead [go here](http://www.quicklisp.org/beta/) and follow the installation instructions, then type

```lisp
(ql:quickload (list :cl-who :hunchentoot :clsql))
```

That same statement will load local copies if you already have them installed, by the way. After all that, load your original file by typing

```lisp
(load "path-to/cl-chan.lisp")
```
</div>

then crack a browser open and head on over to `http://localhost:4242` to see the default Hunchentoot page.

Woo.

Party.

Ok, ok, lets get to something at least slightly workable quickly.

Add the following just below that `defvar`:

```lisp
(define-easy-handler (front-page :uri "/") ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
           (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                  (:title "Test Page"))
           (:body (:h1 "This is NOT the default page")
                  (:p "Nope.")))))
```

and refresh your browser.

<div class="note beginner">
## Beginner Note

The first bit we did was define a namespace ("package") called `:cl-chan`. You can find specifics and some more advanced uses [here](http://www.gigamonkeys.com/book/programming-in-the-large-packages-and-symbols.html), but the basic reason for this is to manage how the symbols we'll be defining interact on the global level. The basic `:use` directive we used specified that our package would import all exported symbols from three other packages;

- `:cl` (all the basic Common Lisp functions; if you get into the situation where you've defined a package and odd things are happening in expressions that really shouldn't error, what's probably going on is that you forgot to include `:cl`),
- `:cl-who` (an HTML generation library) and
- `:hunchentoot` (a lisp-based web-server). We'll talk about namespace conflicts a later.


`(in-package :cl-chan)` means that any symbol following it will be evaluated in the package `:cl-chan` instead of in the global namespace.

The `defvar` line defines a new variable, stores an acceptor there and starts a server listening on port 4242 using the `start` method. You can later stop the server by evaluating `(stop *web-server*)` (you don't need to at the moment).

Finally, the `define-easy-handler` line sets up a handler at "localhost:4242/" that will return a simple page. The `:cl-who` markup you see should be self-explanatory if you know HTML.

</div>

Right. So a chan is a collection of boards, each of which is a collection of threads, each of which is a collection of comments. Lets start at the bottom, since that'll be the fastest way of getting something relevant on screen. A comment is composed of


- a name
- an email
- a subject
- a comment
- an image
- a posted date/time
- a password (for deletion purposes)


We won't be dealing with the image and password for a while, so the obvious thing to do is

```lisp
(defclass comment ()
  ((author :reader author :initarg :author :initform nil)
   (email :reader email :initarg :email :initform nil)
   (subject :reader subject :initarg :subject :initform nil)
   (body :reader body :initarg :body :initform nil)
   (date-time :reader date-time :initarg :date-time)))
```

Which is an awful lot of repetitious typing. Almost enough that I'm tempted to write myself a macro, since I try to use the same symbol as the name, reader and initarg for a given class. The first bit in each slot is a slot name, the keyword arguments are


- `:reader` -- the name of the function which will return that slots' value (if you want one that will also let you modify the value, you should make it an `:accessor` instead)
- `:initarg` -- the name of the parameter that will accept a value for this slot when you call `make-instance` (you actually have to name it with a colon at the beginning there; `:author`, not `author`, it matters)
- `:initform` -- the default value of the slot if none is passed in (a class doesn't store `nil` by default; if you try to get the value of a slot that hasn't been set, you get an error).


So, lets try it out.

```lisp
(defparameter test-comment
  (make-instance 'comment
                 :author "me" :email "my@email.com" :subject "FRIST!!1!one!"
                 :body "I am most certainly the first poster in this fine establishment"
                 :date (get-universal-time)))
```

<div class="note beginner">
## Beginner Note

A parameter is like a [variable](http://www.lispworks.com/documentation/HyperSpec/Body/m_defpar.htm#defvar), except that they act differently if you try to define one with an existing name. If you `defvar` a variable that already exists, it keeps the old value (you actually have to use `setf` explicitly), whereas if you `defparameter` a parameter that exists, it gets the new value. `(get-universal-time)` is a function that returns the current number of seconds since the epoch (in Common Lisp, that's 00:00 Jan 1, 1900 GMT, as it happens). We won't need to do anything with it 'till later.
</div>

Hop on over to the REPL and kick the tires a bit;

```
> (author test-comment)
"me"
> (body test-comment)
"I am most certainly the first poster in this fine establishment"
> (setf (body test-comment) "BLARGFGHH!")
The function (SETF BODY) is undefined.
   [Condition of type UNDEFINED-FUNCTION]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [TERMINATE-THREAD] Terminate this thread (#<THREAD "repl-thread" RUNNING {1003108271}>)

Backtrace:
  0: ("bogus stack frame")
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (SETF (BODY TEST-COMMENT) "Blah") #<NULL-LEXENV>)
 --more--
```

So that didn't work out so well. Remember, we defined all of these slots as having a `:reader`, not an `:accessor`. We could have given them both, or just an accessor, but we won't really be needing to mess with a comment once it's been posted, so this should be ok. Use the `ABORT` restart to get back into the REPL.

<div class="note beginner">
## Beginner Note

I'm not sure how it works elsewhere, but if you're using Emacs, you can invoke a restart by typing the number next to it and hitting Return. So in this case, it'll be `1 RET`
</div>

Ok, now lets show that. Head over to that `define-easy-handler` from earlier and change the contents of the body tag to

```lisp
(:body (with-slots (author email subject body date-time) test-comment
         (htm (:div :class "comment"
                    (:span :class "header"
                           (str author) (str email)
                           (str date-time) (str subject))
                    (:span :class "body"
                           (:p (str body)))))))
```

and evaluate the function again.

<div class="note beginner">
## Beginner Note

You can certainly do that by using the same `(load "path-to/cl-chan.lisp")` statement as earlier, or by copy-pasting the function into your REPL. If you're using Emacs, you can also just get your cursor somewhere in the body of the function and hit `C-M-x`. Other environments probably have similar functionality.
</div>

Refreshing your browser should show you a fairly poorly formatted comment across two lines. Hey, it's a start. The first two things to notice are the `htm` and `str` tokens. These are part of the `:cl-who` library; they're actually tokens for the HTML generator, and not real functions so they'll error if you try to use them outside a `with-html-...` macro. They're just escapes to let you write dynamic HTML as part of `:cl-who` markup (without the `htm` after `with-slots`, you'd get `undefined function` errors for `:div`, `:span` and `:p`.

The `with-slots` macro is something you can use to address several slots from an object at once. Without it, we would have to do `(author test-comment)`, `(email test-comment)` and so on (had we not defined `:reader`s earlier, we'd have to use the even more laborious `(slot-value test-comment 'author)`).

One comment does not a board make, though. So lets get another in here. Add this one below the first `test-comment`

```lisp
(defparameter test-comment2
  (make-instance 'comment
                 :author "someone else" :email "you@fmail.com" :subject "Stop being a douchebag"
                 :date-time (get-universal-time)))
```

and load it into your lisp. Now, it would obviously be annoying as fuck to write out the entire display code for yet another comment when we know we'll have to deal with dozens. So, lets add the first dose of actual object-orientation.

```lisp
(defmethod echo ((a-comment comment))
  (with-html-output-to-string (*standard-output* nil :indent t)
    (with-slots (author email subject body date-time) a-comment
      (htm (:div :class "comment"
                 (:span :class "header"
                        (str author) (str email)
                        (str date-time) (str subject))
                 (:span :class "body"
                        (:p (str body))))))))
```

Put that anywhere you like in the file (after your `in-package` line) and evaluate it, then test it out in the REPL;

```lisp
> (echo test-comment)
"
<div class='comment'>
  <span class='header'>memy@email.com3522954339FRIST
  </span>
  <span class='body'>
    <p>I am most certainly the first poster in this fine establishment
    </p>
  </span>
</div>"
> (echo test-comment2)
"
<div class='comment'>
  <span class='header'>someone elseyou@fmail.com3522956120Stop being a douchebag
  </span>
  <span class='body'>
    <p>
    </p>
  </span>
</div>"
```

A `method` is like an `un` (in that you can `def` them both), but it can specialize on one or more classes. Take a look at where it says `((a-comment comment))`; that means that the `method` accepts one argument named `a-comment`, and that argument must be of class `comment`. From what I've seen, the correct convention is actually to name the argument after its class (so I really should have done ((comment comment)), but that's a bit harder to explain clearly).

Now that we've got the method, lets go ahead and re-define the body of the `front-page`

```lisp
(:body (str (echo test-comment))
       (str (echo test-comment2)))
```

Re-evaluate it and refresh your browser; you should now be seeing both poorly-formatted comments one on top of the next. So multiple comments we've got, but there's more to a board than comments; we need threads too, right? A thread is a collection of comments belonging to a board. Lets just get that defined just below the `comment` class.

```lisp
(defclass thread ()
  ((board :reader board :initarg :board)
   (comments :accessor comments :initarg :comments)))
```

Note that we didn't specify `:initform`s for `comments` or `:board` because every thread has those filled (there wouldn't be a thread otherwise). And, lets get a test-thread going. Add this to your file and evaluate it.

```lisp
(defparameter test-thread
  (make-instance 'thread
                 :board "a"
                 :comments (list test-comment test-comment2)))
```

And lets get another couple of comments in there for good measure;

```lisp
(defparameter test-comment3 (make-instance 'comment
                                           :subject "You must be new here"
                                           :body "trolled-softly.jpg"
                                           :date-time (get-universal-time)))

(defparameter test-comment4 (make-instance 'comment
                                           :body "[Something vaguely anti-semetic.]"
                                           :date-time (get-universal-time)))
```

We defined our first `:accessor` earlier, by the way. It's no different from a `:reader` except that you can modify the slot after its declared (which is sort of important if you're going to be replying to threads). Lets add those two other comments to the test thread. Nothing special; just hop over into the REPL

```lisp
> (comments test-thread)
(#<COMMENT {C1F3AF9}> #<COMMENT {100425C101}>)

> ;; your comment will probably look slightly different; that's ok
; No Value

> (setf (comments test-thread)
           (append (comments test-thread)
                   (list test-comment3 test-comment4)))
(#<COMMENT {C1F3AF9}>
 #<COMMENT {10048B3291}>
 #<COMMENT {10048B32F1}>
 #<COMMENT {100425C101}>)
```

There. Now that we have a thread with four comments, lets show that.

```lisp
(defmethod echo ((thread thread))
  (let ((first-comment (car (comments thread))))
    (with-html-output (*standard-output* nil :indent t)
      (with-slots (author email subject body date-time) first-comment
        (htm (:div :class "thread"
                   (:span :class "header"
                          (str author) (str email)
                          (str date-time) (str subject))
                   (:span :class "body"
                          (:p (str body)))
                   (dolist (r (cdr (comments thread)))
                     (str (echo r))))))))
```

<div class="note pedantic">
## Pedantic Note

Note that we're using `with-html-output` instead of `with-html-output-to-string`. Only the top level one actually has to be the `to-string` variant, and using the plain version of the macro lets us omit `str` calls on the output of this function. If you want, you can re-define `echo` for `comment`s the same way, which would let you write the `dolist` in the `thread` `echo` as

```lisp
(dolist (r (cdr (comments thread))) (echo r))
```
</div>

A method for the `thread` object! This is the other difference between a `method` and an `un`; you can have multiple `method`s with the same name which specialize on different objects. If you call `echo` on a `comment`, it'll evaluate the first one we defined. If you call it on a `thread`, it'll evaluate this new one (also note that part of our definition of `echo` for threads involves calling `echo` on each reply to the thread, which means that every element of `(cdr (comments thread))` must be an `echo`able object). Amend your `front-page` `:body` again;

```lisp
(:body (echo test-thread))
```

We defined `echo` such that you don't need the `str` this time (check the *Pedantic Note* above for details). Refreshing your screen should now show you four poorly styled comments stacked on top of one another. Actually, lets do something about that too. Create a new file called cl-chan.css and add this to it:

```css
.thread { background-color: #ddf; padding: 10px; margin-bottom: 10px; }
.thread .omitted { color: #88f; font-weight: bold; }
.comment { background-color: #aaf; border: 2px solid #88f; padding: 5px 5px 0px 5px; margin-bottom: 10px; }
.header span { margin-right: 3px; }
.header .subject { font-weight: bold; }
.comment .header span { color: #55f; }
.comment p { margin: 3px; }
```

then redefine your `front-page` to

```lisp
(define-easy-handler (front-page :uri "/") ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
           (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                  (:title "Test Page")
                  (:link :rel "stylesheet" :type "text/css" :href "/cl-chan.css"))
           (:body (echo test-thread)))))
```

finally, add this at the bottom of the file:

```lisp
(push
 (create-static-file-dispatcher-and-handler
  "/cl-chan.css" (merge-pathnames "cl-chan.css"))
 *dispatch-table*)
```

<div class="note style">
## Style Note

If you really want to do it right, you should probably create a sub-directory for the CSS files and make that `create-folder-dispatcher-and-handler` instead.

You might also want to look into the existing [CSS generators](http://www.cliki.net/CSS) in Lisp instead of doing the work by hand, although it probably won't save you much typing on something this minimal.
</div>

`create-static-file-dispatcher-and-handler` takes a uri relative to your site and a file path, and serves up that path when that uri is requested.

`*dispatch-table*` is where Hunchentoot figures out how to route incoming requests. By default it only has one entry; `default-dispatcher`, which gets called if no other dispatcher fits the request (by default, it's set to a Hunchentoot error page).

`merge-pathnames` creates a path given two other paths (we passed in "cl-chan.css", the second path is optional and defaults to the directory you ran your Lisp from).

Refreshing after *that* should get you something slightly better than plain text. The header bar is still unreadable though. You may have noticed a few lines in the CSS that said something about `.comment .header span` and `.header span`; that's called foreshadowing.

```lisp
(defmethod echo-header ((comment comment))
  (with-html-output (*standard-output*)
    (:span :class "header"
           (dolist (elem '(author email date-time subject))
             (htm (:span :class (format nil "~(~a~)" elem) (str (slot-value comment elem))))))))
```

We're resorting to `slot-value` and `dolist` instead of using `with-slots` because we're assigning a CSS class to each span that matches the slot name. We'll also need to redefine the `echo` methods to call this one where they need to output comment headers

```lisp
(defmethod echo ((thread thread))
  (let ((first-comment (car (comments thread))))
    (with-html-output (*standard-output* nil :indent t)
      (htm (:div :class "thread"
                 (echo-header comment)
                 (:span :class "body"
                        (:p (str (body first-comment))))
                 (dolist (r (cdr (comments thread)))
                   (str (echo r))))))))

(defmethod echo ((comment comment))
  (with-html-output-to-string (*standard-output* nil :indent t)
    (htm (:div :class "comment"
               (echo-header comment)
               (:span :class "body"
                      (:p (str (body comment))))))))
```

And we can now actually *read* the comments. Feel free to take some time out and make it pretty, if you like; it's beyond the scope of this tutorial to teach CSS, so [here's reference](http://www.w3schools.com/css/) instead.

Now, we've got a thread, which is ok, but we also need boards. A board is a collection of threads with a name. It'll probably have other stuff as we move through this exercise, but a name and some threads will suffice for now.

```lisp
(defclass board ()
  ((name :reader name :initarg :name)
   (threads :accessor threads :initarg :threads :initform nil)))
```

For our purposes, it would also be helpful to have a slightly longer thread.

```lisp
(defparameter test-thread2
  (make-instance 'thread
                 :board "a"
                 :first-comment test-comment
                 :replies (make-list 42 :initial-element test-comment3)))
```

And define the test board

```lisp
(defparameter test-board
  (make-instance 'board
                 :name "a"
                 :threads (list test-thread
                                test-thread2
                                test-thread
                                test-thread2
                                test-thread)))
```

Now, we know how to echo a thread (output all of its comments with the first one acting as the root element), and we know how to echo an individual comment (output the header in spans, followed by the body), but a board isn't dealt with the same way. The way you `echo` a board is you output the board's name in giant letters, followed by an `<hr />`, followed by a summary of each thread it contains (a summary is the first comment along with the last five, along with a little label telling us how many were omitted). The best way to do that would probably be to let a thread summarize itself instead of echoing its whole contents.

```lisp
(defmethod summarize ((thread thread) &optional (preview-comment-count 5))
  (let* ((preview-comments (last (cdr (comments thread)) preview-comment-count))
         (omitted-count (- (length (cdr (comments thread))) (length preview-comments)))
         (first-comment (car (comments thread))))
    (with-html-output (*standard-output* nil :indent t)
      (:div :class "thread"
            (echo-header first-comment)
            (:span :class "body"
                   (:p (str (body first-comment))))
            (when (> omitted-count 0)
              (htm (:p :class "omitted"
                       (str (format nil "~a comments omitted (and we don't do pictures yet)"
                                    omitted-count)))))
            (dolist (r preview-comments)
              (str (echo r)))))))
```

With that, echoing a board becomes trivial.

```lisp
(defmethod echo ((board board))
  (with-html-output (*standard-output* nil :indent t)
    (:h1 (str (name board))) (:hr)
    (dolist (thread (threads board))
      (summarize thread))))
```

There's really nothing new in either of these code blocks (other than my obvious fear of magic numbers exemplified by making the `preview-comment-count` an optional argument), so change the `:body` of your `define-easy-handler` declaration to `(echo test-board)` and check out your handiwork in the browser. The next step here is adding navigation. You want to be able to click on a comment to read the thread, and, more importantly, reply to it. To start with, add a link to your `summarize` method that leads to "/thread". The method should now look something like

```lisp
(defmethod summarize ((thread thread) &optional (preview-comment-count 5))
  (let* ((preview-comments (last (cdr (comments thread)) preview-comment-count))
         (omitted-count (- (length (cdr (comments thread))) (length preview-comments)))
         (first-comment (car (comments thread))))
    (with-html-output (*standard-output* nil :indent t)
      (:div :class "thread"
            (echo-header first-comment)
            (:a :href "/thread" "Reply")
            (:span :class "body" (:p (str (body first-comment))))
            (when (> omitted-count 0)
              (htm (:p :class "omitted"
                       (str (format nil "~a comments omitted (and we don't do pictures yet)"
                                    omitted-count)))))
            (dolist (r (cdr preview-comments)) (str (echo r)))))))
```

Also, declare the "/thread" page itself.

```lisp
(define-easy-handler (thread :uri "/thread") ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
           (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                  (:title (str (board test-thread)))
                  (:link :rel "stylesheet" :type "text/css" :href "/cl-chan.css"))
           (:body (echo test-thread)))))
```

Go ahead and check out the result in your browser. Click around a bit, if you like. Before we go on, you may have noticed that `thread` and `front-page` have a lot in common. In fact, the only differences are the `:title` property and the contents of `:body`. Normally, I wouldn't bother pulling out a pattern that only occurs twice, but I'm fairly sure we're going to want pages other than a board and a thread before we're done, *and* this one is fairly laborious to type out so...

```lisp
(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
            (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                   (:title (str ,title))
                   (:link :rel "stylesheet" :type "text/css" :href "/cl-chan.css"))
            (:body ,@body))))
```

That'll let us re-write `front-page` and `thread` as

```lisp
(define-easy-handler (front-page :uri "/") ()
  (page-template (:title "cl-chan")
    (echo test-board)))

(define-easy-handler (thread :uri "/thread") ()
  (page-template (:title (board test-thread))
    (echo test-thread)))
```

without really losing any readability. Right, now then. The inputs. Teaching how HTML forms work isn't really the focus of this guide, so I'm gonna go ahead and cheat because I really *really* don't feel like going through the entire submit->validate->show-errors||proceed semi-loop.

```lisp
> (asdf-install:install 'formlets)
[[snip]]
FORMLETS

> (defpackage :cl-chan (:use :cl :cl-who :hunchentoot :formlets))
#<PACKAGE "CL-CHAN">
```

It's [a library](https://github.com/Inaimathi/formlets) I wrote a little while ago to help me reduce the boilerplate involved with using HTML forms, drawing inspiration heavily from the [Racket](http://racket-lang.org/) (then PLT Scheme) implementation. I won't teach you this, just show you the code involved and offer a shortened explanation so we can move on.

```lisp
(define-formlet (post-comment-form)
    ((author text) (email text) (subject text) (body textarea) (captcha recaptcha))
  (let ((new-comment (make-instance 'comment
                                    :author author :email email
                                    :subject subject :body body
                                    :date-time (get-universal-time))))
    (setf (replies test-thread)
          (append (replies test-thread) (list new-comment)))
    (redirect "/thread")))
```

Since we're using [recaptcha](http://www.google.com/recaptcha), you'll also need to do

```lisp
(setf formlets:*public-key* [my-public-key] formlets:*private-key* [my-private-key])
```

You can get your keys by [signing up](http://www.google.com/recaptcha/whyrecaptcha) (it's free, and they don't need any personal details other than, I believe, an email). Finally, modify your thread page to show that formlet

```lisp
(define-easy-handler (thread :uri "/thread") ()
  (page-template (:title (board test-thread))
    (show-formlet post-comment-form)
    (echo test-thread)))
```

That was a formlet declaration, by the by; it has 5 fields (three regular inputs named `author`, `email` and `subject` respectively, a textarea named `body` and a [recaptcha](http://www.google.com/recaptcha) field named `captcha`). In this case, all the fields are un-validated (except for the captcha which always validates). When the user correctly enters the captcha, we'll add their comment to the test-thread and redirect them to the "/thread" page. After evaluating all that, you should be able to see a `reply` link on on each thread on the front page and a comment form at the top of the thread page. It actually works, for some value of "works", so you can try to add some messages to the `test-thread`.

We want to be able to add threads too though, not just reply to them. There's two ways we could do that;

First, we could add a hidden field to the `post-comment-form` that would contain either a reference to the thread or "new" (and post a new thread when it was "new"). That would let us reuse the same formlet.

Second, we could define a new formlet that just added a new thread to the board. There would be some additional boilerplate, but the two would be kept entirely separate rather than relying on a piece of information being passed to the client and then being passed back.

Despite the fact that relying on the client isn't always the best idea, it would probably work well here. However, we'll actually want to make the `body` (and eventual `image`) fields mandatory when you're starting a new thread, which means that we do actually need to handle validation differently for both situations, even though they involve the same fields. Ah well. If we need to define a third similar formlet, we can factor the common points out with a macro later.

```lisp
(define-formlet (post-thread-form)
    ((author text) (email text) (subject text)
     (body textarea :validation ((longer-than? 5) "You need to type at least six characters here."))
     (captcha recaptcha))
  (let* ((new-comment (make-instance 'comment
                                     :author author :email email
                                     :subject subject :body body
                                     :date-time (get-universal-time)))
         (new-thread (make-instance 'thread :board "a" :first-comment new-comment)))
    (push new-thread (threads test-board))
    (redirect "/")))
```

Add it to the `board` class' `echo` method too

```lisp
(defmethod echo ((board board))
  (with-html-output (*standard-output* nil :indent t)
    (:h1 (str (name board)))
    (:hr)
    (show-formlet post-thread-form)
    (:hr)
    (dolist (thread (threads board))
      (summarize thread))))
```

Refreshing should get you the same front page, but with a form to let you start a new thread. We're temporarily cheating on the *display* of threads by just showing test-thread all the time, so you can't actually see or reply to the others, but that still Actually Works™. For a tutorial titled "Crash Course on CLSQL and Hunchentoot", we haven't done a whole lot of CLSQL yet. We, actually, haven't so much as included it. Lets change that. Change your `defpackage` line to

```lisp
(defpackage :cl-chan (:use :cl :cl-who :hunchentoot :formlets :clsql))
```

and re-evaluate it. That should cause an error. Ok, ok, I promise to stop teasing after this, but this is just a point you should probably know if you're going to be developing in Lisp. Those of you who know what just happened, resolve the conflict by picking the `clsql:select` option, and skip the following note.

<div class="note beginner">
## Beginner Note

Namespace conflicts happen sometimes. Packages you want to include both export the same, perfectly reasonable name and when you `:use` them both without specifying what to do about the conflict, your Lisp throws you an error.

In this case, the conflict is with the symbol `select`. `:formlets` exports a `select` class (named after the HTML [Select](http://www.w3schools.com/tags/tag_select.asp) tag that it models) and `:clsql` exports a `select` function (named after the SQL [SELECT](http://www.w3schools.com/sql/sql_select.asp) statement that *it* models). Both packages made the right choice of name for the thing they're trying to represent, but they wouldn't play nice in the same namespace. So it's a good thing Lisp has built-in namespace management.

Now, if we weren't planning on using `select` at all, we could just add a shadow statement like so:

```lisp
(defpackage :cl-chan (:use :cl :cl-who :hunchentoot :formlets :clsql)
  (:shadow :select))
```

and be done with it. However, while our project won't call for the `select` **tag** yet (possibly at all), we will be using the `select` **statement** quite a bit. In that situation, you actually want to specify a `:shadowing-import-from` like this:

```lisp
(defpackage :cl-chan (:use :cl :cl-who :hunchentoot :formlets :clsql)
  (:shadowing-import-from :clsql :select))
```

That tells Lisp to import the `select` symbol from the `:clsql` package, and shadow the rest of them.
</div>

Ok, first thing to do is make sure you have a database and user set up in whatever db engine you use. You'll need to create a user too, and give the user permissions to the database (for this tutorial, you can just use your root user instead of creating a new one, but you shouldn't do that on a production server). The process varies depending on DB, so check the docs for yours.

Next, we need to change our defclass statements slightly. Lets start with `comment`

```lisp
(def-view-class comment ()
  ((id :accessor id :initarg :id :type integer
       :db-constraints (:not-null :auto-increment) :db-kind :key)
   (thread-id :reader thread-id :initarg :thread-id :type integer)
   (author :reader author :initarg :author :initform nil :type string)
   (email :reader email :initarg :email :initform nil :type string)
   (subject :reader subject :initarg :subject :initform nil :type string)
   (body :reader body :initarg :body :initform nil :type string)
   (date-time :reader date-time :initarg :date-time :type wall-time)))
```

Not all *that* much has actually changed. It's defined with def-view-class rather than plain defclass, we added an `id` field (whose `:db-constraints` and `:db-kind` field should make the intent clear if you know anything about databases), we added a `thread-id` field to show what thread this comment belongs to, and we added some admittedly poor type annotations to the rest of the fields. You actually need the `id` field, by the way. If you want `clsql` to update your data properly through the class-based interface, each record needs a `:key`, and it needs to be set (if that isn't the case, it'll just add a new record rather than editing the existing one).

Note that `date-time` is of type `wall-time` rather than `integer` which means we'll need to do a bit of shuffling in how we assign it. CLSQL actually has a bunch of really useful, but as far as I know largely undocumented, utilities for dealing with times, dates and durations. Define a new function called `now`:

```lisp
(defun now () (clsql-sys:utime->time (get-universal-time))
```

and call it instead of `(get-universal-time)` to set the `date-time` slot on a comment (you can just do a search-and-replace here; we haven't used `univeral-time`s for anything else).

<div class="note package">
## Package Note

A point of interest, `clsql-sys` has plenty of similar utility functions, but they're not documented anywhere other than in the code itself (check out the test suite buried in the clsql-sys source) and in [three half-line blurbs about wall-time, date and duration](http://clsql.b9.com/manual/def-view-class.html) in the official documentation. This kind of poor visibility is what led me to re-invent the wheel [last time](/posts/clsql) in defining my own `mysql-time` function the hard way.

Other stuff you might find interesting (some of which we will touch on later):

```lisp
make-duration
parse-timestring
parse-datestring
time-difference ;; it has all the usual arithmetic items too,
                ;; this just happens to be the most useful, IMO
print-date ;; which, oddly, takes a `wall-time`,
           ;; not a `date`, and accepts the following
           ;; format options:
           ;;    :time-of-day :long-day :month :month-year
           ;;    :full :full+weekday :daytime :day
clsql-sys:date->time
clsql-sys:time->date
clsql-sys::days-in-month ;;yup, not even external
```
</div>

The transformation of a thread is a little more interesting.

```lisp
(def-view-class thread ()
  ((id :accessor id :initarg :id :type integer
       :db-constraints (:not-null :auto-increment) :db-kind :key)
   (board-id :reader board-id :initarg :board-id :type integer)
   (comments :accessor comments :db-kind :join
             :db-info (:join-class comment :home-key id :foreign-key thread-id :set t))))
```

We add an `id` slot here too, but the declaration of `comments` is our first example of the `:db-kind :join` notation. The important parts are


- `:db-info` *must* be provided if you have `:db-kind :join`.
- `:join-class` must designate another class defined by `def-view-class` (in this case `comment`, obviously).
- `:home-key` and `:foreign-key` specify which columns to join on (you can specify either single columns or multiple columns, as in `'(id thread-id)`, for example).
- `:set` is a boolean that specifies whether this join should expect multiple values. It's `nil` by default, but in this case, we *are* expecting a set of comments to be returned, so we need to set it.
- the results of a `:join` are returned as a list of matching elements, so we won't need to change how we deal with comments in any of the `thread` methods


The boards declaration shouldn't present any surprises

```lisp
(def-view-class board ()
  ((id :accessor id :initarg :id :type integer
       :db-constraints (:not-null :auto-increment) :db-kind :key)
   (name :reader name :initarg :name :type (string 5))
   (threads :accessor threads :db-kind :join
            :db-info (:join-class thread :home-key id :foreign-key board-id :set t))))
```

The only new thing here is that `name` is of type `(string 5)`, which just means that `5` will be passed as the width of that column (in MySQL, this will be represented as a `VARCHAR(5)` column; it may be different in other databases). You can do the same sort of thing with `integer`, `varchar`, `float`, and `number` fields to limit length.

Once you've got the classes defined, you can automatically create tables based on them by using `create-view-from-class`. Lets connect and create those tables

```lisp
> (connect '("localhost" "cl_chan" "me" "my password") :database-type :mysql) ;; obviously, you'll want to change the :mysql to your DB type and "me"/"my password" to your information
#<CLSQL-MYSQL:MYSQL-DATABASE localhost/cl_chan/me OPEN {BA80359}>
> (dolist (c '(board thread comment)) (create-view-from-class c))
NIL
```

A similar function, `drop-view-from-class` will let you delete the tables later (you don't need to do this right now). Lets get our data into our DB. We're kind of starting from scratch because we want the DB to handle assigning IDs to everything (even though we could easily guess them at this point).

```lisp
> (defparameter test-board (make-instance 'board :name "a"))
TEST-BOARD
> (update-records-from-instance test-board)
1
```

The return value from the function is the ID the database assigned to that record. Obviously, "a" being the first board, it's assigned the id 1. At this stage, we also need to change our handlers and our methods slightly. First up, lets get a board page up

```lisp
(define-easy-handler (board :uri "/board") ()
  (page-template (:title "cl-chan")
    (let ((board (caar (select 'board :where [= [slot-value 'board 'id] 1]))))
      (echo board))))
```

The `select` statement there should be decipherable to you if you're familiar with SQL (we only have one board, so I'm hard-coding the board ID right now, we'll change that later). First thing to note is that `caar` call wrapping `select`. `select` always returns a list of lists. It's a bit annoying here because we're only selecting one thing, so it would be helpful to just return a single item. But if we did, for example

```lisp
(select 'board 'thread 'comment)
```

then we'd actually want a list of lists (`((board thread comment) (board thread comment) ...)`). I guess the developers of `clsql` thought it better to be consistent than convenient (which I agree with up to a point, and this isn't past that point yet).

Notice also that the `:where` clause is expressed as a keyword argument. The square brackets delimit expressions that are going to be translated mechanically to SQL behind the scenes. The thing is, they're implemented as reader macros, so you'll need to add

```lisp
(file-enable-sql-reader-syntax)
```

to your file (just below the `in-package` line), as well as evaluate

```lisp
> (enable-sql-reader-syntax)
NIL
```

at the REPL. If you don't, you'll get some odd undefined-variable errors. We'll need to re-write our `post-thread-form` too

```lisp
(define-formlet (post-thread-form)
    ((author text) (email text) (subject text)
     (body textarea :validation ((longer-than? 5) "You need to type at least six characters here."))
     (captcha recaptcha))
  (let* ((thread-id (update-records-from-instance
                     (make-instance 'thread :board-id 1)))
         (new-comment (make-instance 'comment
                                     :thread-id thread-id
                                     :author author :email email
                                     :subject subject :body body
                                     :date-time (now))))
    (update-records-from-instance new-comment)
    (redirect "/board")))
```

After evaluating all that, hop into your browser and add a thread. If you've done everything correctly, you'll notice that *nothing happened*. Hop into the REPL, just to make sure something else didn't go wrong; you should be able to do

```lisp
> (select 'thread)
((#<THREAD {B5D5D49}>))

> (select 'comment)
((#<COMMENT {B64AE29}>))
```

There'll be more of them if you tried a few times. So the threads and comments are being generated, but they're not showing up on your page. This is actually a feature of CLSQL. A caching feature. I don't mean for that to sound tongue-in-cheek, it's very useful when you have a somewhat static set of data and you'd like to save database round-trips. In those situations, you'd want to keep caching on globally and specify the individual non-caching `select`s by doing

```lisp
(select 'foo :caching nil)
```

It's just that *this* isn't that kind of project. We'd like non-caching to be the default (and we'll specify the places where caching *should* happen). So add

```lisp
(setf *default-caching* nil)
```

near the top of your file and evaluate it. Go ahead and refresh, and you should see your new threads. Of course, clicking "Reply" does *not* do what we want at this point. Lets fix the `thread` page and finally get this thing off the ground.

```lisp
(define-formlet (post-comment-form)
    ((thread-id hidden)
     (author text) (email text) (subject text) (body textarea)
     (captcha recaptcha))
  (let ((new-comment (make-instance 'comment
                                    :thread-id (parse-integer thread-id)
                                    :author author :email email
                                    :subject subject :body body
                                    :date-time (now))))
    (update-records-from-instance new-comment)
    (redirect (format nil "/thread?thread-id=~a" thread-id))))

(define-easy-handler (thread-page :uri "/thread") (thread-id)
  (let ((thread (caar (select 'thread :where [= [slot-value 'thread 'id] thread-id]))))
    (page-template (:title (or (title (car (comments thread))) (id thread)))
      (echo thread))))
```

You'll also need to change the `show-formlet` line in the `thread`s' `echo` method to

```lisp
(show-formlet post-comment-form :default-values (list (id thread)))
```

so that the hidden field we've got going will actually have the correct default value.

Note that we've got a new field to let us know which thread is being replied to (we could have done this through hunchentoots' `[session](http://weitz.de/hunchentoot/#sessions)`, but that would have some odd corner cases if someone wanted to keep in multiple threads at once). Adding an argument to an easy-handler just makes sure it's bound appropriately (so the `thread-id` in those `:where` and `:default-values` clauses will be set to the correct values).

<div class="note pedantic">
## Pedantic Note

Just as an addendum, keep in mind two things here.

One, if you feel like calling `get-parameter` manually, you call it with the lower-cased string. Above, it would be `(get-parameter "thread-id")`, not `(get-parameter :thread-id)` or `(get-parameter "THREAD-ID")`. Those are all different things (the second one will throw an error, I think, the third one will just return NIL).

Two, any values you get back this way are **strings**, no matter what they actually represent. It doesn't matter in this case because the `CLSQL` reader macro handles it intelligently, and `formlets` converts anything it gets into a string anyway. However, if we wanted to create a new DB object (as in the formlet above), or do a standard numeric comparison, however, we'd need to convert from string. So,

```lisp
(= 1 (parse-integer thread-id))
```

and not

```lisp
(= 1 thread-id)
```

`the-more-you-know.jpg`
</div>

Finally, change the reply link in the `summarize` method to point to `(format nil "/thread?thread-id=~a" (id thread))` instead of `"/thread"` (so that you can just click on the "Reply" link to get around). Go ahead and add some threads, then respond to them. You should be able to at this point.

I honestly thought this was going to be a shorter piece, but it's at 6k words and I've just managed to explain how to put together a very simple non-image message board. I'm calling this Part 1 and picking it up later. The code so far is over at [github](https://github.com/Inaimathi/cl-chan), in case you feel like starting from something when you tinker. If you feel like doing some exercises in the meantime, use what we've learned here to implement multiple boards (everything should be in place for that to be a simple tweak).

Still to Come: multiple boards, images, working with uploads and defining `asdf-system`s.
