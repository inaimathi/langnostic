I've fallen to a fit of OCD in the past few days, which has compelled me to clean out my langnostic drafts folder. Yes, I have a drafts folder. It turns out that it contained something on the order of 12 almost finished articles that I just never got around to polishing. I'm still working on the authentication system, and poking around at the prospect of a real-time message board, but I'll *also* be either scrapping or refining+posting those forgotten drafts for the next few weeks. Starting with one about a year and a half in the making.

Start with [Part 1](/posts/cl-chan-a-clsql-and-hunchentoot-crash-course) if you like.

* * *

Aaaand we're finally back.

This time we'll be looking at extending the board features and dealing with images, but first, I'm sure I'm not the only one tired of having to type out

```lisp
(ql:quickload (list :hunchentoot :cl-who :clsql :formlets))
(load "cl-chan.lisp")
```

every time I want to load the package. Ideally, I'd like that to be a single statement ("Load all cl-chan files in the correct order, and do the same for each dependency"). That's one of the things an `asdf-system` definition lets you do[^the-other-is]. First up, we've been keeping everything in one file, and we really shouldn't. At the very least, the model should be isolated since it's going to grow shortly. It's also typical for CL projects to have a separate `package.lisp` file[^so-that-when-someone]. Our package file is going to be very simple, since we don't export anything yet.

[^the-other-is]: The other is put together a downloadable archive so that other Lispers can install your package through `asdf-install`, but we'll discuss that later.
[^so-that-when-someone]: So that when someone else is using your package, they can go to one consistent place to see all the symbols you're exporting and including. I tend to put conf variables there too if there aren't very many of them, but they should probably be in their own `conf.lisp` file.

```lisp
(defpackage :cl-chan (:use :cl :cl-who :hunchentoot :formlets :clsql)
  (:shadowing-import-from :clsql :select))
(in-package :cl-chan)

(connect '("localhost" "cl_chan" "me" "my password") :database-type :mysql)

(setf *default-caching* nil)

(setf formlets:*public-key* "my-public-key"
      formlets:*private-key* "my-private-key")

(defvar *web-server* (start (make-instance 'hunchentoot:easy-acceptor :port 4242)))
(push (create-static-file-dispatcher-and-handler "/cl-chan.css" (merge-pathnames "cl-chan.css")) *dispatch-table*)
```

That should do it.

Next, lets pull out our testing data into a file named "testing-data.lisp" (we'll remove this later, but it will let you re-create your database fairly easily if you need to while we're still playing around)

```lisp
(in-package :cl-chan)

(defun create-test-database ()
  (create-tables)
  (insert-test-data))

(defun drop-tables ()
  (dolist (i '(board thread comment))
    (drop-view-from-class i)))

(defun create-tables ()
  (dolist (i '(board thread comment))
    (create-view-from-class i)))

(defun insert-test-data ()
  (loop for i in (list (make-instance 'board :name "a")
                       (make-instance 'thread :board-id 1)
                       (make-instance 'thread :board-id 1)

                       (make-instance 'comment
                            :thread-id 1
                            :author "me" :email "my@email.com" :subject "FRIST"
                            :body "I am most certainly the first poster in this fine establishment"
                            :date-time (now))
                       (make-instance 'comment
                            :thread-id 1
                            :author "someone else" :email "you@fmail.com" :subject "Stop being a douchebag"
                            :date-time (now))
                       (make-instance 'comment
                            :thread-id 1
                            :subject "You must be new here"
                            :body "trolled-softly.jpg"
                            :date-time (now))
                       (make-instance 'comment
                            :thread-id 2
                            :body "[Something vaguely anti-semetic.]"
                            :date-time (now)))
        do (update-records-from-instance i)))
```

Next, pull out the model (the classes and related methods) into `model.lisp`. This'll actually be the most complicated individual file in the project.

```lisp
(in-package :cl-chan)
(file-enable-sql-reader-syntax)

(defun now () (clsql-sys:utime->time (get-universal-time)))

;;;;;;;;;; board
(def-view-class board ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (name :reader name :initarg :name :type (string 5))
   (threads :accessor threads :db-kind :join
            :db-info (:join-class thread :home-key id :foreign-key board-id :set t))))

(defmethod echo ((board board))
  (with-html-output (*standard-output* nil :indent t)
    (:h1 (str (name board))) (:hr)
    (show-formlet post-thread-form) (:hr)
    (dolist (thread (threads board))
      (summarize thread))))

;;;;;;;;;; thread
(def-view-class thread ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (board-id :reader board-id :initarg :board-id :type integer)
   (comments :accessor comments :db-kind :join
             :db-info (:join-class comment :home-key id :foreign-key thread-id :set t))))

(defmethod summarize ((thread thread) &optional (preview-comment-count 5))
  (let* ((preview-comments (last (cdr (comments thread)) preview-comment-count))
         (omitted-count (- (length (cdr (comments thread))) (length preview-comments)))
         (first-comment (car (comments thread))))
    (with-html-output (*standard-output* nil :indent t)
      (:div :class "thread"
            (echo-header first-comment)
            (:a :href (format nil "/thread?thread-id=~a" (id thread)) "Reply")
            (:span :class "body" (:p (str (body first-comment))))
            (when (> omitted-count 0)
              (htm (:p :class "omitted"
                       (str (format nil "~a comments omitted (and we don't do pictures yet)"
                                    omitted-count)))))
            (dolist (r preview-comments) (str (echo r)))))))

(defmethod echo ((thread thread))
  (let ((first-comment (car (comments thread))))
    (with-html-output (*standard-output* nil :indent t)
      (:a :href "/board" "[Back]") (:hr)
      (show-formlet post-comment-form :default-values (list (id thread))) (:hr)
      (:div :class "thread"
            (echo-header first-comment)
            (:span :class "body"
                   (:p (str (body first-comment))))
            (dolist (r (cdr (comments thread))) (str (echo r)))))))

;;;;;;;;;; comment
(def-view-class comment ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (thread-id :reader thread-id :initarg :thread-id :type integer)
   (author :reader author :initarg :author :initform nil :type string)
   (email :reader email :initarg :email :initform nil :type string)
   (subject :reader subject :initarg :subject :initform nil :type string)
   (body :reader body :initarg :body :initform nil :type string)
   (date-time :reader date-time :initarg :date-time :type wall-time)))

(defmethod echo ((comment comment))
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:div :class "comment"
          (echo-header comment)
          (:span :class "body"
                 (:p (str (body comment)))))))

(defmethod echo-header ((comment comment))
  (with-html-output (*standard-output*)
    (:span :class "header"
           (dolist (elem '(author email date-time subject))
             (htm (:span :class (format nil "~(~a~)" elem) (str (slot-value comment elem))))))))
```

Having chopped all that off, your `cl-chan.lisp` file should be left at

```lisp
(in-package :cl-chan)
(file-enable-sql-reader-syntax)

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
            (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                   (:title (str ,title))
                   (:link :rel "stylesheet" :type "text/css" :href "/cl-chan.css"))
            (:body ,@body))))

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
    (redirect"/board")))

(define-easy-handler (board-page :uri "/board") ()
  (page-template (:title "cl-chan")
    (let ((board (caar (select 'board :where [= [slot-value 'board 'id] 1]))))
      (echo board))))

(define-easy-handler (thread-page :uri "/thread") (thread-id)
  (let ((thread (caar (select 'thread :where [= [slot-value 'thread 'id] thread-id]))))
    (page-template (:title (or (subject (car (comments thread))) (id thread)))
      (echo thread))))
```

Now that we've broken everything up, start up a new file called `cl-chan.asd` and put this in it

```lisp
;;; -*- Mode: Lisp -*-
(defpackage :cl-chan-system (:use :cl :asdf))
(in-package :cl-chan-system)

(asdf:defsystem cl-chan
  :version "0.001"
  :author "Inaimathi"
  :maintainer "you"
  :licence "AGPL"
  :description "A simple message board server"
  :depends-on (:hunchentoot :cl-who :clsql :formlets)
  :serial t
  :components ((:file "package") (:file "model") (:file "cl-chan") (:file "testing-data")))
```

You should now be able to load up your lisp (with cl-chan/ as your working directory) and

```lisp
> (ql:quickload :cl-chan)
To load "cl-chan":
  Load 1 ASDF system:
    cl-chan
; Loading "cl-chan"
..................................................
[package cl-chan]...
(:CL-CHAN)
```

Much better than doing it manually, and if you're showing off your app, you get to pretend you know [Zach Beane](http://www.xach.com/) for about two seconds until people realize what's going on :p. Now keep in mind that in this particular project, it hasn't saved us all that much. Even with our better organized code, we'd only really need to evaluate

```lisp
(ql:quickload (list :hunchentoot :cl-who :clsql :formlets))
(load "package.lisp")
(load "model.lisp")
(load "cl-chan.lisp")
(load "testing-data.lisp")
```

in order to launch our little message board. Once you start adding files, you rapidly see the value of being able to use a single load statement instead, since a properly written `.asd` automatically loads them all in the correct order, as well as loading any noted dependencies. Note that once you understand what `asd`/`package.lisp` files are supposed to look like, you can auto-generate them with [quickproject](https://github.com/xach/quickproject/) (thanks again, Zach).

Ok, it's about damn time we figured out what to do about these images we want, otherwise it's not much of an **image**board.

<div class="note alternatives">
### Alternatives Note

I'm going to continue the rest of the tutorial assuming we're taking a slightly reduced functionality, Lisp-only approach. It'll make the system really easy to set up, and increase its portability across various platforms (since everything we'll be using is a lisp library, it'll run anywhere you can load a thread-capable Common Lisp implementation). However, that's far from the only option.

#### The FFI Route
First, if you don't mind some installation headaches, you can use the faster-at-run-time `:cl-gd` (which is a set of UFFI bindings to the [C-based GD graphics library](http://www.boutell.com/gd/)).

Ostensibly, you can install libgd2-xpm or libgd2-noxpm (if you don't know the difference, just use the first one) and then `(ql:quickload :cl-gd)`. And if that works for you, fantastic, you lucky fucker. Before you celebrate though, make sure to try out [an example or two](http://weitz.de/cl-gd/), because I thought it really *was* this easy to begin with.

If you're getting `undefined alien function` errors, as I did, you'll actually need to install the debian package from the `lenny` repos. If you're already running `lenny`, just do `apt-get install cl-gd`, otherwise you'll need to add

    deb http://ftp.us.debian.org/debian/ lenny main contrib

to your `/etc/apt/sources.list` file, *then* run `apt-get install cl-gd`. I have no idea what issues you'll run into with lisps other than SBCL on systems other than Debian linux. Which is why I'm doing the portable thing in the actual tutorial.

#### The Lazy Bastard Route

 Don't let the name throw you, it may be a legitimate option, depending on the circumstances. Basically, instead of dicking around with native libraries, or FFI calls, you just load up `:trivial-shell` and do something like

    (shell-command (format nil "convert ~a -resize 300\\> ~a-preview.jpg" image-file image-name))

The performance on it sucks donkey dong, and it's not portable to non-posix-compliant platforms, and it requires you to have ImageMagick installed on the deployment environment, *and* it means you need to handle server-side image naming yourself (which we were going to do anyway) to avoid shell injection attacks. However, it's one line of code and it covers conversion for pretty much every graphical format under the sun (the main tutorial will be doing things the hard way, so you'll fully appreciate the simplicity of `shell`ing out). If you can afford those hits, it's not a bad option.
</div>

I'll be continuing with the pure-Lisp version for portability purposes, and that gives us a bit of a problem. Unlike the "FFI" and "Lazy Bastard" options outlined above, Common Lisp doesn't have a general image-formatting library. We'll need to use separate libraries (and slightly different processes) for different image formats. Three in all, since we want to fully implement the 4chan formats; `[ch-image](http://cyrusharmon.org/static/projects/ch-image/doc/ch-image.xhtml)` for JPEGs, `[imago](http://common-lisp.net/project/imago/)` for PNGs and `[skippy](http://www.xach.com/lisp/skippy/)` for GIFs. That's a pretty obvious place to apply method calls, actually, so lets start by formalizing the process for a single image type, then extending it to the others.

There are exactly two things we'll want to do to an incoming image: store the original, and store a 250x250 pixel proportional preview image. Before we get to that, we'll need to include a new library. `:cl-fad` will give us some easy ways of dealing with files on disk, so add it to your `asd` file `package.lisp` and load it into your `REPL` too. While we're at it, lets add the image manipulating libraries and the new file "images.lisp" into the mix.

```lisp
;;; cl-chan.asd

...
:depends-on (:hunchentoot :cl-who :clsql :formlets :cl-fad
                            ;; image related
                            :imago :skippy :ch-image)
...
:components ((:file "package") (:file "model") (:file "cl-chan") (:file "testing-data") (:file "images")))

;;; package.lisp

(defpackage
    :cl-chan (:use :cl :cl-who :hunchentoot :formlets :clsql :cl-fad)
    (:import-from :imago :read-png :write-png) ;; resize
    (:import-from :ch-image :read-image-file :write-image-file) ;; resize-image
    (:import-from :skippy :load-data-stream :output-data-stream) ;; scale
    (:shadowing-import-from :clsql :select))
...

```

Note that we're importing the entirety of `:cl-fad`, but just select symbols from the image libraries. This isn't strictly necessary, but since we're going to be including three different utilities that do similar things, I get the sneaking suspicion that we'd get symbol collisions otherwise. I've imported relevant operations from all three libraries, even though we're starting out with JPGs only. Now then, start a new file called `images.lisp`, and add the following to it


```lisp
(in-package :cl-chan)

(defclass image-upload ()
  ((name :reader name :initarg :name)
   (file-path :reader file-path :initarg :file-path)))

;;;;;;;;;; utility
(defun file-tuple->image-upload (hunchentoot-file-tuple)
  (destructuring-bind (file-path original-file-name mimetype) hunchentoot-file-tuple
    (make-instance (intern (string-upcase mimetype) :cl-chan)
                   :name (file-namestring file-path)
                   :file-path file-path)))

(defun store! (hunchentoot-file-tuple)
  (when hunchentoot-file-tuple
    (let ((img (file-tuple->image-upload hunchentoot-file-tuple)))
      (store-images! img))))

(defun new-dimensions (size width height)
  "Given a target size and width/height, returns a new width/height preserving aspect ratio.
Does not scale images smaller than 250x250."
  (let ((ratio (max 1 (float (/ (max width height) size)))))
    (values (round (/ width ratio)) (round (/ height ratio)) ratio)))

;;;;;;;;;; PNGs
(defclass png (image-upload) ())
(defclass image/x-png (png) ())
(defclass image/png (png) ())

(defmethod store-images! ((img png))
  "Saves a large version, and creates a preview of the given image in directories specified by the *big-dir* and *preview-dir* config variables"
  (let* ((pic (read-png (file-path img)))
         (w (imago:image-width pic))
         (h (imago:image-height pic))
         (pic-name (make-pathname :name (name img) :type "png")))
    (copy-file (file-path img) (merge-pathnames pic-name *big-dir*))
    (multiple-value-bind (new-width new-height) (new-dimensions 250 w h)
      (write-png (imago:resize pic new-width new-height)
                 (merge-pathnames pic-name *preview-dir*)))
    (namestring pic-name)))
```

So. What we just did was create a new class called `image-upload`, subclass it specifically for `png`, and write the `store-images!`  method. `:imago` doesn't seem to provide a way to preserve aspect ratio for an image as you resize it, so we have to do that manually. I resisted the temptation to make it png-specific, because it's entirely possible that we'll need to call the same code as part of generating previews for the other formats.

Take a closer look at the `file-tuple->image-upload`. We're using the incoming file `mimetype` as a class name. That may sound like a bad idea, but as you'll see in a few minutes, we're going to be restrictive about what input we accept. It's just that in order to build a system we can extend later, we can't really be restrictive *here*.

<div class="note extensibility">

### Extensibility Note

The way we're going to restrict input is by doing server-side validation on the files our users will upload. That's a good idea, but doing *just* that will leave validation for this function elsewhere in the codebase. Without seeing that validation, the definition for `file-tuple->image-upload` can easily be mistaken for an injection attack vector (if we didn't validate, a user could send a bogus mimetype and cause us to spawn, for example, a `pathname` instead of an image. I can't think of an obvious attack that would be enabled by that, but it's still best to minimize vectors). Lets think through the alternatives here

#### Validate in `file-tuple->image-upload`

Whether we do it by writing an assertion to make sure that the incoming mimetype meets some criteria, or by creating a specific class using a `cond` statement, we run into the same problem: in order to add a new supported image type, it won't be enough to just evaluate a new `store-images!` method and new classes. We'll actually have to slightly re-write `file-tuple->image-upload`. That's bad; we'd like extensibility to be possible without involving edits to an existing `cond` in our package.

If you really, really feel nervous about leaving a naked class declaration like we did, you can add something along the lines of `(assert (string= "image/" (subseq mimetype 0 6)))` to the function, just to ensure the incoming is an image. Even doing that is going to come back for a bite of your ass if you decide to allow PostScript/PDF uploads in your forum (since both of those have the "application/something" mimetype).

#### Break `file-tuple->image-upload` up into methods

Instead of doing validation in-function, it's also possible to break the image-upload-creating function up into different methods. This is a viable, and technically more object-oriented, approach to the problem. I'm choosing not to go that way because it would mean defining something like

```lisp
(defmethod make-image-upload ((mime (eql 'image/png)) hunchentoot-file-tuple)
  "Handles PNG image-upload creation with the image/png mimetype"
  (destructuring-bind (file-path original-filename mime-string) hunchentoot-file-tuple
    (declare (ignore original-filename mime-string))
    (make-instance 'png :name (file-namestring file-path) :file-path file-path)))

(defmethod make-image-upload ((mime (eql 'image/x-png)) hunchentoot-file-tuple)
  "Handles PNG image-upload creation with the image/x-png mimetype"
  (make-image-upload 'image/png hunchentoot-file-tuple))

[repeat for every image type]
```

rather than merely something like

```lisp
(defclass image/x-png (png) ())
(defclass image/png (png) ())

[repeat for every image type]
```

Yes, it's more object-oriented, but it's a *lot* more verbose, and it'll get even worse if you want to support an image type that has more than two common mimetypes. Keeping the amount of code you type to a minimum is very good practice for all the reasons you've probably already heard.
</div>

Note that we've actually got three subclasses for `png`. There's two reasons for that. I go over one in the Extensibility Note above. The other is that, while PNGs are technically supposed to be of mimetype `image/png`, I've seen several in the wild with `image/x-png` instead. Now, even though there are multiple mimetypes a PNG could have, we won't be dealing differently with each of them, so it's enough to create a `png` class with the appropriate methods, and then subclass that for individual mimetypes we plan to encounter. If you've seen others, feel free to add them.

Now that we have a way of dealing with images, lets set up the rest of our system to deal with them. First off, add the following lines to your `package.lisp`

```lisp
(defparameter *image-storage-directory* "img")
(defparameter *big-dir* (merge-pathnames (make-pathname :directory `(:relative ,*image-storage-directory* "big"))))
(defparameter *preview-dir* (merge-pathnames (make-pathname :directory `(:relative ,*image-storage-directory* "preview"))))
(ensure-directories-exist *big-dir*)
(ensure-directories-exist *preview-dir*)
(push (create-folder-dispatcher-and-handler
       "/img/"
       (merge-pathnames (make-pathname :directory `(:relative ,*image-storage-directory*))))
      *dispatch-table*)
```

I'll format it a bit better in the code I check in, but it'll basically do the same thing. That's a specifier for a local image storage directory, and two specific subdirectories (which we `ensure-exist` just in case) where we'll be keeping the images that get uploaded to our board. The last few lines push our new storage folder onto the dispatch table so that Hunchentoot can serve the contained files. Now that we've got that, we'll need to change our formlets to accept an image file, and tell them what to do with it. We'll also need to add an image field to our comment class,

```lisp
;;; model.lisp
...
(def-view-class comment ()
  ((id :accessor id :initarg :id :type integer :db-constraints (:not-null :auto-increment) :db-kind :key)
   (thread-id :reader thread-id :initarg :thread-id :type integer)
   (author :reader author :initarg :author :initform nil :type string)
   (email :reader email :initarg :email :initform nil :type string)
   (subject :reader subject :initarg :subject :initform nil :type string)
   (body :reader body :initarg :body :initform nil :type string)
   (date-time :reader date-time :initarg :date-time :type wall-time)
   (image :reader image :initarg :image :type string)))

...
```

<div class="note state">
### State Note

You'll also need to re-create your database tables (or evaluate `alter table COMMENT add column IMAGE varchar(255);` against your database. The actual SQL you need to run will vary slightly based on what database you're using. The above works with MySQL.)

</div>

and change its `echo` method to output the image preview along with the comment. You'll also want to add the image echoing code to the thread `echo` method, since that does its own thing. In fact, it'd probably be a better idea to define a new echo-image method and call it in those three places.

```lisp
;;; model.lisp

(defmethod summarize ((thread thread) &optional (preview-comment-count 5))
...
            (:a :href (format nil "/thread?thread-id=~a" (id thread)) "Reply")
            (echo-image first-comment)
            (:span :class "body" (:p (str (body first-comment))))
            (:br :class "clear")
...

(defmethod echo ((thread thread))
...
            (echo-header first-comment)
            (:span :class "body"
                   (echo-image first-comment)
                   (:p (str (body first-comment))))
            (dolist (r (cdr (comments thread))) (str (echo r)))))))
...

(defmethod echo ((comment comment))
  (with-html-output-to-string (*standard-output* nil :indent t)
    (:div :class "comment"
          (echo-header comment)
          (:span :class "body"
                 (echo-image comment)
                 (:p (str (body comment)))
                 (:br :class "clear")))))

...

(defmethod echo-image ((comment comment))
  (when (image comment)
    (with-html-output (*standard-output* nil :indent t)
      (:a :href (merge-pathnames (image comment) "/img/big/")
          (:img :class "pic" :src (merge-pathnames (image comment) "/img/preview/"))))))
```

The last thing we need to do is change up the comment formlets so that they accept images (with validation) and add the appropriate image URI to the comments they produce.

```lisp
;;; cl-chan.lisp

...
(defun validate-image (hunchentoot-file-tuple)
  (or (null hunchentoot-file-tuple)
      (and (funcall (file-type? "image/x-png" "image/png") hunchentoot-file-tuple)
           (funcall (file-smaller-than? 3000000) hunchentoot-file-tuple))))

...
     (author text) (email text) (subject text) (body textarea)
     (image file :validation (#'validate-image "We accept PNGs smaller than 3MB"))
     (captcha recaptcha))
    (let* ((pic (store! image))
           (new-comment (make-instance 'comment
                                       :thread-id (parse-integer thread-id)
                                       :author author :email email
                                       :subject subject :body body
                                       :date-time (now)
                                       :image pic)))
      (update-records-from-instance new-comment)
...

     (image file :validation (#'validate-image "We accept PNGs smaller than 3MB"))
     (captcha recaptcha))
  (let* ((thread-id (update-records-from-instance
                     (make-instance 'thread :board-id 1)))
         (pic (store! image))
         (new-comment (make-instance 'comment
                                     :thread-id thread-id
                                     :author author :email email
                                     :subject subject :body body
                                     :date-time (now)
                                     :image pic)))
...
```

Note the image validation function I was talking about. That makes sure that the file coming at `file-tuple->image-upload` is only going to be one of the options it can handle.

That should be that. If you head over to your browser now, you should be able to upload a PNG and see it kind of shittily rendered in the middle of your comment. If you'd rather do without the "shittily", add these two lines to your cl-chan.css

```css
.pic { float: left; }
.clear { clear: both; }
```

![A screenshot of cl-chan kinda working](/static/img/cl-chan--screenshot1--png.png)

Whew!

Ok, before you relax, remember, we've still got two more image types to handle, and one subtle bug to fix. Lets add those image types first.

```lisp
;;; image.lisp

...

;;;;;;;;;; JPGs
(defclass jpg (image-upload) ())
(defclass image/jpeg (jpg) ())
(defclass image/pjpeg (jpg) ())

(defmethod store-images! ((img jpg))
  "Saves a large version, and creates a preview of the given image in directories specified by the *big-dir* and *preview-dir* config variables"
  (let ((pic-name (make-pathname :name (name img) :type "jpg")))
    (copy-file (file-path img) (merge-pathnames pic-name *big-dir*))
    (let* ((pic (read-image-file (merge-pathnames pic-name *big-dir*)))
           (w (ch-image:image-width pic))
           (h (ch-image:image-height pic)))
      (multiple-value-bind (new-width new-height) (new-dimensions 250 w h)
        (write-image-file (merge-pathnames pic-name *preview-dir*)
                          (ch-image:resize-image pic new-height new-width))))
    (namestring pic-name)))
```

also, we need to modify the validation functions on our formlets.

```lisp
;;; cl-chan.lisp

(defun validate-image (hunchentoot-file-tuple)
  (or (null hunchentoot-file-tuple)
      (and (funcall (file-type? "image/x-png" "image/png" "image/jpeg" "image/pjpeg") hunchentoot-file-tuple)
           (funcall (file-smaller-than? 3000000) hunchentoot-file-tuple))))

...
     (image file :validation (#'validate-image "We accept PNGs or JPGs smaller than 3MB"))
...
     (image file :validation ((file-type? "image/x-png" "image/png" "image/jpeg" "image/pjpeg")
                              "You need to upload an image of type PNG or JPG"
                              (file-smaller-than? 3000000) "Your file needs to be smaller than 3MB"))
...
```

That should do it.

![A different screenshot of cl-chan kinda working](/static/img/cl-chan--screenshot2--jpg.png)

We implemented JPGs ahead of GIFs because it's much closer to the PNG scaling we already did, but note the number of differences there.

1. the `width` and `height` arguments are ordered `y x` rather than `x y`
2. the `pathname` and `image` parameters to the write function are in a different order
3. the functions involved in reading/writing/resizing are named differently
4. the steps we need to take are in a different order because ch-image can't seem to read image files with no file extension. That means we copy the big one first, rename it, then read that instead of the original temp file

Those are all small differences that you nevertheless need to get right if you don't want a face full of errors or odd results (like that screenshot above) when you start your board up. Before we tackle GIFs and that subtle bug, let me just point out that someone who picked our "Lazy Bastard" route earlier has saved themselves all of this trouble, and probably has higher quality previews to boot. But we're here to learn things, so we're going the hard way. Now then.

```lisp
;;; images.lisp
...
;;;;;;;;;; GIFs
(defclass image/gif (image-upload) ())

(defmethod store-images! ((img image/gif))
  "Saves a large version, and creates a preview of the given image in directories specified by the *big-dir* and *preview-dir* config variables"
  (let* ((pic (load-data-stream (file-path img)))
         (first-frame (aref (skippy:images pic) 0))
         (width (skippy:width pic))
         (height (skippy:height pic))
         (pic-name (make-pathname :name (name img) :type "gif")))
    (copy-file (file-path img) (merge-pathnames pic-name *big-dir*))
    (multiple-value-bind (new-w new-h) (new-dimensions 250 width height)
      (let ((new-pic (skippy:make-data-stream
                      :width new-w :height new-h
                      :color-table (skippy:color-table pic))))
        (skippy:add-image
         (skippy:composite first-frame
                           (skippy:make-image :width new-w :height new-h)
                           :width new-w :height new-h)
         new-pic)
        (output-data-stream new-pic (merge-pathnames pic-name *preview-dir*))))
    (namestring pic-name)))
```

and the appropriate formlet changes. In fact, we'd really better pull out the image types into a separate variable so that we only need to change them in one place.

```lisp
;;; package.lisp

...
(defparameter *allowed-image-fn*
  (file-type? "image/x-png" "image/png" "image/jpeg" "image/pjpeg" "image/gif"))

(defparameter *image-message*
  "You need a PNG, JPG or GIF smaller than 3MB")
...

;;; cl-chan.lisp

(defun validate-image (hunchentoot-file-tuple)
  (or (null hunchentoot-file-tuple)
      (and (funcall *allowed-image-fn* hunchentoot-file-tuple)
           (funcall (file-smaller-than? 3000000) hunchentoot-file-tuple))))

(define-formlet (post-comment-form)
...
     (image file :validation (#'validate-image *image-message*))
...

(define-formlet (post-thread-form)
      (image file :validation (*allowed-image-fn* *image-message*
                              (file-smaller-than? 3000000)
                              "Your file needs to be smaller than 3MB"))
...
```

And that's that.

![Yet another screenshot of cl-chan kinda working](/static/img/cl-chan--screenshot3--gif.png)

You'll notice that the GIF resizing process is a lot more complicated than what we had to do for PNGs or JPGs. That's because GIFs are potentially animated, so the Lisp library that handles them treats them as streams of images. That allows for better frame control, but it does mean that we need to

1. load the stream
2. pull out the first frame
3. resize that frame[^actually-theres-no]
4. shove it into a fresh stream
5. write that new stream of one image

[^actually-theres-no]: Actually, there's no usable resize or scale option, so we just crop it to the top left.

Once again, notice how much work the Lazy Bastards have saved themselves with that one line of code. Incidentally, had I known about Skippy's lack of ability to scale an image down, I probably would have gone with the lazy option myself and chucked portability in a fucking bin.

It's already done, so no sense in tearing out half of this column now. Especially since its been a good what... year and a half since I started it? Yeah, sounds like it's about time to [get the FILDI out](http://www.youtube.com/watch?v=RYlCVwxoL_g).

Really quickly before we go, I mentioned a subtle bug. We're letting Hunchentoot generate tempnames for our files. That's very simple, since we do *nothing*, but its internal name counter gets reset every time it shuts down. Which means that if you shut it down in production, new images are going to start clobbering your old ones. The easiest way to solve this is appending a `timestamp` to them. Between that and Hunchentoots' internal temporary file counter, we should be set in terms of unique names. Given how we use the `name` field of the `image-upload` class, the simplest way to do this is actually in `file-tuple->image-upload`.

```lisp
(defun file-tuple->image-upload (hunchentoot-file-tuple)
  (destructuring-bind (file-path original-file-name mimetype) hunchentoot-file-tuple
    (make-instance (intern (string-upcase mimetype) :cl-chan)
                   :name (format nil "~a-~a" (file-namestring file-path) (get-universal-time))
                   :file-path file-path)))
```

Note the change in the line starting with `:name`. Ok, I've had enough of this bullshit. New codebase [up at github](https://github.com/Inaimathi/cl-chan), now get out.

Tune In Next Time _(heh heh)_[^note-from-the-future] For:

- finally getting to multiple boards!
- walking through the lazy option in its entirety!
- some better usability and UI, maybe!

[^note-from-the-future]: I'm rightly chuckling, because as of June 2016, there is no such thing as "Next Time".
