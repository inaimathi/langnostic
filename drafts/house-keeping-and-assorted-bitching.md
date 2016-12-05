So much for updates becoming a regular event I guess. I'm not sure it's for lack of effort; I really do want to write more about my exploits. But it seems like the more I learn, the more I realize how little I know, and that knowledge silences a lot of my output. It seems like a waste, because it's almost certainly not the case that [the stuff I've been learning is common knowledge](http://www.cs.cmu.edu/~rwh/pfpl.html), but most of it would end up being links to specific pages of some dense comp-sci tome or paper, and I'm not sure how useful that would be. So, lets keep it concrete, I guess.

For a massive change, I've been doing some light mobile development. And what I've learned is that it's absolute garbage.

## Why Mobile Development Is Absolute Garbage

For starters, the toolchain is a pile of ass. In order to install and get running a full environment, you need to get [the `SDK`](https://developer.android.com/studio/intro/update.html), [the "IDE"](https://developer.android.com/studio/index.html), [the appropriate emulator image](https://developer.android.com/studio/run/managing-avds.html), and a giant pile of external dependencies, some of which are pinned at odd intermediate versions. As a sidenote, not all of the emulator images are available for every architecture/CPU, and there's no word of warning about this until _after_ you've downloaded the relevant multi-gigabyte files. Thank god for good wi-fi, I guess. The hands down most useful installation guide I've come across so far is [this massive wall of text on SO](http://stackoverflow.com/documentation/android/85/getting-started-with-android/9496/android-programming-without-an-ide#t=20161129053336634177), which explains how the installation runs step-by-step, without relying on the poorly-thought-out automated downloaders.

Once you have all of that stuff down, starting and understanding a project is another herculian task. It's possible that today's Java developers are just so resigned to boilerplate and slog that this doesn't bother them too much, but I've yet to meet one of these people in real life, so I'm not yet inclined to believe they exist. The short, or at least somewhat understandable non-video, version is in [that same massive wall of text on SO](http://stackoverflow.com/documentation/android/85/getting-started-with-android/9496/android-programming-without-an-ide#t=20161129053336634177). That explains how to get a project more-or-less working, but _doesn't_ explain what massive mis-firing of brain-power caused this process to be so byzantine and horrific that it requires this much exposition.

What we ended up doing for the project I'm on is using [React Native](https://facebook.github.io/react-native/). Because, while all of the above is still a requirement, once you have it done, you can at least skip the steps that involve manually mucking with several XML-formatted build manifest files and instead write some fucking javascript. Of course, that's a little pre-mature. We haven't factually got as far as running anything _on a device_ yet, but I'm hoping that the process isn't going to be too stress-inducing.

Honestly, if it weren't for the fact that people other than me need to work on this in the very short term, I very probably would have built the fucker [in Clojure](http://clojure-android.info/) and called it a day. Not only is the toolchain more easily installed on a `debian` machine, it mostly works the way you'd expect from other Clojure development, and compiling down to a device is a matter of `lein droid doall`. It's something I plan to explore for myself even outside the context of this particular contract, but I still find it vaguely unsatisfying that this is even necessary. I'm sure this is how Lispers felt around the 80s and 90s with the rise of personal computers. "Your build-chain involves how many external tools? You have to _allocate memory yourself_? And you can't even **introspect on your running image**? How do you get anything done?" Those are all more or less the objections I'm making here at the next step. I'm not convinced they're wrong.

## [`house`](https://github.com/inaimathi/house)-keeping

You may or may not have noticed a few commits to the [`house`](https://github.com/inaimathi/house) server. Mostly as a result of developments in [`cl-congregate`](https://github.com/inaimathi/cl-congregate). That is, I'm doing my usual and adding features to the underlying substrate exactly as quickly as I need them, and no quicker. They started as concrete requirements for the congregate project, and ended with me addressing some noticeably absent pieces from the `house` system.

### In-handler redirects

It used to be possible to set up `redirect-handler`s in `house`, but I hadn't considered the use case wherein a particular page might need to conditionally redirect the client elsewhere. One part of this is that there needs to be a way to `redirect!` internally to a handler.

```lisp
;;; define-handler.lisp
...
(defun redirect! (target &key permanent?)
  (make-instance
   'response
   :response-code (if permanent? "301 Moved Permanently" "307 Temporary Redirect")
   :location target :content-type "text/plain" :body "Resource moved..."))
...
```

That's the easy and obvious bit; take a `target` URI and a `permanent?` flag, and return a `response` object encoding either a `301` or `307` HTTP response as appropriate. The slightly less obvious part is that `make-handler` expects the result of evaluating `body` to be a string, which it then wraps in a `response`. So, that needs a slight tweak. Instead of always expecting a string, we need to `typecase` the result of `body`, and return it unmodified if it's already a `response` object.

```lisp
;;; define-handler.lisp
...
		   `(let* ((result (progn ,@body))
			   (response
			    (if (typep result 'response)
				result
				(make-instance
				 'response
				 :content-type ,content-type
				 :cookie (unless ,cookie? (token session))
				 :body result))))
		      (write! response sock)
		      (socket-close sock))))))
...
```

This is going to be one of those internal implementation details that I'm probably going to want to optimize away later, because between creating an intermediate string where one probably isn't necessary and that runtime type dispatch, I get the feeling we're losing some performance here. I'm waiting on one or two more revisions to submit this thing to `quicklisp`, then start running it through a profile on a regular basis. That seems like a decent next step.

### Path variables

I'm sure the `house` users out there were about as annoyed as I was at the lack of URL path variables in `define-handler`. Especially since [`cl-handlers`](https://github.com/inaimathi/cl-handlers) perfectly outlines how to implement a system that gives you all of

1. minimal repetition between routing, validating and business-logic code
2. decentralized routing table construction
3. consistent path collision detection
4. fast handler lookup
5. path variables

Point #1 is what I find myself missing from a lot of other web frameworks I end up having to use these days. They do often have handlers separated out, but have this weird habit of forcing you, the programmer, to

- manually assert that each required incoming parameter is present
- if so, manually assert that each parameter parses into the expected type of object you're expecting
- if so, run the actual business logic required of this particular handler
- otherwise return the appropriate HTTP-level error

The main goal of `house` was to eliminate this extremely repetitive and annoying pattern by letting you annotate parameter names with their expected types. At that point, `make-handler` can expand those into the appropriate HTTP-related logic for you, without you needing to worry on it further. Because we want to integrate the steps involved in processing parameters somehow, it also makes sense to break out the handler table. Specifically, frameworks like [`compojure`](https://github.com/weavejester/compojure) or [`rails`](#)[^no-im-not] like to cluster the routing table in some central structure that ends up owning its own file in any project of sufficient size. This file then inevitably becomes the hands-down number one source of merge collisions I've seen. Ostensibly, this is in the name of avoiding route collisions, which would be much harder to detect if that central routing structure was split across many files.

[^no-im-not]: No, I'm not giving them any more mind-share or traffic. Go google it if you like, but I'm not helping you injure yourself by working with this thing.

Except, we can solve this with the appropriate data-structure. The [`cl-handlers`](https://github.com/inaimathi/cl-handlers) approach is to implement the handler table as a [trie](https://www.topcoder.com/community/data-science/data-science-tutorials/using-tries/) of path components. Which at once solves the problem of path collision-detection, and fast handler lookup. Collision detection is solved because we can easily see whether a particular walk through the handlers trie already identifies a handler at the time we insert a new one (and can then immediately issue an `error` or  `warning` as necessary). Fast lookup is solved in the sense that looking up a particular handler in our structure depends on the length (in path components) of the path we're looking up, rather than the total number of handlers we're defining. This _is_ slower than the previous `gethash` strategy that `house` was using, but critically, will allow us to pick out components of the path to use as input to the handler we eventually find.

Again, [`cl-handlers`](https://github.com/inaimathi/cl-handlers) did all of this appropriately. So I've just lifted the code involved.

```
;;; handler-table.lisp
(in-package :house)

;;;;; A minimal, custom Trie
;;;;;;;; (It needs to allow for variables at each level, including prospective matching of the rest of a URI segment)
(defstruct trie
  (value nil)
  (map (make-hash-table :test 'equal))
  (vars (make-hash-table)))

(defun any-vars? (trie)
  (> (hash-table-count (trie-vars trie)) 0))

(defun path-var? (str)
  (and (stringp str)
       (> (length str) 0)
       (eql #\- (char str 0))))

(defun var-key (str)
  (let ((pair (split-at #\= (string-upcase (subseq str 1)))))
    (intern (car pair) :keyword)))

(defun trie-insert! (key value trie)
  (labels ((rec (key-parts trie)
             (cond ((null key-parts)
                    (setf (trie-value trie) value))
                   ((path-var? (first key-parts))
                    (next! (var-key (first key-parts)) (rest key-parts) (trie-vars trie)))
                   (t
                    (next! (first key-parts) (rest key-parts) (trie-map trie)))))
           (next! (k rest map)
             (let ((next (gethash k map)))
               (if next
                   (rec rest next)
                   (rec rest (setf (gethash k map) (make-trie)))))))
    (rec key trie)
    trie))

(defun trie-lookup (key trie)
  (labels ((rec (key-parts trie bindings)
             (if key-parts
                 (let ((next (gethash (canonical (first key-parts)) (trie-map trie))))
                   (cond (next
                          (rec (rest key-parts) next bindings))
                         ((any-vars? trie)
                          (loop for k being the hash-keys of (trie-vars trie)
                             for v being the hash-values of (trie-vars trie)
                             do (multiple-value-bind (val bindings)
                                    (rec (rest key-parts) v (cons (cons k (first key-parts)) bindings))
                                  (when val
                                    (return-from trie-lookup (values val bindings))))))
                         (t
                          nil)))
                 (values (trie-value trie) bindings)))
	   (canonical (thing)
	     (typecase thing
	       (string (string-upcase thing))
	       (t thing))))
    (rec key trie nil)))

;;;;; And using it to structure our handler table
(defclass handler-table ()
  ((handlers :initform (make-trie) :initarg :handlers :reader handlers)))

(defun empty () (make-instance 'handler-table))

(defparameter *handler-table* (empty))

(defmethod process-uri ((uri string)) (split-at #\/ (string-upcase uri)))
(defmethod process-uri ((uri symbol)) (process-uri (symbol-name uri)))

(defun insert-handler! (uri handler-fn &key (handler-table *handler-table*))
  (trie-insert! uri handler-fn (handlers handler-table))
  handler-table)

(defun find-handler (method uri-string &key (handler-table *handler-table*))
  (let ((split (split-at #\/ uri-string))
	(handlers (handlers handler-table)))
    (or (trie-lookup (cons method split) handlers)
	(trie-lookup (cons :any split) handlers))))

(defmacro with-handler-table (tbl &body body)
  `(let ((*handler-table* ,tbl))
     ,@body))
```

This doesn't bother with `cl-handlers`' error handling strategy of having a separate errors table, because `house` handles HTTP-level errors above the routing level. It would eventually be nice to be able to specify your own `not-found` handler, but I'll leave that feature for when I end up needing it.

I'm not sure if this code is complicated enough to benefit from my usual almost-literate style, but lets do it anyway. I miss the form.

```lisp
...
(defstruct trie
  (value nil)
  (map (make-hash-table :test 'equal))
  (vars (make-hash-table)))

(defun any-vars? (trie)
  (> (hash-table-count (trie-vars trie)) 0))
...
```

First things first, a `trie` is a thing that has a `value`, a `map` (the usual second trie component) and some `vars`. This is not a usual Trie because of that last chunk. Essentially, we're separating out handlers that have variable path components at each stage. As you'll see later, this will let us say something like

1. Match the current path component literally
2. If no literal matches are found, prospectively try matching against each variable component we know about.

This is what will let us support path variables.

`any-vars?` is just a piece of minor utility to make it easier to check whether a given tier of a `trie` has any variables in it. We'll do this occasionally, because it's possible to skip some work in `trie`s that only bind constant path components.

```lisp
...
(defun path-var? (str)
  (and (stringp str)
       (> (length str) 0)
       (eql #\- (char str 0))))

(defun var-key (str)
  (let ((pair (split-at #\= (string-upcase (subseq str 1)))))
    (intern (car pair) :keyword)))
...
```

The functions `path-var?` and `var-key` implement the path variable syntax[^or-rather-part]. In particular, `path-var?` states that a path component starting with `-` is a variable, while `var-key` specifies that the variable name is separated from its type annotation by an `=`. Which means that path parameters in this syntax look like `-name=string` or `-id=integer` or `-arglebargh=a-user-defined-type`[^a-note-on-syntax].

[^or-rather-part]: Or rather, part of it. There's a piece over in `define-handler` that does the job of extracting type annotations from path variables for the purposes of setting up checking machinery, but it should almost certainly be a third function here instead. This is a note to self for the future; I won't belabor it further in this post.

[^a-note-on-syntax]: I would very probably gone with the more commonly seen `:var-name::type` syntax for defining handlers, except that this would prevent me from allowing handler URIs to be defined as symbols in the `define-handler` macro. This is because `:` is a piece of syntax reserved for `keyword` symbols in Common Lisp, so the reader would complain unless they were escaped in the middle of said URIs. Specifically, evaluating `:foo::bar` at the REPL throws the error `too many colons in :FOO`. This makes it a non-starter here, but this choice would work perfectly fine in Scheme or Clojure.

The next function is `trie-insert!`, and it does exactly what you think it does given the additional constraints in place here.

```lisp
...
(defun trie-insert! (key value trie)
  (labels ((rec (key-parts trie)
             (cond ((null key-parts)
                    (setf (trie-value trie) value))
                   ((path-var? (first key-parts))
                    (next! (var-key (first key-parts)) (rest key-parts) (trie-vars trie)))
                   (t
                    (next! (first key-parts) (rest key-parts) (trie-map trie)))))
           (next! (k rest map)
             (let ((next (gethash k map)))
               (if next
                   (rec rest next)
                   (rec rest (setf (gethash k map) (make-trie)))))))
    (rec key trie)
    trie))
...
```

If it weren't for the variables we want to bind on later, you might expect that `rec` function to have only two branches. However, given our situation, we have to check if the next path component is a `path-var?`. If it is, we get the `var-key` out of it, and propagate the rest of the components under it.

Ok, this is where it gets a bit interesting and complicated.

```lisp
(defun trie-lookup (key trie)
  (labels ((rec (key-parts trie bindings)
             (if key-parts
                 (let ((next (gethash (canonical (first key-parts)) (trie-map trie))))
                   (cond (next
                          (rec (rest key-parts) next bindings))
                         ((any-vars? trie)
                          (loop for k being the hash-keys of (trie-vars trie)
                             for v being the hash-values of (trie-vars trie)
                             do (multiple-value-bind (val bindings)
                                    (rec (rest key-parts) v (cons (cons k (first key-parts)) bindings))
                                  (when val
                                    (return-from trie-lookup (values val bindings))))))
                         (t
                          nil)))
                 (values (trie-value trie) bindings)))
	   (canonical (thing)
	     (typecase thing
	       (string (string-upcase thing))
	       (t thing))))
    (rec key trie nil)))
```

If we can find a literal path component at a given `trie` level that matches the next key component, we recur into it. Otherwise, we try to match against the variables interned at this level of the `trie` by prospectively recurring into each sub-`trie` that leads out from it. We need to do that, because we want to handle the situation wherein there is a variable comonent potentially followed by many possible path components. For instance, `-group/view` and `-group/list`. If we fail to match either case, we return `nil`. The check for the second branch is actually `any-vars?`. Because, if a literal match fails, and the current trie level has no variables, there's no point in trying further. The variable binding itself, if it comes to that, goes depth-first down each variable path and returns the first full match it finds.

Tadaah! That's it. That's the hard part. The rest of this is just the obvious plumbing for incorporating this lookup method into a larger server.

```lisp
(defclass handler-table ()
  ((handlers :initform (make-trie) :initarg :handlers :reader handlers)))

(defun empty () (make-instance 'handler-table))

(defparameter *handler-table* (empty))

(defmethod process-uri ((uri string)) (split-at #\/ (string-upcase uri)))
(defmethod process-uri ((uri symbol)) (process-uri (symbol-name uri)))

(defun insert-handler! (uri handler-fn &key (handler-table *handler-table*))
  (trie-insert! uri handler-fn (handlers handler-table))
  handler-table)

(defun find-handler (method uri-string &key (handler-table *handler-table*))
  (let ((split (split-at #\/ uri-string))
	(handlers (handlers handler-table)))
    (or (trie-lookup (cons method split) handlers)
	(trie-lookup (cons :any split) handlers))))

(defmacro with-handler-table (tbl &body body)
  `(let ((*handler-table* ,tbl))
     ,@body))
```

So we've got a `handler-table`, which we can create `empty` instances of. We've got `insert-handler!` that adds a new handler to a table, and we've got `find-handler`, which searches for one given a URI. We've also got the `process-uri` utility method for getting a URI into a trie-lookup-able form. Finally, we've got a `*handler-table*` special var that contains the default table, and a `with-handler-table` form you can use if you have other ideas.

### Cross-domain sessions

One of the things I want to do with `congregate` is put together an arbitrary subdomain system. So that you could point humans at your particular group by giving them a URL like `code-retreat.congregate.ca` instead of one more like `congregate.ca/groups/CA/ON/Toronto/code-retreat`. That particular feature requires two things; firstly, the ability to dispatch on other parts of an incoming request[^which-weve-had], and secondly, the ability to share a particular session cookie across multiple domains.

[^which-weve-had]: Which we've had for a little while at this point.

That second one was non-obvious to me, actually. The problem turns out to be with running implementations of OAuth, and in particular the one that [github](https://developer.github.com/v3/oauth/) provides. The trouble is that they allow exactly one callback URL, and that URL is forced to share a domain with the origin of the incoming authentication request[TODO - fact check]() or its set to your default.

In other words, if I set my authentication URL to `congregate.ca/auth/github/callback`, and someone tries to run through the auth process from `code-retreat.congregate.ca`, they'll get booted back to the root domain URL. Which by extension means that their browser won't send their session token along for the ride, because of the domain change. In other words, what we'd really want here is to be able to share a particular `house` session token across multiple domains[^determined-by-config].

[^determined-by-config]: Determined by some piece of server configuration, rather than a guessing mechanism, because I'd ideally like to include the domain `congregate.inaimathi.ca` in on the deal, and I'm not entirely convinced that there aren't other potential use cases for this.

Code-wise, this meant parsing cookies mildly differently than we had been, and generating cookie headers in a different way.

```lisp
;;; house.lisp

(defmethod parse-cookies ((cookie string))
  (loop for c in (split "; " cookie) for s = (split "=" c)
     if (and (string= "name" (first s)) (second s)) collect (second s)
     else collect c))

...
	   if (eq n :cookie) do (setf (session-tokens req) (parse-cookies value))
...
```

We used to just split on `"; "` and leave it at that, but we're about to start encoding them properly, so we can't be quite so lax anymore[^theres-one-more].

[^theres-one-more]: There's one more dispatch in there than there really ought to be. We're doing `else collect c`, because previous versions of `house` didn't bother setting a `name` key here, instead storing the raw session token. In an effort not to cause problems for people upgrading their distributions, we need to hadle both the old and new formats properly, which is why the alternative is there. I think I'll end up removing it later on, once this version has been out for long enough.

In addition to parsing cookie headers properly, we also need to *emit* them properly. Which is why `write!` now does the appropriate thing as part of header emissions.

```lisp
;;; house.lisp

...
(defmethod write! ((res response) (stream stream))
  (flet ((write-ln (&rest sequences)
	   (mapc (lambda (seq) (write-sequence seq stream)) sequences)
	   (crlf stream)))
    (write-ln "HTTP/1.1 " (response-code res))
    (write-ln "Content-Type: " (content-type res) "; charset=" (charset res))
    (write-ln "Cache-Control: no-cache, no-store, must-revalidate")
    (write-ln "Access-Control-Allow-Origin: *")
    (awhen (cookie res)
      (if (null *cookie-domains*)
	  (write-ln "Set-Cookie: name=" it)
	  (loop for d in *cookie-domains*
	     do (write-ln "Set-Cookie: name=" it "; domain=" d))))
    (awhen (location res)
      (write-ln "Location: " it))
    (when (keep-alive? res)
      (write-ln "Connection: keep-alive")
      (write-ln "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
    (awhen (body res)
	   (write-ln "Content-Length: " (write-to-string (length it)))
	   #-windows(crlf stream)
	   #+windows(format stream "~%")
	   (write-ln it))
    (values)))
...
```

Specifically,

```lisp
;;; house.lisp

...
    (awhen (cookie res)
      (if (null *cookie-domains*)
          (write-ln "Set-Cookie: name=" it)
          (loop for d in *cookie-domains*
		        do (write-ln "Set-Cookie: name=" it "; domain=" d))))
...
```

So, if there's a `cookie` set in the result, we check if there are any `*cookie-domains*` set. If not, we do the default thing of writing a single `Set-Cookie` header with the appropriate session token, and we make it available only to the origin domain (this is the default behavior, and we don't bother correcting it in this case). However, if there are any `*cookie-domains*`, we iterate through them and make sure that the given session token will be returned to each of the as part of the request header[^whether-this-actually].

[^whether-this-actually]: Whether this actually happens is, of course, a decision for the HTTP client we're communicating with. But today's popular web browsers should all respect the approach we're taking.

The last part of this change involves declaring that `*cookie-domains*` variable, which I've decided to do in `package.lisp`.

```
;;; package.lisp
...
(defparameter *cookie-domains* nil)
...
```

That's, kind of surprisingly, all. We didn't have to touch `session.lisp` at all in order to implement this domain change. While we're here though, there's one more issue I have with response `Header` handling...

### CORS headers

The CORS thing just outright sucks balls, as far as I'm concerned. There's a decent write-up of [how it works](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing) on wiki, but the _why_ of it escapes me. If it's a security feature, then it sounds misguided to give the origin server the ability to override it by sending back a particular header. If it's to protect servers from DDOS attacks/SNAFUs, then it seems to fail outright because the target still needs to read, buffer and parse the request before it can make the decision to throw it out on the basis of header content. It really seems that you'd always want `Access-Control-Allow-Origin: *` being sent over as part of the response[^reserve-the-right], so that's what `house` now does by default. The change to our `write!` method was a pretty straight-forward

```lisp
...
    (write-ln "Access-Control-Allow-Origin: *")
...
```

[^reserve-the-right]: Though, as always, I thoroughly reserve the right to be wrong about this.

## I Fucking Hate This Computer

The trackpad is the single most annoying thing I've had to deal with in recent memory. It seems to delight in teleporting my cursor to insane places while I'm typing away. I haven't gotten around to replacing those stripped screws either, which means that the chassis doesn't hold together quite as well as it should, which _in turn_ means that it's more difficult to use in transit[^because-it-keeps]. This in addition to its uncomfortably large size, extremely scuff-attracting metal finish and the surprisingly shoddy rubber feet on its underside[^three-of-which][^and-actually-nevermind].

[^because-it-keeps]: Because it keeps shifting around internally, and occasionally unseating the already annoying trackpad so as to prevent its tactile feedback.

[^three-of-which]: Three of which have actually fallen off at this point, and I'm considering just cutting the last one off and calling it a day.

[^and-actually-nevermind]: And actually nevermind, over the course of writing and editing this post, the last one fell off. I'm considering [gorilla-gluing](http://www.gorillatough.com/) the two that I still have back at some point to give the unit some semblance of traction while I work at a desk.

In summary, I can not recommend [this thing](https://puri.sm/products/librem-15/) with a straight face, and am seriously considering picking up one of [these](http://www3.lenovo.com/ca/en/laptops/thinkpad/thinkpad-x/X260/p/20F6CTO1WWENCA0) or [these](https://www.amazon.ca/Lenovo-Quad-Core-Processor-Bluetooth-Professional/dp/B01J5YMF90/ref=sr_1_1?s=pc&ie=UTF8&qid=1480397797&sr=1-1) fairly soon. The only thing I'll really miss the horse-power for is my Clojure development. Which is admittedly a large chunk of it these days, but I think I'll be able to deal somehow.
