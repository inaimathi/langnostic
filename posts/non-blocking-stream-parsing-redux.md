So I've got [the library](https://github.com/Inaimathi/cl-lazy-parse) mostly put together. Centralizing the state in our stream construct did the trick. `rapid`s now carry around a timestamp, a retry-count and a character allowance in service to erroring when any of those start looking suspicious

```lisp
...
(defclass rapid ()
  ((stream-of :reader stream-of :initarg :stream-of)
   (cached :reader cached :initform (buffer) :initarg :cached)

   (max-age :reader max-age :initarg :max-age)
   (created :reader created :initform (get-universal-time))
   (max-pauses :reader max-pauses :initarg :max-pauses)
   (pauses :accessor pauses :initform 0)
   (allowance :accessor allowance :initarg :allowance)))
...
```

I've also written two HTTP parsers with the system, now that I've solved what I consider to be the big problems. There's a stateless one that looks like

```lisp
...
(defparameter crlf>> (and>> #\return #\linefeed))

(defparameter param>>
  (and>> (many>> (none-of>> "=& ")) "=" (many>> (none-of>> "& ")) (optionally>> #\&)))

(defparameter request-line>>
  (and>> (or>> "GET" "POST" "PUT" "DELETE") " " 
	  (many>> (none-of>> "? "))
	  (optionally>> (snd>> (and>> "?" (many>> param>>))))
	  " HTTP/1.1" crlf>>))

(defparameter header>>
  (and>> (many>> (char>> #'header-char?)) ": " (many>> (none-of>> '(#\return #\linefeed))) crlf>>))

(defparameter request-stateless>>
  (and>> request-line>>
	 (many>> header>>)
	 crlf>>
	 (optionally>> (many>> param>>))))
...
```

and a stateful one that looks quite a bit hairier

```lisp
...
(defun request>> ()
  (let ((method) (uri)
	(parameters)
	(headers) (content-length 0) (content-type))
    (let ((par>>
	   (with param>>
		 (_fn (k _ v _)
		   (push (cons (to-key k) (to-string v)) parameters)))))
      (with (and>> 
	     ;;; request-line
	     (with (or>> "GET" "POST" "PUT" "DELETE")
		   (lambda (&rest m) 
		     (setf method (to-key m))))
	     " " (many>> (none-of>> "? ")) (optionally>> (and>> "?" (many>> par>>)))
	     " HTTP/1.1" crlf>>

	     ;;; headers
	     (many>> 
	      (with header>>
		    (_fn (key _ val _)
		      (let ((k (to-key key))
			    (v (to-string val)))
			(case k
			  (:content-length (setf content-length (parse-integer v)))
			  (:content-type (setf content-type v))
			  (t (push (cons k v) headers))))
		      nil)))

	     ;;; body
	     (if>> (and (eq method :post) (> content-length 0))
		   (and>> crlf>> (many>> par>>))))
	    (_fn (&rest _)
	      (list :method method :uri uri :headers headers :parameters (reverse parameters)))))))
...
```

They'll both do the right thing, but the stateless version is a bit more liberal in accepting input, since it doesn't check whether the gicven request is a `POST`, or whether it purports to have a `Content-Length` greater than `0`. I think that'll hang it in a few cases that the stateful one will handle, but haven't gone through the step of testing either one with *real* requests. I'm still trying to make up my mind about whether it's better to synthesize results as part of individual parsers as you go, or give the top-level as raw an output as you can manage. Pros and cons both ways. On the one hand, deferring that work gives you more flexible and reusable parsers *(for instance, in places where you have the same incoming construct in multiple cases, but want to produce differently grouped output)*, while doing it up front means you'll have much less work to do at the top-level.

I've gotta be honest though, after some initial interest in these ideas, I kind of got side-tracked by a [few](https://500px.com/) [other](https://github.com/Inaimathi/serve-sml) [things](http://www.vips.ecs.soton.ac.uk/supported/current/doc/html/libvips/using-from-python.html). Which I'll hopefully be writing about at some point fairly soon. Streaming parsers kind of figure into one of those. So I might come back around to this project at some point, but it probably won't be in the near term.
