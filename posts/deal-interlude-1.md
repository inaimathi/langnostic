Just a heads up: you won't see any of the below code checked into [the Deal repo](https://github.com/Inaimathi/deal) quite yet. And [production](http://deal.inaimathi.ca/) still uses the [nginx PushStream module](https://github.com/wandenberg/nginx-push-stream-module) for its asynchronous needs, and will continue to do so either until I finish enough of the other tasks to pull back enough time to re-structure its server, or until the contest ends and I no longer have to worry about making fast forward progress. Once I get around to it though, yes the real, actual Deal server is going to use this approach. It'll still use nginx as a reverse proxy to make sure static files are being served as fast as possible, but it won't rely on external SSE machinery longer than it has to. Not that the machinery's bad, mind you. It just complicates deployment more than I'd like.

### <a name="why-and-when"></a>Why and When

Lets just get this out of the way. Yes, there [already exist](http://weitz.de/hunchentoot/) some [pretty](http://www.cliki.net/araneida) good [general-purpose](https://github.com/orthecreedence/wookie) web servers written in Common Lisp. The reason you'd build your own is if you had a very particular purpose in mind. If you're out to host a vanilla web site, or a traditional stateless, HTML-emitting web application, you'd already have your bases covered. But imagine you were building an application that had a heavy focus on JSON-interaction and session-tracking, with built-in support for SSEs. Say for the sake of argument, [something like](https://github.com/Inaimathi/strifebarge) a [web-game](https://github.com/Inaimathi/deal). You probably *could* make one of the existing things work for you. If you tried hard enough, and were willing to hack in support for one or two things<a name="note-Sun-Sep-15-225241EDT-2013"></a>[|1|](#foot-Sun-Sep-15-225241EDT-2013). But as you can see by reading the [`define-handler` mini-language](https://github.com/Inaimathi/deal/blob/master/define-handler.lisp) I put together for Deal, using a general purpose server for such a specific task incurs complexity penalties that you could completely sidestep by building something minimal and specialized.

That's the point at which I'd start aiming for a goal like this. When using the existing, tested, reasonably-performing options is introducing conceptual and deployment complexity into my project. And, just in case you [missed it](https://github.com/Inaimathi/deal#installation), it is.

### <a name="gotchas"></a>Gotchas

HTTP is old. A child of [the mid-ninties](http://www.w3.org/Protocols/HTTP/1.0/spec.html). There are people alive and programming professionally today who were born after it was created. Lisp is [a bit](http://en.wikipedia.org/wiki/Common_Lisp) older. And I can only assume no one working on its standardization knew how big the web would actually get, so they made a couple choices that must have made sense to them, but that will annoy the fuck out of you if you need to generate valid HTTP responses from a CL application.

`#\newline` is a "platform independent line break", you see. It expands to `#\linefeed`, `#\return` or `#\return #\linefeed` depending on where and how you write it. And the `format` directive `~%` outputs a `#\newline`. This is relevant because HTTP specifies terminators for header lines and request bodies as `#\return #\linefeed`, which means that if you were expecting an expression like `(format stream "~a ~a~%~{~a~%~}~%~%~a~%~%" protocol response-code headers body)` to output a valid response string, I have [some news for you](http://stackoverflow.com/a/445712/190887).

It's not [particularly easy](http://stackoverflow.com/questions/2619172/common-lisps-equivalent-of-r-inside-the-format-function) to do this sort of formatting with the `format` function, ironically. There *aren't* directives like `~%` specified for `crlf`, or even for `#\return` and `#\linefeed` individually. About the best you can hope to do using the defaults is `(format t "Hello world.~C~C" #\return #\linefeed)`, which is a shit sandwich if I ever saw one.

So. Step one, if you're going to be hacking on HTTP from lisp is to define a shortcut for that. It can either be something like

```lisp
(defun crlf (&optional (stream *standard-output*))
  (write-char #\return stream)
  (write-char #\linefeed stream))
```

or it can be something like

```lisp
(defconstant crlf (list #\return #\newline))

(defun cat (&rest things)
  (apply #'concatenate 'string things))
```

I'm not committed yet, and may just go with defining all of the above. The `crlf` function looks like it would be more useful for an approach where I tried to do as close to single-traversal output as possible, while the `crlf` constant/`cat` function pair looks like they'd be more useful for testing purposes. Like I said, they don't really get in each others' way, so "both" is not, in fact, an unreasonable choice.

That out of the way, lets spend just a moment discussing...

### <a name="the-approach"></a>The Approach

As far as I can tell, there are two legitimate approaches to building an async server in Common Lisp.


-   First, you could install `libevent-core-2.0` and `libevent-extra-2.0`, and make use of the [`cl-async`](https://github.com/orthecreedence/cl-async) bindings. This way seems to save you having to buffer messages yourself, and it probably provides better performance at the expense of a very mildly complicated deployment.
-   Second, you could go pure Lisp and build the whole thing out of [`usocket`s](http://common-lisp.net/project/usocket/).


I'm taking that second approach in this article, but you can use the same theory to construct a `cl-async`-based equivalent without too much trouble.

### <a name="now-then"></a>Now Then

We're building a toy example. The simplest async server that can possibly be constructed while remaining worthy of the name. We're going to have three applicable handlers:


-   one to serve up a basic front end, which will use JavaScript to connect to our subscription handler and get future messages from our server
-   one to handle that subscription
-   one to trigger a message send to all existing listeners


nothing fancy like user-specified messages, or multiple channels, both of which will be fairly easy changes once you understand what the basic server structure is. To start with, we need to generate responses, which means tacking a body message onto some situation-dependent HTTP headers, and handling the above `crlf` problems elegantly. So, here are the basics:

```lisp
(ql:quickload (list :cl-ppcre :usocket :cl-who :parenscript :babel))
(defpackage :ts-usocket (:use :cl :cl-ppcre :usocket :cl-who :parenscript :babel))
(in-package :ts-usocket)

(defvar crlf (list #\return #\linefeed))
(defparameter day-names '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter month-names '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun http-date ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~a, ~a ~a ~a ~a:~a:~a GMT~@d"
            (nth day-of-week day-names) date
            (nth month month-names) year hour minute second (- tz))))

(defun cat (&rest seqs)
  (apply #'concatenate 'string seqs))

(defun response (&key (response-code "200 OK") (content-type "text/html") keep-alive? body)
  (cat "HTTP/1.1 " response-code crlf
       "Content-Type: " content-type "; charset=utf-8" crlf
       "Cache-Control: no-cache, no-store, must-revalidate" crlf
       (when keep-alive?
         (cat "Connection: keep-alive" crlf
              "Date: " (http-date) crlf
              "Expires: Thu, 01 Jan 1970 00:00:01 GMT" crlf))
       (when body 
         (cat "Content-Length: " (write-to-string (length body)) crlf crlf 
              body crlf)) 
       crlf))
```

`crlf` and `cat` are pretty self-explanatory. The `response` function is going to take some basic options, populated with sane defaults, and use them to generate appropriate HTTP headers. The `http-date` function just formats the current date in a particular format. In a real implementation, I'd probably end up ditching this one. Its presence/absence doesn't seem to make much of a difference, and it seems like it might be resource intensive<a name="note-Sun-Sep-15-225317EDT-2013"></a>[|5|](#foot-Sun-Sep-15-225317EDT-2013). Next up, lets set up our three responses:

```lisp
(defparameter *channel* nil)

(defparameter interface
  (response 
   :body 
   (with-html-output-to-string (str nil :prologue t)
     (:html
      (:head (:title "Test page"))
      (:body
       (:div :id "console")
       (:script
        :type "text/javascript"
        (str (ps (defvar src (new (-event-source "/sub")))
                 (defun p (msg)
                   (let ((elem (chain document (get-element-by-id "console"))))
                     (setf (@ elem inner-h-t-m-l)
                           (+ (@ elem inner-h-t-m-l) "&lt;p>" msg "&lt;/p>"))))
                 (setf (@ src onerror)
                       (lambda (e) 
                         (p "ERROR OCCURRED...")
                         (p (chain -j-s-o-n (stringify e))))
                       (@ src onopen)
                       (lambda (e) (p "STREAM OPENED..."))
                       (@ src onmessage)
                       (lambda (e) 
                         (p "GOT MESSAGE!")
                         (p (+ "data: " (@ e data)))))))))))))

(defun subscribe! (sock)
  (let ((s (socket-stream sock)))
    (write-string (response :keep-alive? t :content-type "text/event-stream") s)
    (force-output s)
    (push sock *channel*)))

(defun publish! (msg)
  (loop for sock in *channel*
     do (handler-case
            (ignore-errors
              (let ((s (socket-stream sock)))
                (write-string (cat "data: " msg crlf crlf) s)
                (force-output s)))
          (error (e)
            (format t "~s" e)
            (setf *channel* (remove sock *channel*))))))
```

The `interface` is just a flat file. We'll be sending out exactly the same one to anyone that asks for it, and it would be fairly resource-intensive to generate that each time, so I just cached the result of a `response` call. What you see in the `ps` there is a call to create a new `EventStream`, and calls to attach `onopen`, `onerror` and `onmessage` listeners to it. When the client trips any of those events, it'll add some relevant information to the `innerHTML` of a `div` with id `console`<a name="note-Sun-Sep-15-225327EDT-2013"></a>[|6|](#foot-Sun-Sep-15-225327EDT-2013). The `subscribe!` function sends headers appropriate for starting an SSE stream, calls `force-output` then pushes the relevant socket into `*channel*`. Finally, `publish!` takes a message and sends it out to each member of `*channel*`. If the write errors, the relevant socket is removed from `*channel*`.

Notice that we're doing literally everything so far in a very naive way. The `http-date` mechanics could be made much more efficient, or removed entirely with little negative effect, we're calling `concatenate 'string` like it's going out of style, and we're calling `remove` on each and every socket removal. It's just conceptually easier this way. Once we get to a reasonably well-tested server, we'll go back and make sure that we traverse messages as little as possible, writing directly where we can, and make sure to collect running sockets for re-assignment rather than incurring a traversal per removal. Maybe. If the end result doesn't profile well enough.

Don't worry about it for now. Next up is the actual handler.

```lisp
(defmethod handle-request (socket request-uri)
  (let ((s (socket-stream socket)))
    (cond ((string= "/sub" request-uri)
           (subscribe! socket))
          ((string= "/pub" request-uri)
           (publish! (format nil "Got a message! ~a" (gensym)))
           (write-string (response 
                          :content-type "text/plain"
                          :body "Published!") s)
           (socket-close socket))
          (t
           (write-string interface s)
           (socket-close socket)))))
```

That'll do it. If we get a request for `/sub`, we just pass the socket off to the `subscribe!` function, and pointedly *don't* close it. If we get one for `pub`, we `publish!` a unique message, and send a small, token response to the current requester, then close the socket. Note that if we're closing the connection right away, we don't need to worry about `force-output` calls, since that automatically gets done on cleanup. Lastly, if we get a request for any other resource, we send out the `interface` we defined earlier.

We're almost done. We need exactly one more component; a piece that'll monitor a particular port, buffer incoming HTTP requests and call the handler when it gets a completed one.

```lisp
(defvar *socket-handle* nil)

(defun stop ()
  (when *socket-handle*
    (loop while (socket-close *socket-handle*))
    (setf *socket-handle* nil
          *channel* nil)))

(defclass buffer ()
  ((contents :accessor contents :initform nil)
   (started :reader started :initform (get-universal-time))
   (state :accessor state :initform :empty)))

(defmethod buffered-read! (stream (buffer buffer))
  (loop for char = (read-char-no-hang stream nil :eof)
     until (or (null char) (eql :eof char))
     do (push char (contents buffer))))

(defmethod starts-with? ((prefix list) (list list) &optional (test #'eql))
  (loop for (p . rest-p) on prefix for (l . rest-l) on list
     when (or (and rest-p (not rest-l)) (not (funcall test p l))) 
     do (return nil)
     finally (return t)))

(defun start (port &optional (log-stream *standard-output*))
  (stop)  
  (setf *socket-handle* (socket-listen "127.0.0.1" port :reuse-address t))
  (let ((conns (list *socket-handle*))
        (buffers (make-hash-table)))
    (loop (loop for ready in (wait-for-input conns :ready-only t)
                do (if (typep ready 'stream-server-usocket)
                       (push (socket-accept ready) conns)
                     (let ((buf (gethash ready buffers (make-instance 'buffer))))
                       (buffered-read! (socket-stream ready) buf)
                       (when (starts-with? (list #\newline #\return #\newline #\return)
                                           (contents buf))
                         (format log-stream "COMPLETE ~s~%"
                                 (coerce (reverse (contents buf)) 'string))
                         (setf conns (remove ready conns))
                         (remhash ready buffers)
                         (let ((parsed (parse buf)))
                           (format log-stream "PARSED: ~s~%" parsed)
                           (handle-request ready (parse buf))))))))))
```

Ok, I *may* have gone a little overboard in defining `starts-with?` myself, since there's a similar one already available as part of the [library called `alexandria`](http://common-lisp.net/project/alexandria/), but you get the idea. `stop` stops our listener explicitly because `usocket` *doesn't* free up the socket it's listening on if it errors out, and it also clears out `*channel*` for the next go.

The `buffer` class and accompanying `buffered-read!` procedure are going to make the job of collecting possibly chunked requests easier, and `starts-with?` does exactly what it says on the tin. It walks a `list` and a `prefix`, comparing for equality element-wise by a user-specified predicate, and it keeps going until


-   it finds a predicate failure (in which case, it returns `nil`)
-   the list runs out before the prefix (in which case it returns `nil` again)
-   the prefix runs out (in which case it returns `t`)


The last piece is the meat. The `start` function is going to take a `port` and a `log-stream`, and listen on that `port` while dumping logging data to `log-stream`. Lets take this one slow.

```lisp
...
  (setf *socket-handle* (socket-listen "127.0.0.1" port :reuse-address t))
...
```

That sets up the listener on `localhost` listening on the specified `port` and ensures `*socket-handle*` is a reference to that listener.

```lisp
...
  (let ((conns (list *socket-handle*))
        (buffers (make-hash-table)))
...
```

That initializes `conns` to be a list whose only element is the listener we just set up. It also sets up a blank `hash-table` called `buffers`.

```lisp
...
    (loop (loop for ready in (wait-for-input conns :ready-only t)
...
```

We're setting up an infinite loop here. And on each iteration, we're going to wait for some of the sockets in `conns` to need attention, at which point we will iterate through all `ready` sockets to *give* it some attention.

```lisp
...
                do (if (typep ready 'stream-server-usocket)
                       (push (socket-accept ready) conns)
...
```

If the ready socket is a `stream-server`, that means it has a new listener wanting to connect to our server. We push the newcomer onto `conns`; its request will be handled when it becomes ready.

```lisp
...
                     (let ((buf (gethash ready buffers (make-instance 'buffer))))
                       (buffered-read! (socket-stream ready) buf)
                       (when (starts-with? (list #\newline #\return #\newline #\return)
                                           (contents buf))
                         (format log-stream "COMPLETE ~s~%"
                                 (coerce (reverse (contents buf)) 'string))
                         (setf conns (remove ready conns))
                         (remhash ready buffers)
                         (let ((parsed (parse buf)))
                           (format log-stream "PARSED: ~s~%" parsed)
                           (handle-request ready (parse buf))))))))))
```

If it's *not* a `stream-server`, that means it's a regular `usocket`, which means that it has some data ready for us to read. If we wanted to be *extremely* naive here, we'd just call `read-line` repeatedly, but just because *some* data is ready doesn't mean that it represents a complete HTTP request. If the client on the other end is particularly slow, or has a particularly large request to make, it might arrive in chunks. Lets go through that step-by-step.

```lisp
...
                     (let ((buf (gethash ready buffers (make-instance 'buffer))))
                       (buffered-read! (socket-stream ready) buf)
                       (when (starts-with? (list #\newline #\return #\newline #\return)
                                           (contents buf))
...
```

We're either getting this particular connections' buffer (if one exists), or assigning it a fresh buffer. We're then calling `buffered-read!` which will result in the `contents` of that buffer getting filled with all data available for reading from the ready socket. If that data ends with a reversed `crlf crlf`, we should do something about it. As a Note to Self here, if it doesn't we should make sure that the buffered data doesn't exceed some pre-determined threshold and that this particular request hasn't been around for too long. `buffer` already has a `started` slot, we'll just need to check it and evict ones that get too old.

```lisp
...
                         (format log-stream "COMPLETE ~s~%"
                                 (coerce (reverse (contents buf)) 'string))
                         (setf conns (remove ready conns))
                         (remhash ready buffers)
                         (let ((parsed (parse buf)))
                           (format log-stream "PARSED: ~s~%" parsed)
                           (handle-request ready (parse buf))))))))))
```

If we have a complete HTTP request, we emit some logging data, remove that socket from `conns` and its buffer from `buffers`, since we won't be reading from it again one way or the other, then call `handle-request`, passing along the socket and the result of calling `parse` on the buffer. *Eventually*, `parse` should provide a tree of relevant data such as `POST`/`GET` requests and all incoming headers, but for right now, we just care about one property of the incoming request.

```lisp
(defmethod parse ((buf buffer))
  (let ((lines (split "\\r?\\n" (coerce (reverse (contents buf)) 'string))))
    (second (split " " (first lines)))))
```

That'll return the request `uri` of a well-formed, incoming HTTP request. `handle` will then run it through that logic we set up earlier to decide what is to be done.

And that's that. Assuming I've actually got the SSE points ironed out, and we'll see what the mailing lists say on that score, we have a very minimal asynchronous HTTP server built in just under 150 lines of un-obfuscated Common Lisp.

```lisp
(ql:quickload (list :cl-ppcre :usocket :cl-who :parenscript :babel))
(defpackage :ts-usocket (:use :cl :cl-ppcre :usocket :cl-who :parenscript :babel))
(in-package :ts-usocket)

(defvar *socket-handle* nil)
(defparameter *channel* nil)
(defvar crlf (list #\return #\linefeed))

(defun stop ()
  (when *socket-handle*
    (loop while (socket-close *socket-handle*))
    (setf *socket-handle* nil
          *channel* nil)))

(defun start (port &optional (log-stream *standard-output*))
  (stop)  
  (setf *socket-handle* (socket-listen "127.0.0.1" port :reuse-address t))
  (let ((conns (list *socket-handle*))
        (buffers (make-hash-table)))
    (loop (loop for ready in (wait-for-input conns :ready-only t)
                do (if (typep ready 'stream-server-usocket)
                       (push (socket-accept ready) conns)
                     (let ((buf (gethash ready buffers (make-instance 'buffer))))
                       (buffered-read! (socket-stream ready) buf)
                       (when (starts-with? (list #\newline #\return #\newline #\return)
                                           (contents buf))
                         (format log-stream "COMPLETE ~s~%"
                                 (coerce (reverse (contents buf)) 'string))
                         (setf conns (remove ready conns))
                         (remhash ready buffers)
                         (let ((parsed (parse buf)))
                           (format log-stream "PARSED: ~s~%" parsed)
                           (handle-request ready (parse buf))))))))))

(defmethod handle-request (socket request-uri)
  (let ((s (socket-stream socket)))
    (cond ((string= "/sub" request-uri)
           (subscribe! socket))
          ((string= "/pub" request-uri)
           (publish! (format nil "Got a message! ~a" (gensym)))
           (write-string (response 
                          :content-type "text/plain"
                          :body "Published!") s)
           (socket-close socket))
          (t
           (write-string interface s)
           (socket-close socket)))))

(defun publish! (msg)
  (loop for sock in *channel*
     do (handler-case
            (ignore-errors
              (let ((s (socket-stream sock)))
                (write-string (cat "data: " msg crlf crlf) s)
                (force-output s)))
          (error (e)
            (format t "~s" e)
            (setf *channel* (remove sock *channel*))))))

(defun subscribe! (sock)
  (let ((s (socket-stream sock)))
    (write-string (response :keep-alive? t :content-type "text/event-stream") s)
    (force-output s)
    (push sock *channel*)))

(defclass buffer ()
  ((contents :accessor contents :initform nil)
   (started :reader started :initform (get-universal-time))
   (state :accessor state :initform :empty)))

(defmethod buffered-read! (stream (buffer buffer))
  (loop for char = (read-char-no-hang stream nil :eof)
     until (or (null char) (eql :eof char))
     do (push char (contents buffer))))

(defmethod parse ((buf buffer))
  (let ((lines (split "\\r?\\n" (coerce (reverse (contents buf)) 'string))))
    (second (split " " (first lines)))))

(defun response (&key (response-code "200 OK") (content-type "text/html") keep-alive? body)
  (cat "HTTP/1.1 " response-code crlf
       "Content-Type: " content-type "; charset=utf-8" crlf
       "Cache-Control: no-cache, no-store, must-revalidate" crlf
       (when keep-alive?
         (cat "Connection: keep-alive" crlf
              "Date: " (http-date) crlf
              "Expires: Thu, 01 Jan 1970 00:00:01 GMT" crlf))
       (when body 
         (cat "Content-Length: " (write-to-string (length body)) crlf crlf 
              body crlf)) 
       crlf))

(defun cat (&rest seqs)
  (apply #'concatenate 'string seqs))

(defparameter interface
  (response 
   :body 
   (with-html-output-to-string (str nil :prologue t)
     (:html
      (:head (:title "Test page"))
      (:body
       (:div :id "console")
       (:script
        :type "text/javascript"
        (str (ps (defvar src (new (-event-source "/sub")))
                 (defun p (msg)
                   (let ((elem (chain document (get-element-by-id "console"))))
                     (setf (@ elem inner-h-t-m-l)
                           (+ (@ elem inner-h-t-m-l) "&lt;p>" msg "&lt;/p>"))))
                 (setf (@ src onerror)
                       (lambda (e) 
                         (p "ERROR OCCURRED...")
                         (p (chain -j-s-o-n (stringify e))))
                       (@ src onopen)
                       (lambda (e) (p "STREAM OPENED..."))
                       (@ src onmessage)
                       (lambda (e) 
                         (p "GOT MESSAGE!")
                         (p (+ "data: " (@ e data)))))))))))))

(defmethod starts-with? ((prefix list) (list list) &optional (test #'eql))
  (loop for (p . rest-p) on prefix for (l . rest-l) on list
     when (or (and rest-p (not rest-l)) (not (funcall test p l))) 
     do (return nil)
     finally (return t)))

(defparameter day-names '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter month-names '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun http-date ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~a, ~a ~a ~a ~a:~a:~a GMT~@d"
            (nth day-of-week day-names) date
            (nth month month-names) year hour minute second (- tz))))
```

Loading that into your `repl`, then evaluating

```lisp
(in-package :ts-usocket)
(start 3000)
```

should start that minimal server on your local port `3000`. At which point you can go to `http://localhost:3000` in one browser, and hit `http://localhost:3000/pub` twice in another to see output like

```
STREAM OPENED...

GOT MESSAGE!

data: Got a message! G1042

GOT MESSAGE!

data: Got a message! G1043
```

If you keep hammering `F5` on `localhost:3000/pub`, you'll see new messages streaming into the listening window. Once I've gotten this tested, and worked out the kinks, I'll be able to use it to radically simplify the deployment process for [the Deal project](https://github.com/Inaimathi/deal).

What you don't see above is an implementation of sessions, or very many performance optimizations, but there's a reason I titled this "Interlude Part 1".


* * *
##### Footnotes

1 - <a name="foot-Sun-Sep-15-225241EDT-2013"></a>[|back|](#note-Sun-Sep-15-225241EDT-2013) - Of the three I linked, Araneida has been superseded by Hunchentoot, Hunchentoot works on a thread-per-request model so you'd need to add SSE/websockets/what-have-you support, and Wookie doesn't have sessions (or very good performance, apparently) out of the box.

2 - <a name="foot-Sun-Sep-15-225259EDT-2013"></a>[|back|](#note-Sun-Sep-15-225259EDT-2013) - And currently have [questions](https://groups.google.com/a/chromium.org/forum/#!topic/chromium-dev/qcDiw-QP4RE) lodged with the [Chromium devs](https://groups.google.com/a/chromium.org/forum/#!forum/chromium-dev), as well [as SO](http://stackoverflow.com/a/18819542/190887). We'll see what comes of it, I suppose.

3 - <a name="foot-Sun-Sep-15-225303EDT-2013"></a>[|back|](#note-Sun-Sep-15-225303EDT-2013) - Which explains why it errors if you omit it.

4 - <a name="foot-Sun-Sep-15-225306EDT-2013"></a>[|back|](#note-Sun-Sep-15-225306EDT-2013) - Which paranthetically means that you want a reasonably big initial buffer allocated so it doesn't re-request the stream on every other event.

5 - <a name="foot-Sun-Sep-15-225317EDT-2013"></a>[|back|](#note-Sun-Sep-15-225317EDT-2013) - To be fair, I probably *could* put together a system that just generates one per second, and sends that pre-serialized version to each endpoint. That would, at least, save me the effort of having to generate it per-user, but it would complicate things.

6 - <a name="foot-Sun-Sep-15-225327EDT-2013"></a>[|back|](#note-Sun-Sep-15-225327EDT-2013) - In case you're wondering, the main reason I didn't just `console.log` is that I happen to use [a browser that doesn't have very good console facilities](http://conkeror.org/). Or leastwise, it didn't when I last installed it.
