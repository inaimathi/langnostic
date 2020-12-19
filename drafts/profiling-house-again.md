So I've plowed some of my vacation time into polishing up/hacking on some old projects. Including [`house`](TODO), the web server I [complained](TODO) was garbage, but still had one distinct advantage over other Common Lisp webservers. Namely; because it's the only natively implemented one, it will work out-of-the-box, without issue, anywhere you can install [`quicklisp`](TODO) and a LISP it runs on.

This hacking attempt was aimed at addressing the complaint. _Most_ of [the `major-overhaul` branch](TODO) was aimed at making the code more readable and sensical. Making `handlers` and `http-type`s much simpler, both implementationally and conceptually. But I want to throw at least _a little_ effort at performance. With that in mind, I wanted a preliminary benchmark. I'm following [`fukamachi`s' procedure for `woo`](https://github.com/fukamachi/woo/blob/master/benchmark.md). Note that, since `house` is a single-threaded server (for now), I'm only doing single-threaded benchmarks.

```
; SLIME 2.26
CL-USER> (ql:quickload :house)
To load "house":
  Load 1 ASDF system:
    house
; Loading "house"
.....
(:HOUSE)
CL-USER> (in-package :house)
#<PACKAGE "HOUSE">
HOUSE> (define-handler (root) () "Hello world!")
#<HANDLER-TABLE {1004593CF3}>
HOUSE> (house:start 5000)
```


```
inaimathi@this:~$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.01ms    5.85ms 204.63ms   98.73%
    Req/Sec     2.64k     0.89k    7.22k    62.16%
  104779 requests in 10.10s, 30.58MB read
  Socket errors: connect 0, read 104775, write 0, timeout 0
Requests/sec:  10374.93
Transfer/sec:      3.03MB
inaimathi@this:~$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     2.74ms   19.05ms 408.54ms   98.18%
    Req/Sec     2.58k     0.85k    4.64k    57.39%
  102543 requests in 10.10s, 29.92MB read
  Socket errors: connect 0, read 102539, write 0, timeout 0
Requests/sec:  10152.79
Transfer/sec:      2.96MB
inaimathi@this:~$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     4.56ms   59.54ms   1.66s    99.27%
    Req/Sec     3.10k     1.83k    9.56k    76.72%
  103979 requests in 10.01s, 30.34MB read
  Socket errors: connect 0, read 103979, write 0, timeout 4
Requests/sec:  10392.46
Transfer/sec:      3.03MB
inaimathi@this:~$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     8.49ms   85.22ms   1.66s    98.81%
    Req/Sec     3.23k     2.16k   11.90k    81.01%
  102236 requests in 10.01s, 29.83MB read
  Socket errors: connect 0, read 102232, write 0, timeout 4
Requests/sec:  10215.87
Transfer/sec:      2.98MB
inaimathi@this:~$
```

So that puts `house` comfortably in the same league as Tornado on PyPy or the `node.js` server. This is not a bad league to be in, but I want to see if I can do better.

## Step 1 - Kill Methods

`defmethod` is a thing I was seemingly obsessed with when I wrote `house`. This isn't necessarily a bad thing from the legibility perspective; because they have type annotations, it's clearer what an expected input is from a reading of the code. However, there's two disadvantages to using `method`s where you don't have to.

1. You'll often get a `no-defined-method` error on weird input, rather than something more descriptive and specific the way you probably would when using a normal function
2. Your performance will sometimes irredeemably suck.

The first point is a nit, but the second one is worth dealing with in the context of a library that should probably perform reasonably well at least _some_ time. The _cause_ of that problem is that `method`s can't be `inline`d. Because the point of them is to dispatch on a type-table of their arguments at runtime, they can't do their work at compile-time to inline the result without some [serious trickery](http://metamodular.com/SICL/generic-dispatch.pdf)[^wait-why-methods]. Today, I'm avoiding trickery and just re-writing every `method` in `house` that I can into a function, usually by using `etypecase`.

[^wait-why-methods]: Wait, why use methods then? They're good _specifically_ in the situation where

Some of these are trivial conversions

```
;;; house.lisp
...
-(defmethod start ((port integer) &optional (host usocket:*wildcard-host*))
+(defun start (port &optional (host usocket:*wildcard-host*))
+  (assert (integerp port))
...
-(defmethod process-ready ((ready stream-server-usocket) (conns hash-table))
-  (setf (gethash (socket-accept ready :element-type 'octet) conns) nil))
-
-(defmethod process-ready ((ready stream-usocket) (conns hash-table))
+(defun process-ready (ready conns)
+  (assert (hash-table-p conn))
+  (etypecase ready
+    (stream-server-usocket (setf (gethash (socket-accept ready :element-type 'octet) conns) nil))
+    (stream-usocket
...
-(defmethod parse-cookies ((cookie string))
+(defun parse-cookies (cookie)
+  (assert (stringp cookie))
...
-(defmethod handle-request! ((sock usocket) (req request))
+(defun handle-request! (sock req)
...
-(defmethod error! ((err response) (sock usocket) &optional instance)
-  (declare (ignorable instance))
+(defun error! (err sock)
,,,
```

```
;;; session.lisp
...
-(defmethod new-session-hook! ((callback function))
+(defun new-session-hook! (callback)
...
-(defmethod poke! ((sess session))
+(defun poke! (sess)
...
```

```
;;; util.lisp
...
-(defmethod path->uri ((path pathname) &key stem-from)
+(defun path->uri (path &key stem-from)
...
-(defmethod path->mimetype ((path pathname))
+(defun path->mimetype (path)
...
```

Some are _slightly_ more complicated. In particular, `parse` looks like it would conflate two entirely separate functions, but on inspection, we know the type of its argument at every call site.

```
./house.lisp:46:		      (setf (parameters (request buf)) (nconc (parse buf) (parameters (request buf)))))
./house.lisp:68:	   do (multiple-value-bind (parsed expecting) (parse buffer)
./house.lisp:92:(defmethod parse ((str string))
./house.lisp:110:(defmethod parse ((buf buffer))
./house.lisp:116:	(parse str))))
```

So, we can convert `parse` to two separate, named functions. `write!` is basically the same situation.

```
;;; house.lisp
...
-(defmethod parse ((str string))
+(defun parse-request-string (str)
...
-(defmethod parse ((buf buffer))
+(defun parse-buffer (buf)
...
-(defmethod write! ((res response) (stream stream))
+(defun write-response! (res stream)
...
-(defmethod write! ((res sse) (stream stream))
+(defun write-sse! (res stream)
...
```

Not pictured; changes at each call-site to call the correct one.

The `parse-params` method is a bit harder to tease out. Because it looks like it genuinely is one polymorphic function. Again, though, on closer inspection of the _fully internal to `house`_ call-sites makes it clear that we almost always know what we're passing as arguments at compile-time.

```
./house.lisp:78:(defmethod parse-params (content-type (params null)) nil)
./house.lisp:79:(defmethod parse-params (content-type (params string))
./house.lisp:83:(defmethod parse-params ((content-type (eql :application/json)) (params string))
./house.lisp:107:	(setf (parameters req) (parse-params nil parameters))
./house.lisp:113:	(parse-params
					 (->keyword (cdr (assoc :content-type (headers (request buf)))))
					 str)
```

That always is going to be a slight pain though; we need to do a runtime dispatch inside of `parse-buffer` to figure out whether we're parsing JSON or a param string.

```
...
-(defmethod parse-params (content-type (params null)) nil)
-(defmethod parse-params (content-type (params string))
+(defun parse-param-string (params)
   (loop for pair in (split "&" params)
-     for (name val) = (split "=" pair)
-     collect (cons (->keyword name) (or val ""))))
-
-(defmethod parse-params ((content-type (eql :application/json)) (params string))
-  (cl-json:decode-json-from-string params))
+	for (name val) = (split "=" pair)
+	collect (cons (->keyword name) (or val ""))))
...
-	(parse-params
-	 (->keyword (cdr (assoc :content-type (headers (request buf)))))
-	 str)
-	(parse str))))
+	(if (eq :application/json (->keyword (cdr (assoc :content-type (headers (request buf))))))
+	    (cl-json:decode-json-from-string str)
+	    (parse-param-string str))
+	(parse-request-string str))))
...
```

The _last_ one is going to be a headache. The `lookup` method is meant to be a general accessor, _and_ has a `setf` method defined. I'm not going that way right now; lets see if we gained anything with our current efforts.

Second verse same as the first.

```
; SLIME 2.26
CL-USER> (ql:quickload :house)
To load "house":
  Load 1 ASDF system:
    house
; Loading "house"
.....
(:HOUSE)
CL-USER> (in-package :house)
#<PACKAGE "HOUSE">
HOUSE> (define-handler (root) () "Hello world!")
#<HANDLER-TABLE {1004593CF3}>
HOUSE> (house:start 5000)
```

```
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.96ms    4.02ms  76.87ms   98.43%
    Req/Sec     2.70k     0.98k    7.57k    73.83%
  103951 requests in 10.10s, 30.34MB read
  Socket errors: connect 0, read 103947, write 0, timeout 0
Requests/sec:  10292.48
Transfer/sec:      3.00MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   846.32us    2.63ms  58.29ms   98.26%
    Req/Sec     2.64k     0.94k   11.13k    72.89%
  102661 requests in 10.10s, 29.96MB read
  Socket errors: connect 0, read 102658, write 0, timeout 0
Requests/sec:  10165.46
Transfer/sec:      2.97MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     8.57ms   90.07ms   1.66s    98.96%
    Req/Sec     3.71k     2.87k   11.73k    74.30%
  105162 requests in 10.10s, 30.69MB read
  Socket errors: connect 0, read 105159, write 0, timeout 2
Requests/sec:  10412.91
Transfer/sec:      3.04MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     5.69ms   70.32ms   1.66s    99.25%
    Req/Sec     3.06k     1.82k    9.46k    74.40%
  101302 requests in 10.10s, 29.56MB read
  Socket errors: connect 0, read 101299, write 0, timeout 3
Requests/sec:  10030.14
Transfer/sec:      2.93MB
inaimathi@this:~/quicklisp/local-projects/house$
```

Aaand it looks like the effect was neglegible. Oh well. I honestly think that the untangling we've done so far makes the parts of the codebase that its' touched _more_ readable, so I'm keeping them, but there's no great improvement yet. Perhaps if we inline some things?

```
;;; package.lisp
-(declaim (inline crlf write-ln idling? flex-stream))
+(declaim (inline crlf write-ln idling? flex-stream write-response! write-sse! process-ready parse-param-string parse-request-string))
```

```inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.71ms   15.37ms 412.51ms   98.91%
    Req/Sec     2.69k     0.91k    6.28k    65.37%
  103607 requests in 10.10s, 30.24MB read
  Socket errors: connect 0, read 103603, write 0, timeout 0
Requests/sec:  10258.44
Transfer/sec:      2.99MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   837.49us    2.66ms  58.36ms   98.36%
    Req/Sec     2.63k   836.52     3.81k    49.37%
  103449 requests in 10.10s, 30.19MB read
  Socket errors: connect 0, read 103446, write 0, timeout 0
Requests/sec:  10242.91
Transfer/sec:      2.99MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     6.23ms   74.76ms   1.89s    99.08%
    Req/Sec     4.01k     2.20k   10.23k    58.89%
  101524 requests in 10.10s, 29.63MB read
  Socket errors: connect 0, read 101522, write 0, timeout 4
Requests/sec:  10052.56
Transfer/sec:      2.93MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     5.75ms   70.98ms   1.67s    99.27%
    Req/Sec     3.19k     2.11k   10.26k    81.39%
  100944 requests in 10.01s, 29.46MB read
  Socket errors: connect 0, read 100941, write 0, timeout 1
Requests/sec:  10088.23
Transfer/sec:      2.94MB
```

Again, no huge difference. On closer inspection, `lookup` is only used in one place internally, and it's easy to replace with `gethash` so I'm just going to do that and re-check real quick.

```
;;; channel.lisp
...
-  (push sock (lookup channel *channels*))
+  (push sock (gethash channel *channels*))
...
```

```
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.95ms    3.72ms  72.70ms   98.43%
    Req/Sec     2.66k     1.00k   11.52k    73.45%
  102839 requests in 10.10s, 30.01MB read
  Socket errors: connect 0, read 102835, write 0, timeout 0
Requests/sec:  10183.46
Transfer/sec:      2.97MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.87ms    2.85ms  59.32ms   98.19%
    Req/Sec     2.62k     0.86k    3.87k    54.82%
  102818 requests in 10.10s, 30.00MB read
  Socket errors: connect 0, read 102814, write 0, timeout 0
Requests/sec:  10180.62
Transfer/sec:      2.97MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     6.96ms   80.03ms   1.68s    99.10%
    Req/Sec     3.11k     2.12k   11.72k    78.40%
  105460 requests in 10.10s, 30.78MB read
  Socket errors: connect 0, read 105456, write 0, timeout 5
Requests/sec:  10441.77
Transfer/sec:      3.05MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     8.22ms   83.95ms   1.66s    98.84%
    Req/Sec     3.19k     2.07k   11.66k    73.23%
  103933 requests in 10.10s, 30.33MB read
  Socket errors: connect 0, read 103930, write 0, timeout 5
Requests/sec:  10290.43
Transfer/sec:      3.00MB
```

To no ones' great surprise, still not much of a difference. I'm going to let the `lookup` issue dangle for the moment, because it has to do with a trick I want to pull a bit later on, but before we get to that...

## Step 2 - Kill Classes

The second step is to kill `class` definitions entirely. Their `accessor` functions are _also_ generic, and therefore rely on method dispatch. `struct`s are a bit clumsier, but _probably_ faster in the end. Now, we can't _really_ mess with `session`, `request` and `response`, because those are part of `house`s' external interface, but there's three places where we can replace `defclass` with `defstruct`.

Re-writing `buffer`, `sse` and `handler-entry` ...

```
;;; model.lisp
...
-(defclass sse ()
-  ((id :reader id :initarg :id :initform nil)
-   (event :reader event :initarg :event :initform nil)
-   (retry :reader retry :initarg :retry :initform nil)
-   (data :reader data :initarg :data)))
...
-(defclass buffer ()
-  ((tries :accessor tries :initform 0)
-   (contents :accessor contents :initform nil)
-   (bi-stream :reader bi-stream :initarg :bi-stream)
-   (total-buffered :accessor total-buffered :initform 0)
-   (started :reader started :initform (get-universal-time))
-   (request :accessor request :initform nil)
-   (expecting :accessor expecting :initform 0)))
...
-(defclass handler-entry ()
-  ((fn :reader fn :initarg :fn :initform nil)
-   (closing? :reader closing? :initarg :closing? :initform t)))
...
```

```
;;; house.lisp
...
-(defun write-sse! (res stream)
-  (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
-	  (id res) (event res) (retry res) (data res)))
...
-(defun buffer! (buffer)
-  (handler-case
-      (let ((stream (bi-stream buffer)))
-	(incf (tries buffer))
-	(loop for char = (read-char-no-hang stream)
-	   until (or (null char) (eql :eof char))
-	   do (push char (contents buffer))
-	   do (incf (total-buffered buffer))
-	   when (request buffer) do (decf (expecting buffer))
-	   when (and #-windows(char= char #\linefeed)
-		     #+windows(char= char #\newline)
-		 (line-terminated? (contents buffer)))
-	   do (multiple-value-bind (parsed expecting) (parse-buffer buffer)
-		(setf (request buffer) parsed
-		      (expecting buffer) expecting
-		      (contents buffer) nil)
-		(return char))
-	   when (> (total-buffered buffer) +max-request-size+) return char
-	   finally (return char)))
-    (error () :eof)))
...
-(defun parse-buffer (buf)
-  (let ((str (coerce (reverse (contents buf)) 'string)))
-    (if (request buf)
-	(if (eq :application/json (->keyword (cdr (assoc :content-type (headers (request buf))))))
-	    (cl-json:decode-json-from-string str)
-	    (parse-param-string str))
-	(parse-request-string str))))
...
```

```
;;; define-handler.lisp
+(defstruct handler-entry
+  (fn nil)
+  (closing? t))
...
-    (make-instance
-     'handler-entry
+    (make-handler-entry
```

```
;;; channel.lisp
...
+(defstruct (sse (:constructor make-sse (data &key id event retry)))
+  (id nil) (event nil) (retry nil)
+  (data (error "an SSE must have :data") :type string))
...
-(defun make-sse (data &key id event retry)
-  (make-instance 'sse :data data :id id :event event :retry retry))
+(defun write-sse! (res stream)
+  (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
+	  (ss-id res) (sse-event res) (sse-retry res) (sse-data res)))
...
```

```
;;; buffer.lisp
+(in-package :house)
+
+(defstruct (buffer (:constructor make-buffer (bi-stream)))
+  (tries 0 :type integer)
+  (contents nil)
+  (bi-stream nil)
+  (total-buffered 0 :type integer)
+  (started (get-universal-time))
+  (request nil)
+  (expecting 0 :type integer))
+
+(defun buffer! (buffer)
+  (handler-case
+      (let ((stream (buffer-bi-stream buffer)))
+	(incf (buffer-tries buffer))
+	(loop for char = (read-char-no-hang stream)
+	   until (or (null char) (eql :eof char))
+	   do (push char (buffer-contents buffer))
+	   do (incf (buffer-total-buffered buffer))
+	   when (buffer-request buffer) do (decf (buffer-expecting buffer))
+	   when (and #-windows(char= char #\linefeed)
+		     #+windows(char= char #\newline)
+		 (line-terminated? (buffer-contents buffer)))
+	   do (multiple-value-bind (parsed expecting) (parse-buffer buffer)
+		(setf (buffer-request buffer) parsed
+		      (buffer-expecting buffer) expecting
+		      (buffer-contents buffer) nil)
+		(return char))
+	   when (> (buffer-total-buffered buffer) +max-request-size+) return char
+	   finally (return char)))
+    (error () :eof)))
+
+(defun parse-buffer (buf)
+  (let ((str (coerce (reverse (buffer-contents buf)) 'string)))
+    (if (buffer-request buf)
+	(if (eq :application/json (->keyword (cdr (assoc :content-type (headers (buffer-request buf))))))
+	    (cl-json:decode-json-from-string str)
+	    (parse-param-string str))
+	(parse-request-string str))))
```


```
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.09ms    6.18ms 202.73ms   98.55%
    Req/Sec     2.69k     0.89k    4.02k    56.74%
  105108 requests in 10.10s, 30.67MB read
  Socket errors: connect 0, read 105105, write 0, timeout 0
Requests/sec:  10406.92
Transfer/sec:      3.04MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.98ms    5.78ms 204.47ms   98.86%
    Req/Sec     2.67k   848.77     3.98k    54.71%
  104242 requests in 10.10s, 30.42MB read
  Socket errors: connect 0, read 104242, write 0, timeout 0
Requests/sec:  10321.40
Transfer/sec:      3.01MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     6.93ms   79.75ms   1.66s    99.10%
    Req/Sec     3.33k     2.46k   11.95k    79.87%
  105920 requests in 10.10s, 30.91MB read
  Socket errors: connect 0, read 105918, write 0, timeout 2
Requests/sec:  10487.59
Transfer/sec:      3.06MB
inaimathi@this:~/quicklisp/local-projects/house$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     4.78ms   61.11ms   1.68s    99.30%
    Req/Sec     2.83k     1.26k    7.01k    70.22%
  103381 requests in 10.10s, 30.17MB read
  Socket errors: connect 0, read 103378, write 0, timeout 0
Requests/sec:  10235.14
Transfer/sec:      2.99MB
```

Very little noticeable gain, I'm afraid. Ok, there's one more thing I'm tempted to try. There were hints earlier that this was coming, including [this](TODO - link to ticket), but if you don't follow my `github` you might still be surprised.

## Step 3 - Musing on CLJ

Now that we have what I _think_ is a reasonably fast implementation of house, I want to see whether[^more-realistically-how] [`clj`](TODO) does performance damage to the implementation. I want to see this because, the `clj` datastructures and syntax _really_ improve readability and `REPL` development; there's a _bunch_ of situations in which I missed having that level of visibility into my structures before I even began this benchmark article. There's even probably a few places where it _saves_ some performance by referencing other partial structures. The problem is that _I'm guessing_ it's a net negative in terms of performance, so I want to see what a conversion would do to my benchmark before I go through with it.

[^more-realistically-how]: More realistically, "how much" rather than "whether"

This is going to be _especially_ useful for `house`s' external interface. And given that I've already had to break compatibility to write this overhaul, this is probably the best possible time to test the theory. The trouble is that I'm not _entirely_ sure what the real interface looks like quite yet, so I'm _not_ going to be implementing it today. These are just some musings.

The current `house` model for `handler`/`response` interaction is that a handler returns either a `response` (in the event of a `redirect!`) or a `string` (in any other event). This makes a few things kind of difficult. Firstly, it means that `session` and `header` manipulation has to happen by effect. That is, they're not included as part of the return value; they have to be exposed in some other way. In the case of `headers`, it's via an `alist` bound to the invisible symbol `headers` inside of the handler body. This ... is less than ideal.

If we take the `http-kit` approach, we'd expect our handlers to always return a `map`. And if that `map` had slots for `headers`/`session`, those things would be set as appropriate in the outgoing `response` and/or server state. Our input would _also_ be a `map`. And it would naturally contain `method`/`headers`/`path`/`parameters`/`session`/etc slots that a handler writer would want to make use of. I'm not _entirely_ clear on whether we'd want to make this the primary internal and external representation, or if we're just looking for an easily manipulated layer for the users. I'm leaning towards the first of those options.

This ... actually doesn't sound too hard if cut at the right level. Lets give it a shot, I guess.

![](/static/img/one-eternity-later.jpg)

It wasn't.

There's enough weird shit happening here that I need a fresh brain for it. That was enough for now.
