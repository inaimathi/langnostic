### Being The Thoughts on Implementation Minutia of Custom HTTP Servers

### On the Mechanisms of Stopping A Server...

The [last prototype](http://langnostic.blogspot.ca/2013/09/deal-journal-interlude-one-treatise-on.html) I posted had a laughably mis-named `stop` function

```lisp
(defun stop ()
  (when *socket-handle*
    (loop while (socket-close *socket-handle*))
    (setf *socket-handle* nil
          *channel* nil)))
```

See, because the server I'm putting together is single-threaded, you need to `C-c C-c` out of it to get back to the REPL. Except, that still leaves the `socket-server` listening on the specified TCP port. The half-assed solution I'd come up with involved setting a handle into which I'd put the listener so that I could close the process and kill the listener externally later.

```lisp
(defvar *socket-handle* nil)

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

The much more elegant way of solving this is by using `unwind-protect`:

```lisp
(defun start (port &optional (log-stream *standard-output*))
  (let ((conns (list (socket-listen usocket:*wildcard-host* port :reuse-address t)))
        (buffers (make-hash-table)))
    (unwind-protect
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
                               (handle-request ready (parse buf))))))))
      (loop for c on conns do (loop while (socket-close c)))
      (setf *channel* nil))))
```

That'll automatically clean up on any kind of error, including an Emacs interrupt, *and* it completely removes the need for `stop` and `*socket-handle*`. The above also uses `usocket:*wildcard-host*` instead of `"127.0.0.1"`, but that's a tiny change.

### On The Mechanism for Listening to Sockets

There's a less obvious place that I wanted to figure something out for. Here's the above server with elided chunklets, just so we can focus in on the relevant details

```lisp
(defun start (port &optional (log-stream *standard-output*))
  (let ((conns (list (socket-listen usocket:*wildcard-host* port :reuse-address t)))
        ...)
    (unwind-protect
         (loop (loop for ready in (wait-for-input conns :ready-only t)
                  do (if (typep ready 'stream-server-usocket)
                         (push (socket-accept ready) conns)
                         (let (...)
                           (buffered-read! (socket-stream ready) buf)
                           (when ...
                             ...
                             (setf conns (remove ready conns))
                             ...)))))
      ...)))
```

The point I've been thinking about in particular is that bit that says `(wait-for-input conns ...)`, and the associated places where I either remove things from, or add things to `conns`. As written up there, it's a `list`. Which is to say, a singly linked list. And that means that adding a thing to it is `O1`, but removing a thing from it is `On`, and since we're doing that `(setf conns (remove ready conns))` inside of a loop, this version of start`start` is effectively an `On^2` procedure in the worst case. Not *horrible*, I guess, but I think I can do better.

The challenge here is that no matter what data structure we use to store connections, `wait-for-input` needs either a `socket`, or a list of `sockets`. Here's one attempt to do somewhat better

```lisp
(defun start (port &optional (log-stream *standard-output*))
  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t))
        (conns (make-hash-table))
        ...)
    (unwind-protect
         (loop (loop for ready in (wait-for-input (cons server (hash-keys conns)) :ready-only t)
                  do (if (typep ready 'stream-server-usocket)
                         (setf (gethash (socket-accept ready) conns) :in)
                         (let (...)
                           (buffered-read! (socket-stream ready) buf)
                           (when ...
                             ...
                             (remhash ready conns)
                             ...)))))
      ...)))
```

If we represent `conns` as a hash table, we can effectively pay some memory and some best-case time to mitigate worst-case time. Seems worth it, I'd say, but I'm not at all sure. New connection insertion now takes the form of `(setf (gethash (socket-accept ready) conns) :in)`, and connection removal is written as `(remhash ready conns)`, both of which are `O1`. The thing that gets markedly worse, ironically is the `wait-for-input` call itself. Unlike the original, which just passed the raw `conns`, we now have to pass `(cons server (hash-keys conns))`, which requires not only the `cons`ing of an entirely new list each time through, but also a full traversal of `conns`. Since the interface of `wait-for-input` demands an actual list, and not a generator or similar, the best you can do on the implementation of `hash-keys` is something like `(loop for k being the hash-keys of conns collect k)`. Which works, but isn't exactly stellar.

To its credit though, it *does* save us time in the worst case. As a result of this representation change, `start` now has `On` performance in both the worst and the best case. I get the feeling we could save some constants by opening up `usocket` and twiddling with `wait-for-input`, but a glance at the relevant files tells me that it's implemented something like four times, sometimes in expected configurations I can't easily test.

Ah well. The naive hash is a good enough improvement for now, and I am in the middle of reading through notes about [Berkeley Sockets](http://en.wikipedia.org/wiki/Berkeley_sockets) and [their uses](http://beej.us/guide/bgnet/output/html/singlepage/bgnet.html#man). Hopefully, when I get to work I can convince one of my co-workers to take me through the nuts and bolts of the implementation in C. Maybe that will give me enough insight to write something that solves this problem in a satisfactory fashion.
