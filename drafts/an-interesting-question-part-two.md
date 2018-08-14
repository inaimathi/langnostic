Ok, lets give this a [different shot](https://github.com/inaimathi/cl-clocking).

```commonlisp
(defparameter *generator* (session-token:make-generator :token-length 1000))
(defun gen! () (funcall *generator*))

(defun sha256 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256 (ironclad:ascii-string-to-byte-array str))))

(defun tcp-send (host port input)
  (let* ((sock (usocket:socket-connect host port))
         (stream (usocket:socket-stream sock)))
    (format stream "~a" input)
    (force-output stream)
    (read-sequence (make-string 2) stream)
    (usocket:socket-close sock)))

(defun tcp-receive (server)
  (let* ((sock (usocket:socket-accept server))
         (stream (usocket:socket-stream sock))
         (buf (make-string 1000)))
    (read-sequence buf stream)
    (format stream "~a" "ok")
    (force-output stream)
    (usocket:socket-close sock)
    buf))

(defun battery-status ()
  (loop for b in (trivial-battery:battery-info)
     collect (trivial-battery:battery-details
              (cdr (assoc "name" b :test #'string=)))))

(defun profile! (&key (times 1) (port 8080))
  (let* ((server (usocket:socket-listen usocket:*wildcard-host* port :reuse-address t))
         (server-thread (bt:make-thread (lambda () (loop (tcp-receive server)))))
         (before (battery-status)))
    (loop repeat times
       do (let ((inp (gen!)))
            (sha256 inp)
            (tcp-send "localhost" port inp)))
    (usocket:socket-close server)
    (bt:destroy-thread server-thread)
    (list :before before :after (battery-status))))
```

Since this version is written in CL, we don't need to worry about the effect of JVM overhead on energy consumption. On the downside, the only implementation of [ECDSA for Common Lisp](https://github.com/krkhan/cl-ecc) is a prototype that isn't yet included in the default `quicklisp` distribution. This makes it a bit more difficult to test `ecdsa-sign` and `ecdsa-verify` steps, but we already got decently reliable results for that [last time](/posts/an-interesting-question). What we're _really_ after is a TCP test that doesn't saturate memory, and I'm a lot more comfortable with Common Lisps' [`usocket` library](https://common-lisp.net/project/usocket/) than I am with [clj-sockets](https://github.com/atroche/clj-sockets).

Also, I know that [`slime`](https://common-lisp.net/project/slime/) has a pretty good built-in profiler, so we won't have to worry about including that as a separate library. A preliminary run with `M-x profile-package CL-CLOCKING` and `(cl-clocking::profile! :times 5000000)` (admittedly with some minor prior REPL testing) shows us

```
  seconds  |     gc     |      consed     |    calls   |  sec/call  |  name  
------------------------------------------------------------------
 10886.748 |      7.400 | 107,713,781,632 |  5,055,014 |   0.002154 | CL-CLOCKING::TCP-RECEIVE
  9789.168 |      7.380 |  83,018,785,424 |  5,055,010 |   0.001937 | CL-CLOCKING::TCP-SEND
   686.740 |      0.020 |  31,413,748,800 |  5,055,010 |   0.000136 | CL-CLOCKING::GEN!
   248.054 |      0.000 |       1,147,808 |          4 |  62.013577 | CL-CLOCKING::PROFILE!
   209.580 |      0.000 |       2,777,824 |  5,055,010 |   0.000041 | CL-CLOCKING::SHA256
     0.084 |      0.000 |       3,027,360 |          8 |   0.010499 | CL-CLOCKING::BATTERY-STATUS
------------------------------------------------------------------
 21820.375 |     14.800 | 222,153,268,848 | 20,220,056 |            | Total

estimated total profiling overhead: 22.16 seconds
overhead estimation parameters:
  2.4e-8s/call, 1.096e-6s total profiling, 5.04e-7s internal profiling
```

as the result of `M-x slime-profile-report`. So, even without doing anything at all with batteries, we know that `tcp-receive` and `tcp-send` are going to dominate the energy expenditure here. Which shouldn't be a surprise, really; if absolutely nothing else, `tcp-receive` needs to allocate 1k of memory into which to read the incoming message.
