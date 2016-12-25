Ok, it's about time I put [this fucker](https://github.com/inaimathi/house) through its paces.

```lisp
;; in SLIME
; SLIME 2016-04-19
CL-USER> (ql:quickload :house)
To load "house":
  Load 1 ASDF system:
    house
; Loading "house"
........
(:HOUSE)
CL-USER> (in-package :house)
#<PACKAGE "HOUSE">
HOUSE> (define-handler (hello-world :content-type "text/plain") ()
      "Hello world!")
#<HANDLER-TABLE {100A1BF7A3}>
HOUSE> (house:start 4040)
```

```shell
# in eshell

~/quicklisp/local-projects/house $ wrk -t12 -c400 -d30s http://127.0.0.1:4040/hello-world
Running 30s test @ http://127.0.0.1:4040/hello-world
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.84ms   50.75ms   1.63s    99.56%
    Req/Sec     0.98k   667.17     8.12k    82.60%
  172737 requests in 30.03s, 43.00MB read
  Socket errors: connect 0, read 173088, write 0, timeout 19
Requests/sec:   5752.02
Transfer/sec:      1.43MB
~/quicklisp/local-projects/house $
```

So that's a decent start. Out of the gate, according to [this](https://github.com/fukamachi/woo/blob/master/benchmark.md#benchmarks), `house` outperforms `tornado` (unless running in `pypy`), `wookie` and `hunchentoot` in terms of requests/second[^alternatively-of-course] on a single thread. Which is not bad for a server that had no intention whatsoever of outperforming anything.

[^alternatively-of-course]: Alternatively, the hardware I'm testing on is so much better than that of the initial benchmark that it annihilates all losses. Although it doesn't seem like it, based on the environment readout found on that benchmark page. They've got about half the memory that I do, but more CPU, and this doesn't seem like it would be a memory-bound operation given that my memory use barely registers the benchmark according to `htop`. Anyhow, given that I've been assuming that `house` is the cheap-seat web-server for Common Lisp, usable only because it's the only one written without calling into FFI code, I'm pleasantly surprised to find that it also runs decently quickly.

That's all well and good, but it's not really what I'm interested in. Enhance!

```lisp
C-c C-c
; Evaluation aborted on NIL.
HOUSE>
```

```emacs
M-x slime-profile-pakcage HOUSE y y
```

```lisp
HOUSE> (house:start 4040)
```

```shell
~/quicklisp/local-projects/house $ wrk -t12 -c400 -d30s http://127.0.0.1:4040/hello-world
Running 30s test @ http://127.0.0.1:4040/hello-world
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    22.42ms   71.10ms   1.60s    93.01%
    Req/Sec    96.75    108.40   690.00     88.96%
  9446 requests in 30.04s, 2.35MB read
  Socket errors: connect 0, read 9456, write 0, timeout 1
Requests/sec:    314.47
Transfer/sec:     80.15KB
~/quicklisp/local-projects/house $
```

So it performs predictably poorly with the profiler running, but again, not really what we're interested in.

```lisp
C-c C-c
; Evaluation aborted on NIL.
HOUSE>
```

```emacs
M-x slime-profile-report
```

```
measuring PROFILE overhead..done
  seconds  |     gc     |    consed   |    calls   |  sec/call  |  name
--------------------------------------------------------------
     2.197 |      0.000 |      43,472 | 16,862,405 |   0.000000 | HOUSE::IDLING?
     1.346 |      0.000 |         160 | 16,862,405 |   0.000000 | HOUSE::LAST-POKED
     0.221 |      0.000 |   1,805,136 |      9,473 |   0.000023 | HOUSE::BUFFER!
     0.188 |      0.000 |  47,902,976 |     18,926 |   0.000010 | HOUSE::FLEX-STREAM
     0.165 |      0.000 |   3,924,080 |     18,926 |   0.000009 | HOUSE::WRITE!
     0.099 |      0.000 |  25,477,856 |     18,924 |   0.000005 | HOUSE::PARSE
     0.080 |      0.000 |  35,093,408 |    482,562 |   0.000000 | HOUSE::LINE-TERMINATED?
     0.075 |      0.000 |           0 |    984,048 |   0.000000 | HOUSE::CONTENTS
     0.072 |      0.000 |           0 |    965,135 |   0.000000 | HOUSE::TOTAL-BUFFERED
     0.048 |      0.000 |           0 |    482,562 |   0.000000 | (SETF HOUSE::TOTAL-BUFFERED)
     0.042 |      0.000 |           0 |    492,024 |   0.000000 | (SETF HOUSE::CONTENTS)
     0.040 |      0.000 |           0 |     75,703 |   0.000001 | HOUSE::CRLF
     0.032 |      0.000 |          64 |     18,924 |   0.000002 | HOUSE::->KEYWORD
     0.030 |      0.000 |     880,192 |      9,462 |   0.000003 | HOUSE::NEW-SESSION-TOKEN!
     0.006 |      0.000 |           0 |      9,463 |   0.000001 | HOUSE::KEEP-ALIVE?
     0.006 |      0.000 |          96 |    510,958 |   0.000000 | HOUSE:REQUEST
     0.004 |      0.000 |           0 |     18,924 |   0.000000 | (SETF HOUSE:PARAMETERS)
     0.002 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE::EXPECTING
     0.002 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE::SPLIT-AT
     0.002 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE::HTTP-METHOD
     0.002 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE::ANY-VARS?
     0.002 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE:SESSION-TOKENS
     0.002 |      0.000 |           0 |      9,462 |   0.000000 | (SETF HOUSE:REQUEST)
     0.002 |      0.000 |           0 |      9,463 |   0.000000 | HOUSE::CONTENT-TYPE
     0.002 |      0.000 |           0 |      9,463 |   0.000000 | HOUSE::CHARSET
     0.002 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE::FIND-HANDLER
     0.000 |      0.000 |   5,079,696 |      9,462 |   0.000000 | HOUSE::HANDLE-REQUEST!
     0.000 |      0.000 |      32,736 |     18,924 |   0.000000 | HOUSE::TRIE-LOOKUP
     0.000 |      0.000 |      32,768 |      9,462 |   0.000000 | (SETF HOUSE::EXPECTING)
     0.000 |      0.000 |           0 |     18,924 |   0.000000 | HOUSE::TOKEN
     0.000 |      0.000 |           0 |      9,473 |   0.000000 | HOUSE::BI-STREAM
     0.000 |      0.000 |           0 |      9,463 |   0.000000 | HOUSE::COOKIE
     0.000 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE::PARSE-PARAMS
     0.000 |      0.000 |      42,832 |         95 |   0.000000 | HOUSE::CLEAN-SESSIONS!
     0.000 |      0.000 |           0 |      9,473 |   0.000000 | HOUSE::STARTED
     0.000 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE::HANDLERS
     0.000 |      0.000 |           0 |      9,463 |   0.000000 | HOUSE::RESPONSE-CODE
     0.000 |      0.000 |           0 |      9,463 |   0.000000 | HOUSE::BODY
     0.000 |      0.000 |           0 |      9,463 |   0.000000 | HOUSE::LOCATION
     0.000 |      0.000 |           0 |     18,946 |   0.000000 | HOUSE::TRIES
     0.000 |      0.000 |           0 |      9,473 |   0.000000 | (SETF HOUSE::TRIES)
     0.000 |      0.000 |           0 |          1 |   0.000000 | HOUSE::ERROR!
     0.000 |      0.048 |  56,180,928 |     18,936 |   0.000000 | HOUSE::PROCESS-READY
     0.000 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE:RESOURCE
     0.000 |      0.000 |   1,828,992 |      9,462 |   0.000000 | HOUSE:NEW-SESSION!
     0.000 |      0.000 |  10,214,832 |          1 |   0.000000 | HOUSE:START
     0.000 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE:PARAMETERS
     0.000 |      0.000 |           0 |      9,462 |   0.000000 | HOUSE:HEADERS
     0.000 |      0.000 |           0 |      9,462 |   0.000000 | (SETF HOUSE:HEADERS)
--------------------------------------------------------------
     4.666 |      0.048 | 188,540,224 | 38,153,240 |            | Total

estimated total profiling overhead: 24.72 seconds
overhead estimation parameters:
  3.2000003e-8s/call, 6.48e-7s total profiling, 2.16e-7s internal profiling

These functions were not called:
 HOUSE::ARG-EXP HOUSE::ARGS-BY-TYPE-PRIORITY HOUSE::ARGUMENTS
 HOUSE::ASSERTION (SETF HOUSE::BODY) (SETF HOUSE::CACHE-CONTROL)
 HOUSE::CACHE-CONTROL (SETF HOUSE::CHARSET) HOUSE::CHECK-FOR-DUPES
 HOUSE:CLEAR-SESSION-HOOKS! (SETF HOUSE::CONTENT-TYPE)
 (SETF HOUSE::COOKIE) HOUSE::COPY-TRIE HOUSE::DATA HOUSE::DEBUG!
 HOUSE:DEFINE-FILE-HANDLER HOUSE::EMPTY HOUSE::EVENT
 (SETF HOUSE::EXPIRES) HOUSE::EXPIRES HOUSE:GET-SESSION!
 (SETF HOUSE::HTTP-METHOD) HOUSE::ID HOUSE::INSERT-HANDLER!
 (SETF HOUSE::KEEP-ALIVE?) (SETF HOUSE::LAST-POKED)
 (SETF HOUSE::LOCATION) (SETF HOUSE:LOOKUP) HOUSE:LOOKUP HOUSE:MAKE-SSE
 HOUSE::MAKE-TRIE HOUSE:NEW-SESSION-HOOK! HOUSE::PARSE-COOKIES
 HOUSE::PARSE-VAR HOUSE::PATH->MIMETYPE HOUSE:PATH->URI HOUSE::PATH-VAR?
 HOUSE::POKE! HOUSE::PROCESS-URI HOUSE:PUBLISH! HOUSE::READ-ALL
 HOUSE:REDIRECT! (SETF HOUSE:RESOURCE) (SETF HOUSE::RESPONSE-CODE)
 HOUSE::RETRY (SETF HOUSE:SESSION-TOKENS) HOUSE::SESSION-VALUES
 HOUSE:SUBSCRIBE! HOUSE::TRIE-INSERT! (SETF HOUSE::TRIE-MAP)
 HOUSE::TRIE-MAP HOUSE::TRIE-P (SETF HOUSE::TRIE-VALUE)
 HOUSE::TRIE-VALUE (SETF HOUSE::TRIE-VARS) HOUSE::TRIE-VARS
 HOUSE::TYPE-ASSERTION HOUSE::TYPE-EXPRESSION HOUSE::URI-DECODE
 HOUSE::VAR-KEY
 ```

 So a bunch of stuff was never called, and `buffer!`, `parse`, `write!` and `flex-stream` are predictably near the top time-sinks list. What mildly surprises me is that the session-handling primitives `idling?` and `last-poked` are generating so many `cons`es. The same can be said of `line-terminated?`, except I'm pretty sure I know what the issue _there_ is.

## Low-Hanging Fruit

This is already a pretty opportunistic optimization session, so we're going fairly surface-level in terms of chages we can make. The _lowest_ of the low hanging fruit is `line-terminated?`, which is currently defined as

```lisp
...
(defun line-terminated? (lst)
  (starts-with-subseq
   #-windows(list #\linefeed #\return #\linefeed #\return)
   #+windows(list #\newline #\newline)
   lst))
...
```

You can see that this seemingly innocent function is producing a lot of `cons`es, which is a shorthand for memory consumption in the above profiler report.

```
...
  seconds  |     gc     |    consed   |    calls   |  sec/call  |  name
--------------------------------------------------------------
...
     0.080 |      0.000 |  35,093,408 |    482,562 |   0.000000 | HOUSE::LINE-TERMINATED?
...
```

The reason is that we're using `list` to create the sequence we're checking against. Even though it's effectively a constant, it's not getting allocated once and treated as such because of the way we construct it. Rather that list of characters gets newly allocated on each `line-terminated?` call, which happens just shy of 500k times over the course of only a few thousand requests. The easy fix here is quoting the list.

```
M-x slime-profile-reset
```

```
HOUSE> (loop repeat 1000000 do (line-terminated? "testing\\r\\n"))
NIL
```

```
M-x slime-profile-report

  seconds  |     gc     |   consed   |   calls   |  sec/call  |  name
------------------------------------------------------------
     0.184 |      0.000 | 63,995,888 | 1,000,000 |   0.000000 | HOUSE::LINE-TERMINATED?
------------------------------------------------------------
     0.184 |      0.000 | 63,995,888 | 1,000,000 |            | Total

estimated total profiling overhead: 0.65 seconds
overhead estimation parameters:
  3.2000003e-8s/call, 6.48e-7s total profiling, 2.16e-7s internal profiling
...
M-x slime-profile-reset
```

```
HOUSE> (defun line-terminated? (lst)
  (starts-with-subseq
   #-windows'(#\linefeed #\return #\linefeed #\return)
   #+windows'(#\newline #\newline)
   lst))

WARNING: redefining HOUSE::LINE-TERMINATED? in DEFUN
LINE-TERMINATED?
HOUSE> (loop repeat 1000000 do (line-terminated? "testing\\r\\n"))
NIL
HOUSE>
```

```
M-x slime-profile-report
  seconds  |     gc     | consed |   calls   |  sec/call  |  name
--------------------------------------------------------
     0.136 |      0.000 |      0 | 1,000,000 |   0.000000 | HOUSE::LINE-TERMINATED?
--------------------------------------------------------
     0.136 |      0.000 |      0 | 1,000,000 |            | Total

estimated total profiling overhead: 0.65 seconds
overhead estimation parameters:
  3.2000003e-8s/call, 6.48e-7s total profiling, 2.16e-7s internal profiling
M-x slime-profile-reset
```

Bam. Did you catch the difference there? This is one of those arcane finer-points that lisp newbs wouldn't notice, so don't feel bad if you missed it.

```lisp
...
(defun line-terminated? (lst)
  (starts-with-subseq
   #-windows'(#\linefeed #\return #\linefeed #\return)
   #+windows'(#\newline #\newline)
   lst))
...
```

The difference is that we're now using `'` to create the comparison list. Which, according to either the [CLHS](http://clhs.lisp.se) or convention, _does_ signal to the compiler/runtime that the given list is going to be an absolutely constant piece of data that never changes. It therefore gets allocated once at compile-time, and gets re-used on every `line-terminated?` call thereafter.

## Session-Related Cruft

The next two two offenders, according to our highly-specific and not-at-all-real-world-reflecting profiling trial are session-related. Specifically `idling?` and `last-poked`. They both get called the same number of times, so my suspicion is that `last-poked` _only_ gets called inside of `idling?`. One look at the body of `idling?` tells me this is a justified suspicion

```
...
(defmethod idling? ((sess session))
  (> (- (get-universal-time) (last-poked sess)) +max-session-idle+))
...
```

Additionally, `last-poked` is a getter method on the `session` class, so it does the brain-dead simple job of checking an instance slot and returning its current value. It seems like the only reasons either of these methods registered on the profiling report are

1. This test focuses on a ridiculously simple handler that does nothing but write `Hello World!` to the client regardless of inputs or other considerations, which therefore means that usually very minor machinery is taking up more comparable runtime/memory than it would with more complicated business logic.
2. These particular functions get called extremely often. To the tune of 16 _million_ times over the course of our very minor tests.

So there seems to be two possible ways to address the issue:

- Call these methods less frequently
- Have the compiler inline them

### Call Them Less Frequently...

..._may_ be a non-starter. The whole point of calling either function is to evict stale sessions so that they're more difficult to hijack, which means we very probably _should_ be willing to take the consistent hit on performance to ensure security. Hypothetically, if we wanted to call `idling?` probabilistically, a quick `grep` tells us that it only gets called in `get-session!`.

```
...
(defun get-session! (token)
  (awhen (gethash token *sessions*)
    (if (idling? it)
	(progn (remhash token *sessions*) nil)
	(poke! it))))
...
```

Which means that we could, but very probably _shouldn't_ do something like

```
...
(defun get-session! (token)
  (awhen (gethash token *sessions*)
    (if (and (= 0 (random 3)) (idling? it))
	(progn (remhash token *sessions*) nil)
	(poke! it))))
...
```

so that we only actually do the stale check 33% of the time we'd like to. This would have no noticeable effect on behavior during a high-traffic period, but seems like it would have a pretty large impact on effective `session` lifetimes during low-traffic periods. I'm not sure I'd want to implement this naively, but _will_ leave a note-to-self to seriously think about implementing some performance tweaks that only awaken during traffic spikes, when they're likely to have a large impact, and stay dormant otherwise without seriously affecting performance or behavior.

### Inline Them

This _would_ be close to trivial, except that both `idling?` and `last-poked` are methods. `idling?` because I've declared it that way to increase flexibility, and `last-poked` because it's created by the `accessor` option on a `defclass` form. So it'll be a bit more effor for us specifically. First off, we basically can't use `last-poked`, and must instead resort to the slightly more verbose `(slot-value sess 'last-poked)` instead. Since calls to `last-poked` only appear in two places, and it's not an exported symbol, this sounds like a reasonable price to pay.

```lisp
;; session.lisp
...
(defmethod idling? ((sess session))
  (> (- (get-universal-time) (slot-value sess 'last-poked)) +max-session-idle+))

(defmethod poke! ((sess session))
  (setf (slot-value sess 'last-poked) (get-universal-time))
  sess)
```

Second, `idling?` can no longer be a method. Which kind of sucks from the readability standpoint (since we will no longer be explicit about what type of input it's expecting), but should be worth our time from the performance perspective.

```lisp
;; session.lisp
...
(defun idling? (sess)
  (> (- (get-universal-time) (slot-value sess 'last-poked)) +max-session-idle+))
...
```

Once that's done, we just need to add an `inline` declaration preceding the function definition, and we're good.

```lisp
;; session.lisp

...
(declaim (inline idling?))
(defun idling? (sess)
  (> (- (get-universal-time) (slot-value sess 'last-poked)) +max-session-idle+))
...
```

I've also moved the definition up above its calls in the code, _just in case_ that ends up mattering for some reason. Now that we've chopped our primary suspects, a quick re-run of the test should show us our next target.

```
M-x slime-profile-reset
```

```shell
# in eshell

~/quicklisp/local-projects/house $ wrk -t12 -c400 -d30s http://127.0.0.1:4040/hello-world
Running 30s test @ http://127.0.0.1:4040/hello-world
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     4.42ms   29.03ms   1.71s    99.87%
    Req/Sec   346.45    172.64     1.35k    77.96%
  57772 requests in 30.03s, 14.38MB read
  Socket errors: connect 0, read 58157, write 0, timeout 7
Requests/sec:   1923.57
Transfer/sec:    490.29KB
~/quicklisp/local-projects/house $
```

```
M-x slime-profile-report

  seconds  |     gc     |     consed    |    calls   |  sec/call  |  name
----------------------------------------------------------------
     5.519 |      0.000 |    52,729,616 |    116,112 |   0.000048 | HOUSE::BUFFER!
     3.388 |      0.000 |            64 |      1,160 |   0.002920 | HOUSE::CLEAN-SESSIONS!
     2.366 |      0.032 |   272,263,248 |    232,184 |   0.000010 | HOUSE::FLEX-STREAM
     1.877 |      0.188 |   383,491,456 |    232,184 |   0.000008 | HOUSE::WRITE!
     1.087 |      0.124 |   351,142,944 |    232,180 |   0.000005 | HOUSE::PARSE
     0.823 |      0.000 |            16 |  5,920,590 |   0.000000 | HOUSE::LINE-TERMINATED?
     0.789 |      0.028 |    59,615,552 |    116,090 |   0.000007 | HOUSE::HANDLE-REQUEST!
     0.664 |      0.000 |     3,401,552 |    928,734 |   0.000001 | HOUSE::CRLF
     0.385 |      0.000 |    22,653,792 |    116,090 |   0.000003 | HOUSE::NEW-SESSION-TOKEN!
     0.318 |      0.000 |     7,857,040 |    232,180 |   0.000001 | HOUSE::->KEYWORD
     0.197 |      0.012 |   125,392,640 |    116,090 |   0.000002 | HOUSE:NEW-SESSION!
     0.120 |      0.000 |     9,105,056 |    232,180 |   0.000001 | HOUSE::TRIE-LOOKUP
     0.117 |      0.008 |     6,587,360 |    116,090 |   0.000001 | HOUSE::SPLIT-AT
     0.044 |      0.000 |             0 |    116,090 |   0.000000 | HOUSE::FIND-HANDLER
     0.029 |      0.000 |     1,340,640 |    116,090 |   0.000000 | HOUSE::PARSE-PARAMS
     0.017 |      0.000 |             0 |    116,112 |   0.000000 | (SETF HOUSE::TRIES)
     0.013 |      0.000 |             0 |    116,092 |   0.000000 | HOUSE::COOKIE
     0.013 |      0.000 |             0 |    116,092 |   0.000000 | HOUSE::CHARSET
     0.009 |      0.000 |             0 |    116,112 |   0.000000 | HOUSE::BI-STREAM
     0.005 |      0.000 |             0 |    116,090 |   0.000000 | (SETF HOUSE:REQUEST)
     0.001 |      0.000 |             0 |    116,090 |   0.000000 | HOUSE::ANY-VARS?
     0.001 |      0.000 |             0 |    116,090 |   0.000000 | HOUSE:PARAMETERS
     0.000 |      0.000 |             0 |    116,092 |   0.000000 | HOUSE::KEEP-ALIVE?
     0.000 |      0.000 |             0 |    232,180 |   0.000000 | HOUSE::TOKEN
     0.000 |      0.000 |             0 |    116,092 |   0.000000 | HOUSE::CONTENT-TYPE
     0.000 |      0.000 |             0 |    116,090 |   0.000000 | HOUSE::HTTP-METHOD
     0.000 |      0.000 |             0 |    116,092 |   0.000000 | HOUSE::LOCATION
     0.000 |      0.000 |             0 |    232,224 |   0.000000 | HOUSE::TRIES
     0.000 |      0.000 |             0 | 11,841,202 |   0.000000 | HOUSE::TOTAL-BUFFERED
     0.000 |      0.000 |             0 |  5,920,590 |   0.000000 | (SETF HOUSE::TOTAL-BUFFERED)
     0.000 |      0.000 |             0 |    116,092 |   0.000000 | HOUSE::RESPONSE-CODE
     0.000 |      0.000 |             0 |    116,090 |   0.000000 | HOUSE::EXPECTING
     0.000 |      0.000 |             0 |    116,090 |   0.000000 | (SETF HOUSE::EXPECTING)
     0.000 |      0.000 |             0 |    116,112 |   0.000000 | HOUSE::STARTED
     0.000 |      0.056 |   558,371,856 |    232,204 |   0.000000 | HOUSE::PROCESS-READY
     0.000 |      0.000 |             0 |          2 |   0.000000 | HOUSE::ERROR!
     0.000 |      0.000 |             0 |    116,090 |   0.000000 | HOUSE::HANDLERS
     0.000 |      0.000 |             0 | 12,073,360 |   0.000000 | HOUSE::CONTENTS
     0.000 |      0.000 |             0 |  6,036,680 |   0.000000 | (SETF HOUSE::CONTENTS)
     0.000 |      0.000 |             0 |    116,092 |   0.000000 | HOUSE::BODY
     0.000 |      0.000 |             0 |    116,090 |   0.000000 | HOUSE:RESOURCE
     0.000 |      0.000 |    93,299,376 |          2 |   0.000000 | HOUSE:START
     0.000 |      0.000 |             0 |    232,180 |   0.000000 | (SETF HOUSE:PARAMETERS)
     0.000 |      0.000 |             0 |    116,090 |   0.000000 | HOUSE:HEADERS
     0.000 |      0.000 |             0 |    116,090 |   0.000000 | (SETF HOUSE:HEADERS)
     0.000 |      0.000 |             0 |    116,090 |   0.000000 | HOUSE:SESSION-TOKENS
     0.000 |      0.000 |           128 |  6,268,880 |   0.000000 | HOUSE:REQUEST
----------------------------------------------------------------
    17.781 |      0.448 | 1,947,252,336 | 54,331,518 |            | Total

estimated total profiling overhead: 37.81 seconds
overhead estimation parameters:
  8.000001e-9s/call, 6.9600003e-7s total profiling, 3.36e-7s internal profiling

These functions were not called:
 HOUSE::ARG-EXP HOUSE::ARGS-BY-TYPE-PRIORITY HOUSE::ARGUMENTS
 HOUSE::ASSERTION (SETF HOUSE::BODY) (SETF HOUSE::CACHE-CONTROL)
 HOUSE::CACHE-CONTROL (SETF HOUSE::CHARSET) HOUSE::CHECK-FOR-DUPES
 HOUSE:CLEAR-SESSION-HOOKS! (SETF HOUSE::CONTENT-TYPE)
 (SETF HOUSE::COOKIE) HOUSE::COPY-TRIE HOUSE::DATA HOUSE::DEBUG!
 HOUSE:DEFINE-FILE-HANDLER HOUSE::EMPTY HOUSE::EVENT
 (SETF HOUSE::EXPIRES) HOUSE::EXPIRES HOUSE:GET-SESSION!
 (SETF HOUSE::HTTP-METHOD) HOUSE::ID HOUSE::IDLING?
 HOUSE::INSERT-HANDLER! (SETF HOUSE::KEEP-ALIVE?)
 (SETF HOUSE::LAST-POKED) HOUSE::LAST-POKED (SETF HOUSE::LOCATION)
 (SETF HOUSE:LOOKUP) HOUSE:LOOKUP HOUSE:MAKE-SSE HOUSE::MAKE-TRIE
 HOUSE:NEW-SESSION-HOOK! HOUSE::PARSE-COOKIES HOUSE::PARSE-VAR
 HOUSE::PATH->MIMETYPE HOUSE:PATH->URI HOUSE::PATH-VAR? HOUSE::POKE!
 HOUSE::PROCESS-URI HOUSE:PUBLISH! HOUSE::READ-ALL HOUSE:REDIRECT!
 (SETF HOUSE:RESOURCE) (SETF HOUSE::RESPONSE-CODE) HOUSE::RETRY
 (SETF HOUSE:SESSION-TOKENS) HOUSE::SESSION-VALUES HOUSE:SUBSCRIBE!
 HOUSE::TRIE-INSERT! (SETF HOUSE::TRIE-MAP) HOUSE::TRIE-MAP
 HOUSE::TRIE-P (SETF HOUSE::TRIE-VALUE) HOUSE::TRIE-VALUE
 (SETF HOUSE::TRIE-VARS) HOUSE::TRIE-VARS HOUSE::TYPE-ASSERTION
 HOUSE::TYPE-EXPRESSION HOUSE::URI-DECODE HOUSE::VAR-KEY
```

## Buffer-related cruft

Ok, there's one more piece of session infrastructure that's still causing pains; `clean-sessions!`. That's something we very probably _can_ handle probabilistically, so I'll leave it for a bit later. But seven of the top-ten biggest time/space consumers at this point are either a direct or indirect result of an architectural choice inside of `buffer!` that I think it's finally time to explore.

```
  seconds  |     gc     |     consed    |    calls   |  sec/call  |  name
----------------------------------------------------------------
     5.519 |      0.000 |    52,729,616 |    116,112 |   0.000048 | HOUSE::BUFFER!
...
     2.366 |      0.032 |   272,263,248 |    232,184 |   0.000010 | HOUSE::FLEX-STREAM
     1.877 |      0.188 |   383,491,456 |    232,184 |   0.000008 | HOUSE::WRITE!
     1.087 |      0.124 |   351,142,944 |    232,180 |   0.000005 | HOUSE::PARSE
     0.823 |      0.000 |            16 |  5,920,590 |   0.000000 | HOUSE::LINE-TERMINATED?
     0.789 |      0.028 |    59,615,552 |    116,090 |   0.000007 | HOUSE::HANDLE-REQUEST!
     0.664 |      0.000 |     3,401,552 |    928,734 |   0.000001 | HOUSE::CRLF
...
```

Specifically, early on, I made the decision that `buffer!` needed to work in a streaming fashion. Which meant doing a very low-level non-blocking read in a tight loop. Unfortunately, there's no way to do this on byte-streams in Common Lisp so I ended up having to call `read-char-no-hang` through a bi-valent stream abstraction layer provided by [`flexi-streams`](http://weitz.de/flexi-streams/). That may also have had a ripple effect on the `write!` procedure, as well as `line-terminated?` and `crlf`. And according to my profiler, that means the decision may very well be coming back to bite me in the ass right now.

The alternative decision would be to chuck streaming in a fucking bin, and read bytes directly into an in-memory array with a blocking, but very small timeout using `trivial-timeout`, and do a fairly aggressive but probably cheaper line-termination check before we even bother converting things into `ascii`. So, lets see how this pans out.

First off, `buffer!` needs to change completely.

```lisp
...
(defmethod buffer! ((buffer buffer))
  ;; TODO - grow buffer up to +max-request-size+ when exhausted by doubling size
  ;; TODO - binary search for the first empty slot (rather than iterating)
  ;; TODO - seriously refactor this for repetition
  (unless (contents buffer)
    (setf (contents buffer) (coerce (make-array '(500)) '(vector (unsigned-byte 8)))))
  (let* ((buffed (total-buffered buffer))
	 (count
	  (handler-case
	      (trivial-timeout:with-timeout (0.01)
		(read-sequence
		 (contents buffer) (bi-stream buffer)
		 :start (total-buffered buffer)))
	    (com.metabang.trivial-timeout:timeout-error ()
	      (- (loop for i from buffed
		    when (zerop (aref (contents buffer) i)) return i)
		 buffed)))))
    (incf (total-buffered buffer) count)
    (when (request buffer) (decf (expecting buffer) count))
    (when (line-terminated? (contents buffer) (total-buffered buffer))
      (multiple-value-bind (parsed expecting) (parse buffer)
	(setf (request buffer) parsed
	      (expecting buffer) expecting
	      (contents buffer) (coerce (make-array '(100)) '(vector (unsigned-byte 8))))))
    (aref (contents buffer) (max 0 (- count 1)))))
...
```

Instead of doing a char-wise read through a `flexi-stream` like we were doing before, we're now instead reading raw octets into an array. This means we also need to change our line-termination check

```lisp
(defun line-terminated? (vec fill)
  (and (> fill 4)
       (= (aref vec (- fill 4)) 13)
       (= (aref vec (- fill 3)) 10)
       (= (aref vec (- fill 2)) 13)
       (= (aref vec (- fill 1)) 10)))
```

...and `process-ready` needs to pass the raw `socket-stream` instead of a `flex`ed stream to a new `buffer`.

```lisp
...
(defmethod process-ready ((ready stream-usocket) (conns hash-table))
  (let ((buf (or (gethash ready conns) (setf (gethash ready conns) (make-instance 'buffer :bi-stream (socket-stream ready))))))
...
```

And, finally, `parse` needs to expect an octet vector in the `contents` slot of its input buffer, rather than a reversed `list` of `char`s.

```lisp
...
(defmethod parse ((buf buffer))
  (let ((str (babel:octets-to-string (subseq (contents buf) 0 (total-buffered buf)))))
...
```

Ok; moment of truth here. Evaluating that, killing the profiler, emptying session cache and running the benchtest gives us...

drumroll...

significant, further pause...

```
~/quicklisp/local-projects/house $ wrk -t12 -c400 -d30s http://127.0.0.1:4040/hello-world
Running 30s test @ http://127.0.0.1:4040/hello-world
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    72.87ms    8.30ms 270.73ms   96.97%
    Req/Sec    62.30     29.22   101.00     54.98%
  2878 requests in 30.04s, 733.55KB read
  Socket errors: connect 0, read 2878, write 0, timeout 3
Requests/sec:     95.81
Transfer/sec:     24.42KB
~/quicklisp/local-projects/house $
```

Well... fuck. Ok; so I'm guessing `trivial-timeout` introduces a bunch of overhead into the equation, which cancels out any gains we get from using the faster data-structure. The macro-expander tells me that in `sbcl`, it basically just expands out to an `sb-ext:with-timeout` call.

```lisp
(LET ((#:|seconds-800| 0.01))
  (FLET ((#:|doit-801| ()
           (PROGN
            (READ-SEQUENCE (CONTENTS BUFFER) (BI-STREAM BUFFER) :START
                           (TOTAL-BUFFERED BUFFER)))))
    (COND
     (#:|seconds-800|
      (HANDLER-CASE
       (SB-EXT:WITH-TIMEOUT #:|seconds-800|
         (#:|doit-801|))
       (SB-EXT:TIMEOUT (COM.METABANG.TRIVIAL-TIMEOUT::C)
        (DECLARE (IGNORE COM.METABANG.TRIVIAL-TIMEOUT::C))
        (ERROR 'COM.METABANG.TRIVIAL-TIMEOUT:TIMEOUT-ERROR))))
     (T (#:|doit-801|)))))
```

So, just to satisfy my curiosity, lets see if we get anything out of calling the implementation-specific thing directly. That means `buffer!` changes yet again

```lisp
(defmethod buffer! ((buffer buffer))
  ;; TODO - grow buffer up to +max-request-size+ when exhausted by doubling size
  ;; TODO - binary search for the first empty slot (rather than iterating)
  ;; TODO - seriously refactor this for repetition
  (unless (contents buffer)
    (setf (contents buffer) (coerce (make-array '(500)) '(vector (unsigned-byte 8)))))
  (let* ((buffed (total-buffered buffer))
	 (count
	  (handler-case
	      (sb-ext:with-timeout 0.01
		(read-sequence
		 (contents buffer) (bi-stream buffer)
		 :start (total-buffered buffer)))
	    (sb-ext:timeout ()
	      (- (loop for i from buffed
		    when (zerop (aref (contents buffer) i)) return i)
		 buffed)))))
    (incf (total-buffered buffer) count)
    (when (request buffer) (decf (expecting buffer) count))
    (when (line-terminated? (contents buffer) (total-buffered buffer))
      (multiple-value-bind (parsed expecting) (parse buffer)
	(setf (request buffer) parsed
	      (expecting buffer) expecting
	      (contents buffer) (coerce (make-array '(100)) '(vector (unsigned-byte 8))))))
    (aref (contents buffer) (max 0 (- count 1)))))
```

Ok; one more time.

```
~/quicklisp/local-projects/house $ wrk -t12 -c400 -d30s http://127.0.0.1:4040/hello-world
Running 30s test @ http://127.0.0.1:4040/hello-world
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    74.18ms   45.39ms   1.81s    99.41%
    Req/Sec    32.94     14.26    70.00     68.08%
  2875 requests in 30.04s, 732.79KB read
  Socket errors: connect 0, read 2875, write 0, timeout 2
Requests/sec:     95.71
Transfer/sec:     24.39KB
~/quicklisp/local-projects/house $
```

Ok, so I'm sort of ready to admit defeat here. I mean, I know that I'm serching element-by-element through each incoming buffer for its termination point, and that could be done more efficiently, **but**

1. That's a pretty tiny buffer. Straight up 500 bytes at the moment, which means that it won't be a _major_ source of slowdown.
2. Hypothetically, even if that was the case, it can't possibly be making our requests/sec ~100 times lower.

In other words, I guess I was wrong; the char-by-char processing approach doesn't cost us very much here. Lets put all of that away and focus on more micro-optimization. Incidentally, just to make sure I'm not going insane somehow, once I put it back, perf metrics go back up to the level expected.

```lisp
~/quicklisp/local-projects/house $ wrk -t12 -c400 -d30s http://127.0.0.1:4040/hello-world
Running 30s test @ http://127.0.0.1:4040/hello-world
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     4.24ms   54.74ms   1.63s    99.51%
    Req/Sec     1.47k     1.05k    6.88k    79.98%
  174596 requests in 30.03s, 43.46MB read
  Socket errors: connect 0, read 174651, write 0, timeout 17
Requests/sec:   5813.36
Transfer/sec:      1.45MB
~/quicklisp/local-projects/house $
```

## Back to Micros

So the current major culprit here is `clean-sessions!`.

### `clean-sessions!`

Which is understandable, because

1. it currently runs every 100 times we start a fresh session (which means it runs ~500 or so times over the course of one of these bench tests)
2. it iterates over the full session table when it runs
3. it never cleans out any sessions, since they won't age enough over the course of a test to get evicted, which means that the table it's iterating over only ever gets larger

The easiest solution is to make calling it probabilistic. In addition to being mildly faster, that will also remove the need for the local state variable `session-count`, which always kind of bugged me.

So, step one, `new-session!` changes to

```lisp
;; session.lisp
...
(defun new-session! ()
  (when (zerop (random +clean-sessions-every+))
    (clean-sessions!))
  (let ((session (make-instance 'session :token (new-session-token!))))
    (setf (gethash (token session) *sessions*) session)
    (loop for hook in *new-session-hook*
       do (funcall hook session))
    session))
...
```

and while we're at it, we may as well bump `+clean-sessions-every+` up a bit.

```lisp
;; package.lisp
...
(defparameter +clean-sessions-every+ 10000)
```

I'm also considering re-factoring the server to make sessions optional. It's not a strong concern mainly because most applications of any size will want session state, and the ones that don't probably don't care about using an application server to begin with. In other words, it feels like this would be making things less convenient for the programmer specifically to do better on a benchmark, and that's not the sort of shit I typically like pulling.

With the new settings and implementation, `clean-sessoins!` and `new-session!` both drop pretty far down our list of culprits anyhow.

```
  seconds  |     gc     |     consed    |    calls   |  sec/call  |  name
----------------------------------------------------------------
     1.721 |      0.000 |    56,793,248 |     70,064 |   0.000025 | HOUSE::BUFFER!
     1.475 |      0.052 |   111,406,304 |    140,108 |   0.000011 | HOUSE::FLEX-STREAM
     1.079 |      0.036 |   146,585,744 |    140,108 |   0.000008 | HOUSE::WRITE!
     0.526 |      0.012 |   166,425,216 |    140,106 |   0.000004 | HOUSE::PARSE
     0.468 |      0.012 |    14,476,848 |     70,053 |   0.000007 | HOUSE::HANDLE-REQUEST!
     0.305 |      0.000 |             0 |    560,431 |   0.000001 | HOUSE::CRLF
     0.298 |      0.000 |    17,791,024 |     70,053 |   0.000004 | HOUSE::NEW-SESSION-TOKEN!
     0.172 |      0.000 |             0 |         12 |   0.014333 | HOUSE::CLEAN-SESSIONS!
     0.159 |      0.000 |    16,344,288 |    140,106 |   0.000001 | HOUSE::->KEYWORD
     0.127 |      0.000 |    93,279,184 |     70,053 |   0.000002 | HOUSE:NEW-SESSION!
     0.105 |      0.012 |    34,233,792 |    420,318 |   0.000000 | HOUSE::LINE-TERMINATED?
     0.074 |      0.000 |     2,032,096 |     70,053 |   0.000001 | HOUSE::SPLIT-AT
     0.030 |      0.000 |             0 |    140,106 |   0.000000 | HOUSE::TRIE-LOOKUP
     0.028 |      0.000 |             0 |     70,053 |   0.000000 | HOUSE::FIND-HANDLER
     0.014 |      0.000 |             0 |     70,053 |   0.000000 | HOUSE::ANY-VARS?
...
```

I'm not taking a look at `buffer!` right this very second, because I just spent a bunch of time on it. Which means that `flex-stream` is my next target.

## `flex-stream` and `crlf`

This is another method, and I get the feeling that's hurting us here. Specifically, it means that method dispatch happens every time we call `flex-stream`, _and_ it means we can't inline it. That second one is also the only problem I can see with `crlf`. So lets give this a shot, I guess.

```lisp
;; util.lisp

...
(declaim (inline flex-stream))
(defun flex-stream (sock)
  (flex:make-flexi-stream (socket-stream sock) :external-format :utf-8))
...
```

```lisp
;; house.lisp
...
(declaim (inline crlf))
...
```

For the record, by the way, turning the profiling off at this point gives us

```
~/quicklisp/local-projects/house $ wrk -t12 -c400 -d30s http://127.0.0.1:4041/hello-world
Running 30s test @ http://127.0.0.1:4041/hello-world
  12 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.67ms   50.90ms   1.68s    99.48%
    Req/Sec     1.76k     1.19k    8.94k    76.33%
  232264 requests in 30.05s, 57.81MB read
  Socket errors: connect 0, read 232620, write 0, timeout 20
Requests/sec:   7730.33
Transfer/sec:      1.92MB
~/quicklisp/local-projects/house $
```

which means that we're very slowly building up to the performance of `tornado` in `pypy` running on one thread. Optimizing the hell out of `buffer!` and `write!` may in fact get us there. So, lets try.

## `write!`


## TODO

- more miro-opts to major sources of slowdown
- think a bit more about spike-conditional optimization, but don't bother implementing yet
