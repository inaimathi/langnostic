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

So that's a decent start. Out of the gate, according to [this](https://github.com/fukamachi/woo/blob/master/benchmark.md#benchmarks), `house` outperforms `tornado` (unless running in `pypy`), `wookie` and `hunchentoot` in terms of requests/second. Which is not bad for a server that had no intention whatsoever of performing at all well.

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
