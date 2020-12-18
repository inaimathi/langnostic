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

- TODO - some code conversions
- TODO - another benchmark round

## Step 2 - Kill Classes

The second step is to kill `class` definitions entirely. Their `accessor` functions are _also_ generic, and therefore rely on method dispatch. `struct`s are a bit clumsier, but _probably_ faster in the end.

- TODO - some code conversions
- TODO - another benchmark round

## Step 3 - CLJ

This is for my own edification. Now that we have what I _think_ is the fastest possible version of house without going multi-worker, I want to see whether[^more-realistically-how] [`clj`](TODO) does performance damage to the implementation. I want to see this because, the `clj` datastructures and syntax _really_ improve readability and `REPL` development; there's a _bunch_ of situations in which I missed having that level of visibility into my structures before I even began this benchmark article. There's even probably a few places where it _saves_ some performance by referencing other partial structures. The problem is that _I'm guessing_ it's a net negative in terms of performance, so I want to see what a conversion would do to my benchmark before I go through with it.

[^more-realistically-how]: More realistically, "how much" rather than "whether"
