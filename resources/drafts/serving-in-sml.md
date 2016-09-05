This is about the third or fourth approach I'm trying to the explanation of [this](https://github.com/Inaimathi/serve-sml). And I think It might be about time to try it piece-wise.

## HTTP Hello World dot SML

And yes, I'm totally submitting this to the appropriate Rosetta Code task, assuming no-one beats me to it. The ML examples in general leave a lot to be desired from the completeness perspective.

```
fun sendHello sock = 
    let val res = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 12\r\n\r\nHello world!\r\n\r\n"
        val slc = Word8VectorSlice.full (Byte.stringToBytes res)
    in 
	Socket.sendVec (sock, slc);
	Socket.close sock
    end;

fun acceptLoop serv =
    let val (s, _) = Socket.accept serv
    in sendHello s;
       acceptLoop serv
    end
    handle _ => Socket.close serv;

fun serve port =
    let val s = INetSock.TCP.socket()
    in Socket.Ctl.setREUSEADDR (s, true);
       Socket.bind(s, INetSock.any port);
       Socket.listen(s, 5);
       print "Entering accept loop...\n";
       acceptLoop s
    end;

serve 8181;;
```

That is the most basic possible HTTP server in existence. It's entirely serial, ignores all input, and sends out a plain-text `Hello world!` response. In addition to showing you a slowly accumulating server, I'm also going to do something else different this time. Namely, I care about performance. Because as easy as it is to say "nope, optimize later", I want to see how the low-level nuts-and-bolts *really* fit together. To that end, I'm taking a page from the [`woo` benchmarks](https://github.com/fukamachi/woo#how-fast)

```sh
$ mlton basic.sml
$ ./basic
```

*(and in another terminal window)*


```sh
inaimathi@self:~$ cd ~/projects/wrk
inaimathi@self:~/projects/wrk$ ./wrk -c 10 -t 4 -d 10 http://127.0.0.1:8181
Running 10s test @ http://127.0.0.1:8181
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   379.27us  427.87us   0.86ms   56.21%
    Req/Sec   425.25      2.48k   15.89k    97.14%
  21341 requests in 10.01s, 1.12MB read
  Socket errors: connect 0, read 2, write 21331, timeout 32
Requests/sec:   2132.86
Transfer/sec:    114.56KB

inaimathi@self:~/projects/wrk$ ./wrk -c 10 -t 4 -d 10 http://127.0.0.1:8181
Running 10s test @ http://127.0.0.1:8181
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.00us    0.00us   0.00us  100.00%
    Req/Sec     0.00      0.00     0.00    100.00%
  6816 requests in 10.01s, 366.09KB read
  Socket errors: connect 0, read 10, write 6798, timeout 32
Requests/sec:    681.12
Transfer/sec:     36.58KB

inaimathi@self:~/projects/wrk$ ./wrk -c 10 -t 4 -d 10 http://127.0.0.1:8181
Running 10s test @ http://127.0.0.1:8181
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   304.85us  466.49us   1.04ms   71.14%
    Req/Sec     1.81k     4.83k   16.00k    87.72%
  69485 requests in 10.02s, 3.64MB read
  Socket errors: connect 0, read 18, write 69459, timeout 30
Requests/sec:   6936.41
Transfer/sec:    372.56KB

inaimathi@self:~/projects/wrk$ ./wrk -c 10 -t 4 -d 10 http://127.0.0.1:8181
Running 10s test @ http://127.0.0.1:8181
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   166.66us  157.56us 316.00us   53.96%
    Req/Sec    79.78      1.04k   15.44k    99.37%
  8995 requests in 10.01s, 483.13KB read
  Socket errors: connect 0, read 3, write 8984, timeout 33
Requests/sec:    898.61
Transfer/sec:     48.27KB

inaimathi@self:~/projects/wrk$ ./wrk -c 10 -t 4 -d 10 http://127.0.0.1:8181
Running 10s test @ http://127.0.0.1:8181
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   626.32us  492.40us   1.01ms   61.73%
    Req/Sec   130.96      1.37k   15.33k    99.10%
  11388 requests in 10.01s, 611.66KB read
  Socket errors: connect 0, read 16, write 11364, timeout 34
Requests/sec:   1137.89
Transfer/sec:     61.12KB
inaimathi@self:~/projects/wrk$
```

So. In a single thread, without bothering to read input from the incoming client sockets, using a completely sequential technique, without writing anywhere other than the client socket, we can expect to serve something on the order of 3000 requests/second; between 600 and 7000. Which has us sitting [between `tornado` and `hunchentoot`, according to the `woo` graphs](https://github.com/fukamachi/woo#how-fast). Lets see if we can do better, more flexibly. And, if we're going to be any more flexible *at all*, we'll need to read input from the client servers making requests of us.

```ml
fun sendHello sock = 
    let val res = "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nHello world!\r\n\r\n"
        val slc = Word8VectorSlice.full (Byte.stringToBytes res)
	val arr = Word8Array.array (500, Word8.fromInt 0)
    in 
	Socket.recvArrNB (sock, Word8ArraySlice.slice (arr, 0, NONE));
	(* pr (Word8ArraySlice.slice (arr, 0, NONE)); *)
	Socket.sendVec (sock, slc);
	Socket.close sock
    end;

fun acceptLoop serv =
    let val (s, _) = Socket.accept serv
    in
	sendHello s;
	acceptLoop serv
    end
    handle _ => Socket.close serv;

fun serve port =
    let val s = INetSock.TCP.socket()
    in Socket.Ctl.setREUSEADDR (s, true);
       Socket.bind(s, INetSock.any port);
       Socket.listen(s, 5);
       print "Entering accept loop...\n";
       acceptLoop s
    end;

serve 8181;;
```

And this has got to be the most depressing part of this entire writeup. Seriously. If it isn't, I'm not fucking publishing it, so I can tell you that much without having actually written all of it.

```sh
inaimathi@self:~/projects/wrk$ ./wrk -c 10 -t 4 -d 10 http://127.0.0.1:8181
Running 10s test @ http://127.0.0.1:8181
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     4.33ms    4.00ms   8.03ms   53.91%
    Req/Sec   413.67      2.33k   15.22k    96.90%
  20718 requests in 10.00s, 1.09MB read
  Socket errors: connect 0, read 223, write 20487, timeout 32
Requests/sec:   2071.38
Transfer/sec:    111.26KB

inaimathi@self:~/projects/wrk$ ./wrk -c 10 -t 4 -d 10 http://127.0.0.1:8181
Running 10s test @ http://127.0.0.1:8181
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   143.93us  122.81us 250.00us   57.27%
    Req/Sec   274.83      1.95k   15.11k    98.02%
  15599 requests in 10.01s, 837.84KB read
  Socket errors: connect 0, read 0, write 15591, timeout 32
Requests/sec:   1558.62
Transfer/sec:     83.71KB

inaimathi@self:~/projects/wrk$ ./wrk -c 10 -t 4 -d 10 http://127.0.0.1:8181
Running 10s test @ http://127.0.0.1:8181
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.82ms    5.64ms  12.16ms   68.57%
    Req/Sec     0.90k     3.43k   15.11k    93.41%
  37290 requests in 10.01s, 1.96MB read
  Socket errors: connect 0, read 134, write 37148, timeout 31
Requests/sec:   3726.06
Transfer/sec:    200.13KB
inaimathi@self:~/projects/wrk$
```

*Just reading input* knocks us down to between one half and one third of our `"Hello World!"` requests/second stats.
