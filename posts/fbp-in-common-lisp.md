Guess what!

No.

I can finally tell you what I'm doing at work. A friend has suggested that I just not mention that there are things I still can't talk about, so I won't. In any case, all the interesting stuff is fair game. Apparently, I'm allowed to talk about it in much greater detail than I can write about it because a persistent record is still frightening to some humans. Progress, I guess.

We've been doing [Flow Based Programming](http://en.wikipedia.org/wiki/Flow-based_programming) in [Common Lisp](http://www.gigamonkeys.com/book/).

**Fuck** it feels good to finally get that off my chest. I have no idea how [Yegge](http://steve-yegge.blogspot.ca/) stopped blogging. I'm guessing he hasn't, but rather just stopped publishing the results. I've been writing about one thing for the past little while, and the number of ideas I *need* to discuss with the [rubber duck](http://en.wikipedia.org/wiki/Rubber_duck_debugging) at some point is staggering. If my output rate were zero, I would probably have a pretty tenuous grip on my sanity by this point.

This is heading off topic. Once more, *with feeling*

### <a name="flow-based-programming-in-common-lisp" href="#flow-based-programming-in-common-lisp"></a>Flow Based Programming in Common Lisp

I'm not sure what I think about it yet. Lets just be clear about that up-front. You'll find plenty of FBP True Believers on the appropriate [Google group](https://groups.google.com/forum/#!forum/flow-based-programming)<a name="note-Wed-Nov-06-161759EST-2013"></a>[|1|](#foot-Wed-Nov-06-161759EST-2013), but I am not one of those. The fact that I'm willing to throw a couple years behind the idea implies curiosity, nothing more.

A matter quite apart from the underlying structure is our implementation of it, and I **am** certain that it's over-complicated. Granted, based on all the others I've seen, ours is the *least* over-complicated, but still. That's something I'll aim to fix, with a personal project as a last resort, if I can't convince anyone else about it. But I digress. Again.

Here's why I'm curious. This is what a basic web server looks like in flow-based terms:

[fbp-web-server.jpg]

And here's what it looks like once you add SSE capability to it:

[fbp-web-with-SSE.jpg]

and finally, here's what it looks like when we add sessions into the mix

[fbp-web-with-SSE+Session.jpg]

The above is by far the most useful set of images I've got for understanding what's actually going on behind the scenes of a page-view. I've worked through the principles in multiple languages and spent quite a bit of time thinking about it, but until I sat down to draw it out, it didn't feel like I *really* understood what needed to be done. You probably don't know the same languages I do, but the above is still likely intelligible to you. So that's why I'm curious.

### <a name="flow-based-programming-vs-functional-programming" href="#flow-based-programming-vs-functional-programming"></a>Flow Based Programming vs. Functional Programming

Before I go, I want to tackle this, because several people I've talked to have gotten tripped up in the comparison. Including me. I ended up deleting a few lines from this post that said


>   The underlying problem for my lack of "wow" reaction might actually be my usual languages. I'm *used* to thinking about streams moving between inter-connected, lazy processors. That's the main way I conceptualize Haskell. In fact, if you squint just a bit, it's the way you can conceptualize most functional programs, pure or not.   
> --Inaimathi  


The difference is that functional programming focuses on partial conceptual separation, whereas FBP takes the isolation concept a few steps further by enforcing *complete* conceptual separation as well as complete *temporal* separation. Here's the accompanying thought experiment, just to clarify what I mean by that.

Suppose you were writing in a functional language and wrote the following:

```python
def foo():
   ...
   bar("something")
   baz("something else")

def bar(arg):
   ...
   doStuff()

def baz(arg):
   ...
   doOtherStuff()
```

Firstly, notice that, while the functions are conceptually separate units<a name="note-Wed-Nov-06-161805EST-2013"></a>[|2|](#foot-Wed-Nov-06-161805EST-2013), `foo` still has to know about `bar` and `baz`. That prevents total isolation. Yes, you can re-define `bar` and `baz` in-flight if your language is dynamic enough, but you need to have them both defined and you need to have them named `bar` and `baz`.

Secondly, note that what's happening there is most likely a bunch of synchronous work. That is, when `foo` calls `bar` and `baz`, both calls complete and then `foo` returns the return value of `baz`<a name="note-Wed-Nov-06-161811EST-2013"></a>[|3|](#foot-Wed-Nov-06-161811EST-2013). If you wrote the equivalent in a *pure*-functional language, the actual work may happen in a different order than you see it written out, subject to what the compiler can prove about the behavior of the functions involved, but `bar` and `baz` will still complete before `foo` does. If they didn't, you'd get some unexpected behavior in any callers of `foo`.

Now, lets take a look at the apparently equivalent, Lisp-flavoured, FBP-style program.

```lisp
(define-part foo ()
  ...
  (out! :out "something")
  (out! :log "something else"))

(define-part bar ()
  ...
  (do-stuff))

(define-part baz ()
  (do-other-stuff))

(define-container box 
    (:foo (foo) :bar (bar) :baz (baz))
  ((:foo :out) -> (:bar :in))
  ((:foo :log) -> (:baz :in))))
```

First, notice that `foo` doesn't know anything about `bar` or `baz`, or anything about the *existence* of either. At some point during its execution, it outputs two messages to some implementation-dependent output structure, but critically *`foo` itself is not responsible for delivering those outputs to their consumers*. That allows for total part-agnosticism; you really can shuffle parts around with pin-equivalent parts and have the result work. In functional or actors-based systems, you can *almost* do the same; the exception is that since each sender/caller has to name targets in some way, you need to change small chunks of code in places where functions/actors interoperate.

Second, note that there's nothing in this system about the timing of `bar` and `baz`. This omission includes the fact that both or neither may run before `foo` completes. In this model of the world, if `bar` takes a while to run, neither `baz` nor `foo`, nor any of their callers are prevented from further operation. The second critical difference is, essentially, that asynchronous operation is the norm.


* * *
##### Footnotes

1 - <a name="foot-Wed-Nov-06-161759EST-2013"></a>[|back|](#note-Wed-Nov-06-161759EST-2013) - Currently mostly Javascripters thanks to the hype surrounding [noflo](http://noflojs.org/).

2 - <a name="foot-Wed-Nov-06-161805EST-2013"></a>[|back|](#note-Wed-Nov-06-161805EST-2013) - Assuming the programmer hasn't done something "clever" with global variables, `goto`s or `comefrom`s.

3 - <a name="foot-Wed-Nov-06-161811EST-2013"></a>[|back|](#note-Wed-Nov-06-161811EST-2013) - which happens to be the return value of `doOtherStuff`.
