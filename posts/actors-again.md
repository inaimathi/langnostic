That [`cl-actors` fork](https://github.com/Inaimathi/Common-Lisp-Actors) has gotten a few hours thrown at it. This time around, I integrated the [Optima pattern-matching library](https://github.com/m2ym/optima), and made certain things a little easier. I'm nowhere near done yet though; in addition to the rest of the ToDos from [last time](/posts/actors), and seeing what I can do with [green threads](https://github.com/deliciousrobots/green-threads), I need to settle things that the Erlang guys have clearly been thinking about for a few years.

## <a name="first" href="#first"></a>First

How do you deal with error reporting/handling here? And I specifically mean

```lisp
(defmethod initialize-instance :after ((self actor) &amp;key)
  "Uses the main function name to create a thread"
  (with-slots (behavior in name thread) self
    (setf thread
          (bt:make-thread
           (lambda ()
             (loop
                (handler-case
                    (let ((res (funcall behavior (dequeue in))))
                      (loop for target in (targets self)
                         do (enqueue res target)))
                  (match-error (e)
                    (format t "There isn't a match clause that fits. Do something more intelligent with unmatched messages.~%~a~%~%" e))
                  (error (e)
                    (format t "BLEARGH! I AM SLAIN! (this should kill the actor, and possibly call some fall-back mechanism)~%~a~%~%" e)))))
           :name name))))
```
here. Spitting unmatched messages out at `*standard-output*` sounds *kind of* ok, until you start thinking about how you'd deal with any kind of non-trivial system, or a situation where you want to defer those messages to someone else who might know how to deal with them. The standard [Supervisor infrastructure](http://www.erlang.org/doc/man/supervisor.html) that Erlang implements looks like it would be a good solution, and will probably going to be easier to put together in Lisp. That's more or less the only sane option for things like unmatched messages, because you don't ever want those to derail the whole system.

The second `case` there is more ambiguous though.

```lisp
(error (e)
  (format t "BLEARGH! I AM SLAIN! (this should kill the actor, and possibly call some fall-back mechanism)~%~a~%~%" e))
```

That handles all other `error`s. Run-time snafus like passing the wrong number of arguments to `format`. For these, you do really, truly want to take the actor out of commission until it gets fixed; there's no point whatsoever in trying to process further messages until that time. So another reasonable approach here would be to use Common Lisp's built-in condition system. That is, re-raise the `error` and give the user a restart option to define a new behavior in-line.

I don't know, there might be pitfalls there that I'm not seeing, which is why I need to think pretty hard about it, and then try it out.

## <a name="second" href="#second"></a>Second

I want to make sure that networks resulting from this system are flexible enough to withstand change. That's a tougher one, and the built in [behav](https://github.com/naveensundarg/Common-Lisp-Actors/blob/master/actors.lisp#L100-L104) function from [the original `cl-actors`](https://github.com/naveensundarg/Common-Lisp-Actors) doesn't quite satisfy. The two things we want to preserve from an actor when we're modifying it, if we want robust networks, are its state and its inbound message queue. The second is hopefully obvious, but the first might not be given how that `behav` function I just linked is implemented. It leaves the task of assigning new state up to the user, who may not know what the latest state of the actor is. Worse than that, there may be no way for them to find out, because that state is locked away in a closure with no hooks other than the ones manually defined by the behavior they're trying to replace. I'm not entirely sure what the solution there is, but it probably won't be straightforward.

What I *apparently* want is a macro that takes a series of `ematch` clauses, and returns a macro that accepts a list of state variables/values, which returns a function that takes a message and returns whatever we want to pass on. I'd call that first macro to define the skeleton of my new behavior, then pass the result through to an actor which would provide its own internal state to fill in the gaps, then take the result of that operation and assign it to a new behavior. The end result would be a function which I can define on the outside somewhere, but which will take into consideration an actors most current internal state when finally applied. The queue, of course, goes nowhere, and assuming the new behavior function doesn't error out anywhere, the actor should continue `dequeue`ing on its merry way

## <a name="finally" href="#finally"></a>Finally

That "assuming" isn't to be glossed over. Replacing an actor or behavior with another is straightforward in toy examples, but in an actual, running system, you want good fail-over capabilities. Specifically, if the new guy chokes on his first couple of messages, you want the option of slotting in the old process before too much time has elapsed. You definitely *don't* want to grind the entire downstream system to a halt while the programmers figure out what the issue is with the new code. Another thing that might be useful is a default `:test` message that you can throw at an actor which should tell the sender whether it can respond to the given message. In a situation where you're replacing an existing actor with a new one, or just replacing the behavior of an existing actor, you want to know that the messages sent out by the new thing are going to be intelligible to their targets before committing to the change-over. How the reporting for this is going to work, I haven't the first clue, but I've got more than enough implementation to do already, so I'll probably leave that one for the next article.
