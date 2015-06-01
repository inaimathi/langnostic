I've been thinking about this for a bit, and I'm honestly not sure I can understand it while it's in my head. You know what time it is.

In general, having Falsy things makes it easier to use the `if` statement, and not having them means getting more explicit code. With that in mind, here's the state of play across the spectrum.

### <a name="haskell"></a>Haskell

doesn't take your shit. If you want to use `if` instead of pattern matching or guards, you're damn well going to pass it a `Bool`. 

```haskell
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> let yayNay val = if val then "Yay!" else "Nay!"
Prelude> yayNay False
"Nay!"
Prelude> yayNay []

&lt;interactive>:4:8:
    Couldn't match expected type `Bool' with actual type `[a0]'
    In the first argument of `yayNay', namely `[]'
    In the expression: yayNay []
    In an equation for `it': it = yayNay []
Prelude> yayNay ""

&lt;interactive>:5:8:
    Couldn't match expected type `Bool' with actual type `[Char]'
    In the first argument of `yayNay', namely `""'
    In the expression: yayNay ""
    In an equation for `it': it = yayNay ""
Prelude> yayNay 0

&lt;interactive>:6:8:
    No instance for (Num Bool)
      arising from the literal `0'
    Possible fix: add an instance declaration for (Num Bool)
    In the first argument of `yayNay', namely `0'
    In the expression: yayNay 0
    In an equation for `it': it = yayNay 0
Prelude> 
```

If you want to check whether something is `not empty` rather than merely `False`, check for *that*.

```haskell
empty [] = True
empty _ = False
```

or, better yet, just use pattern matching in the function you're writing instead of explicit `if`

```haskell
foo [] = handleEmpty
foo (a:b:rest) = handleTwo
foo list = handleN
```

Finally, if you want to express "this computation can fail": be explicit about it and use a `Maybe`. Then handle the `Nothing` case, whether with a `>>=` or a `case` or a `do`. Haskell likes being explicit.

### <a name="python"></a>Python

At the entirely other end of the spectrum is Python. It'll take your shit, interpreting `if` in a Pythonic™© way to mean the appropriate empty check in some contexts.

```python
>>> def yayNay(val):
...     if val:
...             return "Yay!"
...     else:
...             return "Nay!"
... 
>>> yayNay(False)
'Nay!'
>>> yayNay([])
'Nay!'
>>> yayNay({})
'Nay!'
>>> yayNay("")
'Nay!'
>>> yayNay(()) ## the empty tuple
'Nay!'
>>> yayNay(None)
'Nay!'
>>> yayNay(0)
'Nay!'
>>> 
```

I'm not sure how to feel about that. On the one hand, yes, empty checks are easier. On the other, empty checks usually aren't what you actually want<a name="note-Fri-May-24-141347EDT-2013"></a>[|1|](#foot-Fri-May-24-141347EDT-2013), and I can't see another benefit of doing things this way. I also don't really understand why `[]` and `0` should be Falsy, but `[0]` should be Truthy. Also, it's not quite as simple as "Empty values should be Falsy". Because there are classes that are in the standard library for which this behavior would also make sense<a name="note-Fri-May-24-141352EDT-2013"></a>[|2|](#foot-Fri-May-24-141352EDT-2013), but that lack it.

```python
>>> import Queue
>>> Queue.Queue()
&lt;Queue.Queue instance at 0x2837518>
>>> yayNay(Queue.Queue())
'Yay!'
>>> 
```

Despite those built-ins, it does seem to be possible to define your own Falsies. You can either define a `[__nonzero__](http://docs.python.org/2/reference/datamodel.html#object.__nonzero__)` method for your class, which should return the explicit boolean value you want it mapped to, or a `[__len__](http://docs.python.org/2/reference/datamodel.html#object.__len__)` method, in which case your class will be treated as Falsy if its `len` is zero.

```python
>>> class Falsy(object):
...     def __nonzero__(self):
...             return False
... 
>>> yayNay(Falsy())
Nay!
>>> class ShittyStack(object):
...     def __init__(self):
...             self.stack = []
...     def __len__(self):
...             return len(self.stack)
...     def push(self, thing):
...             self.stack.append(thing)
...     def pop(self):
...             return self.stack.pop()
... 
>>> s = ShittyStack()
>>> yayNay(s)
Nay!
>>> s.push(1)
>>> yayNay(s)
Yay!
```

### <a name="javascript"></a>JavaScript

does almost the same thing as Python, but adds `null`, `undefined` and `NaN` to the list of Falsy values, and considers empty sequences *other* than the empty string Truthy. That is,

```javascript
>// from the Chromium prompt
undefined
> function yayNay(val) { if (val) return "Yay!"; else return "Nay!"; }
undefined
> yayNay(0)
"Nay!"
> yayNay([])
"Yay!"
> yayNay({})
"Yay!"
> yayNay(undefined)
"Nay!"
> yayNay(null)
"Nay!"
> yayNay("")
"Nay!"
> yayNay(NaN)
"Nay!"
```

There are no tuples in JS, so it can't do anything there. I have no idea what the reasoning is otherwise though. I especially have no idea what would possess someone to think that making the empty array Truthy *and* the empty string Falsy, unless strings are actually implemented as linked lists underneath.

### <a name="common-lisp"></a>Common Lisp

is middle-of-the road in this respect. It has a canonical `t` and `nil` as `True`/`False`, but `nil` also pulls double-duty as the empty list.

```lisp
; SLIME 2012-09-04
CL-USER> (defun yay-nay (val) (if val :yay! :nay!))
YAY-NAY
CL-USER> (yay-nay 0)
:YAY!
CL-USER> (yay-nay (vector))
:YAY!
CL-USER> (yay-nay (make-hash-table))
:YAY!
CL-USER> (yay-nay (list))
:NAY!
CL-USER> (yay-nay '())
:NAY!
CL-USER> (yay-nay nil)
:NAY!
CL-USER> 
```

To a first approximation, it seems that the rationale here is "Sequences that you're likely to interact with through recursion should be Falsy when empty". Except that breaks down when you think about it a bit, because Common Lisp doesn't support tail call optimization either, the way that say, Scheme does.

Speaking of which...

### <a name="scheme"></a>Scheme

```scheme
CHICKEN
(c)2008-2011 The Chicken Team
(c)2000-2007 Felix L. Winkelmann
Version 4.7.0 
linux-unix-gnu-x86-64 [ 64bit manyargs dload ptables ]
compiled 2011-09-05 on gladstone.duckburg.org (Linux)

#;1> (define (yay-nay val) (if val 'yay! 'nay!))
#;2> (yay-nay 0)
yay!
#;3> (yay-nay nil) ;; wait, fuck, that doesn't exist here

Error: unbound variable: nil

        Call history:

        &lt;syntax>          (yay-nay nil)
        &lt;eval>    (yay-nay nil) &lt;--
#;3> (yay-nay '())
yay!
#;4> (yay-nay (list))
yay!
#;5> (yay-nay #f)
nay!
#;6> (yay-nay (null? (list)))
yay!
#;7> (yay-nay (not (list)))
nay!
#;8>
```

Scheme, or at least [Chicken Scheme](http://www.call-cc.org/), *doesn't* seem to treat anything but an actual `#f` as false. Which is mildly bizarre, because the recursion logic would actually make sense here. 

### <a name="clojure"></a>Clojure

seems to do its usual and split the difference between Common Lisp and Scheme.

```clojure
REPL started; server listening on localhost port 40977
user=> (defn yay-nay [val] (if val :yay! :nay!))
#'user/yay-nay
user=> (yay-nay 0)
:yay!
user=> (yay-nay [])
:yay!
user=> (yay-nay "")
:yay!
user=> (yay-nay (list))
:yay!
user=> (yay-nay '())
:yay!
user=> (yay-nay nil)
:nay!
user=> (yay-nay false)
:nay!
user=> 
```

That is, empty sequences are all Truthy, `false` is obviously `false`, and `nil` is a Falsy value that doesn't double as the empty list.

### <a name="conclusion"></a>Conclusion

Hah! You thought I was going to conclude something?

### <a name="nonconclusion"></a>Non-Conclusion

Why is `nil` Falsy in Clojure? Why is `[]` Truthy but `""` Falsy in JavaScript? Why, if a language has already decided to support empty sequences and values as Falsy, is a sequence composed of nothing but Falsy values Truthy? Why give this treatment only to some sequences and container values?

I haven't a fucking clue. And I guess I'll be asking pointed questions at the various language groups I attend periodically. Each of these languages have gotten me mileage, and I'm used to doing explicit `empty` checks, so Falsy empty values don't give me any sort of concrete advantage. Anything other than the Haskell approach seems arbitrary. Though to be fair, purely functional languages don't have to deal with the idea of pointers<a name="note-Fri-May-24-141409EDT-2013"></a>[|3|](#foot-Fri-May-24-141409EDT-2013) which makes things easier.

There definitely isn't any kind of consensus on the issue, and I really wasn't expecting any, but it bothers me mightily that I can't even get my head around what the parameters of the problem are. It's not that we haven't decided whether Falsy or Explicit is better, it's that I'm not sure what factors would play into picking one over the other.

So much for peeling that one, I guess. I'll keep you posted if I have any more thoughts.

* * *
##### Footnotes

1 - <a name="foot-Fri-May-24-141347EDT-2013"></a>[|back|](#note-Fri-May-24-141347EDT-2013) - And don't make much sense anyway since [Tail Recursion is unpythonic](http://neopythonic.blogspot.ca/2009/04/tail-recursion-elimination.html) for some odd-sounding non- or semi-reasons.

2 - <a name="foot-Fri-May-24-141352EDT-2013"></a>[|back|](#note-Fri-May-24-141352EDT-2013) - For some values of `make` and `sense`.

3 - <a name="foot-Fri-May-24-141409EDT-2013"></a>[|back|](#note-Fri-May-24-141409EDT-2013) - Or ["names"](http://python.net/~goodger/projects/pycon/2007/idiomatic/handout.html#python-has-names), since Python insists on renaming things.
