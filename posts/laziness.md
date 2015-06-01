I've gotten some questions about what, exactly, [laziness](http://en.wikipedia.org/wiki/Lazy_evaluation) is good for, so I'll want to touch on it briefly.

Short answer: **it saves you space and time**.

## <a name="space"></a>Space

Really, this should be obvious, but if we're going through this, lets do it properly. Here are some lists

```lisp
(defparameter foo (list 1 2 3 4 5 6 7 8 9))
```

```python
foo = [1, 2, 3, 4, 5, 6, 7, 8, 9] ## This one's from Python
```

```haskell
foo = [1, 2, 3, 4, 5, 6, 7, 8, 9] -- This one's from Haskell
```

```javascript
foo = [1, 2, 3, 4, 5, 6, 7, 8, 9] // This one's from Javascript
```

and here's some lazy lists

```lisp
(defun lazy-numbers (&optional (starting-with 1))
  (cons starting-with (lambda () (lazy-numbers (+ starting-with 1)))))

(defparameter foo (lazy-numbers))
```

```python
def lazyNumbers(startingWith=1):
    i = startingWith
    while True:
        yield i
        i += 1

foo = lazyNumbers()
```

```haskell
foo = [1..]
```

```javascript
function lazyNumbers (startingWith) {
    if (startingWith === undefined) startingWith = 1;
    return { num: startingWith,
             next: function () {
                 this.num += 1;
                 return this.num
             }
           }
}

foo = lazyNumbers();
```

Granted the second set looks more complicated, except for the Haskell line, but it has some advantages. Firstly, while the first bunch of lists is bounded, this second bunch is infinite, and you do sometimes want to express that. More to the point though, the reason these can *be* infinite is that they're lazy. They only compute as much of the remainder of the sequence as you actually ask for, which means they save you space in two ways


-   they don't keep the entire list in memory by default; they deal with only one element at a time and any used ones are garbage collected unless you decide to keep a pointer to them yourself
-   they never bother computing parts of the list you don't call for, so they don't waste space storing values that'll never actually get used somewhere


That's basic stuff, it should be pretty obvious. Less obvious, but more significant, is how this saves you time.

## <a name="time"></a>Time

By its lonesome, it actually doesn't.

```lisp
(loop with lst = (list 1 2 3 4 5 6 7 8 9)
   repeat 5 for n in lst
   do (format t "~a~%" n))
```

takes exactly the same amount of run time as

```lisp
(loop with lst = (lazy-numbers 1) repeat 5
     repeat 5 for n = (car lst) for lst = (funcall (cdr lst))
     do (format t "~a~%" n))
```

But what about when we compose multiple operations on one sequence? You'll recall that back when I sat down to [optimize Life](http://langnostic.blogspot.ca/2012/12/life-extreme-optimizing-for-time-in.html) for [a couple of days](http://langnostic.blogspot.ca/2012/12/not-optimizing-haskell.html), I made a big deal of reducing the number of iterations of our corpus. Specifically, recall that the Haskell version of [Gridless Life](http://langnostic.blogspot.ca/2012/12/life-common-lisp-haskell-and-clojure.html) started out like this:

```haskell
import Data.List

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x+dx, y+dy) | dx &lt;- [-1..1], dy &lt;- [-1..1], (dx,dy) /= (0,0)]

lifeStep :: [(Int, Int)] -> [(Int, Int)]
lifeStep cells = [head g | g &lt;- grouped cells, viable g]
  where grouped = group . sort . concat . map neighbors
        viable [_,_,_] = True
        viable [c,_] = c `elem` cells
        viable _ = False
```

and zoom in on one line in particular.

```haskell
...
  where grouped = group . sort . concat . map neighbors
        ...
```

*That right there* is what laziness trivially enables. If you think about what's being done here, you'll realize that you'd never do this in an eager context. Because if you did, what you'd get is


1.   the argument would get evaluated
1.   it would get traversed once by `map neighbors`
1.   and then it would get traversed again by `concat`
1.   and then it would get traversed again by `sort`<a name="note-Mon-Apr-29-130749EDT-2013"></a>[|1|](#foot-Mon-Apr-29-130749EDT-2013)
1.   and then it would get traversed again by `group`


which means six total trips over the entire list. That's bad because every element you touch adds to your run time, *and* each element you have to touch again as part of the computation is one that you can't throw out of memory. On the other hand, expressing a computation by composing smaller, easy computations is very straightforward<a name="note-Mon-Apr-29-130806EDT-2013"></a>[|2|](#foot-Mon-Apr-29-130806EDT-2013). This is a place where the code you want to *run*, and the code you want to *write* are completely different.

What you want to run is a tight loop that walks over the entire corpus *once*, and applies all of the chained functions at once per element. What you want to write is the naive composition of those chained functions, because you've otherwise created an algorithm that will only ever be useful for your particular computation, and that computation will be burdened with the specifics of iteration which will make it non-trivial at best and [impenetrable](https://github.com/slyrus/cl-typesetting/blob/master/tables.lisp#L168-L277) at worst.

Now, granted, Common Lisp has other ways of dealing of dealing with this<a name="note-Mon-Apr-29-131658EDT-2013"></a>[|3|](#foot-Mon-Apr-29-131658EDT-2013), but laziness is one general solution. If each of those functions were lazy (as, in fact, they are in Haskell), what you'd get instead is exactly what you want. One, tight loop running over the entire corpus, applying only as much `group`ing, `sort`ing, `concat`ing and `neighbors`ing as it needed to for the next part of the computation. That saves you a few trips over the input with no additional effort on your part. Talk about low-hanging fruit.

I'll be honest, I was *also* going to talk about my latest thoughts on the square dissection problem, but this ended up being longer than I'd like as it is. It'll probably happen next time, I guess. Probably. Stay tuned if you're into that sort of thing.


* * *
##### Footnotes

1 - <a name="foot-Mon-Apr-29-130749EDT-2013"></a>[|back|](#note-Mon-Apr-29-130749EDT-2013) - A note on functional, lazy sorts, because I was wondering about this both back when tuning the Haskell version of Life and as I was re-visiting it for today's feature. The way that lazy sorts seem to work is basically by using a destructured [heapsort](http://en.wikipedia.org/wiki/Heapsort). Specifically, if you take a look at [this pseudocode](http://en.wikipedia.org/wiki/Heapsort#Pseudocode), what's happening is that a lazy sort runs `heapify` right away and passes up the first element, then pulls out the next element each time it's asked for one. That results in `On` performance for finding the first element (which as far as I'm aware is what you'd have to do in order to get the "nextest" element in the general case anyway), followed by `O(log n)` performance on looking up each next element. That's good because it means you don't have to do it all at once, and it means that if you only want the first 5 sorted elements of a list of 10000, you get to avoid most of the work. On the other hand, note that this *doesn't* save you much memory, if any; you still need to store that heap for the whole list, even if you only want the first few chunklets.

2 - <a name="foot-Mon-Apr-29-130806EDT-2013"></a>[|back|](#note-Mon-Apr-29-130806EDT-2013) - Which is why I did this for the first pass of that program, not realizing that Haskell's lazy-by-default outlook would *also* make it about as efficient as it could be.

3 - <a name="foot-Mon-Apr-29-131658EDT-2013"></a>[|back|](#note-Mon-Apr-29-131658EDT-2013) - In fact, the situation of "I want to write the pretty code, but run the ugly, optimized code" should send any Lisp veterans off thinking about how you'd `defmacro` your way out of this particular annoyance.
