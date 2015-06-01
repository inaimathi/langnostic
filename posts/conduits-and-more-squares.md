Ok, so I've been coding something for the past little while in between [thinking about squares](http://oeis.org/A045846) and reading up on [concurrency in haskell](http://www.haskell.org/haskellwiki/Haskell_for_multicores). I'm not going to tell you what it is, because we've established [my track record](http://langnostic.blogspot.ca/2012/08/indirect-reflections.html), but I did want to note that it looks like it's going to rely on [Conduits](http://www.tfeb.org/lisp/hax.html#CONDUITS). A minimal module that provides some interesting additional options for `defpackage`.

It doesn't have a `[quicklisp](http://www.quicklisp.org/beta/)` installation package yet, so I've written a basic `.asd` and chucked it up onto [my github](https://github.com/Inaimathi/conduits) in the meantime.

### <a name="more-square-thoughts"></a>More Square Thoughts

I haven't been actively working on this, just sort of letting it percolate at the back of my mind while other stuff is going on. Having taken a closer look at [the original solution for 1..4](http://oeis.org/A224239/a224239_4.jpg), and imagining how its author went about getting it, it looks like we might be able to cut a lot of placements out of the process if we picked a representation that was easy to `reflect` and `rotate`. Then the actual solution would look something like

```lisp
(defclass grid ...)

(defmethod rotate ((grid grid) squares direction) ...)
(defmethod reflect ((grid grid) squares axis) ...)

(defmethod collect-rotations ((grid grid) squares)
  (insert-dissection grid squares)
  (loop repeat 3 (insert-dissection grid (rotate grid squares :cw)))
  (rotate grid squares :cw))

(defmethod collect-reflection ((grid grid) squares)
  (loop for axis in '(:x :y :xy :yx)
     do (collect-rotations (reflect grid squares axis))))

(defmethod collect-dissections ((grid grid) squares)
  (collect-rotations grid squares)
  (collect-reflections grid squares))

(loop for sq in (starting-positions a-grid)
   do (loop until (full? g) collect (place-next g) into g
         do (collect-dissections a-grid g))
   finally (return (dissection-count grid)))
```

Assuming this works, we could easily write a solution to the "hard" version too; just call `insert-dissection` directly rather than going through `collect-dissection` and friends.

I'm carefully refraining from defining `dissection-count`, `insert-dissection`, `grid`, and indeed `rotate` and `reflect`. What I know is


-   `insert-dissection` is going to have to do the work of ensuring that duplicate dissections are discarded, which implies storing them as a set or hash
-   the definitions of `rotate` and `reflect` will depend heavily on the definition of `grid`, and square storage needs to be thought out to make sure they're all fast
-   it seems like there should be a better way to solve this problem than "brute force", but I'm not seeing it yet. I've joked about it before, but this may actually be the problem that gets me off my ass and into seriously learning about [genetic algorithms](http://www.obitko.com/tutorials/genetic-algorithms/index.php)


That's that for now. I was going to talk a bit about how work has been going, and the shape of small-scale development in Toronto's medical industry, but it looks like a client has finally decided to respond to me, so back to the grindstone I go.
