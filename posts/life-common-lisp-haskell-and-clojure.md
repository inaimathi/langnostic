I should be working now, but fuck it, someone put an idea in my head about the [Global Day of Coderetreat: Toronto](https://guestlistapp.com/events/130467)<a name="note-Thu-Dec-06-114334EST-2012"></a>[|1|](#foot-Thu-Dec-06-114334EST-2012), so I've been thinking, coding and doing some light research. Dann, if you're reading, this article is about 80% your fault.

The standard game of Life is played on a grid of squares. Each square may be alive or dead, and the rules of the game go like this<a name="note-Thu-Dec-06-114414EST-2012"></a>[|2|](#foot-Thu-Dec-06-114414EST-2012):


1.   Any live cell with fewer than two live neighbours dies, as if caused by under-population.
1.   Any live cell with two or three live neighbours lives on to the next generation.
1.   Any live cell with more than three live neighbours dies, as if by overcrowding.
1.   Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.


When I initially talked at my wife<a name="note-Thu-Dec-06-114438EST-2012"></a>[|3|](#foot-Thu-Dec-06-114438EST-2012) about this, I made the assumption that the way to model the problem is the obvious one. That is, define a grid, or an array of arrays, make some of them living, and then compute the next step according to the above rules. Because I had never actually written this program before, I reached for Common Lisp. I was going to try to be functional about it, and the first thing you need to do *that* is a function that takes a cell and finds its neighbors.

```lisp
(defun neighbors (cell)
  (let ((l '(-1 0 1)))
    (destructuring-bind (x . y) cell
      (mapcan (lambda (dx) 
                (loop 
                   for dy in l
                   unless (and (zerop dx) (zerop dy))
                   collect (cons (+ x dx) (+ y dy)))) 
              l))))
```

At this point I was still assuming that I'd be representing the board as a 2-dimensional array, so the output of this function was going to be used down the line to index into said array, returning a list of found live `cell`s. Except that as I was thinking about how best to do that<a name="note-Thu-Dec-06-114752EST-2012"></a>[|4|](#foot-Thu-Dec-06-114752EST-2012) it occurred to me that it might be better to model the problem as a [sparse array](http://en.wikipedia.org/wiki/Sparse_array) of living cells. If I were going for that approach, the output of `neighbors` wouldn't be used for indexing at all; instead, using I could use it to get a count of how many neighbors each referred square has. That is,

```lisp
CL-USER> (defparameter *blinker* '((1 . 0) (1 . 1) (1 . 2)))
*BLINKER*
CL-USER> (mapcan #'neighbors *blinker*)
((0 . -1) (0 . 0) (0 . 1) (1 . -1) (1 . 1) (2 . -1) (2 . 0) (2 . 1) (0 . 0)
 (0 . 1) (0 . 2) (1 . 0) (1 . 2) (2 . 0) (2 . 1) (2 . 2) (0 . 1) (0 . 2)
 (0 . 3) (1 . 1) (1 . 3) (2 . 1) (2 . 2) (2 . 3))
CL-USER> 
```

That output tells me that, for example, the `cell` `(2 . 2)` currently has two living neighbors since it's mentioned twice in that list. Common Lisp doesn't have a built-in `group` or similar, so I'm stuck `hash`ing things myself to collect those frequencies.

> EDIT:
> The original way of doing this (still shown below this edit) is inelegant. `gethash` takes an optional third argument, which it returns as a default value. Meaning that if you're looking to use a `hash-table` to keep count of stuff you can express that as

> ```lisp
> (defun cells->cell-count-hash (cells)
>   (let ((h (make-hash-table :test 'equal)))
>     (loop 
>        for c in cells
>        do (incf (gethash c h 0)))
>     h))
> ```
> Sat, 22 Dec, 2012

```lisp
(defun cells->cell-count-hash (cells)
  (let ((h (make-hash-table :test 'equal)))
    (loop 
       for c in cells
       if (gethash c h)
         do (incf (gethash c h))
       else
         do (setf (gethash c h) 1))
    h))
```

You might think that you can bind a temporary variable to `(gethash c h)` to get rid of that duplication. It won't do what you think it should. Feel free to try it and develop a theory of why. Anyhow, over to the REPL

```lisp
CL-USER> (let ((h (cells->cell-count-hash (mapcan #'neighbors *blinker*))))
           (loop for k being the hash-keys of h 
              using (hash-value count)
              do (format t "~a : ~a~%" k count)))
(0 . -1) : 1
(0 . 0) : 2
(0 . 1) : 3
(1 . -1) : 1
(1 . 1) : 2
(2 . -1) : 1
(2 . 0) : 2
(2 . 1) : 3
(0 . 2) : 2
(1 . 0) : 1
(1 . 2) : 1
(2 . 2) : 2
(0 . 3) : 1
(1 . 3) : 1
(2 . 3) : 1
NIL
CL-USER> 
```

That should illustrate it sufficiently for you. As an aside here, note that `hash-keys` is not a function. It's actually a `loop` keyword. And so is `hash-value`. If you try to use either in plain ol' Common Lisp code, you'll get a plain ol' `unbound symbol` error. This is the main reason I don't like to grub around hashes in CL; it's not very pleasant and involves a lot of plumbing built *specifically* for it. `[maphash](http://www.lispworks.com/documentation/HyperSpec/Body/f_maphas.htm)` also exists<a name="note-Thu-Dec-06-114846EST-2012"></a>[|5|](#foot-Thu-Dec-06-114846EST-2012), but counter-intuitively<a name="note-Thu-Dec-06-114856EST-2012"></a>[|6|](#foot-Thu-Dec-06-114856EST-2012) always returns `NIL` rather than a sequence, relying on side-effect to actually do things. That's ... less than satisfying.

Anyhow, moving on. We've got a pretty simplistic model of the world here, but it's enough to let me step through `life`.

```lisp
(defun life-step (cells)
  (let ((freqs (cells->cell-count-hash (mapcan #'neighbors cells))))
    (loop for k being the hash-keys of freqs
       using (hash-value count)
       when (or (= 3 count) (and (= 2 count) (member k cells :test #'cell=)))
         collect k)))
```

I also defined `cell=` just to help that `member` call along<a name="note-Thu-Dec-06-115423EST-2012"></a>[|7|](#foot-Thu-Dec-06-115423EST-2012).

```lisp
(defun cell= (a b)
  (and (= (car a) (car b)) (= (cdr a) (cdr b))))
```

That conditional after the `when` clause is a contraction of the Life rules, but the effect is the same. The single rule is, effectively


1.   If a cell has three neighbors, or has two neighbors and is alive, it's alive in the next step.


Because of the way we're representing the "board", that suffices. So this produced the expected output for the given `blinker` input, and I didn't particularly feel the need to write a fancy grid-display for it, so I moved on to Haskell.

Same theory; simplified rules and represent the world as a sparse array of living cells; how would it look?

```haskell
neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = filter (/=(x,y)) . concat $ map xs l
  where l = [-1..1]
        xs dy = map (\dx -> (x + dx, y + dy)) l
```

> EDIT:
>
> And **then**, I remembered list comprehensions, which turn `neighbors` into a one-liner.
> 
> ```haskell
> neighbors (x, y) = [(x+dx, y+dy) | dx &lt;- [-1..1], dy &lt;- [-1..1], (dx,dy) /= (0,0)]
> ```
> 
> Thu, 06 Dec, 2012

Neighbors looks a bit different. As far as I know, there isn't a `loop` analogue available in Haskell, so I'm "stuck" composing the functional primitives. It ends up saving me about four lines of code, and that's typically the case when I transform a destructive function into a functional one, so I want to make it clear that that wasn't a complaint. Before we move on, let me just spotlight something

```haskell
(/=(x,y))
```

I pointed out a similar construct at a [Toronto Lisp Group](http://www.lisptoronto.org/) meeting a little while ago and didn't emphasize this enough. That bracketed expression is *"the function of one argument that compares its argument to the tuple of `x` and `y`"*. Please note that this *isn't* a compiler macro, or similar trickery. Haskell curries<a name="note-Thu-Dec-06-115647EST-2012"></a>[|8|](#foot-Thu-Dec-06-115647EST-2012) by default, and `/=` is the inequality operator. So that tiny parenthesized expression is literally currying the tuple `(x, y)` onto the right argument of the infix function `/=`. You can generalize this to any other function.

Moving on.

```haskell
lifeStep :: [(Int, Int)] -> [(Int, Int)]
lifeStep cells = step cells
  where step = map head . filter viable . group . sort . concat . map neighbors
        viable [_,_,_] = True
        viable [c,_] = c `elem` cells
        viable _ = False
```

> EDIT:

> List comprehensions strike again, but don't buy quite as much clarity this time.

> ```haskell
> lifeStep :: [(Int, Int)] -> [(Int, Int)]
> lifeStep cells = [head g | g &lt;- grouped cells, viable g]
>   where grouped = group . sort . concat . map neighbors
>         viable [_,_,_] = True
>         viable [c,_] = c `elem` cells
>         viable _ = False
> ```

> Thu, 06 Dec, 2012

You can see the simplified life rules again in the `viable` local function. If there are 3 neighbors a cell is viable, if there are 2 neighbors and the cell is already a member of the living then it's viable, otherwise it's not.

```haskell
where step = map head . filter viable . group . sort . concat . map neighbors
```

`step` is written entirely in [point-free style](http://en.wikipedia.org/wiki/Tacit_programming), which might look odd to people coming from a background of purely mainstream languages. This might just be why newbs have a harder time with Haskell than we strictly seem we should. Once you understand it, it's obviously the most elegant and appropriate way of expressing a series of computations. Until you understand it, you can be forgiven for thinking that I'm just making shit up. Lets go through it step by `step`. I mean, step.

First, we `map neighbors` over the input, giving us a list of lists of cells. Then we `concat`enate that, giving us a list of cells, then we `group . sort` that, giving us a list of lists of cells again<a name="note-Thu-Dec-06-115853EST-2012"></a>[|9|](#foot-Thu-Dec-06-115853EST-2012). Finally, we `filter` those lists for `viable` cells and pull a single cell out of each list using `map head`.

Note that, because Haskell is a [lazy](http://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_need) language, the actual program probably won't run in that sequence. Which is a good thing, because we'd otherwise be looking at 6 traversals of our input per `lifeStep`, which is pretty `On^scary`.

Oh, that was it, by the way.

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

I was amazed the first time I wrote it too; those 6-lines-plus-type-signatures-and-import of Haskell do exactly the same thing as my Common Lisp program from earlier. The term is elegance, I think, since I can still understand it, but it's possible to disagree on these points. The next step obvious step was writing the display functions and test output. Which promptly more than doubled the line-count.

> EDIT:

> I could probably use list comprehensions here too, but I don't care enough about the display code to optimize it.
> Thu, 06 Dec, 2012

> EDIT:

> Ok, dammit, *here*

> ```haskell
> showWorld :: [(Int, Int)] -> IO ()
> showWorld cells = mapM_ putStrLn $ map cellStr groupedByY
>   where groupedByY = [[fst c | c &lt;- cells, snd c == y] | y &lt;- range]
>         cellStr xs = [if c `elem` xs then '#' else ' ' | c &lt;- range]
>         range = worldRange cells

> worldRange cells = [least..greatest]
>   where least = min x y
>         greatest = max x' y'
>         (x, y) = head cells
>         (x', y') = last cells
> ```

> Thu, 06 Dec, 2012

```haskell
showWorld :: [(Int, Int)] -> IO ()
showWorld cells = mapM_ putStrLn $ map cellStr $ map byY range
  where byY y = map fst $ filter (\c -> snd c == y) cells
        (x1, y1) = head cells
        (x1', y1') = last cells
        least = min x1 y1
        greatest = max x1' y1'
        range = [least..greatest]
        cellStr xs = map (\c -> if c `elem` xs then '#' else ' ') range

runLife :: Int -> [(Int, Int)] -> IO ()
runLife steps cells = rec (steps - 1) cells
  where rec 0 cells = showWorld cells
        rec s cells = do showWorld cells
                         rec (s - 1) $ lifeStep cells


main :: IO ()
main = do
  putStrLn "Glider >> 10"
  putStrLn "------------"
  runLife 10 [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
  putStrLn ""
  putStrLn "Blinker >> 3"
  putStrLn "------------"
  runLife 3 [(1, 0), (1, 1), (1, 2)]
```

I'm not going to go through this step-by-step; it does the not-very-interesting job of taking a sparse living cell array and outputting the minimum required grid to display it using sharps (for living cells) and spaces (for dead ones). It could probably be made more efficient, or it could be made more scalable by introducing grid limits rather than computing a needed grid size, but it's enough to show the principle.

```
inaimathi@lambda:~/langnostic/play/haskell$ ghc life.hs
[1 of 1] Compiling Main             ( life.hs, life.o )
Linking life ...
inaimathi@lambda:~/langnostic/play/haskell$ ./life
Glider >> 10
------------
 # 
  #
###
   
# #
 ##
    
  # 
# # 
 ## 
#  
 ##
## 
 # 
  #
###
   
# #
 ##
    
  # 
# # 
 ## 
#  
 ##
## 
 # 
  #
###
   
# #
 ##

Blinker >> 3
------------
 # 
 # 
 # 
   
###
   
 # 
 # 
 # 
inaimathi@lambda:~/langnostic/play/haskell$ 
```

At that point, I went on to write the Clojure version, but I won't be showing that. After a good three hours total, I had three programs written up in three languages, all using the same less-than-perfectly-obvious model of the world, and I kind of wanted to figure out whether I was barking up the right tree.

[Life](http://rosettacode.org/wiki/Conway%27s_Game_of_Life) is on the [Rosetta Code](http://rosettacode.org/wiki/Category:Programming_Tasks) task list, so I took a look at the languages I'd covered. The [Haskell version](http://rosettacode.org/wiki/Conway%27s_Game_of_Life#Haskell) uses the grid approach with a multi-indexed `Array`. I sort of understand what it's doing, even if the `life` function has very obviously been written by a mathematician rather than a programmer, and it's not at all the same as what I'm doing.

The [Common Lisp version](http://rosettacode.org/wiki/Conway%27s_Game_of_Life#Common_Lisp) does more or less the same thing, except without the emphasis on one-character variable/function names, so that it's pretty understandable if you're comfortable with Common Lisp. He's doing something similar to the Haskell mathematician though; defining a 2-dimensional array to hold the game board, then manipulating it<a name="note-Thu-Dec-06-122553EST-2012"></a>[|10|](#foot-Thu-Dec-06-122553EST-2012) to produce the next generation of the world.

`Page Down` starts getting some mileage as I look through [implementations in other languages](http://rosettacode.org/wiki/Conway%27s_Game_of_Life), pointedly avoiding Prolog and Forth so that I'll have something interesting to explore with my pairing partner on Saturday. I'm beginning to get more and more worried as I read on; everything from D to TCL is using the grid representation, so I'm beginning to think that there's some obvious shortcoming to the sparse-array approach that I must have missed.

Before I go read up on what that is and how royally it bones performance or whatever, I turn to the [Clojure implementation](http://rosettacode.org/wiki/Conway%27s_Game_of_Life#Clojure), reproduced here in full:

```clojure
(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1]
        :when (not (and (zero? dx) (zero? dy)))]
    [(+ x dx) (+ y dy)]))
 
(defn next-step [cells]
  (set (for [[cell n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells cell)))]
         cell)))
```

That's why I'm not showing the solution I worked up, by the way. I didn't even bother saving it after I read that. It was doing exactly the same thing, in algorithmic terms, but I'm a Clojure n00b. So I didn't know about `for`<a name="note-Thu-Dec-06-122654EST-2012"></a>[|11|](#foot-Thu-Dec-06-122654EST-2012), or `frequencies`<a name="note-Thu-Dec-06-122659EST-2012"></a>[|12|](#foot-Thu-Dec-06-122659EST-2012), and that made it quite a bit longer and thornier. This version *doesn't* pretty-print its grid, but shows the essence of the approach beautifully<a name="note-Thu-Dec-06-122714EST-2012"></a>[|13|](#foot-Thu-Dec-06-122714EST-2012).

Looking through the rest of the languages<a name="note-Thu-Dec-06-122721EST-2012"></a>[|14|](#foot-Thu-Dec-06-122721EST-2012), they all use the obvious grid model too. I actually haven't gone out to read up on the comparison of these two approaches<a name="note-Thu-Dec-06-122733EST-2012"></a>[|15|](#foot-Thu-Dec-06-122733EST-2012) but having at least one crazy bastard other than me on the same train of thought at least tells me there might be something here. Or, this might be one of those places where the traditional representation has just become so well-known and obvious that everyone reaches for it outright even though it might not be the most elegant thing available.

Ok, that's my lunch hour up, I'm heading back to work. Hopefully I get some *actual* hacking done for the next installment and finally wrap up the authentication series. Though, to be fair, I guess it's more likely to be a write-up of my experience on [Saturday](https://guestlistapp.com/events/130467).

> EDIT:

> **Addendum**, because I hate myself. The Python version (minus display code). Woo.

> ```python
> from collections import defaultdict

> glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
> blinker = [(1, 0), (1, 1), (1, 2)]

> def neighbors(cell):
>     x,y = cell
>     r = range(-1,2)
>     return [(x+dx, y+dy) for dx in r for dy in r if not (dx, dy) == (0, 0)]

> def frequencies(cells):
>     res = defaultdict(int)
>     for cell in cells:
>         res[cell] += 1
>     return res
        
> def lifeStep(cells):
>     freqs = frequencies([n for c in cells for n in neighbors(c)])
>     return [k for k in freqs if freqs[k]==3 or (freqs[k]==2 and k in cells)]
> ```

> Thu, 06 Dec, 2012

> EDIT:

> **Addendum the Second:** All code from this article (plus printing code for each language) now available [here](https://github.com/Inaimathi/life).
> Sun, 09 Dec, 2012

* * *
##### Footnotes
1 - <a name="foot-Thu-Dec-06-114334EST-2012"></a>[|back|](#note-Thu-Dec-06-114334EST-2012) - Registration is closed, but it's apparently fine to just show up if you're [in the area](https://guestlistapp.com/events/130467).

2 - <a name="foot-Thu-Dec-06-114414EST-2012"></a>[|back|](#note-Thu-Dec-06-114414EST-2012) - They actually reduce to between one and three simpler rules, depending on how you look at it, but we'll talk about that later.

3 - <a name="foot-Thu-Dec-06-114438EST-2012"></a>[|back|](#note-Thu-Dec-06-114438EST-2012) - She always indulges me in these things, even though her own interest in the problem is minimal at best. I'm thankful for her patience on the multitude of days she's watched me talk to myself.

4 - <a name="foot-Thu-Dec-06-114752EST-2012"></a>[|back|](#note-Thu-Dec-06-114752EST-2012) - I'll admit that I was, perhaps prematurely, also thinking about how I'd do it in Haskell, where indexing into a nonexistent cell is non-trivial and not very idiomatic in any case.

5 - <a name="foot-Thu-Dec-06-114846EST-2012"></a>[|back|](#note-Thu-Dec-06-114846EST-2012) - And to be fair, the situation above could have been written more succinctly as 

```lisp
(maphash 
  (lambda (k v) (format t "~a : ~a~%" k v))
  (cells->cell-count-hash (mapcan #'neighbors *blinker*)))
```


6 - <a name="foot-Thu-Dec-06-114856EST-2012"></a>[|back|](#note-Thu-Dec-06-114856EST-2012) - For a function with `map` in its name.

7 - <a name="foot-Thu-Dec-06-115423EST-2012"></a>[|back|](#note-Thu-Dec-06-115423EST-2012) - This is one of those places where better `destructuring-bind` syntax would help to no end, by the way. If Clojure didn't have a polymorphic `=`, neatly sidestepping the need for a `cell=` at all, I could do

```clojure
(defn cell= [a b]
  (let [[ax ay] a
        [bx by] b]
    (and (= ax bx) (= ay by))))
```

or just

```clojure
(defn cell= [[ax ay] [bx by]]
  (and (= ax bx) (= ay by)))
```

In Common Lisp, using the destructuring version would actually take more code than just looking up `car`s and `cdr`s


8 - <a name="foot-Thu-Dec-06-115647EST-2012"></a>[|back|](#note-Thu-Dec-06-115647EST-2012) - Har har, [Simon](http://en.wikipedia.org/wiki/Simon_Peyton_Jones). That's [Hilarious](http://en.wikipedia.org/wiki/Haskell_Curry).

9 - <a name="foot-Thu-Dec-06-115853EST-2012"></a>[|back|](#note-Thu-Dec-06-115853EST-2012) - But organized by identity rather than by [moore neighborhood](http://en.wikipedia.org/wiki/Moore_neighborhood) center.

10 - <a name="foot-Thu-Dec-06-122553EST-2012"></a>[|back|](#note-Thu-Dec-06-122553EST-2012) - Destructively, unlike in the Haskell version.

11 - <a name="foot-Thu-Dec-06-122654EST-2012"></a>[|back|](#note-Thu-Dec-06-122654EST-2012) - Instead using `map`/`reduce`.

12 - <a name="foot-Thu-Dec-06-122659EST-2012"></a>[|back|](#note-Thu-Dec-06-122659EST-2012) - Instead using the same tactic I used in Haskell, which is a pretty shitty thing to do in a language that *isn't* lazy by default.

13 - <a name="foot-Thu-Dec-06-122714EST-2012"></a>[|back|](#note-Thu-Dec-06-122714EST-2012) - The earlier Haskell display code should be pretty easy to port anyhow, in case you really, *really* must see a grid of life squares *printed* as part of the solution.

14 - <a name="foot-Thu-Dec-06-122721EST-2012"></a>[|back|](#note-Thu-Dec-06-122721EST-2012) - The ones *before* Clojure on [that page](http://rosettacode.org/wiki/Conway%27s_Game_of_Life).

15 - <a name="foot-Thu-Dec-06-122733EST-2012"></a>[|back|](#note-Thu-Dec-06-122733EST-2012) - The [article linked to from Rosetta Code](http://rosettacode.org/wiki/Conway%27s_Game_of_Life#Clojure) isn't very informative.
