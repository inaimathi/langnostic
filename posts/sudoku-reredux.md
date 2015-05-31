Ok, *this* is why I'm less than proud of [that actually, factually working solution](http://langnostic.blogspot.ca/2013/06/sudoku-redux.html).

```haskell
import List
 
main = putStr . unlines . map disp . solve . return . input =&lt;&lt; getContents
 
solve s = foldr (\p l -> [mark (p,n) s | s &lt;- l, n &lt;- s p]) s idx
 
mark (p@(i,j),n) s q@(x,y)
    | p == q                             = [n]
    | x == i || y == j || e x i && e y j = delete n (s q)
    | otherwise                          = s q
    where e a b = div (a-1) 3 == div (b-1) 3
 
disp s = unlines [unwords [show $ head $ s (i,j) | j &lt;- [1..9]] | i &lt;- [1..9]]
 
input s = foldr mark (const [1..9]) $
  [(p,n) | (p,n) &lt;- zip idx $ map read $ lines s >>= words, n>0]
 
idx = [(i,j) | i &lt;- [1..9], j &lt;- [1..9]]
```

Except, as I mentioned, that one cheats by omitting the type signatures<a name="note-Sun-Jun-02-232557EDT-2013"></a>[|1|](#foot-Sun-Jun-02-232557EDT-2013), so here's the original on which it was based:

```haskell
import Data.List

type T = (Int,Int) -> [Int]

main = do
  s &lt;- getContents
  putStr $ unlines $ map disp $ solve [input s]

solve :: [T] -> [T]
solve s = foldr search s idx where
    search p l = [mark (p,n) s | s &lt;- l, n &lt;- s p]

mark :: ((Int,Int),Int) -> T -> T
mark (p@(i,j),n) s q@(x,y) =
  if p==q then [n] else
  if x==i || y==j || e x i && e y j then delete n $ s q else s q
  where e a b = div (a-1) 3==div (b-1) 3

disp :: T -> String
disp s  = unlines [unwords [show $ head $ s (i,j) | j &lt;- [1..9]] | i &lt;- [1..9]]

input :: String -> T
input s = foldr mark (const [1..9]) $
  [(p,n) | (p,n) &lt;- zip idx $ map read $ lines s >>= words, n>0]

idx :: [(Int,Int)]
idx = [(i,j) | i &lt;- [1..9], j &lt;- [1..9]]
```

This is not the most readable code ever; its goal is supreme elegance<a name="note-Sun-Jun-02-232606EDT-2013"></a>[|2|](#foot-Sun-Jun-02-232606EDT-2013), not instant clarity. It took me a couple of days thinking on-and-off, as well as a read-through of this almost equivalent [Python transliteration](http://www.thenewsh.com/~newsham/x/machine/sud.py)<a name="note-Sun-Jun-02-232612EDT-2013"></a>[|3|](#foot-Sun-Jun-02-232612EDT-2013) to finally understand what the hell is going on here.

Lets get the obvious out of the way.

```haskell
disp :: T -> String
disp s  = unlines [unwords [show $ head $ s (i,j) | j &lt;- [1..9]] | i &lt;- [1..9]]
```

This takes a board (whose type is named `T` for some reason), and returns its string representation.

```haskell
input :: String -> T
input s = foldr mark (const [1..9]) $
  [(p,n) | (p,n) &lt;- zip idx $ map read $ lines s >>= words, n>0]
```

This takes a string representation and returns a board.

```haskell
idx :: [(Int,Int)]
idx = [(i,j) | i &lt;- [1..9], j &lt;- [1..9]]
```

This returns all the `(y, x)` coordinates in a 9x9 board.

```haskell
main = do
  s &lt;- getContents
  putStr $ unlines $ map disp $ solve [input s]
```

This takes from standard in, tries to interpret the result as a board, solve it and print it.

```haskell
type T = (Int,Int) -> [Int]
```

And finally, this is how a board is represented; it's a function of one argument, an `Int, Int` tuple, and returns a list of possible values, a `[Int]`.

Before we go any further, there are a lot of naming conventions here that are aimed at terseness rather than comprehensibility of the resulting code. So lets just do a naive renaming for now.

```haskell
import Data.List

type Board = (Int,Int) -> [Int]

main = do
  boardString &lt;- getContents
  putStr . unlines . map disp $ solve [input boardString]

solve :: [Board] -> [Board]
solve boards = foldr search boards idx where
    search (x, y) boards = [mark ((x, y),val) brd | brd &lt;- boards, val &lt;- brd (x, y)]

mark :: ((Int,Int),Int) -> Board -> Board
mark (p@(x,y),val) board p'@(x',y') = 
  if p==p' then [val] else 
    if x==x' || y==y' || blockBound x x' && blockBound y y' then delete val $ board p' else board p'
  where blockBound a b = div (a-1) 3==div (b-1) 3

disp :: Board -> String
disp board = unlines [unwords [show . head $ board (x,y) | y &lt;- [1..9]] | x &lt;- [1..9]]

input :: String -> Board
input boardString = foldr mark (const [1..9]) $
  [((x, y),val) | ((x, y),val) &lt;- zip idx . map read $ lines boardString >>= words, val>0]

idx :: [(Int,Int)]
idx = [(x,y) | y &lt;- [1..9], x &lt;- [1..9]]
```

Granted, we can no longer claim "707 bytes", but even this minor renaming makes the end result a bit more understandable. On to the difficult parts.

```haskell
mark :: ((Int,Int),Int) -> Board -> Board
mark (p@(x,y),val) board p'@(x',y') = 
  if p==p' then [val] else 
    if x==x' || y==y' || blockBound x x' && blockBound y y' then delete val $ board p' else board p'
  where blockBound a b = div (a-1) 3==div (b-1) 3

input :: String -> Board
input boardString = foldr mark (const [1..9]) $
  [((x, y),val) | ((x, y),val) &lt;- zip idx . map read $ lines boardString >>= words, val>0]

solve :: [Board] -> [Board]
solve boards = foldr search boards idx where
  search (x, y) boards = [mark ((x, y),val) brd | brd &lt;- boards, val &lt;- brd (x, y)]
```

The high level of what's going on here is that you're representing a board as a function of `(Int, Int) -> [Int]`, and `mark`ing spaces by wrapping that function up in a dispatch/delete which returns pruned results in some circumstances.

```haskell
mark :: ((Int,Int),Int) -> Board -> Board
mark (p@(x,y),val) board p'@(x',y') = 
  if p==p' then [val] else 
    if x==x' || y==y' || blockBound x x' && blockBound y y' then delete val $ board p' else board p'
  where blockBound a b = div (a-1) 3==div (b-1) 3
```

This function uses some uncommon notation, and isn't really structured the way you'd expect in a Haskell program. That initial 12-line solution actually does a marginally better job of it. Here's a slightly revised, but equivalent version<a name="note-Sun-Jun-02-232722EDT-2013"></a>[|7|](#foot-Sun-Jun-02-232722EDT-2013)

```haskell
mark :: ((Int,Int),Int) -> Board -> Board
mark (p@(x,y),val) board p'@(x',y') 
  | p == p' = 
    [val]
  | x==x' || y==y' || blockBound x x' && blockBound y y' = 
    delete val $ board p'
  | otherwise =
    board p'
  where blockBound a b = div (a-1) 3==div (b-1) 3
```

That uses the more common [guard statements](http://learnyouahaskell.com/syntax-in-functions#guards-guards) rather than a cascaded `if`/`then`/`else`. The input line and type signature on this one is what threw me for the longest time, so I'm going to linger there for a moment.

```haskell
mark :: ((Int,Int),Int) -> Board -> Board
mark (p@(x,y),val) board p'@(x',y') 
```

Remember, our `Board` is defined as `((Int, Int) -> [Int])`, so that type signature could also be written 

```haskell
mark :: ((Int,Int),Int) -> (Int, Int) -> [Int] -> (Int, Int) -> [Int]
```

which should ironically clarify things. The actual arguments aren't doing anyone any favors either. The `@`s there are applying labels to some destructured constructs. The end result is that you can use the name `p` instead of `(x, y)` and `p'` instead of `(x', y')`. The following code is equivalent, but very slightly longer<a name="note-Sun-Jun-02-232734EDT-2013"></a>[|8|](#foot-Sun-Jun-02-232734EDT-2013):

```haskell
mark ((x,y),val) board (x',y') 
  | (x, y) == (x', y') = 
    [val]
  | x==x' || y==y' || blockBound x x' && blockBound y y' = 
    delete val $ board (x', y')
  | otherwise =
    board (x', y')
  where blockBound a b = div (a-1) 3==div (b-1) 3
```

Right, so that's how you `mark` a value. Except it doesn't really make sense in isolation. Not until we take a look at, at minimum, `input`

```haskell
input :: String -> Board
input boardString = foldr mark (const [1..9]) $
  [((x, y),val) | ((x, y),val) &lt;- zip idx . map read $ lines boardString >>= words, val>0]
```

This is the function that takes a board string and returns an actual board constructed from it. The main operation there is `foldr`, and I'm going to assume you understand how a fold works for this exercise. If you don't, read [this](http://stackoverflow.com/questions/1757740/how-foldr-works) and [this](http://stackoverflow.com/questions/384797/implications-of-foldr-vs-foldl-or-foldl), then do some googling. `(const [1..9])` is a function that always returns the list of integers from `1` to `9`, inclusive. It's equivalent to `(\_ -> [1,2,3,4,5,6,7,8,9])`<a name="note-Sun-Jun-02-232746EDT-2013"></a>[|9|](#foot-Sun-Jun-02-232746EDT-2013). What it produces... is a bit trickier. It has to do with an inherent property of Haskell, and that type signature for `mark` I showed earlier.

```haskell
mark :: ((Int,Int),Int) -> (Int, Int) -> [Int] -> (Int, Int) -> [Int]
```

First off, Haskell partially applies everything by default. Meaning that if you pass fewer than 4 arguments to `mark`, what you actually get back is a function that takes the next argument, and returns either the next partial or the final result. If you take a look at `foldr`, its type is

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

which means that it'll be treating `mark` as a function of two arguments. Note that the second argument is itself a function. Specifically, a `(Int, Int) -> [Int]`, which means that `mark` will be getting three of its arguments filled. It might be easier to think about it like this

```haskell
mark :: ((Int,Int),Int) -> ((Int, Int) -> [Int]) -> (Int, Int) -> [Int]
```

but since *every* function in Haskell can be applied partially, those are equivalent types. The end result of that fold operation is another function of `(Int, Int) -> [Int]`. Lets take a real close look at what's going on there.

This is the empty board

```
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
```

Because it only contains zeros, it'll be represented as `(const [1..9])`. Of course, it also has to be encoded as

```
"0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0"
```

but that first one is easier to read.

```haskell
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :load "/home/inaimathi/projects/code-retreat/sudoku/sudoku-elegant.hs"
[1 of 1] Compiling Main             ( /home/inaimathi/projects/code-retreat/sudoku/sudoku-elegant.hs, interpreted )
Ok, modules loaded: Main.
*Main> let b = input "0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0"
*Main> b (1, 2)
[1,2,3,4,5,6,7,8,9]
*Main> b (1, 3)
[1,2,3,4,5,6,7,8,9]
*Main> b (6, 3)
[1,2,3,4,5,6,7,8,9]
*Main> 
```

Now, adding a value makes sure it recurs once.

```haskell
*Main> let b2 = input "4 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0"
*Main> b2 (1, 1)
[4]
*Main> b2 (1, 2)
[1,2,3,5,6,7,8,9]
*Main> b2 (2, 1)
[1,2,3,5,6,7,8,9]
*Main> b2 (2, 3)
[1,2,3,5,6,7,8,9]
*Main> b2 (5, 5)
[1,2,3,4,5,6,7,8,9]
*Main> 
```

That's the key to understanding this. Lets do the [Little Schemer](http://mitpress.mit.edu/books/little-schemer) thing, and break `input` down. Not necessarily the way GHC does it, but so that we can conceptually understand what's happening here.

```haskell
00}} input "4 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0"

01}} foldr mark (const [1..9]) $ 
                [((x, y),val) | 
                 ((x, y),val) &lt;- zip idx . 
                                 map read $ 
                                     lines boardString 
                                     >>= words, 
                                     val>0]

02}} foldr mark (const [1..9]) $ 
                [((x, y),val) | 
                 ((x, y),val) &lt;- zip idx . 
                                 map read $ 
                                     ["4 0 0 0 0 0 0 0 0",
                                      "0 0 0 0 0 0 0 0 0",
                                      "0 0 0 0 0 0 0 0 0",
                                      "0 0 0 0 0 0 0 0 0",
                                      "0 0 0 0 0 0 0 0 0",
                                      "0 0 0 0 0 0 0 0 0",
                                      "0 0 0 0 0 0 0 0 0",
                                      "0 0 0 0 0 0 0 0 0",
                                      "0 0 0 0 0 0 0 0 0"]
                                     >>= words, 
                                     val>0]

03}} foldr mark (const [1..9]) $ [((1, 1), 4)]

04}} foldr (\((x,y),val) board (x',y') 
             | (x, y) == (x', y') = 
               [val]
             | x==x' || y==y' || blockBound x x' && blockBound y y' = 
               delete val $ board (x', y')
             | otherwise =
               board (x', y')
             where blockBound a b = div (a-1) 3==div (b-1) 3)
           (const [1..9]) $
           [((1, 1), 4)]

05}} (\board (x',y') 
        | (1, 1) == (x', y') = 
          [4]
        | 1==x' || 1==y' || blockBound 1 x' && blockBound 1 y' = 
          delete 4 $ board (x', y')
        | otherwise =
          board (x', y')
        where blockBound a b = div (a-1) 3==div (b-1) 3) (const [1..9])

06}} (\(x',y') 
        | (1, 1) == (x', y') = 
          [4]
        | 1==x' || 1==y' || blockBound 1 x' && blockBound 1 y' = 
          delete 4 $ (const [1..9]) (x', y')
        | otherwise =
          (const [1..9]) (x', y')
        where blockBound a b = div (a-1) 3==div (b-1) 3)
```

And there. If we added another space, it would unfold another level, with the entire step `06}}` there being slotted in instead of `(const [1..9])`. Ok, last bit.

```haskell
solve :: [Board] -> [Board]
solve boards = foldr search boards idx where
  search (x, y) boards = [mark ((x, y),val) brd | brd &lt;- boards, val &lt;- brd (x, y)]
```

Hopefully, now that I've un`foldr`d the definition of `input`, this is intuitively obvious. `search` is an internal function that takes a list of boards and a space `(x, y)`, and attempts to solve for them. It does this by taking each possibility for that space on each board and `mark`ing them, collecting all the results. If you look carefully, and have read those `foldr` links from earlier, this also explains why the Haskell version starts returning answers very quickly. The way iteration unfolds here, the first board is going to be solved quite a while before the complete sequence is solved, which means it'll be returned and printed quickly and thereafter not take further resources from the program.

The [page "explaining" this code](http://web.math.unifi.it/users/maggesi/haskell_sudoku_solver.html) claims that it's "neither fast nor clever", and the Python version states that it's "Not the ideal way to solve Sudoku", but I'm honestly having a hard time imagining one that would give you any kind of gain, either in terms of performance or elegance<a name="note-Sun-Jun-02-232819EDT-2013"></a>[|10|](#foot-Sun-Jun-02-232819EDT-2013).

Possibly the most interesting thing about this solution for me is that, since it generates a list of all possible boards given a solution, you write a generator fairly simply<a name="note-Sun-Jun-02-232823EDT-2013"></a>[|11|](#foot-Sun-Jun-02-232823EDT-2013) using something along the lines of `choice $ solve [input . take 161 $ cycle "0 "]`, then redacting the results to a desired difficulty level. That might be another thing for me to throw some time at.


* * *
##### Footnotes
1 - <a name="foot-Sun-Jun-02-232557EDT-2013"></a>[|back|](#note-Sun-Jun-02-232557EDT-2013) -Which, judging by the responses I get whenever I ask for comments on my Haskell code, is somewhere between grossly impolite and crime-against-[humanatee](http://i.imgur.com/YfUwh.jpg).

2 - <a name="foot-Sun-Jun-02-232606EDT-2013"></a>[|back|](#note-Sun-Jun-02-232606EDT-2013) -Which it hits, in my opinion.

3 - <a name="foot-Sun-Jun-02-232612EDT-2013"></a>[|back|](#note-Sun-Jun-02-232612EDT-2013) -Python doesn't have the same approach to partial functions that Haskell does, so the transliteration is both slightly easier to understand *and* slightly clunkier.<a name="note-Sun-Jun-02-232619EDT-2013"></a>[|4|](#foot-Sun-Jun-02-232619EDT-2013) It also uses `[foldl](http://foldl.com/)` instead of `[foldr](http://foldr.com/)`, because Python only comes with an implementation of `foldl`. Something tells me this kneecaps the `.py` versions' performance. Testing it out on the sample data listed at the bottom of [this page](http://web.math.unifi.it/users/maggesi/haskell_sudoku_solver.html), after manually sanitizing for spaces, seems to confirm that suspicion. On my machine, it spun up to 100% usage on one core until it occupied all of my memory, then sat there paging until I `kill`ed it. The Haskell solution, by contrast, starts producing results very close to instantly, puts all 4 cores to good use, and utterly fails to mem-rape my laptop before computing all possible solutions, which it does well before the Python version produces any solutions<a name="note-Sun-Jun-02-232625EDT-2013"></a>[|5|](#foot-Sun-Jun-02-232625EDT-2013).

4 - <a name="foot-Sun-Jun-02-232619EDT-2013"></a>[|back|](#note-Sun-Jun-02-232619EDT-2013) -That's Python for you far as I can tell, in case you were wondering. It could almost be their slogan. "Python: Easier to understand and fatter than Haskell.".

5 - <a name="foot-Sun-Jun-02-232625EDT-2013"></a>[|back|](#note-Sun-Jun-02-232625EDT-2013) -So I guess that slogan should really be "Python: Easier to understand, fatter and much slower than Haskell."<a name="note-Sun-Jun-02-232633EDT-2013"></a>[|6|](#foot-Sun-Jun-02-232633EDT-2013).

6 - <a name="foot-Sun-Jun-02-232633EDT-2013"></a>[|back|](#note-Sun-Jun-02-232633EDT-2013) -Ok, that isn't entirely fair; this example wasn't optimized in any sense of the word. It uses list comprehensions instead of generators, and could probably implement a lazy `foldr` equivalent to start returning results right away. I'll put a bit of time into that later.

7 - <a name="foot-Sun-Jun-02-232722EDT-2013"></a>[|back|](#note-Sun-Jun-02-232722EDT-2013) -As an aside here, it's mildly frustrating that every single gain in clarity in this exercise adds lines to the final count. I wish there was a way of being clearer *while* being more succinct.

8 - <a name="foot-Sun-Jun-02-232734EDT-2013"></a>[|back|](#note-Sun-Jun-02-232734EDT-2013) -which I get the feeling is why the author chose to use the `@`s.

9 - <a name="foot-Sun-Jun-02-232746EDT-2013"></a>[|back|](#note-Sun-Jun-02-232746EDT-2013) -though in this particular case, it'll be treated as `(\(_, _) -> [1,2,3,4,5,6,7,8,9]) :: ((Int, Int) -> [Int])` because of how `Board` is defined.

10 - <a name="foot-Sun-Jun-02-232819EDT-2013"></a>[|back|](#note-Sun-Jun-02-232819EDT-2013) -Though, obviously, I think clarity could be somewhat improved.

11 - <a name="foot-Sun-Jun-02-232823EDT-2013"></a>[|back|](#note-Sun-Jun-02-232823EDT-2013) -I was going to say "Trivially", and then promptly lost 40 minutes to trying to figure out why exactly it is that an `RVar [Int]` can't be `show`n by default, *or* have its contents `putStr`d no matter how much `liftM` was applied. "Easier to understand, but fatter and slower than Haskell", also happens to be why I've been using Python at work. Haskell makes certain, very well understood things supremely easy, but as soon as I sit down to do something like output or randomness that's trivial in other languages, I find myself suffering a few hours of headaches before figuring it out. I also happen to agree [with Sussman about the core point](http://vimeo.com/12060509); implementing things that are very well understood is not the primary goal of programming.
