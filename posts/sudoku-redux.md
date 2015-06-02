I was in a particularly blah mood today, so I decided to sharpen my teeth on a problem I had half-solved from earlier. Solving Sudoku in Haskell. The code for the solution is up at [the appropriate github](https://github.com/Inaimathi/sudoku/blob/master/sudoku.hs).

### <a name="interlude"></a>Interlude

Before we get to the actual code though, do you remember [that page](http://www.haskell.org/haskellwiki/Sudoku) I linked, chock full of Sudoku solvers written in Haskell? Well, there aren't as many there as I thought. About half the links from that page actually lead to 404 pages of various intricacies instead of to the examples they promise. The ones you can see source for are all there, but that's really all you can guarantee.

Also, I'll have to take it back.


>   The [appropriate Rosetta Code page](http://rosettacode.org/wiki/Sudoku) doesn't have any solutions that leave me gobsmacked by elegance the way that [the Clojure Game of Life](http://rosettacode.org/wiki/Conway%27s_Game_of_Life#Clojure) did.  
>   
> -Inaimathi  


That's false. Specifically, once I sat down to actually read *read* the examples there instead of just flipping through them, I got caught by [one that I passed over the first time](http://web.math.unifi.it/users/maggesi/haskell_sudoku_solver.html). The code actually on the Haskellwiki page is even shorter than that, but it does it by omitting the type declarations, which is borderline cheating in Haskell. It took me an embarrassingly long time to understand the approach in my bones, so I'll go over it in depth in a [follow-up article](http://langnostic.blogspot.ca/2013/06/sudoku-reredux.html) just in case I'm not the only one.

### <a name="sudoku"></a>Sudoku


>   So all [these people](http://www.haskell.org/haskellwiki/Sudoku) are using Haskell to commit sudoku? Oh what a world...  
>   
> -Anonymous  



>   ![](/static/img/oh-you-spj.jpeg)  
>   
> -Inaimathi  


Like I said, we did Sudoku solvers at the last Toronto Code Retreat. The group of three I worked in for the Haskell attempt came up with [this](https://github.com/Inaimathi/sudoku/blob/f3b442ce84b58cbe953817a4a96b2b5bfa4c0e5c/sudoku.hs). And I've since expanded that to a solver that works in the general case, although admittedly, *very slowly*<a name="note-Sun-Jun-02-173100EDT-2013"></a>[|1|](#foot-Sun-Jun-02-173100EDT-2013).

Here's the code

```haskell
module Main where

import Data.Set (Set(..), toList, fromList, difference, member)
import qualified Data.Set as Set
import Data.List (sort, sortBy, intercalate, group, find)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Data.Char (intToDigit)
import Data.Maybe (fromJust)

---------- Class Definition, constructors and sample data
data Board = Board { values :: [[Int]], 
                     empty :: Set (Int, Int),
                     size :: Int, 
                     ixs :: [Int],
                     blockSize :: Int } deriving (Eq)

instance Show Board where
  show board = (:) '\n' $ unlines . intercalate hdelim . split . lns $ values board
    where lns = map (intercalate "|" . split . map sq)
          split = chunksOf bs
          sq n = if n == 0 then ' ' else intToDigit n
          hdelim = [replicate (size board + (bs - 1)) '-']
          bs = blockSize board

sampleSmall = toBoard [[1, 0, 3, 0],
                       [0, 4, 0, 2],
                       [0, 3, 4 ,0],
                       [4, 0, 2, 3]]

sample = toBoard [[0,7,1,4,0,0,0,0,5],
                  [0,0,0,0,5,0,0,8,0],
                  [0,0,3,9,0,7,6,0,0],
                  [0,0,0,0,0,1,0,0,0],
                  [0,9,0,8,0,6,0,0,3],
                  [0,0,0,0,0,0,8,2,0],
                  [0,6,0,0,4,0,7,0,8],
                  [3,0,0,0,0,0,0,9,0],
                  [0,0,0,0,8,5,0,0,0]]

sampleHard = toBoard [[0,7,1,4,0,0,0,0,5],
                      [0,0,0,0,5,0,0,8,0],
                      [0,0,3,9,0,7,6,0,0],
                      [0,0,0,0,0,1,0,0,0],
                      [0,9,0,0,0,6,0,0,3],
                      [0,0,0,0,0,0,8,2,0],
                      [0,0,0,0,4,0,0,0,8],
                      [3,0,0,0,0,0,0,9,0],
                      [0,0,0,0,8,5,0,0,0]]

sampleDevilish = toBoard [[0,7,1,4,0,0,0,0,0],
                          [0,0,0,0,5,0,0,0,0],
                          [0,0,3,9,0,7,6,0,0],
                          [0,0,0,0,0,0,0,0,0],
                          [0,9,0,0,0,6,0,0,3],
                          [0,0,0,0,0,0,0,0,0],
                          [0,0,0,0,4,0,0,0,8],
                          [0,0,0,0,0,0,0,9,0],
                          [0,0,0,0,8,5,0,0,0]]

toBoard :: [[Int]] -> Board
toBoard values = findEmpties $ Board { values = values, empty = fromList [],
                                       size = len, ixs = [0..len - 1], blockSize = bs }
  where bs = fromEnum . sqrt . toEnum $ length values
        len = length values

findEmpties :: Board -> Board
findEmpties board = board { empty = fromList [(x, y) | y &lt;- is, x &lt;- is, blank (x, y)] }
  where blank (x, y) = 0 == ((values board) !! y !! x)
        is = ixs board

---------- The solver
main = putStr . show $ solve sampleDevilish

solve :: Board -> Board
solve board = rec [naiveSolve [obvious, blockwise] board]
  where solved board = 0 == (Set.size $ empty board)
        impossible board = any ((==0) . length) . map (toList . possibilities board) . toList $ empty board
        rec [] = board -- Failed
        rec boards = case find solved $ boards of
          Just b -> b
          Nothing -> rec . map (naiveSolve [obvious, blockwise]) . concatMap guess $ filter (not . impossible) boards

naiveSolve :: [(Board -> Board)] -> Board -> Board
naiveSolve functions board = rec functions board
  where rec [] board = board
        rec fns board = case Set.size $ empty new of
          0 -> new
          _ -> rec nextFns new
          where new = (head fns) $ board
                nextFns = if new == board then tail fns else functions
        
---------- The solve stages
obvious :: Board -> Board
obvious board = findEmpties $ board { values = newVals }
  where newVals = [[newVal (x, y) | x &lt;- ixs board] | y &lt;- ixs board]
        ps x y = toList $ possibilities board (x, y)
        newVal (x, y) = case ((values board) !! y !! x, ps x y) of
          (0, [val]) -> val
          (val, _) -> val

blockwise :: Board -> Board
blockwise board = findEmpties $ board { values = new }
  where new = [[newVal (x, y) | x &lt;- ixs board] | y &lt;- ixs board]
        newVal (x, y) = case find (\(x', y', v) -> (x == x') && (y == y')) uniques of
          Just (_, _, v) -> v
          Nothing -> (values board) !! y !! x
        uniques = concat [uniqueInBlock board (x, y) | y &lt;- bIxs, x &lt;- bIxs]
        bIxs = [0, bs..size board-1]
        bs = blockSize board

guess :: Board -> [Board]
guess board = map (\v -> findEmpties $ board { values = newVals v }) vs
  where (x, y, vs) = head $ sortBy (comparing (length . thd)) posMap
        newVals v = [[if x == x' && y == y' then v else (values board) !! y' !! x' | x' &lt;- ixs board] | y' &lt;- ixs board]
        posMap = [(x, y, toList $ possibilities board (x, y)) | (x, y) &lt;- es]
        es = toList $ empty board


---------- Solver-related utility
possibilities :: Board -> (Int, Int) -> Set Int
possibilities board (x, y) = foldl difference (fromList [1..size board]) sets
  where sets = mapply (board, (x, y)) [row, col, block]

row :: Board -> (Int, Int) -> Set Int
row board (x, y) = fromList $ values board !! y

col :: Board -> (Int, Int) -> Set Int
col board (x, y) = fromList . map (!! x) $ values board

block :: Board -> (Int, Int) -> Set Int
block board (x, y) = fromList . concat . square $ values board
  where square = map (take bs . drop (origin x)) . take bs . drop (origin y)
        origin n = bs * intFloor n bs
        bs = blockSize board

uniqueInBlock :: Board -> (Int, Int) -> [(Int, Int, Int)]
uniqueInBlock board (x, y) = singles $ concatMap (toList . thd) posMap
  where posMap = [(x', y', possibilities board (x', y')) | (x', y') &lt;- es]
        es = blockEmpties board (x, y)
        singles = map (findInMap . head) . filter ((==1) . length) . group . sort
        findInMap n = let (x, y, p) = fromJust $ find (member n . thd) posMap
                      in (x, y, n)

blockEmpties :: Board -> (Int, Int) -> [(Int, Int)]
blockEmpties board (x, y) = [(x', y') | x' &lt;- xs, y' &lt;- ys, blank (x', y')]
  where blank (x, y) = 0 == ((values board) !! y !! x)
        xs = [ox..ox + bs-1]
        ys = [oy..oy + bs-1]
        [ox, oy] = map origin [x, y]
        origin n = bs * intFloor n bs
        bs = blockSize board

---------- General Utility
mapply :: (a, b) -> [(a -> b -> c)] -> [c]
mapply args fns = map (\fn -> uncurry fn $ args) fns

intFloor :: Int -> Int -> Int
intFloor a b = fromEnum . floor . toEnum $ a `div` b

thd :: (a, b, c) -> c
thd (a, b, c) = c
```

Just over 110 lines of pretty ham-fisted Haskell, not counting the example data and general utility functions. At a high level, the way this is supposed to work is by taking a board, repeatedly solving all the obvious spaces, potentially doing a blockwise analysis then repeatedly solving the new obvious spaces, and potentially guessing if neither of those tactics work out. In other words, this is more or less a formalization of the basic brute-force method a human Sudoku beginner might use to solve a board. If we ever get to a solved board, we return it, if we discover we've been given an impossible board, we return the input instead.

First off, we've changed our definition of a board from a naive 2D array to a more complex type that keeps some needed info around...

```haskell
data Board = Board { values :: [[Int]], 
                     empty :: Set (Int, Int),
                     size :: Int, 
                     ixs :: [Int],
                     blockSize :: Int } deriving (Eq)
```

...and we've taken the opportunity to just make it an instance of `Show`.

```haskell
instance Show Board where
  show board = (:) '\n' $ unlines . intercalate hdelim . split . lns $ values board
    where lns = map (intercalate "|" . split . map sq)
          split = chunksOf bs
          sq n = if n == 0 then ' ' else intToDigit n
          hdelim = [replicate (size board + (bs - 1)) '-']
          bs = blockSize board
```

Lets start in the middle this time:

```haskell
obvious :: Board -> Board
obvious board = findEmpties $ board { values = newVals }
  where newVals = [[newVal (x, y) | x &lt;- ixs board] | y &lt;- ixs board]
        ps x y = toList $ possibilities board (x, y)
        newVal (x, y) = case ((values board) !! y !! x, ps x y) of
          (0, [val]) -> val
          (val, _) -> val
```

That's how we solve a board with obvious values in it: just return a new board with the appropriate spaces filled with their only possible value, and removed from the empty space set. Nothing special here. Slightly more interesting is how we go to the next step

```haskell
blockwise :: Board -> Board
blockwise board = findEmpties $ board { values = new }
  where new = [[newVal (x, y) | x &lt;- ixs board] | y &lt;- ixs board]
        newVal (x, y) = case find (\(x', y', v) -> (x == x') && (y == y')) uniques of
          Just (_, _, v) -> v
          Nothing -> (values board) !! y !! x
        uniques = concat [uniqueInBlock board (x, y) | y &lt;- bIxs, x &lt;- bIxs]
        bIxs = [0, bs..size board-1]
        bs = blockSize board
```

Rather than checking for sets that have only one remaining possibility, this checks whether there's a unique position for any value within a `block`. To illustrate:

```haskell
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :load "/home/inaimathi/projects/code-retreat/sudoku/sudoku.hs"
[1 of 1] Compiling Main             ( /home/inaimathi/projects/code-retreat/sudoku/sudoku.hs, interpreted )
Ok, modules loaded: Main.
*Main> sample
Loading package array-0.4.0.0 ... linking ... done.
Loading package deepseq-1.3.0.0 ... linking ... done.
Loading package containers-0.4.2.1 ... linking ... done.
Loading package split-0.2.1.2 ... linking ... done.

 71|4  |  5
   | 5 | 8 
  3|9 7|6  
-----------
   |  1|   
 9 |8 6|  3
   |   |82 
-----------
 6 | 4 |7 8
3  |   | 9 
   | 85|   

*Main> let obv board = if o == board then board else obv o where o = obvious board
*Main> obv sample

 71|4 8| 35
   | 53| 8 
  3|9 7|6  
-----------
   |  1|   
 9 |8 6|  3
   |  4|82 
-----------
 6 | 49|7 8
3  |  2| 9 
   | 85|   

*Main> 
```

This is how far repeatedly solving the obvious blocks gets us. **BUT**, there are still squares there that have unambiguous solutions. Specifically

```haskell
 71|4 8| 35
   | 53| 8X
  3|9 7|6  
-----------
   |  1|   
 9 |8 6|  3
   |  4|82 
-----------
 6 | 49|7 8
3  |  2| 9 
   | 85|X  
```

Those two have only one possible value. If you take a look at their possibilities list, it doesn't look that way

```haskell
*Main> possibilities (obv sample) (8, 1)
fromList [1,2,4,7,9]
*Main> possibilities (obv sample) (6, 8)
fromList [1,2,3,4]
*Main> 
```

but if you take a look at only the intersecting values something becomes clear.

```haskell
 7.|. .| 35
   | ..| 8X
  .|. 7|6  
-----------
   |  .|   
 . |. .|  3
   |  .|.. 
-----------
 . | ..|7 8
3  |  .| 9 
   | ..|X  
```

Because of the placements of `7`s, and the existing values in block `6,0`, the *only* remaining space in that block that could contain a `7` is `(8, 1)`. The same situation is happening with `3`s in block `6,6`. Because our `possibilities` function is only doing a set subtraction, it fails to detect this.

I get the feeling that this is what Josh was getting in my first group; what you want in this situation is to figure out whether there's a unique place within a given block that a given value could go. These squares

```
 71|4 8|X35
   | 53|X8X
  3|9 7|6XX
-----------
   |  1|   
 9 |8 6|  3
   |  4|82 
-----------
 6 | 49|7 8
3  |  2| 9 
   | 85|  
```

have these possibilities:

```haskell
*Main> mapM_ (putStrLn . show . toList) $ map (possibilities (obv sample)) [(6, 0), (6, 1), (8, 1), (7, 2), (8, 2)]
[2,9]
[1,2,4,9]
[1,2,4,7,9]
[1,4]
[1,2,4]
*Main> 
```

As you can see, only one of those possibility sets contains `7`, whereas the other values could go in more than one place. What we want, in terms of our existing board definition, is a way to put that value in the place it can uniquely occupy. That's done here:

```haskell
uniqueInBlock :: Board -> (Int, Int) -> [(Int, Int, Int)]
uniqueInBlock board (x, y) = singles $ concatMap (toList . thd) posMap
  where posMap = [(x', y', possibilities board (x', y')) | (x', y') &lt;- es]
        es = blockEmpties board (x, y)
        singles = map (findInMap . head) . filter ((==1) . length) . group . sort
        findInMap n = let (x, y, p) = fromJust $ find (member n . thd) posMap
                      in (x, y, n)
```

That function takes a `Board` and an `(x, y)`, and returns the coordinates and values of each unique value in `block board (x, y)`. In our example board, 

```haskell
*Main> uniqueInBlock (obv sample) (6, 0)
[(8,1,7)]
*Main> uniqueInBlock (obv sample) (6, 6)
[(6,8,3)]
*Main> 
```

`blockwise` just takes that result and returns a board which includes those values. Last one:

```haskell
naiveSolve :: [(Board -> Board)] -> Board -> Board
naiveSolve functions board = rec functions board
  where rec [] board = board
        rec fns board = case Set.size $ empty new of
          0 -> new
          _ -> rec nextFns new
          where new = (head fns) $ board
                nextFns = if new == board then tail fns else functions
```

I mentioned earlier that the way this works is by trying to repeatedly solve the obvious squares, and resorts to blockwise analysis and guessing only when that doesn't work. This is the part that does the first two. It takes a list of `(Board -> Board)` functions, and repeatedly calls the first one. If that yields a solved board (one with no empty spaces), it returns that. If that yields an unchanged board, it calls the next function, then repeats that pattern until it runs out of functions to call. The effect is:

```
*Main> naiveSolve [obvious, blockwise] sample

 71|4 8| 35
   | 53| 87
  3|9 7|6  
-----------
   |  1|   
 9 |8 6|  3
   |  4|82 
-----------
 6 |349|7 8
3  |  2| 9 
   | 85|3 2

*Main> 
```

Which is a board where the only remaining moves are ones where we need to guess...

```haskell
guess :: Board -> [Board]
guess board = map (\v -> findEmpties $ board { values = newVals v }) vs
  where (x, y, vs) = head $ sortBy (comparing (length . thd)) posMap
        newVals v = [[if x == x' && y == y' then v else (values board) !! y' !! x' | x' &lt;- ixs board] | y' &lt;- ixs board]
        posMap = [(x, y, toList $ possibilities board (x, y)) | (x, y) &lt;- es]
        es = toList $ empty board
```

... which is done by picking the space with the fewest number of possibilities, and returning all possible next boards. In other words, 

```haskell
*Main> guess $ naiveSolve [obvious, blockwise] sample
[
 71|4 8| 35
 2 | 53| 87
  3|9 7|6  
-----------
   |  1|   
 9 |8 6|  3
   |  4|82 
-----------
 6 |349|7 8
3  |  2| 9 
   | 85|3 2
,
 71|4 8| 35
 4 | 53| 87
  3|9 7|6  
-----------
   |  1|   
 9 |8 6|  3
   |  4|82 
-----------
 6 |349|7 8
3  |  2| 9 
   | 85|3 2
]
*Main> 
```

Note space `(1, 1)` there. Finally, we need to solve that.

```haskell
solve :: Board -> Board
solve board = rec [naiveSolve [obvious, blockwise] board]
  where solved board = 0 == (Set.size $ empty board)
        impossible board = any ((==0) . length) . map (toList . possibilities board) . toList $ empty board
        rec [] = board -- Failed
        rec !boards = case find solved $ boards of
          Just b -> b
          Nothing -> rec . map (naiveSolve [obvious, blockwise]) . concatMap guess $ filter (not . impossible) boards
```

That function takes a board, runs `naiveSolve` on it, and returns it if solved. Otherwise, it repeatedly runs `map (naiveSolve [obvious, blockwise]) . concatMap guess` on the list of boards that aren't impossible. and there, that solves Sudoku.

```haskell
*Main> solve sample

971|468|235
624|153|987
853|927|641
-----------
538|291|476
492|876|153
716|534|829
-----------
265|349|718
387|612|594
149|785|362

*Main> 
```

That particular solution gets returned in under a second, even in `GHCi`. I mentioned that it works "in the general case". What I mean by that is that it can solve boards which are obvious, *and* those which require guessing, **and** those which are non-standard sizes<a name="note-Sun-Jun-02-173523EDT-2013"></a>[|2|](#foot-Sun-Jun-02-173523EDT-2013)

So there. I'm not at all proud of this because it does fairly poorly on boards that require extensive guessing, even with the `-O2` option, and because as I'll show you [later today](http://langnostic.blogspot.ca/2013/06/sudoku-reredux.html), it's not anywhere near as elegant a solution as you can get.

But first, I need some tea.

* * *
##### Footnotes

1 - <a name="foot-Sun-Jun-02-173100EDT-2013"></a>[|back|](#note-Sun-Jun-02-173100EDT-2013) - Though still quicker than that bogo-sort solution I described from the actual event.

2 - <a name="foot-Sun-Jun-02-173523EDT-2013"></a>[|back|](#note-Sun-Jun-02-173523EDT-2013) - Specifically, it handles 4x4, 9x9, 16x16, 25x25, etc. Any board with a block size such that `blockSize^2 == boardSize`. Most of the solutions both at the [Haskellwiki](http://www.haskell.org/haskellwiki/Sudoku) and at [Rosetta Code](http://rosettacode.org/wiki/Sudoku) solve 9x9 only. The larger boards obviously take more time and memory to solve.
