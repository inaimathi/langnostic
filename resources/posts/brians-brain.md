So we ended up implementing Brian's Brain at the last [Toronto Code Retreat](https://bentomiso.com/events/toronto-code-retreat-2014-aug). I'm going to show you the sparse solution I worked up in Haskell in a moment. But I have to admit that the highlight of the evening was definitely [Brian](https://en.wikipedia.org/wiki/Brian_Silverman) coming by and giving us an impromptu talk about various [cellular automata](http://playfulinvention.com/tests/pft/), including some comments about how and why he investigated [seeds](https://en.wikipedia.org/wiki/Seeds_%28cellular_automaton%29), [brian's brain](https://en.wikipedia.org/wiki/Brian%27s_Brain) and [wireworld](http://en.wikipedia.org/wiki/Wireworld). The `cliffs-notes.tar` version is that at the time there was lots of academic activity going on related to the [Turing completeness](https://en.wikipedia.org/wiki/Turing_completeness) of the Game of Life. It did end up getting proved, as mentioned in the [examples section](https://en.wikipedia.org/wiki/Turing_completeness#Examples) of that wiki page, but the Life-based Turing machine took some (for the time) obscenely large amount of memory. Brian basically wanted a Turing-complete cellular automaton that would fit on the machine he had access to. Hence experiments with things like [this binary counter](http://playfulinvention.com/tests/pft/14%20addone.html).

I may have more to say about the conversation, but for now, lets talk about gridless cellular automata. In Haskell.

```haskell
{-# LANGUAGE DeriveDataTypeable #-}

module BriansBrain where

import Data.Data (Data, Typeable)
import Data.List (nub, groupBy, sortBy)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.IxSet (Indexable(..), IxSet(..), (@=), getOne, ixFun, ixSet, fromList, toList)

data State = Off | Recovering | On deriving (Eq, Ord, Show, Typeable)
data Coords = Coords { x :: Int, y :: Int } deriving (Eq, Ord, Show, Typeable)
data Cell = Cell { coords :: Coords, state :: State } deriving (Eq, Ord, Show, Typeable)
instance Indexable Cell where
    empty = ixSet [ ixFun $ (:[]) . coords
                  , ixFun $ (:[]) . state ]

type Grid = IxSet Cell
type Rules = (Cell -> [Cell] -> Maybe Cell)

neighbors :: Grid -> Cell -> [Cell]
neighbors grid (Cell (Coords x y) _) = map cellAt cs
    where cs = [Coords (x'+x) (y'+y) | x' <- [-1..1], y' <- [-1..1], (x', y') /= (0, 0)]
          cellAt c = case getOne $ grid @= c of
                       Just cell -> cell 
                       Nothing -> Cell c Off

next :: Rules -> Grid -> Grid
next fn grid = fromList $ catMaybes $ map (\c -> fn c (neighbors grid c)) potentials 
    where potentials = nub $ concatMap (neighbors grid) $ toList grid

briansRules :: Rules
briansRules (Cell coords On) _ = Just $ Cell coords Recovering
briansRules (Cell _ Recovering) _ = Nothing
briansRules (Cell coords Off) ns = case filter (==On) $ map state ns of
                                     [_, _] -> Just $ Cell coords On
                                     _ -> Nothing

main = mapM_ print $ take 4 $ iterate (next briansRules) osc
    where print g = do putStr $ showGrid g
                       putStrLn "------------------------------"

----- Show routines
showGrid :: Grid -> String
showGrid g = unlines $ map showLine lines
    where desc = sortBy (onY compare) $ toList g
          lines = groupBy (onY (==)) desc
          onY = (`on` (y . coords))

showLine :: [Cell] -> String
showLine ln = recur ln 0 ""
    where recur [] _ acc = reverse acc
          recur ln@((Cell (Coords x _) state):rest) ct acc
              | x == ct = recur rest (succ ct) $ (showState state):acc
              | otherwise = recur ln (succ ct) $ ' ':acc

showState Off = ' '
showState On = 'X'
showState Recovering = 'O'

----- Test data
osc :: Grid
osc = fromList [ Cell (Coords 2 1) Recovering, Cell (Coords 2 2) On, Cell (Coords 3 2) On
               , Cell (Coords 4 2) Recovering, Cell (Coords 1 3)  Recovering, Cell (Coords 2 3) On
               , Cell (Coords 3 3) On, Cell (Coords 3 4) Recovering
               ]
```

And as usual, lets go through it chunkwise.

```haskell
{-# LANGUAGE DeriveDataTypeable #-}

module BriansBrain where

import Data.Data (Data, Typeable)
import Data.List (nub, groupBy, sortBy)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.IxSet (Indexable(..), IxSet(..), (@=), getOne, ixFun, ixSet, fromList, toList)
```

I'm using one language extension, because this sparse solution will be calling on [`IxSet`](http://hackage.haskell.org/package/ixset), and I don't feel like declaring my own `Typeable` instances. Otherwise, module declaration and importing boilerplate. Nothing to see here.

```haskell
data State = Off | Recovering | On deriving (Eq, Ord, Show, Typeable)
data Coords = Coords { x :: Int, y :: Int } deriving (Eq, Ord, Show, Typeable)
data Cell = Cell { coords :: Coords, state :: State } deriving (Eq, Ord, Show, Typeable)
instance Indexable Cell where
    empty = ixSet [ ixFun $ (:[]) . coords
                  , ixFun $ (:[]) . state ]

type Grid = IxSet Cell
type Rules = (Cell -> [Cell] -> Maybe Cell)
```

These are the data declarations. You might get enough information out of these alone to understand what the rest of the program does<a name="note-Sat-Aug-16-231431EDT-2014"></a>[|1|](#foot-Sat-Aug-16-231431EDT-2014). A Brian's Brain cell has three possible states: `Off`, `Recovering` and `On`. The `Coords` are just `x`/`y` pairs of `Int`s. Because I'm doing a 2-dimensional implementation. For more dimensions, we'd just change the shape of this type. A `Cell` is a set of `Coord`s and a `State`. As you can see from that `Indexable` instance<a name="note-Sat-Aug-16-231436EDT-2014"></a>[|2|](#foot-Sat-Aug-16-231436EDT-2014), `Cell`s can be indexed either by `coords` or by `state`. Which is to say that if we had an `IxSet` of `Cell`s, we could either ask for the cell at a particular set of coordinates, or we could get all cells in a particular state. Coincidentally, a `Grid` is an `IxSet` of `Cell`s, so we can *actually* pull that trick I just mentioned. Finally, the `Rules` is a function that takes a `Cell`, a list of `Cell`s representing that first `Cell`s' neighborhood, and returns a `Maybe Cell`. It's a `Maybe` rather than just a `Cell` because we're using a sparse representation, and the end result may be a cell that won't be represented explicitly.

```haskell
neighbors :: Grid -> Cell -> [Cell]
neighbors grid (Cell (Coords x y) _) = map cellAt cs
    where cs = [Coords (x'+x) (y'+y) | x' <- [-1..1], y' <- [-1..1], (x', y') /= (0, 0)]
          cellAt c = case getOne $ grid @= c of
                       Just cell -> cell 
                       Nothing -> Cell c Off
```

Given a particular `Grid`, and a `Cell`, we find the cells' neighbors by indexing into the `Grid` at each member of its [Moore Neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood). If there is a cell at that index, we return the cell itself, otherwise we return an empty cell at that coordinate. We need to do that, because some `Rules` functions might involve empty cells (as indeed Brian's Brain does), so it would complicate individual `Rules` if we didn't return them as part of a neighborhood.

```haskell
next :: Rules -> Grid -> Grid
next fn grid = fromList $ catMaybes $ map (\c -> fn c (neighbors grid c)) potentials 
    where potentials = nub $ concatMap (neighbors grid) $ toList grid
```

Getting a fresh `Grid` means


1.   finding all cells that could potentially change in the next generation, which might be any represented cell or any implicit neighbor of the same<a name="note-Sat-Aug-16-231446EDT-2014"></a>[|3|](#foot-Sat-Aug-16-231446EDT-2014)
1.   running the given `Rules` function over each of them
1.   filtering out the `Nothing`s from the results and unpacking the `Just`s with `catMaybes`
1.   making an `IxSet` out of the list of cells that emerges


Incidentally, that was everything you need for a core solution. The rest of this program is the specific Brian's Brain `Rules`, the code for pretty-printing a `Grid`, and a test layout that represents [an oscillator](https://en.wikipedia.org/wiki/Brian%27s_Brain#mediaviewer/File:BriansBrain_oscillator.gif). The stuff we've already looked at is enough to take a `Grid` to its next generation.

```haskell
briansRules :: Rules
briansRules (Cell coords On) _ = Just $ Cell coords Recovering
briansRules (Cell _ Recovering) _ = Nothing
briansRules (Cell coords Off) ns = case filter (==On) $ map state ns of
                                     [_, _] -> Just $ Cell coords On
                                     _ -> Nothing
```

The `Rules` for Brian's Brain are


1.   An `On` cell goes to `Recovering`
1.   A `Recovering` cell goes to `Off`, which we're representing implicitly so we actually return `Nothing`
1.   An `Off` cell with exactly two neighbors goes to `On`


And this function is the most straight-forward expression of those rules I could manage.

```haskell
main = mapM_ print $ take 4 $ iterate (next briansRules) osc
    where print g = do putStr $ showGrid g
                       putStrLn "------------------------------"
```

The `main` function is the only one that causes side-effects, because it prints things out. It takes the grid layout labelled `osc`, cranks it forward four generations, and prints each one with a separating row of `-`s.

```haskell
----- Show routines
showGrid :: Grid -> String
showGrid g = unlines $ map showLine lines
    where desc = sortBy (onY compare) $ toList g
          lines = groupBy (onY (==)) desc
          onY = (`on` (y . coords))

showLine :: [Cell] -> String
showLine ln = recur ln 0 ""
    where recur [] _ acc = reverse acc
          recur ln@((Cell (Coords x _) state):rest) ct acc
              | x == ct = recur rest (succ ct) $ (showState state):acc
              | otherwise = recur ln (succ ct) $ ' ':acc

showState Off = ' '
showState On = 'X'
showState Recovering = 'O'
```

The `showGrid`/`showLine`/`showState` functions are the things that format, respectively, a `Grid`, line and `State`. We print a `State` as the `space`, `X` and `O` characters as appropriate. We print a line of `Cell`s by starting from 0, and collecting either a space or the current `Cell`s' formatted state, then recurring (incrementing the counter, and dropping the head `Cell` as appropriate). Finally, we print a `Grid` by grouping its contained `Cell`s by their `y` coordinate, giving us lines, then mapping `showLine` over the resulting list of lines.

```haskell
----- Test data
osc :: Grid
osc = fromList [ Cell (Coords 2 1) Recovering, Cell (Coords 2 2) On, Cell (Coords 3 2) On
               , Cell (Coords 4 2) Recovering, Cell (Coords 1 3)  Recovering, Cell (Coords 2 3) On
               , Cell (Coords 3 3) On, Cell (Coords 3 4) Recovering
               ]
```

Last bit; this is what [that oscillator](https://en.wikipedia.org/wiki/Brian%27s_Brain#mediaviewer/File:BriansBrain_oscillator.gif) looks like when encoded for a gridless cellular automaton. It doesn't look quite as intuitively nice as a grid, but that's what our formatting functions are for.

```haskell
*BriansBrain> :load "BriansBrain.hs"
[1 of 1] Compiling BriansBrain      ( BriansBrain.hs, interpreted )
Ok, modules loaded: BriansBrain.
*BriansBrain> showGrid osc
Loading package syb-0.4.0 ... linking ... done.
Loading package old-locale-1.0.0.5 ... linking ... done.
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package time-1.4.0.1 ... linking ... done.
Loading package bytestring-0.10.0.2 ... linking ... done.
Loading package pretty-1.1.1.0 ... linking ... done.
Loading package old-time-1.1.0.1 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
Loading package cereal-0.4.0.1 ... linking ... done.
Loading package template-haskell ... linking ... done.
Loading package text-1.1.1.3 ... linking ... done.
Loading package primitive-0.5.3.0 ... linking ... done.
Loading package vector-0.10.11.0 ... linking ... done.
Loading package safecopy-0.8.3 ... linking ... done.
Loading package syb-with-class-0.6.1.5 ... linking ... done.
Loading package ixset-1.0.5 ... linking ... done.
"  O\n  XXO\n OXX\n   O\n"
*BriansBrain> putStr $ showGrid osc
  O
  XXO
 OXX
   O
*BriansBrain> main
  O
  XXO
 OXX
   O
------------------------------
   X
 XOO
  OOX
  X
------------------------------
  XO
 O  X
 X  O
  OX
------------------------------
  O
  XXO
 OXX
   O
------------------------------
*BriansBrain> 
```

That's that. Hope you learned something.


* * *
##### Footnotes

1 - <a name="foot-Sat-Aug-16-231431EDT-2014"></a>[|back|](#note-Sat-Aug-16-231431EDT-2014) - But don't worry, we're still going through everything.

2 - <a name="foot-Sat-Aug-16-231436EDT-2014"></a>[|back|](#note-Sat-Aug-16-231436EDT-2014) - Which is the only reason I need `Typeable` imported in this project.

3 - <a name="foot-Sat-Aug-16-231446EDT-2014"></a>[|back|](#note-Sat-Aug-16-231446EDT-2014) - Actually, because of the way Brian's Brain rules are constructed, we only need to be concerned with cells that are `On`, and their non-`Recovering` neighbors. It's probably possible to write rules that are more general, and I didn't want to do the extra filtering work, so I just did the easier thing here.
