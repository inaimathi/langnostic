At a recent [CodeRetreat](https://github.com/CodeRetreatTO), we tackled [Langton's Ant](http://en.wikipedia.org/wiki/Langton%27s_ant) as the problem. It's another two-state cellular automaton, except that the rules are centered around a cursor named the "ant", rather than each cells' neighborhood. As of this writing, [`rabraham`](https://github.com/rabraham) and I are the only ones who've posted their solution. Although, to be fair, what you'll see [here](https://github.com/CodeRetreatTO/projects/blob/master/2014-09-langtons-ant/rabraham-and-inaimathi.hs) is *not* the raw solution, but rather the result of the raw solution with another hour or two of polish put on it after the fact. Lets go sequentially though, starting with [what we actually wrote](https://github.com/CodeRetreatTO/projects/blob/961fc0376e0074e8af34c9f93e55a6121b5c2981/2014-09-langtons-ant/rabraham-and-inaimathi.hs) at the event from the initial state in [history](https://github.com/CodeRetreatTO/projects/commits/master/2014-09-langtons-ant/rabraham-and-inaimathi.hs).

#### <a name="langtons-haskelly-ant-take-one" href="#langtons-haskelly-ant-take-one"></a>Langton's Haskelly Ant, Take One

```haskell
module Langton where

import Prelude hiding (Left, Right, flip)

data Direction = Up | Right | Down | Left deriving (Eq, Enum, Show)

right :: Direction -> Direction 
right Left = Up
right dir = succ dir

left :: Direction -> Direction
left Up = Left
left dir = pred dir

data Ant = Ant Int Int Direction deriving (Eq, Show)
data World = World Ant Coords deriving (Show)
type Coords = [(Int, Int)]


flip :: (Int, Int) -> Coords -> Coords
flip cell coords= if cell `elem` coords
                  then filter (/=cell) coords
                  else cell:coords

turn :: Ant -> Coords -> Ant
turn (Ant x y dir) coords = Ant x y $ if (x, y) `elem` coords
                                      then left dir
                                      else right dir

onwards :: Ant -> Ant
onwards (Ant x y Up) = Ant x (pred y) Up
onwards (Ant x y Right) = Ant (succ x) y Right
onwards (Ant x y Down) = Ant x (succ y) Down
onwards (Ant x y Left) = Ant (pred x) y Left

step :: World -> World
step (World ant@(Ant x y _) coords) = World newAnt newCoords
    where newCoords = flip (x, y) coords
          newAnt = onwards $ turn ant coords

test = World (Ant 4 4 Up) []

main = mapM_ (putStrLn . show) . take 400 $ iterate step test
```

Yes, it's in Haskell. I sort of assumed you knew that. First things first, module imports and namespace minutia.

```haskell
module Langton where

import Prelude hiding (Left, Right, flip)
```

We'll be defining our own `Left` and `Right` constructors, and a different `flip` function, so we need to hide the defaults from `Prelude`. Next up, basic data declarations:

```haskell
data Direction = Up | Right | Down | Left deriving (Eq, Enum, Show)

right :: Direction -> Direction 
right Left = Up
right dir = succ dir

left :: Direction -> Direction
left Up = Left
left dir = pred dir

data Ant = Ant Int Int Direction deriving (Eq, Show)
data World = World Ant Coords deriving (Show)
type Coords = [(Int, Int)]
```

We've got a `Direction` type that we'd like to wrap around at either end. As far as I know, there isn't a built-in Haskell typeclass that gives us that, so we define our own `right` and `left` functions that do the right thing on `Up` and `Down` respectively<a name="note-Sat-Sep-20-133912EDT-2014"></a>[|1|](#foot-Sat-Sep-20-133912EDT-2014).

An `Ant` is a pair of `Int`s that designate its current `x` and `y` coordinate, and a `Direction` that encodes its current facing.

```haskell
data Ant = Ant Int Int Direction deriving (Eq, Show)
```

We used my usual sparse world representation, since this *is* a two-state automaton. Which means that a world is only going to track "on" cells. A set of coordinates is an `(Int, Int)` list. Finally, a `World` is one `Ant` and a set of coordinates.

```haskell
data World = World Ant Coords deriving (Show)
type Coords = [(Int, Int)]
```

Since we're using a sparse list for the board representation, `flip`ping a cell means either filtering it from the list (if it's there already) or `cons`ing it onto the list (if it isn't)

```haskell
flip :: (Int, Int) -> Coords -> Coords
flip cell coords= if cell `elem` coords
                  then filter (/=cell) coords
                  else cell:coords
```

We could have done this as a guard statement

```haskell
flip :: (Int, Int) -> Coords -> Coords
flip cell coords
    | cell `elem` coords = filter (/=cell) coords
    | otherwise = cell:coords
```

I'm not entirely sure why we didn't. I too was busy explaining Haskell syntax to really think through this thing at the time, I guess. Incidentally, this is why we wanted to shadow the default `Prelude.flip`. Ok, next up, turning an ant according to [the automata rules](http://en.wikipedia.org/wiki/Langton%27s_ant#Rules) means that we have to `turn` it to the `left` if it's on an `on` cell and `turn` it `right` if it isn't.

```haskell
turn :: Ant -> Coords -> Ant
turn (Ant x y dir) coords = Ant x y $ if (x, y) `elem` coords
                                      then left dir
                                      else right dir
```

Moving an `Ant` forward means changing either its `x` or `y` coordinate depending on its `Direction`.

```haskell
onwards :: Ant -> Ant
onwards (Ant x y Up) = Ant x (pred y) Up
onwards (Ant x y Right) = Ant (succ x) y Right
onwards (Ant x y Down) = Ant x (succ y) Down
onwards (Ant x y Left) = Ant (pred x) y Left
```

Ok, getting to the end here. `step`ping a world means `flip`ping the cell at the `Ant`, `turn`ing the ant as appropriate and moving the ant `onwards` once it has been turned.

```haskell
step :: World -> World
step (World ant@(Ant x y _) coords) = World newAnt newCoords
    where newCoords = flip (x, y) coords
          newAnt = onwards $ turn ant coords
```

Since we're writing Haskell, the result of this operation is going to have to be a new `World`, with the newly turned and re-positioned `Ant` and modified `Coords`, rather than merely a mutation of an existing area of memory.

Now that we know how to step a world, we can define a test starting position

```haskell
test = World (Ant 4 4 Up) []
```

and see how it shapes up across a few generations.

```haskell
main = mapM_ (putStrLn . show) . take 400 $ iterate step test
```

Running that in GHCi gets us

```haskell
Prelude> :load "/home/inaimathi/projects/code-retreat/projects/rabraham-and-inaimathi.hs"
[1 of 1] Compiling Langton          ( /home/inaimathi/projects/code-retreat/projects/rabraham-and-inaimathi.hs, interpreted )
Ok, modules loaded: Langton.
*Langton> Langton.main
World (Ant 4 4 Up) []
World (Ant 5 4 Right) [(4,4)]
World (Ant 5 5 Down) [(5,4),(4,4)]
World (Ant 4 5 Left) [(5,5),(5,4),(4,4)]

...snip a few hundred more...

World (Ant 7 3 Down) [(7,2),(6,2),(5,3),(6,4),(4,5),(3,4),(2,1),(1,1),(0,1),(-1,0),(-2,0),(-2,1),(0,0),(-1,-1),(1,-2),(0,-2),(2,-1),(3,1),(4,1),(5,1),(6,0),(7,-1),(7,-2),(6,-2),(7,0),(8,-1),(9,1),(9,0),(8,2),(5,2),(6,7),(7,7),(8,7),(9,8),(10,8),(10,7),(8,8),(9,9),(7,10),(8,10),(6,9),(5,7),(4,7),(3,7),(2,8),(1,9),(1,10),(2,10),(1,8),(0,9),(-1,7),(-1,8),(0,6),(3,6),(7,5),(7,4),(8,5),(1,3),(1,4),(0,3),(7,3),(1,5)]
World (Ant 8 3 Right) [(7,2),(6,2),(5,3),(6,4),(4,5),(3,4),(2,1),(1,1),(0,1),(-1,0),(-2,0),(-2,1),(0,0),(-1,-1),(1,-2),(0,-2),(2,-1),(3,1),(4,1),(5,1),(6,0),(7,-1),(7,-2),(6,-2),(7,0),(8,-1),(9,1),(9,0),(8,2),(5,2),(6,7),(7,7),(8,7),(9,8),(10,8),(10,7),(8,8),(9,9),(7,10),(8,10),(6,9),(5,7),(4,7),(3,7),(2,8),(1,9),(1,10),(2,10),(1,8),(0,9),(-1,7),(-1,8),(0,6),(3,6),(7,5),(7,4),(8,5),(1,3),(1,4),(0,3),(1,5)]
*Langton> 
```

You'll note that some of those coordinates are negative, since we intentionally didn't restrict our `Ant` from walking off the edge of the board. This is what we came up with at the meetup itself, and I mentioned at the time that we probably would have taken a while to write a print routine, as well as handle multiple `Ant`s. I mentioned, half-jokingly, that the printing code would probably be more difficult than the extra cursors. I think I'm going to have to shut up about that, because as you'll see in the next two chunks of this article, neither are particularly complicated<a name="note-Sat-Sep-20-133941EDT-2014"></a>[|2|](#foot-Sat-Sep-20-133941EDT-2014).

#### <a name="langtons-haskelly-ant-take-two-m-m-m-m-multiant" href="#langtons-haskelly-ant-take-two-m-m-m-m-multiant"></a>Langton's Haskelly Ant, Take Two: M m m m multi-ant!

Here's what I wrote on the subway, on my way home from the event:

```haskell
module Langton where

import Prelude hiding (Left, Right)

data Direction = Up | Right | Down | Left deriving (Eq, Enum, Show)

right :: Direction -> Direction 
right Left = Up
right dir = succ dir

left :: Direction -> Direction
left Up = Left
left dir = pred dir

data Ant = Ant Int Int Direction deriving (Eq, Show)
data World = World [Ant] Coords deriving (Show)
type Coords = [(Int, Int)]


flipCell :: (Int, Int) -> Coords -> Coords
flipCell cell coords= if cell `elem` coords
                  then filter (/=cell) coords
                  else cell:coords

turn :: Coords -> Ant -> Ant
turn coords (Ant x y dir) = Ant x y $ if (x, y) `elem` coords
                                      then left dir
                                      else right dir

onwards :: Ant -> Ant
onwards (Ant x y Up) = Ant x (pred y) Up
onwards (Ant x y Right) = Ant (succ x) y Right
onwards (Ant x y Down) = Ant x (succ y) Down
onwards (Ant x y Left) = Ant (pred x) y Left

step :: World -> World
step (World ants coords) = World newAnts newCoords
    where newCoords = foldl (\memo (Ant x y _) -> flipCell (x, y) memo) coords ants
          newAnts = map (onwards . turn coords) ants

test = World [(Ant 4 4 Up), (Ant 3 7 Left)] []

main = mapM_ (putStrLn . show) . take 10 $ iterate step test
```

Since you already read through the single-ant solution, all you need to know are [the deltas](https://github.com/CodeRetreatTO/projects/commit/87cdeb82c39490d4fbf8580804b6bb319b8043e2). Adding more ants comes down to exactly three changes. First, a world is no longer an `Ant` and some `Coords`, rather it's a list of `Ant`s and a some `Coords`.

data World = World [Ant] Coords deriving (Show)

Second, stepping a world now involves `fold`ing over the `Ant`s to generate new `Coords`, and `map`ping over them to generate a new list of `Ant`s

```haskell
step :: World -> World
step (World ants coords) = World newAnts newCoords
    where newCoords = foldl (\memo (Ant x y _) -> flipCell (x, y) memo) coords ants
          newAnts = map (onwards . turn coords) ants
```

As a result, I also changed the order of arguments to `turn` so that it would be more easily composed in that last line of the new `step` definition.

```haskell
turn :: Coords -> Ant -> Ant
turn coords (Ant x y dir) = Ant x y $ if (x, y) `elem` coords
                                      then left dir
                                      else right dir
```

Oh, and I gratuitously renamed our `flip` function to `flipCell` so that we no longer have to shadow `Prelude.flip`. Finally, I had to define a new test world.

```haskell
test = World [(Ant 4 4 Up), (Ant 3 7 Left)] []
```

Which provided very slightly different output.

```haskell
*Langton> :load "/home/inaimathi/projects/code-retreat/projects/rabraham-and-inaimathi.hs"
[1 of 1] Compiling Langton          ( /home/inaimathi/projects/code-retreat/projects/rabraham-and-inaimathi.hs, interpreted )
Ok, modules loaded: Langton.
*Langton> main
World [Ant 4 4 Up,Ant 3 7 Left] []
World [Ant 5 4 Right,Ant 3 6 Up] [(3,7),(4,4)]
World [Ant 5 5 Down,Ant 4 6 Right] [(3,6),(5,4),(3,7),(4,4)]
World [Ant 4 5 Left,Ant 4 7 Down] [(4,6),(5,5),(3,6),(5,4),(3,7),(4,4)]
World [Ant 4 4 Up,Ant 3 7 Left] [(4,7),(4,5),(4,6),(5,5),(3,6),(5,4),(3,7),(4,4)]
World [Ant 3 4 Left,Ant 3 8 Down] [(4,7),(4,5),(4,6),(5,5),(3,6),(5,4)]
World [Ant 3 3 Up,Ant 2 8 Left] [(3,8),(3,4),(4,7),(4,5),(4,6),(5,5),(3,6),(5,4)]
World [Ant 4 3 Right,Ant 2 7 Up] [(2,8),(3,3),(3,8),(3,4),(4,7),(4,5),(4,6),(5,5),(3,6),(5,4)]
World [Ant 4 4 Down,Ant 3 7 Right] [(2,7),(4,3),(2,8),(3,3),(3,8),(3,4),(4,7),(4,5),(4,6),(5,5),(3,6),(5,4)]

... and so on ...

*Langton> 
```

Now then...

#### <a name="langtons-haskelly-ant-take-three-printing" href="#langtons-haskelly-ant-take-three-printing"></a>Langton's Haskelly Ant, Take Three: Printing

Part two of my subway trip was devoted to printing this world in a more pleasing way.

```haskell
module Langton where

import Prelude hiding (Left, Right)
import Data.Set (Set(..), member, insert, delete, fromList)

data Direction = Up | Right | Down | Left deriving (Eq, Enum, Show)

right :: Direction -> Direction 
right Left = Up
right dir = succ dir

left :: Direction -> Direction
left Up = Left
left dir = pred dir

data Ant = Ant Int Int Direction deriving (Eq, Show)
data World = World [Ant] Coords deriving (Show)
type Coords = Set (Int, Int)

flipCell :: (Int, Int) -> Coords -> Coords
flipCell cell coords= if cell `member` coords
                  then cell `delete` coords
                  else cell `insert` coords

turn :: Coords -> Ant -> Ant
turn coords (Ant x y dir) = Ant x y $ if (x, y) `member` coords
                                      then left dir
                                      else right dir

onwards :: Ant -> Ant
onwards (Ant x y Up) = Ant x (pred y) Up
onwards (Ant x y Right) = Ant (succ x) y Right
onwards (Ant x y Down) = Ant x (succ y) Down
onwards (Ant x y Left) = Ant (pred x) y Left

step :: World -> World
step (World ants coords) = World newAnts newCoords
    where newCoords = foldl (\memo (Ant x y _) -> flipCell (x, y) memo) coords ants
          newAnts = map (onwards . turn coords) ants

test = World [(Ant 4 4 Up), (Ant 3 7 Left)] $ fromList []

showWorld :: (Int, Int) -> World -> String
showWorld (w, h) (World ants coords) = unlines [line y | y &lt;- [0..h]]
    where line y = [charOf (x, y) | x &lt;- [0..w]]
          antCells = map (\(Ant x y _) -> (x, y)) ants
          charOf cell
              | cell `elem` antCells = '+'
              | cell `member` coords = 'O'
              | otherwise = ' '

main = mapM_ (putStrLn . showWorld (10, 10)) . take 40 $ iterate step test
```

Again, since you've been keeping up, you'll only need to know [the deltas](https://github.com/CodeRetreatTO/projects/commit/d9eddbc01d722ab8032608987e3cb1bf6330d04a). Firstly, I decided to change the representation of `Coords`, since printing would entail *a lot* of membership checks, and I wanted those to be at least somewhat fast. To that end, I imported `Data.Set`, and very slightly changed the definitions of `flipCell` and `turn`.

```haskell
...
import Data.Set (Set(..), member, insert, delete, fromList)
...
type Coords = Set (Int, Int)
...
flipCell :: (Int, Int) -> Coords -> Coords
flipCell cell coords= if cell `member` coords
                  then cell `delete` coords
                  else cell `insert` coords
...
turn :: Coords -> Ant -> Ant
turn coords (Ant x y dir) = Ant x y $ if (x, y) `member` coords
                                      then left dir
                                      else right dir
...
```

The functions just needed to use the right membership check, insertions and deletions. Membership happens to be called `elem` for regular linked lists but `member` for `Set`s, deletion from linked lists was a `filter` call but a straight up `delete` for `Set`s, and finally, `cons` was changed out for `insert`. Next up, `showWorld` itself:

```haskell
showWorld :: (Int, Int) -> World -> String
showWorld (w, h) (World ants coords) = unlines [line y | y &lt;- [0..h]]
    where line y = [charOf (x, y) | x &lt;- [0..w]]
          antCells = map (\(Ant x y _) -> (x, y)) ants
          charOf cell
              | cell `elem` antCells = '+'
              | cell `member` coords = 'O'
              | otherwise = ' '
```

Since I'm trying to format these as a grid, I need to know how big to make it. I probably should have taken a pair of coordinates instead, and probably *could* have checked the `min` and `max` of the World instead of accepting them, but it seemed easier this way. Left as an exercise for the reader, I think. For every line of the printed grid, we want to collect a character for each cell. If the cell contains an Ant, we'll collect `'+'`, if it's an "on" cell we'll collect `'O'`, otherwise, we'll collect a space (`' '`). Our main function also needs to change to accommodate this approach to display.

```haskell
main = mapM_ (putStrLn . showWorld (10, 10)) . take 40 $ iterate step test
```

The output should now look rather prettier:

```haskell
*Langton> :load "/home/inaimathi/projects/code-retreat/projects/rabraham-and-inaimathi.hs"
[1 of 1] Compiling Langton          ( /home/inaimathi/projects/code-retreat/projects/rabraham-and-inaimathi.hs, interpreted )
Ok, modules loaded: Langton.
*Langton> main
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
           
           
           
           
    +      
           
           
   +       
           
           
           

           
           
           
           
    O+     
           
   +       
   O       
           
           
           

           
           
           
           
    OO     
     +     
   O+      
   O       
           
           
           

           
           
           
           
    OO     
    +O     
   OO      
   O+      
           
           
           

           
           
           
           
    +O     
    OO     
   OO      
   +O      
           
           
           

           
           
           
           
   + O     
    OO     
   OO      
    O      
   +       
           
           

           
           
           
   +       
   O O     
    OO     
   OO      
    O      
  +O       
           
           

           
           
           
   O+      
   O O     
    OO     
   OO      
  + O      
  OO       
           
           

           
           
           
   OO      
   O+O     
    OO     
   OO      
  O+O      
  OO       
           
           

           
           
           
   OO      
   +OO     
    OO     
   OO      
  OOO      
  O+       
           
           

           
           
           
   OO      
    OO     
   +OO     
   OO      
  OOO      
  O +      
           
           

           
           
           
   OO      
    OO     
  +OOO     
   OO      
  OOO      
  O O      
    +      
           

           
           
           
   OO      
  + OO     
  OOOO     
   OO      
  OOO      
  O O      
   +O      
           

           
           
           
   OO      
  O+OO     
  OOOO     
   OO      
  OOO      
  O+O      
   OO      
           

           
           
           
   OO      
  OOOO     
  O+OO     
   OO      
  OOO      
  OO+      
   OO      
           

           
           
           
   OO      
  OOOO     
  O +O     
   OO      
  OO+      
  OO       
   OO      
           

           
           
... and a few more ...           
           
   OO      
  O  O     
 O   O     
O   O      
+O    +O   
  O  OOO   
   O  O    
    OO     

*Langton> 
```

Now, that's cool and all. And it's all I had time for on my subway ride home, but it did seem kind of a shame to have this newly pretty-ish representation printed to the REPL. So the next day, I whipped out [Haste](http://haste-lang.org/).

#### <a name="langtons-haskelly-ant-bonus-stage-the-dom" href="#langtons-haskelly-ant-bonus-stage-the-dom"></a>Langton's Haskelly Ant, Bonus Stage: The DOM

```haskell
module Main where

import Prelude hiding (Left, Right)

import Haste
import Data.Set (Set(..), member, insert, delete, fromList)
import Data.Maybe
import qualified Data.Map as Map

data Direction = Up | Right | Down | Left deriving (Eq, Enum, Show)

right :: Direction -> Direction 
right Left = Up
right dir = succ dir

left :: Direction -> Direction
left Up = Left
left dir = pred dir

data Ant = Ant Int Int Direction deriving (Eq, Show)
data World = World [Ant] Coords deriving (Show)
type Coords = Set (Int, Int)

flipCell :: (Int, Int) -> Coords -> Coords
flipCell cell coords= if cell `member` coords
                  then cell `delete` coords
                  else cell `insert` coords

turn :: Coords -> Ant -> Ant
turn coords (Ant x y dir) = Ant x y $ if (x, y) `member` coords
                                      then left dir
                                      else right dir

onwards :: Ant -> Ant
onwards (Ant x y Up) = Ant x (pred y) Up
onwards (Ant x y Right) = Ant (succ x) y Right
onwards (Ant x y Down) = Ant x (succ y) Down
onwards (Ant x y Left) = Ant (pred x) y Left

step :: World -> World
step (World ants coords) = World newAnts newCoords
    where newCoords = foldl (\memo (Ant x y _) -> flipCell (x, y) memo) coords ants
          newAnts = map (onwards . turn coords) ants


----- Pretty-print a world state
showWorld :: (Int, Int) -> World -> String
showWorld (w, h) (World ants coords) = unlines [line y | y &lt;- [0..h]]
    where line y = [charOf (x, y) | x &lt;- [0..w]]
          antCells = Map.fromList $ map (\(Ant x y dir) -> ((x, y), dir )) ants
          charDir Up = '&#8593;'
          charDir Right = '&#8594;'
          charDir Down = '&#8595;'
          charDir Left = '&#8592;'
          charOf cell
              | cell `Map.member` antCells = charDir . fromJust $ Map.lookup cell antCells
              | cell `member` coords = 'O'
              | otherwise = ' '

----- Haste stuff
setContent :: ElemID -> String -> IO ()
setContent id newContent = withElem id (\e -> setProp e "innerHTML" newContent)

animate :: Int -> (Int, Int) -> World -> Int -> IO ()
animate delay size world steps = setTimeout delay $ recur world steps
    where puts ct w = do setContent "world" $ showWorld size w
                         setContent "generations" $ show (steps - ct)
          recur world 0 = setTimeout delay $ puts 0 world
          recur world ct = do puts ct world
                              setTimeout delay $ recur (step world) $ pred ct


----- Test data and main
test = World [(Ant 4 4 Up), (Ant 3 7 Left), (Ant 27 34 Down)] $ fromList []

main :: IO ()
main = animate 10 (50, 50) test 4000
```

Firstly, I did make one gratuitous change. Specifically, I made `Ant`s print out directionally, rather than as a `'+'` regardless of their facing. This involved making an intermediate map of `Ant` coordinates to `Direction`s. Which meant importing `Data.Map` and `Data.Maybe`, and slightly complicating `showWorld`.

```haskell
...

import Data.Maybe
import qualified Data.Map as Map

...

showWorld :: (Int, Int) -> World -> String
showWorld (w, h) (World ants coords) = unlines [line y | y &lt;- [0..h]]
    where line y = [charOf (x, y) | x &lt;- [0..w]]
          antCells = Map.fromList $ map (\(Ant x y dir) -> ((x, y), dir )) ants
          charDir Up = '&#8593;'
          charDir Right = '&#8594;'
          charDir Down = '&#8595;'
          charDir Left = '&#8592;'
          charOf cell
              | cell `Map.member` antCells = charDir . fromJust $ Map.lookup cell antCells
              | cell `member` coords = 'O'
              | otherwise = ' '
```

You can see unicode arrows representing `Ant`s now, and you can also see the `fromJust` call that required `Data.Maybe`. Anyway, not important, minor changes. The *major* change is that chunklet under the fingerquotes helpful heading `Haste stuff`.

```haskell
----- Haste stuff
setContent :: ElemID -> String -> IO ()
setContent id newContent = withElem id (\e -> setProp e "innerHTML" newContent)

animate :: Int -> (Int, Int) -> World -> Int -> IO ()
animate delay size world steps = setTimeout delay $ recur world steps
    where puts ct w = do setContent "world" $ showWorld size w
                         setContent "generations" $ show (steps - ct)
          recur world 0 = setTimeout delay $ puts 0 world
          recur world ct = do puts ct world
                              setTimeout delay $ recur (step world) $ pred ct
```

`setContent` is just a helper function to let me easily set the `innerHTML` property of a particular element by referencing its `id`. `animate`, meanwhile does a nice piece of `setTimeout` recursion with which it changes the contents of a `pre` tag with the id `world` to the next iteration of our newly-defined three-ant test `World`.

This doesn't quite do anything by itself, but when you combine it with [this minimal HTML page](https://github.com/CodeRetreatTO/projects/blob/master/2014-09-langtons-ant/rabraham-and-inaimathi.html)

```html
&lt;html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  &lt;head>
    &lt;script type="text/javascript" src="rabraham-and-inaimathi.js">&lt;/script>
  &lt;/head>
  &lt;body>
    &lt;p id="generations">0&lt;/p>
    &lt;pre id="world" style="background-color: #eee;">&lt;/pre>
  &lt;/body>
&lt;/html>
```

what you get is [this](http://173.255.226.138/langtons-ants/rabraham-and-inaimathi.html).

Which is pretty cool by my estimation. You can see those unicode arrows dance around, creating ant tunnels as they go. The thing I find really cool about this, as opposed to the stuff I find myself building with [`parenscript`](https://github.com/vsedach/Parenscript), is that it's very *very* easy to re-use code from a non-Haste project in the context of something that ends up compiling to JS. If I wanted to put a bit more time and thought into this, I could easily have made the Langton's Ant module completely separate from the chunk that shows it off on the DOM, even though the latter would ultimately represent the former as some [pretty inscrutable JavaScript](https://github.com/CodeRetreatTO/projects/blob/master/2014-09-langtons-ant/rabraham-and-inaimathi.js#L2138-L2139). That's not usually the case over in Parenscript land. I often find myself having to carefully consider how a particular piece of code is going to be represented after the compilation step, so it was a pleasant surprise that Haste doesn't make me do the same.

Do take this assessment with a grain of salt though; if [the Parenscript Readme](https://github.com/vsedach/Parenscript/blob/master/README) is to be believed, I just don't know enough about it yet.

* * *
##### Footnotes

1 - <a name="foot-Sat-Sep-20-133912EDT-2014"></a>[|back|](#note-Sat-Sep-20-133912EDT-2014) - As a side-note, We could have also defined `left` as

```haskell
left :: Direction -> Direction
left = right . right . right
```

but didn't feel like gettin' fancy at the time

2 - <a name="foot-Sat-Sep-20-133941EDT-2014"></a>[|back|](#note-Sat-Sep-20-133941EDT-2014) - Although, to be fair, yes, the printing turned out to be more complicated than the extra ants. It turned out to have nothing to do with Haskell, or even the model really. This is just how complicated it is to print a two-dimensional board with no sugar.
