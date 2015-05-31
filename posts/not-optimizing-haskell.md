The flu can go fuck itself in its nonexistent, viral ass. This shit will not beat me. While I run down the clock, I'm profiling more things to make me feel a bit better.

First off, neither GHCi nor Haskell mode doesn't come with an interactive profiler. Or, as far as I can tell, any utilities to make batch profiling any easier. The way you [profile Haskell programs](http://www.haskell.org/ghc/docs/7.0.1/html/users_guide/profiling.html) is by installing the profiling extensions 

```shell
apt-get install libghc-mtl-dev libghc-mtl-prof
```

compiling your program with the profiling flags on

```shell
ghc -prof -auto-all -o outFile yourFile.hs
```

and then running the result with some different profiling flags.

```shell
./outfile +RTS -p
```

That should create a file called `outFile.prof` in the directory you just ran it from, and that file will contain a well formatted couple of tables that will tell you where your space and time cost-centers are.

So... lets automate this.

```emacs-lisp
(defun ha-custom-profile-buffer ()
  (interactive)
  (find-file-other-window 
   (ha-custom-profile-haskell-file (buffer-file-name))))

(defun ha-custom-profile-haskell-file (abs-filename)
  "Compiles the given file with profiling, 
runs it with the +RTS -p flags and returns
the filename of the profiling output."
  (assert (string= "hs" (file-name-extension abs-filename)))
  (let* ((f-name (file-name-sans-extension abs-filename))
         (tmp (make-temp-file f-name))
         (tmp-name (file-name-nondirectory tmp))
         (tmp-dir (file-name-directory tmp)))
    (message "Compiling...")
    (shell-command (format "ghc -prof -auto-all -o %s '%s'" tmp abs-filename))
    (message "Profiling...")
    (shell-command (format "%s./%s +RTS -p" tmp-dir tmp-name))
    (concat tmp-name ".prof")))
```

Those functions are both now part of my [ha-custom](https://github.com/Inaimathi/emacs-utils/blob/master/ha-custom.el) mode. The big one takes a Haskell file, compiles it to a tempfile with the appropriate flags, runs the result with the other appropriate flags, and returns the name of the profiling output file. The little function takes the current buffer and runs it through the big one, then opens the result in a new window. That should make it a bit easier to actually do the profiling.

### Actually Profiling Haskell

We started with pretty much the same thing as the Lisp code. And, I'll strip the printing elements again for the purposes of this exercise; we're not interested in how inefficient it is to actually produce a grid based on our model of the world.

```haskell
module Main where
import Data.List (group, sort, concatMap)
import Data.Set

lifeStep :: Set (Int, Int) -> Set (Int, Int)
lifeStep cells = fromList [head g | g &lt;- grouped cells, viable g]
  where grouped = group . sort . concatMap neighbors . toList
        neighbors (x, y) = [(x+dx, y+dy) | dx &lt;- [-1..1], dy &lt;- [-1..1], (dx,dy) /= (0,0)]
        viable [_,_,_] = True
        viable [c,_] = c `member` cells
        viable _ = False

runLife :: Int -> Set (Int, Int) -> Set (Int, Int)
runLife steps cells = rec (steps - 1) cells
  where rec 0 cells = cells
        rec s cells = rec (s - 1) $! lifeStep cells

glider = fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = fromList [(1, 0), (1, 1), (1, 2)]
gosperGliderGun = fromList [(24, 0), (22, 1), (24, 1), (12, 2), (13, 2), (20, 2), (21, 2), (34, 2), (35, 2), (11, 3), (15, 3), (20, 3), (21, 3), (34, 3), (35, 3), (0, 4), (1, 4), (10, 4), (16, 4), (20, 4), (21, 4), (0, 5), (1, 5), (10, 5), (14, 5), (16, 5), (17, 5), (22, 5), (24, 5), (10, 6), (16, 6), (24, 6), (11, 7), (15, 7), (12, 8), (13, 8)]

main :: IO ()
main = putStrLn . show $ runLife 5000 gosperGliderGun
```

It's *almost* the same, actually, because we determine frequencies differently. Instead of doing a single traversal of the corpus, we do what ***looks like*** a much more expensive operation composing `group` onto `sort` onto `concatMap neighbors`. In a book, that would be called "foreshadowing".

A first run-through of `M-x ha-custom-profile-buffer` gives us

```
        Fri Dec 14 21:48 2012 Time and Allocation Profiling Report  (Final)

           life21765U60 +RTS -p -RTS

        total time  =       30.15 secs   (30153 ticks @ 1000 us, 1 processor)
        total alloc = 24,382,856,840 bytes  (excludes profiling overheads)

COST CENTRE        MODULE    %time %alloc

lifeStep.grouped   Main       57.4   53.6
lifeStep.neighbors Main       24.7   40.9
lifeStep           Main       11.4    5.5
lifeStep.viable    Main        6.5    0.0


                                                                    individual     inherited
COST CENTRE               MODULE                  no.     entries  %time %alloc   %time %alloc

MAIN                      MAIN                     46           0    0.0    0.0   100.0  100.0
 CAF                      Main                     91           0    0.0    0.0   100.0  100.0
  gosperGliderGun         Main                     97           1    0.0    0.0     0.0    0.0
  main                    Main                     92           1    0.0    0.0   100.0  100.0
   runLife                Main                     93           1    0.0    0.0   100.0  100.0
    runLife.rec           Main                     94        5000    0.0    0.0   100.0  100.0
     lifeStep             Main                     95        4999   11.4    5.5   100.0  100.0
      lifeStep.viable     Main                     99    10002308    6.5    0.0     6.5    0.0
      lifeStep.grouped    Main                     96        4999   57.4   53.6    82.1   94.5
       lifeStep.neighbors Main                     98     2314620   24.7   40.9    24.7   40.9
 CAF                      Data.Set                 90           0    0.0    0.0     0.0    0.0
 CAF                      GHC.Conc.Signal          87           0    0.0    0.0     0.0    0.0
 CAF                      GHC.IO.Handle.FD         80           0    0.0    0.0     0.0    0.0
 CAF                      GHC.IO.Encoding          74           0    0.0    0.0     0.0    0.0
 CAF                      GHC.IO.Encoding.Iconv    62           0    0.0    0.0     0.0    0.0
```

We're actually only interested in that small table, so I'll omit the exhaustive one for the future. Basically, yes. `grouped` and `neighbors` are the resource-hogs here. Even still, this compares favorably against the Common Lisp infinite plane version; both in terms of program complexity and in terms of runtime. Not to mention that the initial CL version actually crashed at ~3000 iterations because it doesn't like tail recursion.

Anyhow, the first thing we're doing this time is limiting the size of the world.

```haskell
inRange :: Ord a => a -> a -> a -> Bool
inRange low n high = low &lt; n && n &lt; high

lifeStep :: Int -> Set (Int, Int) -> Set (Int, Int)
lifeStep worldSize cells = fromList [head g | g &lt;- grouped cells, viable g]
  where grouped = group . sort . concatMap neighbors . toList
        neighbors (x, y) = [(x+dx, y+dy) | dx &lt;- [-1..1], dy &lt;- [-1..1], 
                            (dx,dy) /= (0,0), inSize (dx+x) (dy+y)]
        inSize x y = inR x worldSize && inR y worldSize
        inR = inRange 0
        viable [_,_,_] = True
        viable [c,_] = c `member` cells
        viable _ = False

runLife :: Int -> Int -> Set (Int, Int) -> Set (Int, Int)
runLife worldSize steps cells = rec (steps - 1) cells
  where rec 0 cells = cells
        rec s cells = rec (s - 1) $! lifeStep worldSize cells

main :: IO ()
main = putStrLn . show $ runLife 50 5000 gosperGliderGun
```

That's gonna do the same thing it did yesterday; prevent massive, processor-fucking overpopulation.

```
        Fri Dec 14 22:03 2012 Time and Allocation Profiling Report  (Final)

           life21765GEE +RTS -p -RTS

        total time  =        1.61 secs   (1608 ticks @ 1000 us, 1 processor)
        total alloc = 1,132,473,192 bytes  (excludes profiling overheads)

COST CENTRE        MODULE  %time %alloc

lifeStep.grouped   Main     46.5   41.2
lifeStep.neighbors Main     23.4   37.8
inRange            Main     11.1   11.9
lifeStep           Main      6.2    3.0
lifeStep.viable    Main      6.1    0.0
lifeStep.inSize    Main      3.6    6.0
lifeStep.inR       Main      2.9    0.0
```

Granted, `inRange` is on the map as a cost center, but this shaved ~28 seconds off the final run time, I'm gonna call that fair enough. Given the numbers we were posting yesterday, I'm almost tempted to call this good enough. Lets see where it all goes, shall we? Step size of

### 50

```
        Fri Dec 14 22:06 2012 Time and Allocation Profiling Report  (Final)

           life21765TOK +RTS -p -RTS

        total time  =        0.03 secs   (29 ticks @ 1000 us, 1 processor)
        total alloc =  18,129,192 bytes  (excludes profiling overheads)

COST CENTRE        MODULE  %time %alloc

lifeStep.grouped   Main     55.2   42.0
lifeStep.neighbors Main     17.2   36.9
inRange            Main     13.8   11.7
main               Main      3.4    0.1
lifeStep           Main      3.4    3.2
lifeStep.inSize    Main      3.4    5.8
lifeStep.inR       Main      3.4    0.0
```

We've seen 5000 already, so

### 50 000

```
        Fri Dec 14 22:07 2012 Time and Allocation Profiling Report  (Final)

           life21765gYQ +RTS -p -RTS

        total time  =       15.94 secs   (15942 ticks @ 1000 us, 1 processor)
        total alloc = 11,262,873,192 bytes  (excludes profiling overheads)

COST CENTRE        MODULE    %time %alloc

lifeStep.grouped   Main       45.3   41.2
lifeStep.neighbors Main       23.0   37.8
inRange            Main       12.7   11.9
lifeStep           Main        6.6    3.0
lifeStep.viable    Main        5.9    0.0
lifeStep.inSize    Main        3.8    6.0
lifeStep.inR       Main        2.4    0.0
```

### 5 000 000

```
        Fri Dec 14 22:37 2012 Time and Allocation Profiling Report  (Final)

           big +RTS -p -RTS

        total time  =     1594.43 secs   (1594429 ticks @ 1000 us, 1 processor)
        total alloc = 1,125,606,873,896 bytes  (excludes profiling overheads)

COST CENTRE        MODULE    %time %alloc

lifeStep.grouped   Main       45.4   41.2
lifeStep.neighbors Main       23.6   37.8
inRange            Main       12.5   11.9
lifeStep           Main        6.2    3.0
lifeStep.viable    Main        5.8    0.0
lifeStep.inSize    Main        3.6    6.0
lifeStep.inR       Main        2.6    0.0
```

It's funny, after just clipping the board, we start getting *much* better numbers with unoptimized Haskell than we saw with unoptimized Common Lisp. That's not really much of a victory, since optimized lisp *was* handily beating the numbers we're putting down today, but it's also not the showdown I want to see. I want to know how optimized Haskell stacks up, and I want to know how Gridless Life stacks up to a gridded implementation. Back to [Rosetta Code](http://rosettacode.org/wiki/Conway%27s_Game_of_Life#Haskell), I guess. Second verse same as the first; added a grid-appropriate gun<a name="note-Fri-Dec-14-232656EST-2012"></a>[|1|](#foot-Fri-Dec-14-232656EST-2012) and stripped all but the final printing code.

```haskell
import Data.Array.Unboxed
import Data.List (unfoldr) 

type Grid = UArray (Int,Int) Bool
 -- The grid is indexed by (y, x).
 
life :: Int -> Int -> Grid -> Grid
{- Returns the given Grid advanced by one generation. -}
life w h old =
    listArray b (map f (range b))
  where b@((y1,x1),(y2,x2)) = bounds old
        f (y, x) = ( c && (n == 2 || n == 3) ) || ( not c && n == 3 )
          where c = get x y
                n = count [get (x + x') (y + y') |
                    x' &lt;- [-1, 0, 1], y' &lt;- [-1, 0, 1],
                    not (x' == 0 && y' == 0)]
 
        get x y | x &lt; x1 || x > x2 = False
                | y &lt; y1 || y > y2 = False
                | otherwise       = old ! (y, x)
 
count :: [Bool] -> Int
count = length . filter id

grid :: [String] -> (Int, Int, Grid)
grid l = (width, height, a)
  where (width, height) = (length $ head l, length l)
        a = listArray ((1, 1), (height, width)) $ concatMap f l
        f = map g
        g '.' = False
        g _   = True
 
printGrid :: Int -> Grid -> IO ()
printGrid width = mapM_ f . split width . elems
  where f = putStrLn . map g
        g False = '.'
        g _     = '#'
 
split :: Int -> [a] -> [[a]]
split n = takeWhile (not . null) . unfoldr (Just . splitAt n)

gosperGliderGun = grid
    ["........................#.........................",
     "......................#.#.........................",
     "............##......##............##..............",
     "...........#...#....##............##..............",
     "##........#.....#...##............................",
     "##........#...#.##....#.#.........................",
     "..........#.....#.......#.........................",
     "...........#...#..................................",
     "............##....................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     "..................................................",
     ".................................................."]

printLife :: Int -> (Int, Int, Grid) -> IO ()
printLife n (w, h, g) = printGrid w . last . take n $ iterate (life w h) g
 
main = printLife 50 gosperGliderGun
```

Ok, lets rev this sucker up.

### 50

```
        Fri Dec 14 22:29 2012 Time and Allocation Profiling Report  (Final)

           life-grid21765tiW +RTS -p -RTS

        total time  =        1.32 secs   (1319 ticks @ 1000 us, 1 processor)
        total alloc = 891,555,608 bytes  (excludes profiling overheads)

COST CENTRE MODULE  %time %alloc

life.get    Main     59.9   50.4
life.f.n    Main     30.5   41.7
life.f      Main      3.9    3.0
count       Main      3.5    0.8
life        Main      2.0    3.9
```

### 5000

```
        Fri Dec 14 22:32 2012 Time and Allocation Profiling Report  (Final)

           life-grid217656sc +RTS -p -RTS

        total time  =      133.77 secs   (133771 ticks @ 1000 us, 1 processor)
        total alloc = 90,810,516,640 bytes  (excludes profiling overheads)

COST CENTRE MODULE  %time %alloc

life.get    Main     59.1   50.5
life.f.n    Main     31.3   41.8
count       Main      3.5    0.8
life.f      Main      3.4    3.0
life        Main      2.4    3.9
```

That's ... almost sad enough not to be funny. Almost. Do note for the record that this is an *order of magnitude* up from the gridless version with the same inputs. And when you think about what's involved in each traversal of each corpus, it kind of becomes obvious why that is. The grids' corpus traversal *always* has 2500 stops. The gridless traversal is somewhere between 50 and 100 for a comparably populated board of the same size. 2500 is our *worst case*, and we'll probably never hit it.

I'm not even going to bother profiling the higher steps with this approach if 5000 took two minutes. I do still want to see how low we can go, and how we'd go about it.

The first thought I have is to try out that `iterate` approach, rather than recurring manually

```haskell
runLife :: Int -> Int -> Set (Int, Int) -> Set (Int, Int)
runLife worldSize steps cells = last . take steps $ iterate (lifeStep worldSize) cells

main :: IO ()
main = putStrLn . show $ runLife 50 50000 gosperGliderGun
```

Yes, it's more elegant. But will it blend?

```
        Fri Dec 14 22:45 2012 Time and Allocation Profiling Report  (Final)

           life21765UId +RTS -p -RTS

        total time  =        0.01 secs   (12 ticks @ 1000 us, 1 processor)
        total alloc =  20,022,728 bytes  (excludes profiling overheads)

COST CENTRE      MODULE  %time %alloc

runLife          Main     41.7   36.0
lifeStep.grouped Main     33.3   52.1
lifeStep         Main     25.0   11.7
```

Hm.

I'm gonna go ahead and put that one down to a profiler error, especially since running the same program in interactive mode confers no such magical acceleration. This does kind of call the process into question somewhat though...

Oh, well, I'm meant to be exploring. Lets pull the same incremental stuff we did with CL yesterday. Firstly, we're already using `Set` here, so the `memer` check is already as tight as it's going to get. Our last valid profiler ping told us that `lifeStep.grouped` is where the big costs are paid, so lets see if we can't reduce them somewhat.

```haskell
import qualified Data.Map as Map 
  (Map, lookup, insert, adjust, delete, fromList, toList)

frequencies :: [(Int, Int)] -> Map.Map (Int, Int) Int
frequencies list = rec list $ Map.fromList []
  where inc = Map.adjust (+1)
        rec [] m = m
        rec (cell:rest) m = rec rest newM
          where newM = if Nothing == Map.lookup cell m
                       then Map.insert cell 1 m
                       else inc cell m

lifeStep :: Int -> Set (Int, Int) -> Set (Int, Int)
lifeStep worldSize cells = fromList [fst g | g &lt;- grouped cells, viable g]
  where grouped = Data.List.filter viable . Map.toList . frequencies . concatMap neighbors . toList
        neighbors (x, y) = [(x+dx, y+dy) | dx &lt;- [-1..1], dy &lt;- [-1..1], 
                            (dx,dy) /= (0,0), inSize (dx+x) (dy+y)]
        inSize x y = inR x worldSize && inR y worldSize
        inR = inRange 0
        viable (_,3) = True
        viable (c,2) = c `member` cells
        viable _ = False
```

We've added a `Map` of frequencies, rather than doing the naive `group . sort` thing. We've also had to tweak viable just a bit to accomodate.

```
        Fri Dec 14 22:54 2012 Time and Allocation Profiling Report  (Final)

           life21765ucp +RTS -p -RTS

        total time  =        2.41 secs   (2406 ticks @ 1000 us, 1 processor)
        total alloc = 1,216,439,760 bytes  (excludes profiling overheads)

COST CENTRE          MODULE    %time %alloc

frequencies.rec.newM Main       41.2   17.4
lifeStep.neighbors   Main       16.6   35.2
frequencies.inc      Main       12.4   12.2
lifeStep.viable      Main        9.4    4.7
inRange              Main        8.1   11.1
lifeStep.grouped     Main        3.4    8.2
lifeStep             Main        3.4    2.8
lifeStep.inSize      Main        2.3    5.6
lifeStep.inR         Main        1.4    0.0
frequencies.rec      Main        1.3    2.8
```

That's ... hm. Actually an increase of about a second. Maybe it does comparatively better on bigger data-sets?

```haskell
main :: IO ()
main = putStrLn . show $ runLife 50 50000 gosperGliderGun
```

```
        Fri Dec 14 22:57 2012 Time and Allocation Profiling Report  (Final)

           life217657mv +RTS -p -RTS

        total time  =       23.96 secs   (23961 ticks @ 1000 us, 1 processor)
        total alloc = 12,100,319,760 bytes  (excludes profiling overheads)

COST CENTRE          MODULE    %time %alloc

frequencies.rec.newM Main       39.7   17.4
lifeStep.neighbors   Main       16.0   35.2
frequencies.inc      Main       13.3   12.2
lifeStep.viable      Main        9.5    4.7
inRange              Main        8.6   11.1
lifeStep.grouped     Main        3.6    8.2
lifeStep             Main        3.4    2.8
lifeStep.inSize      Main        2.6    5.6
lifeStep.inR         Main        1.8    0.0
frequencies.rec      Main        1.4    2.8
```

Nope. It actually does comparatively worse.

Hmmm.

I'm going to cut it here for now. I think I've done enough damage. I won't be putting the latest up<a name="note-Fri-Dec-14-232729EST-2012"></a>[|2|](#foot-Fri-Dec-14-232729EST-2012) for obvious reasons. Yes, I peeked ahead, which is why I knew this particular optimization wouldn't work in Haskell early enough to foreshadow it, but I still wanted to formalize my thoughts about it.

It's hard not to learn something from playing with a languages' profiler. This experience tells me that I might have the wrong model in my head, or it might be that predicting where a traversal will happen is a lot more difficult in lazy languages, *or*, as I suspect from the latest profiler readouts, it might be that a Haskell `Map`s' `lookup` speed isn't constant time. The reason I suspect this is that some of our biggest cost centers are now `frequencies.rec.newM` (which does a `Map.lookup` each call) and `frequencies.inc` (which manipulates a particular element of a `Map`, so I assume a lookup is part of it).

I'm off to read up on Haskell data structures and test these hypotheses.

Oh. And heal up.

* * *
##### Footnotes
1 - <a name="foot-Fri-Dec-14-232656EST-2012"></a>[|back|](#note-Fri-Dec-14-232656EST-2012) -  (by the way, this makes clear that whatever the performance comparisons come down to, the gridless version has a more elegant notation)

2 - <a name="foot-Fri-Dec-14-232729EST-2012"></a>[|back|](#note-Fri-Dec-14-232729EST-2012) -  (though the limited-size version and the gridded competitor will be checked in)
