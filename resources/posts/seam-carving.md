More reports from Code Retreat! This is getting to be a habit, though not an altogether unwelcome one. This month, we tackled [Seam Carving](en.wikipedia.org/wiki/Seam_carving), which is an algorithm to scale down a bitmap by taking out the "uninteresting"<a name="note-Wed-Oct-22-234842EDT-2014"></a>[|1|](#foot-Wed-Oct-22-234842EDT-2014) parts, leaving interesting points relatively unaffected.

The way it works in outline is:

1.   generate an "energy map" of the image, producing a single numeric score for each pixel
1.   use the above map to trace one-pixel paths from one end to the other<a name="note-Wed-Oct-22-234846EDT-2014"></a>[|2|](#foot-Wed-Oct-22-234846EDT-2014)
1.   remove the lowest cost path from the image

Repeat this as necessary for additional scaling. Steps 1 is not *quite* primitive, though they are quite simple for a toy example. In the simplest case, you compare a pixel to its neighbor and get the delta of their saturation<a name="note-Wed-Oct-22-234849EDT-2014"></a>[|3|](#foot-Wed-Oct-22-234849EDT-2014). If you want to do a bit better<a name="note-Wed-Oct-22-234852EDT-2014"></a>[|4|](#foot-Wed-Oct-22-234852EDT-2014), compare each neighbor rather than just one.

Step two is the meat of the algorithm. It works like this:

- start a new seam for each pixel in the first row of an image, tallying that pixels' cost
- for each other row of pixels, add each pixel in the row to the seam with the lowest tallied cost among its adjacent seams
- return all complete seams in ascending order of tallied cost

Each line eliminates some seams that continued from above; that's intentional, not accidental. We're not looking for all possible seams sorted by total cost, we want the seam with the lowest possible total cost *that's also cheaper at each step than its neighbors*.

### At The Event

It turns out that this isn't an easy problem to approach if this is your first time seeing it, so the code we wrote *at* the event mostly doesn't work very well.

My [first pairing](https://github.com/CodeRetreatTO/projects/blob/master/2014-10-seam-carving/csaunders-and-inaimathi.py) of the evening was with [csaunders](https://github.com/csaunders), and we tackled it in Python. In the interests of getting something workable together fast, we used [the `jp2a` utility](https://packages.debian.org/wheezy/jp2a) to turn the default [seam-carving castle photo](http://en.wikipedia.org/wiki/Seam_carving#mediaviewer/File:Broadway_tower_edit.jpg) into its [ascii equivalent representation](https://github.com/CodeRetreatTO/projects/blob/master/2014-10-seam-carving/scene.txt), then we worked on that. It might not have been too hard to find a bitmap parsing/manipulating library, but neither of us knew one off the top of our head. Also, by the time we got something workable together, we had something like ten minutes left in the session, so we decided to cut the image by naive columns rather than trying to find arbitrarily shaped seams. This, of course, left no time at all for cleanup, which is why this is the single fugliest piece of Python code my name will ever be attached to. God, I hope.

The [second "pairing"](https://github.com/CodeRetreatTO/projects/blob/master/2014-10-seam-carving/dann-josiah-and-inaimathi.lisp) was together with [Dann](https://github.com/dxnn) and Josiah<a name="note-Wed-Oct-22-234855EDT-2014"></a>[|5|](#foot-Wed-Oct-22-234855EDT-2014), and we forged ahead in Common Lisp. I'll admit to making a tweak or two on the way home, but we had the essentials of the algorithm down before breaking. Instead of indexing observed characters, this group decided to just use the numeric char codes representing each character. This meant the spread was a bit uneven, but it didn't seem to affect the quality of scaling much, if at all. We also got enough time to consider real seams, rather than just columns. The effectful approach we took ended up fighting us on it though. Consequently, we had to explicitly copy seams, where we should just have been functionally `cons`ing them up. To be fair, my focus was on presenting a mini guided tour of CLOS rather than keeping things pure...

### Later

...which is why I reached for Haskell when I [tried it out by myself](https://github.com/CodeRetreatTO/projects/blob/master/2014-10-seam-carving/inaimathi.hs) the following day. So lets finally get to some code I want to show, rather than hand-wave away.

```haskell
module SeamCarving where

import Data.Char (ord)
import Data.List (sortBy)
import Data.Function (on)

scoreGrid :: Image -> Score
scoreGrid ls = map scoreLine ls
    where scoreLine ln = map total zipped
              where l = map (toInteger . ord) ln
                    zipped = zip3 (head l:l) l $ tail l
                    total (a, b, c) = abs (a-b) + abs (b-c)

seamsIn :: Score -> [Seam]
seamsIn [] = []
seamsIn s = sortByWeight allSeams
    where allSeams = foldl line (freshSeams $ head s) $ tail s
          line seams ln = concatMap (choose seams) $ zip [0..] ln
          choose seams (ix, w) = map (\seam -> add seam w ix) . take 1 . sortByWeight $ potentials ix seams
          potentials ix seams = filter (\(Seam _ (i:_)) -> i >= ix-1 && ix+1 >= i) seams

scaleBy :: Image -> Int -> Image
scaleBy pic count = head $ drop count $ iterate scaleOne pic
    where scaleOne lns = case seamsIn $ scoreGrid lns of
                           [] -> lns
                           (cheapest:_) -> carveSeam lns cheapest

maskSeam :: Image -> Seam -> Image
maskSeam = applySeam (replace ' ')

carveSeam :: Image -> Seam -> Image
carveSeam = applySeam remove

freshSeams :: [Integer] -> [Seam]
freshSeams ln = map (\(ix, w) -> Seam w [ix]) $ zip [1..] ln

main :: IO ()
main = do f <- fmap lines $ readFile "scene.txt"
          mapM_ putBlock [ f
                         , scaleBy f 10
                         , scaleBy f 30
                         , scaleBy f 50 ]

----- Types and related minutia
type Score = [[Integer]]
type Image = [String]

data Seam = Seam { weight :: Integer, sIxs :: [Integer] } deriving (Eq, Ord, Show)

add :: Seam -> Integer -> Integer -> Seam
add (Seam w ixs) newWeight newIx = Seam (w + newWeight) $ newIx:ixs

indices :: Seam -> [Integer]
indices (Seam _ ixs) = reverse ixs

----- Utility
putBlock :: [String] -> IO ()
putBlock lns = do mapM_ putStrLn lns
                  putStrLn " "

applySeam :: (Integer -> String -> String) -> Image -> Seam -> Image
applySeam fn lns seam = map (\(ix, ln) -> fn ix ln) $ zip (indices seam) lns

sortByWeight :: [Seam] -> [Seam]
sortByWeight = sortBy (compare `on` weight)

remove :: (Num i, Enum i, Eq i) => i -> [a] -> [a]
remove _ [] = []
remove 0 (_:rest) = rest
remove ix (a:rest) = a : (remove (pred ix) rest)

replace :: (Num i, Enum i, Eq i) => a -> i -> [a] -> [a]
replace _ _ [] = []
replace new 0 (_:rest) = new:rest
replace new ix (a:rest) = a : (replace new (pred ix) rest)
```

We're going to skip the module and import boilerplate<a name="note-Wed-Oct-22-234904EDT-2014"></a>[|6|](#foot-Wed-Oct-22-234904EDT-2014), and get directly into the meat of things.

```haskell
scoreGrid :: Image -> Score
scoreGrid ls = map scoreLine ls
    where scoreLine ln = map total zipped
              where l = map (toInteger . ord) ln
                    zipped = zip3 (head l:l) l $ tail l
                    total (a, b, c) = abs (a-b) + abs (b-c)
```

You get a Score from an ascii Image by treating each character as its char code, and summing its difference from the character on the left with its difference from the character on the right.

```haskell
seamsIn :: Score -> [Seam]
seamsIn [] = []
seamsIn s = sortByWeight allSeams
    where allSeams = foldl line (freshSeams $ head s) $ tail s
          line seams ln = concatMap (choose seams) $ zip [0..] ln
          choose seams (ix, w) = map (\seam -> add seam w ix) . take 1 . sortByWeight $ potentials ix seams
          potentials ix seams = filter (\(Seam _ (i:_)) -> i >= ix-1 && ix+1 >= i) seams
```

You find the `Seam`s in a `Score`... well... by applying that algorithm I explained at the beginning of this article. You create a `Seam` for each element in the first line of the `Score`, then, for each character in each other line, add it to the cheapest adjacent `Seam`. You get the potential `Seam`s of a given index by `filter`ing any seam whose latest addition isn't within a radius of 1. You choose<a name="note-Wed-Oct-22-234910EDT-2014"></a>[|7|](#foot-Wed-Oct-22-234910EDT-2014) a seam for a given index to be added to by finding its potentials, sorting them by weight ascending, `take`ing `1`, and `add`ing this index and its weight to the resulting `Seam`. Note that this might result in no seams; if no `Seam` contains an adjacent index from the previous line, then this index won't be added anywhere. Processing a `line` involves processing each of its elements in the manner described two sentences ago. Finally, getting all relevant seams from a `Score` means so processing each line, starting with fresh seams generated from the first one, and sorting the result by weight.

```haskell
scaleBy :: Image -> Int -> Image
scaleBy pic count = head $ drop count $ iterate scaleOne pic
    where scaleOne lns = case seamsIn $ scoreGrid lns of
                           [] -> lns
                           (cheapest:_) -> carveSeam lns cheapest
```

Initially, I thought I'd be scaling an image by finding all seams, and taking the specified number of cheapest seams out in one step. After thinking about what happens in the edge case where you try to scale by a number greater than the available `Seam` count, I decided against it. It later occurred to me that removing a seam from an image might create new potential seams that are cheaper than any of the existing alternatives. At that point, I decided to chuck any semblance of efficiency in a bin and just scale by 1 `count` times. This means scoring the image, finding the cheapest seam *and* removing it, all at each step. It's still surprisingly snappy when you compile it under `-O2`.

Usually, I'd take this to its bloody conclusion by going through every excruciating line in the program. I'm going to cut it here this time, because the rest of it seems perfectly obvious once you understand the above.

And also, because I'm tired as fuck.

Correct me if I'm wrong about the former.

* * *
##### Footnotes

1 - <a name="foot-Wed-Oct-22-234842EDT-2014"></a>[|back|](#note-Wed-Oct-22-234842EDT-2014) -Although, as I've mentioned before, the originators of this algorithm have probably never taken a design course. If you understand basic layout principles, you understand that white space is [sometimes](http://www.shutterstock.com/video/clip-673189-stock-footage-wind-turbines-producing-clean-alternative-energy-in-barren-landscape.html?src=rel/671428:4) [critical](http://en.wikipedia.org/wiki/Think_Small) to the [impact](http://en.wikipedia.org/wiki/Pale_Blue_Dot#mediaviewer/File:Pale_Blue_Dot.png) of a piece, and that's exactly what this approach would prune down. Maybe that's an edge case that we'll always have humans looking at? I don't know. Plausible, at least. In any case, I'd argue that for at least some images, naive cropping and/or scaling would do a better job getting the point across than context-aware scaling.

2 - <a name="foot-Wed-Oct-22-234846EDT-2014"></a>[|back|](#note-Wed-Oct-22-234846EDT-2014) - Top-bottom if you're scaling horizontally, left-right if vertically.

3 - <a name="foot-Wed-Oct-22-234849EDT-2014"></a>[|back|](#note-Wed-Oct-22-234849EDT-2014) - Or their ascii values, if you're cheating mildly and not dealing directly with pixel data.

4 - <a name="foot-Wed-Oct-22-234852EDT-2014"></a>[|back|](#note-Wed-Oct-22-234852EDT-2014) - Though not *much* better, based on our admittedly limited observations.

5 - <a name="foot-Wed-Oct-22-234855EDT-2014"></a>[|back|](#note-Wed-Oct-22-234855EDT-2014) - Whose github profile I don't have a direct link to. I wrote it down on a scrap of paper, but Dann took it.

6 - <a name="foot-Wed-Oct-22-234904EDT-2014"></a>[|back|](#note-Wed-Oct-22-234904EDT-2014) -Incidentally, I've read an article draft semi-recently which made a convincing argument that imports *should not* be at the top of a file. Because it's very rarely the first thing a prospective reader is interested in. Unfortunately, as flexible as Haskell is in the declaration order of functions and `data`, it forces you to put the `module` and `import` statements right at the top of your file. So all I can really do is apologize and elide it in the write-up.S orry.

7 - <a name="foot-Wed-Oct-22-234910EDT-2014"></a>[|back|](#note-Wed-Oct-22-234910EDT-2014) - Which very probably isn't the best name for what's going on here.
