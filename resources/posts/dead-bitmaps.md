I've been pouring some of my time into trying to [automatically translate bitmap images into richer flowchart representation](https://github.com/Inaimathi/EAF#eaf)<a name="note-Tue-Oct-28-230119EDT-2014"></a>[|1|](#foot-Tue-Oct-28-230119EDT-2014). What I want ultimately, is a way of going from an old-fashioned, hand-crafted line drawing to something that I could easily use to pull [this trick](/article?name=the-big-problem-and-visual-compilers.html). I don't quite have anything workable yet, but it feels like I'm making some progress regardless. Enough to blog about it for a bit, in any case.

### High Level

I'm considering two very specific approaches at the moment, but there's a general direction I'm trying to go in. First and foremost, I want something that works, and don't care *too* much about purity or elegance. Which means I'm perfectly willing to cheat by introducing easy to handle out-of-band information. One thing I'm already considering, but won't reach for until I need it, is color-coding lines/arrows differently from shapes. So, for instance, anything red would be interpreted as lines while anything blue would be processed under the assumption that it represented shapes<a name="note-Tue-Oct-28-230122EDT-2014"></a>[|2|](#foot-Tue-Oct-28-230122EDT-2014).

Secondly, I'm working with a very tight set of constraints on the source images I'm considering. These aren't going to be photographs. I admit, it would be cool to be able to draw a program in the sand, take a picture, and have it compile correctly, but that sounds ... hard. What I'll be dealing with to start with is going to be line drawings composed of primitive shapes, arrows and some sparse annotating text on a relatively uniform white background. Most of the work I've read on the subject of edge or feature detection has been aimed either at plain text nuder various transformations<a name="note-Tue-Oct-28-230127EDT-2014"></a>[|3|](#foot-Tue-Oct-28-230127EDT-2014), or at photographs<a name="note-Tue-Oct-28-230131EDT-2014"></a>[|4|](#foot-Tue-Oct-28-230131EDT-2014). The box and wire diagram area seems to be relatively unexplored.

### Thinning

The first approach I'm thinking about is thinning the image down to the minimal number of points it takes to represent. Once that's done, it should be possible to connect the dots at some threshold of proximity and generate the appropriate line/shape facts that we need in order to proceed further. And honestly, that's about as much thought as I've put into this approach. If you want details, check [the README scection](https://github.com/Inaimathi/EAF#thinning) and [`Ping.hs`](https://github.com/Inaimathi/EAF/blob/master/Ping.hs). I might come back to it if the other `Direction` proves fruitless, or hits some unseen roadblocks.

### Directional Mapping

This is the approach I've been considering and prototyping most vigorously. Mostly because it seems within striking distance of workable results in the very near future. At its most distilled form, this involves deciding the directional tendency of each pixel in the image. Each one might be one of the Cardinal directions (North/South or East/West), one of the Ordinal directions (NorthEast/SouthWest or NorthWest/SouthEast), or it might be contested<a name="note-Tue-Oct-28-230150EDT-2014"></a>[|6|](#foot-Tue-Oct-28-230150EDT-2014).

The algorithm for separating an image into these directional maps is relatively simple: for each pixel, count the contiguous filled space in each direction and sum the totals for equivalent directions.

```ocaml
scoreCoord :: Grid -> Coord -> Score
scoreCoord g (x, y) = Score (lengthI contigH) (lengthI contigV)
                      (lengthI contigSW) (lengthI contigSE)
    where contig xs ys = findContiguous g $ zip xs ys
          contigSW = concat [ contig [x, pred x..] [y, pred y..]
                            , contig [x..] [y..]]
          contigSE = concat [ contig [x..] [y, pred y..]
                            , contig [x, pred x..] [y..] ]
          contigH = concat [ contig [x..] $ repeat y
                           , contig [x,pred x..] $ repeat y]
          contigV = concat [ contig (repeat x) [y..]
                           , contig (repeat x) [y, pred y..]]

findContiguous :: Grid -> [Coord] -> [Coord]
findContiguous g cs = recur cs []
    where recur [] acc       = reverse acc
          recur (c:rest) acc = case Map.lookup c (gridMap g) of
                                 Nothing -> recur [] acc
                                 Just _ -> recur rest $ c:acc

ordinal :: Score -> Direction
ordinal (Score _ _ sw se) =
    case se `compare` sw of
      EQ -> C
      GT -> SW
      LT -> SE

cardinal :: Score -> Direction
cardinal (Score n e _ _) =
    case n `compare` e of
      EQ -> C
      GT -> H
      LT -> V

data Score = Score { north :: Integer, east :: Integer
                   , southWest :: Integer
                   , southEast :: Integer } deriving (Eq)
```

That's that. The `scoreCoord` function is admittedly more repetitive than it should be. I'm not sure this

```ocaml
scoreCoord :: Grid -> Coord -> Score
scoreCoord g (x, y) = Score contigH contigV contigSW contigSE
    where contig (xs, ys) = findContiguous g $ zip xs ys
          score = lengthI . concatMap contig
          contigSW = score [ ([x, pred x..], [y, pred y..])
                           , ([x..], [y..])]
          contigSE = score [ ([x..], [y, pred y..])
                           , ([x, pred x..], [y..]) ]
          contigH = score [ ([x..], repeat y)
                          , ([x,pred x..], repeat y)]
          contigV = score [ ((repeat x), [y..])
                          , ((repeat x), [y, pred y..])]
```

makes it any more readable, but it is equivalent, and doesn't repeat itself quite as frequently. The point is, this function takes a `Grid` and a `Coord` (which represents a pixel) and returns the score of that pixel in that `Grid`. `findContiguous` is a helper function used in the above.

```ocaml
findContiguous :: Grid -> [Coord] -> [Coord]
findContiguous g cs = recur cs []
    where recur [] acc       = reverse acc
          recur (c:rest) acc = case Map.lookup c (gridMap g) of
                                 Nothing -> recur [] acc
                                 Just _ -> recur rest $ c:acc
```

It takes a `Grid` and a list of `Coord`s, and takes only the first contiguous chunk; as soon as it fails to find a given coordinate, it terminates and returns the ones its accumulated so far. Having defined all of the above, you can then define

```ocaml
main :: IO ()
main = do g <- readSparse "test.txt"
          let showGrid = showMap (gridWidth g) (gridHeight g)
              score = scoreGrid g
          putBeside [ showGrid $ Map.map cardinal score
                    , showGrid $ Map.map ordinal score ]
```

and hop over to `GHCi`

```ocaml
Prelude> :load "Direction.hs"
[1 of 3] Compiling Util             ( Util.hs, interpreted )
[2 of 3] Compiling SparseRead       ( SparseRead.hs, interpreted )
[3 of 3] Compiling Direction        ( Direction.hs, interpreted )

Direction.hs:44:15: Warning:
    This binding for `showGrid' shadows the existing binding
      imported from `SparseRead' at Direction.hs:4:1-17
      (and originally defined at SparseRead.hs:33:1-8)
Ok, modules loaded: Util, Direction, SparseRead.
*Direction> main
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
   ||                          +    \/
  --------------------         +   \o\////\\\\///\\\\o/
-----------------------        + \\o\o///oo\\oo//o\\\oo/
 --|||-    ----   -|||--       +  \\o///    oooo   \\\o//
  o|||-           -|||-        +   \o///           o\\o/
  ||||             |||o        +   /\//             o\/o
   |||             |||         +    /oo             //\
  ||||            ||||         +   \//\            o///
  |||             ||||         +   o\/             /o//
 -|||o            ||||-        +  /o\\/            /////
 -|||o            ||||-        +  /o\\\            ///oo
 -|||--         --o|||-        +  /\\\\\         /////o\
  --------------------         +   /o\\\\\\\\o////////\
  ---------||-------           +   /o\\\\\\\\o///////
         -||||-                +          \\\///
          ||||-                +           \\///
          ||||                 +           /\/o
          ||||                 +           //\\
         -||||                 +          ////\
         -|||                  +          /o/o
   -o|    ||||-    |||         +    \\o    o\///    ///
   --oo  -||||    -o---        +    \\\o  \\///    /////
    o||   ||||   ------        +     \\\   oo/o   /////\
     ||o -||||  ------         +      \\\ //o\\  /////\
      ----|||o ----o           +       \\\//o\\ /////
       ---|||-----             +        /\\/\\\////
         -|||o---              +          \\o\////
          ||||-                +           \\///
           ||                  +            o\
        ---||----              +         ////\\\\\
    -------||-------           +     o//////oo\\\\\\/
   ||----     -----||          +    o/////     \\\\\\/
  |||--          -|||          +   o////          \\\\
  |||o            ||||         +   ////            \\\\
 o|||             o|||-        +  \///             \\\\/
 o|||              |||--       +  \\/o              \\o\o
 o|||              |||--       +  //\o              o/\\\
 o|||              |||o        +  /\\\              ///\
  |||-           --|||         +   \\\\           /////
  |||----     -----||-         +   /\\\\\\     ///////\
   -----------------           +    /\\\\\\\/////////
      -----------              +       \\\\\o/////
         ----                  +          \\o/
                               +
                               +
                               +
                               +

*Direction>
```

That transformation alone gets you some traction, although not quite enough. What I'd really want is a decision function that would give me the result of trying to merge them, rather than just two separate maps. The easiest thing I can think of is

```ocaml
decide :: Integer -> Score -> Direction
decide threshold s@(Score n e sw se)
    | ((n - threshold) > 0 || (e - threshold) > 0)
      && (abs $ n - e) > threshold = cardinal s
    | ((sw - threshold) > 0 || (se - threshold) > 0)
      && (abs $ sw - se) > threshold = ordinal s
    | otherwise = C
```

Which is to say, if either Cardinal scores are above a certain `threshold`, and one beats the other by said `threshold`, this is a Cardinal pixel. Same deal with the Ordinal scores. If nothing breaks the `threshold`, or if the breaking pair of scores doesn't point to a decisive winner, this is a Contested pixel. That then lets us redefine `main`

```ocaml
main :: IO ()
main = do g <- readSparse "test.txt"
          let showGrid = showMap (gridWidth g) (gridHeight g)
              score = scoreGrid g
              showScore = showGrid . flip Map.map score
          putBeside $ map showScore [cardinal, ordinal, decide 5]
```

which gives us the output

```ocaml
*Direction> :load "Direction.hs"
[1 of 3] Compiling Util             ( Util.hs, interpreted )
[2 of 3] Compiling SparseRead       ( SparseRead.hs, interpreted )
[3 of 3] Compiling Direction        ( Direction.hs, interpreted )

Direction.hs:44:15: Warning:
    This binding for `showGrid' shadows the existing binding
      imported from `SparseRead' at Direction.hs:4:1-17
      (and originally defined at SparseRead.hs:33:1-8)
Ok, modules loaded: Util, Direction, SparseRead.
*Direction> main
   ||                          +    \/                          +    ||
  --------------------         +   \o\////\\\\///\\\\o/         +   --------------------
-----------------------        + \\o\o///oo\\oo//o\\\oo/        + -----------------------
 --|||-    ----   -|||--       +  \\o///    oooo   \\\o//       +  oo||oo    oooo   o|||oo
  o|||-           -|||-        +   \o///           o\\o/        +   o||oo           o|||o
  ||||             |||o        +   /\//             o\/o        +   o||o             |||o
   |||             |||         +    /oo             //\         +    ||o             |||
  ||||            ||||         +   \//\            o///         +   o||o            o|||
  |||             ||||         +   o\/             /o//         +   o||             o|||
 -|||o            ||||-        +  /o\\/            /////        +  oo||o            o|||o
 -|||o            ||||-        +  /o\\\            ///oo        +  oo||\            /|||o
 -|||--         --o|||-        +  /\\\\\         /////o\        +  oo||o\         o/o|ooo
  --------------------         +   /o\\\\\\\\o////////\         +   --------ooo---------
  ---------||-------           +   /o\\\\\\\\o///////           +   -oo-----ooo------o
         -||||-                +          \\\///                +          \|||o/
          ||||-                +           \\///                +           |||/o
          ||||                 +           /\/o                 +           |||o
          ||||                 +           //\\                 +           |||o
         -||||                 +          ////\                 +          o|||\
         -|||                  +          /o/o                  +          /|||
   -o|    ||||-    |||         +    \\o    o\///    ///         +    \\o    |||oo    ///
   --oo  -||||    -o---        +    \\\o  \\///    /////        +    o\\o  o|||o    //ooo
    o||   ||||   ------        +     \\\   oo/o   /////\        +     o\\   |||o   //oooo
     ||o -||||  ------         +      \\\ //o\\  /////\         +      o\\ o|||o  //oooo
      ----|||o ----o           +       \\\//o\\ /////           +       o\-o|||o ///oo
       ---|||-----             +        /\\/\\\////             +        ---o||o----
         -|||o---              +          \\o\////              +          \|||/ooo
          ||||-                +           \\///                +           |||/o
           ||                  +            o\                  +            ||
        ---||----              +         ////\\\\\              +         ---||----
    -------||-------           +     o//////oo\\\\\\/           +     o------oo------o
   ||----     -----||          +    o/////     \\\\\\/          +    ooo///     \\\\ooo
  |||--          -|||          +   o////          \\\\          +   oo|//          \\|o
  |||o            ||||         +   ////            \\\\         +   o||/            \|\o
 o|||             o|||-        +  \///             \\\\/        +  oo||             o|ooo
 o|||              |||--       +  \\/o              \\o\o       +  oo||              |\ooo
 o|||              |||--       +  //\o              o/\\\       +  oo||              |oooo
 o|||              |||o        +  /\\\              ///\        +  oo||              |ooo
  |||-           --|||         +   \\\\           /////         +   o||\           o/|oo
  |||----     -----||-         +   /\\\\\\     ///////\         +   oooo\oo     ooo/oooo
   -----------------           +    /\\\\\\\/////////           +    -----------------
      -----------              +       \\\\\o/////              +       -----------
         ----                  +          \\o/                  +          \ooo
                               +                                +
                               +                                +
                               +                                +
                               +                                +

*Direction>
```

That third version can very nearly be naively converted to lines. The only real issues are


- There isn't much to disambiguate the circle from the square
- The arrow has horizontal breaks in its diagonal lines


The first two together might solve both, but introduce a new one; an ambiguity about where the break between the arrow line and the circle happens.

<pre>
    \/
   \o\////\\\\///\\\\o/
 \\o\o///oo\\oo//o\\\oo/
  \\o///    oooo   \\\o//
   \o///           o\\o/
   /\//             o\/o
    /oo             //\
   \//\            o///
   o\/             /o//
  /o\\/            /////
  /o\\\            ///oo
  /\\\\\         /////o\
   /o\\\\\\\\o////////\
   /o\\\\\\\\o///////
          \\\///
           \\///
           /\/o
           //\\
          ////\
          /o/o
    <span style="color: red;">\\</span>o    o\///    ///
    <span style="color: red;">\\\</span>o  \\///    /////
     <span style="color: red;">\\\</span>   oo/o   /////\
      <span style="color: red;">\\\</span> //o\\  /////\
       <span style="color: red;">\\\</span>//o\\ /////
        /<span style="color: red;">\\</span>/\\\////
          <span style="color: red;">\\</span>o\////
           <span style="color: red;">\\</span>///
            o<span style="color: red;">\</span>
         ////<span style="color: red;">\\\\\</span>
     o//////oo<span style="color: red;">\\\\\\</span>/
    o/////     <span style="color: red;">\\\\\\</span>/
   o////          <span style="color: red;">\\\\</span>
   ////            <span style="color: red;">\\\\</span>
  \///             <span style="color: red;">\\\\</span>/
  \\/o              <span style="color: red;">\\</span>o<span style="color: red;">\</span>o
  //\o              o/<span style="color: red;">\\\</span>
  /\\\              ///<span style="color: red;">\</span>
   \\\\           /////
   /\\\\\\     ///////\
    /\\\\\\\/////////
       \\\\\o/////
          \\o/

</pre>


That is one contiguous region, you see. And I don't see a way of breaking it without a priori knowledge. Except for the cheating approach. Specifically, if I started with the input

<pre>
    ..............................
    ....xx........................
    ...xxxxxxxxxxxxxxxxxxxx.......
    .xxxxxxxxxxxxxxxxxxxxxxx......
    ..xxxxxx....xxxx...xxxxxx.....
    ...xxxxx...........xxxxx......
    ...xxxx.............xxxx......
    ....xxx.............xxx.......
    ..xxxx............xxxx.......
    ...xxx.............xxxx.......
    ..xxxxx............xxxxx......
    ..xxxxx............xxxxx......
    ..xxxxxx.........xxxxxxx......
    ...xxxxxxxxxxxxxxxxxxxx.......
    ...xxxxxxxxooooxxxxxx.........
    ..........oooooo..............
    ...........ooooo..............
    ...........oooo...............
    ...........oooo...............
    ..........ooooo...............
    ..........oooo................
    ....ooo....ooooo....ooo.......
    ....oooo..ooooo....ooooo......
    .....ooo...oooo...oooooo......
    ......ooo.ooooo..oooooo.......
    .......oooooooo.ooooo.........
    ........ooooooooooo...........
    ..........oooooooo............
    ...........ooooo..............
    ............oo................
    .........xxxxxxxxx............
    .....xxxxxxxxxxxxxxxx.........
    ....xxxxxxx....xxxxxxx........
    ...xxxxx..........xxxx........
    ...xxxx............xxxx.......
    ..xxxx.............xxxxx......
    ..xxxx..............xxxxx.....
    ..xxxx..............xxxxx.....
    ..xxxx..............xxxx......
    ...xxxx...........xxxxx.......
    ...xxxxxxx.....xxxxxxxx.......
    ....xxxxxxxxxxxxxxxxx.........
    .......xxxxxxxxxxx............
    ..........xxxx................
    ..............................
    ..............................
    ..............................
</pre>

it would be a fairly simple matter to do the arrow/shape separation first, and then figure out how each shape breaks down in the Cardinal/Ordinal sense. What I'd be looking for at that point is to see which of the two maps yielded full coverage of a particular area with the fewest lines. Which is how I could tell that

<pre>
   ||                          +    \/
  --------------------         +   \o\////\\\\///\\\\o/
-----------------------        + \\o\o///oo\\oo//o\\\oo/
 --|||-    ----   -|||--       +  \\o///    oooo   \\\o//
  o|||-           -|||-        +   \o///           o\\o/
  ||||             |||o        +   /\//             o\/o
   |||             |||         +    /oo             //\
  ||||            ||||         +   \//\            o///
  |||             ||||         +   o\/             /o//
 -|||o            ||||-        +  /o\\/            /////
 -|||o            ||||-        +  /o\\\            ///oo
 -|||--         --o|||-        +  /\\\\\         /////o\
  --------------------         +   /o\\\\\\\\o////////\
  ---------||-------           +   /o\\\\\\\\o///////
</pre>

this is meant to be a Cardinal shape; it can be drawn with four cardinal lines<a name="note-Tue-Oct-28-230220EDT-2014"></a>[|7|](#foot-Tue-Oct-28-230220EDT-2014), whereas the ordinal map specifies between 6 and 8 depending specifically on where you set the threshold for recognizing a line<a name="note-Tue-Oct-28-230224EDT-2014"></a>[|8|](#foot-Tue-Oct-28-230224EDT-2014). This also has the side benefit of disambiguating curves/circles from squares/lines;


<pre>
        ---||----              +         ////\\\\\
    -------||-------           +     o//////oo\\\\\\/
   ||----     -----||          +    o/////     \\\\\\/
  |||--          -|||          +   o////          \\\\
  |||o            ||||         +   ////            \\\\
 o|||             o|||-        +  \///             \\\\/
 o|||              |||--       +  \\/o              \\o\o
 o|||              |||--       +  //\o              o/\\\
 o|||              |||o        +  /\\\              ///\
  |||-           --|||         +   \\\\           /////
  |||----     -----||-         +   /\\\\\\     ///////\
   -----------------           +    /\\\\\\\/////////
      -----------              +       \\\\\o/////
         ----                  +          \\o/
</pre>


Curves are areas where the cardinal and ordinal approaches tie for efficiency. There is another issue though.

<pre>
       ||||          +        ---------        +    ||
      -|||o-         +    ----------------     +   --------------------
       ||||-         +   ||----     -----||    + -----------------------
       ||||          +  |||--          -|||    +  --|||-    ----   -|||--
       ||||          +  |||o            ||||   +   o|||-           -|||-
      -||||          + o|||             o|||-  +   ||||             |||o
      -|||           + o|||              |||-- +    |||             |||
-o|    ||||-    |||  + o|||              |||-- +   ||||            ||||
--oo  -||||    -o--- + o|||              |||o  +   |||             ||||
 o||   ||||   ------ +  |||-           --|||   +  -|||o            ||||-
  ||o -||||  ------  +  |||----     -----||-   +  -|||o            ||||-
   ----|||o ----o    +   -----------------     +  -|||--         --o|||-
    ---|||-----      +      -----------        +   --------------------
      -|||o---       +         ----            +   -||-----    ----||
       ||||-         +                         +
        ||           +                         +

       \\o/          +        ///o\\\\\        +    \/
      \\\///         +    o/////ooo\\\\\\/     +   \o\////\\\\///\\\\o/
       \o///         +   o/////     o\\\\\/    + \\o\o///oo\\oo//o\\\oo/
       /o//          +  o////          \\\\    +  \\o///    oooo   \\\o//
       //o/          +  ////            \\\\   +   \o///           o\\o/
      o///\          + \///             \\\\/  +   /\//             o\/o
      /o/o           + \\/o              \\o\o +    /oo             //\
\\o    o\///    ///  + //\o              o/\\\ +   o//\            o///
\\\o  \\///    ///// + /\\\              ///\  +   o\/             /o//
 \\\   oo/o   /////\ +  \\\\           /////   +  /o\\/            /////
  \\\ //o\\  /////\  +  /\\\\\\     ///////\   +  /o\\\            ///oo
   \\\//o\\ /////    +   /\\\\\\\/////////     +  /\\\\\         o////o\
    /\\/\\\////      +      \\\\\o/////        +   /o\\\\o//oo\\//////\
      \\o\////       +         \\o/            +   /o\\\\\o    //////
       \\///         +                         +
        oo           +                         +
</pre>


If we go this breaking shape direction, we suddenly have the problem of how to compose the arrow. Ideally, we'd want the large vertical from the Cardinal map, and the two side diagonals from the Ordinal map. Which means we're not really checking which map gives us better coverage, we're trying to find maximum coverage of some number of coordinates by several distinct, but connected areas on a map. Which sounds like it lands us in [difficult-problem territory](https://en.wikipedia.org/wiki/Maximum_coverage_problem). The good news is that approximation is very probably good enough for what we're doing here, and I can think of one or two half-way decent ways of doing the needed comparisons quickly enough<a name="note-Tue-Oct-28-230232EDT-2014"></a>[|9|](#foot-Tue-Oct-28-230232EDT-2014).

Anyhow, that's all I've got for now. If you want to see the up-to-the-minute details on how this progresses, keep an eye on [the github](https://github.com/Inaimathi/EAF).

I'll keep you posted on how this develops.

* * *
##### Footnotes

1 - <a name="foot-Tue-Oct-28-230119EDT-2014"></a>[|back|](#note-Tue-Oct-28-230119EDT-2014) - In case you're wondering, "EAF" stands for "Edgy As Fuck", which is the first thing that popped into my head when I thought about what I should name an edge-detecting project. I guess that might say something about me.

2 - <a name="foot-Tue-Oct-28-230122EDT-2014"></a>[|back|](#note-Tue-Oct-28-230122EDT-2014) - Anything purple can be processed as an overlapping area, and essentially added to both the lines and shapes corpus. Black could potentially be text, but once we have the lines and shapes sussed out, there gets to be a relatively small possibility space where there might be text, and it seems like it would be easy enough to just take any occupied area within that space and pass it through some standard OCR software.

3 - <a name="foot-Tue-Oct-28-230127EDT-2014"></a>[|back|](#note-Tue-Oct-28-230127EDT-2014) - Which is part of what I'm doing, but not the core, so I feel no shame whatsoever about punting it to something like [`tesseract`](http://code.google.com/p/tesseract-ocr/).

4 - <a name="foot-Tue-Oct-28-230131EDT-2014"></a>[|back|](#note-Tue-Oct-28-230131EDT-2014) - Which, as I've mentioned, I'm not tackling<a name="note-Tue-Oct-28-230135EDT-2014"></a>[|5|](#foot-Tue-Oct-28-230135EDT-2014).

5 - <a name="foot-Tue-Oct-28-230135EDT-2014"></a>[|back|](#note-Tue-Oct-28-230135EDT-2014) - Yet.

6 - <a name="foot-Tue-Oct-28-230150EDT-2014"></a>[|back|](#note-Tue-Oct-28-230150EDT-2014) - That is, it might be the case that no particular directional tendency exceeds a given threshold, *or* it might be the case that no single direction is the winner by a number exceeding that threshold.

7 - <a name="foot-Tue-Oct-28-230220EDT-2014"></a>[|back|](#note-Tue-Oct-28-230220EDT-2014) - Minus some *very* low-threshold flash.

8 - <a name="foot-Tue-Oct-28-230224EDT-2014"></a>[|back|](#note-Tue-Oct-28-230224EDT-2014) - I could also get fancy and start doing shape comparisons here; we could take a look at the cardinal map and discount it on the basis that two of its "lines" are L-shaped. We could use this information either to impose a penalty, or to discount the ordinal breakdown entirely.

9 - <a name="foot-Tue-Oct-28-230232EDT-2014"></a>[|back|](#note-Tue-Oct-28-230232EDT-2014) - Not that *any* approach would be slow on a corpus this size. The real test is going to happen when I try running this compiler over my first 300 dpi, 11x17 scan. I'm guessing performance-oriented tweakery will be necessary, but I've been wrong before.
