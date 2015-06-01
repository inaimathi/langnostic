Before we get into the [Criterion](http://hackage.haskell.org/package/criterion-0.6.0.0) benchmarking library, I guess I should actually fulfill the function of this journal of mine every so often. This is entirely unrelated to programming, so skip to the next heading if you want to get directly to benching examples.

### <a name="ponies"></a>Ponies

Before the last little while, I could have recommended [it](http://www.youtube.com/watch?v=zy6ELvVdgh4&list=PLFF6671BFA09E4AD8) to you without reservations.

However, apparently some Hasbro exec wasn't happy to leave well enough alone? So Season 3, which concluded this past Saturday was both a) half as long as usual and b) very hit-or-miss.

The finale in particular was excruciating. Not that the animators and writers didn't do their best, I guess, but it's pretty obvious that this episode was rushed as fuck and not at all what they were planning for the ending of the season<a name="note-Sun-Feb-17-224001EST-2013"></a>[|1|](#foot-Sun-Feb-17-224001EST-2013). I actually had to stop watching because my eye-rolling intensity was ramping up at each scene. You *really* have to work to kill my willing suspension of disbelief in any cartoon, let alone this one. That should tell you something. My wife is a more hardcore fan, having started in on ponies at G1, back when the fandom was almost exclusively female. She managed to finish out the episode, then she cried for a while. If she wasn't still nursing, I have no doubt that she'd have pulled out the scotch. That should tell you the same thing, but in 76-point, bold, condensed type.

So anyway, instead of

Go watch ponies, they're quite good. 
> -Inaimathi

my recommendation now has to look something like

Go watch ponies, they're quite good. But avoid episodes 10, 11, 12 and 13 of season 3. And actually, S3E3 and S3E4 weren't up to par either. And S3E9 has some annoyingly out of character behavior. And while otherwise excellent, the season 3 premiere foreshadows some interesting character development and story hooks that never got followed up on, so don't watch that unless you're ok with getting no closure at all. 
> -Inaimathi

That's ... less than a ringing endorsement, but I guess I'll stick around to see what they can pull off in season 4<a name="note-Sun-Feb-17-224008EST-2013"></a>[|2|](#foot-Sun-Feb-17-224008EST-2013). In the meantime, there are some fan projects that look really good<a name="note-Sun-Feb-17-224032EST-2013"></a>[|4|](#foot-Sun-Feb-17-224032EST-2013). [Slice of Life](http://sliceofponylife.tumblr.com/) is a tumblr based webcomic that honestly looks like what the next step for the official media *should* be; a simple, character-driven story of some minor characters introduced over the course of the first two seasons<a name="note-Sun-Feb-17-224036EST-2013"></a>[|5|](#foot-Sun-Feb-17-224036EST-2013). Also, there's [Turnabout Storm](http://www.youtube.com/playlist?list=PL347AD9B9E509804A), a fan-made FiM/Phoenix Wright crossover which treats both source series fairly respectfully. That might just be my wife's and my weird tastes showing, but we're both heavily invested.

### <a name="haskell"></a>Haskell

Right, back to the subject at hand. Last week, I finally got out to [the Haskell group](http://hacklab.to/archives/toronto-haskell-users-meetup/). They don't always meet up, and when they do it's usually in the second Wednesday of the month<a name="note-Sun-Feb-17-224040EST-2013"></a>[|6|](#foot-Sun-Feb-17-224040EST-2013), but this month, they met on a Thursday that wasn't otherwise occupied for me.

First impressions are that I have no idea what the fuck I'm doing.

These guys are far enough beyond me in terms of math chops that I couldn't follow all of the conversation happening. I know it's a good thing to periodically be the dumbest guy in the room, but it doesn't feel good while it's happening. Anyhow, we had a long-ish presentation on [Arrows](http://www.haskell.org/arrows/) and their implications followed by some free-form discussion. One of the things I picked up was the question of how [Acid-State](http://hackage.haskell.org/package/acid-state-0.8.2) compares performance-wise to [other](http://hackage.haskell.org/package/mongoDB) data [back-ends](http://hackage.haskell.org/package/HDBC) available, and another was mention of [Criterion](http://hackage.haskell.org/package/criterion-0.6.0.0). The first is something I aim to get to next time, the second is a profiling library for Haskell that doesn't require you to go through any of that [GHC compilation flag bullshit I took you through last time](http://langnostic.blogspot.ca/2012/12/not-optimizing-haskell.html). So I figured I'd crack it open and see if it can provide decent output for me.

### <a name="profiling-haskell-with-criterion"></a>Profiling Haskell with Criterion

Criterion doesn't work on the same level as the GHC profiler. Specifically, it works on individual functions rather than complete programs. It lets you specify labels and benchmark groups, and it takes your hardware into consideration. In concrete terms, lets take that Life module from [a while ago](http://langnostic.blogspot.ca/2012/12/not-optimizing-haskell.html) for a spin.

```haskell
module Life where
import Data.List (group, sort, concatMap)
import Data.Set

inRange :: Ord a => a -> a -> a -> Bool
inRange low n high = low &lt; n && n &lt; high

lifeStep :: Integer -> Set (Integer, Integer) -> Set (Integer, Integer)
lifeStep worldSize cells = fromList [head g | g &lt;- grouped cells, viable g]
  where grouped = group . sort . concatMap neighbors . toList
        neighbors (x, y) = [(x+dx, y+dy) | dx &lt;- [-1..1], dy &lt;- [-1..1], 
                            (dx,dy) /= (0,0), inSize (dx+x) (dy+y)]
        inSize x y = inR x worldSize && inR y worldSize
        inR = inRange 0
        viable [_,_,_] = True
        viable [c,_] = c `member` cells
        viable _ = False

runLife :: Integer -> Integer -> Set (Integer, Integer) -> Set (Integer, Integer)
runLife worldSize steps cells = rec (steps - 1) cells
  where rec 0 cells = cells
        rec s cells = rec (s - 1) $! lifeStep worldSize cells

glider = fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = fromList [(1, 0), (1, 1), (1, 2)]
gosperGliderGun = fromList [(24, 0), (22, 1), (24, 1), (12, 2), (13, 2), (20, 2), (21, 2), (34, 2), (35, 2), (11, 3), (15, 3), (20, 3), (21, 3), (34, 3), (35, 3), (0, 4), (1, 4), (10, 4), (16, 4), (20, 4), (21, 4), (0, 5), (1, 5), (10, 5), (14, 5), (16, 5), (17, 5), (22, 5), (24, 5), (10, 6), (16, 6), (24, 6), (11, 7), (15, 7), (12, 8), (13, 8)]

main :: IO ()
main = putStrLn . show $ runLife 50 5000 gosperGliderGun
```

The `Critreion` benching specification for that would look something like

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main
import qualified Life

main = defaultMain [
  bgroup "lifeStep" [
     bench "Gun" $ whnf step Life.gosperGliderGun,
     bench "Glider" $ whnf step Life.glider,
     bench "Blinker" $ whnf step Life.blinker
     ],
  bgroup "runLife" [ 
     bench "Gun" $ whnf run Life.gosperGliderGun,
     bench "Glider" $ whnf run Life.glider,
     bench "Blinker" $ whnf run Life.blinker
     ],
  bgroup "main" [
    bench "with IO" $ whnfIO Life.main
    ]
  ]
  where step = Life.lifeStep 50
        run = Life.runLife 50 5000
```

Take note of a few things in here. First, `bgroup` is of type `String -> [Benchmark] -> Benchmark`, which means that you can nest them; I just don't. Second, because you actually have to pass the functions you bench this way, you obviously can't measure internal definitions; you'll need to pull those pieces out to the top-level in order to figure out how much time they're taking. Third, `whnf` actually takes the target function and its last argument as separate arguments. [The documentation](http://hackage.haskell.org/packages/archive/criterion/latest/doc/html/Criterion-Main.html) says this is to prevent `bench` calls themselves from being optimized away by `ghc -O`, which would be less than ideal for obvious reasons. The output of the above will be something like

```haskell
warming up
estimating clock resolution...
mean is 2.233879 us (320001 iterations)
found 52279 outliers among 319999 samples (16.3%)
  158 (4.9e-2%) low severe
  52121 (16.3%) high severe
estimating cost of a clock call...
mean is 53.36260 ns (14 iterations)

benchmarking lifeStep/Gun
mean: 1.741280 ms, lb 1.737541 ms, ub 1.745301 ms, ci 0.950
std dev: 19.92180 us, lb 17.54283 us, ub 24.73115 us, ci 0.950

benchmarking lifeStep/Glider
mean: 202.0392 us, lb 201.8147 us, ub 202.3017 us, ci 0.950
std dev: 1.240524 us, lb 1.058166 us, ub 1.716059 us, ci 0.950

benchmarking lifeStep/Blinker
mean: 113.2888 us, lb 113.1514 us, ub 113.4549 us, ci 0.950
std dev: 775.2598 ns, lb 649.4014 ns, ub 1.132731 us, ci 0.950

benchmarking runLife/Gun
collecting 100 samples, 1 iterations each, in estimated 539.4134 s
mean: 5.436085 s, lb 5.427699 s, ub 5.447670 s, ci 0.950
std dev: 50.19115 ms, lb 38.42313 ms, ub 67.52400 ms, ci 0.950

benchmarking runLife/Glider
mean: 21.11409 ms, lb 20.93418 ms, ub 21.53549 ms, ci 0.950
std dev: 1.325480 ms, lb 478.7184 us, ub 2.322194 ms, ci 0.950
found 5 outliers among 100 samples (5.0%)
  3 (3.0%) high mild
  2 (2.0%) high severe
variance introduced by outliers: 59.525%
variance is severely inflated by outliers

benchmarking runLife/Blinker
mean: 19.77626 ms, lb 19.74970 ms, ub 19.81207 ms, ci 0.950
std dev: 157.6956 us, lb 125.2292 us, ub 204.1448 us, ci 0.950

benchmarking main/with IO
fromList [(3,5),(4,4),(4,6),(5,4),(5,6),(6,5),(17,3),(17,4),(18,3),(18,4),(22,4),(22,5),(23,3),(23,6),(24,4),(24,6),(25,5),(34,2),(34,3),(35,2),(35,3)]
collecting 100 samples, 1 iterations each, in estimated 547.4592 s
fromList [(3,5),(4,4),(4,6),(5,4),(5,6),(6,5),(17,3),(17,4),(18,3),(18,4),(22,4),(22,5),(23,3),(23,6),(24,4),(24,6),(25,5),(34,2),(34,3),(35,2),(35,3)]

&lt;snip a fuckton of duplicates />

fromList [(3,5),(4,4),(4,6),(5,4),(5,6),(6,5),(17,3),(17,4),(18,3),(18,4),(22,4),(22,5),(23,3),(23,6),(24,4),(24,6),(25,5),(34,2),(34,3),(35,2),(35,3)]
mean: 772.6040 us, lb 753.6831 us, ub 785.0876 us, ci 0.950
std dev: 77.70160 us, lb 51.99922 us, ub 110.7696 us, ci 0.950
found 8 outliers among 100 samples (8.0%)
  5 (5.0%) low severe
  2 (2.0%) high severe
variance introduced by outliers: 79.011%
variance is severely inflated by outliers
```

You can apparently generate shiny HTML/JS-based performance reports, judging by what I'm reading [here](https://github.com/bos/criterion/blob/master/Criterion/Report.hs), but I haven't bothered to. Basically what this library does, which you can see by that output, is take each function you pass it and try it a bunch of times, then hand you stats on how long it took. It *doesn't* give you any kind of memory usage information, and won't give you information on sub-expressions. On the other hand, you don't need to dick around with installing the profiling versions of any library, or any of GHC's profiling flags, and you get to be more surgical about it *and* this compiles results from a bunch of trials<a name="note-Sun-Feb-17-224056EST-2013"></a>[|7|](#foot-Sun-Feb-17-224056EST-2013) which gives me slightly more confidence in the results.

So, I mean, pros and cons.

Not having to grub around my `.cabal` config<a name="note-Sun-Feb-17-224100EST-2013"></a>[|8|](#foot-Sun-Feb-17-224100EST-2013) sounds like a good thing, and I typically care a lot more about execution time than memory usage in the applications I write. So [this](https://github.com/bos/criterion) looks like an all-round better way to benchmark than the default.

Tune in next time, when I'll probably put this tool to use in comparing some of the database options I have in Haskell.

* * *
##### Footnotes

1 - <a name="foot-Sun-Feb-17-224001EST-2013"></a>[|back|](#note-Sun-Feb-17-224001EST-2013) - Since there was no mention of this "Equestria Games" thing they were building up for three episodes.

2 - <a name="foot-Sun-Feb-17-224008EST-2013"></a>[|back|](#note-Sun-Feb-17-224008EST-2013) - My wife's theory is that I shouldn't bother. She thinks the studio has been giving FiM less attention because they're preparing to ramp up on [Equestria Girls](http://mlp.wikia.com/wiki/Equestria_Girls_(spinoff)); another spin-off show for kids about chicks in high-school<a name="note-Sun-Feb-17-224017EST-2013"></a>[|3|](#foot-Sun-Feb-17-224017EST-2013). It's plausible, but I'll still reserve judgement.
3 - <a name="foot-Sun-Feb-17-224017EST-2013"></a>[|back|](#note-Sun-Feb-17-224017EST-2013) - Incidentally, my wife adds "Booooo!".

4 - <a name="foot-Sun-Feb-17-224032EST-2013"></a>[|back|](#note-Sun-Feb-17-224032EST-2013) - Except that [they're happening in a parallel universe now](http://tvtropes.org/pmwiki/pmwiki.php/Main/FanonDiscontinuity)<a name="note-Mon-Feb-18-011321EST-2013"></a>[|9|](#foot-Mon-Feb-18-011321EST-2013) or whatever, because Twilight Sparkle is not a unicorn anymore.

5 - <a name="foot-Sun-Feb-17-224036EST-2013"></a>[|back|](#note-Sun-Feb-17-224036EST-2013) - Incidentally, if anyone from Hasbro is reading; one way you could correct course at this point is by walking over to [egophiliac](http://egophiliac.deviantart.com/)'s house with a giant cheque, asking her what number she'd like on it, then putting her in charge of the writing and/or editing staff.

6 - <a name="foot-Sun-Feb-17-224040EST-2013"></a>[|back|](#note-Sun-Feb-17-224040EST-2013) - Which conflicts with my workout schedule.

7 - <a name="foot-Sun-Feb-17-224056EST-2013"></a>[|back|](#note-Sun-Feb-17-224056EST-2013) - The number of which you can specify by using `defaultMainWith` instead of `defaultMain` and passing it a potentially modified `[defaultConfig](https://github.com/bos/criterion/blob/master/Criterion/Config.hs#L73-L89)`.

8 - <a name="foot-Sun-Feb-17-224100EST-2013"></a>[|back|](#note-Sun-Feb-17-224100EST-2013) - Or re-install half my libraries later.

9 - <a name="foot-Mon-Feb-18-011321EST-2013"></a>[|back|](#note-Mon-Feb-18-011321EST-2013) - Actually, after I wrote this, I went over to check out the Slice of Life archives and it turns out that egophiliac has been going back and revising history as new stuff gets added to the show. For instance, Twilight now has added wings and there was a slight conversational change between Pound Cake and Scootaloo. My theory is that she hasn't updated more frequently lately because she's been too busy changing her archives in response to Hasbro's various wankery.
