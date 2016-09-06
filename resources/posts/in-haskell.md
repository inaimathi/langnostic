I needed to blow off some steam over the last couple of weeks. And that principally happens through programming, so as of last weekend, this blog has semi-quietly been re-implemented in [Haskell](https://www.haskell.org/). Since then, I've been trying to go through some of my archives, fixing various spelling and grammar mistakes, as well as markup snafus that've crept in over the years. Most notably, there were [a](/posts/cl-notebook-introductory-thoughts) couple [of](/posts/cl-notebook-thoughts) articles about [`cl-notebook`](https://github.com/Inaimathi/cl-notebook) that just plain got lost in the shuffle.

So far, I've gotten through about 30 of 229 posts, and have come to the conclusion that I write entirely too fucking much. The plan was to spend a day or two fixing everything up, but there's no way in hell I can read, modify and push that much prose around. Especially since I've got [an AOSA chapter](https://github.com/Inaimathi/500lines) to nice down. So the fixes will be getting done piece-wise over the next few months. A bit at a time, every time I need to do something boring for an hour or two.

For the moment, let me just show off two pieces of the blog I found interesting to write.

## Automatic Section Titles

One of the benefits of using [`pandoc`](http://pandoc.org/) for document processing is that I can do certain things programmatically that I would have had to do manually with a `markdown`-specific library. Specifically, this new blog implementation automatically linkifies subheadings in posts. So the title above that reads "Automatic Section Titles"? You can click on that to go to that section, and you can post the resulting link to a forum in order to refer people directly to that section. The code that does this is reasonably simple.

```haskell
...
readPost :: FilePath -> IO Html
readPost fpath = do
  f <- readFile fpath
  return $ case readMarkdown def f of
    Right p -> writeHtml def $ walk linkedHeaders p
    _ -> error $ "Invalid post: " ++ fpath

linkedHeaders :: Block -> Block
linkedHeaders (Header n opts@(slug,_,_) content) = Header n opts linked
    where linked = (anchor slug) ++ [ Link ("",[],[]) content ("#" ++ slug, "") ]
linkedHeaders node = node

anchor :: String -> [Inline]
anchor name = [ RawInline (Format "html") ("<a name=\"" ++ name ++ "\">")
              , RawInline (Format "html") "</a>"]
...
```

`readPost` is the name of the function that reads a `markdown` file from disk. You'll notice that at one point it calls `walk linkedHeaders`. `linkedHeaders` is also defined above. It takes a block, and if that block is a `Header` of any rank, transforms it by adding an `anchor` and a `Link` to it. `anchor` is just a helper function to handle the specific task of generating the `RawInline` HTML anchor tag. The [`walk`](https://hackage.haskell.org/package/pandoc-types-1.16.1/docs/Text-Pandoc-Walk.html) function is what exposes the `Pandoc` syntax tree to external scripting. It will map the given function over a given `Pandoc` tree, and return the result.

I *used* to do the above manually. Which you *can* do because `markdown` accepts HTML markup inline. But it's not fun or easy to do, and I didn't actually remember to do it for each article. So, this way is better.

A similar feature I thought I'd have to put together myself, but which I actually get for free is the footnote system[^which-is].

[^which-is]: Which is provided out of the box by `Pandoc`'s multi-markdown support. It *doesn't* support recursive footnotes, and I'm one of those people fond of building intricate footnote mazes, so I might end up still needing to figure out a thing or two, but this certainly suffices for the moment.

## Post Caching

The second part is the file caching system.

```haskell
module Web.Blog.Langnostic.Cached ( Cache, readCache, newCache
                                  , CacheMap, newCacheMap, insert, hasKey, keys, fromList
                                  , Web.Blog.Langnostic.Cached.lookup
                                  , minutes, hours, zero ) where

import System.Time
import Data.IORef
import Data.Ratio
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

data Cached a = Cached { cachedHandle :: IO a
                       , cacheLimit :: TimeDiff
                       , lastChecked :: Maybe ClockTime
                       , value :: Maybe a }

type Cache a = IORef (Cached a)

newCache :: TimeDiff -> IO a -> IO (Cache a)
newCache limit handle = do
  newIORef $ Cached { cachedHandle = handle, cacheLimit = limit, value = Nothing, lastChecked = Nothing }

readCache :: Cache a -> IO a
readCache cache = do
  now <- getClockTime
  c <- readIORef cache
  case lastChecked c of
    Nothing -> readNew cache c now
    Just last -> let diff = now `diffClockTimes` last
                 in if (cacheLimit c) >= diff
                    then return $ fromJust $ value c
                    else readNew cache c now

bumpTime :: ClockTime -> Cached a -> Cached a
bumpTime now c = c { lastChecked = Just now }

bumpValue :: a -> Cached a -> Cached a
bumpValue v c = c { value = Just v }

readNew :: Cache a -> Cached a -> ClockTime -> IO a
readNew cache c now = do
  newVal <- cachedHandle c
  _ <- writeIORef cache . bumpValue newVal $ bumpTime now c
  return $ newVal

---------- Cache Map infrastructure
data CacheMap a b = CacheMap { mapHandle :: (b -> IO a)
                             , ref :: IORef (Map b (Cache a)) }

newCacheMap :: (b -> IO a) -> IO (CacheMap a b)
newCacheMap n = do
  r <- newIORef $ Map.empty
  return $ CacheMap { mapHandle = n, ref = r }

insert :: Ord b => CacheMap a b -> TimeDiff -> b -> IO (Cache a)
insert cacheMap diff name = do
  m <- readIORef (ref cacheMap)
  c <- newCache diff $ (mapHandle cacheMap) name
  _ <- writeIORef (ref cacheMap) $ Map.insert name c m
  return c

keys :: Ord b => CacheMap a b -> IO [b]
keys cacheMap = do
  c <- readIORef (ref cacheMap)
  return $ Map.keys c

hasKey :: Ord b => CacheMap a b -> b -> IO Bool
hasKey cacheMap k = do
  c <- readIORef (ref cacheMap)
  return $ case Map.lookup k c of
    Just _ -> True
    Nothing -> False

lookup :: Ord b => CacheMap a b -> b -> IO (Maybe a)
lookup cacheMap name = do
  m <- readIORef $ ref cacheMap
  case Map.lookup name m of
    Nothing -> return $ Nothing
    Just looked -> do c <- readCache looked
                      return $ Just c

fromList :: Ord b => TimeDiff -> (b -> IO a) ->  [b] -> IO (CacheMap a b)
fromList limit handle names = do
  cache <- newCacheMap handle
  _ <- mapM_ (\name -> insert cache limit name) names
  return $ cache

---------- Time utilities
epochToClockTime :: Real a => a -> ClockTime
epochToClockTime x =
    TOD seconds secfrac
    where ratval = toRational x
          seconds = floor ratval
          secfrac = floor $ (ratval - (seconds % 1) ) * picosecondfactor
          picosecondfactor = 10 ^ 12

minutes :: Int -> TimeDiff
minutes ms = zero { tdMin = ms }

hours :: Int -> TimeDiff
hours hs = zero { tdHour = hs }

zero :: TimeDiff
zero = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour = 0, tdMin = 0, tdSec = 0, tdPicosec = 0 }
```

You'll recall that I posted a similar piece of code [back when I re-wrote this blog in `go`](/posts/arbitrary-update-4701#new-blog-stuff). With some differences:

1. The `go` edition had a bunch of bugs that I didn't notice at the time, and which of course aren't caught by either type system
2. The Haskell version is able to handle polymorphic file types, rather than a generic `[]byte`, which is nice when you want to store a partially-processed file rather than the raw file input
3. The Haskell version handles `CacheMap`s directly
4. The Haskell version takes its `IO` action as an argument, so that it can also cache things like `TCP`/`HTTP` requests and not just file-reads (but more on that one later)

Lets go through this chunk-wise.

## Talking Through the Basic `Cache`

```haskell
module Web.Blog.Langnostic.Cached ( Cache, readCache, newCache
                                  , CacheMap, newCacheMap, insert, hasKey, keys, fromList
                                  , Web.Blog.Langnostic.Cached.lookup
                                  , minutes, hours, zero ) where

import System.Time
import Data.IORef
import Data.Ratio
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
...
```

Module declaration/import boilerplate. Sorry about that. Moving right along.

```haskell
data Cached a = Cached { cachedHandle :: IO a
                       , cacheLimit :: TimeDiff
                       , lastChecked :: Maybe ClockTime
                       , value :: Maybe a }

type Cache a = IORef (Cached a)
```

This is the meat of it. If you understand what the above says, you understand about three quarters of the machinery I'll be showing you shortly. A `Cached a` has a `cachedHandle`, which is an `IO` action we can run to get a thing of type `a`, a `cacheLimit`, which is how long we should wait between calls to the `cachedHandle`, a `lastChecked` which records when we last checked `cachedHandle` and a `value`, which is the thing we got last time we checked the `cachedHandle`.

A `Cache` of `a` is an `IORef`, which is to say pointer, to a `Cached a`. The idea being that we'll pass that `Cache` around, rather than the object directly.

```haskell
newCache :: TimeDiff -> IO a -> IO (Cache a)
newCache limit handle = do
  newIORef $ Cached { cachedHandle = handle, cacheLimit = limit, value = Nothing, lastChecked = Nothing }
```

We create a `newCache` by providing a `TimeDiff` and an `IO` action. A fresh `Cache` *does not read from its source*. This isn't necessarily a universal choice, so it's one of the things I'm thinking of generalizing; probably with a different constructor. For the purposes of my blog, I don't really want to waste memory on blog posts that no one has requested yet, so the initial read is deferred to the first request for it, rather than happening on server startup. Also, note that we're calling `newIORef`, which means our return value is in `IO`.

```haskell
readCache :: Cache a -> IO a
readCache cache = do
  now <- getClockTime
  c <- readIORef cache
  case lastChecked c of
    Nothing -> readNew cache c now
    Just last -> let diff = now `diffClockTimes` last
                 in if (cacheLimit c) >= diff
                    then return $ fromJust $ value c
                    else readNew cache c now

bumpTime :: ClockTime -> Cached a -> Cached a
bumpTime now c = c { lastChecked = Just now }

bumpValue :: a -> Cached a -> Cached a
bumpValue v c = c { value = Just v }

readNew :: Cache a -> Cached a -> ClockTime -> IO a
readNew cache c now = do
  newVal <- cachedHandle c
  _ <- writeIORef cache . bumpValue newVal $ bumpTime now c
  return $ newVal
```

In order to `readCache`, we dereference it and check the clock[^reverse-order]. Then we have some decisions to make. If we haven't run the `IO` action for this `Cache` yet, we `readNew`. If we *have* run it before, we check how long ago that was. If it's less than our `cacheLimit`, we just return the cached value. Otherwise, we `readNew` [^the-bug].

[^reverse-order]: We don't particularly care what order this happens in. And in fact, the code checks the clock before dereferencing. It'll change behavior very slightly to check the clock first, but not enough to matter for our purposes.

[^the-bug]: The bug I mentioned in the Go code was here, by the way. The way it was written meant that it waited for its `cacheLimit`, and then hit disk on every request rather than waiting for another `cacheLimit` to be hit. It's mildly frustrating that despite a strong type system, *and* using this blog as a sounding board, I managed to fuck it up that badly. I'm mentioning it explicitly because this is one of the reasons why I champion ML-style polymorphic type systems over flaming piles of ass like Go's. They let you write the code once, then use it in many different situations, rather than having to write it once per type of thing you want to deal with. When code gets gnarly enough, re-tailoring it for a slightly different situation incurs an unacceptably high risk of bugs. And simply allowing type-level variables gives you enough flexibility to avoid it more often than you might think.

The `readNew` helper function runs the `IO` action attached to this `Cache`, and `bump`s the associated `Value` and `Time`, so that `lastChecked` and `value` reflect their fresh contents.

That's it for basic `Cache`s. However, my particular use-case calls for lots of these. One per blog post, and staic `markdown` page, along with one for the post index.

## Talking Through `CacheMap`

```haskell
data CacheMap a b = CacheMap { mapHandle :: (b -> IO a)
                             , ref :: IORef (Map b (Cache a)) }
```

A `CacheMap` is a `mapHandle` and an `IORef` to a `Map b (Cache a)`. Which will make very little sense to you if you haven't read Haskell code before. Again, Haskell has type-level variables, which is what the `a` and `b` are above. You can think of `CacheMap` as a type-level function that takes two arguments, `a` and `b` that returns a record type with a `mapHandle` and a `ref`. The `mapHandle` is a value-level function that takes a thing of type `b` and returns a thing of type `IO a`. The `ref` is a pointer to a `Map` *(an associative k/v structure)* of `b` keys and `Cache a` values.

The reason that we need a `mapHandle` to return `IO` actions is that we want a different one per key we'll associate a `Cache` with. In our case, the argument we'll be passing `FilePath` as `b`, so that `mapHandle` will be responsible for taking a `FilePath` and creating an `IO` action that will return the semi-processed contents of the file it designates. Note that this *doesn't* have to be the case. That `b` can be anything we like, as long as the type equation works out. For instance, you could imagine making a `CacheMap ByteString Url` instead of a `CacheMap Html FilePath`.

And again, once you understand this declaration, you understand about 90% of what I'm about to show you implicitly.

```haskell
newCacheMap :: (b -> IO a) -> IO (CacheMap a b)
newCacheMap n = do
  r <- newIORef $ Map.empty
  return $ CacheMap { mapHandle = n, ref = r }
```

So, in order to create a `newCacheMap`, we need to provide it with a function that will take a `b` and return from it an `IO a`. We will get back an `IO (CacheMap a b)`. The body of this constructor is the obvious thing; we create a `newIORef` to an `empty` `Map`, and we `return` a `CacheMap` with the `mapHandle` we got as an argument, along with that pointer to an `empty` `Map`.

```haskell
insert :: Ord b => CacheMap a b -> TimeDiff -> b -> IO (Cache a)
insert cacheMap diff name = do
  m <- readIORef (ref cacheMap)
  c <- newCache diff $ (mapHandle cacheMap) name
  _ <- writeIORef (ref cacheMap) $ Map.insert name c m
  return c
```

We `insert` a new `Cache` into a `CacheMap` by providing a `TimeDiff` and a `b` *(again, `FilePath` in our case)*. The `TimeDiff` will be used as the `CacheLimit` for this particular `Cache`. I'm not entirely sure how much of a win this is; it might also be reasonable to provide a single `TimeDiff` to the constructor, rather than having to provide it at each `insert`ion. I do end up using different `TimeDiff`s for my static files than I do for my blog posts, but I'm not convinced this is the right solution for it.


```haskell
lookup :: Ord b => CacheMap a b -> b -> IO (Maybe a)
lookup cacheMap name = do
  m <- readIORef $ ref cacheMap
  case Map.lookup name m of
    Nothing -> return $ Nothing
    Just looked -> do c <- readCache looked
                      return $ Just c
```

In order to `lookup` a key in a `CacheMap`, we need the key. I mean, duh. We dereference the `CacheMap`'s `ref` slot, then do a `Map.lookup` into it. If we don't find anything we return `Nothing`, otherwise we `readCache` the `Cache` we just found and return `Just` the result. `readCache` involves doing all the `Cache` machinery we defined above.

That's basically it. The rest are utility functions, either for definition purposes or for external usability. And I'm pretty sure I missed on some.

```haskell
keys :: Ord b => CacheMap a b -> IO [b]
keys cacheMap = do
  c <- readIORef (ref cacheMap)
  return $ Map.keys c

hasKey :: Ord b => CacheMap a b -> b -> IO Bool
hasKey cacheMap k = do
  c <- readIORef (ref cacheMap)
  return $ case Map.lookup k c of
    Just _ -> True
    Nothing -> False

fromList :: Ord b => TimeDiff -> (b -> IO a) ->  [b] -> IO (CacheMap a b)
fromList limit handle names = do
  cache <- newCacheMap handle
  _ <- mapM_ (\name -> insert cache limit name) names
  return $ cache
```

I've defined a way to get all the `keys` out of a `CacheMap`, check if it `has` a particular `Key`, and a way of construction a `CacheMap` `from` a `List` of keys. `fromList` is currently unused, but I'm reasonably confident that'll change as I start refactoring pieces of my blog. `keys` and `hasKey` might be going away. `keys` was a first attempt at writing a bit of predicate code that ended up being better expressed by `hasKey`. It turns out that wasn't high-level enough either. What I really wanted was something like `insertNew`; something that would take a `b` and create a new `Cache` for it if one didn't already exist in the given `CacheMap`.

## Improvements

I'm thinking reasonably hard about this one because this is the one chunk of the blog I wrote that could probably stand to be pulled out into a separate library. o that end, I've been thinking about other use cases. Off the top of my head, [`kicktracker`](https://github.com/Inaimathi/kicktracker) could use something like it[^if-rewrite] for caching responses from the API it's wrapping. The use-case I've got for my blog could stand to be mildly more efficient too.

[^if-rewrite]: If I ever decide to re-write it in Haskell. Which sounds like a lot of fun, actually. It might happen the very next time I'm bored, or interested in [`wreq`](http://www.serpentine.com/wreq/tutorial.html). No promises though.

Firstly, the `IO` action in a `Cached` should probably be an `IO (Maybe a)` rather than merely `IO a`.

```haskell
data Cached a = Cached { cachedHandle :: IO a -- <- that
                       , cacheLimit :: TimeDiff
                       , lastChecked :: Maybe ClockTime
                       , value :: Maybe a }
```

The reason is that we sometimes don't want to return a new value. In the case of a cached HTTP request; maybe the latest request errored, and we want to keep the cached value rather than poisoning our cache. In the case of a cached file, it's possible that the file hasn't been edited since the last time we constructed our cached value. We'd want to be able to do something like `stat` the file first, to check its edited timestamp, and only return a fresh value if it has changed since we last checked.

Secondly, we might `cacheLimit` to be a bit more nuanced than a straight-up `TimeDiff`. Maybe something like a `TimeDiff -> TimeDiff` that takes the previous delay and returns a new one. This way we could express the constant delay that we're using now, as well as strategies like [exponential backoff](https://developers.google.com/api-client-library/java/google-http-java-client/backoff). I probably wouldn't want to expose that change directly to a user of the module though. I'd keep the current interface of `hours` and `minutes`, and add one or two variable strategies.

I'll leave both of those for next time though.
