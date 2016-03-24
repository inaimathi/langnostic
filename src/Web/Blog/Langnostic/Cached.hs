{-# LANGUAGE OverloadedStrings #-}
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
