{-# LANGUAGE OverloadedStrings #-}
module Cached ( Cache, readCache, newCache
              , CacheMap, newCacheMap, insert, Cached.lookup
              , minutes, hours ) where

import System.Time
import System.Directory
import System.Posix.Files
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Data.Ratio

import Control.Monad

import Data.Maybe (fromJust)

import Data.Map (Map)
import qualified Data.Map as Map

data Cached a = Cached { reader :: (FilePath -> IO a)
                       , file :: FilePath
                       , cacheLimit :: TimeDiff
                       , lastChecked :: Maybe ClockTime
                       , value :: Maybe a }

type Cache a = IORef (Cached a)

newCache :: TimeDiff -> (FilePath -> IO a) -> FilePath -> IO (Cache a)
newCache cacheLimit reader fname = do
  val <- reader fname
  newIORef $ Cached { reader = reader, file = fname, cacheLimit = cacheLimit, value = Nothing, lastChecked = Nothing }

readCache :: Cache a -> IO a
readCache cache = do
  now <- getClockTime
  c <- readIORef cache
  case lastChecked c of
    Nothing -> readNew cache c now
    Just last -> let diff = now `diffClockTimes` last
                 in if (cacheLimit c) >= diff
                    then return . fromJust $ value c
                    else do stat <- getFileStatus (file c)
                            if last > (epochToClockTime $ modificationTime stat)
                            then do _ <- writeIORef cache $ bumpTime now c
                                    return . fromJust $ value c
                            else readNew cache c now

cacheValue :: Cached a -> IO a
cacheValue c = (reader c) (file c)

bumpTime :: ClockTime -> Cached a -> Cached a
bumpTime now c = c { lastChecked = Just now }

bumpValue :: a -> Cached a -> Cached a
bumpValue v c = c { value = Just v }

readNew :: Cache a -> Cached a -> ClockTime -> IO a
readNew cache c now = do
  newVal <- cacheValue c
  _ <- writeIORef cache . bumpValue newVal $ bumpTime now c
  return $ newVal

---------- Cache Map infrastructure
data CacheMap a = CacheMap { fn :: (FilePath -> IO a)
                   , ref :: IORef (Map FilePath (Cache a)) }

newCacheMap :: (FilePath -> IO a) -> IO (CacheMap a)
newCacheMap f = do
  r <- newIORef $ Map.empty
  return $ CacheMap { fn = f, ref = r }

insert :: CacheMap a -> TimeDiff -> FilePath -> IO (Cache a)
insert cacheMap diff fname = do
  m <- readIORef (ref cacheMap)
  c <- newCache diff (fn cacheMap) fname
  _ <- writeIORef (ref cacheMap) $ Map.insert fname c m
  return c

lookup :: CacheMap a -> FilePath -> IO (Maybe a)
lookup cacheMap fname = do
  m <- readIORef $ ref cacheMap
  case Map.lookup fname m of
    Nothing -> return $ Nothing
    Just looked -> do c <- readCache looked
                      return $ Just c

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
