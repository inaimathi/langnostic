{-# LANGUAGE OverloadedStrings #-}
module Cached ( Cache, readCache, newCache
              , CMap, newCacheMap, insert -- , Cached.lookup
              , minutes, hours) where

import System.Time
import System.Directory
import System.Posix.Files
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Data.Ratio

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

data Cached a = Cached { reader :: (FilePath -> IO a)
                       , file :: FilePath
                       , cacheLimit :: TimeDiff
                       , lastChecked :: ClockTime
                       , value :: a }

type Cache a = IORef (Cached a)

readCache :: Cache a -> IO a
readCache cache = do
  now <- getClockTime
  c <- readIORef cache
  let diff = now `diffClockTimes` (lastChecked c)
  if (cacheLimit c) >= diff
  then return $ value c
  else do stat <- getFileStatus (file c)
          if (lastChecked c) > (epochToClockTime $ modificationTime stat)
          then do _ <- writeIORef cache $ c { lastChecked = now }
                  return $ value c
          else do newVal <- (reader c) (file c)
                  _ <- writeIORef cache $ c { value = newVal, lastChecked = now }
                  return $ newVal


epochToClockTime :: Real a => a -> ClockTime
epochToClockTime x =
    TOD seconds secfrac
    where ratval = toRational x
          seconds = floor ratval
          secfrac = floor $ (ratval - (seconds % 1) ) * picosecondfactor
          picosecondfactor = 10 ^ 12

newCache :: TimeDiff -> (FilePath -> IO a) -> FilePath -> IO (Cache a)
newCache cacheLimit reader fname = do
  now <- getClockTime
  val <- reader fname
  newIORef $ Cached { reader = reader, file = fname, cacheLimit = cacheLimit, value = val, lastChecked = now }

minutes :: Int -> TimeDiff
minutes ms = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour = 0, tdMin = ms, tdSec = 0, tdPicosec = 0 }

hours :: Int -> TimeDiff
hours hs = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour = hs, tdMin = 0, tdSec = 0, tdPicosec = 0 }


data CMap a = CMap { fn :: (FilePath -> IO a)
                   , ref :: IORef (Map FilePath (Cache a)) }

newCacheMap :: (FilePath -> IO a) -> IO (CMap a)
newCacheMap f = do
  r <- newIORef $ Map.empty
  return $ CMap { fn = f, ref = r }

insert :: CMap a -> TimeDiff -> FilePath -> IO (Cache a)
insert cacheMap diff fname = do
  m <- readIORef (ref cacheMap)
  c <- newCache diff (fn cacheMap) fname
  _ <- writeIORef (ref cacheMap) $ Map.insert fname c m
  return c

lookup :: CMap a -> FilePath -> IO (Maybe a)
lookup cacheMap fname = do
  m <- readIORef $ ref cacheMap
  case Map.lookup fname m of
    Nothing -> return $ Nothing
    Just looked -> do c <- readCache looked
                      return $ Just c
