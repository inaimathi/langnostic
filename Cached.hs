{-# LANGUAGE OverloadedStrings #-}
module Cached (readCache, newCache, minutes, hours) where

import System.Time
import Data.IORef
import Control.Monad.IO.Class (liftIO)

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
  else do newVal <- (reader c) (file c)
          _ <- writeIORef cache $ c { value = newVal, lastChecked = now }
          return $ newVal

newCache :: TimeDiff -> FilePath -> (FilePath -> IO a) -> IO (Cache a)
newCache cacheLimit fname reader = do
  now <- getClockTime
  val <- reader fname
  newIORef $ Cached { reader = reader, file = fname, cacheLimit = cacheLimit, value = val, lastChecked = now }

minutes :: Int -> TimeDiff
minutes ms = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour = 0, tdMin = ms, tdSec = 0, tdPicosec = 0 }

hours :: Int -> TimeDiff
hours hs = TimeDiff { tdYear = 0, tdMonth = 0, tdDay = 0, tdHour = hs, tdMin = 0, tdSec = 0, tdPicosec = 0 }
