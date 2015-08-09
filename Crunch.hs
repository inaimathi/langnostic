module Main where

import Data.List
import Data.Function
import Data.Char
import System.Environment

crunch :: String -> [(String, Int)]
crunch s = filterCommons . filterOneOfs . frequencyMap $ map toLower s
    where filterOneOfs = filter ((>2) . snd)
          frequencyMap = map (\w -> (head w, length w)) . group . sort . words
          filterCommons = filter (not . (`elem` commonWords) . fst)
          commonWords = ["the", "of", "to", "a", "is", "and"
                        , "if", "it", "i", "as", "but", "an"
                        , "are", "at", "in", "than", "you"]

main :: IO ()
main = do
  [fname] <- getArgs
  f <- readFile fname
  mapM_ p . byCount $ crunch f
  return ()
      where byCount = sortBy (flip compare `on` snd)
            p (str, ct) = putStrLn $ concat [str, " - ", show ct]
