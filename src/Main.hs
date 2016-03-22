module Main where

import System.Environment
import Web.Blog.Langnostic.Server

main :: IO ()
main = do
  args <- getArgs
  serve $ read $ head args
