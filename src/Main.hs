module Main where

import System.Environment
import Web.Blog.Langnostic.Server

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> serve 4444
    (port:_) -> serve $ read port
