{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Web.Spock.Safe

main :: IO ()
main =
    runSpock 8080 $ spockT id $
    do get root $
           text "Hello World!"
       get ("hello" <//> var) $ \name ->
           text ("Hello " <> name <> "!")
