{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Safe

import Data.Monoid (mconcat)
import Control.Monad.Trans
import Text.Blaze.Html hiding (text)
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (toStrict)

import Cached
import Post

-- main = do
--   c <- cachePost "test.md"
--   scotty 3000 $ do
--   get "/:word" $ do
--     p <- liftIO $ readCache c
--     html $ renderHtml p

main :: IO ()
main = do
  c <- cachePost "test.md"
  runSpock 8080 $ spockT id $
       do get root $ do
            p <- liftIO $ readCache c
            html $ toStrict $ renderHtml p
       -- get ("hello" <//> var) $ \name ->
       --     text ("Hello " <> name <> "!")
