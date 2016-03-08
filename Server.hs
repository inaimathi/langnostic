{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Safe

import Data.Monoid (mconcat)
import Control.Monad.Trans
import Text.Blaze.Html hiding (text)
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (toStrict)

import PostMap hiding (id)
import Post
import Pages

main :: IO ()
main = do
  pm <- postMap
  runSpock 8080 $ spockT id $ handlers pm

handlers :: MonadIO m => PostMap -> SpockCtxT ctx m ()
handlers pm = do
  get root $ do
    p <- liftIO $ readPost "test.md"
    html $ toStrict $ renderHtml p
  get ("posts" <//> var) $ \slug ->
      case bySlug pm slug of
        Nothing -> text "Nope. That doesn't exist."
        Just p -> do body <- liftIO $ postBody p
                     html $ toStrict $ renderHtml $ Pages.article $ body
