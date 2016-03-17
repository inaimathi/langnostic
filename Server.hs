{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Safe
import Network.Wai.Middleware.Static

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
  runSpock 8080 $ spockT id $ (appMiddleware >> handlers pm)

appMiddleware :: SpockT IO ()
appMiddleware = do
  middleware $ staticPolicy (hasPrefix "static" >-> isNotAbsolute >-> noDots >-> addBase ".")

handlers :: PostMap -> SpockT IO ()
handlers pm = do
  get root $ do
    p <- liftIO $ readPost "test.md"
    html $ toStrict $ renderHtml p
  get ("posts" <//> var) $ \slug ->
      case bySlug pm slug of
        Nothing -> text "Nope. That doesn't exist."
        Just p -> do body <- liftIO $ postBody p
                     html $ toStrict $ renderHtml $ Pages.article (toMarkup $ title p) (toMarkup $ posted p) body
  get "archive" $ do
    m <- liftIO postMap
    html $ toStrict $ renderHtml $ Pages.archive m
  get ("archive" <//> "by-tag" <//> var) $ \tag -> do
    m <- liftIO postMap
    html $ toStrict $ renderHtml $ Pages.archive $ filter ((tag `elem`) . tags) m
