{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Safe
import Network.Wai.Middleware.Static

import Data.Monoid (mconcat)
import Control.Monad.Trans
import Text.Blaze.Html hiding (text)
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 (hr)
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
  get root   $ home pm
  get "blog" $ home pm
  get ("posts" <//> var) $ \slug ->
      case bySlug pm slug of
        Nothing -> text "Nope. That doesn't exist."
        Just p -> do body <- liftIO $ postBody p
                     html . toStrict . renderHtml . Pages.template Blog "A Blog Post" $ Pages.article (posts pm) p $ toMarkup body
  get "archive" $ do
    m <- liftIO postMap
    html $ toStrict $ renderHtml $ Pages.archive m
  get ("archive" <//> "by-tag" <//> var) $ \tag -> do
    m <- liftIO postMap
    html $ toStrict $ renderHtml $ Pages.archive $ filter ((tag `elem`) . tags) m
  get "links" $ do
    pg <- liftIO $ readPost "static/content/links.md"
    html $ toStrict $ renderHtml $ Pages.template Links "Links" pg
  get "meta" $ do
    pg <- liftIO $ readPost "static/content/meta.md"
    html $ toStrict $ renderHtml $ Pages.template Meta "Meta" pg

home pm = do
  intro <- liftIO $ readPost "static/content/intro.md"
  p <- liftIO $ postBody $ latest
  html $ toStrict $ renderHtml $ Pages.template Blog "Welcome" $ do
            intro
            hr
            Pages.article (posts pm) latest $ toMarkup p
  where latest = last $ posts pm
