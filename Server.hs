{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Safe
import Network.Wai.Middleware.Static
import Network.HTTP.Types

import Data.Monoid (mconcat)
import Control.Monad.Trans
import Text.Blaze.Html hiding (text)
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 (hr)
import Data.Text.Lazy (toStrict)

import Posts hiding (id)
import Pages
import Feed

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
                     html . toStrict . renderHtml . Pages.template Blog (toMarkup $ title p) $ Pages.article (posts pm) p $ toMarkup body
  get "archive" $ html $ toStrict $ renderHtml $ Pages.archive $ posts pm
  get ("archive" <//> "by-tag" <//> var) $ \tag -> do
    html $ toStrict $ renderHtml $ Pages.archive $ filter ((tag `elem`) . tags) $ posts pm
  get "links" $ do
    pg <- liftIO $ readPost "static/content/links.md"
    html $ toStrict $ renderHtml $ Pages.template Links "Links" pg
  get "meta" $ do
    pg <- liftIO $ readPost "static/content/meta.md"
    html $ toStrict $ renderHtml $ Pages.template Meta "Meta" pg
  get "feed" $ html $ toStrict $ renderHtml $ atom $ posts pm
  get ("feed" <//> "atom") $ html $ toStrict $ renderHtml $ atom $ posts pm
  get ("feed" <//> "atom" <//> var) $ \tag -> do
                html $ toStrict $ renderHtml $ atom $ filter ((tag `elem`) . tags) $ posts pm
  get ("feed" <//> "atom" <//> "by-tag" <//> var) $ \tag -> do
                html $ toStrict $ renderHtml $ atom $ filter ((tag `elem`) . tags) $ posts pm
  hookAny GET $ \url -> do
                setStatus status404
                p <- liftIO $ readPost "static/content/404.md"
                html $ toStrict $ renderHtml $ Pages.template Error "Not Found" p

home pm = do
  intro <- liftIO $ readPost "static/content/intro.md"
  p <- liftIO $ postBody $ latest
  html $ toStrict $ renderHtml $ Pages.template Blog "Welcome" $ do
            intro
            hr
            Pages.article (posts pm) latest $ toMarkup p
  where latest = last $ posts pm
