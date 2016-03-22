{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Safe
import Network.Wai.Middleware.Static
import Network.HTTP.Types

import Data.Maybe
import Data.Monoid (mconcat)
import Control.Monad.Trans
import Text.Blaze.Html hiding (text)
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 (hr)
import Data.Text.Lazy (toStrict)

import Posts hiding (id)
import Pages
import qualified Cached as C
import Feed

main :: IO ()
main = do
  pc <- newPostCache
  pm <- newPostMap pc
  _ <- mapM_ (\path -> C.insert pc (C.minutes 30) path) [
        "static/content/intro.md"
       ,"static/content/links.md"
       ,"static/content/meta.md"
       ,"static/content/404.md"
       ]
  runSpock 8080 $ spockT id $ (appMiddleware >> handlers pm)

appMiddleware :: SpockT IO ()
appMiddleware = do
  middleware $ staticPolicy (hasPrefix "static" >-> isNotAbsolute >-> noDots >-> addBase ".")

handlers :: PostMap -> SpockT IO ()
handlers pm = do
  get root   $ home pm
  get "blog" $ home pm
  get ("posts" <//> var) $ \slug -> do
                p <- liftIO $ bySlug pm slug
                case p of
                  Nothing -> error404 pm
                  Just p -> do body <- liftIO $ postBody pm p
                               ps <- liftIO $ posts pm
                               html . toStrict . renderHtml . Pages.template Blog (toMarkup $ title p) $ Pages.article ps p $ toMarkup body
  get "archive" $ do
                ps <- liftIO $ posts pm
                html . toStrict . renderHtml $ Pages.archive ps
  get ("archive" <//> "by-tag" <//> var) $ \tag -> do
                ps <- liftIO $ byTags pm [tag]
                html . toStrict . renderHtml $ Pages.archive ps
  get "links" $ do
                pg <- liftIO $ bodyByPath pm "static/content/links.md"
                html . toStrict . renderHtml $ Pages.template Links "Links" pg
  get "meta" $ do
                pg <- liftIO $ bodyByPath pm "static/content/meta.md"
                html . toStrict . renderHtml $ Pages.template Meta "Meta" pg
  get "feed" $ do
                ps <- liftIO $ posts pm
                html . toStrict . renderHtml $ atom ps
  get ("feed" <//> "atom") $ do
                ps <- liftIO $ posts pm
                html . toStrict . renderHtml $ atom ps
  get ("feed" <//> "atom" <//> var) $ \tag -> do
                ps <- liftIO $ byTags pm [tag]
                html . toStrict . renderHtml $ atom ps
  get ("feed" <//> "atom" <//> "by-tag" <//> var) $ \tag -> do
                ps <- liftIO $ byTags pm [tag]
                html . toStrict . renderHtml $ atom ps
  hookAny GET $ \url -> error404 pm

error404 pm = do
  setStatus status404
  p <- liftIO $ bodyByPath pm "static/content/404.md"
  html . toStrict . renderHtml $ Pages.template Error "Not Found" p

home pm = do
  intro <- liftIO $ bodyByPath pm "static/content/intro.md"
  postList <- liftIO $ posts pm
  p <-  liftIO $ postBody pm $ last postList
  html . toStrict . renderHtml $ Pages.template Blog "Welcome" $ do
            intro
            hr
            Pages.article postList (last postList) $ toMarkup p
