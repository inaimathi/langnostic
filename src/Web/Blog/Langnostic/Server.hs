{-# LANGUAGE OverloadedStrings #-}
module Web.Blog.Langnostic.Server where

import Web.Spock.Safe
import Network.Wai.Middleware.Static
import Network.HTTP.Types

import Data.List (sortBy)
import Data.Function (on)
import Data.Maybe
import Data.Monoid (mconcat)
import Control.Monad.Trans
import Text.Blaze.Html hiding (text)
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 (hr)
import Data.Text.Lazy (toStrict)

import Web.Blog.Langnostic.Posts hiding (id)
import Web.Blog.Langnostic.Pages
import qualified Web.Blog.Langnostic.Pages as Pages
import qualified Web.Blog.Langnostic.Cached as C
import Web.Blog.Langnostic.Feed

serve :: Int -> IO ()
serve port = do
  pc <- newPostCache
  pm <- newPostMap pc
  _ <- mapM_ (\path -> C.insert pc (C.minutes 90) path) [
        "static/content/intro.md"
       ,"static/content/links.md"
       ,"static/content/meta.md"
       ,"static/content/tipjar.md"
       ,"static/content/404.md"
       ]
  runSpock port $ spockT id $ (appMiddleware >> handlers pm)

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
  get "tipjar" $ do
                pg <- liftIO $ bodyByPath pm "static/content/tipjar.md"
                html . toStrict . renderHtml $ Pages.template TipJar "TipJar" pg
  get "meta" $ do
                pg <- liftIO $ bodyByPath pm "static/content/meta.md"
                html . toStrict . renderHtml $ Pages.template Meta "Meta" pg
  get "feed" $ do
                ps <- liftIO $ posts pm
                withBodies <- liftIO $ feedWithBody pm ps
                html . toStrict . renderHtml $ atom withBodies
  get ("feed" <//> "atom") $ do
                ps <- liftIO $ posts pm
                withBodies <- liftIO $ feedWithBody pm ps
                html . toStrict . renderHtml $ atom withBodies
  get ("feed" <//> "atom" <//> var) $ \tag -> do
                ps <- liftIO $ byTags pm [tag]
                withBodies <- liftIO $ feedWithBody pm ps
                html . toStrict . renderHtml $ atom withBodies
  get ("feed" <//> "atom" <//> "by-tag" <//> var) $ \tag -> do
                ps <- liftIO $ byTags pm [tag]
                withBodies <- liftIO $ feedWithBody pm ps
                html . toStrict . renderHtml $ atom withBodies
  hookAny GET $ \url -> error404 pm

feedWithBody :: PostMap -> [BlogPost] -> IO [(BlogPost, Html)]
feedWithBody pm ps = do
  bodies <- mapM (postBody pm) ps
  return $ zip ps bodies

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
