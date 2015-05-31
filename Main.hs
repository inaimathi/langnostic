{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.List.Split
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class (liftIO)

import Langnostic

main :: IO ()
main = scotty 4444 $ do
         withPosts' "/post/:title" $ flip article
         withPosts  "/feed/atom" atomFeed
         withPosts' "/feed/atom/by-tag/:tag" atomFeedOfTag
         get        "/meta" meta
         get        "/links" links
         withPosts  "/archive" archive
         withPosts' "/archive/by-tag/:tag" archiveOfTag
         static     "/img/langnostic.png"
         static     "/css/langnostic.css"
         static     "/css/default.css"
         static     "/js/highlight.pack.js"
         withPosts  "/" latest

withPosts :: RoutePattern -> ([Post] -> ActionM ()) -> ScottyM ()
withPosts uri fn = get uri $ do posts <- getPosts
                                fn posts

withPosts' :: String -> (String -> [Post] -> ActionM ()) -> ScottyM ()
withPosts' uri fn = 
    let paramName = tail . head . filter ((==":") . take 1) $ splitOn "/" uri
    in get (capture uri) $ do posts <- getPosts
                              p <- param $ pack paramName
                              fn p posts

getPosts :: ActionM [Post]
getPosts = liftIO $ readArchive "posts.json"

static :: String -> ScottyM ()
static name = get (capture $ "/static" ++ name) . file $ "static" ++ name
