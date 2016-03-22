{-# LANGUAGE OverloadedStrings #-}
module Posts ( PostMap, newPostMap, bySlug, byTags, postBody, bodyByPath, posts
             , PostCache, newPostCache
             , BlogPost(..), path) where

import System.Time

import Text.Pandoc
import Text.Pandoc.Walk (walk)
import Text.Blaze.Html (Html)

import Data.Aeson
import Data.List (intersect)
import Data.Maybe (catMaybes, fromJust)
import Text.Blaze.Html (Html)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8

import Cached

data BlogPost = BlogPost {
      id :: Integer
    , title :: String, slug :: String
    , posted :: Integer
    , edited :: Integer
    , tags :: [String]
    } deriving (Eq, Ord, Show)

instance FromJSON BlogPost where
    parseJSON = withObject "post" $ \o ->
                BlogPost
                <$> o .: "id"
                <*> o .: "title"
                <*> o .: "file"
                <*> o .: "posted"
                <*> o .: "edited"
                <*> o .: "tags"

data PostMap = PostMap (Cache [BlogPost]) PostCache
type PostCache = CacheMap Html FilePath

path :: BlogPost -> FilePath
path p = "posts/" ++ slug p ++ ".md"

newPostMap :: PostCache -> IO PostMap
newPostMap pc = do
  ps <- newCache (minutes 30) $ readPostList "posts.json"
  c <- readCache ps
  let addNew post = do
               present <- hasKey pc $ path post
               if present
               then return ()
               else do _ <- insert pc (minutes 30) $ path post
                       return ()
  _ <- mapM_ addNew c
  return $ PostMap ps pc

readPostList :: FilePath -> IO [BlogPost]
readPostList fname = do
  f <- BS.readFile fname
  return $ catMaybes . map decode $ C8.split '\n' f

newPostCache :: IO PostCache
newPostCache = newCacheMap readPost

readPost :: FilePath -> IO Html
readPost fpath = do
  f <- readFile fpath
  return $ case readMarkdown def f of
    Right p -> writeHtml def $ walk linkedHeaders p
    _ -> error $ "Invalid post: " ++ fpath

linkedHeaders :: Block -> Block
linkedHeaders (Header n opts@(slug,_,_) content) = Header n opts linked
    where linked = (anchor slug) ++ [ Link ("",[],[]) content ("#" ++ slug, "") ]
linkedHeaders node = node

anchor :: String -> [Inline]
anchor name = [ RawInline (Format "html") ("<a name=\"" ++ name ++ "\">")
              , RawInline (Format "html") "</a>"]

posts :: PostMap -> IO [BlogPost]
posts (PostMap index _) = readCache index

bySlug :: PostMap -> String -> IO (Maybe BlogPost)
bySlug (PostMap index _) s = do
  c <- readCache index
  return $ case filter ((==s) . slug) c of
             [] -> Nothing
             res -> Just $ head res

byTags :: PostMap -> [String] -> IO [BlogPost]
byTags (PostMap index _) ts = do
  c <- readCache index
  return $ filter hasSome c
      where hasSome post = case ts `intersect` tags post of
                             [] -> False
                             _ -> True

postBody :: PostMap -> BlogPost -> IO Html
postBody pm post = bodyByPath pm $ path post

bodyByPath :: PostMap -> FilePath -> IO Html
bodyByPath (PostMap _ ps) path = do
  looked <- Cached.lookup ps path
  return $ fromJust looked
