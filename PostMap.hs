{-# LANGUAGE OverloadedStrings #-}
module PostMap ( PostMap, postMap, bySlug, byTags, postBody, posts
               , BlogPost, PostMap.id, title, file, edited, posted, tags) where

import System.Time

import Data.Aeson
import Data.List (intersect)
import Data.Maybe (catMaybes)
import Text.Blaze.Html (Html)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8

import Post

data BlogPost = BlogPost {
      id :: Integer
    , title :: String, file :: String
    , edited :: Integer, posted :: Integer
    , tags :: [String]
    } deriving (Eq, Ord, Show)

instance FromJSON BlogPost where
    parseJSON = withObject "post" $ \o ->
                BlogPost
                <$> o .: "id"
                <*> o .: "title"
                <*> o .: "file"
                <*> o .: "edited"
                <*> o .: "posted"
                <*> o .: "tags"

type PostMap = [BlogPost]

postBody :: BlogPost -> IO Html
postBody post = readPost $ "posts/" ++ file post ++ ".md"

bySlug :: PostMap -> String -> Maybe BlogPost
bySlug pm slug = case filter ((==slug) . file) pm of
                   [] -> Nothing
                   res -> Just $ head res

byTags :: PostMap -> [String] -> [BlogPost]
byTags pm ts = filter hasSome pm
    where hasSome post = case ts `intersect` tags post of
                           [] -> False
                           _ -> True

postMap :: IO PostMap
postMap = fetchPosts "posts.json"

posts :: PostMap -> [BlogPost]
posts = Prelude.id

fetchPosts :: FilePath -> IO [BlogPost]
fetchPosts fname = do
  f <- BS.readFile fname
  return $ catMaybes . map decode $ C8.split '\n' f
