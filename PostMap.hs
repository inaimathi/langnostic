{-# LANGUAGE OverloadedStrings #-}
module PostMap where

import Data.Aeson
import Data.Maybe (catMaybes)
import Text.Blaze.Html (Html)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8

import Post
import Cached

data BlogPost = BlogPost {
      id :: Integer
    , title :: String, file :: String
    , edited :: Integer, posted :: Integer
    , tags :: [String]
    } deriving (Eq, Ord, Show)

instance FromJSON BlogPost where
    parseJSON = withObject "post" $ \o ->
                BlogPost <$> o .: "id"
                <*> o .: "title"
                <*> o .: "file"
                <*> o .: "edited"
                <*> o .: "posted"
                <*> o .: "tags"

type PostMap = Cache [(BlogPost, Cache Html)]

-- bySlug :: PostMap -> String -> BlogPost

postMap :: IO PostMap
postMap = newCache (minutes 30) posts "posts.json"

posts :: FilePath -> IO [(BlogPost, Cache Html)]
posts fname = do
  f <- BS.readFile fname
  mapM getCache $ catMaybes . map decode $ C8.split '\n' f
      where getCache p = do
                c <- cachePost ("posts/" ++ file p ++ ".md")
                return $ (p, c)
