{-# LANGUAGE OverloadedStrings #-}
module Posts ( PostMap, postMap, bySlug, byTags, postBody, posts
               , BlogPost, Posts.id, title, file, edited, posted, tags, readPost) where

import System.Time

import Text.Pandoc
import Text.Pandoc.Walk (walk)
import Text.Blaze.Html (Html)

import Data.Aeson
import Data.List (intersect)
import Data.Maybe (catMaybes)
import Text.Blaze.Html (Html)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8

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

readPost :: FilePath -> IO Html
readPost fpath = do
  f <- readFile fpath
  return $ case readMarkdown def f of
    Right p -> writeHtml def $ walk linkedHeaders p
    _ -> error $ "Invalid post: " ++ fpath

----------
-- Transform headers into mid-post anchors
linkedHeaders :: Block -> Block
linkedHeaders (Header n opts@(slug,_,_) content) = Header n opts linked
    where linked = (anchor slug) ++ [ Link ("",[],[]) content ("#" ++ slug, "") ]
linkedHeaders node = node

anchor :: String -> [Inline]
anchor name = [ RawInline (Format "html") ("<a name=\"" ++ name ++ "\">")
              , RawInline (Format "html") "</a>"]
