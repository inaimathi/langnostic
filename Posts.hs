{-# LANGUAGE OverloadedStrings #-}
module Posts where

import Data.Aeson
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8

data BlogPost = BlogPost { id :: Integer, title :: String, file :: String, edited :: Integer, posted :: Integer, tags :: [String] } deriving (Eq, Ord, Show)

instance FromJSON BlogPost where
    parseJSON = withObject "post" $ \o ->
                BlogPost <$> o .: "id"
                         <*> o .: "title"
                         <*> o .: "file"
                         <*> o .: "edited"
                         <*> o .: "posted"
                         <*> o .: "tags"

posts :: FilePath -> IO [BlogPost]
posts fname = do
  file <- BS.readFile fname
  return $ catMaybes . map decode $ C8.split '\n' file
