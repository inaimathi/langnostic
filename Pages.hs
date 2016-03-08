{-# LANGUAGE OverloadedStrings #-}
module Pages (archive, article, meta) where

import Prelude hiding (head, id, div)
import Text.Blaze.Html5 hiding (map, article)
import Text.Blaze.Html5.Attributes hiding (title)

import Control.Monad
import qualified Data.Text as Txt

import Post
import PostMap (BlogPost)
import qualified PostMap as P

data Section = Blog | Archive | Links | Meta | Feed deriving (Eq, Ord, Show)

-- template :: Section -> String -> Html -> Html
template _ pageTitle content =
    html $ do
      head $ do title pageTitle
      body $ do div ! class_ "header" $ do
                  p "Put nav here, I guess"
                content

archive :: [BlogPost] -> Html
archive posts =
    template Archive "The Archive" $
             forM_ posts (\p -> do a ! href "link-goes-here" $ "A Title")

article :: Html -> Html
article content =
    template Blog "A Blog Post" content

plain :: Section -> Html -> Html
plain s content =
    template s "A Page" content
