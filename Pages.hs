{-# LANGUAGE OverloadedStrings #-}
module Pages (archive, article, meta) where

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html5 hiding (map, article)
import Text.Blaze.Html5.Attributes hiding (title, span)
import qualified Text.Blaze.Html5.Attributes as A

import Data.Char
import Data.Monoid
import Control.Monad
import qualified Data.Text as Txt

import Post
import PostMap (BlogPost)
import qualified PostMap as P

data Section = Blog | Archive | Links | Meta | Feed deriving (Eq, Ord, Show)

archive :: [BlogPost] -> Html
archive posts =
    template Archive "The Archive" $
             ul $ forM_ posts (\p -> do li $ a ! postHref p $ toMarkup $ P.title p)

article :: [BlogPost] -> BlogPost -> Html -> Html
article posts p body =
    template Blog "A Blog Post" $ do
      h1 $ toMarkup $ P.title p
      span ! class_ "posted" $ toMarkup $ P.posted p
      body
      postLinks $ adjacents posts p

plain :: Section -> Html -> Html
plain s content =
    template s "A Page" content

---------- Main template
-- template :: Section -> String -> Html -> Html
template section pageTitle content =
    html $ do
      head $ do title $ pageTitle <> " - langnostic"
                link ! href "/feed/atom" ! type_ "application/atom+xml" ! rel "alternate" ! A.title "Site-wide Langnostic Atom Feed"
                stylesheet "/static/css/langnostic.css"
                stylesheet "/static/css/default.css"
                script ! type_ "text/javascript" ! src "/static/js/highlight.pack.js" $ ""
                script ! type_ "text/javascript" $ "hljs.initHighlightingOnLoad();"
      body $ do a ! href "/" $ img ! class_ "logo-bar" ! src "/static/img/langnostic.png"
                navBar section
                div ! class_ "content" $ content
                hr
                pageFooter

---------- Template Components
postLinks :: (Maybe BlogPost, Maybe BlogPost) -> Html
postLinks (prev, next) =
    div ! class_ "post-nav" $ do
      case prev of
        (Just p) -> a ! class_ "prev-post" ! postHref p $ do
                              "<-"
                              toMarkup $ P.title p
        _ -> ""
      case next of
        (Just n) -> a ! class_ "next-post" ! postHref n $ do
                              toMarkup $ P.title n
                              "->"
        _ -> ""

adjacents :: Eq a => [a] -> a -> (Maybe a, Maybe a)
adjacents (a:b:c:haystack) needle
    | needle == a = (Nothing, Just b)
    | needle == b = (Just a, Just c)
    | otherwise = adjacents (b:c:haystack) needle
adjacents (a:b:[]) needle
    | needle == a = (Nothing, Just b)
    | needle == b = (Just a, Nothing)
    | otherwise = (Nothing, Nothing)
adjacents [] _ = (Nothing, Nothing)

postHref p = href . toValue $ "/posts/" <> (P.file p)

stylesheet :: AttributeValue -> Html
stylesheet url = link ! rel "stylesheet" ! href url ! type_ "text/css" ! media "screen"

navBar :: Section -> Html
navBar s = div ! class_ "top-menu-container" $ do
             ul ! class_ "top-menu" $
                forM_ [Blog, Archive, Links, Meta, Feed] (li . navItem s)

navItem :: Section -> Section -> Html
navItem s item
    | s == item = toMarkup $ show item
navItem _ item = a ! href (toValue $ "/" <> (map toLower $ show item)) $ toMarkup $ show item

pageFooter :: Html
pageFooter = div ! class_ "license" $ do
               a ! rel "license" ! href "http://creativecommons.org/licenses/by-sa/3.0/" $ do
                                        img ! alt "Creative Commons License" ! A.style "border-width:0;float: left; margin: 0px 15px 15px 0px;" ! src "http://i.creativecommons.org/l/by-sa/3.0/88x31.png"
               p $ do span ! customAttribute "xmlns:dct" "http://purl.org/dc/terms/" ! customAttribute "property" "dct:title" $ "all articles at langnostic"
                      " are licensed under a "
                      a ! rel "license" ! href "http://creativecommons.org/licenses/by-sa/3.0/" $
                        "Creative Commons Attribution-ShareAlike 3.0 Unported License"
               p $ do
                 "Reprint, rehost and distribute freely (even for profit), but attribute the work and allow your readers the same freedoms. "
                 a ! href "http://creativecommons.org/choose/results-one?q_1=2&q_1=1&field_commercial=y&field_derivatives=sa&field_jurisdiction=&field_format=&field_worktitle=this+langnostic+article&field_attribute_to_name=Inaimathi&field_attribute_to_url=http%3A%2F%2Flangnostic.inaimathi.com&field_sourceurl=http%3A%2F%2Flangnostic.inaimathi.com&field_morepermissionsurl=&lang=en_US&n_questions=3" $ "Here's"
                 " a license widget you can use"
               p $ do
                 "The menu background image is "
                 a ! href "https://www.flickr.com/photos/danzen/2360096926/in/photostream/" $ "Jewel Wash"
                 ", taken from "
                 a ! href "https://www.flickr.com/photos/danzen/" $ "Dan Zen's"
                 " flickr stream and released under a "
                 a ! href "https://creativecommons.org/licenses/by/2.0/" $ "CC-BY"
                 " license."
