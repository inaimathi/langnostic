module Web.Blog.Langnostic.Feed (atom) where

import System.Time
import System.Locale

import Data.List (sortBy)
import Data.Function (on)
import Data.Text (replace)

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html5 hiding (map, article, title)
import Text.Blaze.Html5.Attributes hiding (title, span, id)
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html.Renderer.String as ToString
import qualified Text.Blaze.Html5.Attributes as A

import Web.Blog.Langnostic.Posts

atom :: [(BlogPost, Html)] -> Html
atom posts = preEscapedString $
                concat [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
                       , "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
                       , "<title>Language Agnostic</title>"
                       , "<subtitle>Langnostic Atom Feed</subtitle>"
                       , "<link href=\"http://langnostic.inaimathi.ca/feed/atom\" rel=\"self\" />"
                       , "<link href=\"http://langnostic.inaimathi.ca\" />"
                       , concatMap atomPost . take 20 $ sortBy (flip compare `on` (posted . fst)) posts
                       , "</feed>"
                       ]

atomPost :: (BlogPost, Html) -> String
atomPost (b, body) = concat [ "<entry>"
                    , "<title>", title b, "</title>"
                    , "<updated>", editedTime b, "</updated>"
                    , "<link href=\"http://langnostic.inaimathi.ca/posts/", slug b, "\" />"
                    , "<author><name>inaimathi</name></author>"
                    , "<content type=\"html\">"
                    , escapeHtml $ ToString.renderHtml body
                    , "</content>"
                    , "</entry>"
                    ]

-- (defn escape-html
--   "Change special characters into HTML character entities."
--   [text]
--   (.. #^String (as-str text)
--     (replace "&" "&amp;")
--     (replace "<" "&lt;")
--     (replace ">" "&gt;")
--     (replace "\"" "&quot;")))

escapeHtml [] = []
escapeHtml ('&':rest) = "&amp;" ++ escapeHtml rest
escapeHtml ('<':rest) = "&lt;" ++ escapeHtml rest
escapeHtml ('>':rest) = "&gt;" ++ escapeHtml rest
escapeHtml ('"':rest) = "&quot;" ++ escapeHtml rest
escapeHtml (c:rest) = c:(escapeHtml rest)

editedTime :: BlogPost -> String
editedTime b = formatCalendarTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" $ toUTCTime $ TOD (edited b) 0
