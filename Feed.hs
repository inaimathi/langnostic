module Feed (atom) where

import System.Time
import System.Locale

import Data.List (sortBy)
import Data.Function (on)

import Prelude hiding (head, id, div, span)
import Text.Blaze.Html5 hiding (map, article, title)
import Text.Blaze.Html5.Attributes hiding (title, span, id)
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5.Attributes as A

import Posts

atom :: [BlogPost] -> Html
atom posts = preEscapedString $
             concat [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
                    , "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
                    , "<title>Language Agnostic</title>"
                    , "<subtitle>Langnostic Atom Feed</subtitle>"
                    , "<link href=\"http://langnostic.inaimathi.ca/feed/atom\" rel=\"self\" />"
                    , "<link href=\"http://langnostic.inaimathi.ca\" />"
                    , concatMap atomPost $ sortBy (flip compare `on` posted) posts
                    , "</feed>"
                    ]

atomPost :: BlogPost -> String
atomPost b = concat [ "<entry>"
                    , "<title>", title b, "</title>"
                    , "<updated>", editedTime b, "</updated>"
                    , "<link href=\"http://langnostic.inaimathi.ca/posts/", file b, "\" />"
                    , "<author><name>inaimathi</name></author>"
                    , "</entry>"
                    ]

editedTime :: BlogPost -> String
editedTime b = formatCalendarTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" $ toUTCTime $ TOD (edited b) 0
