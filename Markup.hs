module Markup (Html(..), Rss(..), render) where

import Data.Text.Lazy (Text, pack)

data Html = P String
          | Ul [Html]
          | Ol [Html]
          | A String Html
          | Html [Html]
          | Head [Html]
          | Body [Html]
          | Meta [(String, String)]
          | Title String
          | Stylesheets [String]
          | Scripts [String]
          | Script String
          | Span [String] [Html]
          | Img String String
          | Raw String
          | H1 String
          | Hr

instance Show Html where
    show (Raw s) = s
    show (P s) = wrap "p" s
    show (Ul items) = wrap "ul" $ concatMap ((wrap "li") . show) items
    show (Ol items) = wrap "ol" $ concatMap ((wrap "li") . show) items
    show (A link text) = concat ["<a href=\"", link, "\">", show text, "</a>"]
    show (Html cont) = concat [ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
                              , wrap "html" $ concatMap show cont]
    show (Head cont) = wrap "head" $ concatMap show cont
    show (Body cont) = wrap "body" $ concatMap show cont
    show (Meta values) = concat ["<meta ", concat $ concatMap (\(k, v) -> [k, "=", show v, " "]) values, "/>"]
    show (Title txt) = wrap "title" txt
    show (Stylesheets sheets) = concat $ concatMap (\s -> ["<link rel=\"stylesheet\" href=", show s, " />"]) sheets
    show (Scripts scripts) = concat $ concatMap (\s -> ["<script type=\"text/javascript\" src=", show s, "></script>"]) scripts
    show (Span cls cont) = concat ["<span class=\"", unwords cls, "\">", concatMap show cont, "</span>"]
    show (Script str) = concat ["<script type=\"text/javascript\">", str, "</script>"]
    show (Img title src) = concat ["<img src=", show src, " title=", show title, " />"]
    show Hr = "<hr />"
    show (H1 title) = wrap "h1" title

data Rss = Feed [Rss]
         | Name String
         | Subtitle String
         | Self String
         | Link String
         | Entry [Rss]
         | Id String
         | Updated String
         | Author String 

instance Show Rss where
    show (Feed rest) = concat ["<feed xmlns=\"http://www.w3.org/2005/Atom\">", concatMap show rest, "</feed>"]
    show (Name t) = wrap "title" t
    show (Subtitle t) = wrap "subtitle" t
    show (Self href) = concat ["<link href=", show href, " rel=\"self\" />"]
    show (Link href) = concat ["<link href=", show href, " />"]
    show (Entry rest) = wrap "entry" $ concatMap show rest
    show (Id i) = wrap "id" i
    show (Updated t) = wrap "updated" t
    show (Author name) = wrap "author" $ wrap "name" name

wrap :: String -> String -> String
wrap name cont = concat ["<", name, ">", cont, "</", name, ">"]

render :: Show a => a -> Text
render = pack . show
