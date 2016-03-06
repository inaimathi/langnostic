{-# LANGUAGE OverloadedStrings #-}
module Post where

import Text.Pandoc
import Text.Pandoc.Walk (walk)
import Text.Blaze.Html (Html)

import Cached

cachePost :: FilePath -> IO (Cache Html)
cachePost fpath = newCache (minutes 30) fpath readPost

readPost :: FilePath -> IO Html
readPost fpath = do
  f <- readFile fpath
  return $ case readMarkdown def f of
    Right p -> writeHtml def . footnotes $ walk linkedHeaders p
    _ -> error $ "Invalid post: " ++ fpath

----------
-- Transform headers into mid-post anchors
linkedHeaders :: Block -> Block
linkedHeaders (Header n opts@(slug,_,_) content) = Header n opts linked
    where linked = (anchor slug) ++ [ Link ("",[],[]) content ("#" ++ slug, "") ]
linkedHeaders node = node

----------
-- Transform footnote "links" into footnotes
footnotes :: Pandoc -> Pandoc
footnotes doc = Pandoc meta $ content ++ compileFootnotes tbl
    where tbl = flip zip [1..] $ queryWith extractFootnotes doc
          Pandoc meta content = walk (replaceFootnote tbl) doc

extractFootnotes :: Inline -> [Inline]
extractFootnotes lnk@(Link _ _ ("footnote",_)) = [lnk]
extractFootnotes _ = []

replaceFootnote :: [(Inline, Int)] -> Inline -> Inline
replaceFootnote tbl elem@(Link _ content _) =
    case lookup elem tbl of
      Nothing -> elem
      Just n -> Span ("", [], []) $
                concat [ anchor ("note-" ++ show n)
                       , [Link ("", [], []) [Str $ concat ["[", show n, "]"]] ("#foot-" ++ show n, "")]]
replaceFootnote _ elem = elem

compileFootnotes :: [(Inline, Int)] -> [Block]
compileFootnotes fs = [ HorizontalRule
                      , Header 3 ("", [], []) [Str "Footnotes"]
                      , BulletList $ map toFootList fs]
    where toFootList ((Link _ content _), n) = [Para $ concat [ anchor ("foot-" ++ show n)
                                                              , [Link ("", [], []) [Str "[Back]"] ("#note-" ++ show n, "")]
                                                              , content]]

anchor :: String -> [Inline]
anchor name = [ RawInline (Format "html") ("<a name=\"" ++ name ++ "\">")
              , RawInline (Format "html") "</a>"]

replaceFootnotes :: Inline -> Inline
replaceFootnotes (Link _ _ ("footnote",_)) = Str ""
