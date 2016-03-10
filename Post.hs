{-# LANGUAGE OverloadedStrings #-}
module Post (readPost) where

import Text.Pandoc
import Text.Pandoc.Walk (walk)
import Text.Blaze.Html (Html)

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
