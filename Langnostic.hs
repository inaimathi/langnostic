{-# LANGUAGE OverloadedStrings #-}

module Langnostic where

import Web.Scotty
import Text.Pandoc hiding (Meta, Link, Span)

import qualified Data.Map as Map
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)

import System.Posix.Files (getFileStatus, modificationTime)
import System.Posix.Types (EpochTime)

import System.Locale
import Data.Time.Format
import Data.Time.Clock.POSIX

import Markup

-- ----------
-- Handlers
atomFeed :: [Post] -> ActionM ()
atomFeed ps = do
  setHeader "Content-Type" "application/atom+xml"
  text . render . Feed $
          concat [ [ Name "Language Agnostic"
                   , Subtitle "Site-Wide Langnostic Atom feed"
                   , Self "http://langnostic.inaimathi.ca/feed/atom"
                   , Link "http://langnostic.inaimathi.ca" ]
                 , entries ]
    where ent e = Entry [ Name $ postTitle e
                        , Link $ "http://langnostic.inaimathi.ca/posts/" ++ postTitle e
                        , Id $ concat ["tag:langnostic.inaimathi.ca,", show $ postId e]
                        , Updated . atomTime $ posted e
                        , Author "Inaimathi" ]
          entries = map ent . take 25 $ reverse ps

atomTime :: Integer -> String
atomTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000000-04:00" . posixSecondsToUTCTime . fromIntegral

atomFeedOfTag :: String -> [Post] -> ActionM ()
atomFeedOfTag tag = atomFeed . filter (elem tag . postTags)

meta :: ActionM ()
meta = do
  f <- liftIO $ readFile "static/content/meta.md"
  page "meta" [renderMarkdown f]

links :: ActionM ()
links = do
  f <- liftIO $ readFile "static/content/links.md"
  page "links" [renderMarkdown f]

archive :: [Post] -> ActionM ()
archive ps = do 
  page "archive" [ articleList ps, tagList $ tagCounts ps ]

archiveOfTag :: String -> [Post] -> ActionM ()
archiveOfTag tag ps = do
  page "archive" [articleList $ filter (elem tag . postTags) ps, tagList $ tagCounts ps]

articleList :: [Post] -> Html
articleList ps = Ul $ map postLink ps

tagList :: [(String, Int)] -> Html
tagList ts = Span ["tags"] $ map tagLink ts 
    where tagLink (t, ct) = Span [] [A ("/archive/by-tag/" ++ t) $ Raw t, Raw "(", Raw $ show ct, Raw $ ") "]

latest :: [Post] -> ActionM ()
latest ps = do
  let p = head $ reverse ps
      (prev, next) = postLinks ps $ postId p
  intro <- liftIO $ readFile "static/content/intro.md"
  post <- liftIO . readFile $ concat ["posts/", postFile p , ".md"]
  page "blog" [ renderMarkdown intro
              , Hr
              , H1 $ postTitle p
              , renderMarkdown post
              , Hr
              , Span ["post-nav"] [prev, next]]

article :: [Post] -> String -> ActionM ()
article ps name = do
  case filter ((==name) . postFile) ps of
    [p] -> let (prev, next) = postLinks ps $ postId p
           in do f <- liftIO $ readFile $ concat ["posts/", name, ".md"]
                 page "blog" [H1 $ postTitle p, renderMarkdown f, Hr, Span ["post-nav"] [prev, next]]
    []  -> page "blog" [Raw "No such post..."]
    _   -> page "blog" [Raw "Something weird just happened..."]

-- ----------
-- Templates
page :: String -> [Html] -> ActionM ()
page section content = 
    html . render 
             $ Html [ Head [ Title $ section ++ " - langnostic"
                           , Meta [("content", "width=device-width, initial-scale=1"), ("name", "viewport")]
                           , Stylesheets ["/static/css/langnostic.css", "/static/css/default.css"]
                           , Scripts ["/static/js/highlight.pack.js"] 
                           , Script "hljs.initHighlightingOnLoad();"]
                    , Body $ [ Span ["logo-bar"] [ A "/" $ Img "Langnostic logo bar" "/static/img/langnostic.png"]
                             , Hr, Span ["top-menu"] [menu], Hr ] ++ content ]
    where menu = Ul $ map (\(uri, name) -> if name == section then Raw name else (A uri $ Raw name)) sections
          sections = [("/", "blog"), ("/archive", "archive"), ("/links", "links"), ("/meta", "meta"), ("/feed/atom", "atom feed")]

postLinks :: [Post] -> Int -> (Html, Html)
postLinks ps ix = arrange $ filter (inRange . postId) ps
    where inRange i = and [succ ix >= i, i >= pred ix]
          arrange [a, _, b] = (Span ["prev-post"] [ Raw "<-", postLink a], Span ["next-post"] [ postLink b, Raw "->"])
          arrange [a, b]
              | ix == postId a = (Raw "", Span ["next-post"] [ postLink b, Raw "->"])
              | otherwise = (Span ["prev-post"] [ Raw "<-", postLink a], Raw "")
          arrange _ = (Raw "", Raw "")

postLink :: Post -> Html
postLink p = A ("/post/" ++ postFile p) . Raw $ postTitle p

renderMarkdown :: String -> Html
renderMarkdown = Raw . writeHtmlString def . readMarkdown def

-- ----------
-- Archive
readArchive :: String -> IO [Post]
readArchive fname = do 
  arcRef <- liftIO postRef
  stat <- getFileStatus fname
  arc <- readIORef arcRef
  if loadedAt arc == modificationTime stat
  then return $ posts arc
  else do ps <- readFile fname
          writeIORef arcRef $ Archive {
                           posts = catMaybes . map (decode . BS.pack) $ lines ps
                         , loadedAt = modificationTime stat
                         }
          fmap posts $ readIORef arcRef

tagCounts :: [Post] -> [(String, Int)]
tagCounts ps = Map.toAscList $ foldl (Map.unionWith (+)) Map.empty tagMaps
    where tagMaps = map (Map.fromList . map (\t -> (t, 1)) . postTags) ps

postRef :: IO (IORef Archive)
postRef = newIORef $ Archive [] 0

data Archive = Archive { posts :: [Post]
                       , loadedAt :: EpochTime}

data Post = Post { postId :: Int, postTitle :: String, postFile :: String, posted :: Integer, postTags :: [String] } deriving (Show)

instance ToJSON Post where
    toJSON (Post id title file posted tags) = object [ "id" .= id, "title" .= title, "file" .= file, "posted" .= posted, "tags" .= tags ]

instance FromJSON Post where
    parseJSON (Object v) = Post <$> v .: "id"
                                <*> v .: "title"
                                <*> v .: "file"
                                <*> v .: "posted"
                                <*> v .: "tags"
    parseJSON _ = mzero
