Here's a quick and dirty chat application written in Wai<a name="note-Tue-Apr-16-133808EDT-2013"></a>[|1|](#foot-Tue-Apr-16-133808EDT-2013).

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Chan

import Control.Monad.Trans (liftIO)

import Network.Wai
import Network.Wai.EventSource
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.HTTP.Types (status200, ok200)

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.ByteString.Char8 (ByteString, unpack)

app :: Chan ServerEvent -> Application
app chan req = do
  (params, _) &lt;- parseRequestBody lbsBackEnd req
  case pathInfo req of
    [] -> return $ ResponseFile status200 [("Content-Type", "text/html")] "static/index.html" Nothing
    ["post"] -> liftIO $ postMessage chan $ lookPost "message" params
    ["source"] -> eventSourceAppChan chan req
    path -> error $ "unexpected pathInfo " ++ show (queryString req)

lookPost :: ByteString -> [(ByteString, ByteString)] -> String
lookPost paramName params = case lookup paramName params of
  Just val -> unpack val
  _ ->  ""

postMessage :: Chan ServerEvent -> String -> IO Response
postMessage chan msg = do
  writeChan chan $ ServerEvent (Just $ fromString "message") Nothing $ [fromString msg]
  return $ responseLBS ok200 [] "Posted"

main :: IO ()
main = do  
  chan &lt;- newChan
  run 8000 $ gzip def $ app chan
```

That's the most basic example I could find/cobble together of using [SSEs in Wai](https://github.com/yesodweb/wai/tree/master/wai-eventsource). That's the library called `Network.Wai.EventSource` up there, and you can see the channel represented in the expressions involving `newChan`, `eventSourceAppChan` and `writeChan`. Basically, we set up a `Chan`<a name="note-Tue-Apr-16-133913EDT-2013"></a>[|2|](#foot-Tue-Apr-16-133913EDT-2013) at server startup, we hand out an endpoint whenever someone requests `/source`, and we write to all endpoints whenever someone requests `/post`.

The file `index.html` is exactly what you think it is; about 10 lines each of HTML and JavaScript that set up the front-end `EventSource` hooks and make sure the chat list gets updated with each new message. You could write it yourself without very much trouble.

This isn't particularly interesting. Firstly because, as you can see, it's *ridiculously* simple, and secondly because it doesn't scale. I mean it scales with users, sure. According to the [Warp benchmarks](http://www.yesodweb.com/blog/2011/03/preliminary-warp-cross-language-benchmarks), we can expect this to support somewhere between 20k and 50k people chatting depending on their loquaciousness, but since they'll all be chatting anonymously in the same room, the experience will stop being useful well before that. The next step confounded me for a little while because I had the assumption that using state in Haskell meant using [the `State` monad](http://www.haskell.org/haskellwiki/State_Monad)<a name="note-Tue-Apr-16-134338EDT-2013"></a>[|3|](#foot-Tue-Apr-16-134338EDT-2013). It turns out that's probably not what you'd want here.

What we're after is a system where you can start up arbitrary new rooms, and post to a specific one. In other words, something like

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Chan
import Control.Concurrent (forkIO, threadDelay)

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource (ResourceT)

import Network.Wai
import Network.Wai.EventSource
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)

import Network.HTTP.Types (status200, ok200)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import qualified Data.ByteString.Char8 as C

import Data.IORef
import Data.Text (unpack, pack)

app :: IORef [(String, Chan ServerEvent)] -> Application
app channels req = do
  (params, _) &lt;- parseRequestBody lbsBackEnd req
  case pathInfo req of
    [] -> serveFile "text/html" "static/index.html"
    ["jquery.js"] -> serveFile "text/javascript" "static/jquery.min.js"
    ["chat.js"] -> serveFile "text/javascript" "static/chat.js"
    [channelName, action] -> do
      chan &lt;- liftIO $ getOrCreateChannel channels $ unpack channelName
      case action of
        "post" -> 
          liftIO $ postMessage chan $ lookPost "message" params
        "source" -> 
          eventSourceAppChan chan req
        _ -> serveFile "text/html" "static/index.html"
    _ -> serveFile "text/html" "static/index.html"

serveFile :: C.ByteString -> FilePath -> ResourceT IO Response
serveFile mime filePath = return $ ResponseFile status200 [("Content-Type", mime)] filePath Nothing

lookPost :: C.ByteString -> [(C.ByteString, C.ByteString)] -> String
lookPost paramName params = case lookup paramName params of
  Just val -> C.unpack val
  _ ->  ""

getOrCreateChannel :: IORef [(String, Chan ServerEvent)] -> String -> IO (Chan ServerEvent)
getOrCreateChannel channels name = do
  res &lt;- readIORef channels
  case lookup name res of
    Just chan -> 
      return chan
    _ -> do
      new &lt;- newChan
      atomicModifyIORef channels (\cs -> ((name, new):cs, new))
      return new

postMessage :: Chan ServerEvent -> String -> IO Response
postMessage chan msg = do
  writeChan chan $ ServerEvent (Just $ fromString "message") Nothing $ [fromString msg]
  return $ responseLBS ok200 [] "Posted"

main :: IO ()
main = do
  channels &lt;- newIORef []
  run 8000 $ gzip def $ app channels
```

That's a bit chunkier, but not by very much.

The significant operations there all involve something called an `IORef`, which is Haskell-talk for "a pointer". You can think of it an `IO`-based global variable that you can store stuff in<a name="note-Tue-Apr-16-134413EDT-2013"></a>[|4|](#foot-Tue-Apr-16-134413EDT-2013), in this case, a map of channel names to channel streams.

That `index.html` file has a bunch of front-end changes too, mostly to do with acquiring and displaying multiple SSE sources, but we're not interested in that today. In the back-end, you'll notice that we've got a new function, `getOrCreateChannel`, which takes a "pointer" to our channel map and a name, and either returns the result of looking up that name, or inserts and returns a corresponding entry. readIORef "dereferences" that "pointer" to our map, and `atomicModifyIORef` mutates it. The rest of it should be self-explanatory.

Because we need to do a channel lookup before calling `postMessage` or `eventSourceAppChan`, our routes get a bit more complicated. We need to call `getOrCreateChannel` on the passed in `channelName`, then pass that to the appropriate function and return the response<a name="note-Tue-Apr-16-134535EDT-2013"></a>[|5|](#foot-Tue-Apr-16-134535EDT-2013).

Finally, instead of passing a single `channel` to our `app`, we need to pass it a "pointer" to our lookup table. That happens in `main` at the bottom there.

The result of this exercise, as long as we put the front-end together appropriately, is a multi-room, anonymous, HTML chat system. More importantly though, this is a demonstration of how to handle simple global states in Haskell without tearing all your hair out.

I really wish someone else had written this before I started thinking about it...


* * *
##### Footnotes
1 - <a name="foot-Tue-Apr-16-133808EDT-2013"></a>[|back|](#note-Tue-Apr-16-133808EDT-2013) - No, still not [Yesod](http://www.yesodweb.com/). Feel perfectly free to use it if that's your thing, but I'd still recommend [Happstack](http://www.happstack.com/page/view-page-slug/1/happstack) if you absolutely, positively *need* a framework..

2 - <a name="foot-Tue-Apr-16-133913EDT-2013"></a>[|back|](#note-Tue-Apr-16-133913EDT-2013) - Which I assume is reasonably efficient, since it's one of Haskell's [basic concurrency constructs](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-Chan.html).

3 - <a name="foot-Tue-Apr-16-134338EDT-2013"></a>[|back|](#note-Tue-Apr-16-134338EDT-2013) - Also, because I'm still not quite awesome enough at this that I can manipulate type expressions in my head. As a result, successful signature changes rarely happen first try, and I often find myself commenting them out then resorting to `:t` in `GHCi` and following the compilers' lead. I assume that's mechanical rather than a conceptual problem though, and talking about how I need more practice won't really help *you* out in any way.

4 - <a name="foot-Tue-Apr-16-134413EDT-2013"></a>[|back|](#note-Tue-Apr-16-134413EDT-2013) - The [IORef docs](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-IORef.html#v:atomicModifyIORef) warn that using more than one of these in a program makes them unreliable in a multi-threaded setting. The thing is: 
  
-     This chat program is extremely simple, needing only one global map to store open channels
-     If it ever got to the point of needing a more complex model, I'd hook it up to AcidState rather than trying to fiddle with MVars myself.
-   

5 - <a name="foot-Tue-Apr-16-134535EDT-2013"></a>[|back|](#note-Tue-Apr-16-134535EDT-2013) - You can see that happening in the branch labeled `[channelName, action] ->`, though we easily could have separated it into an external function rather than nesting `case`s.
