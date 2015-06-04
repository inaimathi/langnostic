It's been a slow writing month, as you can tell by the number of recent submissions. Hopefully, I remember how to do this.

Other than raising a child<a name="note-Sun-Feb-10-150629EST-2013"></a>[|1|](#foot-Sun-Feb-10-150629EST-2013), taking a long-overdue break from programming in my down time and dealing with various other mundane bullshit, I've been working on a small project in Haskell that'll make coordination for my wife and I marginally simpler. It's a minimal, web-based running shopping list, and the idea is to use it to forget fewer things during the weekly trip as well as make occasional after-work quick grocery stops possible. I could easily have knocked this out in a day or three using Python, but I figured I'd take some time to put my Haskell skills through more rigorous testing.

[Last time](http://barbershop.inaimathi.ca/), I worked with [Happstack](http://happstack.com/clck/view-page-slug/1/home), which is the best looking web framework available in the language, as far as I'm concerned. This time, I just pulled out [warp](http://hackage.haskell.org/package/warp-1.3.7.2), the [wai](https://github.com/yesodweb/wai)-based server, and went frameworkless.

### <a name="why-wai-without-y" href="#why-wai-without-y"></a>Why wai without Y?

Using a framework binds you to it. [Yesod](http://www.yesodweb.com/) especially seems to have a stick up its ass about using something other than Haskell to build pieces of a project. You may recall that [I theorized](http://langnostic.blogspot.ca/2012/09/js-frameworks.html) about keeping front and back-ends entirely separate a little while ago. Well, I'm still on that. So as much as framework maintainers want me to use or [Hamlet](http://hackage.haskell.org/package/hamlet-1.1.3.1) or whatever, doing so would be counter-productive for me. Yesod actually goes one further and has [a JS-generation language too](http://www.yesodweb.com/blog/2012/01/wiki-chat-subsite). The idea is supposed to be that *all* of your code then goes through the same rigorous type checks and optimizations that the ML family is famous for. In practice, what it means is that you're more-or-less forced to use jQuery<a name="note-Sun-Feb-10-150633EST-2013"></a>[|2|](#foot-Sun-Feb-10-150633EST-2013), and it means that all of your project is in Haskell<a name="note-Sun-Feb-10-150637EST-2013"></a>[|3|](#foot-Sun-Feb-10-150637EST-2013) and it means that your server-side code is doing everything<a name="note-Sun-Feb-10-150641EST-2013"></a>[|4|](#foot-Sun-Feb-10-150641EST-2013). I think I'll stick to the manual trio for the front-end and just let Haskell do the heavy database lifting.

The easiest way to do that seems to be to basically keep the model and a controller on server-side, and the view and a controller on the client side. [Happstack](http://happstack.com/clck/view-page-slug/1/home) is the only [one of](http://snapframework.com/) the main three Haskell [web frameworks](http://www.yesodweb.com/) to make something like that reasonably simple, but I already got some time in with it, and the Warp server claims to [bench much better](http://www.yesodweb.com/blog/2011/03/preliminary-warp-cross-language-benchmarks).

So, let's give this a whirl.

### <a name="haskell-dev-general-thoughts" href="#haskell-dev-general-thoughts"></a>Haskell Dev General Thoughts

Before I get to the code in my usual self-review fashion, let me let you in on some lessons I had to learn the hard way by [hitting my head up against the language](http://stackoverflow.com/questions/14721720/ambiguous-type-variable-in-acidstate-functions).

Firstly, don't try to do bottom-up design here. Or, at least, slow down with it until you get fairly good with the language, fairly familiar with the documentation/conventions, and fairly good at understanding how GHCi works. The techniques of [wishful thinking](http://c2.com/cgi/wiki?WishfulThinking) and [building the language up](http://www.paulgraham.com/progbot.html) towards your problem are still applicable, but Haskell has a way of focusing pretty relentlessly on types. Even though it infers a lot of type information without your help, the most common pieces of advice I get from other Haskellers is to 


1.   work out what the type of my function is going to be before writing the function itself and
1.   explicitly write it down above the function


I've found that this *does* help, if for no other reason than thinking through your operations' types will highlight the pieces that you didn't think through well enough, and it'll remind you what the next piece needs to interface with. It's just not what I'm used to doing<a name="note-Sun-Feb-10-150645EST-2013"></a>[|5|](#foot-Sun-Feb-10-150645EST-2013).

Secondly, don't trust GHCi completely. As a Lisper, this sort of blew my mind because the Lisp REPL *is* a running Lisp image. If something works in the SLIME prompt, it's a fairly good bet that you can just copy it out into your `.lisp` file and have it work the same way<a name="note-Sun-Feb-10-150651EST-2013"></a>[|6|](#foot-Sun-Feb-10-150651EST-2013). GHCi isn't exactly that. Its type reflection directive does some odd things when inferring types<a name="note-Sun-Feb-10-150655EST-2013"></a>[|7|](#foot-Sun-Feb-10-150655EST-2013), and it fudges `IO`-related operations in general<a name="note-Sun-Feb-10-150702EST-2013"></a>[|8|](#foot-Sun-Feb-10-150702EST-2013). For simple stuff that doesn't do IO, you can probably still get away with Lisp-style exploratory REPLing, but it doesn't seem to be a good general strategy. For GHCi, at least. For all I know, HUGS is better at this sort of thing, but I haven't used it extensively yet, despite it being the only Haskell REPL on offer for ARM at the moment.

Thirdly, it's possible<a name="note-Sun-Feb-10-150705EST-2013"></a>[|9|](#foot-Sun-Feb-10-150705EST-2013) to apply the venerable technique of [debugging by `printf`](http://en.wikipedia.org/wiki/Debugging#Techniques). At first glance, it seems like it wouldn't be, since doing any output from a function pollutes its type with `IO`, which then cascades to all of the callers of that function and causes you to rewrite half the project if you want to add some output in one place. Oh, and then rewrite it back once you're done looking at debugging output. There's a library called [`Debug.Trace`](http://www.haskell.org/ghc/docs/latest/html/libraries/base//Debug-Trace.html) that lets you pull off something similar enough. It highlights very clearly that this isn't meant for production use though; what you're supposed to do, near as I can tell, is `import qualified Debug.Trace as Debug`, then sprinkle `Debug.trace "A trace message goes here..." $ {{the thing you want trace output for}` throughout your code, and run `M-x query-replace-regexp Debug.trace ".*?" ` later to replace these calls with nothing. It's possible that there's an automatic way of removing them, but I didn't bother finding it for a project this size.

### <a name="routing" href="#routing"></a>Routing

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default (def)
import Data.String (fromString)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Vault as Vault

import Data.Acid (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced (update', query')
import Data.Acid.Local (createCheckpointAndClose)
import Data.IxSet ((@=), Proxy(..), getOne)

import Network.Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.Wai.Session (withSession)
import Network.Wai.Session.Map (mapStore_)
import Network.Wai.Handler.Warp (run)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Network.HTTP.Types (ok200, unauthorized401, status404)

import Control.Exception (bracket)
import Control.Concurrent.Chan (Chan, newChan, dupChan, writeChan)
import Control.Monad.Trans.Resource (ResourceT)
    
import TypeSynonyms
import Util
import Model
import Handlers

routes :: DB -> SessionStore -> Request -> RES
routes db session req = do
  let Just (sessionLookup, sessionInsert) = Vault.lookup session (vault req)
  user &lt;- sessionLookup "user"
  case pathInfo req of
    ("app":rest) -> 
      loggedInRoutes db user rest req
    ("auth":rest) ->
      authRoutes db sessionLookup sessionInsert rest req
    ["static", subDir, fileName] -> 
      serveStatic subDir fileName
    [] -> 
      resFile "text/html" "static/index.html"
    ["favicon.ico"] -> 
      resPlaceholder
    _ -> res404

authRoutes :: DB ->  LookupFN -> InsertFN -> [Text.Text] -> Request -> RES
authRoutes db sLookup sInsert path req = do
  withPostParams req ["name", "passphrase"] route
  where route [name, pass] = 
          case path of
            ["login"] -> 
              login db sInsert name pass
            ["register"] -> 
              case pass of
                "" -> resError "At least pick a non-empty passphrase"
                _  -> register db sInsert name pass
            _ -> res404

loggedInRoutes :: DB -> Maybe String -> [Text.Text] -> Request -> RES
loggedInRoutes db maybeUserName path req = do
  (params, _) &lt;- parseRequestBody lbsBackEnd req
  case maybeUserName of
    Just name -> do
      maybeAccount &lt;- query' db $ AccountByName name
      case maybeAccount of
        Just user -> case path of
          ("item":rest) -> 
            withParams params ["itemName"] route
            where route [itemName] = itemRoutes db user itemName rest params
          ["list"] -> 
            listItems db user
          ["new"] -> 
            withParams params ["itemName", "comment", "count"] new
            where new [name, comment, count] = newItem db user name comment (read count :: Integer)
          ["change-passphrase"] -> 
            withParams params ["newPassphrase"] change
            where change [newPass] = changePassphrase db user newPass
          _ -> res404
        Nothing -> resError "Invalid user"
    Nothing -> resError "Not Logged In"

itemRoutes :: DB -> Account -> String -> [Text.Text] -> BSAssoc -> RES
itemRoutes db user itemName path params = do
  case getOne $ (accountItems user) @= itemName of
    Just item -> case path of
      ["need"] -> 
        needItem db user item
      ["got"] -> 
        gotItem db user item
      ["delete"] -> 
        deleteItem db user item
      ["edit"] ->
        edit $ extractOptional params ["comment", "count"]
        where edit [comment, count] = editItem db user item comment count
      _ -> res404
    Nothing -> resError "Invalid item"

----- Server start
main = do
  session &lt;- Vault.newKey
  store &lt;- mapStore_
  bracket (openLocalState initialDB) (createCheckpointAndClose) 
    (\db -> run 3000 . withSession store (fromString "SESSION") def session $ routes db session)

```
Basically, case statements. `routes` at the top there dispatches on `pathInfo req`, which returns the URI minus `GET`/`#` parameters and `split` on `/`. You then use the standard Haskell pattern matching facilities to figure out what the user has requested and what to do about it.

Lets take a close-up look at the type signature of `routes` before moving on.

```haskell
routes :: DB -> SessionStore -> Request -> RES
```

That should look suspiciously minimal to anyone who's actually done web development in Haskell before, and it is. The why has to do with this import

```haskell
import TypeSynonyms
```

I have no idea whether this is good Haskelling practice or not, but I ended up defining descriptive synonyms for a bunch of the complex types I needed to work with. *Then* I realized that I need to refer to them in more than one module and it would be better to centralize them rather than having copies of the definitions in each relevant file.

```haskell
module TypeSynonyms where

import Data.Acid (AcidState)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vault as Vault

import Network.Wai
import Network.Wai.Session (Session)

import Control.Monad.Trans.Resource (ResourceT)

import Model

type DB = AcidState GoGetDB
type RES = ResourceT IO Response
type SessionStore = Vault.Key (Session (ResourceT IO) String String)
type LookupFN = (String -> ResourceT IO (Maybe String))
type InsertFN = (String -> String -> ResourceT IO ())
type BSAssoc = [(BS.ByteString, BS.ByteString)]
```

So `DB` is shorthand for the database class we're using<a name="note-Sun-Feb-10-150716EST-2013"></a>[|10|](#foot-Sun-Feb-10-150716EST-2013), `RES` is shorthand for the HTTP response `IO` type, and `SessionStore`/`LookupFN`/`InsertFN` are session vault and related lookup/insertion functions respectively. I also defined shorthand for a `ByteString` association table, since that's how parsed request parameters are stored and they get passed around more than once.

Ok, back to routes.

If you read through that file, you'll notice that a lot of validation and lookup logic in with the routing rather than in the specific handlers that might need them. That sort of happened accidentally, and again, I'm not sure it's the best way to organize these files, but it does have two big advantages. First, because validation and error routing happens beforehand, the handler functions themselves can be exclusively concerned with the successful case. By the time an item-related function is called for example, it's guaranteed to have a request from an existing, logged-in user relating to an existing item. So the handler doesn't need to check for any of those internally. Second, we centralize the validation and lookups. If we expected the handlers themselves to deal with it, then *each* of the item-related handlers, for example would need to check for an authenticated user, and they'd each have to check that the item they're asked to operate on actually exists. By doing it beforehand, we only do that check once.

I mentioned that this is new to me. That's because the various Python/Ruby frameworks I'm familiar with all represent a routing table as some sort of ordered associative list of regexes and handlers, while all of the Common Lisp/Clojure servers I'm familiar with give you something along the lines of `define-handler`, which takes a name, handler body *and* routing URI, removing the need for an explicit central routing structure at all. As I recall, Smalltalk works something like the Lisps and Erlang does something similar to Python/Ruby. So this is the first time I did any real work as part of handler routing, and it seems like it might be a good approach. In a dynamically typed language, I'd be really worried about not making it obvious enough that a handler function is expecting thoroughly validated input rather than doing that part itself, which would increase the chances of a dev passing in *un*validated input and causing an explosion somewhere. But the type annotations and rigorous static checking take care of that for me here in Haskell-land.

Lets take a look at these simplified handlers we're passing stuff on to.

### <a name="the-handlers" href="#the-handlers"></a>The Handlers

```haskell
module Handlers ( listItems, needItem, gotItem, editItem, deleteItem, newItem
                , changePassphrase, register, login ) where

import Control.Monad.Trans  (liftIO)

import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS

import Data.Acid (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced (update', query')
import Data.Acid.Local (createCheckpointAndClose)
import Data.IxSet (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet, insert, delete, toAscList, updateIx )

import Crypto.Scrypt (EncryptedPass(..), Pass(..), defaultParams, encryptPass, verifyPass)

import TypeSynonyms
import Util
import Model

---------- HTTP Handlers
----- Item Related
listItems :: DB -> Account -> RES
listItems db user = do
  resIxItems  $ accountItems user

needItem :: DB -> Account -> Item -> RES
needItem db user item = do
  update' db $ ChangeItem user new
  resIxItems $ updateIx (itemName item) new (accountItems user) 
    where new = item { itemStatus = Need }

gotItem :: DB -> Account -> Item -> RES
gotItem db user item = do
  update' db $ ChangeItem user new
  resIxItems $ updateIx (itemName item) new (accountItems user)
    where new = item { itemStatus = Got }

editItem :: DB -> Account -> Item -> Maybe String -> Maybe String -> RES
editItem db user item newComment newCount = do
  update' db $ ChangeItem user new
  resIxItems $ updateIx (itemName item) new (accountItems user)
    where new = item { itemComment = comment, itemCount = count }
          comment = fromMaybe (itemComment item) newComment
          count = fromMaybe (itemCount item) (maybeRead newCount :: Maybe Integer)

deleteItem :: DB -> Account -> Item -> RES
deleteItem db user item = do
  update' db $ DeleteItem user item
  resIxItems $ delete item (accountItems user)

newItem :: DB -> Account -> String -> String -> Integer -> RES
newItem db user name comment count =
  case getOne $ (accountItems user) @= name of
    Just item -> needItem db user item
    Nothing -> do
      update' db $ NewItem user item
      resIxItems $ insert item (accountItems user)
      where item = Item { itemName=name, itemComment=comment, itemCount=count, itemStatus=Need }

----- Account Related
changePassphrase :: DB -> Account -> String -> RES
changePassphrase db user newPassphrase = do
  new &lt;- liftIO . encryptPass defaultParams . Pass $ BS.pack newPassphrase
  update' db . UpdateAccount $ user { accountPassphrase = unEncryptedPass new }
  resOk user

register :: DB -> InsertFN -> String -> String -> RES
register db sessionInsert name passphrase = do
  pass &lt;- liftIO . encryptPass defaultParams . Pass $ BS.pack passphrase
  existing &lt;- query' db $ AccountByName name
  case existing of
    Nothing -> do
      acct &lt;- update' db . NewAccount name $ unEncryptedPass pass
      sessionInsert "user" name
      resOk acct
    _ -> resError "User already exists"

login :: DB -> InsertFN -> String -> String -> RES
login db sessionInsert name passphrase = do 
  res &lt;- query' db $ AccountByName name
  case res of
    Just user -> case verifyPass defaultParams (Pass $ BS.pack passphrase) pass of
      (True, _) -> do
        sessionInsert "user" $ accountName user
        resOk user
      _ -> resNO
      where pass = EncryptedPass $ accountPassphrase user
    _ -> resNO

&lt;p>The authentication functions are predictably complicated, but I'll get to them later. Take a look at the &lt;code>needItem&lt;/code> function.&lt;/p>

needItem :: DB -> Account -> Item -> RES
needItem db user item = do
  update' db $ ChangeItem user new
  resIxItems $ updateIx (itemName item) new (accountItems user) 
    where new = item { itemStatus = Need }
```

It's not expecting an account name and item ID to reference by. It's expecting an `Account`<a name="note-Sun-Feb-10-150754EST-2013"></a>[|11|](#foot-Sun-Feb-10-150754EST-2013) and it's expecting an `Item`<a name="note-Sun-Feb-10-150758EST-2013"></a>[|12|](#foot-Sun-Feb-10-150758EST-2013). It does the work of updating the `DB`, and then sends back an appropriate response.

Really, I could have made one more general function along the lines of `editItem`, then called it for `need`, `got`, and separate handlers for `changeComment` and `changeCount`. In fact, that was officially a `note to self`.

> EDIT:  
>   
> The item-related section now reads  
>   
> ```haskell
> needItem :: DB -> Account -> Item -> RES
> needItem db user item = updateItem db user new
>   where new = item { itemStatus = Need }
> 
> gotItem :: DB -> Account -> Item -> RES
> gotItem db user item = updateItem db user new
>   where new = item { itemStatus = Got }
> 
> editItem :: DB -> Account -> Item -> Maybe String -> Maybe String -> RES
> editItem db user item newComment newCount = updateItem db user new
>   where new = item { itemComment = comment, itemCount = count }
>         comment = fromMaybe (itemComment item) newComment
>         count = fromMaybe (itemCount item) (maybeRead newCount :: Maybe Integer)
> 
> updateItem :: DB -> Account -> Item -> RES
> updateItem db user newItem = do
>   update' db $ ChangeItem user newItem
>   resIxItems $ updateIx (itemName newItem) newItem (accountItems user)
> ```
>   
> Sat, 09 Feb, 2013  

The way it's currently written, the most complex of the item-related handlers is `editItem`, and that's because it needs to optionally change the `comment`, `count` or both depending on what's passed in. This is the price you pay for automatic currying and maximally terse partials; those features don't share space well with optional/keyword/rest arguments. The result is that when you need the latter, you need to represent them as mandatory `Maybe` args, or as a custom type argument. We've already gone through an example of the first approach. You can see the second if you squint at `verifyPass` and `encryptPass`. Specifically, the second argument, `defaultParams` is of type `ScryptParams`, which is defined as

```haskell
data ScryptParams = Params { logN, r, p, bufLen :: Integer} deriving (Eq)
```

which is really a way of representing keyword args in a language without any. `defaultParams` itself is defined as 

```haskell
defaultParams :: ScryptParams
defaultParams = fromJust (scryptParams 14 8 1)
```

and `scryptParams` is a surprisingly complicated function that validates input and returns `Params { logN, r, p, bufLen = 64 }`. In Lisp, `verifyPass` would have an arg line like

```lisp
(verify-pass incoming stored &key (logN 14) (r 8) (p 1) (bufLen 64))
```

and start off with some `assert`ions to mimic the validation done in `scryptParams`. Of course, that's not to say that the Haskell approach is a hack; both approaches have their advantages and disadvantages in practice<a name="note-Sun-Feb-10-150808EST-2013"></a>[|13|](#foot-Sun-Feb-10-150808EST-2013). In the specific situation I'm dealing with above, since we don't have optionals, it would probably have been better to separate the count and comment changing handlers and let the front-end call the specific one it wants. That was another note to self.

Since we're here, and since I'm the guy who's been going on and on about this, something would be slightly amiss if I failed to note the authentication system, at least in passing. We're using the [`scrypt` algorithm](http://en.wikipedia.org/wiki/Scrypt) to store and verify passwords<a name="note-Sun-Feb-10-150811EST-2013"></a>[|14|](#foot-Sun-Feb-10-150811EST-2013). If a password is verified we store the users' name in their session cookie. `wai-session` encrypts its cookies for security, so this system would actually be a simplistic<a name="note-Sun-Feb-10-150814EST-2013"></a>[|15|](#foot-Sun-Feb-10-150814EST-2013) but secure way of maintaining user account as long as we used it over HTTPS.

Lets see, where were we. Oh, right, all those functions beginning with `res` in the `Handlers` and `Main` modules aren't built-ins. They're defined in a generically named `Util` module.

### <a name="util" href="#util"></a>Util

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Util ( resOk, res404, resError, resNO, resFile, resIxItems, serveStatic
            , resPlaceholder
            , extractOptional, withParams, withPostParams 
            , maybeRead) where

import Data.String (fromString)
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS

import Data.IxSet (IxSet(..), Proxy(..), toAscList )

import Network.Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.HTTP.Types (ok200, unauthorized401, status404)

import Control.Monad (sequence, liftM)
import Control.Monad.Trans.Resource (ResourceT)

import TypeSynonyms
import Model

resIxItems :: IxSet Item -> RES
resIxItems body = resOk $ toAscList (Proxy :: Proxy ItemStatus) $ body

resOk :: (ToJSON a) => a -> RES
resOk body = return $ responseLBS ok200 [] $ encode body

resNO :: RES
resNO = resError "NO -_-"

res404 :: RES
res404 = return $ responseLBS status404 [] $ fromString "Not Found"

resPlaceholder :: RES
resPlaceholder = return $ responseLBS status404 [] $ fromString "Not implemented yet"

resError :: String -> RES
resError message = return $ responseLBS unauthorized401 [] $ fromString message

resFile :: BS.ByteString -> FilePath -> RES
resFile contentType filename = return $ ResponseFile ok200 [("Content-Type", contentType)] filename Nothing

serveStatic :: Text.Text -> Text.Text -> RES
serveStatic subDir fName = 
  case sub of
    "js" -> serve "text/javascript"
    "css" -> serve "text/css"
    "img" -> serve "image/png"
    _ -> res404
  where serve mimeType = resFile mimeType $ concat ["static/", sub, "/", Text.unpack fName]
        sub = Text.unpack subDir

withPostParams :: Request -> [BS.ByteString] -> ([String] -> RES) -> RES
withPostParams req paramNames fn = do
  (params, _) &lt;- parseRequestBody lbsBackEnd req
  withParams params paramNames fn

withParams :: BSAssoc -> [BS.ByteString] -> ([String] -> RES) -> RES
withParams params paramNames fn = 
  case extractParams params paramNames of
    Just paramVals -> 
      fn paramVals
    Nothing ->
      resError $ concat ["Need '", paramsList, "' parameters"]
      where paramsList = BS.unpack $ BS.intercalate "', '" paramNames

extractOptional :: BSAssoc -> [BS.ByteString] -> [Maybe String]
extractOptional  params paramNames = map lookunpack paramNames
  where lookunpack k = do
          res &lt;- lookup k params
          return $ BS.unpack res

extractParams :: BSAssoc -> [BS.ByteString] -> Maybe [String]
extractParams params paramNames = do
  res &lt;- allLookups params paramNames
  return $ map BS.unpack res

maybeRead :: Read a => Maybe String -> Maybe a
maybeRead str = do
  res &lt;- str
  return $ read res

allLookups :: Eq a => [(a, a)] -> [a] -> Maybe [a]
allLookups assoc keys = sequence $ map (\k -> lookup k assoc) keys
```

The response functions seem fairly self-explanatory. `resOk` constructs a standard HTTP 200 response, `resIxItems` takes an `IxSet Items` and constructs a response by JSON-encoding it, and the error handlers each return a 404 or perhaps 401 with some error message. The `resPlaceholder` was something I used as a<a name="note-Sun-Feb-10-150826EST-2013"></a>[|16|](#foot-Sun-Feb-10-150826EST-2013) placeholder in the various `routes` functions while writing the final handlers. I think the only place a call to it still exists is in the favicon handler.

`withPostParams` and more generally `withParams` are functions that call functions using the output of serial `lookup` calls, which would be a pain in the ass to do manually. `maybeRead` is exactly what it says on the tin; it's a wrapper around Haskell's polymorphic `read` function wrapped so that it can deal with `Maybe String` rather than `String` input.

That's ... really it. I'm struggling to describe these a bit more than I usually do because the type signatures tell you a pretty significant amount of what you need to know about a given function. Not everything, obviously, but you'd be surprised how many times I've cut "`foo` is self-explanatory" from this write-up.

The last module left is the model; the one that actually takes all this information and stores it in some way.

### <a name="the-model" href="#the-model"></a>The Model

Before we dive into the code on this one, I want to highlight two things.

First, I initially did my best to separate the model entirely from the rest of the logic. Going so far as to define "external" API functions to call from other modules. That didn't last. Using naked `query'` and `update'` caused some type issues that I'm still not entirely clear about, and the code separating the database primitives from other functions was almost tripling the total size of the model. When modularity costs that much, I prefer to chuck it in a bin and resign myself to re-writing most of the application if I need to change out my database engine.

Second, this isn't the first back-end I tried using<a name="note-Sun-Feb-10-150830EST-2013"></a>[|17|](#foot-Sun-Feb-10-150830EST-2013). Before settling on AcidState, I tried out the haskell-MongoDB interface and `[hdbc-sqlite3](http://hackage.haskell.org/package/HDBC-sqlite3-2.3.3.0)`, neither of which impressed me much<a name="note-Sun-Feb-10-150832EST-2013"></a>[|18|](#foot-Sun-Feb-10-150832EST-2013). The Mongo interface just plain does not fit with the Haskll way of doing things. It's a massive, loosely structured, JSON-based key/value store, and since my application mainly responds to clients using JSON feeds, I figured that would be a good fit. One problem is that it turns out that `aeson`<a name="note-Sun-Feb-10-150835EST-2013"></a>[|19|](#foot-Sun-Feb-10-150835EST-2013) has a fundamentally different type architecture than `mongodb`s BSON, which means that converting between them is ... lets go with ["non-trivial"](http://stackoverflow.com/questions/7996140/haskell-correct-way-to-map-bson-to-json-where-to-put-code). The other big problem is that doing k/v lookups in Haskell is harder than dealing with native Haskell types, which means that the easiest way of using Mongo here would have been to define a type, and then specify how it gets encoded/decoded to both JSON and BSON. Given that I'm pretty used to [pymongo](http://api.mongodb.org/python/current/) and [Monger](http://clojuremongodb.info/), that's a lot more work than I was expecting. The `hdbc` interface was slightly better, since relational databases assume some up-front modeling, and slightly worse, since it expected me to write string-template based queries. Both external options required dealing with the actual DB through an external connection, both required a conversion step from associative lists before being converted to JSON, and both seemed to expect me to perform that conversion step.

[AcidState](http://hackage.haskell.org/package/acid-state-0.8.2) didn't. It serializes most native Haskell types<a name="note-Sun-Feb-10-150838EST-2013"></a>[|20|](#foot-Sun-Feb-10-150838EST-2013).

Without further ado

```haskell
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
 
module Model ( initialDB
             , GoGetDB(..), Account(..), Item(..), ItemStatus(..)
             , NewAccount(..), UpdateAccount(..), AccountByName(..), GetAccounts(..)
             , NewItem(..), DeleteItem(..), ChangeItem(..) ) where

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (liftIO)
import Control.Monad.IO.Class (MonadIO)

import Data.Acid (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Advanced (update', query')
import Data.Data (Data, Typeable)
import Data.IxSet (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet, insert, delete, toAscList, updateIx )
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as Text
import Data.ByteString.Char8 (ByteString, pack)
import Data.Maybe
import Data.Aeson

import Crypto.Scrypt (EncryptedPass(..), Pass(..), defaultParams, encryptPass, verifyPass)

---------- Base types (for IxSet and Account components)
newtype AccountId = AccountId { unAccountId :: Integer } deriving (Eq, Ord, Data, Show, Enum, Typeable, SafeCopy)

---------- Item-Related types
data ItemStatus = Need | Got deriving (Eq, Ord, Data, Enum, Read, Show, Typeable)
deriveSafeCopy 0 'base ''ItemStatus

data Item = Item { itemName :: String, itemComment :: String, itemStatus :: ItemStatus, itemCount :: Integer } deriving (Eq, Ord, Show, Data, Typeable)
deriveSafeCopy 0 'base ''Item
instance Indexable Item where
  empty = ixSet [ ixFun $ (:[]) . itemName
                , ixFun $ (:[]) . itemStatus
                , ixFun $ (:[]) . itemCount
                ]
instance ToJSON Item where
  toJSON (Item name comment status count) = object [ "name" .= name
                                                   , "comment" .= comment
                                                   , "status" .= show status
                                                   , "count" .= count 
                                                   ]

---------- Account
data Account = Account { accountId :: AccountId
                       , accountName :: String 
                       , accountPassphrase :: ByteString
                       , accountItems :: IxSet Item
                       } deriving (Eq, Show, Data, Typeable) 

instance Ord Account where
  a `compare` b = (accountId a) `compare` (accountId b)

deriveSafeCopy 0 'base ''Account
instance Indexable Account where
  empty = ixSet [ ixFun $ (:[]) . accountId
                , ixFun $ (:[]) . accountName 
                ]

instance ToJSON Account where
  toJSON (Account id name _ items) = object [ "id" .= unAccountId id
                                            , "name" .= name
                                            , "items" .= toAscList (Proxy :: Proxy ItemStatus) items]

---------- DB root type
  --- This is declared so that acid-state has a top level element to store
data GoGetDB = GoGetDB { nextAccountId :: AccountId, accounts :: IxSet Account
                       } deriving (Show, Data, Typeable)
deriveSafeCopy 0 'base ''GoGetDB

initialDB :: GoGetDB
initialDB = GoGetDB { nextAccountId = AccountId 0, accounts = empty }

---------- Insertion Functions
newAccount :: String -> ByteString -> Update GoGetDB Account
newAccount name passphrase = do
  db@GoGetDB{..} &lt;- get
  let account = Account { accountId = nextAccountId
                        , accountName = name
                        , accountPassphrase = passphrase
                        , accountItems = empty
                        }
  put $ db { nextAccountId = succ nextAccountId
           , accounts = insert account accounts 
           }
  return account

deleteItem :: Account -> Item -> Update GoGetDB ()
deleteItem acct item = do
  db@GoGetDB{..} &lt;- get
  put $ db { accounts = updateIx (accountId acct) removed accounts }
    where removed = acct { accountItems = delete item (accountItems acct)}

newItem :: Account -> Item -> Update GoGetDB ()
newItem acct item = do
  db@GoGetDB{..} &lt;- get
  put $ db { accounts = updateIx (accountId acct) added accounts }
    where added = acct { accountItems = insert item (accountItems acct) }

changeItem :: Account -> Item -> Update GoGetDB ()
changeItem acct item = do
  db@GoGetDB{..} &lt;- get
  put $ db { accounts = updateIx (accountId acct) changed accounts }
    where changed = acct { accountItems = updateIx (itemName item) item (accountItems acct)}

updateAccount :: Account -> Update GoGetDB ()
updateAccount u = do
  db@GoGetDB{..} &lt;- get
  put $ db { accounts = updateIx (accountId u) u accounts }

---------- Query Functions
getAccounts :: Query GoGetDB [Account]
getAccounts = do
  GoGetDB{..} &lt;- ask
  return $ toAscList (Proxy :: Proxy AccountId) accounts

getAccount :: (Typeable a) => a -> Query GoGetDB (Maybe Account)
-- separate so we can get accounts by something else at some point in the future
getAccount ix = do
  GoGetDB{..} &lt;- ask
  return $ getOne $ accounts @= ix

accountByName :: String -> Query GoGetDB (Maybe Account)
accountByName name = getAccount name

makeAcidic ''GoGetDB [ 'newAccount, 'newItem, 'deleteItem, 'changeItem, 'updateAccount, 'accountByName, 'getAccounts ]
```

The temptation is really strong to say "this is self-explanatory", but that's only true if you've also gone through the [relevant Happstack Crash Course section](http://happstack.com/docs/crashcourse/AcidState.html). Basically, AcidState is a Haskell-native noSQL data store. You define Haskell types that represent stuff you want to store, and it serializes them to disk through an interface that looks pretty close to [the state monad](http://www.haskell.org/haskellwiki/State_Monad) if you squint hard enough.

I want to draw your attention to a few things.

```haskell
---------- Account
data Account = Account { accountId :: AccountId
                       , accountName :: String 
                       , accountPassphrase :: ByteString
                       , accountItems :: IxSet Item
                       } deriving (Eq, Show, Data, Typeable) 

instance Ord Account where
  a `compare` b = (accountId a) `compare` (accountId b)

deriveSafeCopy 0 'base ''Account
instance Indexable Account where
  empty = ixSet [ ixFun $ (:[]) . accountId
                , ixFun $ (:[]) . accountName 
                ]

instance ToJSON Account where
  toJSON (Account id name _ items) = object [ "id" .= unAccountId id
                                            , "name" .= name
                                            , "items" .= toAscList (Proxy :: Proxy ItemStatus) items]
```

This is the full definition of the `Account` type. The `ToJSON` declaration allows `aeson` to serialize this type for the front-end, and the `Indexable` class lets `IxSet` construct collections of `Account`s. `IxSet` itself is a multi-indexing set implementation that I'm using to store both the accounts and shopping list items for this project.

The `deriveSafeCopy` call there is actually a `TemplateHaskell` invocation, and not a regular function<a name="note-Sun-Feb-10-150855EST-2013"></a>[|21|](#foot-Sun-Feb-10-150855EST-2013). Basically what that means is that this call will be resolved at compile time; in that sense, TemplateHaskell is a very restricted answer to Lisp macros.

Next, note the type of `accountPassphrase`. If you take a look at the `login` handler from earlier, you'll notice something a bit odd.

```haskell
login :: DB -> InsertFN -> String -> String -> RES
login db sessionInsert name passphrase = do 
  res &lt;- query' db $ AccountByName name
  case res of
    Just user -> case verifyPass defaultParams (Pass $ BS.pack passphrase) pass of
      (True, _) -> do
        sessionInsert "user" $ accountName user
        resOk user
      _ -> resNO
      where pass = EncryptedPass $ accountPassphrase user
    _ -> resNO
```

We're storing a `ByteString` in the database, but converting it to/from the type `EncryptedPass`. We don't have to do that with other types, but the developers who wrote Haskell's `scrypt` library didn't bother deriving `Typeable` for their types. There's quite likely a way for me to derive it manually, but that seems like more trouble in this particular instance. I just wanted to point out that even though we're doing type conversion to fit something into an AcidState DB here, that's not the ideal case, and you typically don't have to.

Finally, note that except for `initialDB`, all the exports from the `Model` module are types rather than functions

```haskell
module Model ( initialDB
             , GoGetDB(..), Account(..), Item(..), ItemStatus(..)
             , NewAccount(..), UpdateAccount(..), AccountByName(..), GetAccounts(..)
             , NewItem(..), DeleteItem(..), ChangeItem(..) ) where
```

If you take a look at any `query'` or `update'` call in the handlers, you'll note that they work by passing arguments to one of these types. What's actually happening is that AcidState is forward-journaling your requests so that it can fulfill the ACID guarantees. The trouble is that functions aren't inherently serializable in Haskell. So what it expects you to do is define the appropriate database functions, then use `makeAcidic` to derive the relevant, serializable types.

That about does it for the back end. I was going to go over the client-side code too, but this piece is getting quite long already. I'll likely write a follow-up to show you how I ended up actually calling the JSON-based handlers I describe above, but in the meantime, you'll have to check out [goget.js](https://github.com/Inaimathi/goget/blob/master/static/js/goget.js) and [index.html](https://github.com/Inaimathi/goget/blob/master/static/index.html) in the [static folder](https://github.com/Inaimathi/goget/tree/master/static) of the [github project](https://github.com/Inaimathi/goget).


* * *
##### Footnotes

1 - <a name="foot-Sun-Feb-10-150629EST-2013"></a>[|back|](#note-Sun-Feb-10-150629EST-2013) - Which has progressed to three-month-old status, in case you care.

2 - <a name="foot-Sun-Feb-10-150633EST-2013"></a>[|back|](#note-Sun-Feb-10-150633EST-2013) - Or whatever JS framework the server-side framework team picked out.

3 - <a name="foot-Sun-Feb-10-150637EST-2013"></a>[|back|](#note-Sun-Feb-10-150637EST-2013) - So good luck getting a front-end specialist in later.

4 - <a name="foot-Sun-Feb-10-150641EST-2013"></a>[|back|](#note-Sun-Feb-10-150641EST-2013) - So any changes, regardless how trivial, actually need a re-compile and re-run on the final server.

5 - <a name="foot-Sun-Feb-10-150645EST-2013"></a>[|back|](#note-Sun-Feb-10-150645EST-2013) - Which shouldn't surprise you in the least: this just in languages that [affect the way you think about programming](http://en.wikiquote.org/wiki/Alan_Perlis#Epigrams_on_Programming.2C_1982) expect you to think differently about programming.

6 - <a name="foot-Sun-Feb-10-150651EST-2013"></a>[|back|](#note-Sun-Feb-10-150651EST-2013) - Modulo the obvious state problems you have from potentially having some intermediary values defined in the image.

7 - <a name="foot-Sun-Feb-10-150655EST-2013"></a>[|back|](#note-Sun-Feb-10-150655EST-2013) - That [SO question](http://stackoverflow.com/questions/14721720/ambiguous-type-variable-in-acidstate-functions) has an example in the [answer](http://stackoverflow.com/a/14738171)s' comments; the type `EventResult` is reported as belonging to the module `Data.Acid.Common`, but that file actually doesn't exist. What's actually happening is that `Common` is a hidden module in the `AcidState` project, and another module is responsible for exporting its symbols. I didn't know this just from looking. The reason it matters is that when you want to make a type signature explicit by *importing* the relevant module, GHCi will tell you where a given type is **defined** and not where it's **exported**. Fun times.

8 - <a name="foot-Sun-Feb-10-150702EST-2013"></a>[|back|](#note-Sun-Feb-10-150702EST-2013) - Which kind of makes sense, because conceptually speaking, a purely functional REPL for a lazy language would more or less have to be implemented in the IO monad.

9 - <a name="foot-Sun-Feb-10-150705EST-2013"></a>[|back|](#note-Sun-Feb-10-150705EST-2013) - If slightly clunkier than in CL or Python.

10 - <a name="foot-Sun-Feb-10-150716EST-2013"></a>[|back|](#note-Sun-Feb-10-150716EST-2013) - There'll be more on that later, obviously. Do note that this is only a reasonable thing to do because we only use one database class for our model; if we used several, we'd need to figure something else out. To be fair though, I'm having a hard time imagining a situation that would call for using several DB classes in a single project.

11 - <a name="foot-Sun-Feb-10-150754EST-2013"></a>[|back|](#note-Sun-Feb-10-150754EST-2013) - Meaning lookup has been done and validated for it.

12 - <a name="foot-Sun-Feb-10-150758EST-2013"></a>[|back|](#note-Sun-Feb-10-150758EST-2013) - Meaning we've already collapsed the waveform and made sure that the user wants to `need` an existing item, otherwise we'd be expecting a `Maybe Item` here instead.

13 - <a name="foot-Sun-Feb-10-150808EST-2013"></a>[|back|](#note-Sun-Feb-10-150808EST-2013) - In fact, I've been meaning to write a piece comparing the two, I just haven't gotten around to it.

14 - <a name="foot-Sun-Feb-10-150811EST-2013"></a>[|back|](#note-Sun-Feb-10-150811EST-2013) - Which you still shouldn't count as a flat out recommendation, but I am using it, and I do intend to deploy this, so draw what conclusions you like.

15 - <a name="foot-Sun-Feb-10-150814EST-2013"></a>[|back|](#note-Sun-Feb-10-150814EST-2013) - We don't do any kind of throttling on login, aside from the complexity of the `scrypt` algorithm itself, and we don't check registration requests for automation with [recaptcha](http://www.google.com/recaptcha) or similar. I'm not sure how I feel about the first, while the second seems entirely unnecessary for an acount that doesn't allow sending any kind of email, or doing anything other than managing associated data.

16 - <a name="foot-Sun-Feb-10-150826EST-2013"></a>[|back|](#note-Sun-Feb-10-150826EST-2013) - Surprise.

17 - <a name="foot-Sun-Feb-10-150830EST-2013"></a>[|back|](#note-Sun-Feb-10-150830EST-2013) - Which, on reflection, is probably why I over-estimated the need to switch out databases at first.

18 - <a name="foot-Sun-Feb-10-150832EST-2013"></a>[|back|](#note-Sun-Feb-10-150832EST-2013) - Although I really could have put a project this small together with either if I felt like it.

19 - <a name="foot-Sun-Feb-10-150835EST-2013"></a>[|back|](#note-Sun-Feb-10-150835EST-2013) - Haskell's main JSON encoding library.

20 - <a name="foot-Sun-Feb-10-150838EST-2013"></a>[|back|](#note-Sun-Feb-10-150838EST-2013) - In order to serialize a type, you need to `derive` `Typeable` and `SafeCopy`. They're both trivial tasks for your own types, assuming you've got `TemplateHaskell` on, but are non-trivial for types you're including from modules you didn't write. You'll see an example of this when you see how I store encrypted passwords later on. That's the only type I needed to massage myself though; had I used an option other than AcidState, I'd have had to do the same for all of them.

21 - <a name="foot-Sun-Feb-10-150855EST-2013"></a>[|back|](#note-Sun-Feb-10-150855EST-2013) - You can tell because of the `'` and `''` argument prefixes.
