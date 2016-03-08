The *other* question mentioned last time was "How does [AcidState](http://acid-state.seize.it/) stack up against the other database back-ends?". So, here's a crack at the answer.

My benchmarking method was basically to port the [GoGet](https://github.com/Inaimathi/goget) back-end to [HDBC](http://hackage.haskell.org/package/HDBC) and [MongoDB](https://github.com/selectel/mongodb-haskell), then see how each one does at


- user insertion
- user listing
- item insertion
- user querying


All of this was done on a 64-bit Debian Wheezy machine running on top of a Core i3. Data for other platforms/architectures welcome, but I'm not going there myself. Without further ado:


- [SM](http://173.255.226.138/report.html) - Starting with empty tables/collections and dealing with user #42
- [MD](http://173.255.226.138/report-medium.html) - Starting with 1000 user records and dealing with user #789
- [LG](http://173.255.226.138/report-large.html) - Starting with 50000 user records and dealing with user #42
- [LG2](http://173.255.226.138/report-large2.html) - Starting with 50000 user records and dealing with user #45678
- [LG-O](http://173.255.226.138/report-large-O.html) - Starting with 50000 user records and dealing with user #45678, compiled with `ghc -O --make` instead of just `ghc --make`


These are hosted on my own server because Blogger doesn't seem to like the Criterion markup. You'll find the same files in [the codebase](https://github.com/Inaimathi/haskell-profile/tree/master/results) if you prefer viewing local copies.

You'll note that I only managed the tiny benchmark for MySQL, and it's absent from the rest; this is because the connection kept choking randomly, which is consistent with my real-world experience. Not pictured is the four or five attempts/clears/restarts that I had to pull before even getting the numbers for a 100 user corpus. No other database, including SQLite, did anything similar.

So lets do a rundown.

### Obvious


- The vast majority of time spent inserting a user goes to generating the `scrypt` hash. This is obvious because of the huge difference between inserting an item and inserting a user. And, really, this is what you want in a real-world scenario. It should take fairly significant resources to try a password so as to make brute-forcing them a poor option, but in hindsight I could have saved myself a lot of time and compute by cutting that portion of user insertion across the board for benchmarking purposes.
- The `ghc` optimization flag approximately halves most numbers, and improves AcidState lookups by about 5x.
- MongoDB consistently outperforms all comers when it comes to user insertion, and performs very well on sub-insertion with small data-sets. The `$push` directive seems to be much faster than merely popping a new top-level record in, which I assume is why it manages to take about 1/3 the time of the next contender until we get up to the 50k corpus.
- SQLite loses in every category at every corpus size, but not by as much as I was expecting. It's actually a pretty good little lightweight DB engine, assuming you don't need to support too many simultaneous requests or too much data.
- AcidState is an absolute fucking monster. The benchmarks it loses, it loses narrowly<a name="note-Sat-Mar-09-134319EST-2013"></a>[|1|](#foot-Sat-Mar-09-134319EST-2013), but the benchmarks it wins, it wins by an 8x or larger margin. Take special note that while the other engines bench in the high single/low double digit milliseconds, Acid consistently posts list and select numbers in the low double-digit *micro*seconds. Granted, insertion speed goes down a bit based on corpus size, but selection speed is always the same range of extremely low numbers. That's excellent for web applications, which tend to have a usage profile of "rare-ish insertions coupled with large and common lookups". It performs suspiciously well on selects. Well enough that I went back to GHCi and tried `mapM (getUserAcid acid) [40000..40050]` and `mapM_ (getUserAcid acid) [40000..45000]` on the large corpus, just to make sure it wasn't recording thunk time instead of actual result time. It isn't. An IxSet lookup is actually just that fast.


### Not-So Obvious

There isn't as big a difference in code size as I was expecting. Honestly, I thought AcidState would be much chunkier than the competition, but it only turns out to be the longest by about 10 lines. This might be because I was determined to work with Haskell-style type declarations in each of the models. The reasoning there was that I'd typically be wanting to pull out data then convert it to some external data format after the fact<a name="note-Sat-Mar-09-134323EST-2013"></a>[|2|](#foot-Sat-Mar-09-134323EST-2013), so getting a record in a canonical typed format was going to happen sooner or later anyway. This ends up working hardest against MongoDB, where conversion from very loosely-typed k/v pairs ends up looking something like

```haskell
itemFromMongo [nameField, commentField, statusField, countField] =
  Item { itemName =  name, itemComment = comment, itemStatus = status, itemCount = count }
  where Database.MongoDB.String n = value nameField
        name = Text.unpack n
        Database.MongoDB.String c = value commentField
        comment = Text.unpack c
        Database.MongoDB.String s = value statusField
        status = read $ Text.unpack s
        Int32 co = value countField
        count = fromIntegral co
```

...which is ugly as fuck. A couple of those lines could have been eliminated by declaring `itemName` and `itemComment` as `Text` rather than string, but that would only make it very slightly less ugly.

MySQL crashes like a champ. I use it in a couple of real applications, and I remember having configuration problems there too. It really seems to want a fresh connection each time you do any significant chunk of work, and that seems like it would slow the whole thing down further. Like I said, this is probably a misconfiguration somewhere, and I welcome help if someone wants to go over the tests again on a different machine, giving MySQL more airtime. For the benchmarks it completed, it performs marginally better than AcidState on insertion and very marginally better than SQLite on selection.

It is almost trivially easy to port between HDBC back-ends. You need to call a different `connect` function and pass it different arguments, but that's more or less it. The only other hiccup I ran into here is the different table creation syntax; SQLite barfs if you try to declare something `AUTO_INCREMENT`<a name="note-Sat-Mar-09-134330EST-2013"></a>[|3|](#foot-Sat-Mar-09-134330EST-2013), but MySQL requires the statement or leaves you to specify the `ID` manually. I'm not sure what the differences are between implementations of the SQL standard across other engines, but they seem minimal enough that hopping around wouldn't be difficult.

MongoDB really *really* doesn't mesh with the Haskell way of doing things. I already mentioned this in the first Not-So Obvious point, but I wanted to highlight it a bit more. This is not to say it's bad. In fact, it would be my top choice if not for the massive impedance mismatch it has with the language. My negative opinion may also be exacerbated by the fact that I've used it in Python and Clojure, where there are no such problems because both languages deal with loosely typed k/v pairs as their primary hash construct<a name="note-Sat-Mar-09-134336EST-2013"></a>[|4|](#foot-Sat-Mar-09-134336EST-2013). As always, it's possible that I'm doing it wrong, in which case, do point that out.

Finally, a hidden advantage that AcidState and to a lesser extent SQLite have is ease of deployment. The other engines all require some degree of setup beyond coding. MySQL needs an installed, running, properly configured server, with appropriate databases and users created, and your program needs to use the appropriate credentials when communicating. MongoDB needs an installed, running server<a name="note-Sat-Mar-09-134340EST-2013"></a>[|5|](#foot-Sat-Mar-09-134340EST-2013). SQLite just requires that the deployment machine have `libsqlite3.so` or `sqlite3.dll` as appropriate. You need to create your tables the first time, but that's it. AcidState doesn't even require that much. All you need to make sure of is that you have the AcidState Haskell library installed when you're compiling your program. The resulting binary has no external deps whatsoever, so you can just run it on any machine of the appropriate architecture and platform. Personally, I'd be willing to give up non-trivial amounts of performance for a simplified setup process, so I'm quite happy that the easiest DB to work with from that perspective is also benching at or near the top for everything, at every corpus size.

### Code Notes

That's all my thoughts on results; if that's all you were here for, you can safely disregard the rest of the article.

The code for these tests is [here](https://github.com/Inaimathi/haskell-profile), but it's very raw at the moment. I plan to write some docs and make it easier to run these tests shortly. There's just a couple of things I want to call attention to explicitly so I don't forget about them.

First, note that I'm making one connection and handing it to each function.

```haskell
main = do
  acid <- openLocalState Acid.initialDB
  mongo <- Mongo.newConn
  sqlite <- Database.HDBC.Sqlite3.connectSqlite3 "GoGetDB"
--  mysql <- Database.HDBC.MySQL.connectMySQL defaultMySQLConnectInfo
  defaultMain
    [
      benchBlock "AcidState" acid
      (insertUserAcid, insertItemAcid, getUserAcid, (\acid -> query' acid Acid.GetAccounts)),
      bgroup "HDBC"
      [
--        hdbcBlock "MySQL" mysql,
        hdbcBlock "SQLite" sqlite
      ],
      benchBlock "MongoDB" mongo
      (insertUserMongo, insertItemMongo, getUserMongo, Mongo.getAccounts)
    ]
  Mongo.close mongo
  Database.HDBC.disconnect sqlite
--  Database.HDBC.disconnect mysql
  createCheckpointAndClose acid
```

This is the recommended usage for AcidState and MongoDB, but I'm not entirely convinced it's the best approach for arbitrary SQL databases, or entirely sure how HDBC handles connection-pooling. The end result is, I think, to somewhat deflate the times attributed to using the HDBC back-ends.

Second, if you look at how the SQL database is organized

```haskell
createTables :: Connection -> IO [Integer]
createTables conn = withCommit conn q
  where q conn = mapM (\s -> run conn s [])
            ["CREATE TABLE accounts (id INTEGER PRIMARY KEY AUTO_INCREMENT, name VARCHAR(120), passphrase VARCHAR(250))",
             "CREATE TABLE items (user VARCHAR(120), name VARCHAR(120), comment VARCHAR(120), status VARCHAR(4), count INTEGER)"]
```

You'll see that I slightly re-configured the storage approach to match the back-end's relational model. Also, while the individual account selectors return a `Maybe Account`, `getAccounts` just does the naive SQL thing of `SELECT * FROM`

```haskell
getAccounts :: Connection -> IO [[SqlValue]]
getAccounts conn = withCommit conn q
  where q conn = quickQuery' conn "SELECT * FROM accounts" []

getAccountBy conn column value = withCommit conn q
  where qString = "SELECT * FROM accounts WHERE " ++ column ++ " = ?"
        q conn = do
          res <- quickQuery' conn qString [toSql value]
          case res of
            [] -> return $ Nothing
            (u@[_, SqlByteString name, _]:rest) -> do
              items <- getAccountItems conn $ unpack name
              return $ Just $ accountFromSql u items

accountByName :: Connection -> String -> IO (Maybe Account)
accountByName conn name = getAccountBy conn "name" name

accountById :: Connection -> Integer -> IO (Maybe Account)
accountById conn id = getAccountBy conn "id" id
```

The MongoDB back-end does the same sort of thing

```haskell
getAccounts pipe = do
  res <- run pipe $ find (select [] "accounts") >>= rest
  return $ case res of
    Right accounts -> accounts
    _ -> []

getAccountBy pipe property value = do
  res <- run pipe $ findOne $ select [property =: value] "accounts"
  return $ case res of
    Right (Just acct) -> Just $ accountFromMongo acct
    _ -> Nothing

accountById :: Pipe -> Integer -> IO (Maybe Account)
accountById pipe id = getAccountBy pipe "id" id

accountByName :: Pipe -> String -> IO (Maybe Account)
accountByName pipe name = getAccountBy pipe "name" name
```

That means that both `Mongo` and the `HDBC` back-ends should have a massive advantage over `Acid` for this particular function. Really, if I wanted to make it fair and get everyone to return the same data type, I'd have to write a `JOIN` for the `SQL` approach and `map` a conversion function over the whole thing. `Acid` gets that for free and, just in case I haven't pointed it out thoroughly enough yet, *still* schools all the rest on listing accounts.

Thirdly, I used manual auto-incrementing for MongoDB

```haskell
newAccount :: Val v => Pipe -> v -> v -> IO Account
newAccount pipe name pass = do
  Right ct <- run pipe $ count $ select [] "accounts"
  let u = ["id" =: succ ct, "items" =: ([] :: [Database.MongoDB.Value]), "name" =: name, "passphrase" =: pass]
  run pipe $ insert "accounts" u
  return $ accountFromMongo u
```

I don't think this adds too much overhead to user insertion, since the Mongo docs imply that `count` is not an expensive operation, but I thought I'd mention it. This is not how I'd do it in the real world, but I didn't feel like figuring out how to reliably predict a MongoDB ID hash for the purposes of benching it.

Now, to be fair, while the field is tilted slightly towards `HDBC`, the *task* actually favors noSQL data stores because of how `Account`s and `Item`s relate to one another. Where I'd have to pull some `JOIN` trickery in a relational database, a mere `IxSet` lookup gives me the same effect with AcidState, and a recursively nesting `Document` type does it for Mongo.

### Next Steps

What I'd really like at this point is some peer review. Either refinements to the tasks, or implementations for other database back-ends<a name="note-Sat-Mar-09-134353EST-2013"></a>[|6|](#foot-Sat-Mar-09-134353EST-2013), or data from different machines/environments, or general comments on approach/results. It would also be nice if someone did benchmarks with a large enough corpus that the entire thing didn't fit in memory. Remember, the question I'm trying to answer here is "How well does AcidState stack up against other data storage options in Haskell", and at this point the answer looks to be "It destroys them and pisses on the ashes". If that's not your experience, or if I missed some approach or edge case in my testing, it would be nice to find out before I start outright recommending it to everyone.

And that error checking is something I'll have to leave to the internet.

* * *
##### Footnotes

1 - <a name="foot-Sat-Mar-09-134319EST-2013"></a>[|back|](#note-Sat-Mar-09-134319EST-2013) - Except that MongoDB is significantly better at item insertion for data sets in the ~1k user range.

2 - <a name="foot-Sat-Mar-09-134323EST-2013"></a>[|back|](#note-Sat-Mar-09-134323EST-2013) - Probably JSON, for most of my purposes.

3 - <a name="foot-Sat-Mar-09-134330EST-2013"></a>[|back|](#note-Sat-Mar-09-134330EST-2013) - It just automatically does the Right Thing©™ with a `PRIMARY KEY` field.

4 - <a name="foot-Sat-Mar-09-134336EST-2013"></a>[|back|](#note-Sat-Mar-09-134336EST-2013) - If you've never tried [Clojure](http://clojure.org/) with [Monger](http://clojuremongodb.info/), incidentally, I suggest you stop reading now and go give it a shot. It is goddamned **glorious**.

5 - <a name="foot-Sat-Mar-09-134340EST-2013"></a>[|back|](#note-Sat-Mar-09-134340EST-2013) - I've heard that at higher traffic levels, Mongo tends to need more configuration and apparently assumes that it has the run of all server resources itself. I haven't run into such problems, and it seems like you could fix this by putting it on its own server in any case.

6 - <a name="foot-Sat-Mar-09-134353EST-2013"></a>[|back|](#note-Sat-Mar-09-134353EST-2013) - Specifically, I'm looking for pointers on how to get MySQL working properly, and I wouldn't mind someone putting together running code for PostgreSQL, but I won't be doing either myself.
