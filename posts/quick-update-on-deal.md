[Deal](https://github.com/Inaimathi/deal) proceeds apace.

I've spent the past while putting together a minimal, single-threaded asynchronous server to simplify the deployment process. Almost done, and you can see the progress on [this branch](https://github.com/Inaimathi/deal/tree/house-server-port) in [this subdirectory](https://github.com/Inaimathi/deal/tree/house-server-port/house). The remaining stuff left ToDo is:

**Better Errors.** I need to put together an appropriate assertion mechanism. Straight up `assert` works fine in a multi-threaded context, but does some mean things when you've only got the one thread. Normally, it wouldn't be that big a deal, but I have to special-case my `handler-case` statements for `SIMPLE-ERROR` in order to allow shell interruption. Unfortunately, they're both conditions of type `simple-error`, which means that if I do it naively, I either let both or neither through. What I'm planning to do is define a macro named something like `http-assert`, which will throw a type of error I can then safely convert into [an HTTP-`400` response](http://en.wikipedia.org/wiki/List_of_HTTP_status_codes#4xx_Client_Error).

**Basic Static Files.** Currently, I'm serving static files through `nginx` only. Which is the efficient way of doing it. However, one use-case I'm thinking of for Deal is that of a small, geographically disparate team setting up a private server for themselves. It's kind of a pain in the ass to have to set up a reverse proxy for that situation, so it would be nice if [House](https://github.com/Inaimathi/deal/tree/house-server-port/house) provided a basic file handler for people to use.

That's ... going to get complicated though. A first crack at the implementation is [here](https://github.com/Inaimathi/deal/blob/house-server-port/house/util.lisp#L54-L90) and [here](https://github.com/Inaimathi/deal/blob/house-server-port/house/house.lisp#L196-L220). That only works for text files so far<a name="note-Sun-Dec-01-230324EST-2013"></a>[|1|](#foot-Sun-Dec-01-230324EST-2013), and it only works for a laughably small number of mimetypes. A more complete map can be found [here](http://www.stdicon.com/mimetypes) or [here](http://svn.apache.org/viewvc/httpd/httpd/branches/2.2.x/docs/conf/mime.types?view=annotate), but I'm not going to be anywhere near as thorough; remember, this is an edge case. This lightweight server is *not* in the business of serving out static files in an efficient manner. *That's* what things like `nginx` are for, and I've got no doubt they're doing a better job than I could.

**Touch Ups** Sessions don't expire yet. And when they do, I'll want to give them the same kind of behavior hooks that I've got going for `new-session`. There's also the non-trivial matter of porting the rest of the Deal system to work better with the House server, but I get the feeling I'm most of the way there already.

Famous last words, right?


* * *
##### Footnotes
1 - <a name="foot-Sun-Dec-01-230324EST-2013"></a>[|back|](#note-Sun-Dec-01-230324EST-2013) - I'm still trying to iron out kinks; in particular there seems to be some kind of character encoding issue left in the way that I just can't get my head around. I'll be [asking on SO](http://stackoverflow.com/questions/20320540/serving-static-files-with-common-lisp) shortly.
