 So you already know I'm [building a custom HTTP server for Deal](http://langnostic.blogspot.ca/2013/10/deal-journal-interlude-two-being.html). A question has arisen in my mind: how customized do I want it?

My initial assumption was that I'd just make it the usual general purpose HTTP server, with a few pieces focused on my end goal, but now that I'm waist deep in the guts of this thing, it occurs that I could take it further if I wanted to. For my current purposes, a relatively small subset of HTTP would do just fine. Here are the points I've noticed:


- Deal never runs into the situation where both a GET and a POST parameter have the same name. As a result, I can do the Hunchentoot-standard thing and mangle parameters so that `get-params` and `post-params` are actually kept in the same associative structure. That simplifies the structure of both `request` objects and of handler functions, at the cost of differentiating between parameter types.
- Because there's never an ambiguity between GET and POST parameters, I don't even really need to differentiate between GET and POST *requests*. That'll simplify the class tree of incoming requests. Specifically, it'll cap its depth at 1.
- There are only two handlers that allow session-less requests, the rest require that the requester have a session<a name="note-Sat-Nov-09-184009EST-2013"></a>[|1|](#foot-Sat-Nov-09-184009EST-2013), so what I could do is just make each request start up a session if a valid session token isn't passed in. That'll occasionally burn a few milliseconds of extra work<a name="note-Sat-Nov-09-184013EST-2013"></a>[|2|](#foot-Sat-Nov-09-184013EST-2013), but it'll remove the need to assert session presence at the application level.
- Because Deal is already targeting recent browsers<a name="note-Sat-Nov-09-184016EST-2013"></a>[|3|](#foot-Sat-Nov-09-184016EST-2013), there's no need to support any older version of HTTP.
- Finally, because there's *only one* handler that's going to need to handle extended connections, and the rest will always return immediately, I can more or less ignore request headers. In particular, I never care about `Connection`<a name="note-Sat-Nov-09-184019EST-2013"></a>[|4|](#foot-Sat-Nov-09-184019EST-2013), `Accept`<a name="note-Sat-Nov-09-184022EST-2013"></a>[|5|](#foot-Sat-Nov-09-184022EST-2013) or `Cache-Control`<a name="note-Sat-Nov-09-184025EST-2013"></a>[|6|](#foot-Sat-Nov-09-184025EST-2013).


Acting on each of these assumptions is going to narrow the usefulness of the server, but also significantly simplify it. The optimum is eluding me, though I suspect it might be "as simple as possible" given my end goal for this particular project. Interestingly, even if I decide to do the simple thing for each of the choices outlined above, the end result will still probably be a useful general-purpose game server.

The only really funny part is that, now that I've thought it through, implementing this system as a websocket server<a name="note-Sun-Nov-10-113904EST-2013"></a>[|7|](#foot-Sun-Nov-10-113904EST-2013) seems like it would yield an even simpler architecture. I'll save that one for later though. Step one: simple SSE+session based engine, make sure it works, *then* rip out its guts again and see what the other way looks like.

* * *
##### Footnotes

1 - <a name="foot-Sat-Nov-09-184009EST-2013"></a>[|back|](#note-Sat-Nov-09-184009EST-2013) - And most of *those* require that the session be associated with a particular, already-instantiated game table, but optimizing for that seems like it would be going too far.

2 - <a name="foot-Sat-Nov-09-184013EST-2013"></a>[|back|](#note-Sat-Nov-09-184013EST-2013) - When someone who *just* wants to watch rather than play first checks out a table.
3 - <a name="foot-Sat-Nov-09-184016EST-2013"></a>[|back|](#note-Sat-Nov-09-184016EST-2013) - We're using quite a few features of HTML5 to make the whole thing playable.

4 - <a name="foot-Sat-Nov-09-184019EST-2013"></a>[|back|](#note-Sat-Nov-09-184019EST-2013) - That one that I mentioned will be assumed `keep-alive`, while the rest assume the reverse.

5 - <a name="foot-Sat-Nov-09-184022EST-2013"></a>[|back|](#note-Sat-Nov-09-184022EST-2013) - You're expecting `text/html` on the main request, `text/event-stream` on the subscription and `application/json` on the rest. If you can't handle those, you won't be able to play in any case.
6 - <a name="foot-Sat-Nov-09-184025EST-2013"></a>[|back|](#note-Sat-Nov-09-184025EST-2013) - Everything other than the static file handlers always re-generates content; the static handlers don't need to change unless the files themselves have actually changed.

7 - <a name="foot-Sun-Nov-10-113904EST-2013"></a>[|back|](#note-Sun-Nov-10-113904EST-2013) - Which multiple people have already recommended to me, so I'm not taking credit for that particular insight.
