I'll be posting an incremental StrifeBarge update later this week, but I wanted to think aloud about a particular piece of it first. My last article concluded with a paragraph which included the action-item

figure out a good way to periodically notify clients about new developments in the game

Don't worry if you missed it, that post may just have shatter the "most words" record for Langnostic, so it's perfectly understandable if you just read up the point where I begin talking about the code and then went to do something else. I quote it above because that quote just about sets the stage for this.

It turns out that periodic notifications from the server are still a pain in the ass. Your options, as of almost-March 2012 are [Websockets](http://dev.w3.org/html5/websockets/), [Comet](http://en.wikipedia.org/wiki/Comet_(programming)) (aka Long Poll, aka (God help me) Reverse Ajax), [Ajax Polling](http://ajaxpatterns.org/Periodic_Refresh) and HTML5 [Server-Sent-Events](http://www.html5rocks.com/en/tutorials/eventsource/basics/).

## Websockets

seem to have gotten the most press, but have so far failed to materialize. If you're familiar with network programming<a name="note-Thu-Feb-23-154642EST-2012"></a>[|1|](#foot-Thu-Feb-23-154642EST-2012), this will be intuitive to you. Instead of relying on the standard request/response architecture of the web, a client and server do a handshake and establish something like a traditional, secure, bi-directional socket through which they can send each other data. In theory, that completely eliminates the need for any of the other methods. In practice, I'm not convinced.

The big problem is that the word "secure" in the previous sentence should have been in quotes, because that's been a big point of contention among implementations. Websockets seem to be, right now, where JavaScript was circa 2002. The various clients are all doing crazy or semi-crazy things their own way, which means that a server either has to make peace with the fact that a large number of visitors won't be using the tool correctly, or it has to try to disambiguate between individual *versions* of various browsers and provide a tailored protocol for most of them. Fun times, huh?

The first place I heard of this concept was [way back when Joe Armstrong posted](http://armstrongonsoftware.blogspot.com/2009/12/comet-is-dead-long-live-websockets.html) a<a name="note-Thu-Feb-23-154647EST-2012"></a>[|2|](#foot-Thu-Feb-23-154647EST-2012) example of Websocket use with YAWS, outright proclaiming the death of all the other options. I'm going to have to respectfully disagree, three years later. Of the big language implementations out there right now, only node.js has [a decent solution](http://socket.io/) for Websocket use. Essentially, they have a server and client framework that simplifies the interface, and provides automatic fallback behavior for clients that speak an older version of the protocol, or [don't speak it at all](http://www.microsoft.com/download/en/details.aspx?id=43), or [speak it but disable it by default](http://www.mozilla.org/en-US/firefox/4.0b9/releasenotes/). Worryingly, the ultimate fall-through is a Flash applet that establishes that same socket connection, which means some people won't be getting this either, but at least it works most of the time. No one else seems to have thought it out quite as far<a name="note-Thu-Feb-23-154658EST-2012"></a>[|3|](#foot-Thu-Feb-23-154658EST-2012).

In any case, this is a decent choice where you need true bi-directional communication, but it seems like implementing it here would cause me some unnecessary headaches, and I don't think turn-based games strictly require it.

## Comet

This is just a bending of the standard request/response protocol that the web is built out of. Usually, the client sends a request and the server responds to it right away, either with the requested information or an error code. "Comet" is a name for the situation where the server instead sits on the request until there's something new to send over, at which point it responds and the client immediately sends a new request that the server will respond to at its leisure. That's actually a pretty good option, except that I happen to be using a server<a name="note-Thu-Feb-23-154704EST-2012"></a>[|4|](#foot-Thu-Feb-23-154704EST-2012) that spawns a new thread per connection. In a Comet situation, this gets out of hand, because you essentially have a thread running *constantly* per user<a name="note-Thu-Feb-23-154716EST-2012"></a>[|5|](#foot-Thu-Feb-23-154716EST-2012). If I were running a single threaded server, this may be a better option, but as it stands, it seems like I'd have to do a *lot* more work for what I was hoping would be a simple project. So, no dice here either, sadly.

## Ajax Polling

I'm reasonably sure everyone who cares knows what this is by now. You have a specific page built to send out updates, and each client has code that looks something like

```javascript
setInterval(5000, "updateFoo()");

function updateFoo(){
    $.get("/update-url", function (data) {
              $("#result").html(data);
          });
}
```

The end result being that you can fake bi-directional communication by just having the client poke the server repeatedly and integrate data from the responses as it goes. The only issue with this approach is the overhead; go ahead and take a look at [this breakdown of the process](http://en.wikipedia.org/wiki/XMLHttpRequest). Calling complexity aside<a name="note-Thu-Feb-23-154755EST-2012"></a>[|6|](#foot-Thu-Feb-23-154755EST-2012), by my count, a request ends up transferring twice and a bit the obvious amount of data involved<a name="note-Thu-Feb-23-154808EST-2012"></a>[|7|](#foot-Thu-Feb-23-154808EST-2012). Some issues also arise from naive use of the method, which I'll get into with the final option I considered.

## Server-Sent-Events

are basically formalized, lightweight Ajax polling with a few small benefits. The bad part is that you're still basically instructing the client to poke the server at a given interval, but the response is structured differently. Something like

```
data: Foo bar baz
```

with options, instead of the giant XML response. The options include multi-lining the message<a name="note-Thu-Feb-23-154814EST-2012"></a>[|8|](#foot-Thu-Feb-23-154814EST-2012)

```
data: Foo bar
data: baz
```

providing each message with an identifier for synchronization purposes

```
id: 1
data: Foo bar
data: baz
```

letting the server specify when the next ping should happen

```
retry: 10000
data: Stop bothering me
```

and specifying event types

```
event: question
data: How I parse HTML with regular expression
event: deathThreat
data: Fuck off and die
```

Putting it all together, this communication method seems to be passable for writing turn-based web games.

```
id: 2
event: join
data: Bazmonkey
event: shot
id: 3
data: { "player" : "Bazmonkey", "result": "miss", "x" : "10", "y" : "32" }
event: turn
id: 4
data: You
event: shot
id: 5
data: { "player" : "You", "result": "hit", "x" : "23", "y" : "14" }
event: turn
id: 6
data: Bazmonkey

```

The `id` message is automatically used by the client to sync messages<a name="note-Thu-Feb-23-154832EST-2012"></a>[|9|](#foot-Thu-Feb-23-154832EST-2012), the `event` message can be used to set up different client behavior based on what kind of event happened on the server, and the `retry` message gives the server a way to tap out if too many users are pile-driving it at the moment. It still doesn't "solve" the fundamental asymmetry between client and server in HTTP, and it will never be as responsive as an actual socket connection, but it seems to be a Good Enough™ solution that address most of the issues I'd be thinking about if I tried to implement [StrifeBarge](https://github.com/Inaimathi/strifebarge) using Ajax polling.

In addition to working on StrifeBarge for the next little while, I'll also be poking semi-seriously at [node.js](http://nodejs.org/)<a name="note-Sun-Feb-26-032221EST-2012"></a>[|10|](#foot-Sun-Feb-26-032221EST-2012), so I may end up using websockets for *something*, but SSE wins it for the time being.

* * *
##### Footnotes

1 - <a name="foot-Thu-Feb-23-154642EST-2012"></a>[|back|](#note-Thu-Feb-23-154642EST-2012) - As opposed to just web-programming.

2 - <a name="foot-Thu-Feb-23-154647EST-2012"></a>[|back|](#note-Thu-Feb-23-154647EST-2012) - Since thoroughly outdated.

3 - <a name="foot-Thu-Feb-23-154658EST-2012"></a>[|back|](#note-Thu-Feb-23-154658EST-2012) - Though, as usual, someone has taken it upon themselves to [clone relevant bits in Common Lisp](https://github.com/e-user/hunchensocket), so there.

4 - <a name="foot-Thu-Feb-23-154704EST-2012"></a>[|back|](#note-Thu-Feb-23-154704EST-2012) - Hunchentoot, at the moment.

5 - <a name="foot-Thu-Feb-23-154716EST-2012"></a>[|back|](#note-Thu-Feb-23-154716EST-2012) - Typically, each thread lives just long enough to send a response, but since we're `sleep`ing on each Comet request, they pile up fast.

6 - <a name="foot-Thu-Feb-23-154755EST-2012"></a>[|back|](#note-Thu-Feb-23-154755EST-2012) - Which has largely been smoothed out by modern JS frameworks.

7 - <a name="foot-Thu-Feb-23-154808EST-2012"></a>[|back|](#note-Thu-Feb-23-154808EST-2012) - Since the raw response contains that data twice, and HTTP headers are sent each way.

8 - <a name="foot-Thu-Feb-23-154814EST-2012"></a>[|back|](#note-Thu-Feb-23-154814EST-2012) - Though I can't find a line limit anywhere in [the spec](http://dev.w3.org/html5/eventsource/), so that seems pointless unless you plan to manually format text you're sending in this fashion.

9 - <a name="foot-Thu-Feb-23-154832EST-2012"></a>[|back|](#note-Thu-Feb-23-154832EST-2012) - So if for some reason your connection blows, you won't miss the fact that your opponent fired, *or* end up getting 27 separate notifications of the same event.

10 - <a name="foot-Sun-Feb-26-032221EST-2012"></a>[|back|](#note-Sun-Feb-26-032221EST-2012) - Thanks in part to some links from a friend from the Toronto Lisp Group, if you'll believe that.
