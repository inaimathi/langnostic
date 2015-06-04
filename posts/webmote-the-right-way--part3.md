I've had a, shall we say, *pretty busy* two weeks, but I still managed to do some half-way decent work on [Web Mote](https://github.com/Inaimathi/web-mote). The process is teaching me small things as I go, both about the language I'm currently using<a name="note-Thu-Oct-18-200653EDT-2012"></a>[|1|](#foot-Thu-Oct-18-200653EDT-2012), and about the architectural approach, so I wanted to organize them in my own mind.

### <a name="pythonisms" href="#pythonisms"></a>Pythonisms

First off, just to make sure I'm clear, Python is definitely in the "Popular" side of the [Powerful/Popular continuum I described a while ago](http://langnostic.blogspot.ca/2012/02/self-titled.html). If you're dealing with something that isn't a fundamentally unsolved problem, chances are there's a library that resolves it fairly well. Because the church of OO still seems to be going strong, you'll probably wind up needing to subclass one or two somethings to get the behavior you want, but there's still a lot to be said for just having Good Enough™© solutions lying around at your disposal.

A couple more odd syntactic corners are showing up as I do more involved Python code. Nothing huge, but I've had to look them each up at least once, so lets get this over with

## <a name="nested-loops" href="#nested-loops"></a>Nested Loops

Are broken out of with `return`. This shouldn't really come up very often<a name="note-Thu-Oct-18-200828EDT-2012"></a>[|2|](#foot-Thu-Oct-18-200828EDT-2012), but in case you ever need to, this is how you do it.

```python
def pollingLoop(foo):
    for a in foo:
        while True:
            if bar():
                baz()
            else:
                return "Done"
```

That `return` will return from `pollingLoop`, incidentally terminating both the `for` and the `while`. Again, it seems rare enough that I'm not about to complain for lack of more fine-grained flow control. The only place I could think of using this idiom off-hand is in a polling situation, which is [how I was originally using it](https://github.com/Inaimathi/web-mote/blob/master/player.py#L42).

## <a name="functional-shuffling" href="#functional-shuffling"></a>Functional Shuffling

The standard `random.shuffle` function is destructive, which typically isn't what you want when you're trying to be functional. Luckily, you can use `[sorted](http://docs.python.org/library/functions.html#sorted)` to fake a Good Enough™© functional shuffle by passing it a random `key`.

```python
import random

def shuffled(aList):
    return sorted(aList, key=lambda e: random.random())
```

## <a name="checking-membership" href="#checking-membership"></a>Checking Membership

The idea of `member?` is a primitive to me, but there's no such function in Python. You instead need to use the standalone `in` syntax.

```python
>>> 1 in [1, 2, 3, 4, 5]
True
>>> "Bob" in ["Alice", "Bradley", "Charles"]
False
>>> 
```

It's an infix boolean for some inscrutable reason, but it does the job, and is presumably *really fast* since it's a keyword rather than a function.

### <a name="separating-client-and-server" href="#separating-client-and-server"></a>Separating Client and Server

About half the point<a name="note-Thu-Oct-18-201323EDT-2012"></a>[|3|](#foot-Thu-Oct-18-201323EDT-2012) of Web Mote is doing some light experimentation on a particular architectural approach. I've made changes to the front-end which lets me play an entire directory<a name="note-Thu-Oct-18-201410EDT-2012"></a>[|4|](#foot-Thu-Oct-18-201410EDT-2012), and I'm beginning to ask myself what the correct way of separating that behavior is. There are options, and I'll start by outlining the way it's currently implemented<a name="note-Thu-Oct-18-201434EDT-2012"></a>[|5|](#foot-Thu-Oct-18-201434EDT-2012)

## <a name="semiclient" href="#semiclient"></a>Semi-Client

Client-side sends a target, which can be either a file or a folder, and the server handles playing it. A file is merely passed to the player, a folder gets searched recursively for all files it contains, and those files are then sent in sorted order to the player to be played one at a time, but note that this decision is made by the *server*.

It's semi-client because the client doesn't particularly care what message it's sending or what responding to it entails. For instance, if we're shuffling, it would be convenient to display the current file, and a list of previous/next files. In the Semi-Client architecture, the server would start up a player, then report this play queue order back to the client for display. This keeps clients somewhat interchangeable, since the current play queue can be fetched by anyone connecting in.

A problem this might raise later is that if we decide to change the behavior of the shuffle function, or add a playlist, we'll need to make extensive changes on both the server and client sides<a name="note-Thu-Oct-18-201648EDT-2012"></a>[|6|](#foot-Thu-Oct-18-201648EDT-2012). Further, the server and client need to synchronize in various non-trivial ways which complicates their communication even if we change nothing else.

## <a name="clientoriented" href="#clientoriented"></a>Client-Oriented

This solution would involve doing all relevant work on the client. We wouldn't send a target to the server, we'd send a filename. The way we'd handle playing or shuffling a directory would be by asking the server for its *deep* file list, potentially shuffling it on the client side, then sending the list off to the server for playing one file at a time.

Going down this path radically simplifies the server. It has to expect exactly three kinds of requests:


1.   Gimme the file list of `directory` (optionally, make it a deep list)
1.   Play `file` right now
1.   Send `command` to the running player


Ok, we do also need to be able to ping the client in some way to notify them that file is done playing, but that seems like it could be trivially done by long-polling the `play` request. If you want to get slightly fancier, for instance in the case where you want to be able to deal with multiple simultaneous clients, you can use SSEs or Ajax polling to send out a `done` signal when you need to. You may also need to support session/option-caching, but if you want to do it right, you'll probably be using cookies rather than any server-side storage options. Ideally, the server doesn't have to care about *anything* the client does or keeps track of.

The downside is that the client is suddenly expected to be very smart. If doing Ajax-based JSON communication didn't *already* commit you to mandatory JavaScript<a name="note-Thu-Oct-18-201802EDT-2012"></a>[|7|](#foot-Thu-Oct-18-201802EDT-2012), this technique would be the point of no return. Because if your client needs to be the smart component, it needs to be somewhat stateful, and it needs to manipulate its DOM in various ways. You still *could* write a minimal client that attaches to a server like this, but emphasis would be heavily on "minimal", both in terms of interactivity and in terms of available control options. The only other concern with this approach is that clients are suddenly not interchangeable; if I use my phone to queue up a playlist, then sit down at my laptop, I either need to keep my phone on, or I need to duplicate that playlist on my laptop in order to keep the media going.

## <a name="serveroriented" href="#serveroriented"></a>Server-Oriented

This solution involves a disproportionate amount of server-based work, and it's about as close as you can get to a traditional web site while still using the separated architecture. Your client can suddenly be almost as dumb as a post, only needing to be able to accept UI changes through JSON feeds. You store any kind of stateful information on the server, which means that you've got a central place to save playlists and such. A shuffle would be implemented more or less as in the Semi-Client solution, but it wouldn't bother streaming back state updates. Rather, the client would make periodic requests of the style "What are you doing now?" and display the results. The same thing would be true for playlists and similar behavior; we would store them in a server-side database somewhere and send status updates out to clients as requested.

It maintains a higher level of decoupling than the Semi-Client solution, and simplifies the client enough that a slightly clunkier, pure-HTML version starts looking feasible.

The main downside is that the server needs to send much more exhaustive readouts of its state. Which means more, and potentially more complex, handlers.

### <a name="decision-time" href="#decision-time"></a>Decision Time

I can't take the Server-oriented option seriously, because it would nudge the shape of the application much closer to a traditional web app. It might also introduce one or two dependencies across client and server as well as greatly complicating the server, and almost-significantly simplifying the client. This does not sound like a good trade.

The current codebase takes the Semi-Client approach, but I'm not too keen on keeping it because of the extra coupling it demands between client and server operations. Playing a list of files properly also needed a surprisingly large amount of work<a name="note-Thu-Oct-18-203423EDT-2012"></a>[|8|](#foot-Thu-Oct-18-203423EDT-2012).

The second option, the Client-oriented one seems like the correct approach. It complicates the client, but not excessively. It greatly simplifies the server, saving me from needing to deal with even basic multiprocessing concerns for the purposes of actually playing media. Finally, it keeps the client and server completely decoupled, making it even simpler to release new clients and keeping client data entirely out of the servers' reach.

Ok.

I'd better get on that.

* * *
##### Footnotes

1 - <a name="foot-Thu-Oct-18-200653EDT-2012"></a>[|back|](#note-Thu-Oct-18-200653EDT-2012) - That'd be Python at the moment. And some people at the [Hacklab](http://hacklab.to/) open houses asked, so I guess I'd better clarify again, I don't have any particular affection for Python. It's just everywhere and not horrible, so I figured I may as well. I'm still honestly attempting to line up a Haskell and CL port as soon as I can possibly manage it.

2 - <a name="foot-Thu-Oct-18-200828EDT-2012"></a>[|back|](#note-Thu-Oct-18-200828EDT-2012) - And in fact, the place where I used the idiom has been re-written such that the inner loop is in a secondary function.

3 - <a name="foot-Thu-Oct-18-201323EDT-2012"></a>[|back|](#note-Thu-Oct-18-201323EDT-2012) - The other half is split between making use of old, closed hardware I have lying around and fulfilling the next part of my personal crusade aimed at putting a web server in everything within my reach.

4 - <a name="foot-Thu-Oct-18-201410EDT-2012"></a>[|back|](#note-Thu-Oct-18-201410EDT-2012) - And at least theoretically shuffle it, but there isn't a front-end for that yet.

5 - <a name="foot-Thu-Oct-18-201434EDT-2012"></a>[|back|](#note-Thu-Oct-18-201434EDT-2012) - It's not necessarily *staying* this way, but if you do a checkout from [that github](https://github.com/Inaimathi/web-mote) as of this writing, this is what you're getting.

6 - <a name="foot-Thu-Oct-18-201648EDT-2012"></a>[|back|](#note-Thu-Oct-18-201648EDT-2012) - Which is precisely what we wanted to avoid.

7 - <a name="foot-Thu-Oct-18-201802EDT-2012"></a>[|back|](#note-Thu-Oct-18-201802EDT-2012) - Or a desktop/mobile binary client, to be fair, we did briefly mention that option in part one.

8 - <a name="foot-Thu-Oct-18-203423EDT-2012"></a>[|back|](#note-Thu-Oct-18-203423EDT-2012) - Granted, a lot of this complexity was a result of needing to use different players in different situations, but still.
