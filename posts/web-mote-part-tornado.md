So [that](https://github.com/Inaimathi/web-mote) went pretty well.

Except that `github` still thinks this project is 20% Perl. If I were mean, I'd make a joke about binary files being improperly recognized.

Granted, the re-write encompassed `player.py` and not just `main.py`, but that's because I never stopped to sit down and think through the threading model. Because `web.py` does a thread-per-request, it *technically* worked anyway, but that module was due for a proper tear-down and re-build whether I moved servers or not.

```python
## player.py

from subprocess import Popen, PIPE, call
from threading import Thread
from Queue import Queue
import os
import util, conf
from main import ServerStatus

############################################################
### MASSIVE AMOUNTS OF CONFIG (this should probably be in a DB somewhere)
############################################################
defaultPlayer = ["mplayer"]

### If `omxplayer` is available, use it for `mp4`s and `ogv`s (with audio output to the HDMI port)
### If not, use the default player for everything
try:
    call(["omxplayer"])
    playerTable = { 
        'mp4': ["omxplayer", "-o", "hdmi"], 
        'ogv': ["omxplayer", "-o", "hdmi"] }
except:
    playerTable = {}

commandTable = {
    'mplayer':
        {'step-backward': "\x1B[B", 'backward': "\x1B[D", 'forward': "\x1B[C", 'step-forward': "\x1B[A",
         ## down | left | right | up
         'volume-down': "9", 'volume-off': "m", 'volume-up': "0",
         'stop': "q", 'pause': " ", 'play': " "},
    'omxplayer':
        {'step-backward': "\x1B[B", 'backward': "\x1B[D", 'forward': "\x1B[C", 'step-forward': "\x1B[A",
         'volume-off': " ", #oxmplayer doesn't have a mute, so we pause instead
         'volume-down': "+", 'volume-up': "-", 
         'stop': "q", 'pause': " ", 'play': " "}
    }
### END THE MASSIVE CONFIG
############################################################
try:
    commandQueue ## Global multi-process queue to accept player commands
    playQ        ## Global multi-process queue to accept files to play
except:
    commandQueue = Queue()
    playQ = Queue()

def listen():
    while True:
        aFile = playQ.get()
        if util.isInRoot(aFile):
            ServerStatus.write_message_to_all(aFile, event='playing')
            playerCmd = __getPlayerCommand(aFile)
            cmdTable = commandTable[playerCmd[0]]
            playFile(playerCmd, aFile, cmdTable)

def playFile(playerCmd, fileName, cmdTable):
    __clearQueue(commandQueue)
    activePlayer = Popen(playerCmd + [fileName], stdin=PIPE)
    while activePlayer.poll() == None:
        try:
            res = commandQueue.get(timeout=1)
            activePlayer.stdin.write(cmdTable[res])
            ServerStatus.write_message_to_all(res, event="command")
            if unicode(res) == unicode("stop"):
                __clearQueue(playQ)
                activePlayer.terminate()
                return False
        except:
            None
    ServerStatus.write_message_to_all(fileName, event="finished")
    activePlayer = False
    return True

### Local Utility
def __getPlayerCommand(filename):
    global playerTable, defaultPlayer
    name, ext = os.path.splitext(filename)
    return playerTable.get(ext[1:], defaultPlayer)

def __clearQueue(q):
    while not q.empty():
        q.get()
    return True

### Start the player process
playerThread = Thread(target=listen, args=())
playerThread.start()
```

The relevant parts are actually just those two functions in the center. I'll assume you know what all the `import`s mean, and that we can just gloss over the `MASSIVE CONFIG`. The utility functions are self-explanatory-ish. `__getPlayerCommand` takes a file name, and figures out which player that file is going to be using by looking its extension up in the command table. By default, that's `mplayer`, but as you can see by that `try` block in the config section, if `omxplayer` is available, we use it for `mp4`s and `ogv`s<a name="note-Thu-Nov-29-163338EST-2012"></a>[|1|](#foot-Thu-Nov-29-163338EST-2012). `__clearQueue` takes a `Queue` and pulls from it while it's not empty, resulting in an empty queue.

Like I said, the real meat here is

```python
def listen():
    while True:
        aFile = playQ.get()
        if util.isInRoot(aFile):
            ServerStatus.write_message_to_all(aFile, event='playing')
            playerCmd = __getPlayerCommand(aFile)
            cmdTable = commandTable[playerCmd[0]]
            playFile(playerCmd, aFile, cmdTable)

def playFile(playerCmd, fileName, cmdTable):
    __clearQueue(commandQueue)
    activePlayer = Popen(playerCmd + [fileName], stdin=PIPE)
    while activePlayer.poll() == None:
        try:
            res = commandQueue.get(timeout=1)
            activePlayer.stdin.write(cmdTable[res])
            ServerStatus.write_message_to_all(res, event="command")
            if unicode(res) == unicode("stop"):
                __clearQueue(playQ)
                activePlayer.terminate()
                return False
        except:
            None
    ServerStatus.write_message_to_all(fileName, event="finished")
    activePlayer = False
    return True
```

`listen` pulls stuff out of the `playQ`<a name="note-Thu-Nov-29-163503EST-2012"></a>[|2|](#foot-Thu-Nov-29-163503EST-2012), checks that the thing it got is a file it should be able to play and if so, pulls the relevant metadata and passes it on to `playFile`.

`playFile` is probably the oddest function I've ever had to write. It has to be blocking, because we don't want its caller to think it can play another file before the last one is done<a name="note-Thu-Nov-29-163509EST-2012"></a>[|3|](#foot-Thu-Nov-29-163509EST-2012), but it also has to launch its player in an asynchronous `subprocess`, because it needs to be able to receive input from the user, **but** it can't *wait* for input because that means that it would *have* to receive some before it returned<a name="note-Thu-Nov-29-163517EST-2012"></a>[|4|](#foot-Thu-Nov-29-163517EST-2012). The result is what you see there. The first thing we do is clear the `commandQueue`<a name="note-Thu-Nov-29-163521EST-2012"></a>[|5|](#foot-Thu-Nov-29-163521EST-2012) and launch the player and retain a handle to it. Then, until playback finishes, we poll `commandQueue` for user input. We have to leave a timeout for that input check, because we'd otherwise wait here even after the file finished playing, and that's not fun. `ServerStatus.write_message_to_all` write out an SSE notifying the front-end of


-   playing a file
-   receiving a user command
-   finishing the file


respectively. Hmm. I should probably notify the front end that I've finished playback even when a `stop` command is received. Just for completeness. I'll make a note of it.

Those changes essentially make the `player` into an actor, except that it reaches into surrounding state in order to send notifications. If I really felt strongly about it, I could instead give it an output queue that other processes could pull from in order to communicate, rather than have it send messages into `ServerStatus` directly. I don't today, but you knever no.

Now that we've got that out of the way, here's what the new `main.py` looks like using `tornado`

```python
import tornado.ioloop, tornado.web, os, json, random, time
import util, conf, sse, player

class ShowDirectory(tornado.web.RequestHandler):
    def post(self):
        try:
            dir = self.get_argument("dir")
            assert util.isInRoot(dir)
            self.write(util.dirToJSON(dir))
        except:
            self.write(util.entriesToJSON(conf.root))

class Play(tornado.web.RequestHandler):
    def post(self):
        t = self.get_argument('target')
        player.commandQueue.put('stop')
        if os.path.isfile(t):
            fileList = [t]
        elif os.path.isdir(t):
            fileList = util.deepListDir(t)
        else:
            fileList = json.loads(t)
        if self.get_argument('shuffle', False):
            random.shuffle(fileList)
        self.write(json.dumps(fileList))
        time.sleep(1)
        [player.playQ.put(f) for f in fileList]

class ServerStatus(sse.SSEHandler): 
    def on_open(self):
        self.write_message(self.connection_id, event='connection_id')
    def on_close(self):
        self.write_message_to_all(self.connection_id, event='left')

class Command(tornado.web.RequestHandler):
    def post(self):
        cmd = self.get_argument('command')
        player.commandQueue.put(cmd)

class Index(tornado.web.RequestHandler):
    def get(self):
        self.redirect("/static/web-mote.html", permanent=True)

urls = [(r"/", Index),
        (r"/show-directory", ShowDirectory),
        (r"/play", Play),
        (r"/command", Command),
        (r"/status", ServerStatus),
        (r".*", Index)]

settings = {
    "static_path": os.path.join(os.path.dirname(__file__), "static")
    }

app = tornado.web.Application(urls, **settings)

if __name__ == "__main__":
    app.listen(8080)
    tornado.ioloop.IOLoop.instance().start()
```

As you can tell, it's not significantly different. The handler classes now need lowercase `POST`/`GET` methods, we use `self.write` and `self.redirect` instead of `return` and `raise`, we use `self.get_argument` instead of indexing into `web.input` to get request parameters, handlers now subclass `tornado.web.RequestHandler`, the routing table has slightly different syntax, and that's basically it. We also communicate with the player slightly differently, but that's due to the rewrite in the player. The only really significant difference (which I actually prefer the `tornado` approach for) is

**You can specify your own static directory.**

This bugged the ever-loving crap out of me in `web.py`, where doing the same required [non-trivially subclassing `StaticMiddleware`](http://stackoverflow.com/questions/6960295/changing-the-static-directory-path-in-webpy). It's also not obvious based on the documentation, but the default was a `static` folder relative to `cwd`, rather than relative to `__file__`, which meant that running `python a/long/path/to/my-app.py 4141` was needlessly tricky. `tornado` just takes a path, and you get to decide how complete/relative it is.

Oh, I should mention, `ServerStatus` is not actually a default `tornado` class. I didn't have to write it myself, but the `sse.py` file is derived from [this](https://github.com/marinho/tornado/blob/master/tornado/server_sent_events.py). The `diff` is minimal; I added the capability to specify `event` fields, and made the `id` auto-increment by default. The class itself implements Server Sent Events; an asynchronous handler which assumes it isn't getting closed by the other end; a message written to it is going to be sent over to the client without a new request coming your way.

That's an essentially working, non-blocking media server.

Now, it's not done yet. I still have to re-write the `Play` handler, because I'm currently doing something fishy with `time.sleep` and the `stop` command instead of formalizing `new-queue` as a separate directive, and I still have to make mild edits to the front-end to actually *use* all this data that's being `SSE`d over, *and* setting up a play queue makes it almost trivial to implement skip forward/backward functionality so you bet I'll fucking do it, ***and*** it would be really nice to be able to make config changes through the front-end somehow.

But you should be able to use [it in its current state](https://github.com/Inaimathi/web-mote).

* * *
##### Footnotes
1 - <a name="foot-Thu-Nov-29-163338EST-2012"></a>[|back|](#note-Thu-Nov-29-163338EST-2012) - That's the RasPi video player; it's more primitive than `mplayer`, works specifically on the RasPi hardware, and can only really play a few different kinds of video, but the upside is that it can do surprisingly smooth HD output. So we really *want* to use it if at all possible.

2 - <a name="foot-Thu-Nov-29-163503EST-2012"></a>[|back|](#note-Thu-Nov-29-163503EST-2012) - Which is where we keep the files we still need to play.

3 - <a name="foot-Thu-Nov-29-163509EST-2012"></a>[|back|](#note-Thu-Nov-29-163509EST-2012) - There's only one TV.

4 - <a name="foot-Thu-Nov-29-163517EST-2012"></a>[|back|](#note-Thu-Nov-29-163517EST-2012) - And it won't necessarily; a file might get played with no further intervention from me at all. I don't want to put my music on shuffle and still *have* to press a button after each song.

5 - <a name="foot-Thu-Nov-29-163521EST-2012"></a>[|back|](#note-Thu-Nov-29-163521EST-2012) - Any commands still there were meant for the previous file, not this one.
