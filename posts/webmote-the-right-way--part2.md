And we're back. Part one was the previous article, so I'm not going to bother linking you to it. This time around we're taking a look at one possible back-end to the web-based media interface we cooked up last time. [The github has been updated](https://github.com/Inaimathi/web-mote), by the by. First off, there ended up being some change required to the front-end, mostly for browser compatibility purposes.

First, some browsers apparently don't pick up content from a jQuery ajax request. I ran into this with an older version of [Safari](http://www.apple.com/safari/), and comparably recent versions of [Conkeror](http://conkeror.org/). If you try to do 

```javascript
$.ajax({url: "foo", 
        data: {...}, 
        success: function (data) { 
           alert(JSON.stringify(data)); 
        }});
```

what you'll get is not `"Stuff the page sent you"`, but rather `""`. I have no idea why this is; checking the return value from the server confirmed that it was sending the correct stuff, but it wasn't making it to client side for some reason. I know it's a *browser* issue because Iceweasel and Chromium both did what I was expecting them to and returned the request contents. I solved *that* by using the jQuery `jqXHR` object return value available in reasonably recent versions of the framework. In other words

```javascript
$.ajax({url: "foo", 
        data: {...}, 
        success: function (data, status, jq) { 
           alert(JSON.stringify(jq.responseText)); 
        }});
```

No idea why this works while the earlier doesn't, but there you have it. The other problem I ran into was that [older versions of mobile Safari don't respect `position: fixed;`](http://stackoverflow.com/questions/743123/fixed-positioning-in-mobile-safari). That doesn't sound like it'll be a big deal to most people, but I *specifically* wrote [Web-Mote](https://github.com/Inaimathi/web-mote) so that I could use my first-gen iPod Touch as a remote.

Styling the control panel at runtime solves that problem, assuming the stylesheet doesn't have any `position` settings.

```javascript
// older versions of safari don't like `position: fixed`.
// they also don't like when you set `position: fixed` in a stylesheet,
//   then override that with inline styles.
// what I'm saying is that older versions of safari are assholes
if ($.browser.safari) {
    $("#controls").css({ "position": 'absolute' });
    window.onscroll = function() {
        $("#controls").css({ 
            "top" : window.pageYOffset + 'px'
        });
    };
} else {
    $("#controls").css({ "position": 'fixed' });    
}
```

Finally, it turns out that there's at least one case where we'll be rendering a directory, but not want the play/shuffle buttons. To that end, the protocol needs to change very slightly to accommodate a field specifying whether to render a button. The template needs to change too.

```html
    &lt;script id="tmp-folder" type="text/x-handlebars-template">
      &lt;li class="{{type}}">
        {{#if buttons}}
        &lt;button class="play icon play" onclick="mote.play('{{path}}')">&lt;/button>
        &lt;button class="shuffle icon shuffle" onclick="mote.shuffleDir('{{path}}')">&lt;/button>
        {{/if}}
        &lt;a class="dir-link" href="#navigate{{path}}">{{name}}&lt;/a>
      &lt;/li>
    &lt;/script>
```

Oh, actually, I also ended up making those handler changes mentioned last time. WebMote now has exactly four required handlers<a name="note-Mon-Oct-08-140346EDT-2012"></a>[|1|](#foot-Mon-Oct-08-140346EDT-2012):


-   `/show-directory` *(a zero-parameter request gets the root directory)*
-   `/play`
-   `/shuffle-directory`
-   `/command`
 

That's it for changes to the front-end since last time, but let me share some random thoughts before going on to the server-side.

### Interlude - The State of Lisp Web Development on ARM

My usual web development stack is [Hunchentoot](http://weitz.de/hunchentoot/) on top of [SBCL](http://www.sbcl.org/platform-table.html), which turns out to be a problem. You may have noticed that there's no ARM port in that SBCL link. Not "no Debian package", no port period. I guess I could technically grab the source, and laboriously `gcc` up my own, but I'm honestly neither patient nor smart enough to. [GCL doesn't play nice with quicklisp](http://savannah.gnu.org/support/?107611#discussion) which kind of makes that a non-starter for me regardless of how mind-bogglingly fast it claims to be, [CMUCL](http://packages.debian.org/sid/cmucl-source) requires [a working CMUCL system](http://xkcd.com/754/) to be built from source and isn't in the Wheezy repos, which leaves [CLISP](http://www.clisp.org/)<a name="note-Mon-Oct-08-140353EDT-2012"></a>[|2|](#foot-Mon-Oct-08-140353EDT-2012).

Which I *would* use if it played nicely with [external-program](https://github.com/sellout/external-program/wiki/API). Sadly, 

```lisp
*** - CLISP does not support supplying streams for input or output.
The following restarts are available:
ABORT          :R1      Abort main loop
```

Meaning that I could spawn an instance of `mplayer` or `omxplayer`, but I wouldn't be able to communicate with it after the fact.

Woo.

Anyway, the long and the short of it is that putting together a Common Lisp solution to this problem on an ARM machine is pretty far from trivial, involving one of


-   manual installation of Hunchentoot<a name="note-Mon-Oct-08-140419EDT-2012"></a>[|3|](#foot-Mon-Oct-08-140419EDT-2012)
-   resolving the CMUCL cyclical requirements graph
-   compiling your own SBCL


Which is why this first stab is written in Python, and a follow-up is probably going to be using Haskell rather than CL.

### WebMote the Right Way™© -- Server Side

First off, have an updated tree

```
web-mote
├── conf.py
├── LICENSE
├── README.md
├── static
│   ├── css
│   │   ├── custom-theme ## same as last time
│   │   ├── icons ## this too
│   │   ├── style.css
│   │   └── watermark ## and this
│   ├── js
│   │   ├── backbone-min.js
│   │   ├── handlebars-1.0.rc.1.js
│   │   ├── jquery.min.js
│   │   ├── jquery-ui-1.8.13.custom.min.js
│   │   ├── underscore-min.js
│   │   └── web-mote.js
│   ├── show-directory
│   └── web-mote.html
├── util.py
└── web-mote.py
```

You can easily find this over at [the github repo](https://github.com/Inaimathi/web-mote), of course, but I wanted to let you know what you were in for. There's only three files to go through, and we'll tackle the meat first this time around.

```python
## web-mote.py
from subprocess import Popen, PIPE
import web, os, json
import util, conf

urls = (
    '/show-directory', 'showDirectory',
    '/shuffle-directory', 'shuffleDirectory',
    '/play', 'play',
    '/command', 'command',
    '.*', 'index'
)
app = web.application(urls, globals())

class showDirectory:
    def POST(self):
        if web.input() == {}:
            res = util.entriesToJSON(conf.root)
        elif web.input()['dir'] == "root":
            res = util.entriesToJSON(conf.root)
        else:
            res = util.dirToJSON(web.input()['dir'])
        return res

class shuffleDirectory:
    def POST(self):
        web.debug(["SHUFFLING", web.input()])

class play:
    def POST(self):
        try:
            playFile(web.input()['file'])
        except:
            web.debug(web.input())

def playFile(aFile):
    if os.path.exists(aFile):
        if conf.currentPlayer:
            conf.currentPlayer[1].terminate()
        t = util.typeOfFile(aFile)
    ## mplayer suicides if its stdout and stderr are ignored for a while,
    ## so we're only grabbing stdin here
        conf.currentPlayer = (conf.player[t][0], Popen(conf.player[t] + [aFile], stdin=PIPE))

class command:
    def POST(self):
        cmd = web.input()['command']
        if conf.currentPlayer:
            (playerName, proc) = conf.currentPlayer
            proc.stdin.write(conf.commands[playerName][cmd])
            if cmd == 'stop':
                conf.currentPlayer = False

class index:
    def GET(self):
        raise web.seeother("/static/web-mote.html")

if __name__ == "__main__":
    app.run()
```

It's a very simple [web.py](http://webpy.org/) application that does the job of spawning external OS processes, then feeding them `input` based on user clicks on the front-end. I'll assume the routing table, `app.run()` call and `import` statements are self-explanatory. `web.py` routes requests to various named classes which are expected to have `POST` and/or `GET` methods attached. If you know the basics of how HTTP works, it should be obvious why.

The `index` handler at the bottom there just routes a request to our "static" front-end. `showDirectory` expects a pathname and returns a list of contents of the target<a name="note-Mon-Oct-08-140558EDT-2012"></a>[|4|](#foot-Mon-Oct-08-140558EDT-2012) using a bunch of `toJSON` utility functions from `util`. `shuffleDirectory` is currently a no-op that echoes something to the debug stream. `play` attempts to `playFile` its argument, and prints a debug statement if that fails. The only two interesting constructs here are `playFile` itself and `command`.

```python
def playFile(aFile):
    if os.path.exists(aFile):
        if conf.currentPlayer:
            conf.currentPlayer[1].terminate()
        t = util.typeOfFile(aFile)
        conf.currentPlayer = (conf.player[t][0], Popen(conf.player[t] + [aFile], stdin=PIPE))
```

`playFile` first checks whether the file its being asked to play exists<a name="note-Mon-Oct-08-140607EDT-2012"></a>[|5|](#foot-Mon-Oct-08-140607EDT-2012). If it does, then we check whether a `player` is already active, and kill it if it is<a name="note-Mon-Oct-08-140613EDT-2012"></a>[|6|](#foot-Mon-Oct-08-140613EDT-2012). At that point, we check the type of file we've been passed and start a player based on that. This'll be explained once we go over `conf.py`, but just to save you the suspense, it's because we want videos running in `omxplayer` while audio files play in `mplayer`.

```python
class command:
    def POST(self):
        cmd = web.input()['command']
        if conf.currentPlayer:
            (playerName, proc) = conf.currentPlayer
            proc.stdin.write(conf.commands[playerName][cmd])
            if cmd == 'stop':
                conf.currentPlayer = False
```

`command` expects a `POST` argument called `command`, uses it to look up a value in `conf.commands` according to which player is currently active, then writes the result to the active players' `input` stream<a name="note-Mon-Oct-08-140627EDT-2012"></a>[|7|](#foot-Mon-Oct-08-140627EDT-2012). You'll note that I'm representing a player as a `(name, process)` tuple; I could have made a singleton object, or a dictionary, but this is the simplest representation that works at the moment. Now that you understand the logic, we won't learn anything without taking a look at that configuration state.

```python
## conf.py
from subprocess import call

ext = {
    'audio': ['mp3', 'ogg', 'wav'],
    'video': ['mp4', 'ogv', 'mov', 'wmf']
    }

root = ["/home/inaimathi/videos",
        "/home/inaimathi/music"]

commands = {
    'mplayer':
        {'rewind-big': "\x1B[B", 'rewind': "\x1B[D", 'ff': "\x1B[C", 'ff-big': "\x1B[A",
         ## down | left | right | up
         'volume-down': "9", 'mute': "m", 'volume-up': "0",
         'stop': "q", 'pause': " ", 'play': " "},
    'omxplayer':
        {'rewind-big': "\x1B[B", 'rewind': "\x1B[D", 'ff': "\x1B[C", 'ff-big': "\x1B[A",
         'volume-down': "+", 'mute': " ", #omxplayer doesn't have a mute, so we pause instead
         'volume-up': "-", 
         'stop': "q", 'pause': " ", 'play': " "}
    }


player = {
    'audio': ["mplayer"],
    'video': []
    }

try:
    call(["omxplayer"])
    player['video'] = ["omxplayer"]
except:
    player['video'] = ["mplayer", "-fs"]

currentPlayer = False
```

This is a bunch of starting state. `ext` maps various extensions to either `audio` or `video` files, which is relevant both for the presentation layer<a name="note-Mon-Oct-08-140634EDT-2012"></a>[|8|](#foot-Mon-Oct-08-140634EDT-2012) and the back-end<a name="note-Mon-Oct-08-140638EDT-2012"></a>[|9|](#foot-Mon-Oct-08-140638EDT-2012). `root` is a list of directories to start in, and ideally, there should be security checks that any file we play/directory we show is contained in one of these. I have made a second note of it.

`commands` is the table that our `command` handler looks values up in. They're mappings between expected commands from the front-end to values that our player programs will understand. They're similar for the most part, but `omx` doesn't have mute and uses `+`/`-` to manipulate volume, where `mplayer` uses `9`/`0`. The idea is that if you look up a command in these tables, the result you'll get is a string you can write to the player stream in order to get it to respond to that command.

`player` is a mapping of file-type to player command. It always uses `mplayer` for audio<a name="note-Mon-Oct-08-140642EDT-2012"></a>[|10|](#foot-Mon-Oct-08-140642EDT-2012), but checks for the presence of `omxplayer` with that `try`/`except` block before deciding to use it for videos. If it doesn't find `omxplayer`<a name="note-Mon-Oct-08-140646EDT-2012"></a>[|11|](#foot-Mon-Oct-08-140646EDT-2012), it uses `mplayer` in full-screen mode instead.

`currentPlayer` is a hook to the current player process. It's `False` if there isn't a player running, and a `(name, process)` tuple if there is one<a name="note-Mon-Oct-08-140704EDT-2012"></a>[|12|](#foot-Mon-Oct-08-140704EDT-2012).

Moving on to the last piece:

```python
import os, json
import conf

def isExt(filename, extList):
    name, ext = os.path.splitext(filename)
    if ext[1:] in extList:
        return True
    return False

def isAudio(filename):
    return isExt(filename, conf.ext['audio'])

def isVideo(filename):
    return isExt(filename, conf.ext['video'])

def typeOfFile(path):
    if isAudio(path):
        return 'audio'
    elif isVideo(path):
        return 'video'
    else:
        raise LookupError("can't decide filetype of '%s'" % [path])

def nameToTitle(filename):
    return re.sub(" [ ]+", " - ", re.sub("-", " ", os.path.basename(filename).title()))

def entryToJSON(entry):
    name, ext = os.path.splitext(entry)
    if ext == '':
        ext = "directory"
    else:
        ext = ext[1:]
    return {'path': entry, 'type': ext, 'name': nameToTitle(name), 'buttons': True}

def entriesToDicts(entries):
    dirs, videos, music = [[],[],[]]
    for f in entries:
        res = entryToJSON(f)
        if os.path.isdir(res['path']):
            dirs.append(res)
        elif res['type'] in conf.ext['video']:
            videos.append(res)
        elif res['type'] in conf.ext['audio']:
            music.append(res)
    return dirs + videos + music

def entriesToJSON(entries):
    return json.dumps(entriesToDicts(entries))

def dirToJSON(directory):
    entries = entriesToDicts(
        map(lambda p: os.path.join(directory, p), 
            sorted(os.listdir(directory))))
    if directory in conf.root:
        entries.insert(0, {'path': "root", 'name': "..", 'type': "directory"})
    else:
        entries.insert(0, {'path': os.path.dirname(directory), 'name': "..", 'type': "directory"})
    return json.dumps(entries)
```

That ... seems pretty self-explanatory, actually. The file predicates at the top figure out what's what based on `conf.py` data. The last few functions there handle the conversion of directories and directory entries to JSON objects that can easily be fed to the front-end. This is where you'll see why I wanted a `buttons` option in the data itself by the way; some JSON dumps include an entry that lets the user navigate to the previous directory, and we don't really want a play or shuffle option on those. `nameToTitle` takes a filename and returns the corresponding display title based on my own idiosyncratic naming convention<a name="note-Mon-Oct-08-140718EDT-2012"></a>[|13|](#foot-Mon-Oct-08-140718EDT-2012).

There's a few things that this player obviously still needs. I *have* to put together some functions that let me check whether input to the `/list-directory` and `/play` handlers represents allowed files and not, say, `/dev/secret-files/`. That's a security concern, and I mentioned it as a downside to the approach when I [first wrote about the JS MVC frameworks](http://langnostic.blogspot.ca/2012/09/js-frameworks.html). Basically, if your front-end is entirely separate from your back-end, you can't treat it as trusted code<a name="note-Mon-Oct-08-140815EDT-2012"></a>[|16|](#foot-Mon-Oct-08-140815EDT-2012). You need to assume that malicious packets are going to come in through your external handlers, and you need to deal with them appropriately.

Other than that, features I'll be building over the next little while include


-   playing directories and lists of files<a name="note-Mon-Oct-08-140821EDT-2012"></a>[|17|](#foot-Mon-Oct-08-140821EDT-2012)
-   playlist management<a name="note-Mon-Oct-08-140826EDT-2012"></a>[|18|](#foot-Mon-Oct-08-140826EDT-2012)
-   better volume and seek control<a name="note-Mon-Oct-08-140829EDT-2012"></a>[|19|](#foot-Mon-Oct-08-140829EDT-2012)
-   ability to send HDMI events to the output<a name="note-Mon-Oct-08-140833EDT-2012"></a>[|20|](#foot-Mon-Oct-08-140833EDT-2012)


but they're all icing, as far as I'm concerned. This is now a pretty decent, working web-interface for a media server on the RasPi written in 389 lines of Python/JS/HTML/CSS. Once again, [the github](https://github.com/Inaimathi/web-mote) has been updated<a name="note-Mon-Oct-08-140839EDT-2012"></a>[|21|](#foot-Mon-Oct-08-140839EDT-2012) if you want to poke around with it.

Now if you'll excuse me, I'm going to spend a couple of hours putting it to good use.


* * *
##### Footnotes
1 - <a name="foot-Mon-Oct-08-140346EDT-2012"></a>[|back|](#note-Mon-Oct-08-140346EDT-2012) - And, as you'll see, one is still a no-op.

2 - <a name="foot-Mon-Oct-08-140353EDT-2012"></a>[|back|](#note-Mon-Oct-08-140353EDT-2012) - Where Hunchentoot runs in single-threaded mode, but that's not a big deal for an application like this.

3 - <a name="foot-Mon-Oct-08-140419EDT-2012"></a>[|back|](#note-Mon-Oct-08-140419EDT-2012) - You could be forgiven for thinking this is trivial if you haven't done it before.

4 - <a name="foot-Mon-Oct-08-140558EDT-2012"></a>[|back|](#note-Mon-Oct-08-140558EDT-2012) - Defaulting to something called `conf.root`.

5 - <a name="foot-Mon-Oct-08-140607EDT-2012"></a>[|back|](#note-Mon-Oct-08-140607EDT-2012) - No location checking yet, I'm making a note to add that later.

6 - <a name="foot-Mon-Oct-08-140613EDT-2012"></a>[|back|](#note-Mon-Oct-08-140613EDT-2012) - There should only ever one player active, since the point of this server is to control one display.

7 - <a name="foot-Mon-Oct-08-140627EDT-2012"></a>[|back|](#note-Mon-Oct-08-140627EDT-2012) - We're not using the `communicate` method since that closes the target stream and we want it to stay open.

8 - <a name="foot-Mon-Oct-08-140634EDT-2012"></a>[|back|](#note-Mon-Oct-08-140634EDT-2012) - Because we display different icons for videos than for music files.
9 - <a name="foot-Mon-Oct-08-140638EDT-2012"></a>[|back|](#note-Mon-Oct-08-140638EDT-2012) - Because we potentially use a different player for audio and video files.

10 - <a name="foot-Mon-Oct-08-140642EDT-2012"></a>[|back|](#note-Mon-Oct-08-140642EDT-2012) - Though I guess I could figure out what the default RasPi audio player is and use that instead.

11 - <a name="foot-Mon-Oct-08-140646EDT-2012"></a>[|back|](#note-Mon-Oct-08-140646EDT-2012) - Which means it's not running on a RasPi.

12 - <a name="foot-Mon-Oct-08-140704EDT-2012"></a>[|back|](#note-Mon-Oct-08-140704EDT-2012) - As an aside, this is the source of a pretty horrible heisenbug I ran into. You see, `web.py` isn't fully interpreted; when you run it, it starts a process that watches relevant files and re-compiles them if they change. Sounds ok, but because of that global hook assigning `currentPlayer` to `False`, whenever I made a change to the containing file, it would reset *without terminating the current player*. I spent a fun half hour or so trying to figure out what the hell was going on when it occurred to me that my development environment was leaving floating processes lying around. I'm not entirely sure `conf.py` is the best place to keep that start-up variable, since it's the one most likely to change at runtime, but I honestly don't know how to solve the higher problem in a general way

13 - <a name="foot-Mon-Oct-08-140718EDT-2012"></a>[|back|](#note-Mon-Oct-08-140718EDT-2012) - Bonus Points <a name="note-Mon-Oct-08-140743EDT-2012"></a>[|14|](#foot-Mon-Oct-08-140743EDT-2012) if you can figure it out based on that pair of regex substitutions.

14 - <a name="foot-Mon-Oct-08-140743EDT-2012"></a>[|back|](#note-Mon-Oct-08-140743EDT-2012) - Bonus Points can be redeemed for Regular Points <a name="note-Mon-Oct-08-140751EDT-2012"></a>[|15|](#foot-Mon-Oct-08-140751EDT-2012).

15 - <a name="foot-Mon-Oct-08-140751EDT-2012"></a>[|back|](#note-Mon-Oct-08-140751EDT-2012) - Regular Points can be redeemed for nothing.

16 - <a name="foot-Mon-Oct-08-140815EDT-2012"></a>[|back|](#note-Mon-Oct-08-140815EDT-2012) - Which is one reason that I'm glad Python's `subprocess.Popen` takes a list and appropriately escapes the contents rather than taking a string and leaving shell-injection vectors as so many other languages opt to.

17 - <a name="foot-Mon-Oct-08-140821EDT-2012"></a>[|back|](#note-Mon-Oct-08-140821EDT-2012) - Rather than just single files.
18 - <a name="foot-Mon-Oct-08-140826EDT-2012"></a>[|back|](#note-Mon-Oct-08-140826EDT-2012) - Probably as an entirely front-end construct, but we'll see.

19 - <a name="foot-Mon-Oct-08-140829EDT-2012"></a>[|back|](#note-Mon-Oct-08-140829EDT-2012) - Ideally, both would be sliders, but I went with buttons for the first pass because synchronizing state to the extent proper sliders would require seems rickety and error-prone.

20 - <a name="foot-Mon-Oct-08-140833EDT-2012"></a>[|back|](#note-Mon-Oct-08-140833EDT-2012) - This is for TV control; ideally, I'd be able to turn it on, change channels and control actual output volume from the same web interface that tells `mplayer` and `omxplayer` what to do.

21 - <a name="foot-Mon-Oct-08-140839EDT-2012"></a>[|back|](#note-Mon-Oct-08-140839EDT-2012) - Oddly, it lists this as a JavaScript project with code contents `Common Lisp: 100%`, rather than the mixture of Python, JS and HTML/CSS that it currently is.
