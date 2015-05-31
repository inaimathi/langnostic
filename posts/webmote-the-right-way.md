# WebMote the Right Way™©

A [little while ago, I mentioned](http://langnostic.blogspot.ca/2012/09/js-frameworks.html) that while the new wave of JS frameworks I observed were shit overall, they encouraged the correct approach to web UI building. Well, since I'll have to do a pretty serious re-write of [WebMote](https://github.com/Inaimathi/web-mote) for use with the RasPi, I figured it would be a good time to apply the principle. This is going to be a two-parter; firstly because I really want to focus on the front-end today, secondly because I don't have the back-end done yet, thirdly because I'm planning to some pretty odd things or the server-side of this project, and fourthly because I'm trying not to bore the ever-loving fuck out of everyone reading this.

So.

First off, have a `tree`

```
web-mote
├── css
│   ├── custom-theme ## [jQueryUI](http://jqueryui.com/) CSS and images
│   ├── icons ## the [SILK icon set](http://www.famfamfam.com/lab/icons/silk/)
│   ├── style.css
│   └── watermark
│       ├── audio.png
│       ├── folder.png
│       ├── image.png
│       └── video.png
├── js
│   ├── [backbone-min.js](http://backbonejs.org/)
│   ├── [handlebars-1.0.rc.1.js](http://handlebarsjs.com/)
│   ├── [jquery.min.js](http://jquery.com/)
│   ├── [jquery-ui-1.8.13.custom.min.js](http://jqueryui.com/download)
│   ├── [underscore-min.js](http://underscorejs.org/)
│   └── webmote.js
├── root-directory
└── webmote.html
```

Most of that is framework code, of course. The only files I'll be going through today are `webmote.html`, `webmote.css` and `webmote.js`. You'll note the presence of `backbone` and `underscore`; I use their `[Router](http://backbonejs.org/#Router)` class, but don't otherwise touch them for reasons.

```html
&lt;!DOCTYPE HTML>
&lt;html lang="en-US">
  &lt;head>
    &lt;meta charset="UTF-8">
    &lt;title>WebMote&lt;/title>
  &lt;/head>
  &lt;body>

    &lt;!-- --------- -->
    &lt;!-- Templates -->
    &lt;!-- --------- -->
    &lt;script id="tmp-folder" type="text/x-handlebars-template">
      &lt;li class="{{type}}">
        &lt;button class="play icon play" onclick="mote.playDir('{{path}}')">&lt;/button>
        &lt;button class="shuffle icon shuffle" onclick="mote.shuffleDir('{{path}}')">&lt;/button>
        &lt;a class="dir-link" href="#navigate{{path}}">{{name}}&lt;/a>
      &lt;/li>
    &lt;/script>

    &lt;script id="tmp-file" type="text/x-handlebars-template">
      &lt;li class="{{type}}">
        &lt;a class="file-link" href="javascript:void(0);" onclick="mote.play('{{path}}')">{{name}}&lt;/a>
      &lt;/li>
    &lt;/script>

    &lt;script id="tmp-control" type="text/x-handlebars-template">
      &lt;li>
        &lt;button class="icon {{cmd}}{{#if css-class}} {{css-class}}{{/if}}" onclick="mote.command('{{cmd}}');">
        &lt;/button>
      &lt;/li>
    &lt;/script>

    &lt;script id="tmp-control-block" type="text/x-handlebars-template">
      &lt;ul>
        {{#each this}}
        {{#control-button this}}{{/control-button}}
        {{/each}}
      &lt;/ul>
    &lt;/script>
    
    &lt;!-- ---- -->
    &lt;!-- Body -->
    &lt;!-- ---- -->
    &lt;ul id="file-list">&lt;/ul>
    &lt;div id="controls">&lt;/div>
    
    &lt;!-- ------ -->
    &lt;!-- Styles -->
    &lt;!-- ------ -->
    &lt;link rel="stylesheet" href="css/style.css" type="text/css" media="screen" />
    &lt;link rel="stylesheet" href="css/custom-theme/jquery-ui-1.8.13.custom.css" type="text/css" media="screen" />
    
    &lt;!-- ------- -->
    &lt;!-- Scripts -->
    &lt;!-- ------- -->
    &lt;script type="text/javascript" src="js/jquery.min.js">&lt;/script>
    &lt;script type="text/javascript" src="js/jquery-ui-1.8.13.custom.min.js">&lt;/script>
    &lt;script type="text/javascript" src="js/handlebars-1.0.rc.1.js">&lt;/script>
    &lt;script type="text/javascript" src="js/underscore-min.js">&lt;/script>
    &lt;script type="text/javascript" src="js/backbone-min.js">&lt;/script>

    &lt;script type="text/javascript" src="js/webmote.js">&lt;/script>

  &lt;/body>
&lt;/html>
```

That's actually the entire front-end markup. If you haven't seen HTML before, it might look daunting but it's extremely simple. You've got the `head` up top adding a little bit of metadata, all the `text/javascript` and `text/css` includes at the bottom, and only two actual elements in the `body` of the page: placeholders to pump a file list and control buttons into later. The interesting part is those four `handlebars` templates.

```html
    &lt;!-- --------- -->
    &lt;!-- Templates -->
    &lt;!-- --------- -->
    &lt;script id="tmp-folder" type="text/x-handlebars-template">
      &lt;li class="{{type}}">
        &lt;button class="play icon play" onclick="mote.playDir('{{path}}')">&lt;/button>
        &lt;button class="shuffle icon shuffle" onclick="mote.shuffleDir('{{path}}')">&lt;/button>
        &lt;a class="dir-link" href="#navigate{{path}}">{{name}}&lt;/a>
      &lt;/li>
    &lt;/script>

    &lt;script id="tmp-file" type="text/x-handlebars-template">
      &lt;li class="{{type}}">
        &lt;a class="file-link" href="javascript:void(0);" onclick="mote.play('{{path}}')">{{name}}&lt;/a>
      &lt;/li>
    &lt;/script>

    &lt;script id="tmp-control" type="text/x-handlebars-template">
      &lt;li>
        &lt;button class="icon {{cmd}}{{#if css-class}} {{css-class}}{{/if}}" onclick="mote.command('{{cmd}}');">
        &lt;/button>
      &lt;/li>
    &lt;/script>

    &lt;script id="tmp-control-block" type="text/x-handlebars-template">
      &lt;ul>
        {{#each this}}
        {{#control-button this}}{{/control-button}}
        {{/each}}
      &lt;/ul>
    &lt;/script>
```

Firstly, notice that they're all in `script type="text/x-handlebars-template"` tags. I think this technically goes against some markup standard, so it may get screamed at by an XHTML validator somewhere, but it's not a huge deal. If you really feel that's something you want to fix, you can also put these templates in hidden `div`s instead of `script` tags; as you'll see later, it wouldn't make a difference in how we use them.

Also, note that I'm inlining the `onclick` events rather than dealing with them later. A while ago, I sort of got used to the idea of just putting `id`s and `class`es on pieces of the DOM tree, then using jQuery to apply events later with `$("#foo").click(function () { thingsThatShouldHappen(); });`. It looks cleaner at first glance because it separates the presentation and behavior of your controls, but there are two pretty big problems with the approach. First, it requires additional DOM traversals. Those may or may not get optimized away in modern JS engines, but I'm not entirely sure. Second, and more importantly, it makes it harder to change the layout at runtime later, and that's something you'll want to do fairly frequently if you're looking to make a responsive application.

That's actually why I ended up separating `tmp-control` out of `tmp-control-block`. I honestly wasn't even going to generate those programatically, but I ran into a situation where I wanted to replace one button with another under certain conditions. I'll point it out when I get to it; the short version is, had I stuck to `$`ing `click` events, I would have had to run that setup function each time the change happened<a name="note-Sat-Oct-06-194031EDT-2012"></a>[|1|](#foot-Sat-Oct-06-194031EDT-2012). Having those events inlined lets me do the simple thing of just re-rendering a template at the appointed place.

Back to the code at hand.

```html
    &lt;script id="tmp-folder" type="text/x-handlebars-template">
      &lt;li class="{{type}}">
        &lt;button class="play icon play" onclick="mote.playDir('{{path}}')">&lt;/button>
        &lt;button class="shuffle icon shuffle" onclick="mote.shuffleDir('{{path}}')">&lt;/button>
        &lt;a class="dir-link" href="#navigate{{path}}">{{name}}&lt;/a>
      &lt;/li>
    &lt;/script>
```

The interesting part *of the templates* are those snippets of code in the titular `handlebars` that look something like `{{foo}}` or `{{#foo}}{{/foo}}`. A template expects an object, or possibly a list as its input later, and these snippets act as lookups. So, for example, where it says `{{path}}`, that particular template will look up the value `"path"` in its argument and echo the result. Now that you know that, the first two templates are fairly self-explanatory.

The last two could use some close-up time though.

```html
    &lt;script id="tmp-control" type="text/x-handlebars-template">
      &lt;li>
        &lt;button class="icon {{cmd}}{{#if css-class}} {{css-class}}{{/if}}" onclick="mote.command('{{cmd}}');">
        &lt;/button>
      &lt;/li>
    &lt;/script>
```

The single `control` template demonstrates an `#if` block. When you see the construct `{{#if foo}}{{bar}}{{/if}}` in a template, what happening is `argument.bar` is echoed if `argument.foo` is not one of `""`, `false`, `null`, `[]` or `undefined`.

```html
    &lt;script id="tmp-control-block" type="text/x-handlebars-template">
      &lt;ul>
        {{#each this}}
        {{#control-button this}}{{/control-button}}
        {{/each}}
      &lt;/ul>
    &lt;/script>
```

The `control-block` is responsible for outputting a group of `control`s. `this` in a `handlebars` block refers to the current argument, and the `{{#each foo}}...{{/each}}` construct iterates through a list. It doesn't seem like there's a good way of iterating over k/v pairs built in, but as that `#control-button` block demonstrates, it's possible to define your own helpers (spoiler, the `control-button` helper just renders an individual `control`).

More on that later though, let me take a very quick look at the CSS before we move on to the JS.

```css
/** The Control Panel *******************************************/
#controls { text-align: center; width: 100%; position: fixed; bottom: 0px; padding: 5px 0px; background-color: rgba(255, 255, 255, 0.8); }
#controls ul { list-style-type: none; margin: 0px; margin-bottom: 3px; }
#controls li { display: inline; }

/** The Main File List ******************************************/
#file-list { list-style-type: none; width: 350px; margin: auto; }
#file-list li { margin-bottom: 10px; padding: 3px; clear: both; }
#file-list li button { margin-top: 0px; float: left; }
#file-list li a { min-height: 20px; padding: 4px; float: left; border: 1px solid #ddd; border-radius: 5px; background: right no-repeat; background-size: 50px; }
#file-list li a.dir-link { white-space: nowrap; width: 228px; }
#file-list li a.file-link { word-break: break-all; width: 320px; }

#file-list li.directory a { background-image: url(watermark/folder.png); }
#file-list li.mp4 a, #file-list li.mov a, #file-list li.ogv a { background-image: url(watermark/video.png); }
#file-list li.mp3 a, #file-list li.ogg a { background-image: url(watermark/audio.png); }

/** BASICS ******************************************************/
h1, h2, h3, h4, h5, h6 { margin: 0px; padding: 0px; }

body { margin-bottom: 130px; }

.pointer { cursor: pointer; }
.hidden { display: none; }
.clear { clear: both; }

/** BUTTONS, MOTHERFUCKER, DO YOU USE THEM?! ********************/
button { background: no-repeat 3px 3px; margin: 3px;  height: 28px; width: 40px; background-color: #ddd; border-radius: 8px; cursor: pointer; text-align: left; }
button.icon { text-indent: 15px; }
button.big { width: 90px; }

/***** specific button types ************************************/
button.volume-up { background-image: url(icons/sound.png); }
button.volume-down { background-image: url(icons/sound_low.png); }
button.mute { background-image: url(icons/sound_mute.png); }

button.rewind-big { background-image: url(icons/resultset_previous.png); }
button.rewind { background-image: url(icons/control_rewind_blue.png); }
button.stop { background-image: url(icons/control_stop_blue.png); }
button.pause { background-image: url(icons/control_pause_blue.png); }
button.play { background-image: url(icons/control_play_blue.png); }
button.ff { background-image: url(icons/control_fastforward_blue.png); }
button.ff-big { background-image: url(icons/resultset_next.png); }
button.shuffle { background-image: url(icons/arrow_switch.png); }
```

There isn't a lot to explain here. The button section sets up nice, clickable buttons with those icons I mentioned earlier, the basics provide some utility classes. The `Control Panel` and `Main File List` sections should give you a pretty good idea of what the final UI is going to look like, and what those `watermark` images are for.

Ok, now that we've taken that short, stylish break, onto the meat.

```javascript
var util = {
    requestJSON : function (url, dat, type) {
        var res = null;
        $.ajax({
            url: url,
            data: dat,
            type: type,
            success: function (data) { res = $.parseJSON(data); },
            async: false
        });
        return res;
    },
    getJSON : function (url, dat) { return util.requestJSON(url, dat, "GET"); },
    postJSON : function (url, dat) { return util.requestJSON(url, dat, "POST"); }
};

var mote = {
    targetId: "#file-list",
    render: function (fileList) {
        if (fileList) {
            $(mote.targetId).empty();
            $.each(fileList,
                   function (index, e){
                       if (e.type == "directory") 
                           $(mote.targetId).append(templates.folder(e))
                       else 
                           $(mote.targetId).append(templates.file(e))
                   })
                }
    },
    renderButton: function (control) {
        
    },
    renderControls: function (controlLists) {
        $.each(controlLists,
               function (index, e) {
                   $("#controls").append(templates.controlBlock(e));
               })
            },
    play: function (file) {
        console.log(["cmd", "play", file]);
        $.post("/play",
               {"file" : file},
               function (data, textStatus) { 
                   console.log(["now playing", file, textStatus]);
               });
    },
    playDir: function (dir) {
        console.log(["cmd", "play", "directory", dir]);
        $.post("/play-directory", {"dir": dir});
    },
    shuffleDir: function (dir) {
        console.log(["SHUFFLE", dir]);
        $.post("/shuffle-directory", {"dir": dir});
    },
    command: function (cmd) {
        console.log(cmd);
        $.post("/command", {"command": cmd},
               function () {
                   if (cmd == "pause") {
                       var btn = templates.control({cmd: "play", "css-class": "big"});
                       $("#controls button.pause").replaceWith(btn);
                   } else if (cmd == "play") {
                       var btn = templates.control({cmd: "pause", "css-class": "big"});
                       $("#controls button.play").replaceWith(btn);
                   }
               })
    },
    navigate: function (dir) {
        console.log(["cmd", "display", dir]);
        mote.render(util.getJSON("/show-directory", {"dir": dir}));
    }
}

Handlebars.registerHelper("control-button", function (ctrl) {
    return new Handlebars.SafeString(templates.control(ctrl));
});

var templates = {
    folder : Handlebars.compile($("#tmp-folder").html()),
    file : Handlebars.compile($("#tmp-file").html()),
    control: Handlebars.compile($("#tmp-control").html()),
    controlBlock : Handlebars.compile($("#tmp-control-block").html())
}

var Routes = Backbone.Router.extend({ 
    routes: {
        "navigate*path": "nav"
    },
    nav: mote.navigate
});


$(document).ready(function() {
    mote.renderControls([[{cmd: "rewind-big"}, {cmd: "rewind"}, {cmd: "ff"}, {cmd: "ff-big"}],
                         [{cmd: "volume-down"}, {cmd: "mute"}, {cmd: "volume-up"}],
                         [{cmd: "stop", "css-class": "big"}, {cmd: "pause", "css-class": "big"}]]);
    mote.render(util.getJSON("/root-directory"));

    new Routes();
    Backbone.history.start();
});
```

I'm mostly using `object`s as namespaces here, and find myself wishing<a name="note-Sat-Oct-06-194416EDT-2012"></a>[|2|](#foot-Sat-Oct-06-194416EDT-2012) that JavaScript let me express that more clearly. Ahem. Lets get the quick stuff out of the way first. `util` is the utility namespace, and contains three helper functions to let me pull JSON data from the server more easily than I could by default. I'm following the functional paradigm and having them `return` their results rather than depend on a callback to do work.

A bit further down,

```javascript
Handlebars.registerHelper("control-button", function (ctrl) {
    return new Handlebars.SafeString(templates.control(ctrl));
});

var templates = {
    folder : Handlebars.compile($("#tmp-folder").html()),
    file : Handlebars.compile($("#tmp-file").html()),
    control: Handlebars.compile($("#tmp-control").html()),
    controlBlock : Handlebars.compile($("#tmp-control-block").html())
}
```

is the `handlebars`-related code. `Handlebars.registerHelper` is what makes the template helper function from earlier work properly. Note the `new Handlebars.SafeString` in that `return` line, by the way; `handlebars` templates escape their inputs by default, so passing a plain `string` won't quite do what you'd want it to in this situation. `templates` is the namespace in which we keep individual compiled `templates`. I mean ... templates. Notice that we're just identifying a DOM element by id, and running a dump of its `.html()` through the compiler. This is what I meant when I said that hidden `div`s would work just as well as `script`s. Your templates can be stored in any DOM element<a name="note-Sat-Oct-06-194657EDT-2012"></a>[|3|](#foot-Sat-Oct-06-194657EDT-2012), as long as you can reference it when your JS files load. Incidentally, that's why all the `script` includes in our HTML are at near the bottom of the structure, conveniently after our templates are defined.

Below that, and intruding slightly into the `.ready()` method is our in-page `Router`.

```javascript
var Routes = Backbone.Router.extend({ 
    routes: {
        "navigate*path": "nav"
    },
    nav: mote.navigate
});


$(document).ready(function() {
    mote.renderControls([[{cmd: "rewind-big"}, {cmd: "rewind"}, {cmd: "ff"}, {cmd: "ff-big"}],
                         [{cmd: "volume-down"}, {cmd: "mute"}, {cmd: "volume-up"}],
                         [{cmd: "stop", "css-class": "big"}, {cmd: "pause", "css-class": "big"}]]);
    mote.render(util.getJSON("/root-directory"));

    new Routes();
    Backbone.history.start();
});
```

This is the entire reason I use `backbone` and its requirement `underscore`<a name="note-Sat-Oct-06-194746EDT-2012"></a>[|4|](#foot-Sat-Oct-06-194746EDT-2012) in this file. The `Routes` object sets up `routes` to capture paths starting with `#navigate`, and calls `mote.navigate` if it finds one. We do this so that a user of this system will be able to save a link to a particular starting directory. That's also the reason we start the `Router` up *after* calling `mote.render` on the data coming out of `/root-directory`; that initial rendering would otherwise clobber the result our `navigate` call. The `renderControls` call displays all the assorted media buttons we'll need to acceptably control playback.

Lets take a detour before finishing up though; `root-directory` is for the moment just a text file with some test JSON data in it.

```javascript
[{"path": "/home/inaimathi/videos/a-show", 
  "type": "directory", "name": "a-show"}, 
 {"path": "/home/inaimathi/videos/friendship-is-magic", 
  "type": "directory", "name": "friendship-is-magic"}, 
 {"path": "/home/inaimathi/videos/khan-academy", 
  "type": "directory", "name": "khan-academy"}, 
 {"path": "/home/inaimathi/videos/porn", 
  "type": "directory", "name": "porn"}, 
 {"path": "/home/inaimathi/videos/bizarre-porn", 
  "type": "directory", "name": "bizarre porn"}, 
 {"path": "/home/inaimathi/videos/horrible-porn", 
  "type": "directory", "name": "horrible porn"}, 
 {"path": "/home/inaimathi/videos/unforgivable-porn", 
  "type": "directory", "name": "unforgivable porn"}, 
 {"path": "/home/inaimathi/videos/Clojure-for-Lisp-Programmers-Part-1.mov", 
  "type": "mov", "name": "Clojure-for-Lisp-Programmers-Part-1.mov"}, 
 {"path": "/home/inaimathi/videos/Clojure-for-Lisp-Programmers-Part-2.mov", 
  "type": "mov", "name": "Clojure-for-Lisp-Programmers-Part-2.mov"}, 
 {"path": "/home/inaimathi/videos/Eben-Moglen--Why-Freedom-of-Thought-Requires-Free-Media-and-Why-Free-Media-Require-Free-Technology.mp4", 
  "type": "mp4", "name": "Eben-Moglen--Why-Freedom-of-Thought-Requires-Free-Media-and-Why-Free-Media-Require-Free-Technology.mp4"}, 
 {"path": "/home/inaimathi/videos/Epic-Wub-Time--Musicians-of-Ponyville.mp4", 
  "type": "mp4", "name": "Epic-Wub-Time--Musicians-of-Ponyville.mp4"}, 
 {"path": "/home/inaimathi/videos/Project-Glass--Live-Demo-At-Google-I-O.mp4", 
  "type": "mp4", "name": "Project-Glass--Live-Demo-At-Google-I-O.mp4"}, 
 {"path": "/home/inaimathi/videos/in-the-fall-of-gravity.mp4", 
  "type": "mp4", "name": "in-the-fall-of-gravity.mp4"}, 
 {"path": "/home/inaimathi/videos/beethoven-symphony-no-9.mp3", 
  "type": "mp3", "name": "beethoven-symphony-no-9.mp3"},
 {"path": "/home/inaimathi/videos/first-lsdj.ogg", 
  "type": "ogg", "name": "first-lsdj.ogg"}]
```

It's a list of files and directories that a server could reasonably put together and transmit out to the front end just by `map`ping over the output of `listdir` or similar. The front-end will expect data represented in roughly this format in order to display a list of files.

Now that we've got an idea of what the data looks like, the last piece of this system can fall into place.

```javascript
var mote = {
    targetId: "#file-list",
    render: function (fileList) {
        if (fileList) {
            $(mote.targetId).empty();
            $.each(fileList,
                   function (index, e){
                       if (e.type == "directory") 
                           $(mote.targetId).append(templates.folder(e))
                       else 
                           $(mote.targetId).append(templates.file(e))
                   })
                }
    },
    renderControls: function (controlLists) {
        $.each(controlLists,
               function (index, e) {
                   $("#controls").append(templates.controlBlock(e));
               })
            },
    play: function (file) {
        console.log(["cmd", "play", file]);
        $.post("/play",
               {"file" : file},
               function (data, textStatus) { 
                   console.log(["now playing", file, textStatus]);
               });
    },
    playDir: function (dir) {
        console.log(["cmd", "play", "directory", dir]);
        $.post("/play-directory", {"dir": dir});
    },
    shuffleDir: function (dir) {
        console.log(["SHUFFLE", dir]);
        $.post("/shuffle-directory", {"dir": dir});
    },
    command: function (cmd) {
        console.log(cmd);
        $.post("/command", {"command": cmd},
               function () {
                   if (cmd == "pause") {
                       var btn = templates.control({cmd: "play", "css-class": "big"});
                       $("#controls button.pause").replaceWith(btn);
                   } else if (cmd == "play") {
                       var btn = templates.control({cmd: "pause", "css-class": "big"});
                       $("#controls button.play").replaceWith(btn);
                   }
               })
    },
    navigate: function (dir) {
        console.log(["cmd", "display", dir]);
        mote.render(util.getJSON("/show-directory", {"dir": dir}));
    }
}
```

The `mote` namespace contains all the relevant navigation commands that we'll need<a name="note-Sat-Oct-06-195028EDT-2012"></a>[|5|](#foot-Sat-Oct-06-195028EDT-2012). The `render` functions do exactly what you'd think. `render` clears the current file list, takes a list of file and directory objects, and runs them through the `file` or `folder` template as appropriate, appending the result to the file list. `renderControls` takes a tree of `control` objects and runs them through the `controlBlock` template, which in turn runs each through the `control` template and wraps the results in a `ul`.

The various `play`/`shuffle` functions pass file or directory names to whatever back-end process we'll be running this on top of. The `command` function is the thing that gets called by each control button. For the most part, it just passes that fact along to the back-end system and calls it a day, but there's additional logic for of `pause` and `play` signals. In that case, it also switches out the `oause` button for the `play` button, cycling between the two on every successful invocation. This is the difficulty I mentioned earlier, and it could still use a helper function or two<a name="note-Sat-Oct-06-195220EDT-2012"></a>[|6|](#foot-Sat-Oct-06-195220EDT-2012). Consider how you would write that code


-   using `$("#controls button").click()` instead of inlined `onclick` events.
-   without a separate `control` template.
-   without using templates at all (`handlebars` or otherwise).


The last part is the `navigate` function. It uses `util.getJSON` to request a new set of folders/files and `render`s them.

And that's it.

This complete front-end, including the CSS weighs in at just over 200 lines of code. It doesn't do anything yet, but that's just because we haven't slapped it in front of an application server. Note that it will be *completely*, *entirely* separate from whatever back-end we end up using. It'll work as long as said back-end supports the appropriate requests, and returns the correct data from `/show-directory` and `/root-directory`. In fact, if you'd like to make a compatible back-end, all you need to do is provide the following:


-   `/root-directory`
-   `/show-directory` *(we could probably get rid of the previous handler by letting this one take a zero-parameter request)*
-   `/play`
-   `/play-directory` *(if you wanted to provide a polymorphic `play` handler, you could do without this one)*
-   `/shuffle-directory`
-   `/command`


That's slightly misleading because the `command` handler needs to respond to 10 different parameters, depending on what button the user presses, but it's still much simpler than re-writing the front end for each new back-end. This cuts both ways too; someone wanting to implement their own front-end to the corresponding server needs to implement as many of those handlers as they'll need, and they're good to go.

Keep in mind that this is half the total goal. Next time, we'll see exactly how simple the server-side can get when we completely remove user interface concerns from it. Anyway, thus endeth the lesson. I'm not updating the github repo until I get around to a complete RasPi-compatible media server, but the above code should be easy enough to play with.

* * *
##### Footnotes
1 - <a name="foot-Sat-Oct-06-194031EDT-2012"></a>[|back|](#note-Sat-Oct-06-194031EDT-2012) - It would actually have been even more complicated than that, because it wouldn't have been enough to naively run `$("button").click(...)` again. That would put duplicate events on every button that wasn't replaced. I'd have gotten into the situation of either targeting just the new button separately, incurring code duplication, or writing the setup function in such a way that it unbound the click event for all buttons first, then re-bound them. That's ... inelegant.

2 - <a name="foot-Sat-Oct-06-194416EDT-2012"></a>[|back|](#note-Sat-Oct-06-194416EDT-2012) - Not for the first time.

3 - <a name="foot-Sat-Oct-06-194657EDT-2012"></a>[|back|](#note-Sat-Oct-06-194657EDT-2012) - Or even [pre-compiled](http://handlebarsjs.com/precompilation.html), if you feel like mucking about with [node.js](http://nodejs.org/) for a bit.

4 - <a name="foot-Sat-Oct-06-194746EDT-2012"></a>[|back|](#note-Sat-Oct-06-194746EDT-2012) - To be fair, I guess, `underscore.js` also has a half-way decent looking [templating system](http://underscorejs.org/#template) itself. It looks minimal, but flexible, and I'll be playing around with it a bit after this write up in an effort to potentially boot `handlebars` out of the include tree.

5 - <a name="foot-Sat-Oct-06-195028EDT-2012"></a>[|back|](#note-Sat-Oct-06-195028EDT-2012) - `console.log` calls have been left in for testing purposes, in case you want to take this pure front-end for a spin, by the way. It actually works if you have a reasonably recent browser.

6 - <a name="foot-Sat-Oct-06-195220EDT-2012"></a>[|back|](#note-Sat-Oct-06-195220EDT-2012) - Which I'll put in if it turns out that more than one button needs this kind of behavior.
