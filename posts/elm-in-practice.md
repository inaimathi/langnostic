So I've gotten some time in with it. Not quite enough to finalize the new interface, though I do have an unstyled 95% version running on my local with an apropos [choice of music](http://www.youtube.com/watch?v=YR4TDBXebWc). Firstly, here's the code.

## <a name="the-code"></a>The Code

```haskell
module Mote where

import JavaScript.Experimental (toRecord)
import Json (fromString, toJSObject)
import Graphics.Input (button, buttons, customButtons)
import Window (middle)
import Http (sendGet, send, post)
import Maybe (maybe)

----- Signal Declarations
uriDir str = "/show-directory?dir=" ++ str
reqPlay str = post ("/play?target=" ++ (maybe "" id str)) ""
reqCmd str = post ("/command?command=" ++ (maybe "" id str)) ""

command = buttons Nothing
playing = buttons Nothing
files = buttons "root"

dir = sendGet $ lift uriDir files.events
cmd = send $ lift reqCmd command.events
ply = send $ lift reqPlay playing.events

----- Utility
jstrToRec jStr = let conv = toRecord . toJSObject
                 in maybe [] conv $ fromString jStr

----- Application
box n = container 350 n midTop

cmdButton name = height 42 $ width 80 $ command.button (Just name) name

controls = flow down [ box 48 $ flow right $ map cmdButton ["backward", "stop", "pause", "forward"]
                     , box 50 $ flow right $ map cmdButton ["volume-down", "volume-off", "volume-up"]]
           
entry { name, path, entryType } = let btn = if | entryType == "return" -> files.button path
                                               | entryType == "directory" -> files.button path
                                               | otherwise -> playing.button (Just path)
                                           in width 350 $ btn name

showEntries res = case res of
  Success str -> flow down . map entry $ jstrToRec str
  _ -> plainText "Waiting..."

showMe entries = flow down [ box 100 $ controls
                           , showEntries entries ] 

main = showMe &lt;~ dir
```

And that's all. Seriously. This replaces all of the ~200 lines of JS/HTML/CSS that comprised the [Angular.js edition](https://github.com/Inaimathi/web-mote/blob/master/static/js/mote.js), and the ~300 lines of its [jQuery/Backbone predecessor](https://github.com/Inaimathi/web-mote/blob/a864b926bb8304dba03b32813964291bee2cea9e/static/js/web-mote.js).

So, if nothing else, Elm is very terse.

```haskell
module Mote where

import JavaScript.Experimental (toRecord)
import Json (fromString, toJSObject)
import Graphics.Input (button, buttons, customButtons)
import Window (middle)
import Http (sendGet, send, post)
import Maybe (maybe)
```

That first part is the module declaration and `import`s, hopefully self-explanatory.

```haskell
----- Signal Declarations
uriDir str = "/show-directory?dir=" ++ str
reqPlay str = post ("/play?target=" ++ (maybe "" id str)) ""
reqCmd str = post ("/command?command=" ++ (maybe "" id str)) ""

command = buttons Nothing
playing = buttons Nothing
files = buttons "root"

dir = sendGet $ lift uriDir files.events
cmd = send $ lift reqCmd command.events
ply = send $ lift reqPlay playing.events
```

This declares the main signals of the interaction, and some uri/request helper functions they'll need. `command` is the group of buttons that issues playback commands, `playing` is the group of buttons sending `play` instructions specifically, and `files` is the group of buttons sending `show-directory` commands. These were all handled by the same callback mechanism in earlier versions of the interface, but it makes sense to separate them if we're dealing with their signal streams. `dir`, `cmd` and `ply` just take event signals from those button groups, make appropriate Ajax requests when necessary, and return signals of responses.

```haskell
----- Utility
jstrToRec jStr = let conv = toRecord . toJSObject
                 in maybe [] conv $ fromString jStr
```

That is a short utility function that converts a JSON string to a (potentially empty) list of records. The empty list situation happens in two cases


-   if the server sends back an empty list
-   if the server sends back a malformed JSON string


```haskell
----- Application
box n = container 350 n midTop

cmdButton name = height 42 $ width 80 $ command.button (Just name) name

controls = flow down [ box 48 $ flow right $ map cmdButton ["backward", "stop", "pause", "forward"]
                     , box 50 $ flow right $ map cmdButton ["volume-down", "volume-off", "volume-up"]]
           
entry { name, path, entryType } = let btn = if | entryType == "return" -> files.button path
                                               | entryType == "directory" -> files.button path
                                               | otherwise -> playing.button (Just path)
                                           in width 350 $ btn name

showEntries res = case res of
  Success str -> flow down . map entry $ jstrToRec str
  _ -> plainText "Waiting..."

showMe entries = flow down [ box 100 $ controls
                           , showEntries entries ] 

main = showMe &lt;~ dir
```

This is the meat of the front-end. `box` is just a positioning helper function. `cmdButton` is a helper function to define a playback command element. Note that these are missing a piece of functionality from the old interface: clicking and holding the `rewind`/`forward`/`volume-up`/`volume-down` buttons doesn't do anything. It *used* to make serial requests to the server for the appropriate command, but Elm doesn't have very good support for HTML events. I'll talk more about that in a bit.

`controls` defines the two-row, centered placement of those command elements. `entry` defines a button for the main show/play buttons which comprise the principal interaction with Web Mote. These are missing the play/shuffle sub-buttons for directories and they subtle styling, but that's just because I didn't do it yet. There's no obviously missing feature that would prevent me from implementing all of it; I'd just need to define the appropriate [`customButton`](http://elm-lang.org/docs/Graphics/Input.elm) and slot it in. I'd call it five lines at the outside. Thing is, I want to get to writing this article first, so it'll probably happen in an addendum.

Now that we've got that out of the way, here's what I think.

## <a name="what-i-think"></a>What I Think

To summarize, very good, but obviously not finished yet. Which makes sense, since it's only at `0.8`. I'm going to go through the headaches first, then note the things I particularly like about working with it.

## <a name="headaches"></a>Headaches

### <a name="signal-hell"></a>Signal Hell

Or, alternately, "Type Hell". I'm putting this one front-and-center, because Elm's author is fiercely [anti-callback](http://elm-lang.org/learn/Escape-from-Callback-Hell.elm), but seems to be just fine with introducing a similar situation with the type system.

The argument against callbacks goes like this in a nutshell: if you write one, you're separating pieces of a procedure that should really be unified. You want to express "do this stuff", but part of it has to happen after an asynchronous request, so you have to break your procedure up into pre-async and post-async stuff, then have the request call the function that completes post-async stuff after the request returns. It gets even worse if you need to do multiple async requests as part of your tasks; you might need to split the work up arbitrarily among a large number of functions, all of which should actually be unified conceptually.

Now, I'm *not* disagreeing with this argument, but take a look at the bottom of that code from `Mote.elm`.

```haskell
showMe entries = flow down [ box 100 $ controls
                           , showEntries entries ] 

main = showMe &lt;~ dir
```

What I want to express here is "Stack the controls on top of the file `entries` (figuring out `entries` based on the signal `dir`)". But you can't display an `Element` in the same list as a `Signal Element` because that would make some type theorist somewhere cry apparently. So instead of doing something like

```haskell
main = flow down [ box 100 $ controls, showEntries $ id &lt;~ dir]
```

I have to write a separate callback-like function to accept the sanitized signal value and display *that* instead.

This is the same situation as callback hell. The only difference is that callbacks separate your code at boundaries determined by asynchronous calls, while these signal display functions do it at boundaries determined by the type system. I guess one of those might be better than the other if you squint hard enough, but I'm not seeing it from here.

### <a name="very-few-event-options"></a>Very Few Event Options

A `button` or `customButton` send signals when they're clicked. `input` of `type="text"`, `password`s, `checkbox`es, and `dropDown`s send signals when their value changes. `textarea` and radio buttons don't exist. And that's all.

What do you do if you want a given form to submit when you hit `Ret` in a relevant input? What do you do if you want to define a button that can be held down (requiring a mouse-down event)? How do you implement draggables, or droppables, or datepickers, or any of [the interactive pieces that jQuery has trivially provided since something like 2006](http://jqueryui.com/demos/)? You either don't, or you make liberal use of the JavaScript FFI. Which isn't exactly fun. Since Elm is trying to do all of styling/content/behavior specification, I understand that you need to have elements like `image` that don't actually have behaviors. That is, they're of type `Element` rather than of type `(Element, Signal a)`. But the ones that do send signals should have a menu of signals to provide. I mean, you already have this cool record syntax, what you could do is provide an interface for the user where,

`button : String -> SignalOptions -> (Element, Signal a)`

and `SignalOptions` is something like  `{ click : a, mouseEnter: a, mouseLeave: a, mouseDown: a, mouseUp: a, keyDown: a }`. Granted, maybe that shouldn't be a `button`, but rather a different multi-signal element, but it would give quite a bit more flexibility to front-end developers. If you had an element like that, you could easily implement any of the interactions I mention above.

### <a name="no-encodingdecoding-outofthebox"></a>No Encoding/Decoding Out-of-the-box

I'll probably implement something here when I get around to poking at the language again, but there's no built-in way to call `encodeURI` or `encodeURIComponent` from Elm. Which means that as written, this front-end will fail to play files with `&` in their name. That's less than ideal. I get the feeling it wouldn't be too hard to implement using the [JS FFI](http://elm-lang.org/learn/Syntax.elm#javascript-ffi), but I'm not diving into that right now.

### <a name="gimped-case"></a>Gimped Case

The Elm `case` statement doesn't pattern-match on strings. There's no mention of that behavior in the docs, so I'm not sure whether this is a bug or an unimplemented feature or what, but I missed it once in a ~50 line program. Specifically, in `entries`

```haskell
entry { name, path, entryType } = let btn = if | entryType == "return" -> files.button path
                                               | entryType == "directory" -> files.button path
                                               | otherwise -> playing.button (Just path)
                                           in width 350 $ btn name
```

where I had to resort to using the new, otherwise unnecessary multi-branch `if`. Unfortunately ...

### <a name="gimped-if-indentation"></a>Gimped `if` Indentation

Because there's no `elm-mode` yet, you're stuck using `haskell-mode` for editing `.elm`s. `haskell-mode` craps out on indentation of that multi-branch `if` statement I just mentioned. If you try to indent the following line, it'll yell at you about parse errors rather than inserting the appropriate amount of white-space, which makes working with an already unnecessary-feeling operator just that little bit more annoying. This is similar to that `[markdown| |]` tag indentation issue I mentioned last time, it's just that the Web Mote front-end port didn't happen to need any `markdown`.

### <a name="gratuitous-differences"></a>Gratuitous Differences

[Type annotation](http://elm-lang.org/learn/Syntax.elm#type-annotations) (`::`) and `cons` (`:`) from Haskell have been switched for no obvious reason, and [`if`](http://elm-lang.org/learn/Syntax.elm#conditionals) seems to have a similar treatment. Unlike most of the other things I bumped into, this and the `case` "bug" have no hope in hell of being solved by a mere user of the language, so hopefully the designer does something about them.

## <a name="nitpicks"></a>Nitpicks

These aren't big things, and they're not really related to the language itself, but I noticed them and they were annoying.

### <a name="no-singlefile-option"></a>No Single-File Option

This is just a nice to have. It would have made this front-end marginally easier to deploy, but I'm not sure how it would work if you *needed* more than one file served for your program. Elm targets JavaScript as a platform, which means that the base language is deployed as a `js` file that you have to host manually if you're not using the `elm-server`. When you compile an Elm project, you have an option that looks like this

```haskell
  -r --runtime=FILE           Specify a custom location for Elm's runtime
                              system.
```

It's slightly misleading, because what it actually does is specify where to load `elm-runtime.js` from *in the compiled file*. Literally, it determines the `src` property of the appropriate `script` tag. For that Mote front-end, I had to `elm --make -r "/static/js/elm-runtime.js" --minify Mote.elm`, and then make sure to serve `elm-runtime.js` from that static url (by default, you can find this file in `~/.cabal/share/Elm-0.8.0.3/elm-runtime.js`, in case you were wondering).

Anyhow, it would be nice if there was a compiler option you could activate to just have this runtime inlined in your compiled result, rather than served separately.

### <a name="unstable-website"></a>Unstable Website

[elm-lang.org](http://elm-lang.org/) is down pretty frequently. It seems to be up at the moment, but I'm not sure how long that's going to be the case. It happens often enough that I just went ahead and did a checkout from [its github](https://github.com/evancz/elm-lang.org). Then I found out that the `"Documentation"` pages happen to be missing from that repo...

## <a name="highlights"></a>Highlights

Anything I didn't mention above is *good*, which is to say "most of it", but there are two things I like about the language enough to call out.

### <a name="records"></a>Records

[This](http://elm-lang.org/learn/Syntax.elm#records) is brilliant. Take a bow, you've nailed record interaction. The approach probably wouldn't fit trivially into GHC, but it would solve [some of the problems](http://hackage.haskell.org/trac/ghc/wiki/Records) their records have. It's also something the Erlang devs should probably keep an eye on, because it's much *much* better than [what I remember having access to in Erl-land](http://stackoverflow.com/q/10821930). Probably the biggest win is that Elm records get first-class treatment in terms of the languages' pattern matching facilities, which lets you do things like

```haskell
entry **{ name, path, entryType }** = let btn = if | entryType == "return" -> files.button path
...
```

That's something I miss in almost every single language that has both pattern matching *and* k/v constructs. As usual, Common Lisp has a 95% [solution](https://github.com/m2ym/optima#class) as [part](https://github.com/m2ym/optima#property) of the [Optima](https://github.com/m2ym/optima) pattern matching library.

This dynamic record syntax also lets you trivially handle JSON input from a server. In case you didn't notice, the stuff I was passing into `entry` originates in ajax responses from the server.

### <a name="haskellgrade-terseness"></a>Haskell-grade Terseness

Just a reminder. Despite all those flaws I pointed out above, the Elm version of this particular program weighs in at about 1/4 the code of the reactive Angular.js version, let alone the traditional plain DOM/jQuery approach. It's also more pleasant to work with than JS, but that's an entirely subjective point. Improvements can still be made here; implementing haskell-style sections and multi-line definitions would save a bit of typing, though, to be fair, not as much as I thought it would.

## <a name="conclusions"></a>Conclusions

I've already mentioned that I'm going to take a swing at putting together some SSE support, `encodeURI(component)?` calls and a more appropriate Emacs mode for Elm, but it probably won't be very soon. Thanks to a tip-off from Dann, I managed to squeak into the registration for the [Lisp In Summer Projects](http://lispinsummerprojects.org/) event, which looks very much like a multi-month [NaNoWriMo](http://www.nanowrimo.org/) with parentheses instead of character development and sleep.

I'm going to make a serious attempt at getting a little pet project of mine up-and-running in either Common Lisp or Clojure by September 30, which means I'll have very little time to hack on someone else's up-and-coming language regardless of how interesting it looks.
