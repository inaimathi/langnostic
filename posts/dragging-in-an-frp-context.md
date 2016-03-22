I made an off-the-cuff remark [earlier](/posts/elm-first-impressions) to the effect that Elm doesn't let you easily define drag/drop functionality, or element-originating clicks. Really, the situation is that you can't easily work with *any* of the basic [HTML events](/posts/elm-first-impressions), which also include hovering, element-originating keypresses, various window events, and various form events. When you think about how you'd implement any of them individually, it starts to become obvious why that is.

The first reflex is to reach for callbacks. Which, [as was already discussed](http://elm-lang.org/learn/Escape-from-Callback-Hell.elm), is the exact opposite of what Elm is trying to do. The real trouble begins when you consider how you'd do the same thing *without* callbacks in order to preserve that purity of purpose.

### First Pass

The obvious solution is to use a bunch of signals everywhere. One for each of the element-based events. Let the user specify signal values on elements, and dispatch on their results at the other end.

Except thats quite complex.

At first glance, you're looking at twenty or so global signals, *each* of which are going to have the kind of isolated, complicated dispatch we saw in [that Tic Tac Toe example](http://www.grzegorzbalcerek.net/elm/TicTacToe.elm). That sounds worse in every way than callback hell; all your dispatch needs to be centralized, which means that behavior under various circumstances will by definition be separated from the element it pertains to, and you suddenly can't understand any component of your program without understanding the central signal dispatch code.

### Second Pass

Another approach might be not to let the user specify signal values. Make them hooks to the relevant element. Expose some kind of interface to the user so that they can pipe other signal values into various properties of that element, and call it a day.

Also, we don't really need to have a signal per HTML event. For the situations I'm currently thinking about, we could get away with exactly two. `Keyboard.focus` and `Mouse.focus` will give me most of what I'd want in a pretty simple way. Basically, have `mouseover`, `mouseout`, `mousedown`, `mouseup` and `mouseclick` send `this` over the `Mouse.focus` signal, and let `mouseclick`, `esc` and `tab` send the same over the `Keyboard.focus` signal.

You'd then have some idea of what needs to be moved as a result.

### User Side

Of course, that's all base implementation stuff. On the client side, you don't want to have to do things like maintain your own table of draggables to dispatch a signal to when relevant. You'd want to be able to do something like

```haskell
draggable dragDefs $ plainText "This text is draggable"
```

and have that tap the right signals so that when you `mousedown` or `touch` on `"This text is draggable"`, it starts moving along with the cursor. In basic terms what needs to happen is


- when the mouse down signal is being sent
- and the `Mouse.focus` signal is referring to a draggable
- start piping cursor position, modified by initial deltas, into the `x` and `y` coordinates of that element


and I have no idea what the appropriate way to express that is in the framework of the existing Elm language.

It sounds like it might just be easier to avoid those interactions while I'm starting out. [SSEs](http://www.w3schools.com/html/html5_serversentevents.asp) sound like they'd be a much easier first feature, actually.

### SSEs

The reason being that, when you think about it, this fits perfectly into the FRP paradigm. A source is a signal whose value is the latest matching message body and/or id. That's it. You'd want the declaration to look something like

```haskell
src = eventSource "/my/source/uri" ["message type 1", "message type 2" ...]
```

at which point `src` should be a signal you can pass around, whose current value will be the latest message coming out of `"/my/source/uri"` that has one of the message types specified. It might also be useful to handle unlabeled messages, at which point our message needs to look something like

```haskell
data SSE = SSE { id : Maybe Int, label : Maybe String, body : String }
```

Manageable, if slightly annoying due to the optional fields.

You'd implement a rolling message by piping `src` through `plainText . .body`, and you could put together a very simple chat program with some judicious use of `[foldp](http://elm-lang.org/docs/Signal.elm)`.

These were all just some random thoughts I wanted a good look at, for the time being. Like I said, I'll be throwing my next few spare hours at putting together an Elm-based [WebMote](https://github.com/Inaimathi/web-mote) front-end. Fortunately, this task doesn't involve any in-depth interaction, and the SSEs aren't central to the exercise.
