Two things on the agenda today. First, Elm has gotten some improvements that might mean I end up using it in production at some point. Second, I tried a new language called [Pure](http://purelang.bitbucket.org/), which I found by searching for "dynamically typed haskell". Stick around if that sounds interesting.

> EDIT:
> Do not bother sticking around if that sounds interesting. I ended up talking about Elm so much that I never got into Pure.
>
> Tue, 18 Feb, 2014

### Elm Lang

For those of you just joining us, [Elm](http://elm-lang.org/) is a pure-functional, statically typed, optionally-type-inferring language closely based on [Haskell](http://www.haskell.org/haskellwiki/Haskell), which targets a JavaScript-hosted VM for deployment. That is, there's an `elm-runtime.js` which Elm code compiles to target, and the result is highly reactive web front-ends that don't require any mucking around with the DOM. Now that we're all on the same page...

### Elm. Again.

This came up at a recent [Code Retreat](https://bentomiso.com/events/toronto-code-retreat-2014-feb), and it looks interesting as fuck in context with the FBP stuff I've been doing at work recently. The problem we were solving at the event was autocompletion. That is, given a partial input, return possible completions from some dictionary. Here's a short<a name="note-Mon-Feb-17-150009EST-2014"></a>[|1|](#foot-Mon-Feb-17-150009EST-2014) implementation in Elm.

```haskell
module Autocomplete where

import String
import Keyboard
import Graphics.Input as Input

(field, content) = Input.field "Enter text"

fState : [String] -> Input.FieldState
fState comps = case comps of
                 [] -> {string = "", selectionStart=0, selectionEnd=0}
                 _  -> {string = (head comps), selectionStart=0, selectionEnd=0}

esc = Keyboard.isDown 27
ctrlSpace = dropRepeats . lift and <| combine [Keyboard.ctrl, Keyboard.space]

empty : Signal Element
empty = sampleOn (merge Keyboard.enter esc) . fst <| Input.field "Enter text"

completeElem : Signal Element
completeElem = lift ((Input.fields Input.emptyFieldState).field id "Enter text") . sampleOn ctrlSpace . lift fState <| lift completions content

completions : String -> [String]
completions partial = if | 0 < String.length partial -> filter (String.startsWith partial) wordList
                         | otherwise          -> []

main = lift2 above (merges [field, completeElem, empty]) . lift asText <| lift completions content

--- Dummy data
wordList : [String]
wordList = String.words "one two three four five six seven eight nine ten"
```

The point here is: there's an input that displays completions as you type. If you hit the `enter` or `esc` keys, that input is cleared, and if you hit `Ctrl+Space`, it's filled with the top completion. The above doesn't let you select a different completion, which it should, but it's a pretty instructive example.

Lets go through it.

```haskell
module Autocomplete where

import String
import Keyboard
import Graphics.Input as Input
```

Module and import declarations. Nothing to see here, move along.

```
(field, content) = Input.field "Enter text"

fState : [String] -> Input.FieldState
fState comps = case comps of
                 [] -> {string = "", selectionStart=0, selectionEnd=0}
                 _  -> {string = (head comps), selectionStart=0, selectionEnd=(String.length <| head comps)}
```

The first line in this bit sets up a `field`, which is represented as a pair of `Signal`s; one for the element and one for the content. Signals are a pretty good way of modeling state changes over time in a purely-functional context. You can think of one as the infinite stream of possible values it'll contain, the current of which your program will be continuously operating. An input `field` is a *pair* of signals because you'd like to be able to change it<a name="note-Mon-Feb-17-150016EST-2014"></a>[|2|](#foot-Mon-Feb-17-150016EST-2014), as well as receive updates about its state changes. We'll do that by combining several signals on particular sample points, and `FieldState` is the type we can eventually funnel into a field.

```haskell
esc = Keyboard.isDown 27
ctrlSpace = dropRepeats . lift and <| combine [Keyboard.ctrl, Keyboard.space]
```

These represent two different signals we'd like from the `Keyboard` module. The first will be `True` whenever the Escape key is down<a name="note-Mon-Feb-17-150022EST-2014"></a>[|3|](#foot-Mon-Feb-17-150022EST-2014), the second will be `True` when both the `Ctrl` and `Space` key are pressed<a name="note-Mon-Feb-17-150025EST-2014"></a>[|4|](#foot-Mon-Feb-17-150025EST-2014). The types at each step might be useful. In particular, `ctrlSpace : Signal Bool`, `combine : [Signal a] -> Signal [a]` and `lift and : Signal [Bool] -> Signal Bool`. The `dropRepeats` is the only chunklet whose type signature will give you no further understanding <a name="note-Mon-Feb-17-150223EST-2014"></a>[|5|](#foot-Mon-Feb-17-150223EST-2014); it's there to prevent partial signal changes from triggering a "change" in the `ctrlSpace` signal itself. Also, on a syntax note, the `<|` is identical to [Haskell's $](http://stackoverflow.com/a/1290727/190887).

Onward.

```haskell
empty : Signal Element
empty = sampleOn (merge Keyboard.enter esc) . fst <| Input.field "Enter text"

completeElem : Signal Element
completeElem = lift ((Input.fields Input.emptyFieldState).field id "Enter text") . sampleOn ctrlSpace . lift fState <| lift completions content

completions : String -> [String]
completions partial = if | 0 < String.length partial -> filter (String.startsWith partial) wordList
                         | otherwise          -> []
```

This is the real meat right here. `empty` is the signal of empty elements which will "changes" whenever the `enter` or `esc` keys are pressed<a name="note-Mon-Feb-17-150245EST-2014"></a>[|6|](#foot-Mon-Feb-17-150245EST-2014). `completeElem` is the signal of filled elements that "changes" whenever the user hits `Ctrl + Space`. Finally, `completions` is the signal of completions of the current text in the main input.

```haskell
main = lift2 above (merges [field, completeElem, empty]) . lift asText <| lift completions content

--- Dummy data
wordList : [String]
wordList = String.words "one two three four five six seven eight nine ten"
```

These remaining lines render the input and completions to screen, and set up the extremely minimal test dictionary. That's it. What we have here is exactly what was described. An input, backed by a word list, which is cleared on either Enter or Esc, and completed on Ctrl+Space.

### The New Part

None of that was new.

If you've read any of the articles on this blog tagged Elm, you'd have known all of it already. The new part is that you can now have your Elm programs communicate with the outside world. In the case we're considering above, a solitary auto-completing input is pretty useless. But imagine if you could use it essentially as a minibuffer in a larger project. You'd want to be able to pass it new completion lists, and you'd want it to notify you when the user entered some input. It goes without saying that you'd like all this to be encapsulated within a known, non-global namespace, so that you could combine your Elm minibuffer with arbitrary JS projects.

So, lets do it.

```haskell
module Autocomplete where

import String
import Keyboard
import Graphics.Input as Input

(field, content) = Input.field "Enter text"

fState : [String] -> Input.FieldState
fState comps = case comps of
                 [] -> {string = "", selectionStart=0, selectionEnd=0}
                 _  -> {string = (head comps), selectionStart=0, selectionEnd= (String.length <| head comps)}

esc = Keyboard.isDown 27
ctrlSpace = dropRepeats . lift and <| combine [Keyboard.ctrl, Keyboard.space]

empty : Signal Element
empty = sampleOn (merge Keyboard.enter esc) . fst <| Input.field "Enter text"

completeElem : Signal Element
completeElem = lift ((Input.fields Input.emptyFieldState).field id "Enter text") . sampleOn ctrlSpace . lift fState <| lift2 completions content wordList

completions : String -> [String] -> [String]
completions partial wordList = if | 0 < String.length partial -> filter (String.startsWith partial) wordList
                                  | otherwise          -> []

main = lift2 above (merges [field, completeElem, empty]) . lift asText <| lift2 completions content wordList

port wordList : Signal [String]

port output : Signal String
port output = keepIf (\s -> s/="") "" (sampleOn Keyboard.enter content)
```

That's a minimally changed `.elm` file. The differences are


- We've added port declarations at the bottom there. One incoming, which is just a type declaration, and one outgoing, which has a type declaration and a transmitter function.
- We've changed `completions` so that it takes its `wordList` as input
- Anywhere we used to call `completions` with `lift completions content`, we now have to call it with `lift2 completions content wordList`


The file you'd embed that module into would look something like this<a name="note-Mon-Feb-17-150258EST-2014"></a>[|7|](#foot-Mon-Feb-17-150258EST-2014)

```html
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <title>Embedding Autocomplete - Elm</title>
    <script type="text/javascript" src="/elm-runtime.js"></script>
    <script type="text/javascript" src="/build/Autocomplete.js"></script>
  </head>
  <body>
    <div id="auto" style="position: absolute; left: 50px; top: 50px; width: 600px; height: 100px; border: 2px dashed #000;"></div>
    <script type="text/javascript">
      var can = Elm.embed(Elm.Autocomplete,
                          document.getElementById("auto"),
                          {wordList: ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]});
      can.ports.output.subscribe(function (msg) { console.log("FROM MINIBUFFER :: ", msg) })
    </script>
  </body>
</html>
```

> EDIT:
>
> You can find a running demo of the above [here](http://173.255.226.138/elm-sample/embed.html).
> Sat, 22 Feb, 2014

The relevant bits are the positioned `div`, which will contain our program, and the `Elm.embed` call, which sets it up. Note especially the third argument; you have to do that for any input ports in the component you're embedding. Finally, note the `subscribe` call which fits that output port we defined with a listener, in this case a naive one that just prints everything it gets to the console.

This is awesome.

It's awesome enough that I'm seriously considering Elm for some production work *at* work. Because I want to apply Elm in the places where it'll do massive amounts of good, and leave the other stuff to stateful JavaScript programs. Using the ports approach above, I can get exactly that. If there was something similar for Hskell, I'd probably have taken the plunge and built something with it by now<a name="note-Mon-Feb-17-150312EST-2014"></a>[|8|](#foot-Mon-Feb-17-150312EST-2014).

### In Case You're Reading, [evancz](https://github.com/evancz)

There are still a [few minor headaches with the language](http://langnostic.blogspot.ca/2013/06/elm-in-practice.html), though thankfully I didn't have to stub my toe on most of them this time around. The only ones that ended up being annoying, or will be very shortly are


- **No signal defaults from within `.elm` files**. This bites during development. When you have an Elm module that will depend on an outside signal for its operation, you have to set a default value for that signal outside. This is ok once you've got the embedding file together, but it does mean that that second `Autocomplete.elm` file above will give you the error

```
Initialization Error: port 'wordList' was not given an input!
Open the developer console for more details.
```

  if you try to run it standalone without modifications. The workaround I've been using is to comment out the `port` declaration line, and add one that reads `wordList = constant ["one", "two", "three", "four", "five", "six"]`. It works, but I'd rather not have to do it.
- **No Haskell-style sections**. It only bit once in this program, and it's tolerable, but I'd much rather write `(/="")` than the equivalent, but syntactically noisier `(\s -> s /= "")`.
- **No Indexing**. I'm almost convinced this has to be an omission on my part, and there's actually a way to do it out of the box, because it seems mildly bizarre to have [`List.head`](http://library.elm-lang.org/catalog/evancz-Elm/0.11/List) and [`String.sub`](http://library.elm-lang.org/catalog/evancz-Elm/0.11/String) in a language, but no list indexing operator or function. If there is one, just point me to it. In the meantime, you can define your own minimal version as `(!!) lst ix = lst |> drop ix |> head`, or maybe

```haskell
(!!) lst ix = case drop ix lst of
                 [] -> Nothing
                 sub -> Just <| head sub
```

- if getting out of array bounds gives you pause. Neither of these deals with negative indices, but they'll give you trivial indexing capabilities.


That's that, I guess. I *was* going to talk a bit about [Pure](http://purelang.bitbucket.org/). And [Forth](http://en.wikipedia.org/wiki/Forth_%28programming_language%29). And maybe incidentally a bit about [C](http://gcc.gnu.org/c99status.html), but this is way longer than I was expecting already, so I think I'll call it for today.

* * *
##### Footnotes

1 - <a name="foot-Mon-Feb-17-150009EST-2014"></a>[|back|](#note-Mon-Feb-17-150009EST-2014) - Admittedly this took something like hours total. Writing it involved a lot of experimentation and documentation browsing, and I did a quick clean-up pass afterwards. I'm really hoping the development time goes down as I get more practice with the language and type system.

2 - <a name="foot-Mon-Feb-17-150016EST-2014"></a>[|back|](#note-Mon-Feb-17-150016EST-2014) - Actually hang on. If you're new to the FRP thing, I should clarify that you never *really* change things. Remember; any stateful component is represented as the lazy, infinite list of its complete history. What I really meant by the shorthand "change it" is "merge multiple signals of the same type in a way that gives one of them primacy in certain situations". It's counter-intuitive the first few times, but it's helpful to keep the perspective in mind when you're dealing with Elms.

3 - <a name="foot-Mon-Feb-17-150022EST-2014"></a>[|back|](#note-Mon-Feb-17-150022EST-2014) - And hence will change on a `keyDown` or `keyUp` event for that key.

4 - <a name="foot-Mon-Feb-17-150025EST-2014"></a>[|back|](#note-Mon-Feb-17-150025EST-2014) - And will therefore change on either ctrl/space `keyDown`, whichever is second and on ctrl/space `keyUp`, whichever is first.

5 - <a name="foot-Mon-Feb-17-150223EST-2014"></a>[|back|](#note-Mon-Feb-17-150223EST-2014) - It's `dropRepeats : Signal a -> Signal a`, in case you really, *really* care.

6 - <a name="foot-Mon-Feb-17-150245EST-2014"></a>[|back|](#note-Mon-Feb-17-150245EST-2014) - That's the `(merge Keyboard.enter esc)`.

7 - <a name="foot-Mon-Feb-17-150258EST-2014"></a>[|back|](#note-Mon-Feb-17-150258EST-2014) - Assuming those were accurate urls for `elm-runtime.js` and `Autocomplete.js` on your system.

8 - <a name="foot-Mon-Feb-17-150312EST-2014"></a>[|back|](#note-Mon-Feb-17-150312EST-2014) - For the record, there *might* be something like it for Haskell, you'll hear excited giggling from me if I happen to find it. For the moment, I'm still evaluating [the FRP section of the Hasekellwiki](http://www.haskell.org/haskellwiki/Functional_Reactive_Programming).
