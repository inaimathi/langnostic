So Standard ML is pretty cool. Not just cool enough that it's in the logo bar (which it is, right next to that drugged-up looking gopher), cool enough that I now officially have four languages tied for "favorite". They're [Common Lisp](https://common-lisp.net/), [Scheme](http://www.schemers.org/) ([Racket](http://racket-lang.org/), if you consider those different languages), [Haskell](https://www.haskell.org/) and [Standard ML](http://sml-family.org/), in case you're keeping score.

I've come to this conclusion having spent about two weeks building a [web server](https://github.com/Inaimathi/serve-sml) in it. It's not done yet; I'm still trying to get clear in my own head how the routing/request-handling is supposed to play into all this. There's some significant boilerplate introduced by the language syntax, and I've already had to contribute a commit to the appropriate [learn `x` in `y`](http://learnxinyminutes.com/docs/standard-ml/), but it's still going pretty well so far.

The sad part is that I now get an even harder dose of [Traveler's syndrome for programmers](http://welearntoday.com/the-eternal-traveler-syndrome/) to swallow. At this point,

- When I work in in Common Lisp, I miss the ML type inferencer/checker
- in ML, I miss Haskell's `typeclass` system and implicit lazyness
- in Haskell, I miss macros, hygienic or otherwise
- in Scheme, I miss `loop` and `CLOS`
- and just to top it all off, in any of them, I miss the broad library support from Python and `golang`

So from that perspective, I'm pretty boned regardless of what I'm hacking on these days. I dearly hope these languages converge at some point, but it's not looking too good at the moment. Maybe I'll need to [do something about that](https://github.com/Inaimathi/experimentalisp) at some point. But enough about that crap; lets talk a bit about this server.

## <a name="a-bit-about-this-server" href="#a-bit-about-this-server"></a>A Bit About This Server

Firstly, as usual, I'm not *particularly* interested in efficiency. If *you* are, you should probably read [this](http://www.aosabook.org/en/posa/warp.html) instead. I'm interested in getting a simple server put together so that I can understand how to write code in another language, and get a good impression of how it feels to use it for larger projects.

Like I said above, it's not done yet, so much of what I'm about to show you is subject to massive change. The overall architecture looks like it's going to break down into four, maybe five distinct pieces. You need a **server core** that listens for new connections and accepts them. You need a **buffer** that aggregates all the input you need to grab in order to deal with a single request. You need a **parser** that takes a complete buffer and gives you back a useful structure *(and potentially buffers a bit more, depending on what it emits)*. And finally, you need a **handler**, which takes that request, figures out what to do with it and returns a response, or potentially does some crazy stream-based stuff in service to [SSEs](http://www.w3schools.com/html/html5_serversentevents.asp) or [WebSockets](https://developer.mozilla.org/en/docs/WebSockets). Optionally, you might separate the **router** structure from the handlers, but that's about it.

Each of those components might have some variability. You might have buffers and parsers specialized to work fast with certain kinds or sizes of requests, and you might make certain decisions about your router depending on exactly what you happen to be doing, and you might choose to [conflate buffering/parsing](https://github.com/Inaimathi/cl-lazy-parse) for conceptual reasons, but you'll end up needing all of the above.

## Handler

I'm going to start with the part most likely to change. And I'm doing it in explanatory order rather than code order, because I'm fucking explaining here. If you want to *run* any of what I'm about to show you, that [github link](https://github.com/Inaimathi/serve-sml) will serve you well. Har har, aren't I droll.

```ml
fun hello "GET" ["hello", name] _ = 
    ok ("Hello there, " ^ name ^ "! I'm a server!")
  | hello _ ("rest"::rest) _ =
    ok ("You asked for: " ^ (String.concatWith "/" rest) ^ "' ...")
  | hello _ ["paramtest"] ps =
    let in
	case (ps "a", ps "b") of
	    (SOME a, SOME b) =>
	    ok ("You sent me A: " ^ a ^ " and B: " ^ b ^ "...")
	  | _ => err400 "Need both 'a' and 'b' parameters for this one."
    end
  | hello _ _ _ = 
    err404 "Sorry; I don't know how to do that";
```

I'm betting you could read that even without an [ML syntax primer](http://learnxinyminutes.com/docs/standard-ml/), but it *might* be a good idea. Specifically, we're taking advantage of pattern-matching to destructure our input. You may have inferred that `hello` is a function of three arguments, with the first two being the handler [`method`](TODO http methods) and the handler [`resource`](TODO http resources). The third is a function that, when called with a string, will give you the corresponding request parameter. In fact, it'll give you a `string option`, rather than just a string, which is the SML equivalent of the [`Maybe` monad](TODO haskell maybe monad).

As I mentioned, this is the part most likely to change. In particular, I'm not sure whether I want to pass various selector functions into the handler, or just expose the entire `Request`. Pros and cons either way, and I'm still trying to get straight in my head which will confer maximum advantage. Additionally, while using the languages' built-in pattern-matching facilities to do handler dispatch conforms nicely to other ML code you're likely to read, it's not *quite* reflective enough for some tricks I kind of want to pull. For instance, I want to be able to say something along the lines of "Given this handler table, add a `/help` handler that lists all available handlers along with their `method`s, required `parameter`s, paths and path variables". It's not clear that I could do that with the above definition approach. Ideally I'd want something along the lines of

```ml
GET  "hello/:name" [] (fn name => ok ("Hello there, " ^ name ^ "! I'm a server!"))
GET  "rest/::rest" [] (fn rest => ok ("You asked for: '" ^ (String.concatWith "::" rest) ^ "' ...))
POST "param" ["a", "b"] (fn a b => ok ("You sent me A: " ^ a " and B: " ^ b ^ " ..."))
```

Although, we'll talk a bit more about that later. *Ideally*, ideally I'd also want to be able to do things like

```ml
GET "param" ["a", "b"] (fn a b => ok ("Total: " ^ (Int.toString (a + b))))
```

More or less the way I can over in [`house`](https://github.com/Inaimathi/house). The code block we're discussing uses some utility functions

```ml
fun httpRes responseType extraHeaders body action = 
    ({
	httpVersion = "HTTP/1.1", responseType = responseType, 
	headers = ("Content-Length", Int.toString (String.size body))::extraHeaders,
	body = body
    }, action)

fun route f (request : Serv.Request) socket =
    let val path = case String.fields (curry (op =) #"/") (Serv.resource request) of
		       (""::rest) => rest
		     | p => p
	val (res, act) = f (Serv.method request) path (Serv.param request)
    in
	Serv.sendRes socket res;
	act
    end

fun simpleRes resCode body =
    httpRes resCode [] body Serv.CLOSE;

fun ok body = httpRes "200 Ok" [] body Serv.CLOSE;
fun err400 body = httpRes "404 Not Found" [] body Serv.CLOSE;
fun err404 body = httpRes "404 Not Found" [] body Serv.CLOSE;
```

Here you see all of the minor utility functions that went into writing `hello` above. They're mostly used to generate `Response`s, with the exception of `route`.

```ml
fun route f (request : Serv.Request) socket =
    let val path = case String.fields (curry (op =) #"/") (Serv.resource request) of
		       (""::rest) => rest
		     | p => p
	val (res, act) = f (Serv.method request) path (Serv.param request)
    in
	Serv.sendRes socket res;
	act
    end
```

That's the function we'll be passing onto our server. It takes a function of `(string -> string list -> (string -> string option) -> (Response, SockAction))`, a `Request` and a `Socket`. It takes the method, parameters and resource of the `Request`, splits the resource, wraps the parameters in a closure and feeds the result into the function. It writes the result out to the socket, and returns the `SockAction` as its own result.

## Core

[server code here]

The server core is *not* as straightforward as you might expect. Leastwise, it wasn't as straightforward as I expected the first time. What I used to think is that you could easily write a server that goes `buffer -> parse -> handle -> write` in a fairly straightforward pipeline. And you *could* do that, if not for the existence of `POST` requests and their bodies. For that reason, there actually needs to be a minor recursive step between `buffer` and `parse`. You need to buffer enough of the incoming request to parse its headers, then either continue to the `handle` step, or go back and `buffer` a bit more depending on said headers. Before we get into that though, lets take a quick tour of the syntax.

## Buffer
## Parser

## Standard ML Impressions

- Some weird errors (file handler exception when you run off the end of a [`MONO_ARRAY`](TODO) at runtime)
- Some awkward syntax, certainly compared to Scheme, and also compared with Haskell (although it does away with the whitespace sensitivity, which I see as a good thing)
- Good compilers
- No package system other than [`smackage`](TODO - github), which still requires you to manually mess with environment variables and your compiler/interpreter config
- No [`hoogle`](TODO) equivalent (though I plan to do something about that before *too* long)
- Still some need to annotate types. In particular, certain uses of `Records`
- Really REALLY miss the haskel @ notation
