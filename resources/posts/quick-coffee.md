A very short post today. After having discussed and thought about the state of [client communication](http://langnostic.blogspot.com/2012/02/client-communication.html) a little while ago, I mentioned that I'd be doing some semi-serious work in [node.js](http://nodejs.org/). Mainly because they seem to do [Websockets Properly™](http://socket.io/).

So I downloaded node, and [npm](http://npmjs.org/)[^an-aside], and got ready to go through a tutorial or two. It was pretty fast going because I'm already fairly experienced with JS[^jquery-development], but the reading for [one in particular](http://www.nodebeginner.org/#analyzing-our-http-server) was surreal. Half of the thing was written for an audience that's only just heard of higher order functions. It also involved a lot of server restarting, which I found annoying enough to look for [relief](https://github.com/DracoBlue/spludo/blob/master/build/run_dev_server.js). That's a check, at least.

[^an-aside]: As an aside here, I have to note that I seem to have no patience left for languages without good package managers. I'm pretty sure this is a new development because I've done some work in Erlang, and there's this vague memory kicking around my head of a time before [quicklisp](http://www.quicklisp.org/), but there you have it. Luckily, npm is pretty good and getting better fast.

[^jquery-development]: Having done heavy development in jQuery, and some playing with [Rhino](http://www.mozilla.org/rhino/) and Jaxer back when those "were new" and "existed" respectively.

Sorry, I'm getting off track here. Anyway, once I got refreshed with the examples and moved on to trying to code up something for myself, it took all of 10 minutes and about a file and a half before I remembered exactly why I mostly use [parenscript](http://common-lisp.net/project/parenscript/) these days. Javascript is... well... it's ugly. It doesn't really seem ugly when you just look at examples, but if you try to actually use it for realzies, it'll take surprisingly little to get to an annoying obstacle. Whether you'd really like an optional/rest argument somewhere, or you'd like to have a function return implicitly, or you'd like to do some non-trivial string templating, you will either be annoyed or you'll need to find another way.

There's no real way to solve this from within JavaScript either, which is why I'm considering code transformers. There's the obvious, already mentioned, parenscript[^prefer-not-to-default], and there's a surprisingly expressive alternative JS syntax called [Coffee Script](http://coffeescript.org/) which you've probably heard all about.

[^prefer-not-to-default]: Which I'd prefer not to default to since the whole point of this exercise was to get away from Lisp for a little while. It may still end up winning, but I want to at least look at an alternative first.

I haven't quite got Emacs highlighting it properly, but it seems to do *nearly* as well as parenscript at abstracting the annoying parts.

```coffee-script
# function definition
square = (num) -> num * num
# optional argument and string templating
greet = (subject = "world") -> "Hello, #{subject}!"
# rest arg and array comprehension
squares = (numbers...) -> square n for n in numbers
# multi-quote string
content = """
<div id="content">
  <span class="quote">Blah!</span>
</div>
"""
```

> EDIT:
>   For comparison, here's the equivalent parenscript
```lisp
(ps
  ;;; function definition
  (defun square (num) (* num num))
  ;;; optional argument and string handling
  (defun greet (&optional (subject "world"))
    (+ "Hello, " subject "!"))
  ;;; rest args and iteration
  (defun squares (&rest numbers)
    (loop for n in numbers collect (square n)))
  ;;; generating html
  (defvar content
    (who-ps-html (:div :id "content"
                       (:span :class "quote" "Blah!")))))
```
> Mon, 12 Mar, 2012

That surprisingly terse block of Coffee Script expands out to

```javascript
var content, square, squares, greet,
  __slice = Array.prototype.slice;

square = function(num) {
  return num * num;
};

greet = function(subject) {
  if (subject == null) subject = "world";
  return "Hello, " + subject + "!";
};

squares = function() {
  var n, numbers, _i, _len, _results;
  numbers = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
  _results = [];
  for (_i = 0, _len = numbers.length; _i < _len; _i++) {
    n = numbers[_i];
    _results.push(square(n));
  }
  return _results;
};

content = "<div id=\"content\">\n  <span class=\"quote\">Blah!</span>\n</div>";
```

Granted, it misses some big ones[^macros-obviously], but still.

[^macros-obviously]: Macros, obviously, but I've also got surprisingly used to prefix notation and homoiconicity. There's also the fact that Coffee Scripts' highlighter is misbehavin', and [`coffee`](http://www.opinionatedprogrammer.com/2010/12/installing-coffeescript-on-debian-or-ubuntu/) has nothin' on [`slime`](http://common-lisp.net/project/slime/), *and* I'd have to give up paredit use to go back to syntactic whitespace. In fact, I'm going to stop thinking about this now because it's almost depressing how many things non-lisps are missing that you really wouldn't think are a big deal until you get the option to lose them.

I kinda like it.
