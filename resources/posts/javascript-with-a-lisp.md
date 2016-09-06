Obviously, I'm not a vet yet, so take these musings on [Parenscript](http://common-lisp.net/project/parenscript/) with a grain of salt. Also, feel free to look up the [tutorial](http://common-lisp.net/project/parenscript/tutorial.html) they provide for a more hands-on approach; I'm just talking about my experience with it, not attempting to teach it.

There are some ugly, un-abstractable patterns in JavaScript code[^youll-be-familiar-with]. They show up often, and you can't really do much about them in JS without resorting to `eval`. Which you probably shouldn't do. Parenscript knocks most of them out cold. The argument about `jQuery` being Good Enough™ also turns out to be moot, since you can easily compose work in both[^that-is]. I've created exactly three JS files with this so far, and here are some macros that I'm not sure I'd be willing to do without.

[^youll-be-familiar-with]: Which you'll be familiar with if you've ever done more than a tiny bit of jQuery development.
[^that-is]: That is, include jQuery and use Parenscript to generate framework code rather than plain JavaScript.

```lisp
(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector)
          ,@chains))
```

That's the pretty essential one I mentioned [last time](http://langnostic.blogspot.com/2011/03/parenscript.html); it just lets you do things like

```lisp
($ ".target-div"
     (css (create :height 30 :background-color "#f00"))
     (effect "explode" 3000))
```

it's just keeping pace with jQuery. Next up

```lisp
(defpsmacro \ (&body body) `(lambda () ,@body))
```

I... honestly wasn't expecting to use this. I'm borrowing [Haskell](http://www.haskell.org/haskellwiki/Haskell)'s anonymous function notation for brevity, but only because there's no actual `λ` key on my keyboard. This is something you don't even notice while coding in JavaScript. You just get used to having to wrap various random things in `function () { ... }`. It never occurs how annoying this is until you get the chance to do away with it.

```lisp
(defpsmacro doc-ready (&body body)
  `($ document
      (ready (\ ,@body))))
```

Told you `\` would come in handy[^one-of]. This isn't particularly interesting; just shortcut notation for `$(document).ready(function () { ... });`.

[^one-of]: And this is one of the about twenty places it shows up in a 70-line parenscript file.

```lisp
(defpsmacro defpsajax (name (&rest args) url &optional (success '(lambda (data) ($d data))))
  `(defun ,name ,args
     (chain $ (ajax (create :url ,url
       :data (create ,@(loop for a in args collect (intern (to-str a) :keyword) collect a))
       :context (@ document body)
       :type "POST"
       :success ,success
       :error (lambda (a b error) ($d a b error)))))))
```

An odd note; I have to quote the default optional function (as above), but I must pass unquoted `lambdas` in, otherwise it barfs. This one's a bit heavier. It's a shortcut for defining ajax functions. This is the sort of thing you just plain can't do in vanilla javascript. You'd have to define it as

```javascript
function defPsAjax(address, dataSet, fn) {
    if(!fn) fn = function (data) {$d(data);};
    $.ajax({ url: address,
             type: 'post',
             data: dataSet,
             success: fn,
             error: function (a, b, error) {$d(a, b, error);}
           });
}
```

and then use it by doing something like

```javascript
function foo(bar) {
    defPsAjax("/url", { "bar": bar }, function (data) { baz; });
}
```

instead of being able to

```lisp
(defpsajax foo (bar) "/url" (lambda (data) baz))
```

I have two problems with that. First, it doesn't kill the boilerplate around defining foo. Second, that shorter macro definition expands into a full `$.ajax` call, which means there's no additional overhead from `foo` calling `defPsAjax` at runtime. Together, those problems prevent you from properly expressing things in vanilla jQuery; you'll incur significant readability and probably trivial performance penalties by creating enough intermediate functions. Neither penalty piles up if you use `defpsmacro`.

There are also a few nice things I get for free, rather than having to define them. As I mentioned [last time](/posts/parenscript), having `who-ps-html` and `format` was already enough to tempt me into using `parenscript`. Putting strings together in `js` is fugly. I'm aware of the hacks, and they're not nearly as satisfying as just having a proper string-formatting primitive available in the language. Trying the same tactic with strings which contain HTML tags crosses over into [pug fugly](http://marketthoughtsandanalysis.blogspot.com/2010/05/ugly-ugly.html) territory without so much as a warning. Even if you absolutely must concatenate strings at runtime, `(+ foo ", " bar " || " baz)` is still easier than `foo + ", " + bar + " || " + baz`. There's a couple of other similarly useful things that you don't see until you work with them. `let` and `let*` are both supported, for starters. `let*` is actually pretty straightforward.

```lisp
(let* ((a 2)
       (b (+ 2 a)))
    (foo a b))
```

expands into

```javascript
var a = 2;
var b = 2 + a;
foo(a, b);
```

but the equivalent let maintains the limitation that declarations don't refer to each other.

```javascript
var a1 = 2;
var b = 2 + a;
foo(a1, b);
```

Another free advantage is optional arguments and implicit returns.

```lisp
(defun foo (bar &optional (baz "mumble")) baz)
```

expands into the javascript

```javascript
function foo (bar, baz){
    if(baz === undefined) {
       baz = "mumble";
    }
    return baz;
}
```

That's it for the good stuff I've discovered so far.

Lets talk about where Parenscript can bite you in the ass.

First, avoid it if you're a lisp newb. There are a lot of parentheses running around when you write your javascript code this way, and just one can make the difference between `$(foo).bar({'a': b});` and `$(foo).bar.create('a', b);`. The real downfall here is that, unlike in plain Common Lisp, it won't throw an error about unbalanced parentheses[^not-enough]. Instead of erroring, it will generate incorrect JS code. This is actually a good argument for using macro-heavy parenscript code because the fewer actual expressions you have to type, the less chance there is that you mistype one. Use your macroexpander and `show-paren-mode` aggressively.

[^not-enough]: If you don't have enough parentheses, it'll still tell you, but it won't give you the typical `"expecting [n] args"` error if you transpose one.

Second, the `chain` macro has some fairly odd behaviour with other macros, and it keeps you from abstracting certain patterns without resorting to `ps*` instead of `ps`. For instance

```lisp
(defpsmacro highlight (&optional (color "\#0f0"))
  `(effect "highlight" (create :color ,color) 500))
```

Having defined that, I would expect `(ps ($ "foo" (highlight)))` to expand into `$("foo").effect('highlight', { 'color': '#0f0' }, 500);`, but it actually does `$("foo").highlight();`. If I wanted to get that first expansion, I'd have to define `highlight` as

```lisp
(defun highlight (&optional (color "\#0f0"))
  `(effect "highlight" (create :color ,color) 500))
```

and call it by doing <code>(ps* `($ "foo" ,(highlight)))</code>. That's not actually horrible (we're only into regular fugly here) but it prevents you from fully using your macroexpander, does no work at macroexpansion time and requires you to quote your input. Manageable, but still a net loss.

The last part is that your javascript definitions share the Lisp namespaces. Which makes sense, since one of the goals of Parenscript is to have js and CL interoprerate at some level, but it still caught me slightly by surprise. What I mean specifically is

```lisp
(ps (defun foo () bar))
```

In addition to expanding out to `function foo () { return bar; }`, that line also defines a Lisp function in the current package called `foo`. The reason I found this out is that I have a habit of giving JS ajax functions the same name as the functions they'll be interacting with on the server side. Don't do that. I spent a good 15 minutes trying to debug a very odd wrong number of arguments error before realizing that I was accidentally shadowing the function I needed to call.

As a final note, and this should really go without saying, parenscript is not a way to avoid learning JavaScript or jQuery (or your framework of choice). It's a way to simplify development work with them after you know them cold and have more than a few hours logged with Common Lisp. Use it properly and it'll serve you well, go in with a broken/incomplete understanding of JavaScript at your own peril.
