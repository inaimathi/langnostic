Recently, I picked up [parenscript](http://common-lisp.net/project/parenscript/). I've been meaning to, it's available through [quicklisp](http://www.quicklisp.org/), and there was an increasing amount of repetition in my js code, so I gave it a shot. I'm about half way through putting together my first js file with it, and goddamn is this sweet.

I've read [arguments](http://news.ycombinator.com/item?id=978299) that it creates a needless abstraction over JS that's better handled through jQuery (which I have some experience with). That's actually one of the reasons I've been a bit slow on the draw to this one. A combination of assuming that additional abstraction would cost more than it would benefit, and having experience with a technology that's Good Enough™. After having put together cl-css, though, I've noticed that even a minimal Lisp-based abstraction layer over a target language gives you a lot. If nothing else, you get macros and backquoting for free.

In the case of parenscript, there's actually a bigger win (which has variously been "solved" by [regex templating](http://www.mattsnider.com/javascript/template-string-replacement-function/), [hidden concatenation](http://ejohn.org/blog/javascript-micro-templating/) and [frameworks](http://code.google.com/p/trimpath/wiki/JavaScriptTemplates) of [varying success](http://www.prototypejs.org/api/template)). Well, there's no reason to deal with JavaScripts' downright criminal lack of string templating with the grin-and-bear method. Even beyond that, there are quite a few places where the right thing for your JS code to do is generate further HTML. You sometimes want these components generated on the client side, because they're useless clutter if the client has javascript disabled.

Out of curiosity, have you ever tried doing that by hand? I'll wait, go ahead, give it a shot. Load up a blank page, include jQuery, then let me know what the best way is to generate the HTML for a google-maps-esque UI widget with javascript.

Well, here's how html-generation looks in parenscript

```lisp
(chain ($ ".ui-container")
       (prepend
          (who-ps-html (:div :class "ui-widgets"
                           (:img :id "directions" :src "directions.jpg")
                           (:img :id "street-view" :src "street-view"))))
       (css (create :cursor "pointer")))
```

In other words, exacly like html-generation in cl-who, which is to say, "beautiful". The chain calls are ugly[^for-single-method-calls] in comparison to the standard jQuery `$(".foo").bar()`, and create isn't objectively brilliant[^personally-prefer], but the ability to do string templating and HTML generation in Lisp is such a load off my mind that I don't care. It adds a minor inconvenience in places I couldn't bring myself to care about, but provides relief specifically in the most tedious and error-prone parts of javascript development.

[^for-single-method-calls]: For single method calls, anyway. In any case, I get the feeling I could macro my way out of this pretty easily. Something like
```lisp
(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector)
      ,@chains))
```
seems to more or less solve my pain points.
```lisp
(ps ($ ".ui-container"
       (prepend (who-ps-html (:div :class "ui-widgets"
                                   (:img :id "directions" :src "directions.jpg")
                                   (:img :id "street-view" :src "street-view"))))
       (css (create :cursor "pointer"))))
> "jQuery('.ui-container').prepend('<DIV CLASS=\"ui-widgets\"><IMG ID=\"directions\" SRC=\"directions.jpg\"><IMG ID=\"street-view\" SRC=\"street-view\"></DIV>').css({ 'cursor' : 'pointer' });"
```
`j-query` seems really weird, but it expands properly, and it'll only ever show up in the macro definition anyway.

[^personally-prefer]: I personally prefer `(create :foo "bar" :baz "mumble")` to `{ 'foo' : 'bar', 'baz' : 'mumble' }`, but I'm sure many would disagree.

There's very little that wouldn't strike me as an improvement over

```javascript
$(".ui-container")
    .prepend("<div class='ui-widgets'>"
             + "<img id='directions' src='directions-icon.jpg' />"
             + "<img id='street-view' src='street-view-icon.jpg' />"
             + "</div>")
    .css({"cursor": "pointer"});
```

And god help you if you need to sneak a variable in as the content/class-name of one of those tags.

Keep in mind that this is for a simple, throwaway example. If I wanted to get fancy, I'd throw in some stitched functions and macros[^in-fact].

[^in-fact]: In fact, I'll post the code of that module I'm working on once I finish the thing, and I'll try to work out how I would have done it by hand.

So yeah. My first reaction was a resounding "Meh. I don't need this level of abstraction. I already know jQuery". The ability to have `defmacro` and `format` available to me while writing Javascript piqued my interest, and `who-ps-html` just sort of sealed the deal.

It's a little embarrassing that the title of this blog is becoming less and less accurate the more time I spend with Lisp. I've been using an increasing number of s-exp generators[^score]. Actually, embarrassing isn't the right word for it.

[^score]: [cl-who](http://weitz.de/cl-who/), [cl-css](http://www.cliki.net/cl-css), [clsql](http://clsql.b9.com/) and [parenscript](http://common-lisp.net/project/parenscript/), in case you were keeping score.

Worrying.

> Tools, of course, can be the subtlest of traps. One day, I know, I must smash the emerald.
