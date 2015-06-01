I've had occasion to work with these relatively often, and in various different contexts lately. So I'm going to do the survey just to get some things straight in my mind.

There are two basic approaches to doing routing. The table-oriented one, and the handler-oriented one.

## <a name="table-oriented"></a>Table Oriented

Is probably the most widely known at this point. You have some mechanism for defining handlers, which is entirely separated from the routing, and a central routing table somewhere that contains all the bindings of routes to handlers. You'll see this approach in Python's [Tornado](http://www.tornadoweb.org/en/stable/)

```haskell
...
urls = [(r"/", Index),
        (r"/show-directory", ShowDirectory),
        (r"/play", Play),
        (r"/command", Command),
        (r"/status", ServerStatus),
        (r".*", Index)]
...
```

as well as Clojure's [`compojure`](https://github.com/weavejester/compojure)

```clojure
...
;; snippet from &lt;a href="https://github.com/thephoeron/thephoeron.com">thephoeron.com&lt;/a>, rather than one of my projects

(defroutes app-routes
  (GET "/" [req] (res/splash req))
  (GET "/quantum-computing" [req] (res/quantum-computing req))
  (GET "/physics" [req] (res/physics req))
  (GET "/programming" [req] (res/programming req))
  (GET "/linguistics" [req] (res/linguistics req))
  (GET "/philosophy" [req] (res/philosophy req))
  (GET "/music" [req] (res/music req))
  (GET "/art" [req] (res/art req))
  (GET "/sci-fi" [req] (res/sci-fi req))
  (GET "/impressum" [req] (res/impressum req))
  (route/resources "/static")
  (route/not-found "Not Found"))
...
```

These two have a few minor differences (The `compojure` version routes off to functions, and does so explicitly, while the Tornado version specifies `class`es to handle routing, and does so implicitly (the actual method call itself is generated for you). The `compojure` version is explicit about the HTTP method a particular handler takes, while the Tornado version handles that at the class level; the route targets are expected to have `.get`/`.post`/`.put`/etc. methods defined, which are called as specified by the client request. Finally, the `compojure` version explicitly gives you a `req` argument to pass to your target, which is handled behind the scenes in the Tornado version), of course, but the core concept is the same centralized table of URIs to handlers.

## <a name="handler-oriented"></a>Handler Oriented

Involves putting the routing and handler definition machinery together. This is the approach taken by the `go` server, `house` and `hunchentoot`.

```go
...
http.HandleFunc("/edit/", ShowEdit(wiki))
...
func ShowEdit (wiki *Wiki) func (http.ResponseWriter, *http.Request) {
        t := template.Must(template.ParseFiles("static/templates/edit.html", "static/templates/base.html"))
        return func (w http.ResponseWriter, r *http.Request) {
                pg, err := wiki.GetPage(r.URL.Path[len("/edit"):])
                if err == nil { t.ExecuteTemplate(w, "base", pg) }
        }
}
...
```

```lisp
...
(define-handler (article) ((name :string))
  (aif (for-all `(and (?id :file ,name) (?id :title ?title) (?id :body ?body)) 
                :in *base* 
                :collect (page ((str ?title) :section "blog")
                           (str ?body)
                           (:hr)
                           (prev+next-links ?id)))
       (first it)
       (page ((fmt "Not found: ~s" name) :section "blog"))))
...
```

```lisp
...
(define-easy-handler (llthw-reference :uri "/reference/") (ref-page)
  (let ((the-ref-page (format nil "reference/~(~A~).md" (cl-who:escape-string-all ref-page))))
    (if (probe-file the-ref-page)
        (reference-basic-page ()
          (cl-who:with-html-output (hunchentoot::*standard-output*)
            (str (3bmd:parse-and-print-to-stream the-ref-page hunchentoot::*standard-output* :format :html))))
        ;else
        (reference-basic-page ()
          (cl-who:with-html-output (hunchentoot::*standard-output*)
            (:h4 "Error 404: Not Found"))))))
...
```

Again, minor differences. [`house`](https://github.com/Inaimathi/house) lets you annotate your parameters and handles validation, [`hunchentoot`](http://weitz.de/hunchentoot/) lets you specify a URI that's different than the procedure name, and [the `go` server](http://golang.org/pkg/net/http/) asks for a function rather than giving you a piece of syntax to define it in-line. But the common point they share is that there isn't a table being defined in one fell swoop. It's implicit, and incrementally added to by each handler you define in your codebase.

## <a name="comparing"></a>Comparing...

First off, they're mechanically equivalent. Both of them produce some sort of lookup structure that later gets used in the decision of what response needs to be sent back to a particular client. Which means that the final output of both approaches is ultimately something like `Map URI (Params -> Response)`. The difference is how they get there, and what the implications are for you as the reader of the program.

The Table-Oriented approach keeps all handlers in one place. After having read through that table, you can be reasonably sure that there aren't any handlers sitting around that you've missed. Because it's centralized, this approach


-   **Lends itself to functional handler composition.** You don't need side-effects to compose this table, because you're doing it all at once (so it can be a declaration), and you can imagine writing functions that transform handler tables without breaking abstraction.
-   **Is less flexible regarding runtime handler definition.** Once you're running a server, defining a new handler involves side-effect. In languages that are uppity about side effects, such as Clojure, Haskell or ML, this means it's somewhat more difficult and needs to be explicitly planned for, and the table-oriented approach doesn't allow it out of the box (Although, to be fair, runtime handler-definition is something you only really want while you're writing the program, and almost never something you want to be part of your deployed application. It's very useful while you're writing, but depending on how you set up your environment, you might not actually end up needing it).
-   **Implicitly produces no state clashes.** This is actually a detriment of the handler-oriented approach. The `go` variant doesn't suffer from this, but the Lisp versions do. Imagine what would happen if you used the handler-oriented approach to write two separate micro-service projects, for instance. They'd both be defining handlers into some global table, and if any routes clashed, one would end up clobbering the other. Unless you took some pains to plan for the eventuality, you'd sometimes get a handler silently stomping on another one.


The handler oriented approach is more or less the inverse. Handlers can be scattered about anywhere, so the only real way to be sure you've seen all of them is by loading up your server and inspecting the final table. The advantage you get out of this is that it's more convenient for incremental development, since you can modify one handler definition without touching the rest and can do so without restarting any servers. Additionally, this approach groups parameter validation/parsing structure (where that structure exists) along with the parameter body. That second one is the main win, because as I'll discuss in the next section, it presents an obvious path to removing a level of repetition otherwise found in handler definition.

It's enough to make me wonder whether you could build a hybrid system that had all the advantages and mitigated all of the disadvantages without a large increase in complexity. I'll leave that for another time.

Incoming context shift.

## <a name="the-validation-structure"></a>The Validation Structure

Having worked with a few other web frameworks and servers lately, the main piece of `house` that I end up missing is the automated parameter validation and extraction. And as I've been saying in various real-life conversations, that's a piece that can be abstracted from any particular server. For demonstration purposes, the `article` handler above is actually a bad example

```lisp
...
(define-handler (article) ((name :string))
  (aif (for-all `(and (?id :file ,name) (?id :title ?title) (?id :body ?body)) 
                :in *base* 
                :collect (page ((str ?title) :section "blog")
                           (str ?body)
                           (:hr)
                           (prev+next-links ?id)))
       (first it)
       (page ((fmt "Not found: ~s" name) :section "blog"))))
...
```

...because it has a single `string` parameter, which means we don't need to do any conversion or validation. Here's a somewhat contrived, but more illustrative example:

```lisp
(define-json-handler (v0/api/add) ((a :integer) (b :integer))
  (+ a b))
```

We're expecting `a` and `b` to be integers here. And, we're expecting the return value to be automatically JSON-encoded before its sent back. If you wanted this, you'd normally have to write something along the lines of

```lisp
(lambda (a b)
  (let ((a (parse-integer a))
        (b (parse-integer b)))
    (json:encode-to-string (+ a b))))
```

And if you had done that, you would have introduced the subtle bug involving a failing parse on either `a` or `b`. What you'd *really* want is something closer to

```lisp
(lambda (a b)
  (handler-case
      (let ((a (parse-integer a))
            (b (parse-integer b)))
        (http-200-response (json:encode-to-string (+ a b))))
    (parse-error ()
      (http-400-error
       "Invalid argument. Expected two numbers, got (~a ~a)"
       a b))))
```

And suddenly, a program that should only span seven characters is complex enough that you need to exert non-trivial effort to understand it. And that's without even considering the routing mechanism and actual parameter lookup. As a rule, I like to avoid this level of incidental complexity where I can. And this is a place where I can, because the machinery to automatically do this work is extensive but regular and fairly simple. The current version of `house` has a [built-in solution](https://github.com/Inaimathi/house/blob/master/define-handler.lisp), but it's bound to the handler-oriented style, and is specialized to work with the `house` server. Maybe that's not such a bad thing, but lets think about what we're doing and how we'd generalize, just for fun.

## <a name="thinking-about-it"></a>Thinking About It

Basically, a handler is a function of some number of parameters to a response. Which doesn't sound hard at all. The problem is that those parameters:


1.   don't necessarily arrive in the right order, since they're usually either `x-www-form-encoded`, part of the URI or some combination
1.   arrive in string format, even if they represent values of other types
1.   don't originate from trusted sources, so each parameter might contain *invalid* data given its expected type


Additionally, in many languages it's possible for the handler function to fail in some way unrelated to the input parameters.

Bottom line, we'd like to be able to write a function that naively takes parameters and naively operates on them, and that isn't expected to deal with its own exceptional conditions. Which means that we need some way to take a `Map String String` (our parameters), pull out the relevant parameters, parse them, validate them, pass them to our "core handler" in the proper order, then take the result and send it back to our client using some decoder. **But**, an error during parsing or validation should immediately trigger a 400 error, while an error during the execution of the "core handler" should cause a 500 error to be sent.

My first instinct for a Haskell implementation is clunky, since I can't think of a good way to type functions with arbitrary numbers of parameters. Though it does still gain something; if we do it right, we effectively get the type system for free, and so no longer need to explicitly annotate handlers. Something like

```haskell
module Handlers where

import Control.Monad

type Params = [(String, String)]

class Read a => Param a where
    parse :: String -> Maybe a
    parse str = case reads str of
                  [(thing, "")] -> Just thing
                  _ -> Nothing

instance Param Int
instance Param a => Param [a]

lookP :: Param a => Params -> String -> Maybe a
lookP ps k = lookup k ps >>= parse

withParam :: (Param a) => String -> (a -> r) -> (Params -> Maybe r)
withParam arg fn = \ps -> liftM fn $ lookP ps arg

withParam2 :: (Param a, Param b) => (String, String) -> (a -> b -> r) -> (Params -> Maybe r)
withParam2 (arg, arg') fn = wrapped
    where wrapped ps = liftM2 fn (lookP ps arg) (lookP ps arg')
```

We need `parse` to be a `typeclass` method because we don't want to lock users into using the `reads` approach to decoding their custom types, and we'd basically need a bunch more individual `withParam&lt;n>` declarations, but we could then write something like

```haskell
handlers = [ ("/v0/add/&lt;a>/&lt;b>", withParam2 ("a", "b") (+))
           , ("/v0/sub/&lt;a>/&lt;b>", withParam2 ("a", "b") (-))
           , ("/v0/sum/&lt;nums>", withParam "nums" sum) ]
```

Which isn't the prettiest thing I've ever seen, but spares us the trouble of validation boilerplate every damned time. I'll need to give this some more thought before putting together something more concrete. I'll let you know how it goes.
