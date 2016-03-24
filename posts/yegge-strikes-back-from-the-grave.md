So I've been fooling around with some new stuff.

Actually, before I tell you about that, quick update. dieCast is now in the early beta stages. It's actually capable of supporting games, but it's got a long way to go before it's something I'll be proud of. We're about three months away from a public beta from where I'm sitting. For the testing stage, I'm ending up using some creative-commons enabled sprites. I'll probably keep them as a subset of the final sprite lineup, if the license permits, but the intention is to get original artwork up.

Ok, now then.

I've been fooling around with some new stuff.

Or rather, some very very old stuff. Over the last couple of days, I've decided to pick up Common Lisp and Portable Allegro Serve again. I gave up on trying to install PAS on SBCL after about twenty minutes though, and promptly switched out to Hunchentoot, and all I really have to say is

Holy.

Shit.

I already have some projects underway with Racket (including Diecast), but goddamit, I think I made the wrong decision. The quote from [Yegge](http://steve-yegge.blogspot.com/2006/04/lisp-is-not-acceptable-lisp.html) goes something like

> Most newcomers independently come to the same conclusion; Scheme is the better language, but Common Lisp is the right choice for production work.

Bottom line, I remember disagreeing a long time ago, but I've uh... independently come to the same conclusion. [^from-the-future]

[^from-the-future]: Hello from 2016. Hm. So... huh. So since I've written this post, I've gone back and forth between Common Lisp and Racket a few times, seriously tried out Ruby, Python, Erlang, Haskell, Standard ML and Clojure. I'm not sure I'd come to the same conclusion anymore. Racket is pretty badass as Schemes go, and is in fact my personal go-to recommendation for **First Lisp You Should Learn (if you don't love the JVM)**. Clojure is in a similar position for people who *do* love the JVM. However, when someone asks me "What functional language should I start out with?", these days I'm far more likely to point them at Haskell/OCaml/Standard ML than any of the Lisps. You'll notice that Common Lisp is nowhere to be found in that list. I've already explained why in [this](/posts/recommendations) post.

Racket seems to be as good as Scheme gets. It has built-in support for everything from hashes and regexps to x-path and http. It has file-system bindings, guaranteed tail-recursion and pretty much the best package system I've seen (from the downloaders' perspective, at least, Scribble is a bit of a bitch to get familiar with if you plan to actually document your own code).

So why am I having serious second thoughts?

Lots and lots of little things. Now that I've actually had some time to play with both contenders, mastered both IDEs, played with both macro debuggers, ran web servers on both and lived in each language for a decent length of time, I think I can finally compare them, and gain some sliver of insight from the comparison. And it's a damn close race. The biggest differences turn out not to be what everyone was pointing at. I have a link in the sidebar over there pointing to "Scheme vs. Common Lisp", which purports to tell you the differences between the two, and maybe three of those actually trip you up to any significant degree.

So here's the big stuff. Racket vs Common Lisp from a young hacker's perspective.

## 1. Documentation

The [docs](http://docs.racket-lang.org/) are badass, and centralized. Second to none. They have search running over all functions in their implementation (and you need it with the amount of stuff it has), code examples all over the place, and a comprehensive set of tutorials perfect for beginners. Common Lisp probably has more overall information on it, but it's scattered across [CLiki](http://www.cliki.net/index), [Common Lisp Directory](http://www.cl-user.net/asp/fVOm/sdataQ1UwW195cQ5hDQ3TOH8X8yBX8yBXnMq=/sdataQu3F$sSHnB==), [Hyperspec](http://www.lispworks.com/documentation/HyperSpec/Front/), [various indie package pages](http://weitz.de/hunchentoot/) and [Bill Clementson's archives](http://bc.tech.coop/blog/). `M-x slime-documentation-lookup` helps, but it only searches the Hyperspec. That's plenty of info for the veteran, but (if I could imagine my point of view about two years ago) it wouldn't be sufficient for someone who's, say, looking for a complete listing of format-string options[^public-service].

[^public-service]: As a public service, the way you find that is to look up format, then scroll about half-way down the page where you will be pointed to section `22.3` for more information on formatted output.

## 2. Package Repositories/Installation tools

Common Lisp has asdf[^from-the-not-so-distant-future], which is awesome compared to the tools found in most other languages I've used, but Racket beats it pretty handily. It's basically the same story as documentation. There's technically more stuff out there for CL, but it's scattered, and since development is distributed, you'll get some duplication of effort. There are four or five different HTTP servers, for example, and at least three HTML-templating libraries. Granted, there's a clear "best" in each category, but you really need to do your reading in order to find that out. Racket has a smaller offering[^gaping-holes], but it's neatly organized, indexed, and accessed by typing `(require (planet [package-name]))` in the declaration section of whichever file you need the new package for. No hunting, no missing GPG keys. These first two points are probably the ones I'll miss most from the Racket offering.

[^from-the-not-so-distant-future]: Hello from 2012. I can't believe I managed to go so long without adding this note. As of the end of 2010, [quicklisp](http://www.quicklisp.org/beta/) also exists, and is awesome. That means that the gripes I had about `asdf`ing things are moot, since you don't need to for the most part. Thanks to [`ql:system-apropos`](http://www.quicklisp.org/beta/#basic-commands), it's also fairly easy to find CL packages, so I guess Racket no longer wins this one. I have no idea what they've been up to for the last year though, so they probably made a thousand and one improvements all over the place too.

[^gaping-holes]: The biggest gaping holes are in the document generation area; there is no such thing as a good Racket PDF/PostScript generator.

## 3. The Web Server

This is actually a place where more choice would do Racket some good. They do have a pretty cool web server, but it's far from fast in practice. It also seems to crash more often than I'd like for a production app. Nothing like once per week, but it's happened a few times so far. The trouble is how it behaves. It's basically Tomcat, minus the copious installation headaches; you need to get all your code in order, make sure it'll run, then execute. And that's it. If you need to make changes (like, while developing web apps) you need to tweak the code, then restart the server, then re-navigate to the page you were just on because it auto-generates new urls each time. This is an exercise in frustration, and is one reason that I've still kept up on my PHP and Python skills this entire time. The languages may be slightly worse, but they're interpreted, so a change doesn't need to bring down the whole server. That's how [Hunchentoot](http://weitz.de/hunchentoot/) works too. You load your files, then start the server. If you need to make a change, you evaluate the new code against the actual, still-running server. I wouldn't use this in the wild, but during the development stage, it is hot, buttered, bacon-wrapped power. That alone seems to be enough to pull CL into the lead as far as my use of it is concerned.

Now in, Racket's defense, they're aware of this. There was a concern about keeping LISP's inherently reflective nature, and they decided not to because it trips up so many people that they figured it wasn't worth the headaches. So the server forces you out while it runs, and the REPL bugs you to do a clean run every once in a while if your source has changed. I appreciate the sentiment, because it really was made to be a teaching tool, but I'm being mighty tempted by the dark side regardless. There's also a concerted effort from Racket to keep things byte-oriented. For example, there is no supported way to get a list of POST/GET parameters out of a request (other than "manually") in Racket. "Manually" entails getting a list of binding objects out of the request and mapping over them to get a list of byte-strings out. There's also a few other little gotchas (like how awkward it is to actually create a link whose result is another scheme function, and how url-based dispatch is for whatever reason NOT the default).

## 4. The Format function

This may sound like a nitpick, but I'm not into the nitpicks yet. This is actually a difference. In Racket, you're stuck with (format "~a" blah). It only accepts formatting directives, rather than CL's richer set of formatting, flow control and kitchen sink. It also always returns its result, and doesn't have the option of printing to standard-out (you have to use printf for that). I didn't think this would make as big a difference as it did, actually, because I've gotten used to the simpler Scheme format, but hot damn is it awesome to be able to do something like (format nil "~a ~{ ~a: ~@[ ~a ~]~}" (car blah) (cdr blah)) instead of resorting to several function calls for the same effect.

## 5. Plists

Basically same story as above. I forgot how useful these actually were for day-to-day purposes. I mean, I still bust out hashes for bigger stuff, but little tasks all over the place are made just a tiny bit easier with the use of p-lists instead of a-lists.

## 6. Function names.

Ok, now we're into picking nits. It's not a huge deal, but the scheme conventions are cleaner and more consistent. If you're dealing with a predicate, it ends with "?", if you're dealing with a side-effect function, it ends with "!". Common lisp has a grab-bag. Some predicates end with "p" (as in listp), but most are just the unmodified word (as in member). Also under this category is the lisp-1 vs lisp-2 thing; because there's separate namespaces for functions and variables in CL, there's two let types (let for variables and flet for functions) and two definition types (defun and defvar). Because functions get treated differently from other variables, some things are a bit trickier in CL; for example, while you can still do (apply (lambda () 42) '()) or (mapcar (lambda (num) (* 2 num)) '(1 2 3 4 5)), you actually can't do something like (setf foo (lambda () 42)) followed by (foo) (you would either need to call foo with (funcall foo) or define it as (setf (symbol-function 'foo) (lambda () 42))). The Scheme equivalent is (define foo (lambda () 42)), after which (foo) does exactly what you think it will.

## 7. Macros

Racket has define-syntax-rule and define-syntax, as well as library support for define-macro, which is a copy of CL's non-hygenic defmacro. In practice, I found myself using define-macro most of the time, so it shouldn't be too big a problem to switch here. Admittedly, define-syntax made it extremely easy to define recursive macros, but lisp has a number of iteration options that make it close to a non-issue.

## 8. Iteration

This one's probably the tiniest deal there is. Common Lisp has a bunch of iteration functions/procedures, from the loop macro to dolist, to mapcar and friends. Scheme really only had map and tail recursion, and I sort of preferred that. The reason I list this as "tiny deal" is that my particular CL implementation (SBCL if you must know) does tail-call optimization anyway, so I could just keep up my wicked, functional ways if I wanted to.

## 9. The IDE

For beginners, Racket wins it. I remember having this conversation with myself earlier; a binary IDE portable across OS X, Linux and Windows, with nice buttons to do things like "Run" and "Macro Step". It's perfect when you're starting out because it's nothing like the near-vertical learning curve of Emacs, but it ultimately limits you. Since I started with Racket, Emacs has become the main program I use. Seriously, something like 75% of all my computer time is spent here, and the rest is split between Klavaro and Conkeror. I'm contemplating getting a shirt that says something along the lines of "Emacs is my master now". Long story short, once you know LISP (or, to be more precise, three LISPs), Emacs is by far the better IDE.

Now that I've laid down all my gripes, the pattern emerges, and it's definitely what Yegge was talking about. Scheme is built to teach and learn (and possibly prove things formally). Even Racket, whose developers are self-declared hackers who go above and beyond the R6RS implementation to provide a pretty decent production candidate, errs on the side of making things easier for beginners rather than easy for veterans, and it stresses academic application over production application. Common Lisp is the precise reverse. It exacts a heavy toll in experience and patience, and the reward is a measure of power beyond other options. It's also crafted (or perhaps evolved would be a better word) for production rather than theoretical purity. I can appreciate that.

So there. If you want the executive summary:

**Racket:** Theoretical purity and conistancy before practical considerations. Centralized development, indexed for your convenience. Make it easy to learn, consider the newbies.

**Common Lisp:** Get shit done first, consistency and purity are acceptable collateral damage for terseness. Distributed development, find what you can. Make it powerful, the newbies better watch and learn first.

The choice is pretty simple. Common Lisp wins as soon as you know what you're doing. But while you're getting your bearings straight, go for Racket. For what it's worth, I won't abandon it. I still plan to put out a decent PostScript generation library for PLaneT before I get working on CL hardcore, and I'll always keep it around as a second scripting language (along with Ruby), and I still have several Scheme projects to maintain, but the days of typing M-x run-scheme instead of M-x slime consistently are over for me.
