I went to the [Dynamic Languages Smackdown](http://gtalug.org/wiki/Meetings:2010-12)[^aside] yesterday, and I'm recording my thoughts before losing too many of them. It was an event hosted by [GTALUG](http://gtalug.org/wiki/Main_Page) *(the Greater Toronto Area Linux User Group)*, and basically involved advocates of 7 languages standing up and talking about theirs.

[^aside]: The irony of a guy who writes a blog called "Language Agnostic" going to something called the "Dynamic Languages Smackdown" is not lost on me.

It turns out I wasn't the only language nerd there, surprise, and if nothing else I got a [new book recommendation](http://pragprog.com/titles/btlang/seven-languages-in-seven-weeks) out of it.

The seven languages were Smalltalk, Ruby, Python, Erlang, Lisp, JavaScript and Perl, and the format was

- Introduction
- Questions from the audience
- Questions
- Code examples

To summarize, I kicked off (subtly, so no one seems to blame me yet) a line of questioning dealing with canonical implementations that more or less got touched on repeatedly throughout the talk. Erlang guy had it easy, because that language actually has just one canonical implementation[^admittedly]. Other than Erlang, what struck me here is how diverse the pool actually is. I mostly hack Common Lisp these days, and only vigorously play with Ruby, Erlang and Haskell[^not-proud], so I was under the impression that Lisp was the only freak language that had so many implementations to choose from[^lisps]. That turned out to be a misconception; Ruby has JRuby and Iron Ruby[^rubies], Myles Braithwaite put up a slide listing twenty or so different Python implementations[^pythons], Smalltalk has at least two open-source forks[^gnu-smalltalk], the Perl community is apparently split between 5 and 6 and JavaScript has at least three different server-side implementations and many, *many* client-side ones.

[^admittedly]: With one attempted port to the JVM that apparently no one takes seriously yet.
[^not-proud]: And PHP at work, but as you can see by the logo bar, I'm not terribly proud of that.
[^lisps]: First off, the "language" is split into Scheme, Common Lisp, and Other. In the Scheme corner, you have [Racket (formerly PLT)](http://racket-lang.org/), [Guile](http://www.gnu.org/software/guile/guile.html), [Termite](http://code.google.com/p/termite/) (which runs on top of [Gambit](http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Main_Page)), [Bigloo](http://www-sop.inria.fr/mimosa/fp/Bigloo/), [Kawa](http://www.gnu.org/software/kawa/) and [SISC](http://sisc-scheme.org/) (and a bunch of smaller ones). In Common Lisp, there's [SBCL](http://www.sbcl.org/), [CMUCL](http://www.cons.org/cmucl/), [Clisp](http://www.gnu.org/software/clisp/), [Armed Bear](http://common-lisp.net/project/armedbear/) and [LispWorks](http://www.lispworks.com/) (and about 10 smaller ones). Finally in "Other", you find crazy things like [Emacs Lisp](http://www.gnu.org/software/emacs/emacs-lisp-intro/), [AutoLisp](http://en.wikipedia.org/wiki/AutoLISP), [Arc](http://www.paulgraham.com/arc.html), [Clojure](http://clojure.org/) and [newLisp](http://www.newlisp.org/) (which are all technically Lisps, but conform to neither the Common Lisp nor Scheme standards). This is sort of why I thought having a representative for "Lisp" is kind of a joke at a talk like this; which Lisp are you talking about?
[^rubies]: Both of which purportedly conform to the same spec and are both interchangeable and "official" as far as the community is concerned.
[^pythons]: Which disparately support Python 2.5, 2.9 and 3.x specs.
[^gnu-smalltalk]: And [gnu-smalltalk](http://smalltalk.gnu.org/), but that wasn't really discussed.

It's weird, because as I've said, I was under the impression that "a language" meant one canonical implementation with one or two experimental projects, but *(at least in the dynamic world)* that seems to be false. It's odd, because people cite "difficulty choosing an implementation" as one of the principal reasons not to go with Common Lisp. I guess it's more of an excuse than a reason after all.

The other big surprise was the age of the advocates. Of the seven, only Alan Rocker (the Perlmonger of the group) had the sort of beard you'd expect, and everyone other than Alan and Yanni (the Smalltalk presenter) seemed to be students. I'm particularly happy about this since Lisp gets cast as the old-man's language, but in reality, programmers my age seem to be more common. Not that "age of the community" is important in any tangible way, just idly interesting.

"Smackdown" thankfully turned out to be too strong a word; other than a fierce rivalry between the Python and Ruby presenters *(and a few low-blows from both of them aimed at JavaScript)*, everyone was respectful of the other languages there. It was fairly informative, and I'm going to pick up and play with Clojure, a Smalltalk (either gnu or [Pharo](http://pharo-project.org/home)) and more Python as a direct result.

A note for future presentations in this vein though:

1. Please don't do code examples last. This should have been done up-front with the introductions, and probably allotted 15 minutes or so per language. Alan didn't even get enough time to present his.
2. Either admit that these discussions will take more than two hours, or invite fewer languages at once. The conversations easily could have continued for a further hour or two[^and-probably-did].
3. Be prepared with the slides beforehand[^no-powerpoint].

[^and-probably-did]: And probably did at the pub after the event, but I had work the next day, so I couldn't go.
[^no-powerpoint]: Anyone else would be able to blame PowerPoint, but this was the Linux User Group, so you don't get that luxury.

### Preliminary Impressions of Smalltalk

I did briefly try to get into Pharo, but I found it annoying to say the least. This doesn't mean I won't keep trying[^from-the-future]; I had a negative initial reaction to pretty much every language I currently know and love. There are some definite initial concerns though, the biggest of which is that Pharo insists that you use its "Environment" (which is only really a big deal because of the way that environment is constructed). It's heavily mouse-dependent, going so far as recommending you have a three-button mouse, and it insists on handling its own windowing. Which means if you got used to a [tiling](http://www.bluetile.org/) [window](http://www.nongnu.org/stumpwm/) [manager](http://www.winsplit-revolution.com/), you are *so* screwed. The GNU implementation is titled "The Smalltalk for those who can type", so at least I know I'm not alone. Minor concerns about image-based development include things like "How does source control work?" and "how do I use Pharo on a team?", but I'm sure those are resolved and I simply haven't dug deeply enough to have an idea of how yet.

[^from-the-future]: Hello from 2016! I did end up trying [Pharo](http://pharo-project.org/home) again. You can read about it [here](/posts/smalltalk-first-impressions). Really, I just went through the tutorial for a web framework called Seaside before I started having too little free time to make progress. I still thoroughly recommend the experience but, at least in my case, Smalltalk is a language I ended up learning and then not using very much.
