[Stanza](TODO) is an optionally typed language reminiscent of equal parts [Scheme](TODO) and [Python](TODO). It's still a sorta-research language. Tonight, three eights of the Toronto Computer Science Reading group met with [the implementer|one of the implementers](TODO) to talk a little bit about it. [Scott](TODO) also did some [preliminary hacking](TODO doggerel) before-hand just so he had some questions to ask. It was a pretty interesting presentation, all things considered. [The slides are up over [here](TODO), if you're interested](TODO). The two big things I took away are

## Multi-method based object systems are the way to go

That may sound like more of a conclusion than it really is. The [object system](TODO) in Stanza is a multi-method style system very reminiscent of what we've got over in Common Lisp. And it solves an alleged problem in the CL generic system that I didn't know about. The example from the slides looks something like

```lisp
(defpackage :foo
  (:use :cl))

(in-package :foo)

(defgeneric argle (a b))
(defmethod argle ((a integer) (b integer)) (+ a b))

(defpackage :bar
  (:use :cl))
(in-package :bar)

(defgeneric argle (a))
(defmethod argle ((a string)) (concatenate 'string "argle " a))

(defpackage :baz (:use :cl :foo :bar))
(in-package :baz)

(argle 3 4)
(argle "testing")
```

The alleged bad thing is that there's supposedly an error that comes up at the end there, when Lisp gets confused about which `argle` it is that we're calling, so we have to annotate those even though they're generic.

```
...
(foo::argle 3 4)
(bar::argle "testing")
```

The only problem I have with this is that it's complete horse-shit. In an actual module, you'd have to do

```
(defpackage :foo
  (:use :cl)
  (:export :argle))

(in-package :foo)

(defgeneric argle (a b))
(defmethod argle ((a integer) (b integer)) (+ a b))

(defpackage :bar
  (:use :cl)
  (:export :argle))
(in-package :bar)

(defgeneric argle (a))
(defmethod argle ((a string)) (concatenate 'string "argle " a))

(defpackage :baz (:use :cl :foo :bar))
(in-package :baz)

(argle 3 4)
(argle "testing")
```

At which point it's obvious that the error should be tripped at the point where you import two modules into `baz` that both export the same symbol. Now, it's fair to say that if these symbols both represented generic functions, this should somehow be an automatically resolvable problem, but that seems like a pretty tiny edge-case to go complicating your module system over.

The Stanza behavior in this case is pretty elegant. A transliteration of that initial listing will actually compile and run as expected, with no namespace errors or symbol conflicts. Which tells me two things. One, all symbols are automatically exported in Stanza[^may-or-may-not-be-a-good-idea]. Two, the generic resolution routines in Stanza are _really good_. Good enough to save typing where you'd sort of expect to _have_ to specify something.

[^may-or-may-not-be-a-good-idea]: This may or may not be a good idea. On the one hand, it's pretty useful to hide certain implementation details so that they're changeable later, and so that users of your code don't accidentally start depending on pieces of implementation detail from some internal module you've written only for your own sanity. On the other hand, it's pretty annoying to be on the receiving end of that situation when the function you want is implemented generically enough to serve your particular use case.

Anyway, I whole-heartedly support this design decision, but would still rather the author stop ribbing on Lisp for the purposes of looking cool in front of industry programmers.

## Untagged unions are terse

A tagged union is how you get dynamic-like behavior in statically typed languages. If you've read [chapter n of PFPL](TODO), and perhaps kept up with [Rob Harper's blog output](TODO), you may already have learned that Dynamically Typed Languages Are A Subset of Statically Typed Languages. Which is a pretty bizzarre truth if you haven't thought about it, but it is a truth nontheless. The part that it leaves out is the notational convenience you get from mixed-type collections in dynamic languages. You can reclaim that in statically typed languages by using Tagged Unions, or "sum types".

So if you want a list that looks like

```haskell
[1, "two", 3, "four"]
```

in haskell you can do

```haskell
data MyType = I Int | S String deriving(...)

l = [I 1, S "two", I 3, S "four"]
```

So at the expense of labelling your types, you can have a mixed-type array. The main downfall of this approach is that if you define a new function that acts on `[MyType]`, you can no longer hand that function `[Int]` or `[String]`, directly even though the transformation between these is purely mechanical. You must instead cast the sequence

```haskell
foo $ map I ints
```

This is a fairly small amount of typing, and the Stanza guys apparently don't want you to do it. I've got to admit, I'm skeptical. In theory, this seems legit, but I'm having a _really_ hard time remembering the last time I had to deal with a real polymorphic list[^outside-of].

[^outside-of]: That is, "last time I had to deal with a real polymorphic list outside of those that I have to deal with as a result of the decisions of their implementors". The big examples in my mind at the moment are the DOM, JSON trees, and various Clojure/Common Lisp parse trees. These could in theory have been implemented as tagged-union style structures without too much hassle, so I'm not counting them as counter-examples. I'm also not discounting them, because I'm not sure how much extra friction it would cause to do the explicit type label thing in these situations.


## Thoughts on Stanza

I'm honestly unsure whether I'll have the time and energy to wrestle with it, but I damn well might. For startesrs, [as far as I can tell](TODO), Stanza has no socket implementation yet, which means there's room for enterprising young programmers to build something useful that people end up using there. Given how the FFI system works, I'm not sure it's out of the question for me to hook into some C-based implementation of the basics and bootstrap a full HTTP server from there. I'll let you know how it goes, but certainly not before I write about both [Congregate](TODO) and [Howl](TODO).
