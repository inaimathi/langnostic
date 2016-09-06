So lets talk about when and where you want strong types. I'm not buying into either reactionary camp here, I want to think about it seriously. And I don't want this discussion to disappear up its own ass, so lets keep things concrete with a concrete example.

```
6.join(",")
```

And I'm not about to get side-tracked by the entirely orthogonal discussion about when automatic casts  or overloaded functions are a good idea; we'll get into it later. I actually don't care what language that is, which is why I haven't specified a highlighting mode for it, but I put it to you that this is an unambiguous typo. If you write this, you really meant something else.

So, first question.

## What should a language runtime do if it finds itself trying to evaluate this?

I'm hoping we can all agree that the answer is "it should throw an error". It might be a type error, or something more descriptive and domain-specific. If you're a particular kind of language designer, you might decide to call it "undefined behavior" to save yourself the work of specifying and building a useful exception handling system at the expense of your users. But as a user, if you could help it, you'd rather just not get into this situation.

So, second question.

## If a compiler could prove that a particular expression would lead to the above being evaluated, what should it do?

If your answer to that is "it should raise an error or warning", and you started with the position of "we don't want strong types", I've got some news for you.

Now this is not to say that a compiler will catch all such errors (or, alternately, it will catch *all* such errors at the expense of disallowing some untypable, but otherwise well-behaved programs), but *if* it can, it *should*. Because they *are* errors, and if they're not caught by the type system and analysis machinery, they'll have to be caught by much more expensive and boredom-prone humans. By hand, in the snow, both ways, etc. etc.

The argument for strong types in this sense comes down to

> Programming is hard enough. So if there's a way to let the machine do more of the tedious work, we should use it.

The argument *against* is that it tends to take more effort to learn languages that take the statically typed approach. Because you don't just need to know "this system will catch some of your errors". You really need to know something more complex; something related to the universe of typeable programs and how it interacts with what you're trying to write at the time. You need to be at least vaguely aware of what the type inferencer is trying to do, how and why before you'll be able to grasp some of its error output. In other words, the argument against strong types comes down to [intuitiveness](/posts/killing-ideas).

## "Breaking" Safety

Now, there are two related discussions we could have. Namely, those about overloading and auto-coercion.

```javascript
1 + 2
> 3
3 + 4.5
> 7.5
"a" + "b"
> "ab"
[1] + [2]
> [1, 2]
"test 0" + 1
> "test 01"
2 + "nd test"
> "2nd test"
```

I'm steering clear of [deliberately insane](http://xkcd.com/1537/) examples here. Depending on your point of view, the above may be reasonable or not. I'm not making a judgement call one way or the other. Having a "strong" type system tends to make code like this less convenient. You might expect to find yourself using different functions for integer and float addition, as in [OCaml](http://learnxinyminutes.com/docs/ocaml/), or at *least* different functions for list/string concatenation and number addition, as in [Haskell](http://learnxinyminutes.com/docs/haskell/). In practice, you find yourself having to do lots of little manual number conversions in Haskell too, even though they've made the `+` funcion polymorphic, because it's [sometimes unclear what you want as output](https://wiki.haskell.org/Generic_number_type#Problem).

Now, really, for most of the places you'd *like* to pull these tricks, the problem is that the underlying type system in whose context you're attempting to do so is not elaborate enough to let you. Most of the rest of the time, pulling these tricks can get you [into serious trouble](https://www.destroyallsoftware.com/talks/wat) at the language level. But even that's a complete aside. The point is, *this* is the trade you're making.

**When you reach for a strongly or weakly typed language, you're deciding between the convenience of expressing certain assignments and comparisons more succinctly, and the convenience of having large classes of errors prevented on your behalf.**

That's actually a choice you can debate about. There are pros and cons. You might prefer one side or the other, a particular task might call for one or the other, and there doesn't seem to be a clear-cut `Correct` answer for all situations, but at least make the call knowing what the stakes are.

## My Preference...

...is not particularly important. Leastwise, it shouldn't be important to you, since this *is* partially a matter of preference and partially of problem, but I do have one in general.

1. I like type systems that let me [write](http://sml-family.org/) very [little](https://www.haskell.org/) extra [information](https://ocaml.org/), while giving me strong guarantees about the correctness of my program. And I'm an equally big fan of [type](http://clhs.lisp.se/) systems [that](http://racket-lang.org/) let me write nothing, while giving me some guarantees about the correctness of my program.
2. I can just about stand type systems that let me [write](https://www.ruby-lang.org/en/) nothing [and give](https://www.python.org/) me no guarantees, or type systems that make me [annotate heavily](http://www.rust-lang.org/) in [exchange](http://golang.org/) for strong safety guarantees.
3. I have no patience whatsoever for languages that make me [annotate](https://en.wikipedia.org/wiki/C_%28programming_language%29) heavily [and give](http://www.cplusplus.com/) no or weak guarantees about the safety of the result.
