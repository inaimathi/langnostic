Fuck it.

I can't show you the insides, but I can damn well talk about it. And I kinda have to, because there are already a few things that surprised me while writing my own LISP in C implementation, and a couple of things I need to make decisions on before making more progress.

### It's Easier Than You Think

Seriously.

If you don't have the goal of optimizing any part of the language for performance, you can get a very comfortable environment going with much less work than you'd think. Or at least, much less work than I thought. I dunno, maybe you were more informed about these things. I on the other hand am thoroughly surprised to be sitting within striking distance of a production-capable environment after just over 1K lines of C. *C*. And these aren't line-noise lines. You can actually read all of it without having devoted large chunks of your brain to C-specific trivia. There are even comments in the few places that non-obvious, non-name-related things need to be taken into consideration.

The reader in particular was giving me pause because I had never actually sat down to write one by hand, but it turns out to be perfectly straightforward except for exactly one gotcha.

That gotcha is the leading dash.

See, if you discount leading `-` signs, you can pretty much write a full LISP reader entirely using `getc`<a name="note-Mon-Dec-01-211220EST-2014"></a>[|1|](#foot-Mon-Dec-01-211220EST-2014). It gets mildly tricky depending on precisely how hard-assed you want to be about contiguous symbol/paren/quote situations, but you can do it. As soon as you include `-`, you suddenly *need* some sort of `peek-char` equivalent in order to stay even remotely sane. Because a `-` might start off


-   `-123   ;; a number`
-   `-test  ;; a symbol starting with -`
-   `-      ;; a symbol composed entirely of -`


Unless it's acceptable that users can't enter negative number literals, you have to deal with this ambiguity. I *don't* think it's acceptable, but the only way it would be was if we had a specific negation procedure. And *that* semi-leads us into part one of what I'm considering right now.

### The ROI of Variadic Functions

In the vast majority<a name="note-Mon-Dec-01-211224EST-2014"></a>[|2|](#foot-Mon-Dec-01-211224EST-2014) of LISP interpreters available today, you can do this:

```lisp
> (- 5)
-5
> (- 5 6)
-1
> (- 5 6 7)
-8
> 
```

That's right. The built-in `-` function dispatches on the number of arguments it receives to determine its behavior. If you pass it one argument, it acts as negation, if you pass it two, it's vanilla subtraction, and if you pass it more, it acts like a `fold` of `-` over the arguments, starting with the first one as the memo.

It's not just LISPs either. Most languages let you write negative number literals by putting a dash in front of the number. Whether it's by fucking with their reader hard enough to make the arithmetic operations language *keywords* rather than potentially infix functions, or by providing a variadic minus function. The only exception I can think of is [Standard ML](http://learnxinyminutes.com/docs/standard-ml/), in which `-` is a binary infix function, and there exists a separate negation function represented by `~`.

The reason I'm thinking hard about whether to pull shenanigans like this is that *variadic functions cost you some easy composition at the low level*.

In particular, if I wrote a lisp wherein all functions had a fixed number of arguments, it would be easy for me to imagine being able to do something like `(map (+ 2) (list 1 2 3 4 5))`. Because, *of course* `(+ 2)` is

The function of one argument that adds `2` to its argument.

On the other hand, you absolutely wouldn't be able to do that if `+` were variadic. Because then `(+ 2 3)` might be intended either as "add 2 to 3" or as a partially applied function waiting for an arbitrary number of additional arguments.

It's worth noting that even this basic kind of composition feels mildly restrictive. For instance, you could write `(map (- 2) (list 1 2 3))`, or "subtract each element of this list from two", but you'd still need a manual anonymous function to say `(map (fn (n) (- n 2)) (list 1 2 3))`. It's also not clear to me how useful these compositional tricks are outside of a lazy language, or how often they'd come up over the course of a particular project. That's important, because if I go ahead and implement `&rest` args, I lose the ability to pull said tricks entirely. `&rest` and `&body` args meanwhile, tend to be very useful in the context of new macros, and very few other places<a name="note-Mon-Dec-01-211341EST-2014"></a>[|3|](#foot-Mon-Dec-01-211341EST-2014).

Optional arguments are useful in that they allow you to punch your future self *directly in the mouth* during the prototyping phases of most projects. And I've sort of been trying to avoid being too hard on future me lately. He's got enough problems. So it wouldn't particularly hurt to lose those. Anywhere you could put an optional argument, you could instead put a keyword argument. And *those* are worth their weight in gold in terms of maintaining compatibility with future instances of the same function. However, I can sort of imagine a system wherein you *could* implicitly partially apply key args<a name="note-Mon-Dec-01-211345EST-2014"></a>[|4|](#foot-Mon-Dec-01-211345EST-2014), so I'm unsure that the features can't coexist. I'll let you know how it goes, I guess.

### Part Two

I mentioned earlier that contemplating the `rest`-args vs. implicit partial application dichotomy was *part one* of the things I'm considering. Here's another:

What the ever-loving *fuck* do I do about built-in symbols? Not built in *functions*, certain symbol literals that are literally part of the dispatch table found in the internals of `eval`. Specifically: 

```
true
nil
def
fn
macro
begin
if
set!
quote
quasiquote
unquote
splice
```

There seem to be two directions I can head in:


1.   treat them like any other symbol by interning them in the global environment with values of their name symbols
1.   special case them in memory in the form of specific symbols that get pre-`intern`ed and stored when memory gets initialized


They each come with their own boatload of problems, so I'm not sure where the win is really supposed to be.

If I go route 1, suddenly checks against these symbols get expensive<a name="note-Mon-Dec-01-211352EST-2014"></a>[|5|](#foot-Mon-Dec-01-211352EST-2014). And that's bad because they happen in each call to `eval`, which is mutually recursive with `apply`. In other words, this sounds like exactly the sort of thing that immediately and permanently kneecaps performance. With this approach, it's also entirely possible to `set!` any of them to something else, and suddenly language behavior changes in unpredictable ways. On the *plus* side, users can safely define local symbols and functions named `fn`, `quote` or `splice`, and in all probability, they'll behave as expected.

Route 2 has the property that the definition of memory gets incidentally cumbersome. Suddenly there are a bunch more names I need to keep around outside of the scope of regular memory, and I need to make sure to mark each of them on every garbage collector pass. Woo. Not only that, but it suddenly gets really tricky to have a local variable named `quote`, and outright *dangerous* to have one named `unquote` or `splice`, because they'll share symbol identity with the built ins and behave radically differently in certain contexts.

### Part Three

I've been reading up on [Standard ML](http://sml-family.org/) lately. I think I have to learn it. If for no reason other than that the most vocal ML critics tend to be ML developers and designers, and that's something I have to respect. One of the papers linked from that SML page is entitled ["A Critique of Standard ML"](http://sml-family.org/papers/Appel-critique-SML.pdf), written by [Andrew Appel](https://en.wikipedia.org/wiki/Andrew_Appel), and goes over some of the positive and negative points with the Standard ML language.

Go read it right now. Seriously, it'll save you a lot of time in the long run. There are some points in the "good" that I probably won't get to, given what the target environment for this interpreter is, and there are a couple things he mentions that just outright aren't priorities or that I get to dodge by virtue of prefix notation and s-expression syntax, but a few of his bullets still stuck out to me. In particular


-   "Lack of macros"
-   "No pointer equality"
-   "Overloading" and relatedly "Polymorphic equality"


The first one reads

This is clearly an advantage, not a disadvantage. For the programmer to have to calculate a string-to-string rewrite of the program before any semantic analysis invites problems of the worst kind. Where macros are used to attain the effect of in-line expansion of functions, they are doing something that should be done by an optimizing compiler. Where macros are used to attain call-by-name, the effect can be obtained by passing a suspension as an argument; in ML this is written with the syntax `fn()=>` which though admittedly ugly is fairly concise, and is better than tolerating the semantic havoc wrought by macros.
> --Andrew Appel, [A Critique of Standard ML](http://sml-family.org/papers/Appel-critique-SML.pdf)

Some of this makes me believe that he must be talking about C-style macros, but the two desired effects are ones you are commonly said to want when reaching for Lisp macros. Not sure, and in any event, I'm unconvinced by the argument here. In addition to offering the effect of inlining and delayed functions, having macros in a language gives you the opportunity to write more of the language in itself than you would otherwise be able to. So I'm certainly keeping them, but will keep an ear out for further arguments of this kind<a name="note-Mon-Dec-01-211357EST-2014"></a>[|6|](#foot-Mon-Dec-01-211357EST-2014).

Anyhow, the other two points are more important to me. Both because I've thought about them before, and because I've actually got an opportunity to do something about them at relatively low effort.

The "No pointer equality" thing specifically refers to the lack of an `eq?` function, and that's ridiculously easy to implement. I just say it doesn't exist and we're done. It's hard to see where this would give me a *concrete* win, rather than just some admittedly interesting compiler optimizations that I could potentially do in the future, but the point is still convincing. Having only one kind of equality seems like it gets us something, and doesn't obviously cost us anything<a name="note-Mon-Dec-01-211400EST-2014"></a>[|7|](#foot-Mon-Dec-01-211400EST-2014). So I might just do that.

The other one is more complicated. And again, because of the scope of the interpreter I'm currently putting together, I might not get to go that far. You want certain operations to be type-aware. The most obvious cases being the arithmetic primitives, which you'd (perhaps somewhat unreasonably) expect to behave correctly for a wide range of different number types. Equality is another one; as Appel points out in this article, having a one-size-fits-all structural equality comparison is very probably not what you want. The example he points out is that "The Set 1, 2" and "The Set 2, 1" should compare equal to each other, but probably won't by naive structural comparison. The question is: how? I've seen a few approaches, the most interesting being a: generic functions and b: typeclasses<a name="note-Mon-Dec-01-211416EST-2014"></a>[|8|](#foot-Mon-Dec-01-211416EST-2014). I've thought very superficially about how to implement each, but haven't had a hard stare at the nitty-gritty.

That, possibly along with some comments about `quote`/`unquote`/`splice` implementation will have to wait for next time though. This piece is already quite a bit longer than I'd like.


* * *
##### Footnotes
1 - <a name="foot-Mon-Dec-01-211220EST-2014"></a>[|back|](#note-Mon-Dec-01-211220EST-2014) - And equivalent operations for non-stream structures you want to read from.

2 - <a name="foot-Mon-Dec-01-211224EST-2014"></a>[|back|](#note-Mon-Dec-01-211224EST-2014) - I can't say "all", both because I'm sure there's an experimental LISP out there that doesn't let you, *and* because if you read far enough into this article, you'll realize that I'm basically proposing to build such a LISP. Still, every Common Lisp and Scheme implementation, as well as AFAIK most of the mongrels have the same behavior.

3 - <a name="foot-Mon-Dec-01-211341EST-2014"></a>[|back|](#note-Mon-Dec-01-211341EST-2014) - The big exception, of course, being the `list` function. Which is the only way other than `quote` to get a sequence of literals together. If I'm going to lean on quoting *that* hard, then it damn well better be efficient, well supported in all contexts, and *very* well defined. Not sure how I feel about it. Just another thing to think through over the course of more hacking, looks like...

4 - <a name="foot-Mon-Dec-01-211345EST-2014"></a>[|back|](#note-Mon-Dec-01-211345EST-2014) - As long as you could work out some typing gotchas, and as long as you were ok with only doing it to functions that had at least one *non*-keyword argument. Not sure if there's a way to do without those restrictions. I'll think on it....

5 - <a name="foot-Mon-Dec-01-211352EST-2014"></a>[|back|](#note-Mon-Dec-01-211352EST-2014) - Because I need to intern on every comparison, and then traverse the full environment chain.
6 - <a name="foot-Mon-Dec-01-211357EST-2014"></a>[|back|](#note-Mon-Dec-01-211357EST-2014) - Especially since this isn't the first time I've seen someone open with "Macros are clearly a bad idea". Though I do wish they'd explain their reasoning at some point. As a Common Lisper, I'm not seeing a lot of the downside, but I *do* see damage done in other languages as a result of the absence of macros.

7 - <a name="foot-Mon-Dec-01-211400EST-2014"></a>[|back|](#note-Mon-Dec-01-211400EST-2014) - Since whatever structural equality operator can still do a pointer comparison before doing any kind of deep check.

8 - <a name="foot-Mon-Dec-01-211416EST-2014"></a>[|back|](#note-Mon-Dec-01-211416EST-2014) - As a disclaimer, I have *not yet* read up on Standard ML, though I understand its Functors and Modules also allow some of this.
