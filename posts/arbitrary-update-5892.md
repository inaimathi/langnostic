It feels like my blogging schedule is getting more and more erratic. I honestly can't remember the last time I had enough time and energy to devote a full hour or so to the task of recording my thoughts in prose. It's tempting to blame it on work[^honest], but something tells me that the combination of growing kids, dietary changes and exercise alterations, combined with some amount of mental burnout are also relevant factors. I'm not here to dwell on that though, I'm here to report highlights, and be on my way.

[^honest]: And, lets be honest here, [500px](http://www.500px.com) is easily the highest-pressure environment I've worked in so far. Not in the "the standards are too high" sense, but in the "how much shit can I get done in the absolute minimum amount of time" sense. Anyhow, it probably wouldn't even be inaccurate to say that work is affecting my mental states negatively.

## The Toronto Computer Science Reading Group meets Bob Harper

![](/static/img/rob-harper-0.jpg)
![](/static/img/rob-harper-1.jpg)

So the Toronto Comp-Sci Reading Group, aka The Toronto Society for Recreational Computer Science, aka [The Comp-Sci Cabal](http://compscicabal.github.io/) has been collectively reading a book called [the Practical Foundations of Computer Science](http://www.cs.cmu.edu/~rwh/pfpl.html). Which is fantastic incidentally, I thoroughly recommend it, but be prepared for the fight of your life if you don't already have an extensive math and/or CompSci background. The author of that book is [Bob Harper](https://existentialtype.wordpress.com/), who happened to be in town for the [Homotopy Type Theory Workshop](http://www.fields.utoronto.ca/activities/15-16/homotopy-type) this week. So we seized the chance to have a chat with him afterwards, both about some of the things he says in his book, some of the projects he's currently working on. The members who got their [physical copies](https://www.amazon.ca/Practical-Foundations-Programming-Languages-Robert/dp/1107150302/ref=sr_1_1?ie=UTF8&qid=1463792541&sr=8-1&keywords=practical+foundations+of+programming+languages) managed to get them autographed too.

[Dann](https://github.com/dxnn) talked to him about category theory for much of the night, which was a very interesting sounding conversation that I nevertheless understood nothing about. The main questions I had were about [Successor ML](https://github.com/SMLFamily/Successor-ML)[^specifically], and his statements about [non-strict languages](https://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/).

[^specifically]: Specifically, how I could help at this stage.

The answer to the first one was pretty short; "I'll have to get back to you on that". I'm aware that the preceding quote doesn't convey the tone, so I have to note that the subtext was "Things are still in relative upheaval, but we'll be needing devs soon". So, I'm going to keep watching [their repo](https://github.com/SMLFamily/Successor-ML), looking for a chance to either chime in or do some prototyping.

### What's Wrong With Non-Strict Languages?


> Him: My problem is that there are no natural numbers, or trees, or other useful datastructures in Haskell. There's only approximations

> Me: ... So I'm not sure I'm understanding this, the Numeric and Tree types in Haskell ...?

> Him: Right, they're not real numbers or trees. They all have bottom inhabiting them. That's the problem; in non-strict languages, bottom is a value. In strict languages, bottom is an effect.

The above is in quotes, but I have to admit I'm paraphrasing at this point; this is being writen a couple pretty high-intensity days after the events I'm describing. Anyhow, the bottom line is that numbers in languages that are non-strict by default admit an additional value that isn't present in strict languages; the infinite number. This introduces non-termination in various places that you wouldn't necessarily expect. To Bob, this is a deal breaker; it prevents certain widely used modes of reasoning, so he'd much rather have a strict-by-default language with some laziness thrown in where it's useful than go the other way round. I'm undecided, but at least think I understand the situation at this point.

## More History

I'm still plugging away at the article history of this blog, fixing typos, broken links and fucked formatting. Currently, the corrections are good up to [article 65](/posts/xmonad-keybindings-and-an-aside-on-piracy), which I just plain haven't had time to look at yet. That's still less than a third of the way through my archive. Have I mentioned that I write too fucking much? So it goes I guess. I did mention that I'd be doing more corrections when bored, so it should be no surprise that I did less of it while absolutely not bored.

## Docs Beta

As if I didn't have enough excitement plaguing me lately, I've also signed up for [docs-beta](http://docs-beta.stackexchange.com/) a little while ago, proposed [`common-lisp`](http://docs-beta.stackexchange.com/documentation/common-lisp/topics), [`SML`](http://docs-beta.stackexchange.com/documentation/sml) and [`nix`](http://docs-beta.stackexchange.com/documentation/nix) tags, and then did nothing much about it. The only one that's been approved so far is `common-lisp`[^approval-process]. And all that's been done on that one is a bunch of topic proposals that I keep telling myself I'll get to when I get a free weekend. Which, at this rate, should happen somewhere around late 2018.

I'll keep you posted, as always.

[^approval-process]: In order for a tag to be approved, someone has to propose it, and two other people have to commit to producing/requesting documentation. The result is that the current active tags list is much shorter than it might otherwise be. Which is both good and bad, depending on how you think about it.
