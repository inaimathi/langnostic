I just noticed it's been a while. And not writing does me no favors; it means I've been doing a lot lately that I haven't properly reflected on. Which is par for the course, I suppose, except that I generally do a lot better than par in terms of programming and associated arcana. So bear with me while I put a few thoughts down.

## The Toronto Computer Science Reading Group

is a motley bunch of nerds I've already talked about once or twice. This past Friday, we started in on [PFPL, second edition](http://www.cs.cmu.edu/~rwh/plbook/2nded.pdf). Wiki's [here](https://github.com/CompSciCabal/SMRTYPRTY/wiki/PFPL-And-Related-Arcana), which is where we'll be putting any notes and errata we come across. It might also be a half-way-decent resource for *you* if you feel like reading along, but not necessarily blocking off most of your Friday night for the purposes of thinking about structural induction.

I'm more than a little scared of this one. Granted, [SICP](https://mitpress.mit.edu/sicp/) was also a serious endeavor, but I already had some years of experience with various Lisps at that point. So, while it wasn't exactly an *easy* lift, I knew my metaphorical shoulders wouldn't give out in the attempt. There's no such comfort here. I fully expect to be put through the wringer every week of the following year, and hopefully emerge with a new perspective on the world. It'll be worth it (I consider perspectives important, you'll recall), but it still makes me pretty nervous to consider the details.

## Flexible Networks, and Their Construction

We haven't really been making as much progress on [this](https://github.com/Inaimathi/cl-ring) as we could have. Mostly because I'm tied up with four or five different projects at the moment, and it isn't really the kind of problem you can easily attack in bite-sized chunklets. I'm *hoping* to get some down-time over the next little while to throw at the codebase, and similarly hoping that part one of the effort yields something good enough to bootstrap the rest of the project.

## `cl-handlers`

Ever since writing [`house`](https://github.com/Inaimathi/house), I've had on-and-off thoughts about generalizing the [handler structure](https://github.com/Inaimathi/house/blob/master/define-handler.lisp). In particular, the type annotations on incoming requests save me enough time, thought and effort in general that I've often found myself using `house` instead of otherwise far superior web servers. This is pretty close to the only interesting part of `house`, as far as I'm concerned, so being able to use the same approach with a more thoroughly performant and debugged web server sounds like an unambiguous win.

And that's pretty much what [`cl-handlers`](https://github.com/Inaimathi/cl-handlers) is.

The handler-and-type-definition mini-language ripped bleeding from `house`, and implemented in a way compatible with [`woo`](https://github.com/fukamachi/woo) and [`clack`](https://github.com/fukamachi/clack). That way, I could theoretically write a CL web app once, and *then* decide whether to run it on top of [Hunchentoot](http://weitz.de/hunchentoot/), [Wookie](http://wookie.lyonbros.com/), [Woo](https://github.com/fukamachi/woo), [House](https://github.com/Inaimathi/house), or some other server that comes out and revolutionizes everything again.

It's not *quite* finished, because I still need to put together the session mechanism, and figure out how to deal with event streams. There might be a surprise or two up my sleeve as well, but I'll say no more about that here.

## Actual Work

I'm still mulling over a front-end blog post. The technologies are things I could take or leave. I've already commented about how [Go](http://golang.org/) just plain [rubs me the wrong way](http://langnostic.inaimathi.ca/posts/arbitrary-update-4701#the-go-problem), and [Rails](http://rubyonrails.org/) does more or less the same thing. My problem with both of them boils down to encouraging superstition. They take a particular view of some corner of computer science, declare a Correct™© way to do it, then try their hardest to browbeat users into believing that their way represents the absolute limits of what can be done. [Ruby](http://ruby-lang.org/) in general is a fairly comfortable language to work in, but the best thing I'm willing to say about Rails is that it occasionally fails to get in my way. As a rule, that's not something I'm going to use unless someone is willing to pay me for the effort.

Apart from the underlying frameworks, we also use a few syntax transformers. In particular, [Coffeescript](http://coffeescript.org/) for our JS, [SASS](http://sass-lang.com/) for our CSS and [haml](http://haml.info/) for our HTML markup. Not sure how I feel about any of them. SASS in particular seems a bit superfluous, and mildly annoying to edit in some situations. I've already [briefly talked about Coffeescript](http://langnostic.inaimathi.ca/posts/coffee). Bottom line, I consider it a syntactic improvement over JS.

The team and culture is a significantly new situation for me. For starters, and I'm pretty sure I've mentioned this elsewhere, we do actual code reviews and actual automated testing. Every other team I've been part of has had me as an outlier, asking for colleagues to actually read my code before I merge into `master`, and writing some unit tests on mission-critical components.

Here, both of those tendencies are actively encouraged, and I fucking love it. It might not be my number one choice of language, but it feels like feedback has helped me improve much faster than its absence.
