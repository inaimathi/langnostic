The votes are in, we've confirmed with a quorum-reaching meeting that in May we'll be starting a full read-through of [Paradigms of Artificial Intelligence Programming](https://github.com/norvig/paip-lisp), aka `PAPE`. I've read large chunks of the Norvig book in order to build some [prolog](http://www.swi-prolog.org/)-like [logic engine](https://github.com/inaimathi/fact-base)s, but I've never gone through the whole thing, and certainly not in a group setting.

This should be an interesting year or two, reading-wise.

If you're inclined to join us, either in Toronto or remotely, feel free to contact either [dann](https://github.com/dxnn) or [myself](http://github.com/inaimathi).

## Papers Update

The [`vellum`](https://github.com/CompSciCabal/vellum) project also proceeds apace. This week was mostly data-poking. As I mentioned; we're trying to build a model of all computer science papers in an effort to create a CS curriculum generator.

The next part of our process needs a bunch of test data, and I'm sure you can see where this is going based on the title of the post.

We collected a bunch of URLs to various [`arxiv.org`](https://arxiv.org/) Computer Science papers, then pointed [`aria2`](https://aria2.github.io/) at them using

```
aria2c -x 10 --user-agent="Mozilla/5.0 (X11; Fedora; Linux x86_64; rv:52.0) Gecko/20100101 Firefox/52.0" -i list.csv
```

This was probably not the best idea. Ideally, you'd write a long-running process to extract a bunch of data over a long period of time, kind of like I did with the [BGG API](https://github.com/inaimathi/all-boardgames-ever) a while back. This time, I got seriously ahead of myself and decided that multiple threads pulling papers was a good idea for some reason.

Our access got blocked once we hit about 300 papers downloaded, so we did still get our hands on enough test data to start poking around at things. We briefly considered alternative ways of getting a bunch more papers, including arxiv wardriving and the use of [Amazon Lambda](https://stackoverflow.com/questions/53203573/how-to-ensure-distinct-public-ip-per-aws-lambda), but ultimately figured that the most ethical approach was to just look into [contacting them](https://arxiv.org/help/contact) and seeing if they'd be willing to fill a terabyte drive or two with Computer Science writings. Although the other ideas we had seemed like they'd do pretty well at either [Defcon](https://www.defcon.org/) or [Sigbovik](http://sigbovik.org/) or possibly both.

I guess I'll let you know how it goes.
