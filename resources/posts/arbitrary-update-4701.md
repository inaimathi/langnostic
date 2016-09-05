So I've officially been away from the keyboard long enough that I need to get some thoughts out. And I kind of want to avoid this blog becoming nothing but hasty [lists of interesting papers](https://github.com/CompSciCabal/SMRTYPRTY/wiki/What%27s-next%3F), accompanied by some out-of-context and ever-growing list of ToDo items. So, in no particular order...

## New Blog Stuff

Like I mentioned [last time](/posts/blog-changes), this weekend I finally got around to making some much needed infrastructure changes around here. Really, I've been wanting to do this since it became obvious what a massive maintainability headache it is to use [fact-bases](https://github.com/Inaimathi/fact-base) for general storage. I *might* expand on that at some point. Short version is that it has a few very ugly drawbacks, not very many advantages, and [the most interesting of its advantages](https://github.com/Inaimathi/clj-history) is not intrinsic to `fact-base`s.

It's only recently that I finally got both enough time and cognition to sit down and write the damn thing again properly. It actually started as a [Haskell project](https://github.com/Inaimathi/langnostic/tree/haskell). I had some second thoughts when it became clear that [`blaze`](https://hackage.haskell.org/package/blaze-html), the HTML-templating library, doesn't support general `XML` tags, and therefore wouldn't help me generate my [`atom` feed](/feed/atom). That ended up getting resolved [relatively satisfactorily](https://github.com/Inaimathi/langnostic/blob/haskell/Markup.hs) though. The real deal-breaker happened when I actually went to deploy the final product. This involved installing `nix` on my server, and trying to `nix-env -i haskell-scotty-ghc7.8.4`, only to realize that the package no longer exists. I could have spent more time researching what I'm actually supposed to do about module management these days, but as mentioned [a little while ago](/posts/i-liiiiive), my patience for the Haskell ecosystem is wearing pretty fucking thin. This is the main reason I've never tried to do any professional work with it, despite the fact that I really like the language itself. So I ended up [re-writing the whole thing again in `go`](https://github.com/Inaimathi/langnostic/tree/golang); a *language* I like much less, but which takes deployment and inter-operability issues much more seriously (More on that [later](#the-go-problem)).

That's something I've been meaning to do ever since seeing [these benchmarks](https://github.com/fukamachi/woo#benchmarks) on the `woo` server github, incidentally. And, as expected, `go` presented more chafing restrictions than `ghc`, but ultimately resulted in an artifact that could be built on an arbitrary server with a minimum of fuss. It also runs pretty well. The last bit I wanted to write after the official switch-over yesterday was a minimal caching system for my posts, given that they're now stored in `.md` files on the disk of my server. We won't be doing the pseudo-literate programming thing with the entire [`langnostic` repo](https://github.com/Inaimathi/langnostic), but indulge me in taking a close-up view of that piece.

Given that I've got

1. files on disk
2. that I'll want to process in a consistent way
3. and that might occasionally change

What I *really* want to do is cache the processing step and keep it around until a change occurs. The change tracking can be pretty naive. We won't be doing anything like calling `sha512sum` here, a mere check of the files' modified time will do. The procedure I've got doing `markdown` processing is called, appropriately enough `ProcessMarkdown`. So, what I'd like it to do when called is really

- Check if the file has a "recent" cached version associated. If so, just return that copy.
- If not, check if the target file has been modified since we last hit disk. If not, return the cached copy and record the latest disk check.
- If there *has* been a change, or if there isn't a cached copy, read the file from disk, process it, and save the result in its cache.

Here's how I did it.

```go
type Cached struct {
	contents []byte
	lastChecked time.Time
	lastEdited time.Time
}

var mdCache = make(map[string]Cached)

func ProcessMarkdown (mdFile string) ([]byte, error) {
	cache, present := mdCache[mdFile]
	if present && (time.Minute > time.Since(cache.lastChecked)) {
		return cache.contents, nil
	}

	stat, err := os.Stat(mdFile)
	if err != nil { return nil, err }
	if present && (cache.lastEdited == stat.ModTime()) {
		mdCache[mdFile] = Cached{cache.contents, time.Now(), cache.lastEdited}
		return cache.contents, nil
	} else {
		f, err := ioutil.ReadFile(mdFile)
		if err != nil { return nil, err }
		unsafe := blackfriday.MarkdownCommon([]byte(f))
		mdCache[mdFile] = Cached{bluemonday.UGCPolicy().SanitizeBytes(unsafe), time.Now(), stat.ModTime()}
		return mdCache[mdFile].contents, nil
	}
}
```

We've got a `Cached` type that keeps


- `contents`; a `byte` slice,
- `lastChecked`; a timestamp representing the last time we went checked the file on disk
- `lastEdited`; a timestamp representing the files' modified time at our previous check

And we've got a global `map` of `string` (file name) to `Cached`. Ideally, I'd have hidden this variable in the local scope of `ProcessMarkdown` rather than leaving it lying around, but making it an un-exported variable is close enough for my purposes. `ProcessMarkdown` itself then runs down the options as described above. When we get a request to process a particular `mdFile`, we first checks whether a recent *(younger than one minute, for our purposes)* cache exists and if so, return it immediately. We then call `Stat` on the given `mdFile`. If there is a `Cached` entry, but it isn't recent, we check the files' `ModTime`. If it hasn't change since we last looked, we return the cached version again. Finally, if all else fails, we read the file from disk, process it using [`blackfriday`](https://github.com/russross/blackfriday) and [`bluemonday`](https://github.com/microcosm-cc/bluemonday), and cache the processed result before returning it.

That does a reasonable job of minimizing disk trips while still freeing me of the need to periodically restart the server. I suppose I *could* increase the amount of time a cached file remains fresh to a something on the order of a half-hour or so, but going to disk once a minute per resource doesn't sound *too* bad. I reserve the right to change my mind.

## The Go Problem

You can read my first impressions at the end of [this piece](/posts/golang-wiki). I've hit a few road-bumps since, mostly dealing with modularity and re-usability issues, but there also seems to be a pretty big attitude problem in the `go` community. The modularity issues stem from `go`s' monomorphic type system, which ensures that while the language designers can define transparently polymorphic procedures like `len`, **you** definitely can't. The most annoying place I saw this is while wondering how to go about doing [`sortBy`](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-List.html#v:sortBy) in `go`. Short story, [it ain't pretty](https://gobyexample.com/sorting-by-functions). You need to define your own custom slice type, then define `Len`, `Swap` and `Less` methods on it, then call `sort.Sort` on your original list after coercing it to your custom type.

That's... I mean... I *guess* it could be worse, but not by much.

The same situation ensures that you can't *really* write a general purpose memoization function along the lines of my file-caching trick above, since the type of your `map` would vary by use case. My gut tells me this is going to lead to *a lot* of unnecessary boilerplate getting written if I keep writing `go`. The only apparent recourse is [giving up performance along with either or both of run-time and compile-time type safety](http://blog.burntsushi.net/type-parametric-functions-golang/).

Which brings me to the attitude problem I mentioned. Apparently, `go` developers are expected to develop a boilerplate fetish. That's the only conclusion I can come to when I read things like ["It's simpler to write a `for` loop than iterating over a range"](http://stackoverflow.com/questions/21950244/is-there-a-way-to-iterate-over-a-range-of-integers-in-golang), or ["Reversing an array manually with a `for`-loop is simple enough that you should just write the four lines out each time"](http://stackoverflow.com/a/19239850/190887), or ["That `sort.Sort` boilerplate is no big deal, why would you ever want something simpler?"](http://grokbase.com/t/gg/golang-nuts/132d2rt3hh/go-nuts-how-to-sort-an-array-of-struct-by-field), or, god help me, ["If you want to call `math.Min` on integers rather than floats, you should just define your own `int` version each time"](https://groups.google.com/forum/#!searchin/golang-nuts/min$20max/golang-nuts/dbyqx_LGUxM/tLFFSXSfOdQJ).

![What the fuck am I looking at here?](/static/img/what-the-fuck-am-i-reading.jpg)

Reading this kind of garbage is supremely depressing. It got to the point where I was searching for whatever this language calls `downcase` and half-expected to find a discussion wherein members of the community expressed surprise that anyone would ever need to work with strings. I didn't, and it's called [`ToLower`](http://golang.org/pkg/strings/#ToLower), but it was a real concern for a minute or so there.

I understand that a statically, monomorphically typed language is going to have some obvious limitations, but why the ever-loving fuck shouldn't I point them out? And why, in the year 2015, should I be content defining my own `min` and `max` for integers and [paradoxically not `float`s](http://golang.org/pkg/math/#Max)? The [above 51-message, 27-contributor extraveganza](https://groups.google.com/forum/#!searchin/golang-nuts/min$20max/golang-nuts/dbyqx_LGUxM/tLFFSXSfOdQJ) has me seriously considering re-writing the blog *again* in [ML](http://sml-family.org/) or [Rust](http://www.rust-lang.org/). Except that I still haven't found a clear specification regarding how I'm supposed to set up my Standard ML modules in a cross-implementation fashion, and [`cargo`](http://doc.crates.io/guide.html) looks a good deal more intimidating than `go get`.

Sigh.

## New Places

Last bit, since I'm about to lose coherence; I recently started a new job. Instead of doing some odd R&D work inside of [Moneris](http://www.moneris.com/), I'll now be slinging Ruby and CoffeeScript (along with a few others of my choice) over at [500px](https://500px.com). Or rather, I've already been doing that for the past couple of weeks. It's honestly weird working in OS X again after all this time. Not least because I've since developed a taste for tiling window managers, and XMonad has no satisfying analogue over in Apple-world. But also because I'm still getting comfortable with the built-in keyboard shortcuts. I've finally had to buckle down and start using alternate `execute-extended-command` keybindings *(since the meta key is so hard to hit on the default Apple keyboards)*, and made use of [`auto-dim-other-buffers`](https://github.com/mina86/auto-dim-other-buffers.el) *(since our external monitors are so ginormous)*. I'm sure I'll need to make more changes in my workflow before too long.

The other big contrast is being immersed in the small-company mentality of ["fuck it, ship it"](http://lifehacker.com/5934647/fuck-it-ship-it) again. I say "again", because the last time I was in the start-up situation, I hadn't yet worked with the frindly monoliths of the finance and medical industries. So this time, I'm much more likely to enjoy it. The place looks about as awesome as you'd think based on [the photos you see](http://www.blogto.com/tech/2015/04/inside_the_offices_of_photo_community_site_500px/), though the bean-bags aren't as comfortable as they look for hacking purposes. I'll keep you posted about how it goes, and may blog about their development setup before too long.
