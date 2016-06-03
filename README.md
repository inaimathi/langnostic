# langnostic
###### The personal blog of Inaimathi

- Currently written in [Haskell](https://www.haskell.org/) (formerly in [Go](http://golang.org/), formerly in [Common Lisp](https://common-lisp.net/)). Specifically, the [Spock](https://www.spock.li/) framework.
- Serves posts from the `.md` files found in [posts/](https://github.com/Inaimathi/langnostic/tree/master/posts/), so you could theoretically just read it from this repo (but it seems like it would be a pain in the ass to figure out which post is supposed to be the latest)
- All prose contained herein is licesned under [CC-BY-SA 3.0](http://creativecommons.org/licenses/by-sa/3.0/). The code constituting the site itself is releaed under [Expat](http://directory.fsf.org/wiki/License:Expat)

## ToDo

- Add a `TipJar` section to the site.
	- Definitely links to services I use that provide network deals
		- www.digitalocean.com/?refcode=445ca16e1a76
		- http://invite.ritual.co/LEO4857
	- Link to [`quicklisp`](https://www.quicklisp.org/donations.html), [EFF](https://supporters.eff.org/donate) and [FSF](https://my.fsf.org/donate/) donation pages
	- Potentially a PayPal link, I guess, if nothing else works?

- Revisit old blogs. NOW AT: `id:75`
- Post 66 (passing-notes.md) has a missing image. I think that used to be a pic of skeptical Fry. See if you can grab it from the blogspot blog.

- Re-factor project into something sane
	- Write utility functions for some of the stuff rom `Server.hs`
	- Do some more processing before caching articles
	- Don't worry; plenty mode. Add to this list as you go.

- `BGG Data Sifting` is MIA. All the cl-notebook articles have issues with the translation-from-markdown approach. `cl-notebook` obviously needs an export-to-markdown feature.
