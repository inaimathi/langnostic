# langnostic
###### The personal blog of Inaimathi

- Currently written in [Haskell](https://www.haskell.org/) (formerly in [Go](http://golang.org/), formerly in [Common Lisp](https://common-lisp.net/)). Specifically, the [Spock](https://www.spock.li/) framework.
- Serves posts from the `.md` files found in [posts/](https://github.com/Inaimathi/langnostic/tree/master/posts/), so you could theoretically just read it from this repo (but it seems like it would be a pain in the ass to figure out which post is supposed to be the latest)
- All prose contained herein is licesned under [CC-BY-SA 3.0](http://creativecommons.org/licenses/by-sa/3.0/). The code constituting the site itself is releaed under [Expat](http://directory.fsf.org/wiki/License:Expat)

## Notes

- Deps: `Spock-0.10`, `pandoc-1.16`, `aeson-0.10`

## ToDo

- Revisit old blogs. NOW AT: `id:37`
- Post #13 (On Neutrinos) is haunted. Have a closer look later

- Re-factor project into something sane
	- Write utility functions for some of the stuff rom `Server.hs`
	- Do some more processing before caching articles
	- Don't worry; plenty mode. Add to this list as you go.

- `BGG Data Sifting`, `cl-notebook Thoughts` and `cl-notebook introductory thoughts` all have issues with the translation-from-markdown approach. Figure out what to do about this (also, `cl-notebook` obviously needs an export-to-markdown feature)
