# langnostic
###### The personal blog of Inaimathi

- Currently written in [Haskell](https://www.haskell.org/) (formerly in [Go](http://golang.org/), formerly in [Common Lisp](https://common-lisp.net/)). Specifically, the [Spock](https://www.spock.li/) framework.
- Serves posts from the `.md` files found in [posts/](https://github.com/Inaimathi/langnostic/tree/master/posts/), so you could theoretically just read it from this repo (but it seems like it would be a pain in the ass to figure out which post is supposed to be the latest)
- All prose contained herein is licesned under [CC-BY-SA 3.0](http://creativecommons.org/licenses/by-sa/3.0/). The code constituting the site itself is releaed under [Expat](http://directory.fsf.org/wiki/License:Expat)

## Notes

- Deps: `Spock-0.10`, `pandoc-1.16`, `aeson-0.10`

## ToDo

- Re-factor project into something sane
	- PostMap and Post shouldn't be separate modules; just one named Posts
	- Don't worry; plenty mode. Add to this list as you go.

- `BGG Data Sifting`, `cl-notebook Thoughts` and `cl-notebook introductory thoughts` all have issues with the translation-from-markdown approach. Figure out what to do about this (also, `cl-notebook` obviously needs an export-to-markdown feature)

- Make OL styles consistent (you've got `1.   foo` in some places, and it should be just `1. foo`)
- Properly number the OLs. You've still got a few that just start with consecutive `1.`s
- Add title text to remaining images
- Fix up one or two more images (grep for `![](`, and check the ones that don't start with a `/`)
- Re-write some of the earlier articles that start with parentheses
- Convert to new footnote format (thank you, Pandoc)
