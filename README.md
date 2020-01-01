# langnostic
###### The personal blog of Inaimathi

- Currently written in [Clojure](http://clojure.org/) (formerly in [Haskell](https://www.haskell.org/), formerly in [Go](http://golang.org/), formerly in [Common Lisp](https://common-lisp.net/)).
- Serves posts from the `.md` files found in [resources/posts/](https://github.com/Inaimathi/langnostic/tree/master/resources/posts/), so you could theoretically just read it from this repo (but it seems like it would be a pain in the ass to figure out which post is supposed to be the latest)
- All prose contained herein is licesned under [CC-BY-SA 3.0](http://creativecommons.org/licenses/by-sa/3.0/). The code constituting the site itself is releaed under [Expat](http://directory.fsf.org/wiki/License:Expat)

## ToDo

- Thread the user object through all the pages
	- Make the template look purdy (add stuff to the nav line instead of doing silly things with H1s)
- Make logout and authenticate pages redirect at the end of their lifecycle instead of sticking to a page where refreshing hits an error
- Add a Patreon link to the tipjar

- Fixed FS signal problem. The reload of posts.json seems to hang at around 111 on the live system (not on the dev system, oddly). I suspect memory issues. See what you can do about it, but don't stress; this is going away once you move post metadata into the post markup itself.
- Cache static pages, instead of reading them each time

- Revisit old blogs. NOW AT: `id:93`

- `BGG Data Sifting` is MIA. All the cl-notebook articles have issues with the translation-from-markdown approach. `cl-notebook` obviously needs an export-to-markdown feature.
