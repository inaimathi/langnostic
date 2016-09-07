# langnostic
###### The personal blog of Inaimathi

- Currently written in [Clojure](http://clojure.org/) (formerly in [Haskell](https://www.haskell.org/), formerly in [Go](http://golang.org/), formerly in [Common Lisp](https://common-lisp.net/)).
- Serves posts from the `.md` files found in [resources/posts/](https://github.com/Inaimathi/langnostic/tree/master/resources/posts/), so you could theoretically just read it from this repo (but it seems like it would be a pain in the ass to figure out which post is supposed to be the latest)
- All prose contained herein is licesned under [CC-BY-SA 3.0](http://creativecommons.org/licenses/by-sa/3.0/). The code constituting the site itself is releaed under [Expat](http://directory.fsf.org/wiki/License:Expat)

## ToDo

- Rsync doesn't seem to do the right thing regarding filesystem updates. Look into it.
    - Looks like copying via `rsync` doesn't trip the watcher. Copying via `scp` _does_, but does so even if the given file is unchanged from the previous version at the other end. This might just involve starting to use the `--deploy` option on `new.py`. and having it do the appropriate thing with the latest blog post.
	- A more automated approach is re-writing push.sh to use `rsync` to generate a list of changed posts, then use `scp` on them to trip that fs signal
- Cache static pages, instead of reading them each time

- Revisit old blogs. NOW AT: `id:93`

- `BGG Data Sifting` is MIA. All the cl-notebook articles have issues with the translation-from-markdown approach. `cl-notebook` obviously needs an export-to-markdown feature.
