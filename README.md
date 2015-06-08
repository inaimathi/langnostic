# langnostic
###### The personal blog of Inaimathi

- Currently written in [Go](http://golang.org/)
- Serves posts from the .md files found in [posts/](https://github.com/Inaimathi/langnostic/tree/master/posts/), so you could theoretically just read it from this repo (but it seems like it would be a pain in the ass to figure out which post is supposed to be the latest)
- All prose contained herein is licesned under [CC-BY-SA 3.0](http://creativecommons.org/licenses/by-sa/3.0/). The code constituting the site itself is releaed under [Expat](http://directory.fsf.org/wiki/License:Expat)

## ToDo

- `BGG Data Sifting`, `cl-notebook Thoughts` and `cl-notebook introductory thoughts` all have issues with the translation-from-markdown approach. Figure out what to do about this (also, `cl-notebook` obviously needs an export-to-markdown feature)
- Make OL styles consistent (you've got `1.   foo` in some places, and it should be just `1. foo`)
- Properly number the OLs. You've still got a few that just start with consecutive `1.`s
- Add title text to remaining images
- Fix up one or two more images (grep for `![](`, and check the ones that don't start with a `/`)
