So in preparation for that `ribbit`/`cl-fds` piece I did a little while ago, I also did a bunch of hacking on [Xach's](TODO) pretty decent [`quickproject`](TODO) library. I've been using it to create project skeletons for Common Lisp, but it's vaguely unsatisfying in a few ways I thought I'd fix up.

- A lot of things built-in instead of template based
- the README.txt file is built in and can't be removed, even if you add a project template with a README.md
- the default asd/lisp files are named after the project so generating them is very hard with templates right now
- there isn't an easily available template directory example anywhere

My edits resolve all of the above, and can be found [here](https://github.com/inaimathi/quickproject)

- go into detail about the specifics of the changes, and how each point is adressed. Show how to modify the templates
