This has been the first vacation in a very, _very_ long while that I ended up _actually taking a break_. And man, did I fucking need that. It's not quite over yet, but almost. We're on Saturday of the trailing weekend of my week off. If you've been paying attention to my output, you'd realize how fragmented my thinking time has been lately. Again, mostly a result of [work](https://www.500px.com). That's sort of to be expected from a startup environment, but my off time hasn't been this sparse since I was back at [my first company](/posts/some-free-time), and I'm beginning to wonder if I should be several times louder about it than I currently am. We'll see how my first week back goes, I suppose.

During my downtime, I've _literally_ been eating, sleeping, working out, and spending time with the family. Which is doing me some serious good, if my own subjective experience counts for anything. That's a thing you occasionally need. The extent of my programming-related activity has been

1. Doing a bit of light scripting in OCaml
2. Cleaning up my blog archives
3. Making a slight tweak to [`kicktracker`](https://github.com/Inaimathi/kicktracker)
4. _Very_ leisurely going through the Udacity machine learning course

## Kicktracker and Machine Learning

[`kicktracker`](http://kicktracker.inaimathi.com/) feeds now include project thumbnail images, as well as blurbs. This makes it mildly easier for humans to scan through feeds, and sift out the occasional nugget of gold from among the refuse.

And that not-so-subtle point is exactly where machine learning comes in for my purposes. The probelm is, even with category and search terms being as restricted as they are on [my preferred feed](http://kicktracker.inaimathi.com/board-games), the signal-to-noise ratio is unacceptably low. What I'd really like, I think, is a recommendation engine. Which means taking one or more machine learning techniques, and applying them to the problems of

1. Finding projects likely to be funded
2. Finding projects likely to deliver once funded
3. Finding projects that fit both of the above _and_ that I would like based on my backing history.

That doesn't sound particularly hard, but I'm not entirely sure where to start. Which is where [the course](https://www.udacity.com/course/intro-to-machine-learning--ud120) comes in. The first section is entitled "Naive Bayes", which seems to be a classic linear separator approach to supervised learning. Let me unpack that, for all our benefit.

- Supervised Learning means we start with some labelled corpus. We've got a mountain of data, and we know what a desired example looks like. Whatever learning algo we choose will have to organize those examples, and figure out which features are relevant to success, but it won't have to define or discover success for itself.
- The Linear Separator approach means extracting some features from a bunch of labelled examples, organizing them in a scatterplot, and seeing if there's some line we can draw that maximally separates all the good examples from the bad ones. At that point, we can judge new incoming examples by extracting the same features, plotting them, and seeing which side of the line they fall on.

Goals 1 and 2 above seem like perfect fits for this approach. We've got a corpus of all existing Kickstarter projects available from their archives, we can tell whether they were successfully funded or not, and we can very probably pull out some user data to figure out whether most of the backers have received their rewards.

The last part is the only one I'm a bit iffy about, and it's really only because I've got such a small pool of applicable examples. I've backed about 20 games at this point, so the resulting recommendations are probably going to be very narrowly scoped. Which on reflection might not be a bad thing, given that my problem is way too much noise for too little signal. I'll let you know how all this goes.

## Scripting in OCaml

It honestly isn't enough code that I'd normally mention it, but there's been little enough that this stands out. It's another block of code for my [`shell-ui`](https://github.com/Inaimathi/shell-ui) project [^just-in-case].

[^just-in-case]: Just in case you're hearing about this for the first time, a while ago I got sick enough of typing `tar -jvzomgwtfbbq` and decided to do something about it. So I wrote a few little scripts that take more sensible, human-oriented input and do the appropriate arcane garbage behind the scenes. I'm happier with it, and don't have to worry about changing the original interface for worry of incompatibilities, so I consider it a win even if I'm forever the only user.

```ocaml
(* MTPPut.ml *)

let tail = function
  | [] -> [];
  | (a::rest) -> rest
;;

let copy_file fname =
  Printf.printf "Copying \"%s\"...\n" fname;
  Sys.command (Printf.sprintf "mtp-sendfile \"%s\" \"%s\"" fname fname)
;;

let arg_list = Array.to_list Sys.argv
    in List.map copy_file (tail arg_list)
;;
```

Recently, I got a new Android device, you see. And they no longer just `mount` up like regular external volumes. Now you've got to deal with the [`MTP` protocol](https://en.wikipedia.org/wiki/Media_Transfer_Protocol) in order to get files on or off of the device. there's a [suite of command-line tools](https://wiki.debian.org/mtp#Commandline) in Debian that make this possible, though not exactly easy. `mtp-sendfile` is the one that you'd use to put a file onto such a device. The main beef I've got with it is that I need to enter the name of the target file twice[^once-to], and I don't want to have to say it manually.

[^once-to]: Once to specify the local file, and again to specify what its name on the Android device will be. Every time I've done this so far, I've wanted the two to match. So there's an obvious way to simplify the situation.

This is about four lines longer than I was expecting it to be, mainly because I had to write `tail` myself. I'm ... honestly not sure why. Presumably I need to include the correct library to get this resolved for me, but don't know what "the correct library" is here. I was sort of expecting `List.tail` to be a thing, but it isn't[^and-neither-is] if I trust my `ocaml` shell

[^and-neither-is]: And neither is `List.rest` or `List.cdr`, before you ask.

```ocaml
# List.tail ;;
Characters 0-9:
  List.tail ;;
  ^^^^^^^^^
Error: Unbound value List.tail
#
```

Ho hum. The other bad thing is that I'm using `Printf.sprintf` to put together the argument to `Sys.command`, rather than using something more appropriately high-level and path-respecting. I'm assuming that the [`Shell`](https://ocaml.janestreet.com/ocaml-core/108.07.01/doc/core_extended/Shell.html) library will give me all the usual auto-escaping goodness I'm used to from almost every other language I use.

At this small a scale, I can't honestly tell whether this is any kind of improvement over [`python`](https://www.python.org/) or [`ruby`](https://www.ruby-lang.org/) for the same purposes. The resulting binaries are much smaller than equivalents built over in [Haskell](https://www.haskell.org/)[^actual-numbers], but that's about it.

[^actual-numbers]: ~125k for the OCaml program, and ~1.5m for the same thing in Haskell. Haskell's optimization flags have no effect on this. That isn't huge by the way, but is one of the few notable differences. Both languages are about as expressive, though Haskell has easier flexibility thanks to its typeclass system, and generally has functions where I expect them to be, both have a similarly useful type system, and both seem to have equally fast compilers for my purposes. The differences are:

    1. Haskell is non-strict by default, where OCaml is strict by default (used to think this was a win for Haskell; now have no opinion)
    2. Haskell has significant whitespace (advantage: OCaml)
	3. OCaml lacks typeclasses out of the box (advantage: Haskell)
	4. Haskell binaries tend to be larger (advantage: OCaml)
	5. Minor syntactic differences, such as different comment structure, different list delimiters, different syntactic approaches to arrays and different `end-of-block` delimiters. (advantage: Haskell, but barely. This isn't that huge a difference)

Since I kinda want to do something serious with OCaml or ML, I'm pretty sure I'll try to reproduce the rest of the [Python script suite](https://github.com/Inaimathi/shell-ui/tree/master/python) in one or both, possibly along with Haskell, and report back regarding how it went.

## Cleaning Up The Archives

I've been running through my archives, making corrections and updating the old postst to [MultiMarkdown footnote syntax](https://github.com/fletcher/MultiMarkdown/wiki/MultiMarkdown-Syntax-Guide#footnotes), and it tripped off the need for some light meditation. This is a subject I've [touched on in the past](/posts/autopair-paredit-burnout#bitching), and it hit me again at full force. These days, there is no such language as "the one I enjoy using". There are bits and pieces that I absolutely want from Common Lisp, Scheme, Clojure, Haskell, ML, and Go but there is now no such thing as a language that combines most of them. There _can't ever_ be a language that combines _all_ of them, because some contradict each other, or at least work at cross-purposes.

This at once makes me feel sad and liberated. Sad, because why the fuck would I want to settle for less than the best possible language? Liberated because, if I'm going to be stuck with garbage languages _anyway_, I may as well start looking at cross-language techniques and approaches[^admittedly].

[^admittedly]: Admittedly, while stealthily working on [a prototype](/posts/objective-lisp) or [two](https://github.com/Inaimathi/experimentalisp) or [three](https://github.com/Inaimathi/cl-wolf) for something better, but hey, what exactly do you expect?
