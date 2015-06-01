Just a short update this time, involving things I keep stubbing my toe on in Lisp and Erlang.

### <a name="common-lisp-is-not-object-oriented"></a>Common Lisp is not Object Oriented

The object orientation support is bugging me again. Not just me, either<a name="note-Mon-Aug-06-013854EDT-2012"></a>[|1|](#foot-Mon-Aug-06-013854EDT-2012), because a bunch of modules I've been making use of lately have functions with names like `time-difference` or `queue-push`, which is precisely what the generic functions are supposed to save you from doing. It recently annoyed the fuck out of me while putting together a simple, caching [implementation of a thread-safe queue](https://github.com/Inaimathi/Common-Lisp-Actors/blob/master/queue.lisp). I wanted that construct to have `push`, `pop` and `length`, but because those names already designate top-level **functions**, it's not quite as simple as declaring them.

I'm not about to be dumb enough to propose that this makes Common Lisp an [unacceptable language](http://steve-yegge.blogspot.ca/2006/04/lisp-is-not-acceptable-lisp.html), especially since it looks like this could easily be fixed within the spec as it exists today, and I already [quasi-proposed a semi-solution](http://langnostic.blogspot.ca/2011/11/objective-lisp.html). I just have to give voice to that minor frustration, and point out that what you'd really want in this situation is access to a lot of the basic [CLHS symbols](http://www.lispworks.com/documentation/HyperSpec/Front/X_AllSym.htm) **as methods** rather than functions. Not having this has now bitten me directly in the ass no less than twice<a name="note-Mon-Aug-06-014123EDT-2012"></a>[|2|](#foot-Mon-Aug-06-014123EDT-2012), and signs that it might be worth fixing are showing in various CL libraries.

### <a name="erlang-should-be-more-like-javascript"></a>Erlang Should Be More Like JavaScript

Wow, do [records](http://www.erlang.org/doc/reference_manual/records.html) suck donkey dong!

Ok, to be fair, they're better than having to deal with plain [tuples](http://www.erlang.org/doc/reference_manual/data_types.html#id64180) when you're working with large constructs, and they're arguably The Right Way to deal with database storage, but they're a fundamentally annoying and hacky way of implementing key/value pairs.

The problem is record sharing. Here's a thought exercise: what happens when you have a system that deals with the storage and manipulation of sets of comments<a name="note-Mon-Aug-06-014948EDT-2012"></a>[|3|](#foot-Mon-Aug-06-014948EDT-2012), and a second, completely separate system which would like to consume the output of that first one in order to display these sets in interesting ways for human consumption?

If you had a **real** k/v construct built in, like what [every](http://docs.python.org/tutorial/datastructures.html#dictionaries) [other](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node108.html) [goddamn](http://hackage.haskell.org/packages/archive/base/4.5.1.0/doc/html/Data-HashTable.html) [language](http://www.json.org/js.html) on this earth seems to have, what you would do is pass an instance of that construct across.

If the hash map was a fundamental data type in Erlang, you would have no problem in this situation.

But.

Records are basically tuples, wearing a bunch of reader macros and syntactic sugar. That means they're potentially faster than using a dynamic data structure for the same purpose, but it means that you can't just *pass a record* between two otherwise decoupled systems. If you want the same sort of behavior that you'd get out of native k/v support, you have three options I can see, and they all make me want to glare menacingly at Joe Armstrong, or at least whoever decided that records were a satisfactory solution.

## <a name="option-duplicate-records"></a>Option 1: Duplicate Records

You declare the same record in both systems, then send records across.

This sucks balls because changing the record suddenly requires you to change and recompile both projects. They are not really decoupled anymore. In our theoretical example above, say we've decided that we'd really like to start tracking `comment`s hierarchically. We need to add a pair of new fields, `root` and `parent` so that each comment can tell you which tree its part of and where in that tree it is.

```erlang
-record(comment, {id, user, thread, root, parent, timestamp, title, body, status}).
```

Now, we can't just make this change in the model component, because if you had different record declarations in the model than the view, you'd get compiler errors. If you have multiple views trying to make use of the same model, and not all of them need the new data<a name="note-Mon-Aug-06-015214EDT-2012"></a>[|4|](#foot-Mon-Aug-06-015214EDT-2012), *too fucking bad*, you're changing them all over anyway. This isn't even the worst case scenario, by the way. If you decide that the record shouldn't change fundamentally, but that you merely need to *reorder* fields, you won't even get a compiler error if you forget to change records in both places.

This is not the sort of brittleness that I expect from a key/value construct.

## <a name="option-shared-records"></a>Option 2: Shared Records

You can write one file, lets say `records.hrl`, put all your record declarations in there and then include that file in both projects.

*This* sucks balls because now you don't actually have two decoupled projects at all. You've got one giant, mostly disjoint project with shared data declarations. It's not *horrible*, to be fair, but remember that having a run-time construct rather than a compile-time record system wouldn't even require this much additional planning.

## <a name="option-sending-tuples-or-proplists"></a>Option 3: Sending Tuples or Proplists

This is the option I went with for a recent project, and I'm honestly not sure it was the right approach, but there would have been record name collisions otherwise, so whatever, I guess.

Instead of sending records between components directly, you emit a tuple from the model and consume it in the view, potentially creating an intermediate record if you need to. This has pretty much all the downsides of Option 1, except that you don't have a single `record` name-space to deal with. If you take the [Proplist approach](http://www.erlang.org/doc/man/proplists.html), it gets very slightly better because you only need to put together the one abstraction layer to do look-ups, and if you make it complete enough, you don't need to change it whenever you change the record definitions. That's still a *lot* more annoying than just having this force pre-resolved.

I remember writing up [notes from a talk Joe gave about Erlang](http://langnostic.blogspot.ca/2012/04/notes-from-borders-of-erlang.html). One of the points he covers under the "Missing Things" heading was **Hash Maps**, wherein he pointed out this specific issue with the fundamental architecture of the language. In the notes, I sort of acknowledge that he has a point, but don't linger on it too long. Honestly, I was thinking that it wouldn't bite at all, let alone as hard as it actually has. Joe, if you're reading this, you were right. And for the love of god, if you've got a solution in mind, **DO IT**.

`lists:keyfind/3` and workarounds like [this](http://stackoverflow.com/a/10833019/190887) aren't nearly as satisfying as just having an actual, dynamic key/value construct built into the language from the ground up.

* * *
##### Footnotes

1 - <a name="foot-Mon-Aug-06-013854EDT-2012"></a>[|back|](#note-Mon-Aug-06-013854EDT-2012) - Though I may be the only one who's noticing enough to bitch about it.

2 - <a name="foot-Mon-Aug-06-014123EDT-2012"></a>[|back|](#note-Mon-Aug-06-014123EDT-2012) - That I've noticed.

3 - <a name="foot-Mon-Aug-06-014948EDT-2012"></a>[|back|](#note-Mon-Aug-06-014948EDT-2012) - `-record(comment, {id, user, thread, timestamp, title, body, status}).`

4 - <a name="foot-Mon-Aug-06-015214EDT-2012"></a>[|back|](#note-Mon-Aug-06-015214EDT-2012) - For instance, if there are places that you're displaying the same set of comments, but don't really care about their hierarchy.
