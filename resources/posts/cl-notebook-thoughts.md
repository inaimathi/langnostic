No nuts and bolts this time. Here's a random collection of insights I've had while trying to put together the version 0.1 of a notebook style editor:

## Being async Pays

My initial assumption was that evaluation would be an inline thing. That is, you send out a `POST` request, that code is evaluated synchronously, saved out to your fact base, and the HTTP response consists of that evaluated result which you can then display prettily on the front-end. Turns out that's a big fat no. Even discounting any issues with the Common Lisp return model, and the eventual attempt at making this a multi-user editor, the synchronous approach doesn't quite work. Which I found out the first time I accidentally wrote a `loop` without the required `while`. Something like

```lisp
(loop for next = (pop stack) do (something-to-next))
```

The trouble should be obvious. That's the sort of computation that you want off in its own isolated thread. And you want to be able to kill that thread if it turns out to be as boneheaded a mistake as that was, which means that you have to notify the front-end of computations in progress, go off and perform it, then notify the front-end again when you're done. And that's not something you can do synchronously.

In the end, I end up spawning a single tracked thread to do the work of evaluation. I notify the front-ends when it begins work, and then again when it completes. At any point in between, a front-end can send a termination signal to kill that thread rather than waiting it out to completion. You can see the implementation [here](https://github.com/Inaimathi/cl-notebook/blob/master/cl-notebook.lisp#L51-L70), [here](https://github.com/Inaimathi/cl-notebook/blob/master/cl-notebook.lisp#L162-L163), and [here](https://github.com/Inaimathi/cl-notebook/blob/master/cl-notebook.lisp#L126-L131). This will entirely coincidentally make it much easier to extend `cl-notebook` to a multi-user editor later.

## Being surgical pays. Sometimes. Not in the way you'd think.

The first version of this editor basically spat out the complete current notebook state at the front-end on each action, and the front-end re-drew the entire thing on every change. You'd think that the sheer inefficiency of this approach would get to me, but you'd be wrong. It was actually fast enough that had performance been the only factor, I'd have left it at that. The problem with that approach is that you end up clobbering giant chunks of client-side state each time. In particular, re-drawing every cell (and hence, re-initializing each of their `CodeMirror` instances) meant that I was blowing away undo information for each editor window every time anything happened. And that's annoying as fuck. I'm not sure anyone's formalized the point into a commandment yet, but you really should act as though they have: Thou Shalt Not Mangle Thy Clients' Data. That applies no matter how temporary the data is. Even if your job is to take said data and transform it in some way, say for instance by evaluating it, you should do so with a copy instead of a destructive change. And even when you absolutely must be destructive, be as selectively destructive as you possibly can.

## Thinking About Space Doesn't Pay. No, not even that much.

I almost left this one out entirely because it seemed so self-evident, but on reflection, it's something I've had to learn too. Space doesn't even begin to matter for systems like this. I'm talking both about memory and about disk space. Yes, there are some applications for which this is not the case, but it's true as a rule.

The `fact-base` project in particular has been sticking in my craw in this sense. I kept thinking things like 'Holy shit, I'm keeping history for every cell, on every edit forever. This is going to be huge! I'll have to figure out a way to condense these files, or start them off from a non-zero state so that I can throw out history at some point!'

Completely pointless.

At the beginning of the `cl-notebook` effort, I started a scratch file which I've been steadily editing, putting through various save/auto-save tests and just all round mauling with updates. This thing has been around for months at this point, and it's taken much harder beatings than the average notebook ever will. Wanna know what its current total size is?

```bash
inaimathi@self:~/.cl-notebook/books$ ls -lh base-zg1mm23z
-rw-r--r-- 1 inaimathi inaimathi 675K Apr 20 21:47 base-zg1mm23z
inaimathi@self:~/.cl-notebook/books$
```

That's smaller than many Word and PDF documents I've worked with, and those don't bother keeping my entire editing history around. So I figure I can get away with treating disk space as if it were infinite for my purposes. Absolute worst case scenario, I'll compress it. And since I'm dealing with plaintext files, that should be rather effective.

```bash
inaimathi@self:~/.cl-notebook/books$ pack -t tar.gz base-zg1mm23z
base-zg1mm23z
inaimathi@self:~/.cl-notebook/books$ ls -lh base-zg1mm*
-rw-r--r-- 1 inaimathi inaimathi 675K Apr 20 21:47 base-zg1mm23z
-rw-r--r-- 1 inaimathi inaimathi  23K Apr 20 21:47 base-zg1mm23z.tar.gz
```

Yeah. I think that'll be good enough.

## Return Values are Complicated

I mean, I knew that already, but it turns out there are even more intricacies here. I initially assumed I'd be able to just keep a single return value per `cell` (by which I mean the return from a single Lisp function, which can be zero, one or more values). Then it hit me that a cell might have more than one expression in it. Then it hit me that return values aren't enough; you need to be able to handle `*standard-output*` emissions and warnings on a per-expression basis rather than on a per-cell basis, and that we'd want type annotations in some places since we'll be serializing various things to strings and it would otherwise get confusing. Then I hit me and sat down to write down something workable. Each cell now stores a `result`, which is zero or more `values`, each of which is actually a `value` and a `type`.

That lets the front end figure out what it needs to do on a per cell basis, which means that the server-side implementation of a cells' `noise` becomes very mechanically simple. It's basically just an extra fact we keep around as a label, which the front-end queries to decide how to transform the high-detail `result`.

## `cell-type` is not the same thing as `cell-language`

Early on, I had this idea that I'd be semi-implicit about what's in a cell. At that point there were two `cell-types`; `common-lisp` and `cl-who`. The idea would be that this single cell-type would determine both display and evaluation properties of the contained code. Nope, as it turns out. And the thing that finally made this clear to me is thinking about how I'd treat test code. It's still Common Lisp, you see, so I'd still be evaluating it the same way as any other code cell, but I didn't want it showing up in certain exports.

The solution I ended up settling on is to be explicit about everything. Each cell now has a `cell-type` as well as a `cell-language`. The first one being one of `markup` (for prose blocks), `code` (for code blocks), and `tests` (for code blocks that I'd want to separate from actual runtime code).

## Naming things is difficult

I think there's a joke about this somewhere. Something along the lines of

    The only two difficult problems in programming are naming things, cache invalidation and off-by-one errors.

and man did that ever bite me this time. It's obviously bad to tie the file-system name of something to its display name, if for no reason other than it opens up various injection vectors that you'd rather not open up. It turns out it gets even more complicated when you're dealing with history trees of various documents, and you're trying to reduce headaches for your users. Here, think about this problem for a bit. Say you had a document type that you'd let your users name. We're obviously not naming the on-disk file after the display name of the document, so this is a matter of storing a record in the document that'll keep a user-entered name around for reference purposes. That gives you the additional benefit of being able to roll back renames, and the ability to see what a given document was called at some point in the past. Now, say you want to be able to branch said document. That is, instead of being a single history line, you want to be able to designate certain timelines as belonging to different branches than others. What you need now is four different levels of naming. Five, depending on how ham-handedly you've decided to store and relate those branches. At minimum you need

- The filename of the document, which is different from
- The display name of the same document (which might be different in different branches, and at different points in time), which is different from
- The display name of a particular branch of the document (which might need to be human readable, or user entered) which is different from
- The collective, still human-readable name for a set of branches belonging to one document.

If you've stored a branch collective as a file/folder on disk, you'll have to name that too. So, what would you do?

Confronted with this problem, I punted. Branching is basically going to be a copying operation. What you'll eventually get, once I put the branching system together and you try to use it, is a complete, self-contained fact base that happens to (possibly temporarily) have the same display-name as its origin (plus the word 'branch'), and a fact or two that point to a specific time point in that origin. From there, they'll diverge and be entirely separate entities. No, I'm not entirely sure this is the best approach, or even an acceptable approach, but it seems to be the only way to avoid asking the user to manage four different names in the space of one document. So I'll take it.

There's probably more where those came from, but they're all I could pull out of my head at short-ish notice. I'll try to let you know how the rest of the project goes, as it happens.
