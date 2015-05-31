Just a quick update today.

First, I've pushed an update to [the formlets project](https://github.com/Inaimathi/formlets) on github. It now supports input type file (which includes managing the enctype properly, displaying file inputs and providing a couple of basic predicate generators for validation). Check the [project page](https://github.com/Inaimathi/formlets), or the [new wiki](https://github.com/Inaimathi/formlets/wiki), both of which have slightly more robust documentation than you'll find here. 

There's really no occasion to this, by the way. I try to be a self-centered realist in terms of design philosophy, so the only reason I added file fields here was that I finally found I needed them. It actually surprises me quite a bit that I got by for so long with only inputs, passwords, textareas and recaptchas, but there you have it. I'm in the middle of another project at work now though, so I may soon add options and dates. Don't hold your breath though.

Second, I've been figuring out loop for the past little while (and re-wrote a lot of the formlet internals with it, now that I've realized that it can basically do everything). The pieces that loop helped in are ones that would otherwise have to be expressed in terms of recursion and some intermediary variables. If you want to take a look at it in action, check out the "Validation related functions" section in [this diff](https://github.com/Inaimathi/formlets/commit/0e9f9bd1f608ff9f02867e895385c5455ce365ee#formlets.lisp). Seven lines of loop saved me something like 12 lines of recursion and siz lines of helper function. And not only that, but it's (in my opinion, obviously) much easier for a human reader to parse this way. I haven't learned it yet, and doubt I ever will, given [its specification](http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm). It looks like loop itself is, without exaggeration, several times more complicated than the rest of Common Lisp combined. iterate doesn't seem to be much better in this regard, by the way. It seems to be loop with a few extra parens thrown in. But loop isn't hard to learn because it doesn't have enough parentheses, it's hard to understand becuase it's mind-bendingly complicated.

In any case, I've found tutorials on both [iterate](http://common-lisp.net/project/iterate/doc/Don_0027t-Loop-Iterate.html) and [loop](http://www.ai.sri.com/pkarp/loop.html) (as well as the [CL cookbook loop entry](http://cl-cookbook.sourceforge.net/files.html) and [Seibels' treatment in PCL](http://www.gigamonkeys.com/book/loop-for-black-belts.html)). The two things that I needed to know in order to make that formlets code work were either omitted or buried, or merely implied. Specifically, I needed to interate by two elements of a list, and I needed to be able to return a flat list that had double the elements of the input (collecting two elements per input element). Basically

```lisp
'(:a 1 :b 2 :c 3 :d 4) => '(:a :b :c :d)
'(:a :b :c :d) => '(:a "A" :b "B" :c "C" :d "D")

```

That's very slightly beyond mapcar as far as I know, so the way it ended up getting written was a recursion. That ended up being very complicated (not that the situation helped; this would have been relatively straightforward in regular code, but throw in two or three steps of macroexpansion, and it gets ugly fast). So, for my own future reference (and hopefully, for the benefit of anyone else that does a search on this), here's how you do it with loop.

```lisp
;; Iterating by multiple elements
(defvar test-list '(:a 1 :b 2 :c 3 :d 4))
> TEST-LIST

(loop 
   for (key value) on test-list by #'cddr
   collecting key)
> (:A :B :C :D)

;; Collecting multiple elements
(setf test-list '(:a :b :c :d))
> (:A :B :C :D)

(loop 
   for key in test-list
   collecting key 
   collecting (string key))
> (:A "A" :B "B" :C "C" :D "D")

```

You can actually skip the "ing" in this case, writing the last two clauses as collect key collect (string key). There's also no requirement for making this a multi-line statement, I just feel that it's easier to read here.

Third, and no one other than me cares, so you may want to go read [something interesting](http://bc.tech.coop/blog/081231.html) instead.

[74 wpm.jpg]

I know it's not very impressive yet, but keep in mind that I started off in the 35-45 range. Hopefully, I can crack 100 this year.
