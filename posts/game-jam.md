The first part is a journal of thoughts as they were happening, so try not to laugh at me too hard. The second part is a bit of reflection on the [project so far](https://github.com/Inaimathi/cl-leet). This is a summary/record of my week participating in the [2011 Spring Lisp Game Jam](http://lispgames.org/index.php/2011_Spring_Lisp_Game_Jam). Hopefully, it's useful to someone (the very act of writing it was very useful to me).

* * *

### April 23

I did the first public facing push of this code-base at about 4:00 am my time today, and then curled up into bed. When I got up at 10:30, there was already a bug report waiting for me. Not off to a spectacular start...

The problem turned out to be how cl-css handles file compilation. Specifically, it fails if the directory it's trying to compile to doesn't exist. At this point, I'm ready to load up `cl-fad` to whip up a simple fix, except that `cl-fad` doesn't have a function to create directories. Okay... maybe it's a Lisp primitive then? A quick search for "directory" over at the [hyperspec symbol index](http://www.lispworks.com/documentation/HyperSpec/Front/X_AllSym.htm) turns up nothing. So...huh? I spend 10 minutes checking the [cookbook](http://cl-cookbook.sourceforge.net/) and googling fruitlessly<a name="note-Sat-Apr-30-024647EDT-2011"></a>[|1|](#foot-Sat-Apr-30-024647EDT-2011). That doesn't discourage me though. I commonly run into situations where I have a naive mental model of a given problem that some Lisper has already built an elegant solution to. So I google the more general case `common lisp making directory`, which turns up a [lispworks doc page](http://www.lispworks.com/kb/a461eb08ebd7270580256b680068e9ca.html) abut `[ensure-directories-exist](http://www.lispworks.com/documentation/HyperSpec/Body/f_ensu_1.htm#ensure-directories-exist)`. 

This is what I meant. When I'm making a directory, I really just want a place to put some specific file. Abstracting the basic directory creation means that I don't have to special-case the situation where multiple nested directories are required, and I also don't need to worry about checking whether the directory exists before creating it. Elegant. Maybe this is a good start. Day 1 and I'm already learning something.

### April 24

I went pretty batshit on the code-base today, removing anything that looked like an unnecessary additional step. I'd been keeping the model and view as separate as possible because of an assumption that there would be some sort of SQL database involved in the game eventually. I may still need to switch to one at some point, so the move might come back to bite me in the ass, but the barriers got broken down. Mostly it was intermediate functions all over the place that slightly simplified the next set of intermediate functions<a name="note-Sat-Apr-30-024740EDT-2011"></a>[|2|](#foot-Sat-Apr-30-024740EDT-2011). Things like `planet-info` and `inventory` which took model structs and returned `plists` instead. To be fair, that actually did help at some point; since there doesn't seem to be a way to map over a `struct`, it was very easy to dump an entire `plist` into the view with a `loop`.

Anyway, I made the decision during my shower that I'm keeping the `struct` approach since I think it might help with the concurrency issues I'll be facing next. There's also one decision from earlier that's sort of coming back to haunt me there. The way that a given `captain`'s current planet is stored is as a string of that planets' name. That meant that I needed a function called `planet-name->planet` early on, and it meant that any manipulation of the current planet happened directly on the `*galaxy*`. It's fine for a single player, but more would cause some odd errors that wouldn't be particularly fun. The way I'm thinking of solving that is by having each captain copy out the current planet, and keep track of transactions they conduct. The total changes would then get applied when they travel. A given market could still dip into the negatives in some circumstances, but it should serve to make it much harder to force an error on an opponent sharing your planet.

Finally, markets behave differently now. Instead of static prices, each market generates a new price from the `*tradegoods*` and averages (By which I mean "gets the arithmetic mean of") that with its current price. It just makes sure that markets tend to stabilize over time, while hopefully keeping enough fluctuations to make profit possible.

### April 25

Changed up a lot more of the system. Today, I focused on the `purchase!`/`convey!` end of things rather than the basic model. Basically, captains now have a copy of the planet they take with them and merge transactions back later. As I mentioned yesterday, this isn't a measure to make sure that no more product leaves a planet than absolutely should. In fact, I'm tempted to make deficit exports an explicit mechanic, upping the price of a good fairly substantially if it slides into the negatives. That would also play off a tweak I want to make to the `market-produce!` function. Namely, generating `(+ 4d20-30 (/ productivity tech-level))` instead of the current `(+ 2d20 (/ productivity tech-level))` to allow goods to be consumed as well as created by planets<a name="note-Sat-Apr-30-025032EDT-2011"></a>[|3|](#foot-Sat-Apr-30-025032EDT-2011).

Today's lesson<a name="note-Sat-Apr-30-025041EDT-2011"></a>[|4|](#foot-Sat-Apr-30-025041EDT-2011) was that state is fucking hard. Introducing side-effects to a primarily functional system played more hell with my code than I thought it would. I still don't know if I squashed every possible bug with that transaction system. It's enough to make me go paranoid and start sprinkling `assert`s in the vicinity of all `setf`s and `!`s. I won't yet; I'll wait 'till something unexpected blows up, but I can see an argument for it here. The end result is that this game should now be playable by more than one human at a time. There's no explicit goals yet, I'm going to save that for later, I think, but you can still get a few friends together on a lan<a name="note-Sat-Apr-30-025129EDT-2011"></a>[|5|](#foot-Sat-Apr-30-025129EDT-2011) and play economic hell with a small galaxy with the game as it stands.

[Break time to remodel my kitchen. Did a bit of light view coding, but nothing interesting to report.]

### April 28

Spent today mainly on the UI end. Making it pretty-ish and squashing a few display bugs I came across. Still not happy with the default theme, but it's all I'll have time for in a week. I did make an effort to completely disconnect it from all other code, so others can theoretically just push a folder of images+theme.css to add a new one. The entire experience was a bit surreal; in the middle of using GIMP to do some graphic design work, I found myself embroiled in a conversation on Reddit about how GIMP is not good enough to do design work. Since the best argument I read was "But, I **like** Photoshop!" I'll stick with the free<a name="note-Sat-Apr-30-025452EDT-2011"></a>[|6|](#foot-Sat-Apr-30-025452EDT-2011) version, thank you.

Part of that UI tweaking mentioned above was adding jQuery and jQuery UI to the codebase. I didn't want to have to tell people to go download it themselves, and there's really nothing that needs to be done other than putting them in the correct directories. The downside is that this is now considered a Javascript project by GitHub. Which, ok, I guess is true by character count, but it still feels inaccurate.

### April 29

I realized earlier today that my time's officially up for this little project<a name="note-Sat-Apr-30-025551EDT-2011"></a>[|7|](#foot-Sat-Apr-30-025551EDT-2011). Technically, time ran out about an hour ago, but I kinda got into the swing of things and decided to finish one last TODO before doing a final check-in for the event and collecting my thoughts. That ended up taking more time than I thought it would, but it really was my own fault for giving into indecision for so long. The game looks passable, it plays nicely, doesn't seem to blow up<a name="note-Sat-Apr-30-025613EDT-2011"></a>[|8|](#foot-Sat-Apr-30-025613EDT-2011) and is actually pretty fun<a name="note-Sat-Apr-30-025631EDT-2011"></a>[|9|](#foot-Sat-Apr-30-025631EDT-2011). I really shouldn't have picked out something this ambitious for a week I knew I'd be occupied, but hey. Such is life, I guess. I'll keep working on it for as long as I can stand to look at the code-base, but for the moment, it's done.

Anyway, lets get to the juice; here's the distillation of what I learned in trying to put together a small-ish game in Lisp over the course of a week.

**Incremental beats planned...**

It was honestly surprising *how* true this turned out to be. I started out trying to plan out as much stuff as possible into the future, going so far as to put several levels of indirection into the model in case of a switch to a relational database later. That... was a tremendous waste of time. Both in the sense that useless code was produced as a result, and that I then had to spend time going through call-trees to figure out what a given function was actually doing. I [got fed up](https://github.com/Inaimathi/cl-leet/commit/43d5fe31e00f0bbe2c6eddcab7211d59567f19ac) with that fairly early on in the week and went on an already noted rampage through the model, deleting everything that I wasn't actively calling *right now*, preferably in multiple places.

Overall, the time spent just diving in and writing code resulted in a lot more of the final code-base than carefully plotting out where I needed to go<a name="note-Sat-Apr-30-025825EDT-2011"></a>[|10|](#foot-Sat-Apr-30-025825EDT-2011). That's probably partly a result of the plans not being comprehensive enough, and partly a result of the plans changing mid-way. I'd argue that's healthy though; there are some specific interaction points that I'd have been hard pressed to predict in advance, but that became perfectly obvious when I tried to implement them properly.

Don't read this as "don't plan", because it does save a bit of time, but not as much as I thought<a name="note-Sat-Apr-30-025905EDT-2011"></a>[|11|](#foot-Sat-Apr-30-025905EDT-2011). My best guess is that there's some point of diminishing returns with planning. You want to do about that much because doing less causes some pretty serious headaches in terms of direction, but if you do more, you're not helping (and are likely hurting). That's just a hypothesis based on my experience, of course, I'd love to see some experimental data about it.

**...but remember to fix it up**

The other side of the incremental approach is that it ends up producing a lot more code. The main catch is that you need to remember to stay flexible, since you might be changing large portions of the system at any given point. Even so, prospective coding still turned out to be a very powerful tool. Producing more code does mean that you have to periodically measure and cut again. There are already two places where I can see a lack of planning costing me efficiency, but I'm not sure I would have been able to predict that in advance.

**Distractions hurt. A lot.**

That really shouldn't even need saying, but there you have it. Trying to code up a storm doesn't work out very well when you're also re-modeling your kitchen. I'll know for next time, I guess. The one upshot is that it resulted in a system that's easy to fit in my head basically by *necessity*, because I had no hope whatsoever of getting an entire 8 hour stretch of time devoted to it.

**Deadlines help. A lot. No, more than that.**

I've had this project on the back-burner for close to four months. In those four months, I've managed to put together a half-way decent name generator and comb over the code-base for [Elite for Emacs](http://members.fortunecity.com/salkosuo/elite-for-emacs/) a dozen times or so. In the past five days or so, I did the rest of it<a name="note-Sat-Apr-30-030242EDT-2011"></a>[|12|](#foot-Sat-Apr-30-030242EDT-2011). The deadline helped get me in gear where I probably would have sat indecisively for a further few months. Even with all the distractions, I easily did four times as much work since the beginning of the game jam as I did since starting on this.

**Reflection helps. A lot. No, more than that. Keep going. A bit more. Ok, there.**

Even more than the deadline, the fact that I had to look at this from different perspectives helped a lot. It wasn't just writing code, there was also a lot of brain power spent on thinking about how I would explain the code, what it meant in the grander scheme of the project, and whether there was a better way of doing it. It's a bit counter-intuitive, but it seems that reflection (thinking about things you've just done) is a lot more useful and a lot more productive than planning (thinking about things you're going to be doing soon). I didn't end up explaining it very well regardless, but the process of thinking up said explanations seemed to help in the construction regardless.

**I don't know enough**

Probably the biggest one. I'm not sure if any hypothetical reader can apply this, but I sure as hell will. I've got a lot of learning left to go in pretty much every direction. First, I get the feeling that most of the model could have been put together much better using CLOS than my current approach of `structs` and functions. For example, `planet-produce!`, `add-to-market!`, `banned?` and `local?`<a name="note-Sat-Apr-30-030439EDT-2011"></a>[|13|](#foot-Sat-Apr-30-030439EDT-2011) would have made more sense as methods. I can say that without actually knowing much about CLOS other than the name, and I'll be going through documentation and tutorials as soon as I can squeeze it into my day. The 3D math is also something I'm rusty on. I could swear I used to know this stuff back in high school, but got surprisingly little use out of it since. It would have helped a lot. Specifically, having a clearer understanding of 3D transformation would have let me create an actual 3D interface instead of faking it poorly with layers. I've ... gotta work on that. Lastly<a name="note-Sat-Apr-30-030534EDT-2011"></a>[|14|](#foot-Sat-Apr-30-030534EDT-2011), I still have no clue how to use [`loop`](http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm) properly. I gather I'm not alone since it gets its own chapter in both [PCL](http://www.gigamonkeys.com/book/loop-for-black-belts.html) and the [CL Cookbook](http://cl-cookbook.sourceforge.net/loop.html), but it still caught me by surprise how deep the construct is.

So there. I knew I knew little, but it turns out I knew less than I knew.

That's certainly something to fix for next time.

* * *
##### Footnotes

1 - <a name="foot-Sat-Apr-30-024647EDT-2011"></a>[|back|](#note-Sat-Apr-30-024647EDT-2011) - "I guess Lisp can't create directories." Is what I would have thought about two years ago
2 - <a name="foot-Sat-Apr-30-024740EDT-2011"></a>[|back|](#note-Sat-Apr-30-024740EDT-2011) - And by "slightly", I mean "almost not at all"; the functions that did do necessary abstraction are still there
3 - <a name="foot-Sat-Apr-30-025032EDT-2011"></a>[|back|](#note-Sat-Apr-30-025032EDT-2011) - Produced, on balance, but it opens up that scarcity mechanic
4 - <a name="foot-Sat-Apr-30-025041EDT-2011"></a>[|back|](#note-Sat-Apr-30-025041EDT-2011) - Or, rather, reminder.
5 - <a name="foot-Sat-Apr-30-025129EDT-2011"></a>[|back|](#note-Sat-Apr-30-025129EDT-2011) - Or an open server, if you're feeling especially frisky
6 - <a name="foot-Sat-Apr-30-025452EDT-2011"></a>[|back|](#note-Sat-Apr-30-025452EDT-2011) - Libre, not gratis (although GIMP is that too).
7 - <a name="foot-Sat-Apr-30-025551EDT-2011"></a>[|back|](#note-Sat-Apr-30-025551EDT-2011) - leastwise, my time for the Game Jam is up
8 - <a name="foot-Sat-Apr-30-025613EDT-2011"></a>[|back|](#note-Sat-Apr-30-025613EDT-2011) - even when there are multiple people playing at once
9 - <a name="foot-Sat-Apr-30-025631EDT-2011"></a>[|back|](#note-Sat-Apr-30-025631EDT-2011) - though at the moment, it's really a lot of economic activity for its own sake because I didn't get to [TODO: Goals](https://github.com/Inaimathi/cl-leet/issues/10) or [TODO: Ship upgrades](https://github.com/Inaimathi/cl-leet/issues/11) yet
10 - <a name="foot-Sat-Apr-30-025825EDT-2011"></a>[|back|](#note-Sat-Apr-30-025825EDT-2011) - which is to say, most of the code resulting from careful planning was later replaced by code that had an immediate, unplanned need
11 - <a name="foot-Sat-Apr-30-025905EDT-2011"></a>[|back|](#note-Sat-Apr-30-025905EDT-2011) - and probably not as much as *you* think, if you're a [Joel](http://www.joelonsoftware.com/) fan
12 - <a name="foot-Sat-Apr-30-030242EDT-2011"></a>[|back|](#note-Sat-Apr-30-030242EDT-2011) - which is to say, ported it from Elisp to CL, a working market system, GUI with faux-3D interface, debugging and a copious amount of testing
13 - <a name="foot-Sat-Apr-30-030439EDT-2011"></a>[|back|](#note-Sat-Apr-30-030439EDT-2011) - and others besides, I'm sure
14 - <a name="foot-Sat-Apr-30-030534EDT-2011"></a>[|back|](#note-Sat-Apr-30-030534EDT-2011) - or, at least, the last specific thing I was surprised to find myself ignorant of
