The discussions around communities I frequent has recently touched on the idea of freedom; specifically, what it means in the context of software. We've moved past the [gratis vs libre](http://en.wikipedia.org/wiki/Gratis_versus_libre) confusion, thankfully, but there are still arguments about what "free" really means. Some are under the impression that in this context it means "I should be free to get my work done". That's a noble goal, but it misses the point somewhat. The sentiment behind that statement was [in defending PhotoShop](http://www.reddit.com/r/linux/comments/gywoj/adobe_and_linux_creative_suite_for_linux/) ("It shouldn't matter that I'm using proprietary software, the point is that it can do the job"). A fellow redditor puts it better than I could;


>   Software freedom is as much about choice as Lincoln's Emancipation Proclamation was about giving people the choice as to whether they wanted to retain slavery or not. Software freedom is about ending certain restrictive forms of software licensing and distribution which are harmful to the public and to technological progress in general.   
> --[spiceweasel](http://www.reddit.com/user/spiceweasel)  


Laced with the standard internet forum hyperbole, but it gets the point across. It's not about "freedom of choice", it's about some freedoms that are fundamental to the creation of software

> - The freedom to run the program, for any purpose (freedom 0).  
> - The freedom to study how the program works, and change it to make it do what you wish (freedom 1). Access to the source code is a precondition for this.  
> - The freedom to redistribute copies so you can help your neighbor (freedom 2).  
> - The freedom to distribute copies of your modified versions to others (freedom 3). By doing this you can give the whole community a chance to benefit from your changes. Access to the source code is a precondition for this.  
> --[GNU Free Software Definition](http://www.gnu.org/philosophy/free-sw.html)  

These are important freedoms to programmers and to users who understand programs. I certainly wouldn't trade them for anything in my private life, which is why I've been trying like hell to make this a free software household with as few compromises as possible.

### <a name="freedom-huh"></a>Freedom?

Now lets be honest from the other perspective. 

GIMP contains about [230 000 lines of C code](http://manual.gimp.org/en/gimp-introduction-history-2-0.html). Inkscape was at [132 134 lines of C/C++ back in 2003](http://inkscape.org/status/status_20031215.php) (pretty sure they've racked up a few 10Ks in the past 8 years or so). I have good cause to believe that PhotoShop is much larger<a name="note-Wed-May-18-234811EDT-2011"></a>[|2|](#foot-Wed-May-18-234811EDT-2011). I get that it would be morally better for PhotoShop to become a piece of libre software, but I'm having trouble convincing myself that there would be a practical benefit for the end user.

Here's an example near and dear to my heart. Emacs. It's extensible and reasonably simple to tweak. If you know Elisp, you can push it further than I've seen any other editor go. Aside from the fact that I've hooked in quite a number<a name="note-Wed-May-18-234920EDT-2011"></a>[|3|](#foot-Wed-May-18-234920EDT-2011) of components released by other Emacs users, I've also written a few of my own, and lightly amended some that I include. As the GNU page says, "Access to the source code is a precondition for this". When I say "simple to tweak", by the way, I mean "I can put together a new mode in an hour or two". In fact, as I was writing this article, I decided to take a poke at [blog-mode](https://github.com/Inaimathi/emacs-utils/blob/master/blog-mode.el) to change how a couple things work, and I didn't have to so much as *restart* Emacs. Hypothetically, if I had to restart (or god forbid, re-compile it) every time I tweaked something, there would be a little less incentive for me to engage in this kind of tool-building myself. There's some pre-conditions to building a system that behaves this way:

0. components must be hot-swappable

1. required code must be terse (but not past that threshold that takes it to `[line-noise](http://perl.plover.com/obfuscated/)` levels)

2. it must be simple and consistent enough that it doesn't take all of your brainpower over several hours to get into it and make changes (or, you must be able to more or less ignore the rest of the system while making changes to a specific piece)

3. An error can't bring the whole thing crashing down. It needs to toss you an exception gracefully, let you try some stuff to fix the problem dynamically and then continue on its merry way *without* a restart.

The reason I bring this up is

>   1. The freedom to study how the program works, and change it to make it do what you wish

Exactly how does Freedom One work if the program in question is larger than you can fit into your head? What if you *can* fit it into your head, but it takes several days or weeks of study? What if you can edit it but every time you tweak anything, you need to go through a 20 minute compilation step? Or even if you only had to go through that if you made some mistake in your tweaks? What if you didn't know the language it was written in, or didn't know how to program in the first place? At what point, exactly, can we say that whether the piece of software you've got cracked open is Free or not, you are not free to change it? It surprised me to no end to hear that there are actually people out there using Emacs who have neither the ability nor the desire to program in *any* language, let alone a quirky dialect of Lisp that's used exclusively for Emacs extension and development, but they apparently exist. In fact, outside the Emacs users community, non-programmers are the vast majority. Free Software must seem like the craziest catch 22 ever to them. 

You have the right and freedom to study and change this immeasurably complex machine that you can't possibly understand without devoting your life to (and which *will* barf at you with the slightest provocation).

If you line this up with the argument about Freedom and PhotoShop, it's pretty obvious what's happening. Hell, Stallman knew exactly what's happening a while ago.


>   The public traded nominal freedoms that it was not in a position to exercise and in exchange got some benefit (the benefit of more books being written). So if you have something that you can't use at all and you trade it for something of some value to you, you have gained. Whether or not it's the best possible deal you could have made, that's another question, but at least it was a beneficial deal.  
> --Richard Stallman (in his [talk at University of Calgary](http://www.youtube.com/watch?v=SNBMdDaYhZA))  


(The above was in reference to the establishment of copyright, but the principles still apply)

The people using Adobe's software don't have a hope of studying and changing it even if they had access to the complete source code, so why the hell should they care that they're *also* deprived of that source code? Or legally barred from studying and changing it? From the outside, it must seem like passing a law against unassisted breathing on the fucking moon. The *user-facing* documentation of PhotoShop is thick enough to beat someone to death with (or it used to be before it was digitized), so the sheer volume of developer-facing docs that comes out of this monstrosity is probably enough to drive someone to madness. If you provide a tool to aid in the understanding of an immeasurably complex machine, but that tool is itself immeasurably complex, you've done no good.

Freedom One does matter to me, but it's only because the software I use is (for the most part) simple and open enough to actually change myself. It matters in that situation whether I'm allowed to.

Is it enough that some awe-inspiring genius exists out there somewhere that can dive in and re-write PS in short order? Or that if the source was opened, there could possibly be a group of people that could fork it and make steady progress? Turning this over and over in my mind, I can only see a moral distinction and not a practical one. If the end user has no way of exercising their Freedom One, then I'm doubtful that the majority of them will ever take up the Free Software banner, and opening up something like PhotoShop runs into some fairly obvious walls. First, the sheer size of the thing means that a theoretical fork of the codebase would go literally nowhere for a very long time as people ramped up. Second, the people who tend to use it aren't programmers, and that's actually three big potential problems, take your pick. 

Second a) the size of the team is going to be small. Probably smaller than is useful on a task like this. 

Second b) the people you get aren't going to have the vested interest that comes with building a tool that they themselves want to use. They'll have to rely on the time tested method of [gathering requirements](http://steve-yegge.blogspot.com/2008/08/business-requirements-are-bullshit.html) if they're going to *grow* the program at all.

Second c) the team is going to be made up mostly of non-programmers, which sounds like almost-certain-fail from the get-go.

There's an entirely other discussion to be had about the middling UI quality in projects that aren't of direct use to programmers, but I'm not getting into that one with this little sleep.

### <a name="freedom"></a>Freedom!

It's beginning to dawn on me that if you really support the idea of Free Software, you are also logically opposed to the idea of monolithic systems. That if your goal is to maximize the Freedom of users, you **can not** hand them many hundred thousand lines of code (with an optional several thousand pages of documentation) and be content that you did your job. That suggests some basic principles by which to build Free Software


-   it needs to be built in small, individually understandable pieces (a few hundred lines across one file, not a few hundred thousand across hundreds)
-   large systems need to be built more or less by combining smaller systems
-   a large system shouldn't fail because one or more components failed
-   a system should be modifiable without restarting


Which is more or less how the *nix culture likes to build things from what I've seen. The trouble is how to reconcile this with the needs of end-users, and more specifically the moral/practical ramifications of doing so. Making things as easy as possible for non-programmers<a name="note-Thu-May-19-001035EDT-2011"></a>[|4|](#foot-Thu-May-19-001035EDT-2011) implies monoliths. They need as much of their computing experience handled by the computer as possible. It's really not acceptable, for example, to have the end user specify which audio/video codec and resolution to use for converting an mp4 to an avi. They likely don't know what a codec is, for starters. 

The question I'm going to wrestle with for the next little while is what to do about those people. On the one hand, I really don't think a rational approach is "Well, they should just learn how to program". That's a deathtrap, and if you disagree with me, I invite you to try talking about it to a medical receptionist sometime. The thing is, unless the end user is properly engaged and shown how their own freedoms are being infringed upon by proprietary software, they'll never jump ship. The next question is: is it morally right to grudge them the choice of trading nominal freedoms they are not in a position to exercise in exchange for some benefit? If a graphic designer chooses to use PhotoShop, should I be telling them to stop and use GIMP instead because proprietary software is a social problem, or is that fair play until they try to get *me* to switch back to Adobes' steaming pile? 

I have no answers for now, and I'm heading to sleep before I fall over onto my keyboard, but the thoughts are still bugging me in an odd way. Hopefully this piece released some pressure for the short term, at least.


* * *
##### Footnotes

1 - <a name="foot-Wed-May-18-234811EDT-2011"></a>[|back|](#note-Wed-May-18-234811EDT-2011) - I'm making the assumption since Adobe arguably has more man-hours to throw at the thing, releases new versions fairly regularly, has to implement a lot more compatibility with legacy/third-party file formats and supports color modes that GIMP doesn't. Granted the *combined* SLOCs of Inkscape/GIMP/Scribus/Synfig probably outweigh the Adobe Suite because the latter group shares some code, but comparing one to one programs seems like it would tilt the scales towards the proprietary offering.

2 - <a name="foot-Wed-May-18-234920EDT-2011"></a>[|back|](#note-Wed-May-18-234920EDT-2011) -  17 to be precise.

3 - <a name="foot-Thu-May-19-001035EDT-2011"></a>[|back|](#note-Thu-May-19-001035EDT-2011) - Who, again, are actually the majority of humans, and will probably remain so for a long time yet. Though I agree with Sussman that [there's immense value in teaching programming to non-programmers](http://video.google.com/videoplay?docid=-2726904509434151616#).
