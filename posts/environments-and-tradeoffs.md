I hit up [the Coding Dojo](http://www.meetup.com/Toronto-Coding-Dojo/events/calendar/) again this week, and we switched it up a little bit.

This time, we decided to split into two groups; one using Emacs<a name="note-Fri-Aug-31-105639EDT-2012"></a>[|1|](#foot-Fri-Aug-31-105639EDT-2012) and one using a simple, OS X editor.

Yes, I already pointed out what a bad idea this was, but to no avail. We have at least one person there who really *really* wants to use Emacs, so that's that I guess. Anyway, given that the group has its share of `vim` users, and its share of various IDE users, we were bound to get into a ribbing match by the end.

I may or may not have referred to the OS X setup as "Lowest Common Denominator", and then had to explain that I didn't mean it in a bad way. That told me that there's another part of my internal state that I assume is common knowledge, but may not be. If this *is* already part of your experience, you'll know within the next two sentences or so, and at that point you can skip the rest of the article knowing you're not missing much.

Here's the trade-off you make when you choose your environment, or customize it, or (if you're really hardcore) build your own: **ease-of-use** vs. **worth-learning**. That's ***NOT*** a fancy way of saying `"you fuckers are lazy for not learning #{my_editor}"`. There's an actual trade there.

## <a name="ease-of-use"></a>Ease of Use

When you sit down at this environment, it will be easy to pick up. You may need to learn one or two new keystrokes, and you may need to toggle one or two options to make it a bit comfortable. You will never have it explode on you. You'll never have to make use of its built-in auto-debugger, which it probably doesn't have, because it doesn't ship with the source code.

You'll also never really bend it to your will, which means that you won't be coding as fast as you can possibly be coding. You'll need to make peace with the fact that it just plain won't let you do certain things, or force you to do certain repetitive things manually, and that you'll need to use external tools for certain pieces of your workflow. Assuming you choose to live with it.

## <a name="worth-learning"></a>Worth Learning

You can not pick up this environment in a day or two. It will take you weeks or months. It has substantially different keybindings than general-purpose editors because it is or includes a *special*-purpose editor. You need to go through a lot of configuration before you get it feeling just right. You may need to change your keyboard layout slightly, and/or write up a few custom modules. You *will* see the debugger, and you will say "Thank fucking god that I have access to this", because you will *need* it.

You will likely be able to pull source code for it, and it will likely have its own modification language/framework<a name="note-Fri-Aug-31-110008EDT-2012"></a>[|2|](#foot-Fri-Aug-31-110008EDT-2012)

In the long run, it will make you much more productive. *Noticeably* more productive. You will show someone how you work, and their reaction will be "How the hell did you do that?".

## <a name="where-it-matters"></a>Where It Matters

If you're a professional programmer, and actually want to be effective at it, I'd argue that it's a mistake not to pick the second option for your solo programming time. Note that this *doesn't* mean "pick Emacs". I did, but that's mainly because of the languages I use. [vi](https://en.wikipedia.org/wiki/Vi)/[vim](http://www.vim.org/), [Eclipse](http://eclipse.org/), [leksah](http://leksah.org/), [jEdit](http://www.jedit.org/), or whatever might make as much sense for you. [Gedit](http://projects.gnome.org/gedit/), or [Notepad++](http://www.notepad-plus-plus.org/) or similar doesn't cut it here. And if there are any mutants out there coding in Word or something, just stay away from me, because I will not be able to veil my contempt.

The reason it makes sense there is that you're the only one whose effectiveness you need to worry about maximizing. That means that you can optimize the hell out of it without regard for the learning curve, or the portability<a name="note-Fri-Aug-31-111326EDT-2012"></a>[|3|](#foot-Fri-Aug-31-111326EDT-2012).

Now, on the flipside to the standard solo coding activity where your Worth Learning©™ environment should dominate, are use cases like pair programming. Or, oh, I dunno, Coding Dojos.

You do not want the same things in that situation.

First, you're not trying to maximize your own throughput, but the throughput of the group. It is a sub-optimal outcome if one of you can hit 10 lines per minute, and the rest can't even get to one. That means you have two options

- Set up a standardized environment that everyone in the group agrees to, send out a setup script for whatever platform, and have everyone practice outside of the event
- Set up a minimal environment with a very short learning curve so that everyone can pick it up without practice, and go back to their own customized environments otherwise

This is why I'm against using Emacs for physical, social coding unless it's with a complete group of Emacs users. You'll be handicapping some people pretty severely for no relevant benefit. In fact, unless you set up the vanilla Emacs distro, you'll be handicapping **everyone** for no relevant benefit, because every Emacs setup tends to be set up in its own way.

So yeah. "Lowest Common Denominator" is what you want here.

* * *
##### Footnotes

1 - <a name="foot-Fri-Aug-31-105639EDT-2012"></a>[|back|](#note-Fri-Aug-31-105639EDT-2012) - On GNU/Linux, but this is incidental; I put together the Emacs environment on my machine, and I happen to be a Debian user. We never used anything *other* than Emacs, so the fact that I use a Tiling WM never really came up.

2 - <a name="foot-Fri-Aug-31-110008EDT-2012"></a>[|back|](#note-Fri-Aug-31-110008EDT-2012) - *Good* language and framework optional; Elisp seems to be at the upper end of the curve these days, and it's not a particularly stellar language. Lack of namespace management gets pretty annoying after a while.

3 - <a name="foot-Fri-Aug-31-111326EDT-2012"></a>[|back|](#note-Fri-Aug-31-111326EDT-2012) - Though you probably should keep a setup script somewhere to make it easier for yourself to re-install if necessary.
