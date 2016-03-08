I've actually been meaning to get around to trying this out ever since [the Dynamic Languages Smackdown](/article?name=language-smackdown.html)<a name="note-Wed-Feb-01-220959EST-2012"></a>[|1|](#foot-Wed-Feb-01-220959EST-2012).

So, I dusted off Pharo, which has had at least one major release since I last checked it out.

This time, I didn't pull punches, finding as many practical examples and tutorials to run through as I could. This includes two for Seaside<a name="note-Wed-Feb-01-221359EST-2012"></a>[|2|](#foot-Wed-Feb-01-221359EST-2012), the couple for Pharo proper, and a built-in tutorial named `ProfSteph` which you can run from the greeting screen.

So here are my first impressions<a name="note-Wed-Feb-01-221734EST-2012"></a>[|3|](#foot-Wed-Feb-01-221734EST-2012); I'm sure they'll change as I learn more about the language (and I fully intend to learn more about it)

## It Passes The Compaq Test

This thoroughly surprised me, because Smalltalk has a reputation for being brilliant and elegant but slow. I guess the people perpetuating this reputation mean "relative to C", because Seaside ran quite snappily off of a rather old machine with 256MB of ram. That was an "M". In fact, as I write this on that same ancient machine, I'm running Seaside in the background along with Slime and getting along perfectly well<a name="note-Wed-Feb-01-222417EST-2012"></a>[|4|](#foot-Wed-Feb-01-222417EST-2012).

## It Has "Source" Control

The word source is quoted because it's an image-based system, but a module called [Monticello](http://wiki.squeak.org/squeak/1287) basically does for Smalltalk what `git` would do for other languages. I wouldn't mention this, except that I remember thinking about it last time, and several other people at the Smackdown expressed similar concerns. So if your main excuse for staying away from Smalltalk is "I don't want to give up source control", you no longer have an excuse.

## Fantastic IDE

And this is coming from someone who usually hates IDEs. This one actually fails to get in my way at most opportunities, provides useful information and completions when I need them, is intuitive *and* well documented internally and externally, and (most importantly) does not take longer than Emacs to load up<a name="note-Wed-Feb-01-224001EST-2012"></a>[|5|](#foot-Wed-Feb-01-224001EST-2012). For those of you working cross-platform, it's also fully skinnable and comes with themes appropriate for the big three OSes (each of which it runs on beautifully).

## Turtles All The Way Down

Everything is an object. *Everything* **is** an object. Signs of this show up in the way loops and conditionals are treated, as well as the complete construction of the system<a name="note-Wed-Feb-01-225456EST-2012"></a>[|7|](#foot-Wed-Feb-01-225456EST-2012). It's kind of an extension of the previous point, but I wanted to emphasize it. That fantastic environment I mentioned? It's built in Smalltalk. The main click-menu (called the World Menu) is actually represented in the image. You can head over to the class browser and find a class called `TheWorldMenu`. You can also Ctrl + right-click on any component of the menu to activate its halo and fuck with internal variables. You probably *shouldn't*, but you could. This level of introspection happens for almost<a name="note-Wed-Feb-01-224813EST-2012"></a>[|6|](#foot-Wed-Feb-01-224813EST-2012) every component and sub-component you can see. I imagine this is what it would feel like to work on a full-out lisp machine.

## Great GUI Toolkit

I reserve the right to change my mind since I've only gone through some very basic activities, but it looks like it would be very easy to put together desktop applications with Smalltalk. I'm not super clear on how you'd go about deploying them, but there [seem](http://forum.world.st/Desktop-application-with-Pharo-td3453812.html) to be [ways](http://code.google.com/p/pharo/wiki/HowToDeployAnApplication).

That's the stuff that's attracted me. There's downsides too, of course, but they're not enough to give me pause. If you're just looking for an excuse not to try Smalltalk out, one of these should probably be enough.

## No Respect for BEDMAS

All of the manuals are quite explicit about this too; the fact that everything is an object means that the expression `3 + 5 * 2` isn't actually an expression. It's two binary messages being sent to two `SmallInteger`s. That means that the only reasonable way to be consistent about it is to treat arithmetic strictly from the left; so that the expression above will actually evaluate to `16` rather than the expected `13` if you try it out.

## Mouse-Centric

This may actually be a pro for some people, but it's not for me. The environment expects you to do most things with the mouse<a name="note-Wed-Feb-01-233751EST-2012"></a>[|8|](#foot-Wed-Feb-01-233751EST-2012). There's a greater than usual amount of time spent dealing with objects and widgets, so I guess that might be fair, but look. If your window system doesn't let me move between windows without reaching for the rat, you're doing something wrong. Being already used to a tiling WM just makes it that much more annoying<a name="note-Wed-Feb-01-233848EST-2012"></a>[|9|](#foot-Wed-Feb-01-233848EST-2012). A lot of things *have* keyboard shortcuts, but not everything, and those things that don't are quite annoying. Not *exactly* annoying enough to jump over to [GNU Smalltalk](http://smalltalk.gnu.org/), but still.

## Odd Choice of String Delimiters

In Smalltalk `"foo"` is not the string foo. It's actually the *comment* foo. The string foo looks like `'foo'`. How do you put an apostrophe in a string? You don''t. You either escape it with a second quote, or you use typographers’ quotes. Now you know. I'm still not entirely sure why this decision was made though. It seems like pretty much any other comment delimiters would have made more sense.

## Wonky Keyboard Shortcuts

I'm putting this one at the bottom of the list because I'm convinced that there must be a way to change them that I just haven't discovered yet. By "wonky", I don't mean "it uses the wrong letter", I mean "who the fuck thought this was the correct behavior?". Things like not having Ctrl+backspace `backward-word-kill` (giving that honor to Shift+backspace for comical effect), *or* having Ctrl+x kill a line, but move forward doing it and keep the `\n` in place, **or** having Ctrl+Right move forward a word, but skip newlines so that moving your point to the last symbol of a line is just that little bit more annoying. Also in this category, things like having `(`, `'` and `"` auto-close themselves, but only about half the time and with a noticeable delay. Like I said, this isn't that huge a deal because I'm convinced that


- There must be options I'm missing that will let me fix this
- Even if there is no explicit config option, there's a way to fix this through the object model, and it won't be complicated enough to drive me to drink


So there. That's first impressions after about half a week of poking at Pharo. Hopefully it came off as more positive than negative, because I really do like the language so far, but my internal censor goes a bit wonky at about 11:00, and I won't get a chance to proof this until tomorrow morning.

* * *
##### Footnotes

1 - <a name="foot-Wed-Feb-01-220959EST-2012"></a>[|back|](#note-Wed-Feb-01-220959EST-2012) - And now I'm shocked because I could have sworn it wasn't more than a year ago, but here we are.

2 - <a name="foot-Wed-Feb-01-221359EST-2012"></a>[|back|](#note-Wed-Feb-01-221359EST-2012) - Though the official Seaside tutorial has some issues with registering new handlers, in Pharo at least. They tell you to go the web-interface route, which consistently ignored my brilliant `HelloWorld` handler. Check the sidebar for links to working tutorials ("Seaside 3.0 in Pharo" actually gives you a working tour as of this writing).

3 - <a name="foot-Wed-Feb-01-221734EST-2012"></a>[|back|](#note-Wed-Feb-01-221734EST-2012) - Which differ from [Preliminary Impressions](/article?name=language-smackdown.html) in that they're founded on something more than gut feeling.

4 - <a name="foot-Wed-Feb-01-222417EST-2012"></a>[|back|](#note-Wed-Feb-01-222417EST-2012) - Though I will admit that `squeak` is at the top of `top` by using a whopping 6%-8% of my CPU and 10%-16% of my memory.

5 - <a name="foot-Wed-Feb-01-224001EST-2012"></a>[|back|](#note-Wed-Feb-01-224001EST-2012) - I'm looking at you, Eclipse. Though, to be fair, it's been a while, I guess you may have lost weight since then.

6 - <a name="foot-Wed-Feb-01-224813EST-2012"></a>[|back|](#note-Wed-Feb-01-224813EST-2012) - It does tell you to fuck off if you try to add a halo to a halo component. I'm assuming I have to edit that directly through the class editor rather than at the front end.

7 - <a name="foot-Wed-Feb-01-225456EST-2012"></a>[|back|](#note-Wed-Feb-01-225456EST-2012) - It's not cumbersome, though, every command I've typed so far has been extremely elegant, if alien (though that's just because I'm not used to it).

8 - <a name="foot-Wed-Feb-01-233751EST-2012"></a>[|back|](#note-Wed-Feb-01-233751EST-2012) - In fact, it advises you to go get a three-button mouse if you don't have one already.

9 - <a name="foot-Wed-Feb-01-233848EST-2012"></a>[|back|](#note-Wed-Feb-01-233848EST-2012) - Though I'm sure it must be possible to build one for the VM in such a way that you can easily strip it from your final product; I may look into this once I get my bearings. Edit: [Nevermind](http://forum.world.st/Tiling-Window-Manager-status-update-td3561695.html). Though it might still be a good learning exercise.
