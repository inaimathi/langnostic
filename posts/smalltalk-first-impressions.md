I've actually been meaning to get around to trying this out ever since [the Dynamic Languages Smackdown](/posts/language-smackdown-notes-and-smalltalk)[^now-im-shocked].

[^now-im-shocked]: And now I'm shocked because I could have sworn it wasn't more than a year ago, but here we are.

So, I dusted off Pharo, which has had at least one major release since I last checked it out.

This time, I didn't pull punches, finding as many practical examples and tutorials to run through as I could. This includes two for Seaside[^issues-with-seaside-tutorial], the couple for Pharo proper, and a built-in tutorial named `ProfSteph` which you can run from the greeting screen.

[^issues-with-seaside-tutorial]: Though the official Seaside tutorial has some issues with registering new handlers, in Pharo at least. They tell you to go the web-interface route, which consistently ignored my brilliant `HelloWorld` handler. Check the sidebar for links to working tutorials ("Seaside 3.0 in Pharo" actually gives you a working tour as of this writing).

So here are my first impressions[^which-differ-from]; I'm sure they'll change as I learn more about the language (and I fully intend to learn more about it)

[^which-differ-from]: Which differ from [Preliminary Impressions](http://localhost:4444/posts/language-smackdown-notes-and-smalltalk#preliminary-impressions-of-smalltalk) in that they're founded on something more than gut feeling.

## It Passes The Compaq Test

This thoroughly surprised me, because Smalltalk has a reputation for being brilliant and elegant but slow. I guess the people perpetuating this reputation mean "relative to C", because Seaside ran quite snappily off of a rather old machine with 256MB of ram. That was an "M". In fact, as I write this on that same ancient machine, I'm running Seaside in the background along with Slime and getting along perfectly well[^i-will-admit].

[^i-will-admit]: Though I will admit that `squeak` is at the top of `top` by using a whopping 6%-8% of my CPU and 10%-16% of my memory.

## It Has "Source" Control

The word source is quoted because it's an image-based system, but a module called [Monticello](http://wiki.squeak.org/squeak/1287) basically does for Smalltalk what `git` would do for other languages. I wouldn't mention this, except that I remember thinking about it last time, and several other people at the Smackdown expressed similar concerns. So if your main excuse for staying away from Smalltalk is "I don't want to give up source control", you no longer have an excuse.

## Fantastic IDE

And this is coming from someone who usually hates IDEs. This one actually fails to get in my way at most opportunities, provides useful information and completions when I need them, is intuitive *and* well documented internally and externally, and (most importantly) does not take longer than Emacs to load up[^lookin-at-you-eclipse]. For those of you working cross-platform, it's also fully skinnable and comes with themes appropriate for the big three OSes (each of which it runs on beautifully).

[^lookin-at-you-eclipse]: I'm looking at you, Eclipse. Though, to be fair, it's been a while, I guess you may have lost weight since then.

## Turtles All The Way Down

Everything is an object. *Everything* **is** an object. Signs of this show up in the way loops and conditionals are treated, as well as the complete construction of the system[^not-cumbersome]. It's kind of an extension of the previous point, but I wanted to emphasize it. That fantastic environment I mentioned? It's built in Smalltalk. The main click-menu (called the World Menu) is actually represented in the image. You can head over to the class browser and find a class called `TheWorldMenu`. You can also Ctrl + right-click on any component of the menu to activate its halo and fuck with internal variables. You probably *shouldn't*, but you could. This level of introspection happens for almost[^tells-you-to-fuck-off] every component and sub-component you can see. I imagine this is what it would feel like to work on a full-out lisp machine.

[^not-cumbersome]: It's not cumbersome, though, every command I've typed so far has been extremely elegant, if alien (though that's just because I'm not used to it).

[^tells-you-to-fuck-off]: It does tell you to fuck off if you try to add a halo to a halo component. I'm assuming I have to edit that directly through the class editor rather than at the front end.

## Great GUI Toolkit

I reserve the right to change my mind since I've only gone through some very basic activities, but it looks like it would be very easy to put together desktop applications with Smalltalk. I'm not super clear on how you'd go about deploying them, but there [seem](http://forum.world.st/Desktop-application-with-Pharo-td3453812.html) to be [ways](http://code.google.com/p/pharo/wiki/HowToDeployAnApplication).

That's the stuff that's attracted me. There's downsides too, of course, but they're not enough to give me pause. If you're just looking for an excuse not to try Smalltalk out, one of these should probably be enough.

## No Respect for BEDMAS

All of the manuals are quite explicit about this too; the fact that everything is an object means that the expression `3 + 5 * 2` isn't actually an expression. It's two binary messages being sent to two `SmallInteger`s. That means that the only reasonable way to be consistent about it is to treat arithmetic strictly from the left; so that the expression above will actually evaluate to `16` rather than the expected `13` if you try it out.

## Mouse-Centric

This may actually be a pro for some people, but it's not for me. The environment expects you to do most things with the mouse[^in-fact]. There's a greater than usual amount of time spent dealing with objects and widgets, so I guess that might be fair, but look. If your window system doesn't let me move between windows without reaching for the rat, you're doing something wrong. Being already used to a tiling WM just makes it that much more annoying[^must-be-possible]. A lot of things *have* keyboard shortcuts, but not everything, and those things that don't are quite annoying. Not *exactly* annoying enough to jump over to [GNU Smalltalk](http://smalltalk.gnu.org/), but still.

[^in-fact]: In fact, it advises you to go get a three-button mouse if you don't have one already.

[^must-be-possible]: Though I'm sure it must be possible to build one for the VM in such a way that you can easily strip it from your final product; I may look into this once I get my bearings. Edit: [Nevermind](http://forum.world.st/Tiling-Window-Manager-status-update-td3561695.html). Though it might still be a good learning exercise.

## Odd Choice of String Delimiters

In Smalltalk `"foo"` is not the string foo. It's actually the *comment* foo. The string foo looks like `'foo'`. How do you put an apostrophe in a string? You don''t. You either escape it with a second quote, or you use typographers’ quotes. Now you know. I'm still not entirely sure why this decision was made though. It seems like pretty much any other comment delimiters would have made more sense.

## Wonky Keyboard Shortcuts

I'm putting this one at the bottom of the list because I'm convinced that there must be a way to change them that I just haven't discovered yet. By "wonky", I don't mean "it uses the wrong letter", I mean "who the fuck thought this was the correct behavior?". Things like not having `Ctrl+backspace` perform `backward-word-kill`[^giving-that-honor], *or* having `Ctrl+x` kill a line, but move forward doing it and keep the `\n` in place, **or** having `Ctrl+Right` move forward a word, but skip newlines so that moving your point to the last symbol of a line is just that little bit more annoying. Also in this category, things like having `(`, `'` and `"` auto-close themselves, but only about half the time and with a noticeable delay. Like I said, this isn't that huge a deal because I'm convinced that

[^giving-that-honor]: Giving that honor to `Shift+backspace` for comical effect.

- There must be options I'm missing that will let me fix this
- Even if there is no explicit config option, there's a way to fix this through the object model, and it won't be complicated enough to drive me to drink

So there. That's first impressions after about half a week of poking at Pharo. Hopefully it came off as more positive than negative, because I really do like the language so far, but my internal censor goes a bit wonky at about 11:00, and I won't get a chance to proof this until tomorrow morning.[^from-the-future]

[^from-the-future]: Hello, from the distant world of 2016! As of now, I still haven't sat down to use Smalltalk in any kind of serious project, and I'm beginning to think that I very probably never will. At this point, I've got a bunch of other languages waiting in the queue, all of which look more interesting, and the idea of having to go back to object orientation for any project of significant size is not exactly stirring a longing in my heart. To be fair, I _have_ been using Ruby professionally lately, and that's about as close as you can get to Smalltalk without involving an actual image-based substrate. Maybe that's close enough?
