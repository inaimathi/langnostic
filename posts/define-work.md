So lately, I've been thinking about my work setup for hacking, and it occurs that the environment is in need of an overhaul. I've been using Windows and Eclipse, for god's sake. That's not even really excuseable. I mean yeah, I can hide behind the fact that my business card says "Graphic Designer" and take the perspective that it's the responsibility of our lone network admin to figure shit out for me, but that seems less satisfactory than doing something about it. It's not really perceived as a big deal; the rest of the team uses windows too (and our resident x-programmer is at once a Windows and Java fanatic), so there's bound to be little help and perhaps a little resistance. Here are the big reasons I'm thinking about switching now:


1.   We're planning to upgrade OSes soon anyway; it'll probably end up being to Windows 7, but I think I can at least change a couple of minds in IT given that our production servers are in fact running on Linux anyway.
1.   Between Windows, the anti-virus software, outlook and some other utilities, my boot time is in the area of 4 minutes. That doesn't sound too bad, except that my home workstation (which is ostensibly just for fun) boots in under 20 seconds. Also, because it's windows ...
1.   ... I have to reboot it at least once a day or it starts getting sluggish to the point of unuseability. Just. Perfect.
1.   I get the feeling that our admin could fix this had he taken levels in programmer, but Linux is much more easily scriptable. Specifically, the setup process is scriptable. Because of the apt-get facility (and our particular mix of technologies), it's perfectly possible for me to write a shell-script to set up a Linux dev machine from scratch. It's basically the standard LAMP stack plus Tomcat and Java 6. Eclipse and Subversion might take a bit of configuring, but since I use GIT (which can seamlessly interoperate thanks to git-svn) and Emacs, my install is essentially apt-get emacs23 git-core git-svn. Hell, I've even got lines in there to do a full checkout of the trunk and latest branch once git is installed.
1.   The current "solution" is a base image (Windows with an unconfigured outlook, and printer drivers) which the dev then has to spend about a day (two or three the first time) prepping to get up and running. This leaves something to be desired. ))


### <a name="define-pingemacs-" href="#define-pingemacs-"></a>(define (ping-emacs) (

What, you thought I only got one per post?
I haven't been doing any hardcore elisp hacking for the last few days, but I still make it a habit to eliminate inefficiencies. I ended up removing uniquify from my .emacs. It wasn't getting used (and I noticed a little note it came with to the effect that it has some slight conflicts with iswitchb mode, which I *do* make extensive use of). I also ripped out the option in smart-tab that has it use hippie-expand. It was much too overzealous in various places.
On the additions side, blog-mode now auto-closes tags when I type the ">" character (which is a behavior I'm thinking about just hooking into HTML mode), and I set the otherwise undefined C-' to comment-or-uncomment-region. This is another one of those times where it would have served me quite well to read up on stuff as a first reflex. I ended up reading about comment-region and uncomment-region, then spending 20 minutes or so hacking together a woefully inadequate way of getting one key-press to toggle commented state for a given region, then I stumbled across comment-or-uncomment-region on an unrelated trip through apropos.
Bottom line is, I don't think I'll ever be done optimizing, but I'm ok with that. This is an advantage of Emacs which I'm happy to have.
A little while ago, while I was still using Gedit for my typing needs, I decided that I really wanted non-broken frames ("winows" in Emacs parlance) in my editor, and I spent a good two or three hours searching around for an add-on to it that would do what I wanted. It's been a while so the situation might have changed, but as I recall, there was one add-on that added a single, secondary frame that basically got treated as a second instance of Gedit (except it didn't auto-save, or ask you to save any changes when you quit the main Gedit window), and a second add-on that did precisely what I wanted but wouldn't be available for another year and a half. This illustrates the common state of editors: they're great if you ignore their limitations or resign yourself to living with them.
Emacs is different; because hacking on it is central to the idea, there's very rarely a feature you could imagine that you can't also implement yourself with a few hours of typing. Even better than that, small changes are trivial to add. That tag-closing function I whipped up that auto-fires on my typing">"? That took about 40 seconds to add. And no, it's not done yet, I'll probably change its binding or behavior, but the fact that I *can* is awesome. 
It feels very gratifying that the cycle of 

-   Think up new feature
-   Code it
-   Test it
-   Use it

could be so fast.))

(define (addendum)
'(I decided to change up the interaction on that auto tag-close function to fire when I type "&lt;/" rather than ">" (which is to say, when I type "/", Emacs checks if the previous char is "<", and fires the tag-close event if it is, otherwise it just inserts "/" as usual).
																																	  Closing tags on ">" with a save-excursion is what Eclipse does as I recall, so I sort of assumed it would be the correct behavior. It turns out that if I have any say in the matter, it's vastly preferable to have my editor close that tag when I'm already  half-way into it. I may change it back again later, depending on what I notice during actual use, but this way seems to grant me more control so far.))

<!--  LocalWords:  scriptable interoperate unconfigured uncomment
 -->
