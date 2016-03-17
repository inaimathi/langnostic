I'm officially taking a break from thinking. And I'm doing it by taking in some contract work. NDAs are signed, and contracts exchanged, so while I can't really tell you specifics, it's a WordPress site. Nothing so nourishing as my usual fare; this is the proverbial greasy, bacon and cheese pizza for the soul.

The situation has blown me away, by the way. Did you know that there's a staggering amount of money out there for enterprising young hacker/graphic-designer dual classes willing to tweak around with CSS and PHP? It's bizarre because there's nothing inherently difficult about this. It's a fairly well documented, intuitively-named, neatly-packaged pile of security-errors-waiting-to-happen and all you really need to do is poke at a couple of places to make the colors show up right and the logo line up with the menu. It feels strange. I *know* I struggled with this same shit back at school, but it doesn't feel tough any more. It feels a bit like getting past Cinnabar Island with your pack of level 45 motherfuckers and finding yourself back in idyllic Pallet Town tearing through now-helpless level ~3 Pidgeys. That analogy may brand me as an irredeemable nerd, so feel free to substitute a more mainstream level-grinding RPG if you like. My point is, I used to be helpless in this situation, and I am now arranging reality to suit my whim.

The odd part given my recent thoughts, the really odd part, is that it pays *at all*. Let alone well. This is a simple task that anyone with sufficient time, interest and an internet connection (broadband optional) can learn how to do. It's the ultimate expression of freedom brought about by the GPL style of licensing and application design I talked about earlier. Why the ever-loving **fuck** aren't there more non-moron contractors taking advantage of the situation? Why aren't these jobs being shipped overseas like so many people seem to think all IT should be? If I had to pick *one* type of development that a language barrier wouldn't cock up, it would be CMS tweakery. Oddly, it's the one type of IT work that doesn't seem to be going anywhere, at least for the short term, because every place I've worked at so far and every client I've contracted with is having some local WordPress/Joomla/Drupal work done<a name="note-Sun-Jun-12-123210EDT-2011"></a>[|1|](#foot-Sun-Jun-12-123210EDT-2011).

Back to the Free thing though, since that's what I was thinking about prior to my break. How does PHP+WordPress support those freedoms? First off, it actually [is GPL2](http://wordpress.org/about/gpl/)<a name="note-Sun-Jun-12-123225EDT-2011"></a>[|2|](#foot-Sun-Jun-12-123225EDT-2011) but second off, it hits all of my added requirements, seemingly by accident.


>   0. components must be hot-swappable
>   1. required code must be terse (but not past that threshold that takes it to line-noise levels)
>   2. it must be simple and consistent enough that it doesn't take all of your brainpower over several hours to get into it and make changes (or, you must be able to more or less ignore the rest of the system while making changes to a specific piece)
>   3. An error can't bring the whole thing crashing down. It needs to toss you an exception gracefully, let you try some stuff to fix the problem dynamically and then continue on its merry way without a restart.
> --me, [last time](/posts/freedom)


Components are hot-swappable thanks to how it interacts with Apache<a name="note-Sun-Jun-12-123259EDT-2011"></a>[|3|](#foot-Sun-Jun-12-123259EDT-2011). WordPress is terse yet readable, it's consistent enough that I can get into the codebase easily **and** I can ignore pieces outside the specific widget/CSS component/theme I'm dealing with at whatever granularity. Finally, an error nukes a page, but doesn't crash the entire site (unless it's made in a template, but that's to be expected). Take *that*, every language I have at least a vague interest in. I read somewhere that the winner of a solution race frequently isn't the best, rather the worst that's still Good Enough™. This ... may sadly be that. Much as I wish a better language had a stronger web presence, PHP seems to be it outside of "actual" programmers<a name="note-Sun-Jun-12-123418EDT-2011"></a>[|4|](#foot-Sun-Jun-12-123418EDT-2011).

So yeah. I'd prefer the dominating position to be held by a functional, consistent, macro-enabled (or lazy), assertion-capable (or strong-type-inferencing), namespace-enabled language without a deprecated list that includes half its functions, but PHP has floated to the top. And I'm kind of happy about that. Between its pervasiveness<a name="note-Sun-Jun-12-123509EDT-2011"></a>[|5|](#foot-Sun-Jun-12-123509EDT-2011), GPLv2 licensing of key applications and inherently open code, PHP may be doing more to promote Freedom in the Stallman sense than perhaps any other server-side language. Of course I should probably ding it for also being behind Facebook, but *maybe* it's time to put a new logo up in the title bar...


* * *
##### Footnotes

1 - <a name="foot-Sun-Jun-12-123210EDT-2011"></a>[|back|](#note-Sun-Jun-12-123210EDT-2011) - I've also seen exactly one SharePoint, which I'm told I should be thankful that I didn't have to maintain

2 - <a name="foot-Sun-Jun-12-123225EDT-2011"></a>[|back|](#note-Sun-Jun-12-123225EDT-2011) - so it supports the [four freedoms](http://www.gnu.org/philosophy/free-sw.html)

3 - <a name="foot-Sun-Jun-12-123259EDT-2011"></a>[|back|](#note-Sun-Jun-12-123259EDT-2011) - or [whatever](http://nginx.org/) [server](http://www.lighttpd.net/) [you use](http://www.hiawatha-webserver.org/)

4 - <a name="foot-Sun-Jun-12-123418EDT-2011"></a>[|back|](#note-Sun-Jun-12-123418EDT-2011) - who are a minority as evidenced by the [numbers](http://www.scriptol.com/cms/popularity.php); WordPress, Joomla and Drupal between them have truly intimidating market share

5 - <a name="foot-Sun-Jun-12-123509EDT-2011"></a>[|back|](#note-Sun-Jun-12-123509EDT-2011) - in terms of deployment and number of "native speakers"
