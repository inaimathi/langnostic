For the record, I have no idea how I got here.

Starting from a Windows-using Graphic Design graduate three years ago (who incidentally really really didn't want to learn about PHP and databases because it seemed too complicated), and ending up as a Netbook-toting, Linux-using, command-line slinging Scheme hacker who uses Emacs and Conkeror as his editor and browser respectively.

Which is not to say that I don't draw anymore. In fact (perhaps ironically?) I draw better now than I ever have ever. It actually took dropping comics to get further, which was fucking unexpected to say the least. Not in the sense of "grownups don't draw comics", in the sense that my mind needed change. Had I spent my entire childhood gleefully slinging oil-paints around, I'd need a change from that too. In the same way that it doesn't do to be an [X programmer](http://www.oreillynet.com/ruby/blog/2006/03/interviewing_ruby_programmers.html), it doesn't do to be an X-artist. You don't even see it while you are an X-artist, because it doesn't occur to you to think outside of your own experience, but it's amazing how constricting habits can start to feel after a while. Ironically, you'll only ever recognize the discomfort after you stretch your wings. 

I know roughly ten billion times more than I did back when I was bitching about how JavaScript was too hard, and it's getting to the point where I'm realizing how little that actually is. I'm remembering incidents from not two years ago where I didn't even think to question a colleague of mine when he insisted that Photoshop actions were the best way of automating image-resizing (incidentally, [they're not](http://www.imagemagick.org/script/index.php)). I remember more recent arguments, still from the old office, about how we shouldn't use [git](http://git-scm.com/) because "there's nothing wrong with Subversion", or wondering how to programmtically generate buttons or CSS. Those don't seem like real problems anymore. I'd take a couple days to resolve each. At the outside.

After growing by leaps and bounds, I want to go farther. As weird and intense as the trip so far has been, I want to run faster. And I still think I can do it. Which gives me this eery feeling that I just don't know enough yet.

# Conkering

On a completely different note, I started using a new browser, and it's fucking awesome.

I've sort of been looking for a browser to replace Chrome for a while now. It's good, and it maximizes screen real-estate (which is important because I use a [certain netbook](http://www.laptopmag.com/review/laptops/ocz-neutrino.aspx) very often, and you want every milimeter of a 10 inch screen contributing). A while ago, Firefox started to bother me. I noticed that between the various bars and tool dealies they had, my browsing real-estate was reduced to unacceptably low levels. I switched to [Chrome](http://www.google.com/chrome) because it has exactly one line of tabs at the top (your favorites can be toggled with Ctrl-B, so you have them when you need 'em, and not otherwise). A few bugs started burning pretty badly lately, though. The number 1 being that (ironically) Chrome randomly fails to log into Gmail on Linux. 

So here were my requirements:

1. Maximized screen real-estate
2. Intuitive shortcuts
3. Cross-platform

And last (but I really wasn't too hopeful here)

4. Decent keyboard browsing support. (so I could avoid using the trackpad)

First thing I looked at was [W3 mode](http://www.gnu.org/software/w3/), because I've become quite the Emacs ardent since [first discovering it](http://defpackage.blogspot.com/2009/07/emacs.html). It's really not acceptable. It works ok if you just browse text-based sites, but most of the stuff I browse isn't text-based. I also discovered that I had additional requirements that hadn't been apparent earlier.

3b. It should be sufficient for testing websites/webapps
3c. It should handle graphic browsing

I also tried Konqueror, actually installing KDE on my netbook before realizing that it's not what I'm after. It has some good ideas, but it's heavy on the chrome, and I prefer my computer be fast rather than look cool. As I was searching around for Konqueror tutorials, trying to figure out whether I can at least use it for my desktop, I stumbled across [a page mentioning Conkeror](http://bc.tech.coop/blog/060603.html).

Firefox for Emacs users

...

Go on...

It turns out that [Conkeror](http://conkeror.org/) is a JavaScript-based, Emacs-like browser based on the [XULRunner engine](https://developer.mozilla.org/en/XULRunner). 

It's a clear win.

1. Check. It uses the whole screen (no address-bar, no favorites, no bullshit, just the familiar mini-buffer at the bottom).
2. Check. I realize they wouldn't be intuitive for everyone, but I already love and use Emacs, so it was a very easy transition to make. Two days in, it's as if I've been using it for years.
3. Check. Based on the XULRunner engine, and it runs on Windows, OS X and Linux (it even has apt-get support on Ubuntu).
3b. Check. It's as good as Firefox, and may actually be better for [development/testing](http://conkeror.org/WebDevelopment) because it gives you full access to the DOM via JavaScript hooks.
3c. Check.
4. Oh you bet that's a Check. It chugs a bit when there's six billion links on a page, but otherwise fine.

So yeah. I'll be brushing up on my JavaScript a bit more because I now have reason to. I doubt I'll be customizing my browser anywhere near as much as my editor, but I've already made some edits for the sake of sanity.

```javascript
minibuffer_auto_complete_default=true;
url_completion_use_bookmarks=false;
url_completion_use_history=true;
xkcd_add_title = true;

define_key(content_buffer_normal_keymap, "N", "follow-new-buffer");
define_key(content_buffer_normal_keymap, "V", "view-source");

define_key(default_base_keymap, "C-c", "cmd_copy");
define_key(default_base_keymap, "C-v", "cmd_paste");
define_key(default_base_keymap, "C-w", "kill-current-buffer");

define_webjump("youtube", "http://www.youtube.com/results?search_query=%s&aq=f");
define_webjump("stock", "http://www.sxc.hu/browse.phtml?f=search&txt=%s&w=1&x=0&y=0");
define_webjump("lang", "http://langnostic.blogspot.com");
define_webjump("plt", "http://docs.racket-lang.org/search/index.html?q=%s");
```

It's more verbose than the comparable code would be in Elisp, but there probably won't be as much of it total. One completely unexpected cool thing about this is that I now use my keyboard for everything, except starting Emacs and Conkeror. I'm definitely starting to get the attitude of "mouse use=cache miss", and I find myself wanting to eliminate it as much as possible. To that end, I'll be trying out [Stumpwm](http://www.nongnu.org/stumpwm/) when I get a little free time, just to eliminate those first two mouse clicks. Xmonad is tempting, but I'm really not sure I want to cram Haskell into my head at this point just to run a window manager. I already have Scheme, JavaScript, Elisp, PHP, Erlang, Python and Ruby competing for mindshare, and Common Lisp seems like it'd fit more comfortably. I still want to learn Haskell eventually, but I need some breathing room first. I'm sure there's a mental-indigestion point that I'm approaching, and I really don't want to spend a few weeks on the metaphorical shitter.
