Break time.

I have been researching various options for image sizing, and I do plan on finishing up the crash course sometime soon, but I saw [a thread](http://www.reddit.com/r/gnu/comments/jyh4o/with_the_proliferation_of_static_site_generators/) that sent me off thinking about something else.

Someone on Reddit linked to an article about [static site generators (and their effect on software freedom)](http://www.jeremyscheff.com/2011/08/jekyll-and-other-static-site-generators-are-currently-harmful-to-the-free-open-source-software-movement/), in which the writer posits that it would be really nice to have a Free Software competitor to [Disqus](http://disqus.com/welcome/).

Well, I'm not going to say "[delivered](https://github.com/Inaimathi/clomments)" yet, because this is the merest hint of an attempt at a solution, but I threw something together in the couple of hours I could spare between [postfix woes](http://stackoverflow.com/questions/7247861/sbcl-error-with-cl-smtp) and various marketing initiatives at [the company](http://medirexsys.com/).

I won't try to go over any of the code, that was through [the github link](https://github.com/Inaimathi/clomments), in case you missed [it](https://github.com/Inaimathi/clomments), but I want to formalize a little of my thought process on where this actually needs to go to be a real competitor, just so that I can remember when I go back to work on it this weekend.

### The Idea

Is, simply enough, to offload the comment system for a given page to a third party server. Either so that the maintainer of that page doesn't have to fart around with databases, or for that extra performance kick (since the first server no longer needs to serve up dynamic content at all), or because software as a service is in again, I really don't know.

Anyway, the point is, instead of keeping your own comment database locally, you just echo a static page with a line or two of [trixy javascript](http://api.jquery.com/jQuery.post/), and your comments get pulled in on the clients' time.

So the basic feature set here is pretty sparse:

- Track comments on a per-page basis
- Allow adding/liking/disliking/reporting of individual comments
- Allow replying to individual comments (not *strictly* necessary, but nice)

  It wouldn't be any fun if that was all though. It would also be nice if

- you could submit to [social site of choice] through one button click
- you could edit your comments
- spam sites could be tracked
- spam/low-rating comments could be omitted/hidden by default

Finally, to support the Free Software objective, it needs to:

- be AGPL (so that anyone can run their own for their friends if they feel like)
- be written in a Free language (which is no problem at all)
- allow full data exports (so that you could move pages between servers if you wanted)

This is a reasonably simple problem. Not trivial, but it looks like it would take a couple of weeks of serious work to knock out something useful. To the point that I have no idea how building a company around doing it is even possible. The only thing I can imagine is that the data storage and security around it is somehow more challenging than organizing the actual functionality (which is consistent with my observations of other software).
Well, that's that. [Kick the tires](http://clomments.inaimathi.ca/test), but don't blow my server up, and I'll do some more hacking on it later in the week.

> EDIT:
> It's been brought to my attention that people would like to get stuff running *right now*. Ok, didn't really plan for it but here goes (assuming you're on Debian)
> 1. `apt-get install sbcl mysql-server cl-sql`
> 1. `git clone https://github.com/Inaimathi/clomments.git`
> 1. create a database and user *and change the definition of `*db-spec*` in `package.lisp` to match*
> 1. install [quicklisp](http://www.quicklisp.org/beta/) *Is there still a lisper that doesn't use this? I'm getting kind of sick of mentioning it.*
> 1. `cd clomments; sbcl --eval "(ql:quickload :clomments)"` *If you're on a 64 bit machine, you may get some warnings starting up. Continue through them, and it should be fine (it's to do with cffi bindings for clsql)*
> 1. Once in SBCL
> ```lisp
> (create-view-from-class 'comment)
>  	(create-view-from-class 'page)
> ```
> 1. Go to `http://localhost:4242/test` in a browser
>   I *promise* I will streamline this as soon as I get the regulation 4 hours of sleep I'm entitled to per week.
>   Tue, 30 Aug, 2011

> EDIT:
> Also, I'm perfectly aware why there are extra spaces this time; it's because I'm starting to use regulation xhtml markup instead of relying on Blogger's seemingly flaky spacing feature. It seems like you can only set it globally for a given blog though, so I can't just switch over one post at a time. I'll need to go through my archives and wrap everything in `<p>` tags first.
> Tue, 30 Aug, 2011
