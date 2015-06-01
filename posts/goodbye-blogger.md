This has actually been a long time coming, with or without [this garbage](http://langnostic.blogspot.com/). The point at which I could write my own blog server from scratch in an afternoon or two was passed long ago, and I'd similarly long ago run into the situation where I'd found myself apologizing for using Google services. Mostly because of things like YouTube prompting you to use your real name, and the forcible pushing of G+, and "One account. All of Google."

So whether or not they decided to lock that blog down for non-specific terms-of-service violations, I was eventually going to set up a site on my own box, at my own domain that did precisely what I wanted a blog to do. The lock just sort of forced my hand.

The starting point was when I jumped over to my blog to find a link to [X in Y](https://github.com/adambard/learnxinyminutes-docs) from my sidebar. Only to instead find a Firefox Attack Page warning telling me about an image that was causing some issues<a name="note-Wed-May-28-112820EDT-2014"></a>[|1|](#foot-Wed-May-28-112820EDT-2014). So I hopped onto the various contact links and tried to see what was going on. First, with the [project maintainers](https://www.stopbadware.org/) of the warning modules, which yielded this conversation:

> Me: May 21 (7 days ago)
> To: contact@stopbadware.org
> I just saw one of your warnings on a blogspot.com site (specifically langnostic.blogspot.com, which I know to lack malware as I administer it). Not sure if this is a false positive or an oversight of some sort on my part. I'm not about to go through the de-blacklist process for blogspot.com, but wanted to let you know about it.

> StopBadware Team: May 21 (7 days ago)
> To: me
> Thank you for contacting StopBadware. Answers to many common questions, including how to remove warnings about badware on your site, may be found in our FAQ at https://www.stopbadware.org/faq.
> If your message requires an answer that is not provided by the FAQ, we will try our best to be in touch soon. You may also find assistance in our online community at https://www.BadwareBusters.org.
> Regards,
> The StopBadware team
> --
> StopBadware is a 501(c)3 non-profit organization based in Cambridge, Massachusetts, USA. We are supported by individual donors and partners such as Google, Mozilla, Qualys,and Yandex. For more information, visit https://www.stopbadware.org.

> StopBadware Team: May 21 (7 days ago)
> To: me
> Hi Leo,
> StopBadware doesn't make warnings or have a list of bad sites. The warnings link to us because we're a nonprofit organization that helps website owners figure out how to clean up hacked sites. Google is the company blacklisting your blog for malware. Here are their diagnostics: http://google.com/safebrowsing/diagnostic?site=http://langnostic.blogspot.com.
> The Safe Browsing page indicates that "bikereviews. com" is the domain functioning as an intermediary for distributing malware to your visitors. We ran a quick check on your blog using a free, open tool maintained by one of our community forum moderators (his code name is Redleg). Here's the tool: http://aw-snap.info/blogger-tool/. 
> And here's the output from the tool: http://aw-snap.info/blogger-tool/?tgt=http%3A%2F%2Flangnostic.blogspot.com. A search for "bikereviews. com" (space added) reveals that your blog has what looks to be a JPG image sourced from bikereviews. com, starting on line 1381. That site was probably hacked to serve malware and your blog was likely roped into that distribution process. 
> After you clean that off your site, here are directions on removing Google's malware warnings:
> Via Google directly (requires that you add and verify your site in Webmaster Tools): https://support.google.com/webmasters/answer/168328
> Via StopBadware's independent review process: https://www.stopbadware.org/blacklisted-by-google
> Hope that helps!
> The StopBadware team

> Me: May 21 (7 days ago)
> To: StopBadware
> It did. Thanks for your response. It turns out that I had picked the "from the web" option to post an image a few posts ago (the lines you highlighted using aw-snap.info/blogger-tool/; thanks for pointing me in the right direction, that saved some time), and that sets up a link to the original domain rather than downloading the target and serving it from blogspot.com. My bad; I should have checked that at the time.
> I've fixed the problem and submitted an automated review request through Google.

> StopBadware Team: May 21 (7 days ago)
> To: me
> Great! We hope you don't have any further issues. 
> Cheers,
> The StopBadware team


So the result of that was identifying my mistake and fixing it. Instead of hotlinking, I downloaded, then uploaded the picture of that bike lock onto blogger, so that it would be served from a `blogger` domain. Then I asked for a review of my blog, whereupon it was locked as authors-only, *in addition* to being flagged as an attack page.

![](/static/img/locked-blog.png)

Fantastic.

So I hit up Bloggers' support forum where I had [this exchange](https://productforums.google.com/forum/#!msg/blogger/rTbpSj30Y8Q/8CXXbmUGj_sJ) with a polite and relatively prompt gentleman who nevertheless failed to be of any help at all.

You might be asking why I didn't just click on this "Request Unlock Review" button in my blogger account

![](/static/img/unlock-review.png)

It's because when I click it, I get this

![](/static/img/verify-account.png)

And, no, a company that has this level of customer service, along with a noted anti-privacy bias doesn't get my permission to find out my phone number.

The blog is toast, the only way of reviving it seems to be to spend more frustrating hours dicking around with Google's automated systems and/or explicitly giving them more of my personal information. And I can write code faster than that. And I enjoy writing code. And the code would be Lisp.

So [here we are](https://github.com/Inaimathi/langnostic).

Now the only thing I have to do is figure out what I'm doing to get the hell away from gmail.


* * *
##### Footnotes

1 - <a name="foot-Wed-May-28-112820EDT-2014"></a>[|back|](#note-Wed-May-28-112820EDT-2014) - This was a picture of a bike lock that I had hotlinked from the manufacturer's site. No, I don't feel bad about it, my blog never got anywhere near enough views to make this an issue.
