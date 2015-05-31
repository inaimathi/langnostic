Dear Mr. Spolsky,

I've been a big fan of your blog while you were updating it regularly. It was a very good collection of essays on managing a software enterprise from the Windows/shrinkwrap perspective (the second one is actually a distinction I learned from reading your excellent [Five Worlds](http://www.joelonsoftware.com/articles/FiveWorlds.html) piece which still rings true for me).

During a recent spare moment, I re-read [The Joel Test](http://www.joelonsoftware.com/articles/fog0000000043.html), and seriously considered how closely an organization that scores between 8 and 12 would resemble a place where I'd like to work. A few bugs emerged; 


-   I would *not* enjoy working at a place that still insists on CVS (SVN would be acceptable if for no other reason than [git-svn](http://www.kernel.org/pub/software/scm/git/docs/git-svn.html) exists and is bi-directional)
-   "The Best Tools Money Can Buy", in the sense of hardware, isn't really necessary (in other circumstances, I'd argue that it's unnecessary in the software sense too, because I'm unsure money can buy the best software tools, but I'll put that down to my bias as a Linux user and say no more about it)
-   Specs and schedules aren't necessarily the best way to do things. Both words also have a lot of baggage and history, and as a result, some organizations may claim a point or two here even if they're doing it wrong, while others might be penalized even though they adhere to the principles explained in the associated other articles you've written.


That said, I humbly submit the following patch for your consideration. Hopefully, I'm not wrong about all of it (if nothing else, I welcome a counter-argument and explanation of why I'm wrong).

Yours

-Inaimathi

========================================


```diff
diff -u joel-test.html joel-test-edits.html
--- joel-test.html   2011-07-20 10:28:40.000000000 -0400
+++ joel-test-edits.html  2011-07-20 11:05:06.000000000 -0400
  @@ -5,14 +5,14 @@

The Joel Test

-Do you use source control?
+Do you use distributed source control?
Can you make a build in one step?
Do you make daily builds?
Do you have a bug database?
Do you fix bugs before writing new code?
-Do you have an up-to-date schedule?
+Do you have an up-to-date schedule or work queue?
-Do you have a spec?
+Do you have a minimal spec?
-Do you use the best tools money can buy?
Do programmers have quiet working conditions?
Do you have testers?
Do new candidates write code during their interview?
Do you do hallway usability testing?
@@ -24,8 +24,8 @@
Of course, these are not the only factors that determine success or failure: in particular, if you have a great software team working on a product that nobody wants, well, people aren't going to want it. And it's possible to imagine a team of "gunslingers" that doesn't do any of this stuff that still manages to produce incredible software that changes the world. But, all else being equal, if you get these 12 things right, you'll have a disciplined team that can consistently deliver.
-1. Do you use source control?
-I've used commercial source control packages, and I've used CVS, which is free, and let me tell you, CVS is fine. But if you don't have source control, you're going to stress out trying to get programmers to work together. Programmers have no way to know what other people did. Mistakes can't be rolled back easily. The other neat thing about source control systems is that the source code itself is checked out on every programmer's hard drive -- I've never heard of a project using source control that lost a lot of code.
+1. Do you use distributed source control?
+I've used commercial source control packages, and I've used Mercurial, which is free, and let me tell you, any of the free tools (Mercurial, Git, Bazaar) are fine. But if you don't have source control, you're going to stress out trying to get programmers to work together. Mistakes can't be rolled back easily. The other neat thing about distributed source control systems is that the source code itself is checked out on every programmer's hard drive -- I've never heard of a project using distributed source control that lost any code, or lost any time as a result of a poor network connection to the office.
2. Can you make a build in one step?
By this I mean: how many steps does it take to make a shipping build from the latest source snapshot? On good teams, there's a single script you can run that does a full checkout from scratch, rebuilds every line of code, makes the EXEs, in all their various versions, languages, and #ifdef combinations, creates the installation package, and creates the final media -- CDROM layout, download website, whatever.
@@ -82,7 +82,7 @@
Another great thing about keeping the bug count at zero is that you can respond much faster to competition. Some programmers think of this as keeping the product ready to ship at all times. Then if your competitor introduces a killer new feature that is stealing your customers, you can implement just that feature and ship on the spot, without having to fix a large number of accumulated bugs.
-6. Do you have an up-to-date schedule?
+6. Do you have an &lt;b>up-to-date&lt;/b> schedule or work queue?
Which brings us to schedules. If your code is at all important to the business, there are lots of reasons why it's important to the business to know when the code is going to be done. Programmers are notoriously crabby about making schedules. "It will be done when it's done!" they scream at the business people.
Unfortunately, that just doesn't cut it. There are too many planning decisions that the business needs to make well in advance of shipping the code: demos, trade shows, advertising, etc. And the only way to do this is to have a schedule, and to keep it up to date.
@@ -91,7 +91,9 @@
Keeping schedules does not have to be hard. Read my article Painless Software Schedules, which describes a simple way to make great schedules.
+A work queue is the exact same thing, but without the mental baggage that people bring to the table when discussing scheduling (I prefer to ask this one, because it's inherently up-to-date, and people who make a schedule at the beginning of the project then never look at it again tend to mentally skip over "up-to-date" and just answer "yes"). Please read the [Painless Software Schedules](http://www.joelonsoftware.com/articles/fog0000000245.html) linked above. In fact, here, have another [link](http://www.joelonsoftware.com/articles/fog0000000245.html). The point of the exercise, whether it's a schedule or work queue is that it's something you update each day to reflect the current state as honestly and recently as possible.
-7. Do you have a spec?
+7. Do you have a minimal spec?
Writing specs is like flossing: everybody agrees that it's a good thing, but nobody does it. 
I'm not sure why this is, but it's probably because most programmers hate writing documents. As a result, when teams consisting solely of programmers attack a problem, they prefer to express their solution in code, rather than in documents. They would much rather dive in and write code than produce a spec first.
@@ -100,6 +102,10 @@
My pet theory is that this problem can be fixed by teaching programmers to be less reluctant writers by sending them off to take an intensive course in writing. Another solution is to hire smart program managers who produce the written spec. In either case, you should enforce the simple rule "no code without spec".
+Don't take this to mean that you should mandate several hundred page documents before any code is done; the spec needs to be just detailed enough that someone other than the writer can correctly understand what's being built. If you keep it minimal, you'll also keep yourself out of the temptation to use the spec as a club against your customers (whether internal or external).
+
+An alternative to the spec is a requirements database to go with that bug tracking database that you need. The point here is to make sure your spec can grow and be read by anyone who needs it any time, avoid the "spec-as-club" and "feature-vs-bug" arguments while still collapsing the waveform. You do *not* want to work on Schrödinger's project. Ever.
+
Learn all about writing specs by reading my 4-part series.
8. Do programmers have quiet working conditions?
@@ -117,25 +123,14 @@
Now let's move them into separate offices with walls and doors. Now when Mutt can't remember the name of that function, he could look it up, which still takes 30 seconds, or he could ask Jeff, which now takes 45 seconds and involves standing up (not an easy task given the average physical fitness of programmers!). So he looks it up. So now Mutt loses 30 seconds of productivity, but we save 15 minutes for Jeff. Ahhh!
-9. Do you use the best tools money can buy?
-Writing code in a compiled language is one of the last things that still can't be done instantly on a garden variety home computer. If your compilation process takes more than a few seconds, getting the latest and greatest computer is going to save you time. If compiling takes even 15 seconds, programmers will get bored while the compiler runs and switch over to reading The Onion, which will suck them in and kill hours of productivity.
-
-Debugging GUI code with a single monitor system is painful if not impossible. If you're writing GUI code, two monitors will make things much easier.
-
-Most programmers eventually have to manipulate bitmaps for icons or toolbars, and most programmers don't have a good bitmap editor available. Trying to use Microsoft Paint to manipulate bitmaps is a joke, but that's what most programmers have to do.
-At my last job, the system administrator kept sending me automated spam complaining that I was using more than ... get this ... 220 megabytes of hard drive space on the server. I pointed out that given the price of hard drives these days, the cost of this space was significantly less than the cost of the toilet paper I used. Spending even 10 minutes cleaning up my directory would be a fabulous waste of productivity.
-
-Top notch development teams don't torture their programmers. Even minor frustrations caused by using underpowered tools add up, making programmers grumpy and unhappy. And a grumpy programmer is an unproductive programmer.
-
-To add to all this... programmers are easily bribed by giving them the coolest, latest stuff. This is a far cheaper way to get them to work for you than actually paying competitive salaries!
-
+&lt;!-- I've been doing research on what I want for my next development machine, and come to the conclusion that I don't need anywhere near "The Best Tools Money Can Buy" to do development work anymore. Give bonus points if each of the developers actually have 16 cores, 64GB of ram, and multiple terabytes worth of SATA3 SSDs in their desktops, but you can get a [damn fine machine for well under $3000](http://shop.lenovo.com/SEUILibrary/controller/e/webca/LenovoPortal/en_CA/catalog.workflow:category.details?current-catalog-id=12F0696583E04D86B9B79B0FEC01C087&current-category-id=F074AD2090D2080CC2F15C0005A1FD1F) (less if you want to hand your devs desktops), which wasn't exactly true back in ye olde yeare 2000e. If you have a choice between sending annoying emails to your developers or buying $400 worth of additional hard drive space, you should obviously save your fellows the trouble, but don't ding your organization for failing to allocate an annual $14 000 per developer budget for machines. -->
-10. Do you have testers?
+9. Do you have testers?
If your team doesn't have dedicated testers, at least one for every two or three programmers, you are either shipping buggy products, or you're wasting money by having $100/hour programmers do work that can be done by $30/hour testers. Skimping on testers is such an outrageous false economy that I'm simply blown away that more people don't recognize it.
Read Top Five (Wrong) Reasons You Don't Have Testers, an article I wrote about this subject.
-11. Do new candidates write code during their interview?
+10. Do new candidates write code during their interview?
Would you hire a magician without asking them to show you some magic tricks? Of course not.
Would you hire a caterer for your wedding without tasting their food? I doubt it. (Unless it's Aunt Marge, and she would hate you forever if you didn't let her make her "famous" chopped liver cake).
@@ -144,9 +139,11 @@
Please, just stop doing this. Do whatever you want during interviews, but make the candidate write some code. (For more advice, read my Guerrilla Guide to Interviewing.)
-12. Do you do hallway usability testing?
+11. Do you do hallway usability testing?
A hallway usability test is where you grab the next person that passes by in the hallway and force them to try to use the code you just wrote. If you do this to five people, you will learn 95% of what there is to learn about usability problems in your code.
+If you want the quickest possible primer on minimal testing techniques, take a look at the second half of [Steve Krug's 2008 Business of Software talk](http://businessofsoftware.org/video_08_skrug.aspx). If you're out to do the least amount of user testing you can get away with, just take his advice.
+
Good user interface design is not as hard as you would think, and it's crucial if you want customers to love and buy your product. You can read my free online book on UI design, a short primer for programmers.
But the most important thing about user interfaces is that if you show your program to a handful of people, (in fact, five or six is enough) you will quickly discover the biggest problems people are having. Read Jakob Nielsen's article explaining why. Even if your UI design skills are lacking, as long as you force yourself to do hallway usability tests, which cost nothing, your UI will be much, much better.
Diff finished.  Wed Jul 20 11:09:46 2011
```
