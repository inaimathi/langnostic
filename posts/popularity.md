> **EDITOR'S NOTE:**  
> *This is a piece I wrote about three years ago, then sat down to copy-edit and polish up today. It was very focused on a few particular, topical anecdotes that my then-co-workers were throwing about. Having read over it equipped with three extra years of hindsight, I'm not sure I'd still be quite as vehement about my opinions as I was here, but don't disagree on any large points.*  
> Sun, 10 Nov, 2013  

I'm writing this post because I've seen one too many comments of the sort "Well, if PHP is so shitty, then why is it so popular?". Typically this is the main claim in a rebuttal to "PHP is a shit language"<a name="note-Sun-Nov-10-173728EST-2013"></a>[|1|](#foot-Sun-Nov-10-173728EST-2013), and the end result seems to be that a lot of people just sit back and think "Oh yeah, I guess it is popular. It must not be shit, but rugged." Substitute  "manly", or "quirky" or similar if you like. As in, yeah, it has flaws, but they're endearing flaws. Like [`mysql_escape`](http://us2.php.net/mysql_escape_string) being the deprecated precursor to a different function named [`mysql_real_escape_string`](http://php.net/manual/en/function.mysql-real-escape-string.php) (itself also deprecated). And [static functions having an odd interpretation of "this"](http://bugs.php.net/bug.php?id=12622). And the complete list of [deprecated functions](http://stackoverflow.com/questions/6822446/what-does-php-do-with-deprecated-functions) being longer than my average blog post thanks to the fact that they never actually get removed.

I don't care that we've known how to do better language design for at least thirty years, PHP iss Rugged©™, goddamit! And it's still more popular than whatever properly designed tool it is that you use!

### <a name="how-do-technologies-get-popular" href="#how-do-technologies-get-popular"></a>How do technologies get popular?

This probably won't turn into an article where I mention Common Lisp<a name="note-Sun-Nov-10-173736EST-2013"></a>[|2|](#foot-Sun-Nov-10-173736EST-2013), by the way. The idea of language popularity is almost completely irrelevant to what I'm discussing here; this could be a discussion of any sort of tool and how you'd go about picking one for a particular purpose. The source of popularity is obviously difficult to determine because relatively few things get there, so lets back off a little bit.

### <a name="what-does-it-mean-for-a-tool-to-be-popular" href="#what-does-it-mean-for-a-tool-to-be-popular"></a>What does it mean for a tool to be popular?

Does it have to solve some specific problem better than other tools? Does it need to be better/cheaper/faster than solving that particular problem by hand? Does it need to be more fun or easier to use? Does it have better marketing/sales/promotions than the competition? Is it the first tool to solve a problem sufficiently well?

No. To all of the above.

A tool is popular when enough people have chosen it to perform a given task. Any of the above points **contribute to** a tool getting chosen, but for each, you can find a large number of counterexamples. Both tools that lacked it and became popular, and tools that had it but went nowhere. So no single element of that list of points is going to make or break you.

Lets look at it from the other side instead though.

### <a name="what-does-it-take-for-someone-to-choose-a-given-tool" href="#what-does-it-take-for-someone-to-choose-a-given-tool"></a>What does it take for someone to choose a given tool?

That's a simpler question, but it should get us the same answer. If "popularity" is "being chosen by enough people" then figuring out "how do most people choose" should tell us "what it means to be popular".

A big reason to choose a tool<a name="note-Sun-Nov-10-173739EST-2013"></a>[|3|](#foot-Sun-Nov-10-173739EST-2013) is that **it'll get you a job**. Again, this has nothing to do with language choice. Lots of people claim "I learned `[x]` to get a job", and `[x]` can be "Java" or "C#" with the same probability as "MS Word", "Photoshop", "Wordpress", "typing", "cooking" or "how to drive a bus". So one reason people choose is to get a job. Before we drill down to the next level, any other reasons?

**For fun**. I know about as many people who paint/design professionally as those who just do it on the weekend to relax<a name="note-Sun-Nov-10-173742EST-2013"></a>[|4|](#foot-Sun-Nov-10-173742EST-2013), and I know plenty of people who just plain like to program. For fun. Like, on [nights and weekends](https://bentomiso.com/events). Granted, not everyone works this way, and not every tool has this effect on people to the same degree<a name="note-Sun-Nov-10-173748EST-2013"></a>[|5|](#foot-Sun-Nov-10-173748EST-2013), but it's still one possible reason.

Fitness of purpose maybe? Well, not in practice, no. Fitness of purpose is how you pick a specific class of tool. Which is to say, that's how you know you want a rotary cutter as opposed to a reciprocating saw or a pen and not a sable brush. It still doesn't tell you that you want a Dremel as opposed to a DeWalt, or a Pilot instead of a Bic<a name="note-Sun-Nov-10-173814EST-2013"></a>[|6|](#foot-Sun-Nov-10-173814EST-2013), or ahem, a [Lua](http://www.lua.org/) instead of a [PHP](http://ca.php.net/). It's also not as high a bar as people might think. I try to be objective about it, but from observation, most people tend to treat "fitness of purpose" as "what tool do I currently know how to use that could sort of be put to this use?" rather than "what is the most effective tool for the problem I'm solving?"

**Popularity** is the only other big reason I can think of that tools get chosen, but I don't want to recur just yet, so we'll leave that one alone. Back up a bit.

## <a name="tools-are-popular-if-theyll-get-you-a-job" href="#tools-are-popular-if-theyll-get-you-a-job"></a>Tools are popular if they'll get you a job.

When will a tool get you a job? Well, when enough employers start putting it on their job listings. Until that point, it's not worth learning it *just* for that. Tools before that point mostly get adopters that come by because it's fun for them or the tiny minority that have performed a sufficient comparison and found *that* tool to be the best fit for them out of the ones they compared. In other words *"I choose tools that will get me jobs"* translates to *"I choose tools that employers choose"*.

## <a name="so-how-do-employers-choose-tools" href="#so-how-do-employers-choose-tools"></a>So how do employers choose tools?

Well, here, I can actually share some small amount of real-world data. Anecdotal, so take it with a grain of salt, but enough to form a theory. If anyone wants to try being the experimentalist on this one, be my guest. If you did it well enough, I'm sure it'd be publishable.

There are a few major points that impact on what an organization does in terms of technical tool choice<a name="note-Sun-Nov-10-173817EST-2013"></a>[|7|](#foot-Sun-Nov-10-173817EST-2013).

The biggest one is *"We'll keep using what we're using"*. Which is to say, if the previous project turned out to be successful, there will be a big push to use the same tools on the next one. Interestingly, this happens even if the success of the last project had everything to do with the team pulling constant overtime, and nothing at all to do with tool choice. The tools can be actively detrimental to the goal and *still* reap a rep-boost if the project succeeds. This doesn't really answer anything. How does a company choose their tools on the first project?

A few different ways. The tool choice for Project Number 1™© is contingent on<a name="note-Sun-Nov-10-173820EST-2013"></a>[|8|](#foot-Sun-Nov-10-173820EST-2013)


1.   What do our developers know how to use?
1.   What do our vendors use?
1.   What do our clients want us to use?
1.   What's the industry standard?


The first one at least has some expert input, but works oddly. You don't get choice bias towards the "best"<a name="note-Sun-Nov-10-173836EST-2013"></a>[|9|](#foot-Sun-Nov-10-173836EST-2013) tool, but rather the most popular. If Bleeb is "better" than Blub, but only two people on a team of ten know Bleeb whereas everyone knows Blub, then the team uses Blub<a name="note-Sun-Nov-10-173841EST-2013"></a>[|10|](#foot-Sun-Nov-10-173841EST-2013). In other words, no help there; this criteria will get you the popular language without requiring any level of quality or rigor in its design principles.

The second one is just plain odd, and before sitting back and observing, I would have sworn that it would be a really weak reason to use a tool. Companies seem to not care though; if a given preferred vendor uses tool `[x]` for task `[y]`, then the company tends to use tool `[x]`, even if it's ridiculously awkward to *actually* use. The vendor is also a company, so they use this same process for picking their tools, so substitute that back once we're done.

The third one is obvious, I hope, but it also boils down to "popularity" because very few clients know the problem space. Typically, they listen to the first/best sales people that talk to them. They're a force though; if your target client wants it on MSSQL and .NET, then it'll either be *that* or it won't *be*.

The fourth one is the previous answer on a macro scale; *"We'll keep using what we're using (as an industry)"*. In other words, if there were lots of successful companies using tool `[x]`, we'll use it too<a name="note-Sun-Nov-10-173852EST-2013"></a>[|11|](#foot-Sun-Nov-10-173852EST-2013).

*Now*, we need to go deeper<a name="note-Sun-Nov-10-173855EST-2013"></a>[|12|](#foot-Sun-Nov-10-173855EST-2013).

### <a name="how-does-the-first-company-in-an-industry-pick-their-tools" href="#how-does-the-first-company-in-an-industry-pick-their-tools"></a>How does the first company in an industry pick their tools?

Regardless of any other decision factors, the answer is almost by definition "before they really know what problems those tools will have to solve", and I've already discussed that one [pretty thoroughly](http://langnostic.blogspot.ca/2012/02/self-titled.html). There are no clients, so they can't pick that way, there are no other companies or vendors so there is no industry standard. They might look at what similar industries do. Would they use the best possible tool for the job though? Well, no. They're likely to go the *"What do our developers know how to use?"* route, which we've already discussed above.

The big reasons **teams**<a name="note-Sun-Nov-10-173906EST-2013"></a>[|13|](#foot-Sun-Nov-10-173906EST-2013) pick a given technology include


-   how easy is it to hire people that know how to use this? (easier the better)
-   do we have existing code that we'll need to interface with? (if so, weigh whatever we used on that project favorably)
-   have we used any technologies in the past for similar purposes to what we're doing here? (if so, weigh those favorably)


and the big one


-   if I choose this, and anything blows up, will I still be able to make the case to non-technical humans that it was the right choice? (if not, weigh it very unfavorably)


1, 2 and 3 mean that the more popular the language is, the more chance it has of getting entrenched<a name="note-Sun-Nov-10-173915EST-2013"></a>[|14|](#foot-Sun-Nov-10-173915EST-2013). 4 means the same, but this time "popular" means overall, not just within the tech community. A non-tech has heard the name "PHP" before, enough times to associate it with "the web" and "Facebook" and "Wordpress", but probably hasn't looked into it closely enough to catch complaints from developers<a name="note-Sun-Nov-10-173918EST-2013"></a>[|15|](#foot-Sun-Nov-10-173918EST-2013).

The end result is that, in a sufficiently large company, it's safer to use a popular tool that's poor in the technical sense than it is to use an excellent tool no one's heard of. And that's *also* been [discussed thoroughly](http://paulgraham.com/avg.html), and [this time](http://www.joelonsoftware.com/items/2006/09/01.html) it wasn't even by me. The decision is made purely on the basis of popularity once again.

### <a name="shit" href="#shit"></a>Shit

We just bottomed out our recursion. Just in case you haven't been keeping score, literally every single level at which a tool can be selected is likely to be filled by the most popular tool in some context, and this popularity never requires, therefore never implies, *anything other than popularity*. One more time: **at no point in the process of selecting a toolkit do most choosers even try to see whether it's shit or not.** So I don't care how popular your steaming pile of imperative, counter-intuitive security-exploits-waiting-to-happen is; it's still shit.

Don't let me stop you from eating it, but I remember what it tastes like so I won't be joining you. Or shaking your hand afterwards.

* * *
##### Footnotes

1 - <a name="foot-Sun-Nov-10-173728EST-2013"></a>[|back|](#note-Sun-Nov-10-173728EST-2013) - Which I happen to agree with, actually. If you're looking for details on what language**s**(Spoiler warning) I'd recommend learning, you're better off reading [this](http://langnostic.blogspot.ca/2013/03/what-programming-language-should-i-learn.html) instead.

2 - <a name="foot-Sun-Nov-10-173736EST-2013"></a>[|back|](#note-Sun-Nov-10-173736EST-2013) - Except for that.

3 - <a name="foot-Sun-Nov-10-173739EST-2013"></a>[|back|](#note-Sun-Nov-10-173739EST-2013) - Generally among the people I know at least, no judgment here.

4 - <a name="foot-Sun-Nov-10-173742EST-2013"></a>[|back|](#note-Sun-Nov-10-173742EST-2013) - That's what I get for going to design school.

5 - <a name="foot-Sun-Nov-10-173748EST-2013"></a>[|back|](#note-Sun-Nov-10-173748EST-2013) - Point of fact, only one person I know drives a bus for fun. It's been his obsession to work for the TTC since grade 8. I haven't heard from him in a while, but I still remember his room being full of papercraft Orion 3s, and I'm pretty sure he spent every internship opportunity he had on some streetcar route or another.

6 - <a name="foot-Sun-Nov-10-173814EST-2013"></a>[|back|](#note-Sun-Nov-10-173814EST-2013) - The pen fanciers among you are probably ready to tell me that these are the worst possible examples; they're just the most popular common brand pens, not the really good stuff, where quality can make a difference. Really, I should have used `foo` and `bar`. You can go now, if you ponder that point hard enough, you pretty much got the gist of the article.

7 - <a name="foot-Sun-Nov-10-173817EST-2013"></a>[|back|](#note-Sun-Nov-10-173817EST-2013) - I'm reigning it in a bit to software tools because that's what I have experience with, but this still seems like it might be a general trend; again, experimentalists welcome.

8 - <a name="foot-Sun-Nov-10-173820EST-2013"></a>[|back|](#note-Sun-Nov-10-173820EST-2013) - In varying order, in my experience, but always on these things.

9 - <a name="foot-Sun-Nov-10-173836EST-2013"></a>[|back|](#note-Sun-Nov-10-173836EST-2013) - Which I'm still quoting. In a book, that's called "foreshadowing". In a game or movie, it's called "setting up the sequel".

10 - <a name="foot-Sun-Nov-10-173841EST-2013"></a>[|back|](#note-Sun-Nov-10-173841EST-2013) - And you get a varying amount of childish name-calling and dismissiveness towards Bleeb.

11 - <a name="foot-Sun-Nov-10-173852EST-2013"></a>[|back|](#note-Sun-Nov-10-173852EST-2013) - Again, disregarding whether the tool had any effect at all on success.

12 - <a name="foot-Sun-Nov-10-173855EST-2013"></a>[|back|](#note-Sun-Nov-10-173855EST-2013) - And at this point, all bets are off, I'm just theorizing, because I haven't observed the decision making process in an industry-defining company. That would be an interesting research project though, let me know if you've got one lined up.

13 - <a name="foot-Sun-Nov-10-173906EST-2013"></a>[|back|](#note-Sun-Nov-10-173906EST-2013) - Not individual developers, but groups of corporate developers complete with leaders, technical or not, who are ultimately responsible to non-tech people further up the hierarchy.

14 - <a name="foot-Sun-Nov-10-173915EST-2013"></a>[|back|](#note-Sun-Nov-10-173915EST-2013) - And note that both points are completely unrelated to how "good" a language is, and entirely dependent on how popular it is.

15 - <a name="foot-Sun-Nov-10-173918EST-2013"></a>[|back|](#note-Sun-Nov-10-173918EST-2013) - Or to determine whether there's a lot of overlap between "good developers" and "developers who complain vocally about PHP".
