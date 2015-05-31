This is a complete non-sequitur thought that hit me recently. Ok, I guess what I mean is "non-sequitur assuming you're extrapolating from recent articles", but I promise it makes complete sense from my perspective. I've been keeping up with [Google v Oracle](http://www.groklaw.net/) recently, as well as keeping about a quarter of an eye on [certain developments in Germany](http://www.irishtimes.com/newspaper/finance/2012/0412/1224314636761.html). I'm also dealing with some minor (thankfully non-software, so it at least makes sense) patent issues at my job, and to top it all off, the last [Toronto Lisp User Group](http://www.lisptoronto.org/) meeting included some discussion of patents for a novel way of doing natural language processing (no details, since they're still pending).

Oh, heads up I guess, I'm talking about software patents this time. Now you know. Also, let me preface by making the obvious statements:


-   I am not a lawyer
-   I don't play one on TV
-   I don't even play one on the internet
-   I'm a software developer, illustrator and regular Groklaw reader, and that's almost the entirety of my experience with the field


I'm also not even remotely [the](http://progfree.org/Patents/knuth-to-pto.txt) [first](http://www.cl.cam.ac.uk/~mgk25/stallman-patents.html) [person](http://www.youtube.com/watch?v=oRqsdSARrgk) to think they're bad (and going to get a lot worse very shortly). Some people (mostly ones who own or are paid by extremely large software companies) think they're a good thing, but their arguments tend to boil down to "Patents exist, and we have them, so tough luck".

This is a thought exercise with the goal of defusing software patents. Of making them irrelevant to your actions as an inventive and commercial entity.

Lets imagine that this was your goal, and "you" are bigger than a lone, young lisper who likes to sit and write garbage at one fucking thirty AM for some bizarre reason. One way to do it would be to lobby governments for the abolition of patents. As various people point out, good luck with that. Legislative changes that slightly disadvantage existing powerful companies in order to fertilize the landscape for new competitors are bad at getting past any organization bureaucratic enough to call itself "government".

Another way would be to disregard patents utterly, and just do what you're doing. While that might do something good in large enough numbers, any individual entity has a pretty strong incentive to pay attention to legal concerns in the current landscape.

*Another* way is to make sure to make sure you never make enough money to be a juicy target and hope for the best. If there were enough inventors, a lot of them would probably be successful, by some metric, using this method.

I've been thinking of a particular way, and I'd like to pluck it out of my head to see what it looks like in the harsh-esque light of not-quite-day-yet. Essentially, what I've got in mind is a more aggressive take on the [Open Invention Network](http://www.openinventionnetwork.com/). OIN works by pooling ownership of patents and granting them without fee to anyone that promises not to use their patents against Linux. As much as I like Linux, this is not a general software patent solution, but it looks like it might be tweaked into one.

Lets imagine a similar organization that wants to grab signatories. Except this organization's goal is not merely to protect a particular kernel project, but to kill software patents<a name="note-Fri-Apr-13-012416EDT-2012"></a>[|1|](#foot-Fri-Apr-13-012416EDT-2012).

This organization wouldn't ask you not to use your software patents against Linux. It would establish a charter and ask that each signatory promise never to offensively use a software patent against any other signatory. It would ask that signatories promise not to engage in patent trolling tactics. It would also ask every signatory to promise that they would use their patents to defend any other signatory against outside patent claims. Because the point would be an obligation on each member, the organization itself wouldn't need to collect patents, it would merely need to ask for disclosures and document them. A company or individual with zero patents wouldn't need to be barred from entering, since the eventual goal is obsoleting them.

Now, lets step back for a second. Ignoring the fact that "don't troll", "don't use this patent offensively" and "come to the defense of other signatories" would be ridiculously difficult to express in legal terms<a name="note-Fri-Apr-13-012524EDT-2012"></a>[|2|](#foot-Fri-Apr-13-012524EDT-2012), what we've got looks like a best-case scenario barring the invalidation of all software patents. This is an organization that lets small patent holders huddle for warmth against larger portfolios. It allows defensive use of these tools, but heavily discourages aggressive patent litigation against signatories<a name="note-Fri-Apr-13-012548EDT-2012"></a>[|3|](#foot-Fri-Apr-13-012548EDT-2012). As more companies sign, the defensive value of the collective gets larger, putting more pressure on new and existing companies alike to join in. If it got to the point where some large percentage of patents were in the hands of the collective, it would no longer be a viable strategy to threaten signatories with software patent litigation. Effectively, they'd become dead assets<a name="note-Fri-Apr-13-012801EDT-2012"></a>[|4|](#foot-Fri-Apr-13-012801EDT-2012).

On the flipside, how would you fight such a collective? You could


-   train one of the big players on them before the defensive thicket was built up (at which point a victory would put the collective at square one, but probably invalidate a bunch of the attackers' patents too, if the Oracle case is any indication). 
-   get a shell company to sign up and then get them to start using loopholes to troll outside companies and inventors (that would either precipitate the first situation, or undermine the reputation of the collective)
-   set up a bunch of smaller trolling companies and start pinging members of the collective to bleed them out. This seems like it would be the big problem to solve. You'd do it either by getting enough of the trolls on your side somehow, or by establishing some defensive mechanism to make it easier for signatories to shrug off attacks<a name="note-Fri-Apr-13-013130EDT-2012"></a>[|5|](#foot-Fri-Apr-13-013130EDT-2012)


Of course, there are problems to deal with other than outright attacks. How do you fund something like this? The [Graham](http://paulgraham.com/ambitious.html) angle would probably be to make it a start-up, but I don't think this sort of idea is a very good fit for the approach. You could charge dues, but every obstacle you place to membership means fewer signatories, means an over-all less effective defensive thicket of patents.

How do you avoid cannibalizing (in the best case) or the patent-equivalent of a WWI (in the worst case)? Remember, we're trying to *defuse* weapons of mass destruction here. If more than one of these collectives started up and started seeing success, it would be trivial for a malicious force to play them off against one another. There would need to be a policy mechanism in place to allow different collectives to cooperate with each other while still remaining defensible against such forces.

How do you ensure that we're not just setting up another recursion? Remember, patents and copyrights alike are ostensibly out there to encourage innovation. Or at least, they were. We're now pretty clearly getting to the point that these mechanisms themselves are bigger obstacles to innovation than any problem they supposedly solved. It's no good starting up a software patent collective, only to realize decades from now that the collective itself is strangling creative freedom amongst programmers.

It almost seems like we'd need a protocol rather than an organization. A formalized set of contracts that let two companies say "I won't patent-stab you, cross my heart/hope to fly. Also, I won't patent-stab anyone that implements a compatible agreement, and I promise to help you out". That seems to nail the funding/splintering issues and seems to allow motivation to play out naturally, but it raises new problems. How do you motivate disparate users of the protocol to stick up for each other? How do you prevent predatory companies from exploiting this system via trolling? How *the hell* do you define something like this in legal terms without opening enough loopholes for both Redmond and Cupertino to fit through?

I don't know the answers to any of these questions. It's thorny as fuck, and there would be big teething problems with any such arrangement, but it at least seems viable from this distance. I'll put some more thought and reading into it, I guess. After some sleep, obviously. Feel free to tell me how old/impossible/stupid/"ambitious" this idea is. Also feel free to share some ideas about how to get it rolling and restore sanity to the field.

* * *
##### Footnotes
1 - <a name="foot-Fri-Apr-13-012416EDT-2012"></a>[|back|](#note-Fri-Apr-13-012416EDT-2012) - It could probably work on all patents, but I'm still working under the assumption that a certain portion of non-computational patents actually achieve the goal of protecting inventors. Feel free to convince me otherwise.
2 - <a name="foot-Fri-Apr-13-012524EDT-2012"></a>[|back|](#note-Fri-Apr-13-012524EDT-2012) - Internationally or not
3 - <a name="foot-Fri-Apr-13-012548EDT-2012"></a>[|back|](#note-Fri-Apr-13-012548EDT-2012) - As a side thought, it might be a good short-term idea to encourage patent trolls to join, since they have precisely the sort of just-legal-enough patents you'd need to start a defensive thicket. They're typically just after money, so paying members a certain amount per patent owned might not be an entirely stupid idea. The collective would just need to be careful not to set up a situation wherein *they* become the sole reason new patents are issued. That would be pretty freaking ironic.
4 - <a name="foot-Fri-Apr-13-012801EDT-2012"></a>[|back|](#note-Fri-Apr-13-012801EDT-2012) - At which point the fight to dissolve them would get monumentally easier.
5 - <a name="foot-Fri-Apr-13-013130EDT-2012"></a>[|back|](#note-Fri-Apr-13-013130EDT-2012) - Of course, then you get the additional takedown method, where an attacker can sneak an agent in and start provoking trolls in order to sap that defensive mechanism.
