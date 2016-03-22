> We don't need authentication
> We don't need access control
> No dark sarcasm in the network
> Servers leave them kids alone

![The cover image from Pink Floyd's 'The Wall'](/static/img/the-wall.jpg)

## Housekeeping

Despite the authentication theme, this update is probably going to be a mishmash of content, given how fast my head is spinning at the moment. First, I want to [bring](http://www.groklaw.net/article.php?story=20120712151437524) a couple [things](http://www.theatlantic.com/business/archive/2012/07/why-there-are-too-many-patents-in-america/259725/) to your attention. Not quite enough to earn this post the "Intellectual Property Bullshit" tag, but interesting nonetheless.

Also, before I move on, I polished up what I went over [last time](/posts/authentication-part4), and put it [up on github](https://github.com/Inaimathi/auth). The differences between what went up and what was discussed are minimal, though I did add some automated testing and polished off one or two odd corners that I glossed over in the writeup. I didn't commit a front-end at all, so you'll need to hook that up yourself if you plan to use it, but what's up there should give you all the plumbing you need. Let me know if that was a false statement.

It's not done yet, two-factor auth coming rather soon, but the existing API isn't going to change at this point. I may put another layer on top just to simplify the external interface, and maybe make it easier to expose this system as a web-service, but that's about it.

## Not Authenticating

Before we go any further, lets take a look at some real-life systems to see how they handle the Authentication Problem. I doubt you'll be surprised.

#### IRC

This is the classic online communication protocol. In fact, if you like, we can go further.

#### Mail

Not Email. Old mail. The original dead-tree protocol, just in case IRC wasn't quite classic enough for you. They have one very important thing in common; neither does any kind of authentication. A user in IRC chooses a username each time they log in to a session. They may choose a different name each time, and another user may choose a name that belonged to someone else last time. Likewise, mail doesn't authenticate. There are optional, unvalidated fields on every envelope where someone can put a not-necessarily-correct return address, but that's it. Bottom line, you really don't know who you're talking to. If you want to, you need to authenticate them some other way. Either you know their handwriting, or you're familiar with their word frequency, or maybe you've exchanged keys in the past so that you can now verify who they are, but the channel itself provides you with none of this information. Further, you have no real guarantee that a message you send makes it to your intended target, or that you're getting all messages addressed to you, or that they aren't being observed/tampered with even if they do eventually get where they're going.

These are the prototypical unsecured channels, and they still<a name="note-Sun-Jul-15-004825EDT-2012"></a>[|1|](#foot-Sun-Jul-15-004825EDT-2012) do a pretty decent job of putting humans in touch with one another in indirect ways. Mail used to be a fairly reliable line of communication, but I'm honestly having a hard time remembering the last time I got something other than spam or bills in the mail. I'm not entirely sure why, so I won't theorize. The key with IRC seems to be community size. It's a good bet that you can get some profitable conversation happening on smaller channels, but larger ones seem to exponentially attract various spammers.

The authentication system<a name="note-Sun-Jul-15-004834EDT-2012"></a>[|2|](#foot-Sun-Jul-15-004834EDT-2012) is nonexistent. Moving on.

#### Message Boards

I'm not discussing forums yet. I'm instead referring to the [various](http://kusabax.cultnet.net/) [*aba](http://www.2chan.net/script/)/[*booru](http://danbooru.donmai.us/help/source_code)-descended boards. These typically don't use authentication either, except for the moderators. They do tend to have the common feature of "tripcodes". Basically, hashes with varying levels of security depending on implementation. They don't let you know who you're talking to either, but it's at least semi-possible to verify that a given series of tripped messages come from the same source.

Moderation mostly seems to take the form of deletion passwords. That is, there doesn't seem to be a separate interface for mods, they just have the ability to remove content and hand out bans using an authentication they're given<a name="note-Sun-Jul-15-004841EDT-2012"></a>[|3|](#foot-Sun-Jul-15-004841EDT-2012).

The key takeaways here are twofold. First, non-authentication is the norm. It's not only conceivable but usual to have authentication between endpoints handled entirely separately from the intended communication channel. Second, it's unnecessary to enforce authentication for the purposes of communication or publishing. When you're posting content or sending messages back and forth, it's typically enough to verify that a given set of messages/articles all have the same source without necessarily verifying what that source is. Moving on. Again.

#### Wikis

These sit between IRC and Forums in terms of authentication strength. Here, I'm talking about both actual <a name="note-Sun-Jul-15-004908EDT-2012"></a>[|4|](http://en.wikipedia.org/wiki/Wiki">wikis</a>, and systems like [Stackoverflow](http://stackoverflow.com/). The key is that there is a user account system<a href="#foot-Sun-Jul-15-004908EDT-2012), and it's not just for moderators, but it *is* optional. Anyone can view and contribute some sort of information, but there is an inner circle of trusted users that makes up a wiki's core community, and that core wields the really powerful brushes.

The authentication that does exist is, by and large, the same password based stuff that gets used everywhere regardless of security. Some of them use <a name="note-Sun-Jul-15-004915EDT-2012"></a>[|5|](http://en.wikipedia.org/wiki/OpenID">OpenID</a>, which is a fantastic system in theory, but in practice seems to come down to a password system where four or five giant companies control the user databases for everyone<a href="#foot-Sun-Jul-15-004915EDT-2012). I think we can do better.

#### Forums

We're talking about the standard [phpBB-style](http://www.phpbb.com/) forums, as well as stuff a-la [Reddit](http://www.reddit.com/) and various social/news sites. By and large, these maintain their own user systems. I'm not going to make a blanket statement like **none of them actually need your information to do the job they propose to**, but it's at least partially true. This is the next level of authentication above wikis; you can view information without an account, but contributing any effort at all to the target community requires a registration. An effort is made to have people associate their identity with one account by tracking post numbers, karma, badges, or some combination of the above. That doesn't mean it happens, and in practice, spammers tend to commonly have vast networks of identities at their disposal if the potential audience is big enough. Really, these systems don't require the level of authentication they tend to have. I'm inclined to be cynical about it and say that they only want to have some sort of user estimate in hand, but that may not be the case.

#### Transaction Authorities

These are things like, oh, for example *your bank*. This isn't a publication system, there's a communication channel in place specifically to let you issue orders about resources that are ostensibly under your control. They tend not to use OpenID<a name="note-Sun-Jul-15-004937EDT-2012"></a>[|6|](#foot-Sun-Jul-15-004937EDT-2012), and they tend to [restrict passwords](http://me.veekun.com/blog/2011/12/04/fuck-passwords/) more than other organizations. These places have a vested, legitimate interest in knowing precisely who you are, keeping fake accounts as close to non-existent as possible, and having a traceable, consistent way of verifying where each command they're acting on came from.

The kicker here, the reason I get the feeling that these institutions are fine with enforcing crappy password and security practices for their users is that they don't really trust this whole internet thing. Here, let me regale you with a snippet from [the TOU agreement my former bank puts forth](https://online.pcmastercard.ca/PCB_Consumer/AcctTermsOutside.do)

**2.1 Security and privacy.** You acknowledge that the Internet is not a secure medium and privacy cannot be guaranteed or ensured.
>
> --PC Mastercard Terms of Use

In other words, "sure have your account, we're still calling you, or expecting you to come into a branch physically for the big stuff". I'm not entirely convinced this is a bad idea.

## Spectrum Summary

There's a huge number of different authentication strategies in use on the internet already. You can do most of what you need to without ever signing in to anything. There are a few things that require really strong, working security to do online. There are three potential reactions to this state of affairs.


1.   **Fuck It**. We don't need authentication on the internet. Lets make more anonymous services, and allow communication to happen in a freehand, fully private way. We don't need strong security, and it shouldn't be assumed in public networks; if there's something you need good security for, do it in meatspace.
1.   **We're Good Enough**. Half-assed authentication is good enough if no one looks too closely. It's there to keep honest people honest in any case, so improving it substantially isn't a big win, and exploiting it in a brute-force manner won't happen in ways we don't anticipate.
1.   **We Need Good Auth**. There are at least some situations that *require* intelligent, secure, verifiable identification systems, so we may as well put one together. If for no other reason than to use it where it's absolutely necessary.


As evidenced by my 4-part (and sure to grow) series on authentication, I fall somewhere between groups 2 and 3. Mostly in 3, really, except that I agree that perfect security is probably more trouble than it's worth; I may talk about why I think that in the future, but you can probably work it out yourself. That said, the difference between digital crypto-systems and [the traditional ones](http://en.wikipedia.org/wiki/Locksmithing) is that with the tools we've got, we can do a damn sight better than "keeping honest people honest". That's something to aspire to, and I aim to make a dent in the problem, at least.


* * *
##### Footnotes

1 - <a name="foot-Sun-Jul-15-004825EDT-2012"></a>[|back|](#note-Sun-Jul-15-004825EDT-2012) - For a little while at least, in the case of mail.

2 - <a name="foot-Sun-Jul-15-004834EDT-2012"></a>[|back|](#note-Sun-Jul-15-004834EDT-2012) - Which is what we're interested in here.

3 - <a name="foot-Sun-Jul-15-004841EDT-2012"></a>[|back|](#note-Sun-Jul-15-004841EDT-2012) - Presumably one per user, though I guess I could imagine there being only one moderation password for a given board.

4 - <a name="foot-Sun-Jul-15-004908EDT-2012"></a>[|back|](#note-Sun-Jul-15-004908EDT-2012) - Unlike the various message boards, which provide some trace of authentication capability without user accounts.

5 - <a name="foot-Sun-Jul-15-004915EDT-2012"></a>[|back|](#note-Sun-Jul-15-004915EDT-2012) - Yes, everyone can technically host their own identity, I get the feeling that a vast majority of OpenID users just let Facebook handle it. I reserve judgment on whether that's a good thing.

6 - <a name="foot-Sun-Jul-15-004937EDT-2012"></a>[|back|](#note-Sun-Jul-15-004937EDT-2012) - To put it mildly.
