I've got an idea to peel, and for a change, it's not even mine. I'm in the middle of reading through a [Raskin](https://en.wikipedia.org/wiki/Jef_Raskin) book entitled ["The Humane Interface"](http://www.amazon.com/The-Humane-Interface-Directions-Interactive/dp/0201379376), in which he suggests a different take on user authentication. In section **6-4-3**<a name="note-Tue-Dec-10-132000EST-2013"></a>[|1|](#foot-Tue-Dec-10-132000EST-2013), Raskin suggests that signing on to a system can be accomplished without requiring a user name. That is, instead of ... you know what, here, this is easier:


> Users are doing more work than necessary when signing on to most systems. You first state who you are -- your "handle", "online name" or "system name" -- and then you provide a password. The name presumably tells the system who you are, and the password prevents unauthorized persons from using your account.
> In fact, you are telling the system who you are twice. All that is logically required is that you type a password. There is no loss of system security: The probability of guessing someone's name and password depends on how the password was chosen, its length and the like. Finding the user's online name is usually trivial; in fact, it is commonly made public so that she can be communicated with. A badly chosen password, such as your dog's name, is the most common reason for poor security.
> The technical argument that typing two separate strings of characters gives more security is false. If the online name is *j* characters and the password is *k* characters, the user, to sign on, must type *j+k* characters, of which only *k* characters are unknown to a potential interloper. If the password was chosen randomly -- this is the best you can do -- from a character set with *q* characters, the probability of breaking into the account on a single guess is *1 / q^k*.
>
> -Jef Raskin -- The Humane Interface, p183


If you've given authentication systems anywhere near as much thought as I have, the trouble you should immediately see is that in a system like the one proposed above, *a password must be unique to a user*. Luckily, Raskin sees that one coming.


> The question arises: How can you ensure that everybody has a unique password in a password-only system? What if two or more users chose the same password? The best option is to have the system assign them. This method can result in very unmemorable passwords, such as `2534-788834-003PR7` or `ty6*>fj`d%d`<a name="note-Tue-Dec-10-132003EST-2013"></a>[|2|](#foot-Tue-Dec-10-132003EST-2013). Another technique is to use a random pair of dictionary words, such as *demijohn-shoestring*, *confirmed-tweezers* or *sulphur-dive*. If a dictionary of 60,000 words is used, the chance of guessing a password on the first try is one in three billion, six-hundred thousand. Using three words puts the difficulty of guessing them beyond hacking with current technology; there are 2.16 x 10^14 such combinations, and guessing and checking a billion of these a day, beyond what can be done at present, would still take about 10^5 days, or 275 years. That's reasonably secure. User-created passwords, at least those more readily memorized by the user, are inherently less secure.
>   When the idea of improving the interface to a web site or a computer system by simplifying the sign-on process to require only a password is suggested, it is usually rejected on one of two grounds. Either the programmers say that that's just not the way it's done, or they say that they have no control over the sign-on procedure. But someone, of course, does have that control.
>
> -Jef Raskin -- The Humane Interface, p183,184


Before I discuss this idea with my self, I have to disagree with two points. First, the odds of guessing a correct password on the first try is *not* `1` in `3 600 000 000`, or `1` in `(* 2.16 (expt 10 14))`<a name="note-Tue-Dec-10-132008EST-2013"></a>[|3|](#foot-Tue-Dec-10-132008EST-2013). It's `n` in whichever-you-picked, where `n` is the number of users you have. *With a password-only system, an attacker is no longer trying to guess a particular users' password, they are trying to guess **any** password already assigned by your system.* Second, I'm not entirely sure that badly chosen passwords are any longer the most common reason for poor security<a name="note-Tue-Dec-10-132011EST-2013"></a>[|4|](#foot-Tue-Dec-10-132011EST-2013), but rather [utterly](http://www.zdnet.com/ubuntu-forums-hacked-1-82m-logins-email-addresses-stolen-7000018336/), [mind-fuckingly](http://www.informationweek.com/attacks/sony-hacked-again-1-million-passwords-exposed/d/d-id/1098113?print=yes) stupid security design by password DB teams<a name="note-Tue-Dec-10-132014EST-2013"></a>[|5|](#foot-Tue-Dec-10-132014EST-2013).

With that our of the way, lets all don our white hats<a name="note-Tue-Dec-10-132022EST-2013"></a>[|6|](#foot-Tue-Dec-10-132022EST-2013), and imagine the proposed system in enough detail to implement it.

### How does a user log in?

In the context of a web application, they've got one field to fill out, "passphrase", and one button to click, "Log In". The passphrase entered is then hashed and looked up in our user database; if it matches a passphrase hash we have on file, the user ID is retrieved and used to get the specified users' program state. We then continue along letting them do what they're actually here to do. In an ideal system, this authentication step would be entirely optional, allowing it to happen at the last possible moment, when a user needed to commit some piece of data to their server-side corpus.

This is easily the biggest introduced weakness I see in the proposed system. Because we only have a passphrase to work with, we can only use either an unsalted hash, or a per-server "salt" to keep our passphrases out of plaintext. If we didn't, that user lookup based on the password would take a long time. Scaling at `On` with number of users, with some fairly ridiculous constants tacked on. That's dangerous, because we're suddenly gambling that the rest of the application our auth system is embedded in won't allow any injection attacks, or leak database information any other way. Granted, because we're guaranteed to have unique passwords, such a disclosure isn't as easy to take advantage of as it might be, but it's still a concern.

### What happens when the user enters a passphrase that isn't currently assigned?

There are really only two reasonable possibilities:

**They get an artificial delay, followed by the above message.** The standard log-in procedure also needs to have an equivalent delay, otherwise attackers might abort a guess before getting the response back, which would prevent them from actually being delayed in the practical sense. It doesn't have to be long; a second or two would be enough to prevent the kind of guess hammering I've got in mind, and it wouldn't be too annoying to users provided we put in a little spinning graphic in the meanwhile<a name="note-Tue-Dec-10-132024EST-2013"></a>[|7|](#foot-Tue-Dec-10-132024EST-2013).

**They get a "logged in" response with the default state in place, and no other warning.** Effectively, an "incorrect" passphrase entry becomes a registration. Users might get annoyed at this one, since it would seem at first that their program state is gone.

Having thought about this for a bit, it becomes clear that there's only *one* reasonable possibility, and it's the first one.

### How does the registration process work?

This might be context sensitive by application. For instance, [Deal](https://github.com/Inaimathi/deal) lets users play entirely anonymously. I can easily imagine a system wherein after 10 minutes of play time, a user just automatically got an in-game notice with a passphrase that would let them resume where they were. Because the server controls all the steps to a registration, it can happen behind the scenes with some game time effectively taking the place of a [Captcha](http://www.google.com/recaptcha). This could be used with any system that lets you start off anonymously; wikis, bulletin boards, forums, etc.

That system, elegant as it might be from the implementation and usability side of things, wouldn't work for something like [GoGet](https://github.com/Inaimathi/goget). Where the only possible reason to use the application is to go back later and check what you put in the first time. In that situation, you'd want the usual up-front "Register" button that would do the Captcha thing to make sure you're not a robot<a name="note-Tue-Dec-10-132028EST-2013"></a>[|8|](#foot-Tue-Dec-10-132028EST-2013), and hand the user an account before they start doing stuff. Really, this might be re-designed too though; have the system start you off on a blank check-list, with an unobtrusive "Log In" form at the top of the page, with the added button "Save", which would register you and hand you a passphrase with which you could access the list you just made.

### What do we do when passphrase exhaustion occurs

Granted, 216 000 000 000 000 is a large number, but it's not infinite, which means some clever bastard out there is going to find a way to cut it in half a few times for the purposes of guessing. And it doesn't take very many halvings to get that down to a tractable level. We have to deal with this problem a good deal sooner than "passphrase exhaustion"; if we get to the point where all passphrases are assigned, an attacker suddenly gets access to an account no matter which possibility they guess. But if we did something naive like hand out 2-word passphrases until they ran out, then an attacker who registers and receives a 3-word passphrase would know that any 2-word combination of our dictionary words will give them access to an existing account. We'd really want to generate new passphrases well before we ran out; at something like 10% exhaustion at a guess. Or better yet, don't limit passphrase length to two words, make it `n` random words, where `n` is something like

```lisp
(let ((u-mod (/ user-count 1000000)))
  (random (+ 2 (floor u-mod)) (+ 4 (ceiling u-mod))))
```

That should give attackers less purchase, and scale naturally with additional users.

### What Have We Got?

Switching briefly over to my black hat, I can't see an attack on this system that would get you any traction above and beyond traditional password-based implementations<a name="note-Tue-Dec-10-132038EST-2013"></a>[|9|](#foot-Tue-Dec-10-132038EST-2013). That doesn't mean there isn't a way, of course. I'll present the idea to some discerning and devious thinkers to see what they can come up with. Otherwise, we've got some interesting properties here, mainly because the server-side is the one putting everything together. We have a passphrase system that


- only requires the barest interaction with the user
- can be initiated automatically at some point<a name="note-Tue-Dec-10-132043EST-2013"></a>[|10|](#foot-Tue-Dec-10-132043EST-2013)
- will not suffer the failure mode that someone will use this same passphrase everywhere<a name="note-Tue-Dec-10-132046EST-2013"></a>[|11|](#foot-Tue-Dec-10-132046EST-2013), meaning that even if the system is compromised, all an attacker has is access to an account for one particular service
- will never have to worry about a passphrase as shitty as "password" or "12345"


On the flipside, though:


- You can't change your passphrase to something you want
- There isn't a way to recover or reset a forgotten passphrase<a name="note-Tue-Dec-10-132049EST-2013"></a>[|12|](#foot-Tue-Dec-10-132049EST-2013)


All in all, barring someone pointing out some egregious security flaw in this approach, it seems to be worth implementing.

I'll see what I can do.

* * *
##### Footnotes

1 - <a name="foot-Tue-Dec-10-132000EST-2013"></a>[|back|](#note-Tue-Dec-10-132000EST-2013) - Starting on pg 183 in the copy I'm holding.

2 - <a name="foot-Tue-Dec-10-132003EST-2013"></a>[|back|](#note-Tue-Dec-10-132003EST-2013) - The actual printed second password contains some unicode characters which were not accurately reproduced here.

3 - <a name="foot-Tue-Dec-10-132008EST-2013"></a>[|back|](#note-Tue-Dec-10-132008EST-2013) - Depending on how many words you decide to pack into each generated password.

4 - <a name="foot-Tue-Dec-10-132011EST-2013"></a>[|back|](#note-Tue-Dec-10-132011EST-2013) - Though it may just be a media imparted bias on my part.

5 - <a name="foot-Tue-Dec-10-132014EST-2013"></a>[|back|](#note-Tue-Dec-10-132014EST-2013) - Such as the refusal to use appropriate hashing algorithms, or inadvertent opening of various injection attacks.

6 - <a name="foot-Tue-Dec-10-132022EST-2013"></a>[|back|](#note-Tue-Dec-10-132022EST-2013) - Mine's a tuque because it's cold out and I'm in Canada, but you should feel free to don your hacking fedora, trilby, stetson, what-have-you as regionally appropriate.

7 - <a name="foot-Tue-Dec-10-132024EST-2013"></a>[|back|](#note-Tue-Dec-10-132024EST-2013) - Most authentication systems I interact with take longer anyhow.

8 - <a name="foot-Tue-Dec-10-132028EST-2013"></a>[|back|](#note-Tue-Dec-10-132028EST-2013) - Or not, really. Depending on how much traffic your system can handle, how much you care about preserving disk spce, and whether you give your users the ability to use SMTP facilities, you might get away with putting in an artificial 3 or 4 second delay before registration completes rather than trying to prevent automatic sign-ups. That's what I plan to do, in any case.

9 - <a name="foot-Tue-Dec-10-132038EST-2013"></a>[|back|](#note-Tue-Dec-10-132038EST-2013) - Apart from the situation where our ciphertext passwords have been leaked. Which, granted, isn't a high bar, but still.

10 - <a name="foot-Tue-Dec-10-132043EST-2013"></a>[|back|](#note-Tue-Dec-10-132043EST-2013) - As in the situation in Deal that would automatically hand the user a passphrase ~10 into active use of an unregistered account.

11 - <a name="foot-Tue-Dec-10-132046EST-2013"></a>[|back|](#note-Tue-Dec-10-132046EST-2013) - Hopefully, at least.

12 - <a name="foot-Tue-Dec-10-132049EST-2013"></a>[|back|](#note-Tue-Dec-10-132049EST-2013) - Since the passphrase acts as both a name and password, if you forget it, you just have to start a new account. Allowing the user to save as much of their data as possible locally would work to alleviate some of the pain from this.
