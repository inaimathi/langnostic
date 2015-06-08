> Type your name, click "login",  
> Now count to three  
>   
> Come with me and you'll be  
> In a world of pure authentication  
> Take a look and you'll see  
> Into web authorization  
> We'll commence with a glance  
> at classic watchword authentication  
> Where we'll try for complete explanation  
>   
> If you wish real security  
> Use RSA to guarantee it...  


![Willie Wonka looking at you](/static/img/wonka.jpg)

Ok, that's quite enough of that. There are reasons I don't consider myself a writer, and bullshit like this is right up there.

I've spent the last couple of days doing some quick research on RSA-based, auth systems. This may end up being another one of those things I was severely overconfident about. Since finding out that mathematicians have formalized this thing called Public Key Encryption, I vaguely assumed that someone had put together an authentication system that uses it, and that the only reason such an easy-to-use and secure system was losing out to traditional password auth was... well, I don't know, but it had to be pretty good, right? It turns out that this is only a theoretically solved problem.

Reading material on the subject includes one [pretty good statement of the problem](http://www.phoenix-web.us/compinfo/pubkey/index.html), one [Enterprise implementation of a similar system](http://germany.rsa.com/node.aspx?id=3663), an article [musing idly on the possibility](http://neverfear.org/blog/view/3/Secure_website_authentication_using_GPG_keys), and [one or two](http://webmasters.stackexchange.com/questions/28691/using-public-key-authentication-for-websites) people wondering the same thing as me: why aren't we already doing this?

At the high-level, there's two ways of doing this (which could, and probably should, be combined):

## <a name="prove-that-you-can-read-this" href="#prove-that-you-can-read-this"></a>Prove that you can read this

1. The server sends the user a random ~64 byte code, encrypted with the users' public key
2. The user decrypts the key and sends back the plain-text
3. The server verifies that the plain-text it gets back corresponds to what it sent in step one
      - If it does, that code is revoked and the user is given access.
      - If it doesn't, boot the fucker

## <a name="prove-that-you-can-sign-this" href="#prove-that-you-can-sign-this"></a>Prove that you can sign this

1. The server sends the user a random ~64 code
2. The user signs that plain-text and sends the result back
3. The server verifies that what it got matches the plain-text from step one, and was signed by the user trying to log in
      - If it does and it was, that code is revoked and the user is given access.
      - If it doesn't or it wasn't, boot the fucker *(and probably make a note of the break-in attempt)*

Like I said, you could combine them at low effort, though I'm not actually sure it would add any kind of security boost over one-or-the-other. The trouble, I assume, is the UI; the simplest possible way to implement this system involves some pretty odd (odd for the average computer user on the interwebs today) steps for the user.

First, logging in becomes a minimum two-step process. Three, really, if you count decrypting/signing as a step. Because the server needs to know who you're trying to log in as *before* generating and sending your code, you can't identify yourself and send the answer at the same time the way you can with password systems. You need one, *then* the other. 

Second, the user needs to decrypt/sign output from the server. This is non-trivial for most people, or at least, that's the only conclusion I can draw from the fact that most email is not encrypted. Barring trickery, this would need to be done manually; copy the message out of an HTML page, paste it into your PGP/GPG client 
and have it do its thing.

Third, the user is now effectively tied down to a single computer for their browsing experience, since you can't exactly carry an RSA key around as easily as a passphrase/password. At minimum, you'd need a USB key, and you'd need to trust that computers you were using it with didn't secretly keep a decrypted copy of your key around for nefarious purposes. Good luck with that, I guess.

The [forever hack](http://www.codinghorror.com/blog/2007/05/phishing-the-forever-hack.html) still works, and will continue doing so ... well, forever, but we do gain some real advantages by using public key auth rather than passwords.

1.   No one can sniff your key
2.   Brute-forcing a key is much harder than brute-forcing a password
3.   You don't need to remember passwords (other than the one that encrypts your key)

In other words, if we could solve that UI problem in a semi-automated way, this would be an altogether better way of doing web authentication.

### <a name="the-plan" href="#the-plan"></a>The Plan

I actually intend to build this, because having such a system would be a good thing from my perspective personally, as well as for web security in general. If no one's done it before, I guess I may as well take a crack at it. The steps are already outlined above


1.   [client] requests page requiring auth
2.   [server] asks for a user name/identifier
3.   [client] enters user name/identifier
4.   [server] sends encrypted string, records it, expects signed plain-text
5.   [client] decrypts, signs string, sends it back
6.   [server] compares with what was recorded in 3, authenticates or boots based on comparison


Step 5 can potentially be handled by a browser plugin. Something that would look for an RSA-auth form, and do the right thing. Either by tracking your auth keys itself, or by calling out to OpenSSH or similar on the client side. The server-side actually looks pretty simple, and needs minimal changes from the auth system we put together last time. Really, we'd just need to store a users' public key instead of (or in addition to) their password, and use the Erlang `crypto:rsa_*/\d` functions to process the specified messages.

Step 5, I'm going to want to think about/research some more. I know there are [various](http://www.ohdave.com/rsa/) [RSA](http://www-cs-students.stanford.edu/~tjw/jsbn/) [implementations](http://www.ziyan.info/2008/10/javascript-rsa.html) in JS, so it's at least plausible to write a [browser plugin](http://code.google.com/chrome/extensions/overview.html) that can generate a separate key for you for the purposes of website authentication. Another option is to interface with an external program, such as [OpenSSH](http://www.openssh.org/) or [GnuPG](http://www.gnupg.org/), but that's something with which I have limited experience.

I'll leave those thoughts in the air for now. Feel free to tell me how stupid all this is, and what a better solution would be. I'm off to do some research, before laying down any more words.
