Dear Internet,

I see you're [having security problems](http://www.zdnet.com/ubuntu-forums-hacked-1-82m-logins-email-addresses-stolen-7000018336/), so I'm going to let you in on a technique for doing proper authentication. I've [discussed it before](http://langnostic.blogspot.ca/2012/06/authentication-authentication.html), but I get the feeling you thought I was trafficking in trade secrets, and scrupulously decided not to hear too much. Let me be clear that this is public knowledge, and is meant for sharing.

### <a name="proper-authentication"></a>Proper Authentication

To start with, your server should have a public/private keypair, and so should your users. When a user registers, ask them for their public key, and publish the server's public key in a few disparate places on the web. Then, when a user wants to log in


1.   the user specifies their account with an account name
1.   the server generates a piece of random state, encrypts it with the accounts' public key, signs it, and sends both the cyphertext and the signature to the client
1.   the client verifies the signature, decrypts the cyphertext message, signs the resulting cleartext and sends the signature back to the server
1.   the server verifies the signature against the state it sent out for that account


Assuming everything went well, the server can act on a successful authentication.

What just happened?


-   The user knows that the server they're communicating with has access to the private key they expect
-   The server knows that the user they're speaking to has access to the private key that corresponds to the user account asking for authentication
-   Finally, **critically**, neither has enough information to allow impersonation of the other


There! That's the secret! Now you'll never fuck it up again!

This is a way to prevent any further "Oh noez, our server got hacked!" garbage *forever*, because if a server using this auth method got hacked, all the hackers *actually got* is information that's already public, or can reasonably be.

Before you pipe up with the "But users are too stupid to use private keys" thing, shut up.

Just shut up. 

The user doesn't have to do this manually; it's easy to imagine a series of plugins, one for each browser, that implement key generation, encryption and management for a user without them having to really understand what's inside the black box. More importantly, even a stupid, simplified, operationally insecure PK authentication system with full focus on ease-of-use *would be better than using passwords on the server side*.

Please *please* consider this, Internet, I'm getting really worried about you.

Sincerely yours,

-Inaimathi
