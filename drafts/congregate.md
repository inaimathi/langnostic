So I'm trying to reanimate the [monthly Code Retreat](TODO) event that used to be a staple of my regiment. We went from aping the [annual Code Retreat](TODO)[^which-is-coming-up] with an "Implement A Game of Life with pairing and TDD" focus, to exploring different development approaches, to exploring different languages, to trying to figure out good, general-purpose problem-solving strategies. Then it fizzled a bit, because the people involved got too much other stuff on their plates, and the attendance dipped predictably.

[^which-is-coming-up]: Which is coming up on the 22nd, and I heartily recommend you attend. I'll be facilitating once again.

The thing is, i really fucking miss it.

That was the one bastion of higher-level, practical development practice in Toronto[^that-i-know-of-etc]. There's the [JS Hack Night](TODO), which has been steadily moving into theory,

[^that-i-know-of-etc]: That I know of, that isn't married to a particular development methodology or programming language.

So there's a thing you can do now that OAuth/OAuth2 has become so wide-spread. And that thing is to build a hybrid (or fully foreign authentication system). Basically set up your own auth so that it depends entirely on outside services to veryify users. There's a few problems if you go this way, and while [my current project](TODO - congregate github) doesn't _quite_ run into them yet, I've been thinking about them so they don't sneak up on me when i want to start scaling.

# Non-unique user-names

If you're writing an application, and you want users to be able to log in using, for instance, Github or Twitter, thne you need to reconcile the fact that a username might be claimed by [different](TODO - inaimathi on github) people on [different](TODO - inaimathi on twitter) services. So you can't just trust foreign usernames or IDs to sync up, which means you want to annotate them somehow with their service of origin.
