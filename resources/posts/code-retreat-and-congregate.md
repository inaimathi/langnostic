So I'm trying to reanimate the [monthly Code Retreat](https://bentomiso.com/events/toronto-code-retreat-2015-dec) event that used to be a staple of my regiment. We went from aping the [Global Day of Code Retreat](http://globalday.coderetreat.org/http://globalday.coderetreat.org/)[^which-is-coming-up] with an "Implement A Game of Life with pairing and TDD" focus, to exploring different development approaches, to exploring different languages, to trying to figure out good, general-purpose problem-solving strategies. Then it fizzled a bit, because the people involved got too much other stuff on their plates, and the attendance dipped predictably.

[^which-is-coming-up]: Which is coming up on the 22nd, and I heartily recommend you attend. I'll be facilitating once again, apparently.

The thing is, i really fucking miss it.

That was the one bastion of higher-level, practical development practice in Toronto[^that-i-know-of-etc]. There's the [JS Hack Night](https://bentomiso.com/events/js-hack-night-2016-jan), which has been steadily moving into theory, The [CS Cabal](http://compscicabal.github.io/) for those of you deeply curious about the finer points of language construction and type systems, the [Clojure](http://www.meetup.com/Clojure-Toronto/) and [CL](http://lispwiki.inaimathi.ca/) user groups which are more social than practical _or_ theory, and a few groups devoted to machine intelligence in general, but not to building any concrete artifact.

[^that-i-know-of-etc]: That I know of, that isn't married to a particular development methodology or programming language.

And that's about it.

The giant, gaping hole in the lineup is a gathering where we practice and refine our techniques for taking projects from zero to a running system. And that's what the Monthly Code Retreat was kind of evolving into before it got put on hiatus. I firmly believe that we need this. Or leastwise that _I_ need this in my life somewhere. Because without a practical application to keep things grounded, computer scientists have a tendency to disappear either into their own pet projects written in bizarre languages or into some proprietary system, in any case never to be heard from again. It's a shame to tread new ground in a sub-optimal way, and then not even share the fallout. So the new Code Retreat mandate should, I think, be to help developers of all levels push their skills forward and encourage the sharing of technique and experience.

Of course, it would be massively asinine to expect someone else to put together a community based on my mandate for my convenience. So I guess I'm organizing a group now.

## Organizing a Group

Unfortunately, there isn't a satisfactory group-organizing system out there. [The default](http://www.meetup.com/) is unacceptable for a number of reasons, not least because their approach is that you own none of the data they're publishing for you. They own it. And they will use it in whichever way will maximize their profits. An additional roadblock is that the default is aiming to be a general event hosting platform, which means that a few things specific communities would really like don't get implemented in the product. Unfortunately, I'm a programmer, so this is a problem I can damn well solve. The first thing I need to do, ironically, is start up a pet project in a bizarre language that solves this problem for me.

Which segues us very neatly into [Congregate](https://github.com/inaimathi/cl-congregate).

## Congregate

[Congregate](https://github.com/inaimathi/cl-congregate) is a first step on the road towards a worthy group management system. We're starting off with manual group addition, recurring events and a bare-bones RSVP system that lets you say whether you'll be there or not. That's it for now. No group creation, no comment system, nothing else. The ultimate goal is a protocol for publishing gatherings of humans, mainly because we're looking to get away from the situation where one publisher owns data related to any particular get-together.

Once I get a few more weeks into this project, I'm going to do my usual [`almost-literate-programming`](/archive/by-tag/almost-literate-programming), but not quite yet. There's a pretty big pile of things I still want to fine tune in [`house`](https://github.com/inaimathi/house), and possibly [`cl-handlers`](https://github.com/inaimathi/cl-handlers), not to mention constructing a bunch more obviously desirable features. I _am_ going to be running an instance over at [congregate.ca](http://congregate.ca) before too long so that you can kick the tires, but discussing internals is going to have to wait until I can give it some more serious thought.

The only thing I've really thrown enough thought at to mention is the authentication system. There's a thing you can do now that OAuth/OAuth2 has become so wide-spread. And that thing is to build a hybrid auth sytem[^or-fully-foreign]. Basically set up a user system so that it depends entirely on outside services to verify users.

[^or-fully-foreign]: Or fully foreign, if you can live without differentiating user levels in some way. It seems like you wouldn't have too much trouble making such a server fully anonymous though, so I'm not sure it would be worth the effort.

The main problem I see going this way is user-name inequality. Specifically, if you want users to be able to log in using Github or Twitter, then you need to reconcile the fact that a username might be claimed by [different](https://github.com/inaimathi) people on [different](https://twitter.com/inaimathi) services. So you can't just trust foreign usernames or IDs to sync up, which means you want to annotate them somehow with their service of origin[^and-actually]. At the moment, I'm using appropriately annotated usernames to key users, because they're easier to display, but I'm expecting to eventually have to deal with annotated foreign IDs of some sort.

[^and-actually]: And actually, some services let a [user change their username](https://github.com/settings/admin). So even though you can refer to them via URL containing their username, that URL might not necessarily refer to the same user on two separate network calls. That's an edge case that I'm prepared to ignore for the moment, but still reserve the right to change my mind before publishing  `cl-congregate 1.0`.

Anyhow, that's all for now. I'll keep you up to date regarding future development, but very probably not before I talk a little bit about [Howl](https://github.com/ontodev/howl).
