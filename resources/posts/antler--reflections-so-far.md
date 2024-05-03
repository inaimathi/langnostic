I've got a huge backlog at this point, which you can definitively see by going over to my [drafts folder](https://github.com/inaimathi/langnostic/tree/master/drafts). Still, I haven't gotten the chance to put up polished drafts, and I haven't touched off audio versions in a while. Working on it. But in the meantime, I've been working on something else.

No details, because we're in stealth mode and don't want to get prematurely crushed by the competition (and also, not technically a company yet), but this has been among the most intense few weeks of my life, in a really good way.

### Antler

[Antler](https://www.antler.co/location/canada) is ... extremely interesting. Gotta be honest, I didn't go in with high hopes, but this looks like it might have been one of the top five most significant, influential moments in my life, and it's not even over yet. This program has let me meet some interesting, intense, driven and ambitious people that I wouldn't otherwise have met. That alone made it worth doing. But beyond that, its put me on a path to building a company that I definitely wouldn't have thought to build on my own and that might make a positive change in the world while making a decent chunk of change the founders. I don't think this is something I would have done otherwise, regardless of how [coherent](https://www.lesswrong.com/tag/coherent-extrapolated-volition) I managed to make myself. 

There's something about the combination of

- making explicit a number of implicit background insights about the market, and company formation
- bringing a bunch of smart, ambitious people together in a context where the goal is company formation
- providing space, network (both in the CAT5 sense and the extended-people-to-talk-to-groups), dedicated time, and feedback loops
- giving a carrot incentive to start the process

that seems to make me much more productive than I usually am. 

They send out feedback surveys at the end of every week, and there's always a question in them along the lines of 

> How likely are you to recommend Antler to friends? Scale of 1 to 10.

and I've consistently hit 10. It's not for everyone, just so we're clear. It demands a level of focus, intensity and outright overconfidence (or at least a willingness to laugh at the odds and charge anyway), that I couldn't honestly recommend it to _everyone_ I know. It's certainly a you-get-what-you-put-in-plus-luck-modifiers situation. But if starting a growth company is something you've thought about, and you're willing and able to put almost every waking moment into this for a span of approximately ten weeks, then yes. You should sign up. I hereby recommend.

### The Build

Ok, that's the end of the pitch. Build-wise, I've uncovered interesting, weird facts about the development ecosystem.

Did you know that Windows installers are _not_ a solved problem? One part of the build we're putting together involves a piece that goes on endpoints. Like, laptops, desktops, possibly phones in the future, whatever. Getting something that

1. Runs in the background on windows
2. Can be installed sensibly
3. Restarts on reboot/login
4. Doesn't send up false positives on Windows security/antivirus protections

is surprisingly non-trivial. The approach I ended up taking was a combination of [InnoSetup](https://jrsoftware.org/isinfo.php) and [cx_Freeze](https://cx-freeze.readthedocs.io/en/stable/) (since I'm writing in Python). It has to be both so that users can pick which one's right for their particular use case. InnoSetup creates pretty nice installer wizards, but produces only `.exe`s. Which universally get flagged by various security widgets. `cx_Freeze` can bundle an `msi`, which won't get similarly flagged, but is surprisingly rigid in where the resulting binary gets installed, what additional steps you can take to install dependencies, and what you can specify about the restart characteristics of the resulting application.

This is _extra_ surprising to me because, as a long-time Debian user, the same thing in my world _is_ entirely trivial. I'm not sure if this is some weird historical quirk of early Windows development that I'd understand better by reading through [Joel on Software](https://www.joelonsoftware.com/) again, but it's more than a little annoying.

Anyhow, that task has been put away. The next part is putting together a pretty serious instrumentation layer for droplets. 

Wish me luck. And as always, I'll let you know how it goes.
