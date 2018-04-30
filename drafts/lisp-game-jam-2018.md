Holy fuck, I guess I'm doing this shit.

[This](TODO itch.io link) is a client-side implementation of Boggle implemented entirely using [`cl-notebook`](TODO github). It's called [`Clobble`](TODO repo link), becasue honestly, why wouldn't it be? I used the official die distribution cribbed from [this SE question](https://boardgames.stackexchange.com/questions/29264/boggle-what-is-the-dice-configuration-for-boggle-in-various-languages) and the challenge-mode scoring rules from [this WikiHow post](https://www.wikihow.com/Play-Boggle#/Image:Play-Boggle-Step-22.jpg)[^that-challenge-block]. Also, I used a static dictionary from [this project](https://github.com/insightcoder/boggle-dictionary), released under an MIT license.

[^that-challenge-block]: That `Qu` block was a bitch to implement, by the way. I haven't generalized it completely, but it alone accounted for an extra half-hour or so of coding.

Why Boggle? Er... Clobble? Because it's dead simple, so I could focus on getting something minimal together quickly and leave plenty of time for polishing up issues that come up in [`cl-notebook`](TODO github) itself. In all honesty, I was hoping to get much further than I did. I had plans for alternate game modes, leaderboards and a player-specific progression system, in addition to better visuals, but all that got kicked in the head when I realized how little time I'd _actually_ have this week. So what I ended up putting together was the dumbest thing that would work. There's two modes; single-level and zen (which just gives you infinite levels until you give up and hit the `Quit` button).

The challenges were many, and the [`cl-notebook` issues list](TODO) has already grown somewhat as a result of this experience. So let me do a post-mortem log dump.

## `cl-notebook` needs to deal with external files

So the dictionary itself is contained in a ~6MB external `js` file that declares a global variable with the appropriate `object`[^could-have-cut-that]. I could have cut that down to ~1MB by dropping the definitions and just keeping a word list, since the current implementatoin only really checks for the _presence_ of a word in the dictionary, but I had some plans that might have involved definitions.

## itch.io has some odd behavior for external files in web games
## The `Qu` die face introduces a bunch of special cases
