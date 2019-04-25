So I was just at [Clojure North](https://clojurenorth.com/).

## My talk ...

...went ok.

By which I mean it was the single most beautiful pile of flaming wreckage that I've ever had the priviledge of being involved in. I got about thirty seconds into the live-coding portion before Firefox (in which I was running [`cl-notebook`](https://github.com/inaimathi/cl-notebook)) crashed. Except when I tried to quit out of Firefox, it remained unresponsive. Then when I pulled up `terminal` to try to get `htop` running so I could see what's going on with my system, `terminal` remained completely unresponsive. So my entire computer ended up crashing. And then, when I went to restart my computer, in the middle of the talk that I had literally just spent a week working up on short notice, _the power button remained unresponsive for about a minute_. I ended up recovering from that, and still giving a half-way decent live-coding demonstration/QA session, but goddamn was it touch-and-go for about ten minutes there[^in-case-you-were-wondering].

[^in-case-you-were-wondering]: In case you were wondering, my best guess is that [`kazam`](https://itsfoss.com/kazam-screen-recorder/), a piece of screen-recording software I was running, exploded and ate all my memory at exactly the worst possible time. I'm not saying "stay the hell away from kazam", but I also can't give it a recommendation at the moment.

It was fucking glorious.

I'm kind of hoping they post the full, un-edited thing in all its burning, terrible, complete perfection just so that I can eventually link you to the exact timestamp the explosions start. It was bad enough that eight or so people came up to me afterwards to tell me how much they enjoyed my talk, and in particular how graceful the recovery was. It happened a few more times again the next day. To paraphrase a friend:

> No matter how much people wanted to boo you because you're a crazy foreign Lisp person, when the Vice McMahon of presentation problems walks into the ring with you, _and you take it down_, they're going to cheer at the end. - [_Gaelan_](https://github.com/RobotDisco)

If absolutely nothing else, I've gotten about 15 minutes into making my game for [the 2019 Lisp Game Jam](https://itch.io/jam/lisp-game-jam-2019), and the absolute, cast-iron, certain knowledge that I don't crack under any reasonable amount of pressure. Both of those are good things to have.

## The Rest of the Conference

All of the other talks I went to were interesting, and well put together. I couldn't catch them all, because that would require literally being in two places at once, but there wasn't a dull moment. The talks were all filmed, they'll presumably be linked to from the Clojure North website at some point, so I won't bother summarizing them for you here.

The main takeaway from me is that hiring Clojure people is kinda hard. Which is mildly concerning, given that I'm trying to use it proffessionally. At one point, a speaker mentioned that they'd just plain given up on finding [`clojurescript`](https://clojurescript.org/) developers, because they realized that they may as well be looking for literal unicorns. Their plan now is to just hire smart people and train them in the languages they need.

In practice, this ends up happening anyway. Empirically, if you're hiring `$LANGUAGE_NAME` developers, they're all going to have subtly different takes on what it means to write good `$LANGUAGE_NAME` code, even when `$LANGUAGE_NAME` is something that rigorously encourages conformity and uniformity like [Python](https://www.python.org/) or [Go](https://golang.org/). If you're piloting _any_ flavor of Lisp, you'll need to be aware that you're hiring from a pool of fairly diverse and idiosyncratic thinkers. So the end result is having to "train" most of them no matter what. Whether that's on dealing with your existing codebase if you have one, or on writing mutually interoperable code if you don't. The next logical step is to just admit the lie and say that you're training the remaining 4% of your hires too.

I don't know if this is going to seriously deter me from deploying things in Clojure over the next little while. But it will definitely cause me to make sure that I'm compartmentalizing that code as much as I can.
