So here's what's been taking up a lot of my mind-space lately. I gotta be honest, I've been holding off on formalizing these thoughts, because I have this quasi-rational fear that voicing them will make me get bored[^or-self-conscious], but I'm taking the chance.

[^or-self-conscious]: Or self-conscious. Because, as I've discovered, absolutely nothing shows you exactly how little you know about a subject better than trying to formally explain it.

Dancing seems to be the ultimate co-operative, real-time strategy game. Which is why I've been thinking about it. For the moment, a lot of fun for me is expanding my `moves` (in terms of adding new vertices, finding new connection principles, and figuring out new modifiers to try with existing vertices), and figuring out how much of that graph I have in common with each new partner. There's _a lot_ to learn here. It feels like I've stumbled onto yet another infinite, constantly expanding graph of human experience, stretching out before me in every direction. And mind you, as I hinted in the title, this is _[one style](https://www.youtube.com/watch?v=kHD8lpaqs7I)_ of partnered, social dance, which is is itself _one tiny facet_ [of](https://www.youtube.com/watch?v=IpVf5-aTgys) the [full](https://www.youtube.com/watch?v=MnrxRyr7o0w) gem [of](https://www.youtube.com/watch?v=uleGaEiIh4o) human movement. I'm not sure there's a word for feeling _this_ tiny and short-lived.

Dancing Zouk can be thought of as the navigation of a nested graph.

First off, some definitions:

- A `position` is a point in time with a certain foot placement. It may be modified by some number of knee, hip, hand and/or shoulder placements. All of the above might in turn be informed by relative placement to your partner.
- A `move` is a directed sparse graph of positions Some moves are acyclic, and some aren't.
- A `routine` is a directed sparse graph of moves. Routines typically contain many cycles at various sizes and frequencies of occurrence.
- Your `moves` are represented by the full, directed sparse graph of moves you know.
- To `step` means to take a transition between positions, or from a position into a `compatible` position in a different move, or to change some number of modifiers on the current position.
- To be `compatible`, two positions must have complementary foot placements and modifiers, and the moves they reside in must have similar force directions.
- To `plan` a `routine` means to take as input a `Partner`, `Song`, and starting `position`/`move` and weigh the edges between your `moves` graph accordingly.[^the-starting-move]

[^the-starting-move]: The starting `move` is typically `Basic`, and the starting `position` is typically `First Position, no modifiers`, but it's definitely possible to start elsewhere.

Finally,

- To `dance` means to select a starting vertex on your `moves` graph, `plan` and `step` repeatedly, while taking input from your `Partner`, [cadence and pulse](https://www.youtube.com/watch?v=m205GdmL6pE) of the `Song`, and potentially surrounding `Couples` and `Terrain`.

This is probably harder than it sounds.

![My Moves. Let me show them to you.](/static/img/dance-moves.png)

At the moment, my `moves` graph looks like that. This seems to be something like 1/6th or 1/5th of the full Zouk possibility space, and is a vanishingly small part of the overall partnered dance space. Even so, you'll notice that there are plenty of full and partial cycles available to me, which means I can easily dance for quite a length of time without falling into any overtly repetitive patterns[^anecdotally], even without considering all the possible modifiers.

[^anecdotally]: Anecdotally, this has been enough to keep me moving consistently for 3-hour social dancing events, though I'm sure I do start repeating myself when I reach a certain threshold of tiredness.

Just based on the above graph, you can see that some of these moves are repeatable, while some aren't. That some moves afford more choice for the next move than others, and that some moves are easier to slip back into. At this point, I know enough about the subject to realize I've got a woefully incomplete, but potentially useful model of the endeavor.

That's ... about as far as I've gotten. I'll let you know if it actually helps, and may potentially develop this thought further.
