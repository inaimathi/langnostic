Ok, so here's what I'm planning. Basically, I'm going to use the Lisp Game Jam as an excuse to bootstrap a little games factory here. The idea is to

1. Make some games _(probably stupid browser games)_
2. With `cl-notebook`
3. That can be seamlessly exported and hosted on [Itch](TODO - itch.io)
4. Record and comment on this whole process, while using it as an excuse to file down the rough edges on `cl-notebook` itself _(or at least those edges that chafe most when using it to build stupid browser games in a relatively intelligent manner)_

That seems like a reasonably cool little win that ends up promoting the platform, getting me some more exposure as a programmer, possibly netting me some cash, letting me level up as a game developer.

## Game Ideas

So there's a bunch of low-hanging fruit I have lying around in my head as a result of being an incorrigible videogame/boardgame/programming language nerd, in that I've seen a fuckton of different things that might make fun-ish, unique-ish games when implemented at scale. Here's a brain-dump, off the top of my head.

### Stupid game ideas

- color matching/switching game _(something a-la Bejewelled or similar "explode things of similar colors")_
- hacking mini-game _(like, there's a simulated computer system that you need to pwn in order to win)_
- lemonade stand
- word search
- twinstick shooter
- classic top-down shooter
- classic platformer
- fighting game
- rhythm game
- Choose-your-own-adventure game
- typing tutor
- Roguelike
- idle game _(a-la Cookie Clicker, Progress Quest, or any number of the weird Android games out there right now)_

### Interesting board-game mechanics

- deck-building _(more generally, engine-building)_
- worker placement _(with or without bumping mechanics)_
- round/game specific goals _(roughly equivalent to quests in videogames)_
- card drafting
- dice rolling
- bag building
- hidden identity
- area control/area majority
- semi-cooperative game structures _(generally, there's a winner-take-all property somewhere, but players also sometimes have to cooperate or they get a strictly worse outcome collectively)_

### Economics/Comp-sci/programming concepts

- Graph theory
- Cellular automata
- Continuous automata
- Trading mechanics
- AI programming
- Merge resolution
- Datastructures _(Vectors, linked lists, trees, tries, sets, hashes, etc.)_
- Append only datastores
- Namespace resolution
- Hashing
- Turing completeness

So my suspicion is that, to a first approximation, you can pick between one and three total items from above, slap a leveling/progression scheme on it (whether the D&D style XP-gather-and-level or a base-management thing), and implement it in-browser to get a half-way decent game experience[^minus-excellent-visuals].

[^minus-excellent-visuals]: Minus excellent visuals for now. I'm going to save that for when I've either got the patience to do some, or when I manage to scrape enough funds together with this approach that I an hire an artist/designer or two. We'll see how it goes for me I guess. Also, the point of this exercise is for me to get better at building games. Decorating them can be a later exercise, though I have no illusions that better visuals directly correspond to better uptake to some threshold.

As a test of this idea:

1. hacking/typing tutor hybrid, where you explore a network and hack into nodes by typing esoteric passphrases.
	- Progression scheme that gives you more hacking tries, longer network visibility, some activated abilities involving speeding yourself up or slowing your adversaries down
2. fighting game controlled by drafted abilities some of which might foil each other.
	- Progression scheme can give you more choices in draft, multiple uses of cards, advance notice of opponents' moves
3. Lemonade stand variant with cellular automata as customers
	- Progression scheme involves upgrading stores (capacity, production cost, range), advertising (better direction towards your stand), additional stores, additional products, etc.

Hm. I mean, maybe? I'd play some of those if they were implemented interestingly enough.
