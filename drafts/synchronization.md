One of the problems I'm still wrestling with in taking the proposed massively-distributed system you saw demoed in [The Router](TODO) is syncronization among cell tiles.

So, you know what time it is.

There's a few different ways of dealing with this, and I'm not sure what we'll want to do. Because of how these systems are structured, we're solidly in [CAP theorem](TODO) territory and I don't think we can leave very easily. Although, there is a trade I'm thinking of that might make a decent compromise. The problem we're dealing with is

> How do we provide a consistent event window to the active cell despite the fact that constituent neighbor cells might be network-remote resources?

The cool part is that most of the solutions involve entirely tile-level behavior. Which means I might be able to whip up a few simulators and compare them against each other by running a set of known-behavior programs and seeing how they compare in the statistical sense. Lets start with the exception.


## Don't

In this approach, we make the tile boundary explicit, and expose it to cells as well as their running machines. We're essentially punting the decision of how to treat them differently up to the application developer. Essentially, there are two different types of cells in the neighborhood a running machine might have access to; `local` neighbors and `remote` neighbors. `local` as in hosted on the same tile as the current cell, and `remote` as in hosted elsewhere. The neighborhood a running machine would get access to in this universe would be composed of zero or more `remote` cells and some number of `local` cells, depending on its exact posiiton on a tile.

The availability/consistency decision here is pushed off to the application layer because some programs might be able to behave equivalently with respect to both cell types, but others might have to treat `remote` cells _very_ differently indeed from `local` ones. The tiles get to remain simple this way, but the cells and machines may end up being more complicated, depending on the behavior requirements of a particular program.

## Locks

When a particular cells' turn comes up, lock its entire neighborhood and give it write access. Lock its _entire_ neighborhood, even if this involves remote locks which are a pain in the ass to implement correctly at the best of times, and even then, might behave unexpectedly in the face of extensive hardware and network failures.

This is slower, because we have to implement remote locking. Which at once restricts computations at a number of cell sites depending on the size of the event window, _and_ involves some number of network calls just to establish that lock. It's Consistent and not Available because the locked cells don't get to do anything while locked, so they're guaranteed-ish to have consistent state across the turn. Guaranteed-_ish_ because downed tiles or network partitions cause remote locks to make hard decisions between eternal deadlock and pre-maturely invalidating locks, thereby introducing inconsistency.

Now that I've kind of said this one out loud, it's pretty intuitively obvious that we don't want this solution, for various reasons. But it might still end up being the least evil, so we'll keep it in our back pocket.

## Synch-less

2. don't lock at all
- faster because no locks
- less consistent because working with assumed data that might be pretty stale

## Explicit



## Delayed Synch

4. Before running a turn, _at the tile_ level, make a call to neighboring tiles for necessary information. Store it in a local cache. Let the active cell maniuplate that local cache, then synch up with neighboring tiles after the turn has run
- fast-ish because no locks, but after-the-fact reconciliation
- slightly less consistent because stale data is still a thing (but hopefully consistent enough that we're still ok at the macro-level)
- needs extra space on each tile. This is an engineering decision, but we need enough extra space to store at minimum about three quarters of an event windows' worth of cells (probably several times that so that we can cache to some threshold in case of network partitions)
- more complicated tile behavior
