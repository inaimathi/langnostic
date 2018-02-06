One of the problems I'm still wrestling with in taking the proposed massively-distributed system you saw demoed in [The Router](http://langnostic.inaimathi.ca/posts/the-router) is syncronization among cell tiles. And I'm not exactly sure where to begin implementing this thing.

So, you know what time it is.

There's a few different ways of dealing with this, and I'm not sure what we'll want to do. Because of how these systems are structured, we're solidly in [CAP theorem](http://robertgreiner.com/2014/08/cap-theorem-revisited/) territory and I don't think we can leave very easily. Although, there is a trade I'm thinking of that might make a decent compromise. The problem we're dealing with is

> How do we provide an event window to the active cell despite the fact that constituent neighbor cells might be network-remote resources?

There's a few possible answers.

## Don't

In this approach, we make the tile boundary explicit, and expose it to cells as well as their running machines. We're punting the decision of how to treat them differently up to the application developer. There are two different types of cells in the neighborhood a running machine might have access to; `local` neighbors and `remote` neighbors. `local` as in hosted on the same tile as the current cell, and `remote` as in hosted elsewhere. The neighborhood a running machine would get access to in this universe would be composed of zero or more `remote` cells and some number of `local` cells, depending on its exact position on a tile.

The availability/consistency decision here is pushed off to the application layer because some programs might be able to behave equivalently with respect to both cell types, but others might have to treat `remote` cells _very_ differently indeed from `local` ones. The tiles get to remain simple this way, but the cells and machines may end up being more complicated, depending on the behavior requirements of a particular program.

## Use Locks

When a particular cells' turn comes up, lock its entire neighborhood and give it write access. Lock its _entire_ neighborhood, even if this involves remote locks which are a pain in the ass to implement correctly at the best of times, and even then, might behave unexpectedly in the face of extensive hardware and network failures.

This is slower, because we have to implement remote locking. Which at once restricts computations at a number of cell sites depending on the size of the event window, _and_ involves some number of network calls just to establish that lock. It's Consistent and not Available because the locked cells don't get to do anything while locked, so they're guaranteed-ish to have consistent state across the turn. Guaranteed-_ish_ because downed tiles or network partitions cause remote locks to make hard decisions between eternal deadlock and pre-maturely invalidating locks, thereby introducing inconsistency.

Now that I've kind of said this one out loud, it's pretty intuitively obvious that we don't want this solution naively, for various reasons. But it might still end up being the least evil, so we'll keep it in our back pocket. It also might work half-way decently with some prospective execution stuff bolted on.

## Use No Locks

To avoid using locks, we need to have locally cached copies of remote cells. We refresh them from source by issuing a read on the affected cells at the beginning of each turn, running the selected local cell while giving it access to the proxy cells, and send remote writes/changes back to the appropriate remote tiles after the turn is complete. Depending on how much network bandwidth we feel comfortable using, we might report local changes in border regions too, so as to potentially reduce traffic.

This approach might be a bit faster but less consistent than locking. Because no locking is happening, we don't have to keep cells idle while waiting on remote computations to complete, but it does mean that their contents might change in a way that doesn't get communicated across to its proxy cells in time to make the current turn consistent. The drawback is that tiles get more complicated. The communication protocol between them becomes a bit meatier than just naive cell changes; there also need to be communications that talk about effects on proxy cells. Each tile also needs to have additional cells to store an event-window-wide section of the border region of surrounding tiles. As a result, there's also potentially a more elaborate tile-connection procedure that we need to go through when hooking up new tiles. In particular, it would have to communicate with its neighbors to figure out what its border region looks like to start with.

This sounds like the way to go, if I'm being honest. The real question here is "how much inconsistency are we actually introducing with the border-region proxy scheme?" My gut initially tells me that it's a tolerable amount, especially since this approach would let us gracefully deal with tile failure without busting our locks' consistency guarantees. Given that the goal of the whole system is to be robust in the face of radical inconsistency, this might just be the right tradeoff here.

# General Meditations

No matter which of the above approaches we take, we'll still need to be able to serialize cell contents somehow. This implies that we'll need to construct our cell-internal language at least somewhat carefully[^we-could-avoid]. The bottom line for tiles is that they need to know how to talk to each other with messages that let them communicate the contents and possible changes to local and remote cell contents. If we go the cell-language route, this will need to include a serialized listing for the cells' behavior, but even if we don't, we need to be able to communicate state mutations. Depending on how much or little we care about reducing network traffic, we might want to make that protocol more elaborate to include minimal delta communication, rather than just outright serialization.

[^we-could-avoid]: We could avoid this by trying to build a known set of machines that are built into each tile and instantiated locally, at which point we could just send labels and state contents across. I'm honestly not sure which of these is preferable, and it's a deeper question that I hope to tackle in a future standalone blog-post. Because of [Silverman]()'s work, I know that there are n-state cellular automata that end up being turing complete (which means we could encode larger programs in them), but I'm _not_ sure whether this gets us to anything resembling "robustness" as we mean it in this context. It's not necessarily the case that we can apply existing CA research directly, and it may in fact be a red herring; our goals may be different enough that the similarities between Robust-First and something like [Wire World](TODO) are valid but unproductive comparisons.

If we take either the "Locks" or "No Locks" appraches, we'll also need enough space locally to instantiate some number of proxy cells. Units that represent spaces on other tiles locally for the purposes of computing the effects of a turn. Again, we might make choices here depending on how much we value local space and network transfer[^for-instance-we-might]

[^for-instance-we-might]: For instance, if we really care about minimizing network traffic, we might do something like add a surrounding buffer of cells wide enough to accomodate 1/2 the event window size. This would let us refresh those cells before running the tile, and we could then

## Sync-Related

- no matter which approach we take we'll need to be able to serialize cell contents somehow. (In the threaded situation, we need to report results of a turn back to the locked cells, in the non-threaded situation we need to query remote state and then send remote modifications). Note that if we follow Ackley's "periodic table of cells" approach here, this becomes a trivial problem, potentially at the cost of system-level expressiveness.
- if we take either the locks or no-locks approach, we'll also need buffer cells (not necessarily as many in the locking approach, but we still need them unless we want to do a lot of frequent and probably pointless network calls during a turn)
- so we'll need a serialization format, and some sort of communication protocol involving reads and writes in any case. A buffering system would also be a nice general thing to work out.

## Non-sync related

- Time is a currency; limit potentially harmful operations by forcing them to end the turn (`end-turn` needs to be a thing; a way of allowing a cell to cede the turn early. If we're modelling cell behavior as running a procedure, this is equivalent to `return`)
