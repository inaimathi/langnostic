## Ansible

So I've been using [Ansible](TODO) for the past little while. At work, I mean. I don't deploy anything that requires anywhere near the amount of scale it would take for a tool like this to start paying or itself, so I probably wouldn't have picked it up at all if someone weren't paying me for the experience. Having now used [`nix`](TODO), [`ansible`](TODO), [`chef`](TODO), what strikes me about them is how many insist on a new language syntax. I'm quite curious as to why `ansible` couldn't have been implemented as a library in whatever turing complete language. With functions and environments rather than a giant director tree of `yml` files arranged by "convention"[^as-a-side-note].

[^as-a-side-note]: As a sidenote; the credo of "convention over configuration" has at this point been used to force users to memorize arbitrary garbage as an alternative to having the system designers sit down and think through the principles of their system in a reasonable way. I call this lazy fucking design, and I'm not content to put up with it quietly anymore. The more convention your system requires, the shittier it is on an absolute scale, there I said it.

On the one hand, this would possibly alienate X programmers who don't program your X. On the other, a well-chosen X would give some advantages, non-X programmers wouldn't have to put up with garbage like YML config files[^lisp-weenie-digression], and X programmers whose X was [probably coincidentally](TODO - link to self-titled) well-chosen mildly happier. That's probably the trade-off happening here; ease-of-use vs. likelyhood-of-adoption, so I won't linger on it too much. My only problem with Ansible remains the gobsmacking amount of boilerplate necessary to do small things.

[^lisp-weenie-digression]: Lisp weenie digression here, by the way. The only reason YML/XML/CONF/WTFBBQ files are needed at all is that the languages making use of them have shitty enough syntaxes that you can't express meaningful configuration _in_ them. As you may have heard by now, Lisp doesn't have this problem, which makes certain "trivially solvable" issues from other languages vanish entirely, and makes certain impossible issues from other languages tractable. I'll get into more depth on that at some point in the future, I'm sure.

In summary, I see the need to formalize a deployment process in a machine-manipulable way, and very much agree that the best way to do this is to write scripts that assume the bare minimum of prior infrastructure. I doubt that the syntactic choices made in any of the existing tools are optimal, or even adequate. Before too long, I'll want to do something about this.

## House updates

If you've been keeping an eye on the [`house` repo](https://github.com/inaimathi/house), you'll notice that I've been making updates at an increased pace lately. Mostly, this is so that I can have a better base on which to continue development of [`cl-congregate`](https://github.com/inaimathi/cl-congregate), but it's also the culmination of about a year and a half of background-processing I've been doing on various network-programming-related problems relating to the construction of a decent HTTP server. Most recently, I [ran through the first](/posts/house-performance) of several intended rounds of performance optimizations.

There are two ideas I had there that didn't get developed as far as I'd like, so you know what time it is.

### Spike-Specific Optimizations

One thing I consiedered is changing some of the session-related functions to become probabilistic. Ultimately, I _didn't_ do this because of the security implications, but it also got me thinking about how such modifications would behave. I have a theory that, under heavy enough load, there would be insignificant behavioral difference between cleaning out global session state more or less frequently to some threshold of frequency. Ine of the things we could do to improve performance where it really counts is to develop a series of optimizations that take advantage of this fact.

There's essentially two ways to do this. Both of them involve tracking request throughput in some way, which means that no matter what, there needs to be some global[^actually-this-can-be] store for request metrics. Either a raw count of request/minute, or a running average with corrections to keep spikes from getting hidden in noise.

[^actually-this-can-be]: Actually, this can easily be a per-thread store if we get to the second set of optimizations I've got in mind, just to reduce the number of synchronization points.

Once we've got that, we can either

1. Put together a set of dormant code-paths that start tripping when requests-per-minute (or appropriate average metric) hits some threshold
2. Make certain operations probabilistic with an input of said requests-per-minute (or blah-di-blah-blah-blah)

They sound similar, but implementation and behavior might differ non-trivially, so I'll need to consider them carefully. However; this approach realistically only helps with the session system and

- There are still simpler optimizations left to run on session-cleaning mechanisms. In particular, we could first ensure that no cleanup happens for sessions until enough time has passed for a session to be old enough to evict.
- There are other structural changes we could make that would affect more of the system and therefore very probably make a bigger difference to final performance

Specifically

### Worker-Thread Approach

`house` is currently a single-threaded system. And while I don't have any interest in going thread-per-request, convenient as that would be from the implementation perspective, there is room to use more than one thread to take advantage of the inherent parallelizability of the problem we've got. The existing system does all of its work in the single, central thread where everything does. That includes `listen`ing, `buffer!`ing, `parse`ing, and `handle`ing. And really, most of it needn't be done that way. I think we're stuck `listen`ing in one thread[^though-there-is], but the rest of the processing pipeline can be delegated to other threads if that's what we end up needing.

[^though-there-is]: Though there is also the option of [preforking](http://aosabook.org/en/posa/warp.html) on modern linux implementations. I haven't thought about this at all, except to note that it is possible and should be considered as a future improvement to `house`.

The real trouble is balancing single vs multi-threaded performance here. While we _do_ want to take advantage of more horsepower when we're running on servers that have it, we also don't want to _require_ more than one thread for proper operation, because that would prevent us from realistically servicing the raspberry-pi-ish use cases.

There are a few different ways I can implement this, depending on how much effort I'm willing to expend and how much payout I'd probably get. The difference between them essentially comes down to where in the chain of

```
listen -> buffer -> parse -> dispatch -> handle -> write
            ^_________|
```

there should be a break-out to multiple threads. We'll use the notation `-@->` to mean "handing off to whichever thread is currently ready to process".

##### `parse -@-> dispatch`

Essentially, from the implementation point of view, it seems like `parse -@-> dispatch`, `dispatch -@-> handle` and `handle -@-> write` will be equally difficult to put together, so we may as well go with the one that'll give us the most potential bang for the buck.

##### `buffer -@-> parse`

##### `listen -@-> buffer`

##### `listen -@-> buffer -> parse -@-> dispatch`

## New place

- talk a bit about receptiviti
