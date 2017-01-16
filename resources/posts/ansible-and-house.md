## Ansible

So I've been using [Ansible](https://www.ansible.com/) for the past little while. At work, I mean. I don't deploy anything that requires anywhere near the amount of scale it would take for a tool like this to start paying for itself, so I probably wouldn't have picked it up at all if someone weren't paying me for the experience. Having now used [`nix`](http://nixos.org/nix/), [`ansible`](https://www.ansible.com/), and [`chef`](https://www.chef.io/chef/), what strikes me about them is how many insist on a new syntax. I'm curious why `ansible` couldn't have been implemented as a library in whatever turing complete language. With functions and environments rather than a giant directory tree of `yml` files arranged by "convention"[^as-a-side-note].

[^as-a-side-note]: As a sidenote; the credo of "convention over configuration" has at this point been used to force users to memorize arbitrary garbage as an alternative to having the system designers sit down and think through the principles of their system in a reasonable way. I call this lazy fucking design, and I'm not content to put up with it quietly anymore. The more convention your system requires, the shittier it is on an absolute scale, there I said it.

On the one hand, this would possibly alienate X programmers who don't program your X. On the other, a well-chosen X would give some advantages, non-X programmers wouldn't have to put up with garbage like YML config files[^lisp-weenie-digression], and X programmers whose X was [probably coincidentally](/posts/self-titled) well-chosen mildly happier. That's probably the trade-off happening here; ease-of-use vs. likelyhood-of-adoption, so I won't linger on it too much. My only problem with Ansible remains the gobsmacking amount of boilerplate necessary to do small things.

[^lisp-weenie-digression]: Lisp weenie digression here, by the way. The only reason YML/XML/CONF/WTFBBQ files are needed at all is that the languages making use of them have shitty enough syntaxes that you can't express meaningful configuration _in_ them. As you may have heard by now, Lisp doesn't have this problem, which makes certain "trivially solvable" issues from other languages vanish entirely, and makes certain impossible issues from other languages tractable. I'll get into more depth on that at some point in the future, I'm sure.

In summary, I see the need to formalize a deployment process in a machine-manipulable way, and very much agree that the best way to do this is to write scripts that assume the bare minimum of prior infrastructure. I doubt that the syntactic choices made in any of the existing tools are optimal, or even adequate, and I don't at all understand why the outcome is something other than a function of `TargetServer -> IO InstallationResult`, potentially paired with an `IO [AWSInstance]` in the case of fleet deployment[^ok-ok-lets-be-honest]. Before too long, I'll want to do something about this.

[^ok-ok-lets-be-honest]: Ok, ok, lets be honest, you also want to be able to control the topology of the resulting network in anything but the trivial cases. Even given that requirement, the existing tools seem to be far too complicated for the task they're meant to complete.

## House updates

If you've been keeping an eye on the [`house` repo](https://github.com/inaimathi/house), you'll notice that I've been making updates at an increased pace lately. Mostly, this is so that I can have a better base on which to continue development of [`cl-congregate`](https://github.com/inaimathi/cl-congregate), but it's also the culmination of about a year and a half of background-processing I've been doing on various network-programming-related problems relating to the construction of a decent HTTP server. Most recently, I [ran through the first](/posts/house-performance) of several intended rounds of performance optimizations.

There are two ideas I had there that didn't get developed as far as I'd like, so you know what time it is.

### Spike-Specific Optimizations

One thing I consiedered is changing some of the session-related functions to become probabilistic. Ultimately, I _didn't_ do this because of the security implications, but it also got me thinking about how such modifications would behave. I have a theory that, under heavy enough load, there would be insignificant behavioral difference between cleaning out global session state more or less frequently to some threshold of frequency. One of the things we could do to improve performance where it really counts is to develop a series of optimizations that take advantage of this fact.

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

`house` is currently a single-threaded system. And while I don't have much interest in going thread-per-request[^except-as-an-experiment], convenient as that would be from the implementation perspective, there is room to use more than one thread to take advantage of the inherent parallelizability of the problem we've got. The existing system does _all_ of its work in the single, central thread. That includes `listen`ing, `buffer!`ing, `parse`ing, `handle`ing and `write!`ing. And really, most of it needn't be done that way. I think we're stuck `listen`ing in one thread[^though-there-is], but the rest of the processing pipeline can be delegated if that's what we end up needing.

[^except-as-an-experiment]: Except as an experiment. Which I very probably _should_ run, so that I can validate my theory about the relative performance of non-blocking and thread-per-request servers. To be fair, I may end up getting surprised, but figure that the Common Lisp thread system is too heavy-weight to pull off the kind of numbers that that would require.

[^though-there-is]: Though there is also the option of [preforking](http://aosabook.org/en/posa/warp.html) on modern linux implementations. I haven't thought about this at all, except to note that it is possible and should be considered as a future improvement to `house`.

The real trouble is balancing single vs multi-threaded performance here. While we _do_ want to take advantage of more horsepower when we're running on servers that have it, we also don't want to _require_ more than one thread for proper operation, because that would prevent us from realistically servicing the [raspberry](https://www.creatroninc.com/product/raspberry-pi-3/)-pi-[ish](https://getchip.com/) use cases.

There are a few different ways I can implement this, depending on how much effort I'm willing to expend and how much payout I'd probably get. The difference between them essentially comes down to where in the chain of

```
listen -> accept -> buffer! -> parse -> dispatch -> handle -> write
                       ^_________|
```

there should be a break-out to multiple threads. We'll use the notation `-@->` to mean "handing off the rest of the process to whichever thread is currently ready to process".

##### `parse -@-> dispatch`

From the implementation perspective, it seems like `parse -@-> dispatch`, `dispatch -@-> handle` and `handle -@-> write` would be equally difficult to put together, so we may as well go with the one that'll give us the most potential bang for the buck. And that seems to be the one that exposes the most parallelism to our worker threads. This is still somewhat of a win, because according to the profiling we did [last time](/posts/house-performance), our current top culprits are

```
measuring PROFILE overhead..done
  seconds  |     gc     |     consed    |    calls   |  sec/call  |  name
----------------------------------------------------------------
     1.577 |      0.000 |    88,051,168 |    511,202 |   0.000003 | HOUSE::WRITE-LN
     1.499 |      0.024 |    43,536,368 |     73,039 |   0.000021 | HOUSE::BUFFER!
     1.136 |      0.020 |   106,240,688 |     73,028 |   0.000016 | HOUSE::HANDLE-REQUEST!
     0.643 |      0.024 |   384,474,768 |    146,056 |   0.000004 | HOUSE::PARSE
     0.346 |      0.056 |    25,075,872 |     73,029 |   0.000005 | HOUSE::WRITE!
     0.312 |      0.012 |    83,617,440 |     73,028 |   0.000004 | HOUSE::NEW-SESSION-TOKEN!
     0.231 |      0.072 |    66,517,264 |     73,028 |   0.000003 | HOUSE:NEW-SESSION!
...
```

So handing off `write-ln` and `write!` to a set of workers would already be a decent win.

The idea here would be to wait until a request is fully buffered before tossing into whatever worker-thread system we've got going. The upside is that this is ridiculously easy to implement. Because `dispatch` and onwards has no interaction with any previous step, all it would entail is throwing the fully parsed `request` into a work queue somewhere and letting whatever worker threads compete for it.

The downside is that it doesn't buy us _anything_ unless we're prepared to have at least three threads total running around. One still needs to be in charge of `listen`/`accept`/`buffer!`/`parse` full-time, and the rest can handle `dispatch` and onwards. We'd also have to pull some fancy footwork to special-case the single-thread scenario because, again, one thread needs to be full-timing `listen` through `parse`, and that means it won't have any time at all left for anything else unless we arrange that at the expense of some performance.

##### `buffer! -@-> parse`

This one's a bit scary because we'd need to de-tangle `buffer!` and `parse`, and that's a tall order. The problem is `POST` requests with `BODY` components. Because that scenario exists, we've actually got to do a `parse` on a request buffer _before we know whether we're done buffering_. Because in order to know how much space to allocate to the request body, and therefore when to cut off the incoming request read, we must look into the headers of the request for the `Content-Length` specification. This implies one of

1. A [streaming parser](https://github.com/inaimathi/cl-lazy-parse)
2. Hitting the incoming `request` with `parse` _as part of `buffer!`_
3. Complicating, and very probably significantly slowing, the interaction between `buffer!` and the main event loop

In `house`, I made the decision to run with `#2`, since it was the simplest thing, but it does mean that any thread a `buffer!`ed request gets passed to might have to `buffer!` again. Which either means resolving that interdependence, _or_ it means implementing separate mini-event-loops inside of each worker (since we don't want to have them block on slowpokes). What's more, we need to implement those event loops very carefully indeed, because we _also_ don't want there to be a situation where a new client is added to a workers' purview only to have said worker be busy blocking on `select` due to lack of activity.

The specific situation we'll need to avoid is

1. Request A gets finished buffering and is pushed onto the ready queue
2. The single worker thread pulls it, and determines that its `BODY` needs to be buffered
3. The related client starts slowing down, and buffering blocks for some amount of time
4. While #3 is in progress, Request B gets finished buffering and is pushed onto the ready queue

At that point, if we've only done our work naively, Request B has to wait for Request A to either complete or time out. And that's exactly what we don't want. To top it all off, we're still stuck with the downside that this really only buys us anything if we have three or more threads up and running, for the same reasons as the `parse -@-> dispatch` approach.

##### `accept -@-> buffer`

Mildly less scary implementation-wise than `buffer! -@-> parse`, but has a similarly low payout. Nothing too interesting to see here.

##### `listen -@-> accept`

Here, things get a bit interesting.

Because the situation wherein we establish a `listen`ing socket, then pass the results out to each worker thread round-robin style would allow us to take advantage of all the parallelism inherent in the problem. Since the `server` socket has the same `select`-like interface as a `client` socket we get out of `accept`, this approach scales naturally to any number of threads, including one. And this comes at the cost of a, hopefully minimal, central dispatch structure that would let us add new sockets into the thick of things.

We'd need minimal changes to the dispatch structure; a sequence of `socket` tables rather than a single central one, and some of the processing steps would have to pick one of the tables on offer by some metric rather than always passing in the only option. The sequence of `socket` tables would have one element per thread we're scaling to, and each element would be passed to a different thread. It seems to be up in the air whether the `server` socket should be passed along to the next thread each time it fires, or kept with the first one. The former would open us up to some subtle timing issues when dealing with slow clients, while the latter would mean that a particular thread would have significantly more load than the rest. We could probably compensate for that by giving it proportionally fewer connections to deal with.

Anyway, that particular decision is a complete aside. Given that the `listen -@-> accept` approach gives us the maximal payout in terms of exposed parallellism, _and_ gives us the smallest amount of work to do, all of which is restricted to `start` and `process-ready`, this one sounds like a winner.

Next time I've _got_ time to throw at performance optimizations for `house`, I'll try putting this together and kicking the tires. As always, I'll let you know how it goes.
