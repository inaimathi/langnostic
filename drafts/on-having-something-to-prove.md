I've been doing a lot more coding and writing than usual lately. I'm not _exactly_ back up at full speed, but I'm moving with a lot more determination than I have in a while. I'm honestly not sure what's changed other than that I have something to move forward with.

The work has mostly been in [`catwalk`](TODO) this time. [Last time](TODO) I mentioned putting together a web interface for it, and I kinda have. By the time you're reading or listening to this, I'll probably have gone through a number of revisions to make it beautiful. At the moment though? This might be the first chunk of code in a very long time I'm not proud of. There's a lot of half-formed thought stuff kicking around my head about this, including requirements I'm only vaguely aware of that suddenly slam into stark relief when I get on with the object level objective of actually producing a blogcast with my tools. I'm _hoping_ to get it seriously smoothed and niced down shortly[^and-to-be-perfectly-honest].

[^and-to-be-perfectly-honest]: And, to be perfectly honest, that's mostly done at the time of this writing. I've been coding _a lot_ more than I've been blogging lately. To the point where I think I have content for two or three more blog posts each time I actually reflect on what I've been doing. Specifically, there's been a bunch of movement on the catwalk front end, and some more light mobile development that I'll have to fill you in on in the next couple installments.

## Catwalk Development Notes

### Database

So, apparently `sqlite3` runs in single-threaded mode by default? I discovered this when I started trying to use it as a state store for my local blogcasting. This definitely isn't an approach that scales. I suspect that it couldn't even handle four concurrent users hitting the same cast, or more than 10 threads on the GPU side. As soon as I did anything even _a bit_ bigger than what I've got going currently, I'd want to switch out to [`redis`](https://redis.io/) or somesuch. However, _at the moment_, for a multi-user site with a use case of "under 100 people, each working on a different job, using between one and three worker threads", it would be perfectly serviceable to run a multi-threaded SQLite setup.

The default configuration gets in my way here but apparently [doesn't _need_ to](https://ricardoanderegg.com/posts/python-sqlite-thread-safety/). Hence, the [`sqlite` adapter for `pytrivialsql`](https://github.com/inaimathi/pytrivialsql/blob/master/src/pytrivialsql/sqlite.py) now checks if the local `sqlite` lib has been [compiled for multi-threaded usage](https://github.com/inaimathi/pytrivialsql/blob/master/src/pytrivialsql/sqlite.py#L52-L62). And, if so, disables the `sqlite3` thread-check on connection start.

### Front-End

The front-end is written in [reagent](https://reagent-project.github.io/). Which, honestly, is a really nice way of organizing front-end code. I haven't repoed it yet because of the earlier noted lack of pride, but keep an eye on this space. The goal is to make it a single-page app that connects to [the server](TODO) but manages a lot of the state and workflow client-side. The most evidence you can see of it right this very second is over in the [`main`](https://github.com/inaimathi/catwalk/blob/master/main.py) module. You can see that there's a new `UIHandler` in place, that I've added a new `jobs` interface in the form of the `JobHandler` and `JobsHandler` classes, and that there's now an exposed WebSocket server sitting at `/v1/jobs/updates`.




- Local state in reagent apps is weird. It recommends that you have a single top-level state, but also aggressively re-renders the tree when you modify even a tangentially-related piece of top-level state. Which means that if you're dealing with an appreciable number of elements (I am, thank you) and also want your app to run on anything like a usable clock speed (is that even a question? Yes, goddamit, now quit asking stupid questions), you _have_ to give individual components intermediate pieces and then aggregate later. Forms are the trickiest bits of this, because implementing them naively means poking at your input state and that triggers the dreaded re-renders.
- Websockets are weird in tornado if you want to send messages to clients from a worker thread (which I do, because the entire point of the websocket in my case is to keep the front end updated about the state of individual jobs going through the system)

TODO - Tour of server goes here:


Possibly tour of client if it's done? I feel like that should be its own piece though.
