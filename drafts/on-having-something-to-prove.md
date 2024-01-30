I've been doing a lot more coding and writing than usual lately. I'm not _exactly_ back up at full speed, but I'm moving with a lot more determination than I have in a while. I'm honestly not sure what's changed other than that I have something to move forward with.

The work has mostly been in [`catwalk`](TODO) this time. [Last time](TODO) I mentioned putting together a web interface for it, and I kinda have. By the time you're reading or listening to this, I'll probably have gone through a number of revisions to make it beautiful. At the moment though? This might be the first chunk of code in a very long time I'm not proud of. There's a lot of half-formed thought stuff kicking around my head about this, including requirements I'm only vaguely aware of that suddenly slam into stark relief when I get on with the object level objective of actually producing a blogcast with my tools. I'm _hoping_ to get it seriously smoothed and niced down shortly.

## Catwalk Development Notes

### Database

So, apparently `sqlite3` runs in single-threaded mode by default? I discovered this when I started trying to use it as a state store for my local blogcasting. This definitely isn't an approach that scales. I suspect that it couldn't even handle four concurrent users hitting the same cast, or more than 10 threads on the GPU side. As soon as I did anything even _a bit_ bigger than what I've got going currently, I'd want to switch out to [`redis`](https://redis.io/) or somesuch. However, _at the moment_, for a multi-user site with a use case of "under 100 people, each working on a different job, using between one and three worker threads", it would be perfectly serviceable to run a multi-threaded SQLite setup.

The default configuration gets in my way here but apparently [doesn't _need_ to](https://ricardoanderegg.com/posts/python-sqlite-thread-safety/). Hence, the [`sqlite` adapter for `pytrivialsql`](https://github.com/inaimathi/pytrivialsql/blob/master/src/pytrivialsql/sqlite.py) now checks if the local `sqlite` lib has been [compiled for multi-threaded usage](https://github.com/inaimathi/pytrivialsql/blob/master/src/pytrivialsql/sqlite.py#L52-L62). And, if so, disables the `sqlite3` thread-check on connection start.

### Front-End

The front-end is written in [reagent](https://reagent-project.github.io/). Which, honestly, is a really nice way of organizing front-end code. I haven't repoed it yet because of the earlier noted lack of pride, but keep an eye on this space. The goal is to make it a single-page app that connects to [the server](TODO) but manages a lot of the state and workflow client-side. The most evidence you can see of it right this very second is over in the [`main`](https://github.com/inaimathi/catwalk/blob/master/main.py) module. You can see that there's a new `UIHandler` in place, that I've added a new `jobs` interface in the form of the `JobHandler` and `JobsHandler` classes, and that there's now an exposed WebSocket server sitting at `/v1/jobs/updates`. Spoilers.

One thing I will say is that local state in reagent apps is weird. It recommends that you have a single [top-level state](https://github.com/reagent-project/reagent-cookbook/blob/master/basics/component-level-state/README.md#component-level-state), but also aggressively re-renders the tree when you modify even a tangentially-related piece of top-level state. Which means that if you're dealing with an appreciable number of elements _(I am, thank you)_ and also want your app to run on anything like a usable clock speed _(is that even a question? Yes, goddamit, quit asking stupid questions)_, you _have_ to give individual components intermediate pieces and then aggregate later. Forms are the trickiest bits of this, because implementing them naively means poking at your input state and that triggers the dreaded re-renders.

What I ended up doing was

1. Have a piece of top-level state that represents the server-side objects in the system. When a new websocket update comes in, this is what gets poked. It also triggers a global re-render, but that's almost the only way to keep what the user sees in synch with changes that worker threads or other users make, so whatever.
2. Wherever a user needs to interact with something, have a separate, local piece of state that deals with their input. So like, if there's a `textarea` or `checkbox` that they need to interact with, it's default state is taken from the above global state, but local changes are put into a local atom in order to localize re-renders as much as possible.
3. In the odd case where I need to aggregate local state for `form` purposes, have a piece of intermediate state that each local component reports into, in addition to its local state. This doesn't need to be updated on every user interaction, only when an update is sent to the server, and it also doesn't need to be represented anywhere in the UI thus eliminating more re-renders.

Possibly there's a simpler way to do this, and I'll keep an eye out for how to accelerate interactions further, but it works Well Enough For Now.

### Websocket Channel

[`catwalk`](TODO) still runs on [`tornado`](TODO). Which is weird about messages to clients from separate threads. This is something I absolutely needed to crunch through, because the entire _point_ of the websocket connection in this project is updating the user regarding the activity of the `worker` threads. So they _have_ to be able to send/receive from separate threads.

In order to resolve that, I actually had to end up subclassing `tornado.websocket.WebSocketHandler`?

```python
class SocketServer(tornado.websocket.WebSocketHandler):
    CLIENTS = set()
    IOloop = tornado.ioloop.IOLoop.current()

    def __init__(self, *args):
        super().__init__(*args)
        SocketServer.IOloop = tornado.ioloop.IOLoop.current()

    def open(self):
        SocketServer.CLIENTS.add(self)

    def close(self):
        SocketServer.CLIENTS.remove(self)

    @classmethod
    def send_message(cls, message):
        msg = json.dumps(message)
        print(f"UPDATING {len(cls.CLIENTS)} WS CLIENTS...")
        for client in list(cls.CLIENTS):
            try:
                client.write_message(msg)
            except tornado.websocket.WebSocketClosedError:
                cls.CLIENTS.remove(client)

    @classmethod
    def send_job_update(cls, job):
        if job is None:
            return
        cls.IOloop.asyncio_loop.call_soon_threadsafe(
            cls.send_message,
            {
                "job_id": job["id"],
                "job_type": job["job_type"],
                "status": job["status"],
                "parent": job["parent_job"],
                "input": job["input"],
                "output": job["output"],
            },
        )
```

As you can see, there's class-level state and a couple class methods involved here. It works, in the sense that I've run it and tested out the front end by interacting with it as I pleas for a while. But I haven't found a satisfying explanation for why this limitation is there, so I can't shake the feeling that I'm opening myself up for weird distributed-system-style race conditions here. My guess and hope is that this is just an incidental outgrowth of `tornado` being a non-blocking server, so they just incidentally never bothered dealing with threads even though there's nothing explicitly preventing it. The name `call_soon_threadsafe` is suggestive of a routine that works gracefully under these conditions. Fingers crossed I guess.

I'm going to do a bit more work on the front end, explore a couple other use cases for `catwalk`, and maybe take another run up [the clojurescript-on-android hill](https://cljsrn.org/). It looks like a couple new options have arisen since last I checked.

As always, I'll let you know how it goes.
