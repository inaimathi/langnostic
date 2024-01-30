So here's an idea. Probably as old as networks, but I might have the chance to use it in production in a bit, so what the hell.

Suppose you had a task that you needed to perform remotely across some number of servers. You'd need to put together a model of your problem that lets it get decomposed into individual requests and sent out. Unless you had a way of sending these requests asynchronously, you'd get no time gain from decomposing. So you'd better have that. This seems like it would be relatively trivial in [Clojure](TODO) or [Javascript](TODO), but for whatever reason, I need to do it in [Python](TODO).

So.

```
def remote_map(fn, reqs, loop=None, tries=1):
    if not loop:
        loop = asyncio.get_event_loop()
    return loop.run_until_complete(_remote_map(fn, reqs, tries=tries))
```

At the top level, this should be a non-`async` function, because we eventually want to integrate it into a synchronous system. Which means, we'll be running a bunch of tasks, either on a given event loop or at the top-level.

```
async def _remote_map(fn, reqs, tries=1):
    async with aiohttp.ClientSession() as session:
        return await asyncio.gather(*[remote_apply(session, fn, r, tries=tries) for r in reqs])
```

Internally, the task we'll be running is the `gather` of running a call to `remote_apply`. We're deferring any work, and any retries to the `remote_apply` operation, and sharing a single `ClientSession` among all requests we'll be making.


```
async def remote_apply(session, fn, req, tries=1):
    async with session.request(**req) as response:
        stat = response.status
        if (stat == 200) or (tries <= 1):
            res = await response.read()
            return fn({"status": stat, "headers": dict(response.headers), "body": res})
        else:
            remote_apply(session, fn, req, tries=tries - 1)
```

`remote_apply` is the only even remotely complicated functoin we're dealing with here. It makes a request specified by `req`, with the given `session`, making sure to try a total of `tries` times.
