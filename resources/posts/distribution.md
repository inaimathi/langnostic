So yeah. I said I'd only be considering the implications of already having a [fully distributed server](https://github.com/EveryBit-com/everybit.js), rather than the platform itself, but I couldn't help myself.

It turns out the [everybit](https://github.com/EveryBit-com/everybit.js) team is stuck on this side of a fully connected graph. To that end, I've been reading up on some related [papers](http://pdos.csail.mit.edu/papers/chord:sigcomm01/chord_sigcomm.pdf), [techniques](https://en.wikipedia.org/wiki/Kademlia) and [technologies](https://www.usenix.org/legacy/publications/compsystems/1994/sum_long.pdf). Having thought about it, I'm no longer sure that a fully de-centralized publishing/messaging platform is a thing you can do. The problem has to do with data integrity, rather than security or throughput limitations.

While it's possible to arrange your network so that it's easy to find a given IDs' most adjacent node quickly, there isn't a guarantee that your ID will be found, even if it has existed at some time in the past. Because node failure is a thing. It is *especially* a thing when your nodes are actually clients making requests. And in the event of the uncontrolled failure of a node that has a record of the data you're requesting, you're going to get back a failed query even though you found the "right" node to ask.

This doesn't just hit naive systems. For any such network with `n` layers of replication, `n` node failures can force the absence of some piece of data.

I'm not convinced this is all bad from the [publishing](https://github.com/EveryBit-com/Starfish) perspective. You'd want unpopular, or historic data to go away eventually, and letting it happen implicitly seems pretty elegant. Things change when you start thinking about the point-to-point messaging side of the equation. By definition, that involves data that's only of interest to some very small group of clients. Possibly as small as one.

A possible solution here is to use multiple retreival strategies.

So we could use a DHT with some replication to keep the standard publishing messages (maybe defaulting to a flood query if that fails), and use an alternate routing strategy for targeted messages.

## Publishing

Interesting, but nothing problematic here, really. Standard DHT implementation distributed among client nodes, maybe in multiple rings so that we have some data replication at the cost of a little extra client space. We can resort to querying every node if we fail to find a piece of data; it'll take longer, but might generate a hit where we'd otherwise miss. Remember, because of our use case, it's entirely possible for nodes to have copies of data that they're not responsible for hosting according to whatever DHT scheme we end up picking. This still doesn't guarantee presence of all data forever (a record will eventually effectively die if few enough nodes remember it). If we wanted *that*, we'd need to introduce some kind of archival system with high uptime and a low failure rate, which implies some kind of server, centralized or not.

## Point-to-point

This is where problems arise with brute replication strategies. The issue is that we don't want point-to-point messages going away until they're delivered. It doesn't even particularly matter if they're delivered to *everyone* in the case of multiple recipients; as long as some targets gets the message, others can eventually coordinate to get it from those client nodes. The disaster scenario is that a message is sent while no targets are active in the system, and delivery fails. So, conceptually, what we could do in this situation is keep an encrypted `undelivered` crate getting stored and replicated in the publishing side of our system. The meta-problem is that I'm not entirely sure whether this approach, or anything like it, would keep transmission reliability *and* privacy preservation for the participants. Because if we have this `undelivered` system, we need to track the delivery status of a message.

## Non-conclusion

The brief thoughts above ignore storage of user authentication machinery, and various "server"-stored metadata. And the approaches I'm considering all have some exposed attack vectors, but I don't really see how to solve the problem in a satisfactory manner. It's possible that the best trade-off is actually "the system will not be fully reliable", and I should accept that rather than trying to patch its reliability.

So step one kinda failed. I couldn't peel this idea on my own. I guess step two is asking people smarter than me to chat about it.

I'll let you know how it goes.
