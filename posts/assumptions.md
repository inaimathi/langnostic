Your language is making an assumption about your work<a name="note-Thu-May-24-134853EDT-2012"></a>[|1|](#foot-Thu-May-24-134853EDT-2012). See if you've noticed it:

**Whatever your program does, it will run in one process, on one machine, inside one network.**

That's not to say that your language prevents you from violating that assumption, but if you do, you'll need to do something <a name="note-Thu-May-24-134901EDT-2012"></a>[|2|](http://www.haskell.org/haskellwiki/GHC/Data_Parallel_Haskell">odd</a>, or something [fraught](http://en.wikibooks.org/wiki/C%2B%2B_Programming/Threading#Processes), or [something](http://en.wikipedia.org/wiki/XML-RPC) [arcane](http://en.wikipedia.org/wiki/SOAP_(protocol)). Some languages also make a further assumption that your program will run on one *core*, which is becoming more and more ridiculous, and the result is a broken or missing threading model<a href="#foot-Thu-May-24-134901EDT-2012).

The "hands-down most interesting thing about Erlang" 
> --Inaimathi is that it does not make this assumption. Each of your component pieces are meant to be created either as sets of functional definitions, or as interlocking, message-passing processes running almost completely independently from one another. If your program needs to run from multiple nodes on the same machine for whatever reason, it uses the `rpc:call/4` I quickly demonstrated [last time](http://langnostic.blogspot.ca/2012/05/erlang-and-barcodes.html). Calls across machines and networks look the same, except that the target node is going to be a foreign IP rather than `127.0.1.1`.

Calling out is just one piece, of course, and it wouldn't work particularly well without a standard communication protocol for Erlang processes to use. XML-RPC or similar could work in other languages, I suppose, but I'd always have this sneaking suspicion that I'm paying more for the channel itself than the messages being sent through. Erlang also provides facilities for process management, the most obvious and useful being supervision trees. Those let you specify monitoring and restarting strategies for the components of your program.

Being a web guy, it's sort of obvious to me that this is a good system structuring strategy.

Normally, most of these things would be handled outside the program. In fact, for the most part, I'm *used* to having to handle them outside of the program. I set up OS-level logging and restarting mechanisms, along with some scripted instruction about what to do in the event of a node failure. You'd need to explicitly define an inter-process communication protocol or grab one of the existing ones, and use it to make sure your system had a measure of node awareness<a name="note-Thu-May-24-135243EDT-2012"></a>[|3|](#foot-Thu-May-24-135243EDT-2012).

For what it's worth, that works. The difference is that it takes more work than specifying it in source, it looks different from the program code and increases external dependencies, and (probably the most egregious from my perspective) it's not in source control by default, and therefore probably being treated as part of the deployment steps rather than as a first class citizen of the program proper. I'm going to mangle [Greenspun's 10th](http://en.wikipedia.org/wiki/Greenspun's_tenth_rule) here for illustrative purposes

Any sufficiently complicated, distributed system contains an ad-hoc, informally specified, bug-ridden, slow implementation of half of Erlang/OTP

It's really all stuff you already know cold. 


- Inter-node communication, complete with heartbeats and the appropriate byte-protocol
- a specified, standard remote procedure call mechanism
- formalized and explicit policy settings about failures
- graceful distribution, logging, deployment and re-deployment (as per the ["hot code swapping"](http://langnostic.blogspot.ca/2012/05/hot-erlang-code.html))


The difference is that you're probably used to it being specified in the surrounding infrastructure and systems rather than in the program itself.

* * *
##### Footnotes

1 - <a name="foot-Thu-May-24-134853EDT-2012"></a>[|back|](#note-Thu-May-24-134853EDT-2012) - It actually doesn't matter what your language is; everything short of Erlang, and some experimental/domain-specific languages built to break this particular assumption, make it.

2 - <a name="foot-Thu-May-24-134901EDT-2012"></a>[|back|](#note-Thu-May-24-134901EDT-2012) - Which is not in and of itself bad, the bad part is that the sysadmin is meant to pick up the slack manually.

3 - <a name="foot-Thu-May-24-135243EDT-2012"></a>[|back|](#note-Thu-May-24-135243EDT-2012) - I've actually only done this once, and it ended up using HTTP for inter-process communication. It solved the specific problem, but something tells me that wouldn't be the easiest thing in the world to scale up.

4 - <a name="foot-Thu-May-24-135530EDT-2012"></a>[|back|](#note-Thu-May-24-135530EDT-2012) - Modulo the usual deployment headaches.
