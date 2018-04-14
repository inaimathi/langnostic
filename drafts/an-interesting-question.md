So I was posed an interesting question recently, which is going to take some research and modelling. In case you were wondering, this is the sort of thing I generally get paid to do. So lets step through it and see what we can see.

## How much energy does verified communication cost?

That's the essence, in any case. The more precise set of questions is

1. How much energy does it cost to perform a one-way hash? Something in the `SHA2` family, because we want some level of security.
2. How much energy does it cost to produce one ECDSA signature on `1k` of data?
3. How much energy does it cost to verify an ECDSA signature once on `1k` of data?
4. How much energy does it cost to send a TCP message of `1k` of data from New York, US to London, UK.

We want these numbers to be reasonably hardware and implementation independent, which means they're going to be fairly fuzzy. Even without that constraint, asking about the performance, by any metric, of network calls between heterogeneous clients is going to be a very imprecise exercise.

## Step 1: Cheat

First, lets see if we can get a cache hit out of this. I highly doubt I'm the first person ever wrestling with this question (although that would be pretty cool, in all honesty). A cursory google search gets me

- [A performance comparison between different hashing functions](https://automationrhapsody.com/md5-sha-1-sha-256-sha-512-speed-performance/)
- [A paper on smart-card cryptosystems in mobile devices, and their energy consumption characteristics](http://research.ijcaonline.org/volume66/number19/pxc3886260.pdf)
- [A proposal to improve wireless network performance by taking a different approach to ECDSA](https://pdfs.semanticscholar.org/f43d/e2d69e51123393a417215780f3fd1327695e.pdf)
- [A paper on TCP energy consumption in ad-hoc networks](https://pdfs.semanticscholar.org/52e9/97b59705bb1c0f76a9c72ff16a70a7f8820e.pdf)

Each of those has some useful components of the answer we want, but nothing outright answering the same question.

Google Scholar has a few additional interesting hits:

- [Computational and Energy Costs of Cryptographic Algorithms on Handheld Devices](http://mdpi.com/1999-5903/3/1/31/htm)
- [Computational Energy Cost of TCP](http://www.ruf.rice.edu/~mobile/elec518/readings/wirelesscom/wang04infocom.pdf)
- [Efficient Implementation of NIST-Compliant Elliptic Curve Cryptography for Sensor Nodes](https://link.springer.com/chapter/10.1007/978-3-319-02726-5_22)

The last of these is behind a paywall but, conveniently for me, because the paper focuses on ECC _implementations_, the energy consumption information I'm after is present in the cleartext abstract. According to it, the energy cost of a signature and verification is 46 mJ (for a process that takes 1.91 s on a chip with a clock frequency of 7.37 MHz). It's not strictly speaking relevant, but the same abstract also claims that ECDH can be executed for 42 mJ at 1.75 s on the same setup.

So we've got a first datapoint there, in any case. It's hardware and presumably implementation-specific, and the experimental process isn't outlined in the part of the paper I can read, so there's no telling how accurate this is, but it's a start (and it's not as though I'm about to print answers without verifying anyhow).

## Step 2: Do the work

Ok; so we want to see the energy costs of some `SHA256` implementation, `ECDSA` signing and verification, and a `TCP` message. This sounds like a job for a profiler of some sort. Or rather, kinda. A profiler will tell us how much memory and compute is used
