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

Ok; so we want to see the energy costs of some `SHA256` implementation, `ECDSA` signing and verification, and a `TCP` message. This sounds like a job for a profiler of some sort. Or rather, kinda. A profiler will tell us how much memory and compute is used, but not necessarily how much juice. So we'll need to figure something out. My gut reaction says to use this as an excuse to learn about profiling in Clojure, but realistically, we'll want to do similar things against multiple implementations (and on multiple machines). So, here we go, off the top of my head,

```clojure
(ns clocking.core
  (:require [clojure.java.io :as io]
            [digest :as d]
            [clj-pgp.core :as pgp]
            [clj-pgp.generate :as pgp-gen]
            [clj-pgp.signature :as pgp-sig]

            [taoensso.tufte :as tufte :refer [p profiled profile]]))

;;;;; Dummy Data
(defn random-string [len]
  (apply str (take len (repeatedly #(char (+ (rand 26) 102))))))

(defn fresh-keypair []
  (pgp-gen/generate-keypair (pgp-gen/ec-keypair-generator "secp160r2") :ecdsa))

;;;;; Basic profiling
(tufte/add-basic-println-handler! {})
(defn sha256 [inp] (p :sha256 (d/sha-256 inp)))
(defn ecdsa [keypair inp]
  (let [sig (p :ecdsa-sign (pgp-sig/sign inp keypair))]
    (p :ecdsa-verify (pgp-sig/verify inp sig keypair))))

;;;;; Battery status
(def bat "/sys/class/power_supply/BAT0/")
(defn bslurp [name] (clojure.edn/read-string (slurp (str bat name))))
(defn charging-status []
  (keyword (clojure.string/lower-case (clojure.string/trim (slurp (str bat "status"))))))
(defn battery-status []
  {:charge {:now (bslurp "charge_now") :full (bslurp "charge_full") :design (bslurp "charge_full_design")}
   :current {:now (bslurp "current_now")}
   :voltage {:now (bslurp "voltage_now") :min (bslurp "voltage_min_design")}
   :capacity (bslurp "capacity") :status (charging-status)})

;;;;; Profiling
(defn profile! [ct]
  (let [inputs (take ct (repeatedly #(random-string 1000)))
        keypair (fresh-keypair)]
    (println (count inputs))
    (let [before (battery-status)]
      (profile
       {} (doseq [inp inputs]
            (sha256 inp)
            (ecdsa keypair inp)))
      {:before before :after (battery-status)})))
```

[tufte](https://github.com/ptaoussanis/tufte) is a pretty good profiling library, [clj-digest](https://github.com/tebeka/clj-digest) is an implementation of some digest hash functions, and [clj-pgp](https://github.com/greglook/clj-pgp) is a library that gives us access to ECDSA signing/verification via [BouncyCastle](https://www.bouncycastle.org/). I've included [clj-sockets](https://github.com/atroche/clj-sockets) as the TCP implementation, but haven't actually done anything with it yet. Also, since I'm on Debian, I can poke at `sysfs` to [get battery statistics](https://blog.sleeplessbeastie.eu/2013/01/02/debian-how-to-monitor-battery-capacity/) and hopefully back out energy costs from there.

The gist of the above example is that last function.

```
(defn profile! [ct]
  (let [inputs (take ct (repeatedly #(random-string 1000)))
        keypair (fresh-keypair)]
    (println (count inputs))
    (let [before (battery-status)]
      (profile
       {} (doseq [inp inputs]
            (sha256 inp)
            (ecdsa keypair inp)))
      {:before before :after (battery-status)})))
```

1. Generate a bunch of input data .
2. Force the input strings sequence. _(Clojure is sometimes lazy, and I don't want it caching digest results. It would improve performance, but wouldn't end up giving me an accurate cost model. So instead of serially calling `sha256` or `sign`/`verify` on the same input string, we're generating a long-assed sequence of inputs, forcing it by hitting it with `count`, then calling the appropriate crypto functions on each one in turn)_
3. Capture the `before` state of the battery
4. Profile the appropriate crypto functions
5. Report the `before` and `after` state of the battery

```
clocking.core> (profile! 100000)
100000
{:before {:charge {:now 5968000, :full 7163000, :design 8400000}, :current {:now 2435000}, :voltage {:now 11682000, :min 11100000}, :capacity 83, :status :discharging}, :after {:charge {:now 5916000, :full 7163000, :design 8400000}, :current {:now 2286000}, :voltage {:now 11700000, :min 11100000}, :capacity 82, :status :discharging}}

           pId      nCalls        Min        Max        MAD       Mean   Time%        Time

 :ecdsa-verify     100,000   436.72μs    21.09ms    16.10μs   469.59μs      61     46.96s
   :ecdsa-sign     100,000   232.81μs   100.51ms    12.42μs   248.28μs      32     24.83s
       :sha256     100,000    41.86μs     9.93ms     2.31μs    44.60μs       6      4.46s

    Clock Time                                                             100      1.28m
Accounted Time                                                              99      1.27m

clocking.core>
```

That's about what I expected on a small-scale test. But two things

1. We need a larger test set
2. Because this is battery power on a laptop, I kind of want to know what baseline drain is in addition to the testing drain. Hopefully, I can back out some accurate numbers from the delta.

```
clocking.core> (profile! 1000000)
OutOfMemoryError GC overhead limit exceeded  java.lang.Character.toString (Character.java:4636)
clocking.core>
```

Hmph. I guess forcing the full list up-front is kind of memory intensive. I still don't really want to incur the overhead of generating this list in-line with the test though. I guess risking cache is the lesser evil for now? Or at least, lets do both evils and see where we can factor them out.

```
(defn profile! [ct]
  (let [inp (random-string 1000)
        keypair (fresh-keypair)]
    (let [before (battery-status)]
      (profile
       {} (dotimes [_ ct]
            (sha256 inp)
            (ecdsa keypair inp)))
      {:before before :after (battery-status)})))
```


```
clocking.core> (profile! 1000000)

           pId      nCalls        Min        Max        MAD       Mean   Time%        Time

 :ecdsa-verify   1,000,000   433.54μs    27.67ms    14.40μs   469.96μs      61      7.83m
   :ecdsa-sign   1,000,000   232.17μs    14.93ms     8.74μs   246.32μs      32      4.11m
       :sha256   1,000,000    41.99μs    11.06ms     2.06μs    44.36μs       6     44.36s

    Clock Time                                                             100     12.75m
Accounted Time                                                              99     12.68m

{:before {:charge {:now 4875000, :full 7163000, :design 8400000}, :current {:now 1259000}, :voltage {:now 11491000, :min 11100000}, :capacity 68, :status :discharging}, :after {:charge {:now 4360000, :full 7163000, :design 8400000}, :current {:now 2007000}, :voltage {:now 11216000, :min 11100000}, :capacity 60, :status :discharging}}
clocking.core>
```

Ok, that still only gives us half the story. It tells us about how much energy ECDSA and SHA256 take out of this equation. There's another component we wanted to discuss, which is the TCP component. After a few commits which I won't rehash here, we can take a stab at answering that question.

```
clocking.core> (profile! 10000)

           pId      nCalls        Min        Max        MAD       Mean   Time%        Time

 :ecdsa-verify      10,000   321.01μs    70.34ms    59.25μs   453.67μs      39      4.54s
   :ecdsa-sign      10,000   253.40μs   521.14ms   147.48μs   386.85μs      33      3.87s
     :tcp-send      10,000   130.68μs    28.21ms    52.10μs   206.71μs      18      2.07s
       :sha256      10,000    52.94μs    46.42ms    19.59μs    78.33μs       7    783.31ms

    Clock Time                                                             100     11.68s
Accounted Time                                                              96     11.26s

{:before {"/sys/class/power_supply/BAT1" {:energy {:now 37170000, :full 47860000, :design 47520000}, :power {:now 18652000}, :voltage {:now 11507000, :min 10800000}, :capacity 77, :status :discharging}, "/sys/class/power_supply/BAT0" {:energy {:now 22260000, :full 23170000, :design 23480000}, :power {:now 0}, :voltage {:now 12435000, :min 11400000}, :capacity 96, :status :unknown}}, :after {"/sys/class/power_supply/BAT1" {:energy {:now 37070000, :full 47860000, :design 47520000}, :power {:now 20635000}, :voltage {:now 11357000, :min 10800000}, :capacity 77, :status :discharging}, "/sys/class/power_supply/BAT0" {:energy {:now 22260000, :full 23170000, :design 23480000}, :power {:now 0}, :voltage {:now 12435000, :min 11400000}, :capacity 96, :status :unknown}}}
```

Ok, so far so good. You'll notice I'm also doing this on a laptop with a different battery configuration; I do like being at least minimally complete. Upping the count past 100k in this case runs my machine out of memory, possibly because of all the dangling sockets I'm leaving around. Which kind of sucks, but we can get better data than 10k, at least.

```
clocking.core> (profile! 40000)
{:before {"/sys/class/power_supply/BAT1" {:energy {:now 23540000, :full 47860000, :design 47520000}, :power {:now 24860000}, :voltage {:now 10753000, :min 10800000}, :capacity 49, :status :discharging}, "/sys/class/power_supply/BAT0" {:energy {:now 22260000, :full 23170000, :design 23480000}, :power {:now 0}, :voltage {:now 12433000, :min 11400000}, :capacity 96, :status :unknown}}, :after {"/sys/class/power_supply/BAT1" {:energy {:now 23080000, :full 47860000, :design 47520000}, :power {:now 26716000}, :voltage {:now 10661000, :min 10800000}, :capacity 48, :status :discharging}, "/sys/class/power_supply/BAT0" {:energy {:now 22260000, :full 23170000, :design 23480000}, :power {:now 0}, :voltage {:now 12433000, :min 11400000}, :capacity 96, :status :unknown}}}

           pId      nCalls        Min        Max        MAD       Mean   Time%        Time

 :ecdsa-verify      40,000   324.21μs   146.39ms   108.14μs   577.92μs      42     23.12s
   :ecdsa-sign      40,000   251.15μs   877.47ms   123.02μs   426.50μs      31     17.06s
     :tcp-send      40,000   126.90μs     1.41s     90.62μs   239.04μs      17      9.56s
       :sha256      40,000    50.36μs   569.40ms    38.04μs    96.91μs       7      3.88s

    Clock Time                                                             100     55.36s
Accounted Time                                                              97     53.61s

clocking.core>
```
