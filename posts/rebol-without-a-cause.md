
So Thursday was this months' [Code Retreat](https://bentomiso.com/events/toronto-code-retreat-july) over at [Bento](https://bentomiso.com/). We were solving [the Poker Hands kata](http://codingdojo.org/cgi-bin/wiki.pl?KataPokerHands) that I've [already written about](http://langnostic.blogspot.ca/2012/08/partial-poker-hand-kata-in-common-lisp.html), so [Gaelan](https://github.com/NaleagDeco) and [I](https://github.com/Inaimathi) decided to make an attempt using [REBOL3](https://github.com/rebol/r3). Because I've already [written about it](http://langnostic.blogspot.ca/2012/08/partial-poker-hand-kata-in-common-lisp.html), I'm not going to explain the problem, or go very deeply into code-review-style exposition.

I'll show you some REBOL3 code, point out the highlights and the confusing bits, and call it a day. Hit up the [chat room](http://chat.stackoverflow.com/rooms/291/rebol-and-red) if you have questions.

```lisp

REBOL []

map-f: func [ fn a-list ] [
    res: make block! 5
    foreach elem a-list [ append res do [fn elem] ]
    res    
]

group: func [ a-list ] [
    res: make map! 5
    foreach elem a-list [
        either res/(elem)
        [ poke res elem res/(elem) + 1 ]
        [ append res reduce [ elem 1 ]]
    ]
    res
]

test-hand: [[ 1  hearts ] [ 2 clubs ] [ 3 clubs ] [ 4 diamonds ] [ 5 hearts ]]
test-flush: [[ 1  hearts ] [ 2 hearts ] [ 3 hearts ] [ 4 hearts ] [ 5 hearts ]]

group-by-rank: func [ hand ] [
    group map-f func [ a ] [ first a ] hand
]

group-by-suit: func [ hand ] [
    group map-f func [ a ] [ second a ] hand
]

is-flush: func [ hand ] [
    1 = length? group-by-suit hand
]

is-pair: func [ hand ] [
    grouped: group-by-rank hand
    foreach k grouped [
        if grouped/(k) = 2
    ]
]
```

Our first attempt was pretty pathetic, all things considered. Most of that comes down to lack of familiarity with the language, and a desire on my part to do things functionally. The first meant that we spent about 15 minutes trying to figure out how to set the value of a particular map slot<a name="note-Sat-Jul-27-122302EDT-2013"></a>[|1|](#foot-Sat-Jul-27-122302EDT-2013). The second meant that I had to implement a couple of basics myself, one of which I was used to having provided even in batteries-not-included languages like Common Lisp. The above isn't actually a valid approach because of r3's default scope. Which means

```
>> do %poker-hands.r
do %poker-hands.r
Script: "Untitled" Version: none Date: none
>> res: "Foobarbaz"
res: "Foobarbaz"
== "Foobarbaz"

>> map-f func [ a ] [ a + 1 ] [ 1 2 3 4 5 ]
map-f func [ a ] [ a + 1 ] [ 1 2 3 4 5 ]
== [2 3 4 5 6]

>> res
res
== [2 3 4 5 6]

```

Don't worry; there's a way around this which I'll discuss later. After the event, I made a few refinements and got it up to

### <a name="the-second-crack"></a>The Second Crack

```rebol
REBOL []

fn: make object! [
    map: func [ fn a-list ] [
        res: make block! 5
        foreach elem a-list [ append res do [fn elem] ]
        res
    ]
    range: func [ start end ] [
        res: make block! 10
        step: either start &lt; end [ 1 ] [ -1 ]
        for i start end step [ append res i ]
        res
    ]
    frequencies: func [ a-list ] [
        res: make map! 5
        foreach elem a-list [
            either res/(elem)
            [ poke res elem res/(elem) + 1 ]
            [ append res reduce [ elem 1 ]]
        ]
        res
    ]
    val-in?: func [ val map ] [
        foreach k map [
            if map/(k) = val [ return true ]
        ]
        return false
    ]
]

hands: make object! [
    straight: [[ 1  hearts ] [ 2 clubs ] [ 3 clubs ] [ 4 diamonds ] [ 5 hearts ]]
    straight-flush: [[ 1  hearts ] [ 2 hearts ] [ 3 hearts ] [ 4 hearts ] [ 5 hearts ]]
    pair: [[ 2  hearts ] [ 2 clubs ] [ 3 clubs ] [ 4 diamonds ] [ 5 hearts ]]
    two-pair: [[ 2  hearts ] [ 2 clubs ] [ 3 clubs ] [ 3 diamonds ] [ 5 hearts ]]
]

ranks: func [ hand ] [ fn/map func [ a ] [ first a ] hand ]
suits: func [ hand ] [ fn/map func [ a ] [ second a ] hand ]

count-ranks: func [ hand ] [ fn/frequencies ranks hand ]
count-suits: func [ hand ] [ fn/frequencies suits hand ]


has-flush: func [ hand ] [
    1 = length? group-by-suit hand
]

has-straight: func [ hand ] [
    rs: sort ranks hand
    rs = fn/range rs/1 (rs/1 + (length? rs) - 1)
]

has-straight-flush: func [ hand ] [
    all [ has-straight hand has-flush hand ]
]

has-group-of: func [ size hand ] [
    fs: count-ranks hand
    fn/val-in? size fs
]

has-pair: func [ hand ] [ has-group-of 2 hand ]
has-three: func [ hand ] [ has-group-of 3 hand ]
has-four: func [ hand ] [ has-group-of 4 hand ]
has-two-pair: func [ hand ] [
    fs: fn/frequencies values-of count-ranks hand
    2 = fs/2
]
has-full-house: func [ hand ] [ all [ has-pair hand has-three hand ]]
```

Not much trouble taking that step, once I kind of sort of got what I was doing, but I'd be coding along and occasionally get invalid argument errors. And it would *always* turn out to be a problem with the separation of arguments and calls. It happened in quite a few places, but the worst offender was

```rebol
has-straight: func [ hand ] [
    rs: sort ranks hand
    rs = fn/range rs/1 (rs/1 + (length? rs) - 1)
]
```

That line starting with `rs = `, specifically. Initially, it read `rs = fn/range rs/1 rs/1 + length? rs - 1`. Interpreter says: WTFYFWWYETT?<a name="note-Sat-Jul-27-122315EDT-2013"></a>[|2|](#foot-Sat-Jul-27-122315EDT-2013). What the snippet means is what you can read from the parenthesized version above. That is,


>   Apply the function `fn/range` to the argument "`rs/1`" and the argument "one less than the `length?` of `rs` added to `rs/1`".


This is probably an expressive edge-case, but it's slightly concerning that I ran into it so soon. That scope issue is still outstanding, by the way. `Object!`s don't have internal scope by default either, which begs the question of why they're called "Objects", so the net effect is still the same.

```
>> do %poker-hands.r
do %poker-hands.r
Script: "Untitled" Version: none Date: none
>> res: "Foobarbaz"
res: "Foobarbaz"
== "Foobarbaz"

>> fn/map func [ a ] [ a + 1 ] [ 1 2 3 4 5 ]
fn/map func [ a ] [ a + 1 ] [ 1 2 3 4 5 ]
== [2 3 4 5 6]

>> res
res
== [2 3 4 5 6]
```

Anyhow, it technically runs. As long as you don't nest `map` or `frequency` calls. After a trip over to the [Rebol/Red chat room on SO](http://chat.stackoverflow.com/rooms/291/rebol-and-red) for some quick review by actual rebollers<a name="note-Sat-Jul-27-122340EDT-2013"></a>[|3|](#foot-Sat-Jul-27-122340EDT-2013), I got to

### <a name="the-third-crack"></a>The Third Crack

```rebol
REBOL []

fn: context [
    map: funct [ fn a-list ] [
        res: make block! 5
        foreach elem a-list [ append/only res do [fn elem] ]
        res
    ]
    range: funct [ start end ] [
        res: make block! 10
        step: either start &lt; end [ 1 ] [ -1 ]
        for i start end step [ append res i ]
        res
    ]
    frequencies: funct [ a-list ] [
        res: make map! 5
        foreach elem a-list [
            either res/(elem)
            [ poke res elem res/(elem) + 1 ]
            [ append res reduce [ elem 1 ]]
        ]
        res
    ]
    val-in?: funct [ val map ] [
        foreach k map [
            if map/(k) = val [ return true ]
        ]
        return false
    ]
]

hands: make object! [
    straight: [ &#9829;/1  &#9827;/2  &#9827;/3  &#9830;/4  &#9824;/5 ]
    straight-flush: [ &#9829;/1  &#9829;/2  &#9829;/3  &#9829;/4  &#9829;/5 ]
    pair: [ &#9829;/2  &#9827;/2  &#9827;/3  &#9830;/4  &#9824;/5 ]
    two-pair: [ &#9829;/2  &#9827;/2  &#9827;/3  &#9830;/3  &#9824;/5 ]
]

read-hand: func [ hand-string ] [
    suits-table: [ #"H" &#9829;  #"C" &#9827;  #"D" &#9830;  #"S" &#9824; ]
    ranks-table: "--23456789TJQKA"
    fn/map func [ c ] [
        to-path reduce [ 
            select suits-table c/2 
            offset? ranks-table find c/1 ranks-table ]
    ] parse hand-string " "
]

ranks: func [ hand ] [ fn/map func [ c ] [ probe second c] hand ]
suits: func [ hand ] [ fn/map func [ c ] [ probe first c ] hand ]

count-ranks: func [ hand ] [ fn/frequencies ranks hand ]
count-suits: func [ hand ] [ fn/frequencies suits hand ]


has-flush: func [ hand ] [
    1 = length? group-by-suit hand
]

has-straight: func [ hand ] [
    rs: sort ranks hand
    rs = fn/range rs/1 (rs/1 + (length? rs) - 1)
]

has-straight-flush: func [ hand ] [
    all [ has-straight hand has-flush hand ]
]

has-group-of: func [ size hand ] [
    fs: count-ranks hand
    fn/val-in? size fs
]

has-pair: func [ hand ] [ has-group-of 2 hand ]
has-three: func [ hand ] [ has-group-of 3 hand ]
has-four: func [ hand ] [ has-group-of 4 hand ]
has-two-pair: func [ hand ] [
    fs: fn/frequencies values-of count-ranks hand
    2 = fs/2
]
has-full-house: func [ hand ] [ all [ has-pair hand has-three hand ]]
```

Note that the definitions of `fn`, and in particular `fn/map` have changed subtly. The change to `fn` in general is that each of its functions is now a `[funct](http://www.rebol.com/r3/docs/functions/funct.html)` instead of just a [`func`](http://www.rebol.com/r3/docs/functions/func.html). This is the solution to that scope problem from earlier; `funct` provides an implicit scope for its body block where `func` doesn't. Meaning that if you define `fn` in this new way, you can now actually do

```
>> res: "Foobarbaz"
res: "Foobarbaz"
== "Foobarbaz"

>> fn/map func [ a ] [ a + 1] [ 1 2 3 4 5 ]
fn/map func [ a ] [ a + 1] [ 1 2 3 4 5 ]
== [2 3 4 5 6]

>> res
res
== "Foobarbaz"

>> 
```

and you can safely nest `fn/map`/`fn/frequencies` calls.

The other subtle change to `fn/map` specifically is that it now uses `append/only` rather than `append`. The reason for this is that `append` implicitly splices its arguments. That is

```
>> do %poker-hands.r ;; map defined with plain append
do %poker-hands.r ;; map defined with plain append
Script: "Untitled" Version: none Date: none
>> read-hand "1H 2C 3C 4D 5S"
read-hand "1H 2C 3C 4D 5S"
== [&#9829; 1 &#9827; 2 &#9827; 3 &#9830; 4 &#9824; 5]

>> do %poker-hands.r ;; changed to append/only
do %poker-hands.r ;; changed to append/only
Script: "Untitled" Version: none Date: none
>> read-hand "1H 2C 3C 4D 5S"
read-hand "1H 2C 3C 4D 5S"
== [&#9829;/1 &#9827;/2 &#9827;/3 &#9830;/4 &#9824;/5]

>> 
```

Apparently the original author found that he was doing sequence splicing more than actual `append`ing. But instead of writing a separate `[splice](http://www.rebol.com/r3/docs/functions/splice.html)` function, or maybe a `/splice` refinement to `append`, he made splicing `append`s' default behavior. No, I have no idea what he was smoking at the time.

In order to get the behavior you'd probably expect from plain `append`, you have to run the refinement `/only`, which as far as I can tell, generally means "do what you actually wanted to do" on any function it's provided for. A guy calling himself [Hostile Fork](http://hostilefork.com/) [says it](https://docs.google.com/presentation/d/1pmDE7eNsZtFb3ey8KXvNz8RF21NXZeFqvDKfR5D-dI4/edit#slide=id.geceb89b9_0210) better than I could:


>   We don't tell someone to take out the garbage and then they shoot the cat if you don't say *"Oh...wait... I meant `ONLY` take out the garbage"*! The name `ONLY` makes no semantic sense; if it *did* make sense, then it's what should be done by the operation without any refinements!
> --Hostile Fork


### <a name="afermath"></a>Afermath

So that's that. I didn't get to a working solution yet, because this script doesn't compare two hands to determine a winner (or a draw), and it doesn't handle the aces-low edge case, but I'll leave those as an exercise for the reader. It'll tell you what hand you have, and it can elegantly read the specified input. At the language level, REBOL3 is interesting. And the community is both enthusiastic and smart. And I really hope the r2/3 transition gives them the excuse to clean up the few counter-intuitive things that slipped in over time. It's enough that I'm making an addition to the logo bar, which I don't do lightly<a name="note-Sat-Jul-27-122350EDT-2013"></a>[|4|](#foot-Sat-Jul-27-122350EDT-2013).

This series of tinkering had no particular cause. I was just playing around with a problem I had lying around in a language I was curious about. Next time, I'll pick one, probably some kind of lightweight application server, and see how far I can push it. Hopefully that doesn't get too far in the way of my [LISP](http://lispinsummerprojects.org/) project...

* * *
##### Footnotes

1 - <a name="foot-Sat-Jul-27-122302EDT-2013"></a>[|back|](#note-Sat-Jul-27-122302EDT-2013) - Using `[poke](http://www.rebol.com/r3/docs/functions/poke.html)`, in case you're curious.

2 - <a name="foot-Sat-Jul-27-122315EDT-2013"></a>[|back|](#note-Sat-Jul-27-122315EDT-2013) - What The Fuck You Fucker, Why Would You Ever Type That?

3 - <a name="foot-Sat-Jul-27-122340EDT-2013"></a>[|back|](#note-Sat-Jul-27-122340EDT-2013) - I have no idea why they don't just call themselves "rebels".

4 - <a name="foot-Sat-Jul-27-122350EDT-2013"></a>[|back|](#note-Sat-Jul-27-122350EDT-2013) - PHP logo notwithstanding.
