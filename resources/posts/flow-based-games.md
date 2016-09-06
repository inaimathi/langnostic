So I just tried to write what I thought was a fairly simple game prototype tentatively titled "Gratuitous Resource Gathering" using [Elm](http://elm-lang.org/). Something in the style of [Cookie Clicker](http://orteil.dashnet.org/cookieclicker/)(DO NOT click that link if you were planning on doing anything today). Here's how far I got:

```haskell
import Mouse
import Time

port resourceA : Signal Bool
port building : Signal String

buildingCost = foldp (+) 0 <| keepWhen (lift2 (>) (constant 50) balance) 0 <| sampleOn building <| constant 50

tickIncrement = foldp (+) 0 <| sampleOn buildingCost <| constant 0.01
tick = sampleOn (every Time.millisecond) <| constant True

spent = foldp (+) 0 <| merges [ sampleOn building buildingCost ]

gathered = foldp (+) 0 <| merges [ sampleOn resourceA <| constant 1
                                 , sampleOn tick tickIncrement ]

balance = lift round <| lift2 (-) gathered spent

main = lift (flow down) <| combine [ lift asText balance ]
```

That's almost the complete game, except for one very annoying detail: it doesn't work. When I try to run it in the appropriate HTML harness, I get

```
s2 is undefined
    Open the developer console for more details.
```

The reason is that `buildingCost` signal. When the user purchases a building, I need to check whether they have enough resource balance to buy it. However, the `balance` is the sum of two other signals, `gathered` and `spent`, the second of which *is affected by building purchases*. Looks like circular signals aren't a thing in Elm right now. I'm not sure how to resolve this inside the language, and I already told you all about ports last time, so my natural first reaction was to think about how I'd go about computing that price check outside the Elm module. Unfortunately, once I started mentally pulling things out of Elm, I quickly arrived at the conclusion that the whole thing would probably need to be turned inside-out. In other words, I'd be using Elm purely as a way of avoiding manual DOM manipulation in one or two components of a mostly Javascript project.

Maybe that'd still be worth it, but it feels quite unsatisfying.

No real idea what to do about it though. I'll talk to some people I consider smarter than me and see what they think. Hopefully there's a reasonable way around the problem that doesn't include doing most of it in manual JS.

In the meantime, I hacked together something in [Daimio](https://github.com/dxnn/daimio).

```
outer
        @resource-click dom-on-click resource
        @building-click dom-on-click building
        @show dom-set-html display

        @timer every-half-second

        $building-cost 5
        $click-increment 1
        $tick-increment 0
        $balance 0

        inc-balance 
                { __ | add $balance | >$balance }
        dec-balance 
                { __ | subtract value __ from $balance | >$balance }

        can-afford
                { __ | ($building-cost $balance) | max | eq $balance }
        
        @resource-click -> {__ | $click-increment } -> inc-balance -> @show
        @building-click -> can-afford -> { __ | then $building-cost else 0} -> dec-balance -> @show
                           can-afford -> { __ | then 10 else 0 | add $tick-increment | >$tick-increment }
                           can-afford -> { __ | then "Buying..." | tap }

        @timer -> {__ | $tick-increment } -> inc-balance -> { __ | $balance } ->  @show
```

No highlighting mode for that one yet; I'm workin' on it. Also, the above was kind of non-trivial because I had to add my own timer event, *and* I still want to figure out how to factor out the process of buying a building. But it works well enough on my machine. I'll post the full project up to my [github](https://github.com/Inaimathi) once I do a bit more thinking about it.
