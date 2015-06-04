First things first, there was finally a sale on [these things](http://www.newegg.ca/Product/Product.aspx?Item=N82E16820147189&nm_mc=KNC-GoogleAdwordsCA&cm_mmc=KNC-GoogleAdwordsCA-_-pla-_-Internal+SSD-_-N82E16820147189), and I was getting sick of the [jury-rigged Vertex in my current laptop](http://langnostic.blogspot.ca/2011/12/x220-and-unrelatedly-portable-keyboards.html), so I picked one up. They're back up at ~$170, but mine cost me a much more attractive $90 last week.

![](/static/img/new-hard-drive.jpg)

![](/static/img/new-hard-drive--angle-2.jpg)

First impressions are ok.

I'm not going to endorse SATA3 for anyone, because it's honestly not a mind-blowing change. I mean load times are noticeably faster, but the difference isn't anything like what you get from switching to a SATA2 SSD from a traditional platter drive. If you end up trading up to this from one of the slower SSDs, be prepared to hear yourself say "Oh, that's nice" rather than "This is fucking glorious". Honestly, my purchase was a result of the fact that this sale price came under my may-as-well threshold for hardware and that my previous HD was *literally* held in by rubber stoppers and electrical tape.

## <a name="dissecting-squares" href="#dissecting-squares"></a>Dissecting Squares

I'm blaming [Dann](http://www.meetup.com/Toronto-Code-Retreat/members/2372148/) for this one [again](http://langnostic.blogspot.ca/2012/12/life-common-lisp-haskell-and-clojure.html). Here's the problem we decided to go after for this months' [Toronto Code Retreat](http://www.meetup.com/Toronto-Code-Retreat/):

Suppose you have a grid of squares `n`x`n` large. There is a finite number of ways you can split that square grid up into squares of at least 1x1 size. For example, for a 1x1 grid, there is only `1` possible dissection. For a 2x2 grid, there are `2` (either 4 1x1 squares or 1 2x2 square). For a 3x3 grid there are `6`; (either 9 1x1 squares, 1 3x3 square, or 4 separate combinations of 1 2x2 square and 5 1x1 squares). This is the Number of Square Dissections.

![](/static/img/square-dissections.png)

*(A [complete illustration of the first four terms](http://oeis.org/A224239/a224239_4.jpg) is over in the OEIS page. There's [also](http://oeis.org/A224239/a224239_5.jpg) a [further](http://oeis.org/A224239/a224239_6.jpg) three [pages](http://oeis.org/A224239/a224239_7.jpg) illustrating the fifth term. This gets big in a hurry.)*

The challenge:


-   **"Easy" Mode:** Write a function that takes a number `N` and returns the Number of Square Dissections of an `N` by `N` grid.
-   **"Hard" Mode:** Write a second function with the same input as above that returns the number of **unique**, under rotation and reflection, dissections.


Before you chuckle at this, bear in mind that a general solution is publishable<a name="note-Fri-Apr-19-210429EDT-2013"></a>[|1|](#foot-Fri-Apr-19-210429EDT-2013). According to Dann, the sequence is [solved up to an `N` of 15](http://oeis.org/A045846) or so, and that's already some ungodly number best expressed by exponents.

A couple of people took a stab at working "Hard" mode solutions<a name="note-Fri-Apr-19-210437EDT-2013"></a>[|2|](#foot-Fri-Apr-19-210437EDT-2013), but as far as I know, no one managed to get to a working solution for "Easy" mode. This is the first Code Retreat I've gone to where I've felt deeply inadequate as a programmer, and that the most popular tool "a notebook" rather than a particular programming language.

As to the problem, I tried a bunch of approaches, none of which seemed to do anything other than annoy me. In terms of problem-solving, I'm usually a data-representation guy. Maybe it's the design degree making me focus on that, I don't fucking know, but it works fairly well. Given a thorny problem, my first reflex is to figure out a way of storing the data such that the mechanical components of the solution are going to be as simple as possible with no more moving parts than necessary. Which is why my train of thought as the problem was coming in was something like...

> Can we do this as a naive area calculation? No, that actually fails on the 3x3 grid. We need to represent area *and* position, or we might represent the remaining area of the board in a more explicit way. Too complicated. Ok, we need to fit things into other things; is this actually a factoring problem? No, we're looking for the count of unique ways *of* factoring a thing rather than actual factorization. Doing it that way sounds like it would be computationally expensive. Can we model it as a tree of possible square placements? We'd need to represent each square as a `width x y` triple, at minimum, starting with `(list (- n 2) 0 0)` and working from there. But there are multiple interconnected combinations that can start with each 2x2 square on a 4x4 grid, and we can't actually prune a lot of sub-branches. Can we do this as a graph traversal? Sounds pretty go... wait, no. No, we wouldn't be visiting each node once. In fact, we can't know a-priori how many times a given node would have to be visited, or what the endpoints of any valid traversal are. Can we model this as an inventory system? Each grid has yae space and adding a square depletes it; when you get stuck, fill the rest with ones, each fill is a different dissection, count the results. Wait, isn't that going to be `O^scary^fucking-scary`? You need to generate all possible inventory combinations *and* de-duplicate. I guess you could keep a set of dissections somewhere and keep a canonical sorting order for placed squares. That still implies a representation that tracks position *and* size rather than just a size and a count. Is it possible that a 2D grid is actually a better representation for this one?  
> --Inaimathi's brain

As it happens, sitting down and throwing together a simple program to test some of these didn't do anything to help. I'd start, get about a third of the way through, then realize that I wasn't accounting for this or that edge case. Or I'd figure out that edge-cases were covered, but the approach doesn't scale up to a general solution. The most promising I've got so far, and this is nowhere near a working solution yet, looks like

```lisp
(defun positions (board-size square-size)
  (loop for x from 0 to (- board-size square-size)
     append (loop for y from 0 to (- board-size square-size)
               collect (list square-size x y))))

(defun next-squares (board-size starting-square disregard)
  (destructuring-bind (sq-size sq-x sq-y) starting-square
    (loop for current-sq-size from sq-size downto 2
         append (loop for x from sq-x 
                   append (loop for y from sq-y
                             when (and (or (= x sq-size) (= y sq-size)) 
                                       (not (member (list current-sq-size x y) disregard :test #'equal))) 
                             collect (list current-sq-size x y)
                             until (>= (+ sq-size y) board-size))
                   until (>= (+ sq-size x) board-size)))))

(defun tree-out (board-size list-of-squares)
  (loop for sq in list-of-squares
     collect sq into dis
     collect (next-squares board-size sq dis)))
```

which is godawful, and doesn't work yet, but it isn't obvious that it *can't* work so I'm rolling with it. The (very) general high-level approach is starting from the largest possible sub-squares of a given grid and doing a depth-first placement traversal. Each successful placement will then be sorted and stored in a set, which will ensure uniqueness<a name="note-Fri-Apr-19-210451EDT-2013"></a>[|3|](#foot-Fri-Apr-19-210451EDT-2013). There's a few places where we can definitely prune possibilities, but not as many as I thought there would be, which means that this will ultimately be a fairly expensive operation in any case.

Ugh. This isn't where I was planning on sinking the next week or so of my free time. I *wanted* to say a few words about Raspberry Pi hacking, and about its GPIO facilities specifically, or to talk a bit more about Wai in the context of large web applications, or maybe finally unveil my Plan For World Domination™© centered around building a general HTML-based MMO, but no.

I got a meme on me. So instead, I'll be thinking about squares.

And how they fit into each other.


* * *
##### Footnotes

1 - <a name="foot-Fri-Apr-19-210429EDT-2013"></a>[|back|](#note-Fri-Apr-19-210429EDT-2013) - A proof that it's an NP-complete problem in the general case would also be noteworthy apparently.

2 - <a name="foot-Fri-Apr-19-210437EDT-2013"></a>[|back|](#note-Fri-Apr-19-210437EDT-2013) - That is, taking the list of all dissections of a given grid size and filtering out the rotationally/reflectively unique ones.

3 - <a name="foot-Fri-Apr-19-210451EDT-2013"></a>[|back|](#note-Fri-Apr-19-210451EDT-2013) - This can all happen in a single traversal, though there might be some opportunity for easy parallelism depending on how the implementation shakes out.
