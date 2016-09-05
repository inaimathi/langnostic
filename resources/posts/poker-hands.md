I mentioned the [Toronto Coding Dojo](http://www.meetup.com/Toronto-Coding-Dojo/#calendar) last week<a name="note-Tue-Aug-28-182259EDT-2012"></a>[|1|](#foot-Tue-Aug-28-182259EDT-2012). Specifically, I mentioned trying to go over [the Poker Hand Kata](http://codingdojo.org/cgi-bin/wiki.pl?KataPokerHands) from scratch each week in Clojure.

We haven't solved it yet, but we're getting there. Half the point is getting to know the language, and the TDD technique, so it's not as though failing to get to the end is the worst possible thing, really. I'm warming to the language, but not the technique (more on that next time).

We *were* supposed to have a dojo github page, but there doesn't seem to be a link going out from the meetup, and I can't find it after ten minutes of determined googling, so I can't point you to it. I have, however taken first stabs at the problem in three languages and want to go over the problem a bit.

> EDIT:  
> Turns out the organizer is keeping the progress repos in [his GitHub profile](https://github.com/tlalexan).  
> Fri, 31 Aug, 2012  

Fundamentally, it's a sorting problem. We have cards, whose relevant properties are a `rank` and a `suit`. We have an ordered set of hand types, each of which have their own tie-braking method with other hands of the same type. The task, near as I can tell, is taking a pair of hands, figuring out their types, then sorting them to find out the winner<a name="note-Tue-Aug-28-182537EDT-2012"></a>[|2|](#foot-Tue-Aug-28-182537EDT-2012).

The constructs we need to represent here are ranks, suits, cards (which is just a `(rank suit)` combo) and hands (which are just lists of cards). Here's my first first stab in Common Lisp<a name="note-Tue-Aug-28-182701EDT-2012"></a>[|3|](#foot-Tue-Aug-28-182701EDT-2012).

```lisp
;; poker-hands.lisp

(defpackage :poker (:use :cl :split-sequence))
(in-package :poker)

(defparameter *letter->val* '(#\T 10 #\J 11 #\Q 12 #\K 13 #\A 14))
(defparameter *hand-type->val* '(:high-card 1 :pair 2 :two-pairs 3 :three-of-a-kind 4 
                                 :straight 5 :flush 6 :full-house 7 
                                 :four-of-a-kind 8 :straight-flush 9))

(defclass card ()
  ((rank :reader rank :initarg :rank)
   (suit :reader suit :initarg :suit)))

(defun read-card (card-string)
  (make-instance 'card
                 :rank (or (getf *letter->val* (aref card-string 0))
                           (parse-integer card-string :junk-allowed t))
                 :suit (aref card-string 1)))

(defun read-hand (hand-string)
  (sort (mapcar #'read-card (split-sequence #\space hand-string))
        #'> :key #'rank))

(defun flush-p (cards) 
  (let ((suits (mapcar #'suit cards)))
    (every (lambda (s) (eq s (car suits))) (cdr suits))))

(defun range (start end)
  (loop for i from start to end collect i))

(defun straight-p (cards)
  (equal (mapcar #'rank cards)
         (loop repeat (length cards) 
            for i from (rank (car cards)) downto 0
            collect i)))

(defun find-sets (cards)
  (let ((copy (copy-list cards)))
    (loop for c in copy
       when (remove (rank c) cards :key #'rank :test-not #'=) collect it
       do (setf cards (delete (rank c) cards :key #'rank)))))

(defun set-of-p (n sets)
  (some (lambda (s) (= (length s) n)) sets))

(defun count-sets-of (n sets)
  (count-if (lambda (s) (= (length s) n)) sets))

(defun hand-type (hand)
  (let ((sets (find-sets hand)))
    (cond ((and (flush-p hand) (straight-p hand)) :straight-flush)
          ((set-of-p 4 sets) :four-of-a-kind)
          ((and (set-of-p 3 sets) (set-of-p 2 sets)) :full-house)
          ((flush-p hand) :flush)
          ((straight-p hand) :straight)
          ((set-of-p 3 sets) :three-of-a-kind)
          ((= 2 (count-sets-of 2 sets)) :two-pairs)
          ((set-of-p 2 sets) :pair)
          (t :high-card)))) 

(defmethod break-tie (hand-type (hand-a list) (hand-b list))
  (loop for a in hand-a
        for b in hand-b
        unless (= (rank a) (rank b))
          do (return (> a b))))

(defun hand-type-> (hand-type-a hand-type-b)
  (> (getf *hand-type->val* hand-type-a)
     (getf *hand-type->val* hand-type-b)))

(defun hand-> (hand-a hand-b)
  (let ((type-a (hand-type hand-a))
        (type-b (hand-type hand-b)))
    (or (hand-type-> type-a type-b)
        (when (eq type-a type-b)
          (break-tie type-a hand-a hand-b)))))
```

Not bad for about 20 minutes of work. I punt on the `break-tie` method at the bottom there, opting to just compare high cards until someone wins. Like I said, that really should be doing something else; for instance, if we have two three-of-a-kind hands, we'd want to compare the set of three as opposed to the high cards. Once we've got the hands read into an easier format, we can test `flush-p`, which takes a list of cards and checks if they've all got the same suit, and `straight-p`, which takes a list of cards and checks if they constitute a run.

`read-card` takes a two-character string and returns a new `card` based on it. A `card` is just a rank attached to a suit. `read-hand` takes the specified hand string format, and returns a list of cards from it. Finally, we've got `hand-type->` and `hand->`, which compare hand types and hands respectively<a name="note-Tue-Aug-28-182849EDT-2012"></a>[|4|](#foot-Tue-Aug-28-182849EDT-2012).

It's minimal, and it doesn't *really* solve the problem, but I'm already familiar with the CL way of doing things, so I didn't want to spend any more time on this one than I really had to.

On we go to

```clojure
;; poker-hands.clj

(ns poker-hands.core
  (:use [clojure.string :only [split]]))

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})
(def name-map ["Rules for Draw and Stud Poker" "Ace" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine" "Ten" "Jack" "Queen" "King" "Ace"])
(def suit-map {\H :hearts \C :clubs \S :spades \D :diamonds})
(def hand-map {:straight-flush 8 :four-of-a-kind 7 :full-house 6 :flush 5 :straight 4 :three-of-a-kind 3 :two-pairs 2 :pair 1 :high-card 0})

(defn read-card [card-string]
  (let [rank (or (get rank-map (first card-string)) (read-string (subs card-string 0 1)))
        suit (get suit-map (second card-string))
        name (get name-map rank)]
    {:rank rank :suit suit :name name}))

(defn read-hand [hand-string]
  (sort-by :rank (map read-card (split hand-string #" "))))

(defn flush? [cards]
  (= 1 (count (group-by :suit cards))))

(defn straight? [cards]
  (let [ranks (map :rank cards)]
    (= ranks (range (first ranks) (+ 1 (last ranks))))))

(defn group-of? [n sets]
  (some #(= (count (second %1)) n) sets))

(def four-of-a-kind? (partial group-of? 4))
(def three-of-a-kind? (partial group-of? 3))
(def pair? (partial group-of? 2))

(defn count-sets-of [n sets]
  (count (filter #(= (count (second %1)) n) sets)))

(defn hand-type [hand]
  (let [sets (group-by :rank hand)]
    (cond (and (straight? hand) (flush? hand)) :straight-flush
          (four-of-a-kind? sets) :four-of-a-kind
          (and (three-of-a-kind? sets) (pair? 2 sets)) :full-house
          (flush? hand) :flush
          (straight? hand) :straight
          (three-of-a-kind? sets) :three-of-a-kind
          (= 2 (count-sets-of 2 sets)) :two-pairs
          (pair? sets) :pair
          :else :high-card)))

(defn break-tie [hand-a hand-b] true)

(defn hand-> [hand-a hand-b]
  (let [type-a (hand-type hand-a)
        type-b (hand-type hand-b)]
    (or (apply > (map #(get hand-map %) [type-a type-b]))
        (when (= type-a type-b)
          (break-tie hand-a hand-b)))))
```

The Clojure version took me a bit longer since I'm still at the stage of having to code with a reference open, and I don't even have `clojure-slime` set up to give me argument hints. As I assumed though; there aren't really big conceptual differences between this one and the CL version. It's more compact by about 20 lines, but that's almost entirely due to the fact that Clojure has built-in `range` and `group-by` functions, which I had to define myself in the previous take.

The only other real difference is that there aren't any classes here, since Clojure encourages `map` and `vector` use instead. That's helped a bit by implicit indexing<a name="note-Tue-Aug-28-183106EDT-2012"></a>[|5|](#foot-Tue-Aug-28-183106EDT-2012) and `lambda` shorthand<a name="note-Tue-Aug-28-183114EDT-2012"></a>[|6|](#foot-Tue-Aug-28-183114EDT-2012). Note that this already handles card names, rather than just ranks.

`partial` is what Clojure calls [currying](https://en.wikipedia.org/wiki/Currying), and those three functions are there for readability in the `hand-type` body.

The part that I'm pointedly *not* showing here because it would be really boring, is the ~60 line set of test cases the group wrote up for this little program, as part of the construction process. Mostly, they were things like making sure that the `read` functions returned appropriate values from appropriate-looking strings, and specifying the basic functionality of how different hand types are coordinated and ranked.

On that note, here's the third (and final) stab I'm posting today

```haskell
-- poker-hands.hs

import Data.String
import Data.List
import Data.Ord

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine 
          | Ten | Jack | Queen | King | Ace 
          deriving (Eq, Ord, Show, Bounded, Enum)
                   
instance Read Rank where
  readsPrec _ value =
    let tbl = zip "23456789TJQKA" [Two .. Ace]
    in case lookup (head value) tbl of
      Just val -> [(val, tail value)]
      Nothing -> error $ "\nInvalid rank: " ++ value

data Suit = H | C | D | S deriving (Eq, Ord, Show, Read)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Ord, Show)

instance Read Card where
  readsPrec _ value =    
    [(Card (read r :: Rank) (read s :: Suit), drop 2 value)]
    where r = init value
          s = snd $ splitAt (length r) value
          
data Hand = Hand { handRank :: HandRank, cards :: [Card] } 
          deriving (Eq, Show, Ord)

instance Read Hand where
  readsPrec _ value =
    [(Hand (getHandRank res) res, "")]
    where res = reverse . sort . map read $ words value :: [Card]

data HandRank = HighCard [Rank] 
              | Pair [Rank]
              | TwoPair [Rank]
              | ThreeOfAKind [Rank]
              | Straight [Rank]
              | Flush [Rank]
              | FullHouse [Rank] 
              | FourOfAKind [Rank] 
              | StraightFlush [Rank]
              deriving (Eq, Ord, Show)

isFlush :: [Card] -> Bool
isFlush cards = (1==) . length . group $ map suit cards

isStraight :: [Card] -> Bool
isStraight cards = 
  let rs = sort $ map rank cards
      run = [(head rs) .. (last rs)]
  in rs == run

getHandRank cards =
  let ranks = reverse . sort $ map rank cards
      uniqueRanks = nub ranks
      rankGroups = sortByLen $ group ranks
      handRank = case cards of
        _ | (isFlush cards) && (isStraight cards)  -> StraightFlush
          | has4 rankGroups                        -> FourOfAKind
          | (has3 rankGroups) && (has2 rankGroups) -> FullHouse
          | isFlush cards                          -> Flush
          | isStraight cards                       -> Straight
          | has3 rankGroups                        -> ThreeOfAKind 
          | (countGroupsOf 2 rankGroups) == 2      -> TwoPair
          | has2 rankGroups                        -> Pair
          | otherwise                              -> HighCard
  in handRank uniqueRanks

-------------------------------
-- General Utility Functions --
-------------------------------
hasGroupOf :: Int -> [[a]] -> Bool
hasGroupOf n groups = n `elem` (map length groups)
has4 = hasGroupOf 4
has3 = hasGroupOf 3
has2 = hasGroupOf 2

countGroupsOf :: Int -> [[a]] -> Int
countGroupsOf n groups = length $ filter (\g -> length g == n) groups

sortByLen :: [[a]] -> [[a]]
sortByLen = sortBy (flip $ comparing length)
```

Haskell is... odd. It's up there in the language bar because I poke at it rather vigorously with some frequency, but I've yet to do anything serious with it. I like it, but I always get the feeling that it doesn't like me very much.

This one took me a while. I'd bet it was between three and four hours. First, re-reading some of the documentation I'd already gone through as a refresher, then going through a bunch of reference docs to find particular function names<a name="note-Tue-Aug-28-183200EDT-2012"></a>[|7|](#foot-Tue-Aug-28-183200EDT-2012), and finally writing the actual program.

It contains a few lines more than the Common Lisp solution, and about 20 more than the Clojure piece, but I'll cut it some slack for two reasons in this case. First, because those type signatures and declarations effectively replace between 90% and 95% of those boring test cases I mentioned. And second, because unlike the Lisp approaches, this one is complete apart from printing the output and one piece of input procedure.

That is, if you hand it a pair of hand strings and run `compare`, you'll get back the correct answer, down to the last tie breaker<a name="note-Tue-Aug-28-183206EDT-2012"></a>[|8|](#foot-Tue-Aug-28-183206EDT-2012).

I use `instance Read` to declare readers for `Rank`, just but derive `Read` on `Suit` outright. Those two compose to let us read `Card`s and `Hand`s as well. All of these types derive `Ord`, because the whole point is sorting them, and rank also derives `Bounded` and `Enum` so that I have an easier time of expressing a range of cards.

Once all the types are declared, the rest of the program just kind of falls out. You can see more or less the same `flush` and `straight` detectors, and even the same structure in `getHandRank` (except that it's named differently).

What you don't see is any boilerplate surrounding hand comparisons. Or, in fact, any comparison functions at all. We *sort* cards twice<a name="note-Tue-Aug-28-183224EDT-2012"></a>[|9|](#foot-Tue-Aug-28-183224EDT-2012), but that's it. Because those types are defined deriving among other things `Ord`, you can use all the standard comparison operators to do the rest.

I was going to say a few proper words comparing the approaches and languages here, but this piece is already quite a bit longer than I'd like it to be. It'll have to wait for next time<a name="note-Tue-Aug-28-183322EDT-2012"></a>[|10|](#foot-Tue-Aug-28-183322EDT-2012).

* * *
##### Footnotes

1 - <a name="foot-Tue-Aug-28-182259EDT-2012"></a>[|back|](#note-Tue-Aug-28-182259EDT-2012) - Heads up if you were planning on joining us, by the way, they're holding [a poll](http://www.meetup.com/Toronto-Coding-Dojo/polls/620482/) on what day next weeks' meeting should be held. If you weren't there yet, and your reason was "I'm not free that day", you may want to give your opinion a voice.

2 - <a name="foot-Tue-Aug-28-182537EDT-2012"></a>[|back|](#note-Tue-Aug-28-182537EDT-2012) - There's also a bit of incidental complexity around displaying the winners after that, that I'll ignore for now.

3 - <a name="foot-Tue-Aug-28-182701EDT-2012"></a>[|back|](#note-Tue-Aug-28-182701EDT-2012) - It's what I'm comfortable with. Also, note that all these tries were written before I started writing this post, so they have less thought in them than they otherwise might.

4 - <a name="foot-Tue-Aug-28-182849EDT-2012"></a>[|back|](#note-Tue-Aug-28-182849EDT-2012) - I only implemented one direction, since the problem at hand doesn't call for more.

5 - <a name="foot-Tue-Aug-28-183106EDT-2012"></a>[|back|](#note-Tue-Aug-28-183106EDT-2012) - As seen in that `group-by` call in `hand-type`.

6 - <a name="foot-Tue-Aug-28-183114EDT-2012"></a>[|back|](#note-Tue-Aug-28-183114EDT-2012) - As seen in `group-of?`, `count-sets-of` and probably a couple of other places.

7 - <a name="foot-Tue-Aug-28-183200EDT-2012"></a>[|back|](#note-Tue-Aug-28-183200EDT-2012) - Hoogle helps immensely once you get your head around the type system, but I'd really like to have access to it on my local machine, along with proper auto-completion and type signature hinting.

8 - <a name="foot-Tue-Aug-28-183206EDT-2012"></a>[|back|](#note-Tue-Aug-28-183206EDT-2012) - Just as an aside though, I have no idea what order suits are actually supposed to go in, so I arbitrarily picked `H | C | D | S`, even though that's almost certainly wrong. Don't hold that against the program, or the tools, that's just me being a not poker player.

9 - <a name="foot-Tue-Aug-28-183224EDT-2012"></a>[|back|](#note-Tue-Aug-28-183224EDT-2012) - And sort groups of cards once.

10 - <a name="foot-Tue-Aug-28-183322EDT-2012"></a>[|back|](#note-Tue-Aug-28-183322EDT-2012) - Probably *after* I finish up my thoughts about authentication.
