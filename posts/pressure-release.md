So I've been running around studying [this](http://www.hindawi.com/journals/tswj/2014/481312/) or [that](https://en.wikipedia.org/wiki/Karp%27s_21_NP-complete_problems) for [about](https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode) two [weeks](https://en.wikipedia.org/wiki/Tabu_search#Pseudocode), give or take. Every free moment I've had has gone into learning new interesting things, and it finally occurred to me that I'm staring down a sparse but functionally infinite graph of human knowledge, expanding at an unknown rate in every direction.

I'm not giving up. That doesn't happen. Which is why I've been scarce in the various [places](https://github.com/Inaimathi/cl-notebook) you [might](https://github.com/Inaimathi/clomments) expect [me](http://langnostic.inaimathi.ca/) to [be](https://github.com/Inaimathi/500lines/tree/master/event-driven-web-server) right now. At the very least, I'm solving that second-last one with another disjointed random thoughts update.

## <a name="fbp-and-graph-theory"></a>FBP and Graph Theory

I've had this thought in my head for a while, though I'm sure I'm nowhere near the first one, and since I've been reading up on various graph-related problems and representations, it seems like a good time to note it in passing. My team is mostly composed of Lispers, and entirely composed of dynamic typing fans (Yes, yes, I know, [not a real distinction](http://existentialtype.wordpress.com/2014/04/21/bellman-confirms-a-suspicion/), but you know what I mean...), so I thought it might be fun to give this a quick think from the other perspective.

```haskell
module FBP where

data Message = Str String
             | Num Integer
             | Tbl [(String, Message)]
             | Lst [Message] deriving (Eq, Ord, Show)

data Event = Event { evLabel :: String, evMessage :: Message } deriving (Eq, Ord, Show)

data Graph v e = Graph [v] [Edge v e]
data Edge v e = Edge { edgeFrom :: v
                     , edgeLabel :: e
                     , edgeTo :: v }

data State = State (Message -> (State, [Event]))

data Part = Part { tag :: String, busy :: Bool, body :: PartGuts }

data PartGuts = Map (Graph Part String)
              | Fsm (Graph State String)
              | Fn (Message -> [Event])
              | Io (Message -> IO ())

type Network = Graph Part String
```

This is pretty accurate as far as I can tell. Actual code left as an exercise for the reader, or perhaps a future self, but the signatures are giving me some ideas about what to do with our system. We can't quite do the most obvious things, like flatten out a network, because it would break some of the expected timing characteristics, but I'm sure there's something I'll be able to cook up that'll be sufficiently cool with this approach. Even if it's just a more effective way to explain what we're actually doing.

## <a name="common-lisp-sort-bites-hard"></a>Common Lisp Sort Bites Hard

[This](http://stackoverflow.com/questions/25021832/np-complete-appetizers-bug) is sort of embarrassing. You can see the details there if you like, but the short version is that it slipped my mind that `sort` was destructive, and this led to one of the most difficult-to-diagnose bug I've been on the receiving end of. This is something I've [already bitched about](/article?name=recommendations.html), but thought I was past the point of tripping over.

That'll learn me, I guess.

## <a name="other-tries"></a>Other Tries

[Last time](/article?name=vacation-2014.html), I put up a [trie](https://en.wikipedia.org/wiki/Trie) implementation in Common Lisp. Here's some more:

```clojure
;;; trie.clj
(ns trie)

(def empty-trie [nil {}])

(defn value [trie] (first trie))
(defn children [trie] (second trie))

(defn insert 
  ([word trie]
     (insert word trie word))
  ([word trie full-word]
     (if (empty? word)
       [full-word (children trie)]
       (let [l (first word)]
         [(value trie)
          (assoc (children trie) l
                 (insert (rest word) (get (children trie) l) full-word))]))))

(defn select-to [word-part trie]
  (cond (empty? word-part) trie
        (not (get (children trie) (first word-part))) (list)
        :else (select-to (rest word-part) (get (children trie) (first word-part)))))

(defn member? [word trie]
  (let [res (select-to word trie)]
    (value res)))

(defn values-in [[value children]]
  (let [rest (mapcat (fn [[k v]] (values-in v)) children)]
    (if value
      (cons value rest)
      rest)))

(defn completions-of [word-part trie]
  (values-in (select-to word-part trie)))
```

```haskell
--- Trie.hs
module Trie where

import Data.Map (Map)
import qualified Data.Map as M 

data Trie a b = Trie (Maybe b) (Map a (Trie a b))
            | Empty deriving (Eq, Ord, Show, Read)

insert :: Ord a => [a] -> b -> Trie a b -> Trie a b
insert key value trie = recur key trie
    where recur [] Empty = Trie (Just value) M.empty
          recur [] (Trie val ts) = Trie (Just value) ts
          recur (l:rest) Empty = Trie Nothing $ M.insert l (recur rest Empty) M.empty
          recur (l:rest) (Trie val ts) = Trie val $ M.insert l newT ts
              where newT = case M.lookup l ts of
                             Just t -> recur rest t
                             Nothing ->  recur rest Empty

selectTo :: Ord a => [a] -> Trie a b -> Trie a b
selectTo [] trie = trie
selectTo _ Empty = Empty
selectTo (l:rest) (Trie _ ts) = selectTo rest $ M.findWithDefault Empty l ts 

elem :: Ord a => [a] -> Trie a b -> Bool
elem key trie = case selectTo key trie of
                  Trie (Just _) _ -> True
                  _ -> False

valuesIn :: Trie a b -> [b]
valuesIn (Trie Nothing ts) = concatMap valuesIn $ M.elems ts
valuesIn (Trie (Just v) ts) = v : (concatMap valuesIn $ M.elems ts)

completionsOf :: Ord a => [a] -> Trie a b -> [b]
completionsOf key trie = valuesIn $ selectTo key trie

fromList :: Ord a => [([a], b)] -> Trie a b
fromList lst = foldl (\memo (k, v) -> insert k v memo) Empty lst

fromWordList :: [String] -> Trie Char String
fromWordList lst = fromList $ zip lst lst
```

```python
### trie.py
class Trie:
    def __init__(self):
        self.children = {}
        self.value = None

    def insert(self, key, value):
        if key:
            try:
                child = self.children[key[0]]
            except KeyError:
                child = Trie()
                self.children[key[0]] = child
            child.insert(key[1:], value)
        else:
            self.value = value

    def insert_word(self, word):
        self.insert(word, word)

    def __select_to(self, key):
        if key:
            try:
                child = self.children[key[0]]
                return child.__select_to(key[1:])
            except:
                return None
        else:
            return self

    def is_member(self, key):
        res = self.__select_to(key)
        if res:
            return res.value

    def values_in(self):
        res = []
        if self.value: 
            res.append(self.value)
        for k in self.children:
            res += self.children[k].values_in()
        return res

    def completions_of(self, key):
        res = self.__select_to(key)
        if res:
            return res.values_in()
        else:
            return []
```
        

I'm not going through them step-by-step, because it's the same data structure you saw last time, and it's fairly self-explanatory once you understand the underlying point, but I still wanted to think around it clearly enough to translate the concept to other languages. The same notes on language difference from last time still apply here. Python, Haskell and Common Lisp have very different characteristics. And Clojure seems to be somewhere in between of Haskell and Common Lisp. By the way, don't judge the Clojure version too harshly. I'm nowhere near a `.clj`-wizard yet, and this particular trie implementation was more or less live-coded at the last [Toronto Clojure Users Group](https://github.com/ClojureTO) meeting. Fun to write, but probably neither optimal nor idiomatic, nor respecting Clojure module conventions.

## <a name="binomial-heaps"></a>Binomial Heaps

This one isn't even on the reading list, as I mentioned, but it's interesting enough that I had to sink a few hours into it. What can I say, I'm weak to minimal invariants and logarithmic performance.

```haskell
module BinomialHeap (empty, insert, merge, fromList
                    , BinomialHeap.minimum, deleteMinimum, order, key) where

import Control.Monad (liftM)
import Data.List (delete)

data BinomialTree a = Tree { key :: a
                           , order :: Integer
                           , subTrees :: [BinomialTree a] 
                           } deriving (Eq, Ord, Show)

data BinomialHeap a = Heap [BinomialTree a] deriving (Eq)
instance (Show a) => Show (BinomialHeap a) where
    show (Heap trees) = unlines $ map show trees

addSubTree :: BinomialTree a -> BinomialTree a -> BinomialTree a
addSubTree a b = a { subTrees = b:(subTrees a)
                   , order = succ $ order a }

mergeTree :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
mergeTree a b
    | key a > key b = b `addSubTree` a
    | otherwise = a `addSubTree` b

merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge (Heap as) (Heap bs) = Heap . reverse $ recur as bs []
    where recur [] [] acc = acc
          recur t [] acc = foldl mergeDown acc t
          recur [] t acc = foldl mergeDown acc t
          recur (t:rest) (t':rest') acc
              | order t == order t' = recur rest rest' $ mergeDown acc (mergeTree t t')
              | order t > order t' = recur (t:rest) rest' $ mergeDown acc t'
              | otherwise = recur rest (t':rest') $ mergeDown acc t
          mergeDown [] t = [t]
          mergeDown (t':rest) t
              | order t == order t' = mergeDown rest (mergeTree t t')
              | otherwise = t:t':rest

insert :: Ord a => BinomialHeap a -> a -> BinomialHeap a
insert heap elem = merge heap $ Heap [Tree elem 0 []]

deleteMinimum :: Ord a => BinomialHeap a -> BinomialHeap a
deleteMinimum heap@(Heap ts) = case minTree heap of
                       Nothing -> heap
                       Just t -> merge (Heap . reverse $ subTrees t) $ Heap $ delete t ts

minTree :: Ord a => BinomialHeap a -> Maybe (BinomialTree a)
minTree (Heap []) = Nothing
minTree (Heap ts) = Just $ foldl1 minKey ts
    where minKey memo t = if key memo &lt; key t then memo else t

minimum :: Ord a => BinomialHeap a -> Maybe a
minimum = liftM key . minTree

fromList :: Ord a => [a] -> BinomialHeap a
fromList = foldl (flip insert) empty

empty :: BinomialHeap a
empty = Heap []

-- This is completely stupid
sort :: Ord a => [a] -> [a]
sort lst = recur (fromList lst) []
    where recur heap acc = case BinomialHeap.minimum heap of
                             Nothing -> acc
                             Just v -> recur (deleteMinimum heap) $ v:acc
```

In English, a Binomial Heap is a collection of Binomial Trees, sorted in ascending order of `order`. Which is mildly confusing, but that's what the definition calls a particular property of said trees. Speaking of which,


-   a Binomial Tree of `order` 0 is a single node with no children
-   a Binomial Tree of `order` N is a root node with children, each of which is a Binomial Tree of descending order, starting with `order` N-1 and ending with `order` 0


And now you know all of the invariants involved in maintaining one of these heaps. Lets go through the above code chunk by chunk reasonably quickly.

```haskell
module BinomialHeap (empty, insert, merge, fromList
                    , BinomialHeap.minimum, deleteMinimum, order, key) where

import Control.Monad (liftM)
import Data.List (delete)

data BinomialTree a = Tree { key :: a
                           , order :: Integer
                           , subTrees :: [BinomialTree a] 
                           } deriving (Eq, Ord, Show)

data BinomialHeap a = Heap [BinomialTree a] deriving (Eq)
instance (Show a) => Show (BinomialHeap a) where
    show (Heap trees) = unlines $ map show trees
```

Namespace/importing minutia and the encoded definition I explained above. We're encoding the "root node" of a Binomial Tree in a separate slot called `key`, since every `BinomialTree` will have one of those. Nothing more to see here, moving on.

```haskell
addSubTree :: BinomialTree a -> BinomialTree a -> BinomialTree a
addSubTree a b = a { subTrees = b:(subTrees a)
                   , order = succ $ order a }

mergeTree :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
mergeTree a b
    | key a > key b = b `addSubTree` a
    | otherwise = a `addSubTree` b
```

I honestly considered making `addSubTree` a local definition in `mergeTree`, because it doesn't get called anywhere else. In order to merge two trees of the same `order`, you find out which has a larger `key`, and add that one as a new child to the other. We're maintaining a `min-heap` relationship here, because that's what the default implementation does, but there's no particular reason not to make this predicate an argument and let the user choose.

Adding a new sub-tree to a Binomial Tree means consing onto its list of children and incrementing its order.

As a side-note, these two functions are deliberately not exported as external API functions to the module. I'm following the [original source](https://en.wikipedia.org/wiki/Binomial_heap#Implementation) fairly closely in this implementation, but *if I weren't*, I'd either rename these to make it obvious that they're only supposed to be called on two trees of equal `order`, *or* I'd have them each check for that before doing their thing. As it stands, it's not entirely obvious from the code, which would make it very easy for users of this library to break one of the necessary invariants if either `addSubTree` or `mergeTree` were exposed directly.

So they're not.

```haskell
merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge (Heap as) (Heap bs) = Heap . reverse $ recur as bs []
    where recur [] [] acc = acc
          recur t [] acc = foldl mergeDown acc t
          recur [] t acc = foldl mergeDown acc t
          recur (t:rest) (t':rest') acc
              | order t == order t' = recur rest rest' $ mergeDown acc (mergeTree t t')
              | order t > order t' = recur (t:rest) rest' $ mergeDown acc t'
              | otherwise = recur rest (t':rest') $ mergeDown acc t
          mergeDown [] t = [t]
          mergeDown (t':rest) t
              | order t == order t' = mergeDown rest (mergeTree t t')
              | otherwise = t:t':rest
```

This is the meaty one, because as you'll see, the other significant change operations are implemented in terms of it. So once you understand *this*, you understand Binomial Heaps. And if you don't, you don't.

In order to merge two Binomial Heaps, we go through their Binomial Trees in sequence. For each pair of children, if they're of the same order, we `mergeDown` the result of calling`mergeTree` on them and recur on both Heaps. If they're different orders, we `mergeDown` the larger one and recur on that Heap. If we run out of children in either heap, we fold `mergeDown` on the remaining children of the other heap. Finally, we `reverse` the result and hit it with a `Heap` constructor.

`mergeDown` is the only piece of mystery left, and it's not very mysterious. Whenever we go to accumulate a new Binomial Tree in the result set, we need to check if the result already contains a tree of the same order. If it does, we need to merge the two of them, otherwise we just `cons` the new one onto the result set.

Now that we understand `merge`, the rest should be a breeze. For instance, `insert`ing an element into a Binomial Heap...

```haskell
insert :: Ord a => BinomialHeap a -> a -> BinomialHeap a
insert heap elem = merge heap $ Heap [Tree elem 0 []]
```

...means creating a new heap of one element, and merging it with the initial one.

```haskell
deleteMinimum :: Ord a => BinomialHeap a -> BinomialHeap a
deleteMinimum heap@(Heap ts) = case minTree heap of
                       Nothing -> heap
                       Just t -> merge (Heap . reverse $ subTrees t) $ Heap $ delete t ts
```

Deleting the minimum element of a Binomial Heap means


1.   finding the Tree with the smallest root
1.   throwing away the root
1.   making a new heap composed only of that Tree's children
1.   merging that new heap with the original heap, minus the minimum Tree


Do note that we need to check the result of `minTree`.

```haskell
minTree :: Ord a => BinomialHeap a -> Maybe (BinomialTree a)
minTree (Heap []) = Nothing
minTree (Heap ts) = Just $ foldl1 minKey ts
    where minKey memo t = if key memo &lt; key t then memo else t
```

Because it's a `Maybe`. There is no such thing as the minimum tree of the empty Binomial Heap, you see. The definition of `minimum` is predictably minimal

```haskell
minimum :: Ord a => BinomialHeap a -> Maybe a
minimum = liftM key . minTree
```

It's a composition of `liftM key` and `minTree`. Finally, making a Binomial Heap from a list of `Ord`erable elements means folding `insert` over said list, starting with the `empty` heap.

```haskell
fromList :: Ord a => [a] -> BinomialHeap a
fromList = foldl insert empty
```

And I guess *finally* finally, an `empty` heap is a heap with no trees.

```haskell
empty :: BinomialHeap a
empty = Heap []
```

Tadaah!

That's it.

The only thing I'm thinking is that it might sometimes be useful to change up the predicate for those Binomial Trees. So that you could have `max` versions instead of being restricted purely to `min`. That would require changing the interface a bit; we'd be deleting and selecting `extreme`s instead of `minimum`s, and we'd need to change `fromList` so that it accepted an appropriate comparison function.

Hopefully you found this useful or entertaining, or both. I'm heading back to the infinite, sparse graph.
