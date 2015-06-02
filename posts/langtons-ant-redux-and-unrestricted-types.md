So this past Wednesday was the monthly [Toronto Haskell](https://github.com/HaskellTO/projects) User [Group](https://groups.google.com/forum/#!forum/toronto-haskell) meeting, and I used the opportunity to A) show off [my Langton's Ant implementation](https://github.com/CodeRetreatTO/projects/blob/master/2014-09-langtons-ant/rabraham-and-inaimathi.hs) to people that might be able to offer pointers and B) ask a question about types that I'd been thinking about. We'll handle those in reverse order, with a bit of exposition on the question.

### <a name="splay-trees"></a>Splay Trees

Here's a Haskell implementation of a [Splay Tree](https://en.wikipedia.org/wiki/Splay_tree) not interesting enough to dwell on:

```haskell
module SplayTree (Tree(..), fromList, SplayTree.elem, insert, remove) where

data Tree a = Node { value :: a
                   , left :: Tree a
                   , right :: Tree a }
            | Empty deriving (Eq, Ord, Show, Read)

rotateR Empty = Empty
rotateR tree@(Node _ Empty _) = tree
rotateR (Node val (Node child ll lr) right) = Node child ll (Node val lr right)

rotateL Empty = Empty
rotateL tree@(Node _ _ Empty) = tree
rotateL (Node val left (Node child rl rr)) = Node child (Node val left rl) rr

insertRaw :: Ord a => Tree a -> a -> Tree a
insertRaw Empty a = Node a Empty Empty
insertRaw tree@(Node val left right) a 
    | a == val = tree
    | a > val = Node val left $ insertRaw right a
    | otherwise = Node val (insertRaw left a) right

fromList :: Ord a => [a] -> Tree a
fromList lst = foldl (\memo a -> splayTo (insertRaw memo a) a) Empty lst


splayTo :: Ord a => Tree a -> a -> Tree a
splayTo Empty _ = Empty
splayTo tree@(Node val left right) a
    | a == val = tree
    | a > val = rotateL $ Node val left $ splayTo right a
    | otherwise = rotateR $ Node val (splayTo left a) right

elem :: Ord a => Tree a -> a -> Maybe (a, Tree a)
elem tree a = case a == value splayed of
                True -> Just (a, splayed)
                _ -> Nothing
    where splayed = splayTo tree a

insert :: Ord a => Tree a -> a -> Tree a
insert tree elem = splayTo (insertRaw tree elem) elem

remove :: Ord a => Tree a -> a -> Tree a
remove tree elem = if root == elem
                   then deleteRoot splayed
                   else tree
    where splayed@(Node root _ _) = splayTo tree elem

deleteRoot :: Tree a -> Tree a
deleteRoot (Node val Empty Empty) = Empty
deleteRoot tree@(Node val left Empty) = Node (rightLeaf left) (trim left) Empty
    where rightLeaf (Node val _ Empty) = val
          rightLeaf (Node _ _ right) = rightLeaf right
          trim (Node _ Empty Empty) = Empty
          trim (Node _ left Empty) = left
          trim (Node val left right) = Node val left $ trim right
deleteRoot tree@(Node val left right) = Node (leftLeaf right) left (trim right)
    where leftLeaf (Node val Empty _) = val
          leftLeaf (Node _ left _) = leftLeaf left
          trim (Node _ Empty Empty) = Empty
          trim (Node _ Empty right) = right
          trim (Node val left right) = Node val (trim left) right 
```

Notice in particular that the `Tree` type has an unrestricted `a` in it.

```haskell
--        v
data Tree a = Nod...
--        ^ 
```

but most of the manipulation functions that act on a `Tree a` *do* have a restriction

```haskell
...
insertRaw :: Ord a => Tree a -> a -> Tree a
...
fromList :: Ord a => [a] -> Tree a
...
splayTo :: Ord a => Tree a -> a -> Tree a
...
elem :: Ord a => Tree a -> a -> Maybe (a, Tree a)
...
insert :: Ord a => Tree a -> a -> Tree a
...
remove :: Ord a => Tree a -> a -> Tree a
...
```

They all need an `a` that's a member of the `Ord` typeclass. That is, you need to be able to run `compare` on the elements of a Splay Tree in order for it to do you any good.

So from my perspective, the unrestricted `a` in my `data` declaration feels like lying. I'm giving the impression that you can make a tree out of anything, when in fact you can only make a tree out of anything *you can sort*. My question was, "Is there a way to restrict this structure at the type level?". What I wanted to write was something like

```haskell
data Ord a => Tree a = Nod...
```

To paraphrase [Ben Darwin](https://github.com/bcdarwin) and [Albert Lai](http://www.cs.toronto.edu/~trebla/personal/index.html),

### <a name="you-dont-want-that"></a>You Don't Want That

As it happens, they managed to convince me of this. Here's a paraphrase of the argument they used; maybe it'll work on you too.

> There are only two ways you'll ever want to export your type. You either want to export it as a black box, or you want to export it in a non-opaque way so that your users can see some or all of its implementation details. If you're going the black box route  
>   
> ```haskell
> module Foo (YourTypeConstructor, apiFn, apiFn', apiFn''...) where ...
> ```
>   
> then your users are *only* going to be interacting with your type through your API functions, which are already suitably annotated, and restricted to valid inputs, so there's no need to redundantly restrict the type. If you're going the non-opaque route<a name="note-Fri-Sep-26-113506EDT-2014"></a>[|1|](#foot-Fri-Sep-26-113506EDT-2014),   
>   
> ```haskell
> module Foo (YourTypeConstructor(..), apiFn, apiFn', apiFn''...) where ...
> ```
>   
> then your users might ignore some or all of your API, and just use your structure. In doing so, they may find a purpose for it that *doesn't* require its elements to be members of `Ord`. In this case, you would be doing them a disservice by restricting your type declaration.  

And I'll buy that.

So I don't really want a restricted `Tree`, I just want restricted manipulation functions, which I have. Case closed.

### <a name="langtons-haskelly-ant-redux"></a>Langton's Haskelly Ant Redux

I also ended up showing off [my ants implementation](/article?name=langtons-ant-writeup.html). I'm near the bottom in terms of Haskell skill-level at the group, so the explanation went quite quickly. The only part there was any confusion about was the admittedly over-complicated `animate` function. I mentioned that ideally, I'd want to write something like

```haskell
iterateM_ (\w -> do { printWorld w ; wait 10 ; step w }) test
```

instead of the code I [had actually written](https://github.com/CodeRetreatTO/projects/blob/93983f74a5c9fab3fc472ebff971b034152a0c9b/2014-09-langtons-ant/rabraham-and-inaimathi.hs#L75-L81), but didn't have `iterateM_`. The group considered that no excuse, and promptly found a definition from [Control.Monad.Loop](https://github.com/mokus0/monad-loops)<a name="note-Fri-Sep-26-113520EDT-2014"></a>[|2|](#foot-Fri-Sep-26-113520EDT-2014) via [Hayoo](http://hayoo.fh-wedel.de/). It turned out to be a bit more baroque than necessary.

```haskell
iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f = g
    where g x = f x >>= g
```

I was feeling a bit intimidated by that, but the group pushed me to write my own, starting with just the type signature. "It's extremely simple", they said, "and should really use `do`-notation", unless I shared the Control.Monad.Loop writers' apparently bizarre taste for point-free style. They were right:

```haskell
loop :: Monad m => (a -> m a) -> a -> m b
loop f a = do res &lt;- f a
              loop f res
```

Tadaah.

With that, we can basically write `animate` out of existence entirely by re-defining `main` as

```haskell
main :: IO ()
main = concurrent $ loop nextFrame (test, 0)
    where nextFrame (w, ct) = do liftIO $ setContent "world" $ showWorld w
                                 liftIO $ setContent "generations" $ show ct
                                 wait 50
                                 return $ (step w, succ ct)
```

which is at once easier for me to read, and apparently more idiomatic than [the `setTimeout` recursion](https://github.com/CodeRetreatTO/projects/blob/93983f74a5c9fab3fc472ebff971b034152a0c9b/2014-09-langtons-ant/rabraham-and-inaimathi.hs#L75-L81) I had set up last time. The `concurrent` call at the top, and the `liftIO`s in `nextFrame` are there because [`wait`](http://hackage.haskell.org/package/haste-compiler-0.4.2/docs/Haste-Concurrent.html)<a name="note-Fri-Sep-26-113536EDT-2014"></a>[|3|](#foot-Fri-Sep-26-113536EDT-2014) is a monadic function introduced in [Haste](http://haste-lang.org/), but it's in a monad called [`CIO`](http://hackage.haskell.org/package/haste-compiler-0.4.2/docs/Haste-Concurrent.html#t:CIO), whose goal is to be used for client-side concurrency, while `setContent`, and more importantly `main`, are both plain `IO ()` functions.

The complete revised code, and recompiled `.js` are both up at the same [CodeRetreat github](https://github.com/CodeRetreatTO/projects/blob/master/2014-09-langtons-ant/).

* * *
##### Footnotes

1 - <a name="foot-Fri-Sep-26-113506EDT-2014"></a>[|back|](#note-Fri-Sep-26-113506EDT-2014) - Incidentally, the example below exports *all* internals of `YourTypeConstructor`, but the argument works whenever you expose any internal implementation detail to your users, not just at the transparent extreme.

2 - <a name="foot-Fri-Sep-26-113520EDT-2014"></a>[|back|](#note-Fri-Sep-26-113520EDT-2014) - This would be a [hackage](http://hackage.haskell.org/) link, but it's down as I write this. It probably won't be by the time I get around to the next draft, but it happens with irritating regularity when I'm collecting links for these pieces.

3 - <a name="foot-Fri-Sep-26-113536EDT-2014"></a>[|back|](#note-Fri-Sep-26-113536EDT-2014) - Scroll down to the bottom for the actual documentation of `wait`; it doesn't have a direct link target.
