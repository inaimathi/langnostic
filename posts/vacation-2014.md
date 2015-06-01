So I'm wrapping up my vacation. I took the full week off, and we're into weekend time as I write this. Monday, I go back to work. You know what I've been doing on vacation? Dick all. Oh, sorry, that's what most people do on vacation. Reading up on data structures.

Seriously.

About six years ago, [SteveY](http://steve-yegge.blogspot.ca/) wrote up a post entitled ["Get That Job at Google"](http://steve-yegge.blogspot.ca/2008/03/get-that-job-at-google.html), which incidentally happened to contain a giant pile of stuff to read up on that sounded interesting. I'd read it a couple years back and put a lot of that stuff on a "ToDo" list in the back of my head and didn't think too much about it. Having a kid tends to eat into your personal time, it turns out. It basically got whittled down to just occasionally hacking on [GitHub projects](https://github.com/Inaimathi?tab=repositories), going to [various](https://github.com/CompSciCabal/SMRTYPRTY) programming-[related](https://github.com/HaskellTO/projects) social [events](https://github.com/CodeRetreatTO) and writing the [occasional hundred thousand words](http://langnostic.inaimathi.ca/).

This week, I finally got enough time together that I decided to sit down and plow through the first chunk of that reading list. This may brand me as an irredeemable nerd, but that was easily the most fun I've had in about five years. Not that I'm done yet, mind you. So far I've


-   brushed up on my [Big O](https://en.wikipedia.org/wiki/Big_O_notation), [tree traversal](http://rosettacode.org/wiki/Tree_traversal)/[rotation](https://en.wikipedia.org/wiki/Tree_rotation) and various sorts
-   implemented minimal hash tables
-   done a bit of reading on [NP-complete](https://en.wikipedia.org/wiki/NP-complete) problems and various graph storage techniques
-   done a lot of reading and thinking about, but not yet an implementation of, a [binomial heap](https://en.wikipedia.org/wiki/Binomial_heap)
-   implemented a minimal [trie](https://en.wikipedia.org/wiki/Trie)


Those last two weren't even in the list. I was *supposed* to be reading up on [Red/Black trees](http://eternallyconfuzzled.com/tuts/datastructures/jsw_tut_rbtree.aspx) and [AVL trees](https://en.wikipedia.org/wiki/AVL_tree), but kept getting distracted by more interesting data structures. I've still got a lot to read up on. Specifically, I still need to:


-   implement a [quicksort](http://rosettacode.org/wiki/Sorting_algorithms/Quicksort) and [mergesort](http://rosettacode.org/wiki/Sorting_algorithms/Merge_sort)
-   actually get through Red+Black/AVL/[Splay tree](https://en.wikipedia.org/wiki/Splay_tree) implementations ([Finger trees](http://www.soi.city.ac.uk/~ross/papers/FingerTree.pdf)(PDF warning) also look interesting, but aren't strictly on the reading list)
-   do more graph-related reading and prototyping
-   Do some serious reading related to [NP-complete](https://en.wikipedia.org/wiki/NP-complete) problems
-   brush up on various basic math things (the ones specified in the article are [combinatorics](https://en.wikipedia.org/wiki/Combinatorics) and [probability](https://en.wikipedia.org/wiki/Probability))
-   lightly brush up on OS basics ([threads vs processes](http://stackoverflow.com/questions/200469/what-is-the-difference-between-a-process-and-a-thread) and [performance implications](http://stackoverflow.com/questions/807506/threads-vs-processes-in-linux), [concurrency constructs](http://stackoverflow.com/questions/2332765/lock-mutex-semaphore-whats-the-difference))


By the way, so you know in advance, this article is going to feel pretty disjointed; every five minutes or so I get interested enough in something I'm link-hunting for that I go read about it for fifteen minutes or so before resuming prose. Hopefully it's still comprehensible.

Before I move on to the rest of my reading list, lets dissect the two things I've implemented so far. In an effort to potentially help someone else understand them, *and* with the hope that if I've gotten anything seriously wrong, I'll be yelled at by the internet shortly-ish.

### <a name="hash-tables"></a>Hash Tables

A hash table is a key/value storage structure that's meant to give you as close to constant time access/insertion as possible<a name="note-Sun-Jul-13-194350EDT-2014"></a>[|1|](#foot-Sun-Jul-13-194350EDT-2014). The way you do that is using a vector of linked lists and a hash function. The idea is that when you want to store a value, you hash it and take a modulus to figure out which bucket it goes in, remove that key from the bucket if it already exists, and insert the new key/value pair. You need to keep a list of such pairs<a name="note-Sun-Jul-13-194353EDT-2014"></a>[|2|](#foot-Sun-Jul-13-194353EDT-2014), *and* you need to keep the un-hashed key around, because there might be collisions under your hash function in the set of values you want to be able to store as keys. If you knew enough about your key space in advance that you could guarantee no collisions, you can probably do something more efficient, if less flexible, than hashes..

Here are some implementations, minus a couple of key bits that I'll talk about in a minute.

```lisp
;; Common Lisp
(defclass my-hash-table ()
  ((bucket-count :reader bucket-count :initarg :bucket-count)
   (hash-fn :reader hash-fn :initarg :hash-fn)
   (comparison-fn :reader comparison-fn :initarg :comparison-fn)
   (buckets :reader buckets :initarg :buckets)))

(defun make-my-hash-table (&key (bucket-count 16) (hash-fn #'my-hash) (comparison-fn #'equal))
  (make-instance 
   'my-hash-table
   :bucket-count bucket-count
   :hash-fn hash-fn
   :comparison-fn comparison-fn
   :buckets (make-array bucket-count :initial-element nil)))

(defmethod hash-mod ((table my-hash-table) key)
  (mod (funcall (hash-fn table) key) (bucket-count table)))

(defun make-my-hash-table (&key (bucket-count 16) (hash-fn #'my-hash) (comparison-fn #'equal))
  (make-instance 
   'my-hash-table
   :bucket-count bucket-count
   :hash-fn hash-fn
   :comparison-fn comparison-fn
   :buckets (make-array bucket-count :initial-element nil)))

(defmethod get-hash ((table my-hash-table) key)
  (let ((cmp (comparison-fn table)))
    (loop for (k . v) in (aref (buckets table) (hash-mod table key))
       when (funcall cmp k key) return (values v t)
       finally (return (values nil nil)))))

(defmethod set-hash! ((table my-hash-table) key value)
  (let ((h (hash-mod table key))
        (cmp (comparison-fn table)))
    (setf (aref (buckets table) h)
          (cons (cons key value) 
                (remove-if (lambda (k/v) (funcall cmp (car k/v) key))
                           (aref (buckets table) h))))
    value))
```

```python
## Python
class my_hash_table:
    def __init__(self, buckets=16, hash_fn=my_hash):
        self.bucket_count = buckets
        self.hash_fn = hash_fn
        self.hashmod = lambda thing: self.hash_fn(thing) % self.bucket_count
        self.buckets = [[]] * self.bucket_count

    def get(self, key):
        for (k, v) in self.buckets[self.hashmod(key)]:
            if k == key:
                return v
        raise KeyError("Key '" + str(k) + "' not found...")

    def set(self, key, value):
        h = self.hashmod(key)
        tup = (key, value)
        self.buckets[h] = filter(lambda pair: pair[0] != key, self.buckets[h])
        self.buckets[h].append(tup)
```

```haskell
-- Haskell
module Main where

import Data.Char
import Data.Array 

data HashTable a b = Hash { tblBuckets :: Array Integer [(a, b)]
                          , tblBucketCount :: Integer
                          , tblHashFn :: a -> Integer
                          }

instance (Show a, Show b) => Show (HashTable a b) where
    show h = show $ tblBuckets h

empty :: Integer -> (a -> Integer) -> HashTable a b
empty bucketCount hashFn = Hash { tblBuckets = array (0, bucketCount-1) $ zip [0..(bucketCount-1)] $ repeat []
                              , tblBucketCount = bucketCount
                              , tblHashFn = hashFn
                              }

hashMod :: HashTable a b -> a -> Integer
hashMod hash = (`mod` tblBucketCount hash) . tblHashFn hash

get :: Eq a => HashTable a b -> a -> Maybe b
get hash key = lookup key bucket
    where bucket = buckets ! hashMod hash key
          buckets = tblBuckets hash

set :: Eq a => HashTable a b -> (a, b) -> HashTable a b
set hash (k, v) = hash { tblBuckets = buckets // [(ix, newBucket)] }
    where newBucket = insertOrReplace bucket (k, v)
          ix = hashMod hash k
          insertOrReplace b (k, v) = (k, v):filter ((/=k) . fst) bucket
          bucket = buckets ! ix
          buckets = tblBuckets hash
```

Firstly, you won't see the hash function implementation above. It's sort of incidental to the structure of the rest of the machinery, you probably want different ones for different situations, and the ones that are easy enough to implement off the top of your head tend not to be sufficient except for toy tasks<a name="note-Sun-Jul-13-194401EDT-2014"></a>[|3|](#foot-Sun-Jul-13-194401EDT-2014). If you're really interested in hash functions, rather than hash *tables*, you can find a good introduction and some examples [here](http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx).

Also, none of the above tables grow. In a real situation, they'd have an additional parameter of some threshold at which they re-hash. That is, at some point when the difference between the number of buckets you've got and the number of entries you're tracking gets large enough, you'll want to increase the bucket-count of your table<a name="note-Sun-Jul-13-194404EDT-2014"></a>[|4|](#foot-Sun-Jul-13-194404EDT-2014). Typically, this means doubling the number of buckets, and going back through your existing ones to re-hash entries into their new positions. You'd want to do that on insertion, which means that the Haskell version would probably have the easiest time with the change; conceptually, you're returning a new hash table each time you "insert" anything.

With that out of the way, lets step through these and compare.

#### <a name="constructors"></a>Constructors

```lisp
(defclass my-hash-table ()
  ((bucket-count :reader bucket-count :initarg :bucket-count)
   (hash-fn :reader hash-fn :initarg :hash-fn)
   (comparison-fn :reader comparison-fn :initarg :comparison-fn)
   (buckets :reader buckets :initarg :buckets)))

(defun make-my-hash-table (&key (bucket-count 16) (hash-fn #'my-hash) (comparison-fn #'equal))
  (make-instance 
   'my-hash-table
   :bucket-count bucket-count
   :hash-fn hash-fn
   :comparison-fn comparison-fn
   :buckets (make-array bucket-count :initial-element nil)))
```

```python
class my_hash_table:
    def __init__(self, buckets=16, hash_fn=my_hash):
        self.bucket_count = buckets
        self.hash_fn = hash_fn
        self.hashmod = lambda thing: self.hash_fn(thing) % self.bucket_count
        self.buckets = [[]] * self.bucket_count
...
```

```haskell
data HashTable a b = Hash { tblBuckets :: Array Integer [(a, b)]
                          , tblBucketCount :: Integer
                          , tblHashFn :: a -> Integer
                          }

empty :: Integer -> (a -> Integer) -> HashTable a b
empty bucketCount hashFn = Hash { tblBuckets = array (0, bucketCount-1) $ zip [0..(bucketCount-1)] $ repeat []
                              , tblBucketCount = bucketCount
                              , tblHashFn = hashFn
                              }
```

Those are the declarations and constructors for each language. Python gets to cheat a bit because it has a Smalltalk-inspired object system in which a class owns methods, so its full declaration would also contain the getter and setter we'll go over in a moment. The Common Lisp is obviously the wordiest, mainly because `defclass` makes you explicitly declare `reader`s, but also partially because it has an additional slot. I'm choosing to follow the tradition of the built-in CL hash table and let the user specify a comparison function. It's relevant, because you can compare things on a value or identity basis, and you might want to do either<a name="note-Sun-Jul-13-194409EDT-2014"></a>[|5|](#foot-Sun-Jul-13-194409EDT-2014).

In the Python variant, I decided to do the hash/mod composition once in the constructor. It's not entirely necessary; if you take this approach, you'll need to replace your `hashmod` slot when re-sizing the table. If you don't, you'll need to type it out twice; once in the insertion function/method and again in the lookup. I did the same thing in the Common Lisp and Haskell versions, but those don't suffer the same drawbacks thanks to CLOS and a pure functional approach respectively. And again, you could do something similar by defining a `hashMod` function in Python, that just doesn't seem in-character for the language.

The Haskell version is pretty short and sweet thanks to the record syntax. It's also the only one that clearly states what the hash function is expected to be; a function from your key type to `Integer`<a name="note-Sun-Jul-13-194413EDT-2014"></a>[|6|](#foot-Sun-Jul-13-194413EDT-2014), as well as being the only one to enforce homogeneous key and value types. It sounds mildly annoying, but I can't really think of a situation where I'd really *want* heterogeneous ones, so it doesn't sound like a big deal.

#### <a name="lookup"></a>Lookup

```lisp
(defmethod get-hash ((table my-hash-table) key)
  (let ((cmp (comparison-fn table)))
    (loop for (k . v) in (aref (buckets table) (hash-mod table key))
       when (funcall cmp k key) return (values v t)
       finally (return (values nil nil)))))
```

```python
...
    def get(self, key):
        for (k, v) in self.buckets[self.hashmod(key)]:
            if k == key:
                return v
        raise KeyError("Key '" + str(k) + "' not found...")
...
```

```haskell
get :: Eq a => HashTable a b -> a -> Maybe b
get hash key = lookup key bucket
    where bucket = buckets ! hashMod hash key
          buckets = tblBuckets hash
```

This is where we'll see possibly the biggest difference between the language approaches; if you look semi-closely, you'll notice that they have completely different return tendencies. I tried to follow the in-language style for each, so this is really more a reflection of the attitudes of the communities<a name="note-Sun-Jul-13-194420EDT-2014"></a>[|7|](#foot-Sun-Jul-13-194420EDT-2014). The Python version either finds the key it's looking for or throws a run-time exception. The Haskell variant returns a `Maybe`, which means its actual result will either be `Just &lt;some value>` or `Nothing`. Finally, the CL version returns a value or `NIL`, and returns a second value that specifies whether the first value represents a success. This is to disambiguate the situations where you might want to store `NIL` as a value in your table; if we didn't specify, you wouldn't know whether that represented "Found the value NIL at the specified key" or "Failed to find the specified key".

I'm not sure how to feel about that difference.

The Python approach sounds the least appetizing, but the other two sound fairly similar. In the Haskell variant, you're being very explicit that this computation might fail, and you're forcing the caller to deal with that. In the Common Lisp variant, you're setting up the situation so that if a user of your data structure doesn't care about storing `NIL`s in their table, they can easily ignore the part that deals with that ambiguity. Some implicit knowledge is still required here though; you need to know that the second value exists and you need to know that it'll be `T` on a lookup success and `NIL` on a lookup failure, *and* you need to know that Lisp functions typically return `NIL` in the case where they fail rather than using the condition system.

When you say it like that, it sounds like the Haskell approach is outright better, but I've seen enough aspiring Haskellers get tripped up over the use of `Maybe` that I'm not so sure.

#### <a name="setting"></a>Setting

```lisp
(defmethod set-hash! ((table my-hash-table) key value)
  (let ((h (hash-mod table key))
        (cmp (comparison-fn table)))
    (setf (aref (buckets table) h)
          (cons (cons key value) 
                (remove-if (lambda (k/v) (funcall cmp (car k/v) key))
                           (aref (buckets table) h))))
    value))
```

```python
...
    def set(self, key, value):
        h = self.hashmod(key)
        tup = (key, value)
        self.buckets[h] = filter(lambda pair: pair[0] != key, self.buckets[h])
        self.buckets[h].append(tup)
```

```haskell
set :: Eq a => HashTable a b -> (a, b) -> HashTable a b
set hash (k, v) = hash { tblBuckets = buckets // [(ix, newBucket)] }
    where newBucket = insertOrReplace bucket (k, v)
          ix = hashMod hash k
          insertOrReplace b (k, v) = (k, v):filter ((/=k) . fst) bucket
          bucket = buckets ! ix
          buckets = tblBuckets hash
```

Setting approaches are fairly equivalent, with the obvious giant disclaimer that the Haskell version is trying to be functional, so it returns a new `HashTable` with the appropriate values changed/inserted rather than mutating anything. This means that the return value is, in fact, `HashTable a b` rather than the newly inserted value as in the Common Lisp variant, or `None` as in the Python variant.

That's really the only difference here though. Each of the setters takes a `table`<a name="note-Sun-Jul-13-194427EDT-2014"></a>[|8|](#foot-Sun-Jul-13-194427EDT-2014), a `key` and a `value`. It hashes and mods the `key` to figure out which bucket we're dealing with, then it adds the k/v pair to the result of filtering that bucket on `key`<a name="note-Sun-Jul-13-194433EDT-2014"></a>[|9|](#foot-Sun-Jul-13-194433EDT-2014). The Common Lisp and Haskell versions both `cons` onto the front of said list, where the Python version uses the `append` method which pushes to the back of a list. I guess I could have used `insert(0, tup)` instead, but as I understand, `insert`ing to the beginning of the list [tends to perform significantly worse](http://stackoverflow.com/questions/7776938/python-insert-vs-append) than `append`ing to the end. My intuition says it's not worth bothering with, since the only thing it might affect is get/set performance on recently inserted keys in large buckets, and you kind of want to work to avoid large buckets in general.

Ok, on to the next thing.

### <a name="trie"></a>Trie

This is just an interesting data structure I came across while reading the rest of the tree stuff. It's fairly interesting because it can search for things by prefixes, making it pretty handy in situations where you want string completion. The only real concrete use case I'm thinking of for them at the moment is storage for titles/names at my blog<a name="note-Sun-Jul-13-194438EDT-2014"></a>[|10|](#foot-Sun-Jul-13-194438EDT-2014), so that you'd be able to navigate to one by knowing only the prefix of the appropriate title.

It's not as commonly known as a hash table, so here's a demo interaction first:

```lisp
TRIE> (defparameter *trie* (empty))
*TRIE*
TRIE> (insert! "test" *trie*)
(NIL NIL (#\t NIL (#\e NIL (#\s NIL (#\t "test")))))
TRIE> (insert! "testing" *trie*)
(NIL NIL
 (#\t NIL (#\e NIL (#\s NIL (#\t "test" (#\i NIL (#\n NIL (#\g "testing"))))))))
TRIE> (insert! "trie" *trie*)
(NIL NIL
 (#\t NIL (#\r NIL (#\i NIL (#\e "trie")))
  (#\e NIL (#\s NIL (#\t "test" (#\i NIL (#\n NIL (#\g "testing"))))))))
TRIE> (insert! "banana" *trie*)
(NIL NIL (#\b NIL (#\a NIL (#\n NIL (#\a NIL (#\n NIL (#\a "banana"))))))
 (#\t NIL (#\r NIL (#\i NIL (#\e "trie")))
  (#\e NIL (#\s NIL (#\t "test" (#\i NIL (#\n NIL (#\g "testing"))))))))
TRIE> (insert! "batman" *trie*)
(NIL NIL
 (#\b NIL
  (#\a NIL (#\t NIL (#\m NIL (#\a NIL (#\n "batman"))))
   (#\n NIL (#\a NIL (#\n NIL (#\a "banana"))))))
 (#\t NIL (#\r NIL (#\i NIL (#\e "trie")))
  (#\e NIL (#\s NIL (#\t "test" (#\i NIL (#\n NIL (#\g "testing"))))))))
TRIE> (member? "batman" *trie*)
"batman"
TRIE> (member? "Gabarone" *trie*)
NIL
TRIE> (completions-of "t" *trie*)
("trie" "test" "testing")
TRIE> (completions-of "te" *trie*)
("test" "testing")
TRIE> (completions-of "test" *trie*)
("test" "testing")
TRIE> (completions-of "testi" *trie*)
("testing")
TRIE> (completions-of "G" *trie*)
NIL
TRIE> (completions-of "" *trie*)
("trie" "test" "batman" "banana" "testing")
TRIE> 
```

I'm still not sure if the right thing to return on insertion is the entire `trie`. I guess the newly inserted value would be more in character for Common Lisp. Also, because of that use case I mentioned, I didn't bother implementing a `delete!` either<a name="note-Sun-Jul-13-194442EDT-2014"></a>[|11|](#foot-Sun-Jul-13-194442EDT-2014).

```lisp
(defun empty () (list nil nil))

(defun insert! (str trie)
  (labels ((recur (lst trie)
             (if lst
                 (let ((res (assoc (car lst) (cddr trie))))
                   (if res
                       (recur (cdr lst) res)
                       (let ((new (list (car lst) nil)))
                         (push (recur (cdr lst) new) 
                               (cddr trie)))))
                 (setf (cadr trie) str))
             trie))
    (recur (coerce str 'list) trie)))

(defun lookup-to (lst trie)
  (cond ((null trie) nil)
        ((null lst) trie)
        (t (lookup-to (cdr lst) (assoc (car lst) (cddr trie))))))

(defun member? (str trie)
  (cadr (lookup-to (coerce str 'list) trie)))

(defun completions-of (str trie)
  (let ((level (list (lookup-to (coerce str 'list) trie)))
        (res nil))
    (loop while level
       do (setf level
                (loop for lst in level
                   for val = (cadr lst)
                   when val do (push val res)
                   append (cddr lst)))
       finally (return (nreverse res)))))
```

This is the absolute minimal version. I didn't even bother writing up a definition for a `trie` node, whether explicitly or implicitly through some function definitions. Basically, one looks like this

```lisp
(letter . value . children)
```

The root node has `NIL` as the "letter", otherwise that's the letter you'd have to look up in the previous level to get this far. All terminal nodes have values in the second slot, while non-terminals have `NIL`<a name="note-Sun-Jul-13-194448EDT-2014"></a>[|12|](#foot-Sun-Jul-13-194448EDT-2014). Child nodes are further `trie`s that contain the next possible letters of a string at this point. While the toy example above just keeps a full word at each terminal node, in general there's more than that. It might be the word, a definition and a likelihood of its appearance in a block of text. If I were concerned only with keeping words around, a [DAWG](https://en.wikipedia.org/wiki/Deterministic_acyclic_finite_state_automaton) would do better. Remember, my use case is navigating articles by title prefixes, so my *real* implementation is going to have a title and a pointer to the article body at minimum. But I'm getting ahead of myself.

The `empty` trie is the list of two `NIL`s; one representing the "letter" and the other representing the value. The "letter" needs to be accounted for in the situation where you're asking for the `completions-of` the empty string. In this case, `NIL` makes sense because coercing the empty string to a list gives you the empty list (`NIL`).

```lisp
(defun completions-of (str trie)
  (let ((level (list (lookup-to (coerce str 'list) trie)))
        (res nil))
    (loop while level
       do (setf level
                (loop for lst in level
                   for val = (cadr lst)
                   when val do (push val res)
                   append (cddr lst)))
       finally (return (nreverse res)))))
```

Which means that the `(list (lookup-to (coerce str 'list) trie))` up there will return the whole `trie` when you pass `""` as `str`. And actually, you need to know how `lookup-to` works to be assured of that

```lisp
(defun lookup-to (lst trie)
  (cond ((null trie) nil)
        ((null lst) trie)
        (t (lookup-to (cdr lst) (assoc (car lst) (cddr trie))))))
```

If we ever run out of `trie` to search, the given list isn't in it. If we ever run out of `lst`, we return the rest of the `trie`, since that's the place we stopped. We don't just look up the word here because our initial input might have been a partial word, which means we really want to manipulate the remaining tree of nodes, not just a single terminal one. Finally, if we're out of neither `lst` nor `trie`, we call `lookup-to` again with the rest of `lst` and the appropriate child branch of the current `trie`.

Back up to `completions-of`

```lisp
(defun completions-of (str trie)
  (let ((level (list (lookup-to (coerce str 'list) trie)))
        (res nil))
    (loop while level
       do (setf level
                (loop for lst in level
                   for val = (cadr lst)
                   when val do (push val res)
                   append (cddr lst)))
       finally (return (nreverse res)))))
```

Once we get back the result of `lookup-to`, we establish an initially empty result list, `res`, then doing a breadth-first traversal of our lookup result. That traversal pulls out every non-`NIL` value it finds into `res`, and finally returns the in-place reversal of `res`<a name="note-Sun-Jul-13-194457EDT-2014"></a>[|13|](#foot-Sun-Jul-13-194457EDT-2014).

`insert!`ing a fresh string isn't too much more interesting.

```lisp
(defun insert! (str trie)
  (labels ((recur (lst trie)
             (if lst
                 (let ((res (assoc (car lst) (cddr trie))))
                   (if res
                       (recur (cdr lst) res)
                       (let ((new (list (car lst) nil)))
                         (push (recur (cdr lst) new) 
                               (cddr trie)))))
                 (setf (cadr trie) str))
             trie))
    (recur (coerce str 'list) trie)))
```

You start at the root of the `trie` and the beginning of your string `coerce`d to a list. Each time through, if you're out of string, you're done and need to set the value of the current `trie` node. Otherwise, you need to look the next letter up in the current `trie` level. If you find an entry, you `recur` with the rest of your string and that entry. If there is no such entry, you insert the remainder of the list.

This could probably use some tweaking. Again, I'm not sure the right thing to return here is the modified `trie`. It would also be fairly simple to optimize the case where you know the rest of your string is fresh, and not too much harder to write a version of this that neither `coerce`s its string to a list nor uses recursion.

The last function we need to look at is `member?`.

```lisp
(defun member? (str trie)
  (cadr (lookup-to (coerce str 'list) trie)))
```

Which is fairly straight-forward. It just uses `lookup-to` to traverse to the end-point of the word we're interested in, then checks if the value at that node is non-`NIL`. If it *is* `NIL`, then the given string is not a member of this `trie`.

So that's what I get down to during a fun<a name="note-Sun-Jul-13-194504EDT-2014"></a>[|14|](#foot-Sun-Jul-13-194504EDT-2014) vacation. I sit down at a park or balcony or something and read up on data structures. I'm honestly not sure if this'll end up making me a better programmer in the long run, but it's fun regardless. As always, I'll let you know how it goes.


* * *
##### Footnotes

1 - <a name="foot-Sun-Jul-13-194350EDT-2014"></a>[|back|](#note-Sun-Jul-13-194350EDT-2014) - It doesn't always work, incidentally. Take a look at [this example](https://www.youtube.com/watch?v=R2Cq3CLI6H8) of an exploit that happens when you let clients choose arbitrary keys for your tables. Hashing also doesn't give you much of a performance boost if you force a low number of buckets, or use a hash function that doesn't sufficiently distribute your keys.

2 - <a name="foot-Sun-Jul-13-194353EDT-2014"></a>[|back|](#note-Sun-Jul-13-194353EDT-2014) - Ok, no, you don't *need* to; the linked-list approach is called ["separate chaining"](https://en.wikipedia.org/wiki/Separate_chaining#Separate_chaining) and you can avoid it by using an [open addressing](https://en.wikipedia.org/wiki/Open_addressing) strategy or one of the others listed. You do, however, need to have one of "a perfect hash function" or "collision resolution", otherwise your hash table won't quite do what you think it should.

3 - <a name="foot-Sun-Jul-13-194401EDT-2014"></a>[|back|](#note-Sun-Jul-13-194401EDT-2014) - And vice versa. The best recommended pair seems to be [one](http://www.burtleburtle.net/bob/hash/doobs.html) of [the](http://www.burtleburtle.net/bob/hash/spooky.html) functions due to [Bob Jenkins](http://www.burtleburtle.net/bob/index.html), but I can't cram either into my head at the moment.
4 - <a name="foot-Sun-Jul-13-194404EDT-2014"></a>[|back|](#note-Sun-Jul-13-194404EDT-2014) - And hence re-hash all the entries.

5 - <a name="foot-Sun-Jul-13-194409EDT-2014"></a>[|back|](#note-Sun-Jul-13-194409EDT-2014) - Ok, yes, you can [do the same thing in Python](http://stackoverflow.com/questions/1504717/why-does-comparing-strings-in-python-using-either-or-is-sometimes-produce) using `==` and `is`, but that would put me in the territory of expecting something out of [`operators`](https://docs.python.org/2/library/operator.html) as input. No idea how Pythonic that is, so I'm leaving it out, but I wanted to note that you could do the same here. Haskell, being a pure functional language, doesn't have to worry about pointer comparison at all.

6 - <a name="foot-Sun-Jul-13-194413EDT-2014"></a>[|back|](#note-Sun-Jul-13-194413EDT-2014) - Which is what we're using as indices into our `Array` of buckets.

7 - <a name="foot-Sun-Jul-13-194420EDT-2014"></a>[|back|](#note-Sun-Jul-13-194420EDT-2014) - Or at least, the attitudes of the core library developers/language maintainers.

8 - <a name="foot-Sun-Jul-13-194427EDT-2014"></a>[|back|](#note-Sun-Jul-13-194427EDT-2014) - Granted, the Python version does so implicitly, but still.

9 - <a name="foot-Sun-Jul-13-194433EDT-2014"></a>[|back|](#note-Sun-Jul-13-194433EDT-2014) - To remove any old values of `key` that might be present.

10 - <a name="foot-Sun-Jul-13-194438EDT-2014"></a>[|back|](#note-Sun-Jul-13-194438EDT-2014) - Though I'm thinking that URI routing in House might eventually benefit from this storage mechanism too. I'd just need to figure out where the rough patches are and figure out how to mitigate them. I'll let you know how that goes once I get to it.

11 - <a name="foot-Sun-Jul-13-194442EDT-2014"></a>[|back|](#note-Sun-Jul-13-194442EDT-2014) - If you do it yourself, just keep in mind that you can't do the *really* naive thing of deleting every element of the given string. In particular, if I did `(delete "test" *trie*)` given the demo code above, that should delete the word `"test"`, but should absolutely **not** delete `"testing"`. Left as an exercise for the reader, for now.

12 - <a name="foot-Sun-Jul-13-194448EDT-2014"></a>[|back|](#note-Sun-Jul-13-194448EDT-2014) - In the case of a `trie`, "terminal" doesn't mean "there are no more nodes after this", it means "this node marks the end of a word". As an example, in the case of a `trie` that contains only the words "test" and "testing"; the second `#\t` is going to be a terminal node with the value "test" that *does* have further children.

13 - <a name="foot-Sun-Jul-13-194457EDT-2014"></a>[|back|](#note-Sun-Jul-13-194457EDT-2014) - To compensate for the fact that we've been pushing onto a singly linked list.

14 - <a name="foot-Sun-Jul-13-194504EDT-2014"></a>[|back|](#note-Sun-Jul-13-194504EDT-2014) - For my definition of "fun".
