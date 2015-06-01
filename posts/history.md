So I've been thinking about history. Or, more accurately, history-aware persistent storage, and what it takes to implement it. There's a fairly small core that can be torn out of [`:fact-base`](https://github.com/Inaimathi/fact-base) and generalized, and I wanted to point that out before I forgot.

```lisp
(in-package #:cl-history)

(defclass event ()
  ((timestamp :accessor timestamp :initform (local-time:now) :initarg :timestamp)
   (event-type :accessor event-type :initform :in :initarg :event-type)
   (data :accessor data :initform nil :initarg :data)))

(defgeneric tick (whole part)
  (:documentation "Should take a whole and a part. Should return the whole with the part added."))

(defgeneric wind (whole part)
  (:documentation "Should take a whole and a part. Should return the whole with the part removed."))

(defmethod apply-event (whole (ev event))
  (let ((part (data ev)))
    (case (event-type ev)
      (:in (tick whole part))
      (:out (wind whole part)))))

(defmethod load-from (empty (storage stream))
  (let ((res empty))
    (handler-case
        (loop 
           do (setf res (tick res (cl-store:restore storage)))
           do (read-byte storage))
      (end-of-file () res))))

(defmethod load-from (empty (fname pathname))
  (with-open-file (s fname :element-type '(unsigned-byte 8))
    (load-from empty s)))

(defmethod new-event! (whole (ev event) (storage stream) &rest more-streams)
  (loop for s in (cons storage more-streams)
     do (cl-store:store ev s)
     do (write-byte (char-code #\newline) s))
  (apply-event whole ev))
```

And once more, in Haskell

```haskell
module History where

import System.IO

data Event a = In a
             | Out a deriving (Eq, Read, Show)

data Archive a b = Archive { tick :: (a -> b -> a)
                           , wind :: (a -> b -> a)
                           , empty :: a }

applyEvent :: Archive a b -> a -> Event b -> a
applyEvent arc a (In b) = (tick arc) a b
applyEvent arc a (Out b) = (wind arc) a b

loadFrom :: Read b => Archive a b -> Handle -> IO a
loadFrom arc handle = recur handle (empty arc)
    where recur h acc = do res &lt;- hIsEOF h
                           if res
                           then return acc
                           else do ln &lt;- hGetLine h
                                   recur h . applyEvent arc acc $ read ln

newEvent :: Show b => Archive a b -> a -> Event b -> [Handle] -> IO a
newEvent arc a ev handles = do _ &lt;- mapM_ (\h -> hPutStrLn h $ show ev) handles
                               return $ applyEvent arc a ev
```

The Haskell version is pretty much all in the `IO` monad because this is meant to be a persistence layer. And regardless of whatever else your persistence layer does, it's eventually going to want to write some data out to a stream. Not necessarily to *disk*, mind you, it might be going out over a TCP socket or something<a name="note-Thu-Oct-02-222843EDT-2014"></a>[|1|](#foot-Thu-Oct-02-222843EDT-2014).

If you've been paying any attention at all to [`:fact-base`](https://github.com/Inaimathi/fact-base), or seen [what I'm getting down to with it](https://github.com/Inaimathi/cl-notebook/blob/master/cl-notebook.lisp), you'll notice that a few things are missing from the above.

Lets get the trivials out of the way first. There is no `batch` mode, as implemented in `:fact-base`s'[`:change` event](https://github.com/Inaimathi/fact-base/blob/master/fact-base.lisp#L223-L233). I just haven't bothered yet; that's something we can fix with between four and ten lines of code, so it's not a big deal. Second, there's no indexing system, because that's entirely incidental. In the Haskell version, the `a` part of `Archive a b` might have its own indexing scheme, but it's none of our business as history recorders. It'll be hooked into whatever the appropriate implementation of `tick` and `wind` happen to be. Finally, there aren't deltas because we're not buffering events here; we're just writing them straight out to whatever `Handle`s or `stream`s we get as input.

The *real* hole in functionality is the lack of a rewinding mechanism. I'm still thinking it through, although the core would basically have to look something like

```haskell
opposite :: Event a -> Event a
opposite (In a) = Out a
opposite (Out a) = In a

rewindEvent :: Archive a b -> a -> Event b -> a
rewindEvent arc a = applyEvent arc a . opposite
```

The thing is, if we're not making the assumption that `newEvent` is specifically writing to a file, we can't assume we have random access to it, hence we can't really abstract the traversal of history. That *has* to be handled by the users of this little framework, although we can provide convenience functions to handle the file-storage case specifically.

I'm not entirely sure which direction to go in, because there's exactly one place I use the `:fact-base` `rewind` system heavily, and that one place performed poorly enough from disk that I was forced to go back and implement an in-memory option. And this was on a pretty speedy SSD, so it was almost the optimal case for the technique. In other words, it's not clear to me that we *want* to provide a full rewind-from-disk implementation. This might be the reason to chuck deltas into the mix after all; you could then keep one sessions' worth of data around, which would coincidentally both give you the ability to rewind into the shallow past, *and* reduce the need for disk IO, *and* keep a bit more of the Haskell implementation in the realm of pure code.

So yeah. The three potential directions I can see are:


-   provide a pure-functional rewind system that leaves getting a list of `Event`s up to the user of `History`. It'll be harder to call, and there might be overlap in terms of what each implementer has to put together, but this would afford users the greatest flexibility.
-   provide a file-specific rewind system that knows rather a lot about the on-disk storage format and handles reading `Event`s in as well as rolling them back. This would be less flexible, since we'd assume we had random access to whatever storage solution we're using, but it would be easier for callers to actually use<a name="note-Thu-Oct-02-224232EDT-2014"></a>[|2|](#foot-Thu-Oct-02-224232EDT-2014)
-   provide a system in which archives carry around session deltas. This would give them partial in-memory rewind capability, and give the caller more explicit control over when stream writes happen.


Not quite sure whether I want to pick one, or a hybrid, or something else entirely.

I'll let you know if I figure anything out.


* * *
##### Footnotes

1 - <a name="foot-Thu-Oct-02-222843EDT-2014"></a>[|back|](#note-Thu-Oct-02-222843EDT-2014) - In fact, if I wanted to *really* generalize `newEvent`, it would instead have the signature `newEvent :: Show b => Archive a b -> a -> Event b -> (Event b -> IO ()) -> IO a`. Which would then let you do some mildly crazy things. Like, for instance, using this with the [Haste compiler](http://haste-lang.org/) to set up a client-side database that sends off write requests via [Ajax](http://hackage.haskell.org/package/haste-compiler-0.2.99/docs/Haste-Ajax.html). Left as an exercise for the reader, or perhaps a mildly bored future self.

2 - <a name="foot-Thu-Oct-02-224232EDT-2014"></a>[|back|](#note-Thu-Oct-02-224232EDT-2014) - Assuming they were ok with the assumption, of course.
