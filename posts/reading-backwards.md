Suppose for the sake of argument you've got [an append-only data-structure](https://github.com/Inaimathi/fact-base) that tracks history on a particular scale, and you'd like to use it for very large data sets. Like, larger than will fit in memory. Before you even begin approaching that threshold on a server, you'll need to start pruning the things you keep around. My first reflex was to change up how history is stored and projected. So, rather than keeping history around in memory, keep the current projection, some ~400 history states, and the starting projection<a name="note-Thu-Jun-19-203346EDT-2014"></a>[|1|](#foot-Thu-Jun-19-203346EDT-2014). You then get all the benefits of a total history without the drawback of filling up all your memory with it. The trouble only starts when you want to back out further than the ~400 states you *have* in memory, and then it gets painful fast. You need to get increasingly more chunks off of disk to work with, and in the degenerate case, you need to read all of history anyhow.

When I explained this to [a friend of mine](https://github.com/dxnn), he asked why I needed to keep history in memory at all. And suggested that, since my `:insert`/`:delete`/`:change` tokens are perfectly reversible, when I need to slice up history, I read entries *from the end* of the on-disk representation<a name="note-Thu-Jun-19-203349EDT-2014"></a>[|2|](#foot-Thu-Jun-19-203349EDT-2014) and reverse them off of the `current` projection.

It so happens that Common Lisp provides `file-position` for random-access to on-disk data, so this sounds like a plan. Also, my on-disk data is being kept in trivially serialized `list`s, so it should be fairly simple to read them.

```lisp
(defun read-entry-from-end (fname)
  (with-open-file (s fname)
    (let* ((len (file-length s))
           (depth 0))
      (loop for i from (- len 1) downto 0
         do (file-position s i)
         for c = (peek-char nil s)
         do (case c
              (#\( (decf depth))
              (#\) (incf depth)))
         until (and (= 0 depth) (char= c #\()))
      (read s))))
```

That starts at the end of a file, gets a `list` by skipping to the matching paren, then calling `read`. Except this won't handle some edge cases. Or rather, non-edge cases, given what I frequently write about. Here's a sample entry that would break the above:

```lisp
((5211 72634 468967000) :insert (27 :body "This is a blog post that contains an unmatched paren ("))
```

Right, so we need to handle parens in strings. Those are things, and they won't necessarily match; no problem.

```lisp
(defun read-entry-from-end (fname)
  (with-open-file (s fname)
    (let* ((len (file-length s))
           (in-string? nil)
           (depth 0))
      (loop for i from (- len 1) downto 0
         do (file-position s i)
         for c = (peek-char nil s)         
         if in-string? 
         do (when (char= #\" c) (setf in-string? nil))
         else do (case c
                   (#\" (setf in-string? t))
                   (#\( (decf depth))
                   (#\) (incf depth)))
         until (and (= 0 depth) (char= c #\()))
      (read s))))
```

Wait, fuck, strings can contain escaped quotes. Ok, ok, so...

```lisp
(defun read-entry-from-end (fname)
  (with-open-file (s fname)
    (let* ((len (file-length s))
           (in-string? nil)
           (depth 0))
      (loop for i from (- len 1) downto 0
         do (file-position s i)
         for c = (peek-char nil s)         
         if in-string? 
         do (when (char= #\" c)
              (file-position s (- i 1))
              (unless (char= #\\ (peek-char nil s))
                (setf in-string? nil))
              (file-position s i))
         else do (case c
                   (#\" (setf in-string? t))
                   (#\( (decf depth))
                   (#\) (incf depth)))
         until (and (= 0 depth) (char= c #\()))
      (read s))))
```

Except, no, that'll get you odd behavior too. In the rarer, but still possible case of an escaped `\` followed by a `"`. Maybe I'm worrying too much about this, but just so you know, the Emacs HTML mode I've got running right now is having a hell of a hard time highlighting this article, for something not dissimilar to the edge case I'm discussing. Something like this

```lisp
((5211 72634 468967000) :insert (27 :body "This is a blog post that contains an unmatched paren ( and a half-quoted slash \\\""))
```

specifically.

If you want to handle this situation correctly and semi-elegantly, you need to bust out the FSMs.

```lisp
(defun read-entry-from-end (fname)
  (with-open-file (s fname)
    (let* ((len (file-length s))
           (cur (- len 1))
           (paren-depth 0))
      (labels ((peek () (peek-char nil s))
               (dec () (file-position s (decf cur)))
               (to-open-quote ()
                 (loop for c = (peek) do (dec)
                    until (char= #\" c))
                 (slashes))
               (slashes ()
                 (let ((ct 0))
                   (loop for c = (peek) while (char= #\\ c)
                      do (incf ct) do (dec))
                   (when (oddp ct) (to-open-quote))))
               (to-entry-start ()
                 (loop for c = (peek)
                    do (dec)
                    do (case c
                         (#\" (to-open-quote))
                         (#\( (decf paren-depth))
                         (#\) (incf paren-depth))
                    until (or (zerop cur) (and (char= #\( c) (zerop paren-depth)))))))
        (file-position s cur)
        (to-entry-start)
        (let ((start (file-position s)))
          (values (read s) start))))))
```

Because what you're really trying to do is:


-   read to a matching paren
-   if you find a quote, read until the next quote
-   at a candidate ending quote, keep reading until you run out of `#\\`s, and count them. If you counted an odd number, one of them escapes the quote, so you need to keep looking for another quote. Otherwise, resume looking for a matching paren.


And that's basically what the above does. Lets go step by step so you understand what's going on.

```lisp
(defun read-entry-from-end (fname)
  (with-open-file (s fname)
    (let* ((len (file-length s))
           (cur (- len 1))
           (paren-depth 0))
...
```

We're opening the specified file, finding its length and setting `cur` to one less said length so as to avoid the `EOF` marker.

```lisp
...
(labels ((peek () (peek-char nil s))
         (dec () (file-position s (decf cur)))
         (to-open-quote ()
           (loop for c = (peek) do (dec)
             until (char= #\" c))
           (slashes))
         (slashes ()
           (let ((ct 0))
             (loop for c = (peek) while (char= #\\ c)
                   do (incf ct) do (dec))
             (when (oddp ct) (to-open-quote))))
         (to-entry-start ()
           (loop for c = (peek)
                 do (dec)
                 do (case c
                      (#\" (to-open-quote))
                      (#\( (decf paren-depth))
                      (#\) (incf paren-depth))
                 until (or (zerop cur) (and (char= #\( c) (zerop paren-depth)))))))
...
```

then we set up some intermediate definitions which I'll get to in a moment

```lisp
...
(file-position s cur)
(to-entry-start)
(let ((start (file-position s)))
  (values (read s) start))))))
```

Finally, we set the `file-position` of `s`to `cur` and call `to-entry-start`<a name="note-Thu-Jun-19-203403EDT-2014"></a>[|3|](#foot-Thu-Jun-19-203403EDT-2014). The result of that operation should be to get `s`s' file-pointer to the place where the last history entry starts. At that point, we can read it<a name="note-Thu-Jun-19-203406EDT-2014"></a>[|4|](#foot-Thu-Jun-19-203406EDT-2014).

Now, what does `to-entry-start` do?

```lisp
...
(to-entry-start ()
  (loop for c = (peek)
        do (dec)
        do (case c
             (#\" (to-open-quote))
             (#\( (decf paren-depth))
             (#\) (incf paren-depth))
        until (or (zerop cur) (and (char= #\( c) (zerop paren-depth)))))))
...
```

It peeks at its next char, calls `dec` and dispatches based on the value of the char it `peek`ed.


-   on a quote (`#\"`), call `to-open-quote`
-   on an open-paren, decrement `paren-depth` (since we're going backwards)
-   on a close-paren, increment `paren-depth`


Finally, it checks if either we've reached the beginning of the file or found the open-paren that starts our history entry. If either is true, we're done here.

Next up, lets tackle `dec`, since it's quite simple:

```lisp
...
(dec () (file-position s (decf cur)))
...
```

It decrements `cur`, then sets `file-position` for that files' input stream to `cur`. The end result is moving backwards one character. Next up, `to-open-quote`

```lisp
...
(to-open-quote ()
  (loop for c = (peek) do (dec)
    until (char= #\" c))
  (slashes))
...
```

unsurprisingly jumps to the next quote (`#\"`)<a name="note-Thu-Jun-19-203421EDT-2014"></a>[|5|](#foot-Thu-Jun-19-203421EDT-2014), then calls `slashes` to handle the slashes that may or may not be present.

```lisp
...
(slashes ()
  (let ((ct 0))
    (loop for c = (peek) while (char= #\\ c)
          do (incf ct) do (dec))
    (when (oddp ct) (to-open-quote))))
...
```

`slashes` jumps until it gets to something other than a `#\\`, and keeps count of the slashes it jumps past. If there was an odd number of slashes, then we just saw an escaped quote rather than a real one and we need to call `to-open-quote` again<a name="note-Thu-Jun-19-203425EDT-2014"></a>[|6|](#foot-Thu-Jun-19-203425EDT-2014). Otherwise, we return, which will continue from the call point in `to-entry-start`. Oh, lastly, `peek`

```lisp
...
(peek () (peek-char nil s))
...
```

is shorthand for peeking from the current file stream without skipping whitespace.

So that'll get me the last history entry in a given file

```lisp
FACT-BASE>  (read-entry-from-end "/home/inaimathi/.cl-notebook/books/base-xzxa9f44")

((5211 72634 468967000) :INSERT
 (19 :RESULT
  (((:STDOUT . "") (:WARNINGS)
    (:VALUES
     ((:TYPE . ERROR)
      (:VALUE (CONDITION-TYPE . "SIMPLE-TYPE-ERROR")
       (:ERROR-MESSAGE . "Argument Y is not a NUMBER: :TEST")
       (:FORM . "(+ 2 3 4 :TEST 5)") (:DATUM . :TEST)
       (:EXPECTED-TYPE . NUMBER))))))))
FACT-BASE> (read-entry-from-end "/home/inaimathi/quicklisp/local-projects/langnostic/langnostic.base")
((5222 9633 334075000) :INSERT (181 :EDITED 3612047668))
FACT-BASE> 
```

The principal use-case for this thing is going to be getting some intermediate history entry, not necessarily the last one. We'll need to make a couple of changes to make that possible. First, we could accept a number of entries to skip, and second we could specify a starting position. The `skip` argument should default to 0, and it shouldn't let the user skip negative values 'cause that wouldn't make any sense. The `start` argument can't be larger than one less the length of the file, but we can't really do much if we're passed a pointer to a character in the middle of an s-expression somewhere. Really, I'll only want to use this to skip previous entries that I've read, so a good idea would be to have `read-entry-from-end` return the starting point of the expression it returned<a name="note-Thu-Jun-19-203436EDT-2014"></a>[|7|](#foot-Thu-Jun-19-203436EDT-2014). Also, I'll put up a sign to be careful with that second one.

```lisp
(defun read-entry-from-end (fname &key (skip 0) start)
  "Takes a filename, returns a history entry from the end of that file.
Two keyword arguments:
  - :skip  - is a number of entries to skip before the one we want (defaults to 0, which gives the last one)
  - :start - is the position in the file to start searching. 
             It defaults to the end of the file.
             Careful here; if you pass it a position in the middle of an s-expression, things will explode."
  (assert (>= skip 0) nil "I can't skip a negative number, Dave.")
  (with-open-file (s fname)
    (let* ((len (file-length s))
           (cur (or (and start (> len start -1) start) (- len 1)))
           (paren-depth 0))
      (labels ((peek () (peek-char nil s))
               (dec () (file-position s (decf cur)))
               (to-open-quote ()
                 (loop for c = (peek) do (dec)
                    until (char= #\" c))
                 (slashes))
               (slashes ()
                 (let ((ct 0))
                   (loop for c = (peek) while (char= #\\ c)
                      do (incf ct) do (dec))
                   (when (oddp ct) (to-open-quote))))
               (to-entry-start ()
                 (loop for c = (peek)
                    do (dec)
                    do (case c
                         (#\" (to-open-quote))
                         (#\( (decf paren-depth))
                         (#\) (incf paren-depth)))
                    until (or (zerop cur) (and (char= #\( c) (zerop paren-depth))))))
        (file-position s cur)
        (loop repeat (+ skip 1) until (zerop cur)
           do (to-entry-start))
        (let ((fp (file-position s)))
          (values (read s) fp))))))
```

Most of the difference is at the bottom there. We're calling `to-entry-start` a number of times equal to one more than the number we're `skip`ping<a name="note-Thu-Jun-19-203441EDT-2014"></a>[|8|](#foot-Thu-Jun-19-203441EDT-2014) instead of once, and we're returning the `file-position` at which the returned entry starts as a second value. The only other differences are the added docstring, the changed arglist, the assertion on `skip` and the assignment of `cur`. That last one now checks that the given start point is 0 or greater, and less than the total length of the specified file, otherwise it defaults to the end of the file<a name="note-Thu-Jun-19-203446EDT-2014"></a>[|9|](#foot-Thu-Jun-19-203446EDT-2014). Here it is in action:

```lisp
FACT-BASE> (read-entry-from-end "/home/inaimathi/quicklisp/local-projects/langnostic/langnostic.base")
((5222 9633 334075000) :INSERT (181 :EDITED 3612047668))
3063327
FACT-BASE> (read-entry-from-end "/home/inaimathi/quicklisp/local-projects/langnostic/langnostic.base" :skip 3)
((5222 9633 333109000) :DELETE (181 :EDITED 3612039462))
3043159
FACT-BASE> (read-entry-from-end "/home/inaimathi/quicklisp/local-projects/langnostic/langnostic.base" :start 3043159)
((5222 1068 248101000) :INSERT (181 :TAG :LOGIC-PROGRAMMING))
3043097
```

That should do for what I need. At this point, I need to tear out the in-memory history on `fact-base` and make sure to reconcile that properly with the still in-memory `delta`. Of course, there's a couple of other approaches to consider; one is just caching history entry offsets as I read/write them and keeping that list around. Not sure how I feel about it, since it would still eventually saturate memory, though at a greatly reduced rate. I'll be exploring that for the next little while.

Wish me luck.


* * *
##### Footnotes
1 - <a name="foot-Thu-Jun-19-203346EDT-2014"></a>[|back|](#note-Thu-Jun-19-203346EDT-2014) - The projected sum of the history states before the first one you keep in memory.

2 - <a name="foot-Thu-Jun-19-203349EDT-2014"></a>[|back|](#note-Thu-Jun-19-203349EDT-2014) - Or the in-memory `delta` as appropriate.

3 - <a name="foot-Thu-Jun-19-203403EDT-2014"></a>[|back|](#note-Thu-Jun-19-203403EDT-2014) - Incidentally, on Linux/Unix, both the `file-length` and the `file-position` calls are pretty friggin efficient. I have no idea what the situation is on Windows, so if you're looking to implement something there, you might want to see whether it'll perform well enough for you before getting in too deep.

4 - <a name="foot-Thu-Jun-19-203406EDT-2014"></a>[|back|](#note-Thu-Jun-19-203406EDT-2014) - I thought about trying to do this in a single traversal, but decided that you basically can't and that `read`ing from the stream is the most efficient way to get this s-expression out. Why? Because there's basically two ways to do it. You could accumulate the characters you're peeking into a list by using `push`, then `coerce` it into a string at the end. The trouble is, you still need to call `read-from-string` on the result, and the `coerce` call probably has to allocate a fresh array, then copy the chars into it. Which is worse. The other approach is to try to pre-allocate a string and copy the `peek`ed chars directly into it. The problem with *that* is that you have no idea how big a string you'll need before-hand. The premise here is that I want this to work on files that are too large to fit in memory, so you can't just allocate a string to hold the entire file, but it's anyones' guess where the next s-expression ends, *and* you still need to call `read-from-string` on the result. Even if I decided that I really *really* **really** want this, and snuck length footers into the `fact-base` file-format, I still need to do that `read-from-string` at the end. The length footers might still help marginally, but my best bet still seems like it'd be reading directly from the stream once I have the file pointer where I want it.

5 - <a name="foot-Thu-Jun-19-203421EDT-2014"></a>[|back|](#note-Thu-Jun-19-203421EDT-2014) - Also, in case you're wondering, I'm constantly clarifying that because of that Emacs HTML mode bugbear I mentioned. If you have an unmatched quote in a buffer, even if it's escaped, the rest of the buffer ends up being highlighted as a string. It's surprisingly annoying. So I try to close it up as soon as I can.

6 - <a name="foot-Thu-Jun-19-203425EDT-2014"></a>[|back|](#note-Thu-Jun-19-203425EDT-2014) - Which on reflection I should probably call `to-quote` for accuracy. This was a note to self.

7 - <a name="foot-Thu-Jun-19-203436EDT-2014"></a>[|back|](#note-Thu-Jun-19-203436EDT-2014) - So I can skip to that next time.

8 - <a name="foot-Thu-Jun-19-203441EDT-2014"></a>[|back|](#note-Thu-Jun-19-203441EDT-2014) - With the addendum that we can't skip past the beginning of the file.

9 - <a name="foot-Thu-Jun-19-203446EDT-2014"></a>[|back|](#note-Thu-Jun-19-203446EDT-2014) - One char before its `EOF` marker.
