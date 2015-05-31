Fairly boring update time. I still haven't really had enough time to tear into the next real article I've got planned, but its been long enough that I need to release mental steam.

### Future of Programming

So [this](https://vimeo.com/97623064) is a thing. I'm not sure how I feel about it since it was a zero-edit, one sitting walk-through of a still very incomplete version of [`cl-notebook`](https://github.com/Inaimathi/cl-notebook), but at least it's a thing. I recently submitted it to John Edwards et als' [Future of Programming workshop](http://www.future-programming.org/call.html) where, I guess, I'm hoping to get in as a token "blast from the past" entry. The funny part is that even if I get selected to participate at these conferences, there's no way in hell I could go. My passport is somewhat out of date, and I've got a family to care for and a job to do here. Hopefully they've got a remote option I guess?

Someone just sort of mentioned the thing off-hand, and I immediately assumed from the title that this was just a mechanism by which to get more feedback on particular programming projects you were working on. Presenting to [this crowd](https://thestrangeloop.com/) isn't something I thought was part of the deal.

### Memory

So it turns out that the memory management landscape I briefly mused about [last time](/article?name=arbitrary-update-932.html) is even simpler than I thought. Because "tracers" and "refernce counters" [turn out to be duals](http://www.cs.virginia.edu/~cs415/reading/bacon-garbage.pdf). You can either follow that link to the paper, or [this one](https://www.youtube.com/watch?v=XtUtfARSIv8) to the [Papers We Love](https://www.youtube.com/channel/UCoj4eQh_dZR37lL78ymC6XA) video explaining it. Both elucidate wonderfully.

So there isn't even five different ways of approaching memory, there's four.

### HTTP On Windows...

...is a sack of balls. It's especially a sack of balls in Lisp (or leastwise SBCL), which tries to be clever about how to parse incoming line-endings. Let me back up a bit first.

Since I've apparently got some Windows users now, `house` kind of has to fly on it. Which means that I've had to add little pieces of compatibility cruft like

```lisp
...
#-win32(wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
#+win32(wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t :timeout 5)
...
```

I mean, it's nice that I *can*, but it would be even nicer if `usocket` didn't randomly require a `:timeout` argument to `wait-for-input` on Windows. It's marginally plausible that the underlying implementation there might turn out to eat massive piles of shit, so this might just be an escape hatch to keep it from shit-eating itself into an early grave, but it's still mildly annoying.

However, one such piece of compatibility code crosses the line into outright infuriating.

```lisp
(defun line-terminated? (lst)
  (starts-with-subseq 
   #-win32(list #\linefeed #\return #\linefeed #\return)
   #+win32(list #\newline #\newline)
   lst))
```

Why do I need this? It's because platform-specific line-endings read from a `char` stream are returned as `#\newline`. On Linux machines, `#\newline` is synonymous with `#\linefeed`. Literally.

```lisp
CL-USER> (eq #\linefeed #\newline)
T
CL-USER> 
```

The situation's different on Windows though. There, a line ends with `CRLF`. Which happens to be the actual specified line-ending for HTTP. Combining these fairly sensical decisions gets you the situation that you *can't* check for a char-stream HTTP line-ending on Windows by looking for a terminating `CRLF`.

Why don't I just use a byte stream instead? Especially since I already need bi-valent streams to serve up images and similar?

```lisp
CL-USER> (describe #'read-char-no-hang)
#&lt;FUNCTION READ-CHAR-NO-HANG>
  [compiled function]

Lambda-list: (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T)
              EOF-VALUE RECURSIVE-P)
Declared type: (FUNCTION (&OPTIONAL (OR STREAM (MEMBER NIL T)) T T T)
                (VALUES T &OPTIONAL))
Derived type: (FUNCTION (&OPTIONAL T T T T) (VALUES T &OPTIONAL))
Known attributes: explicit-check
Source file: SYS:SRC;CODE;STREAM.LISP
; No value

CL-USER> (describe #'read-byte-no-hang)
The function COMMON-LISP-USER::READ-BYTE-NO-HANG is undefined.
   [Condition of type UNDEFINED-FUNCTION]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] Abort thread (#&lt;THREAD "repl-thread" RUNNING {10046F8063}>)

Backtrace:
  0: (SB-INT:SIMPLE-EVAL-IN-LEXENV (FUNCTION READ-BYTE-NO-HANG) #&lt;NULL-LEXENV>)
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (DESCRIBE (FUNCTION READ-BYTE-NO-HANG)) #&lt;NULL-LEXENV>)
  2: (EVAL (DESCRIBE (FUNCTION READ-BYTE-NO-HANG)))
  ...
```

It's because there's no non-blocking way to grab a `byte` out of a stream. There *is* such a way to grab a `char`, but that doesn't help in this situation. Anyway, the end result of all of this is that ugly little hack that makes the line-ending check conditional on execution platform.

### [`fact-base`](https://github.com/Inaimathi/fact-base) changes

Two changes I'd been considering for a while got pushed recently.

Firstly, there's an intermediate data-structure backing `delta`s and `history`s. It's extremely simple; just a linked list you can push to the back of. The end result is saving a tiny bit of time when reading or writing things, because you suddenly don't need to reverse anything for it to work properly. Not big, and this changes literally nothing as far as *users* of `fact-base` are concerned, but it feels better not to have to toss `reverse` into the equation every so often.

Secondly, and more relevantly for `cl-notebook`, there's now a primitive `:change` token. It doesn't reduce disk usage because both the complete before and after facts are kept on record, but *does* make changes atomic in `history`. Which will mildly help with the deep undo/branch interface I still need to put together.
