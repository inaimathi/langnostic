### <a name="deal-journal-interlude-three-being-a-brief-musing-on-session-mechanisms-and-their-implementation" href="#deal-journal-interlude-three-being-a-brief-musing-on-session-mechanisms-and-their-implementation"></a>Deal Journal - Interlude Three -- Being a brief Musing on Session Mechanisms and Their Implementation

I'm going to get to the reflections piece eventually, I swear. Or maybe I won't. Fuck I don't know.

Anyhow, sessions are things you'll need to deal with if you want to build any kind of stateful application on top of HTTP. Because an HTTP conversation is stateless by default. When you send an HTTP request out, as a general rule there's nothing in it that could let the server positively identify you. Which means that if you make two serial requests to the same site, they usually can't be absolutely sure that both of the requests you just sent came from you. They'll get data on your user agent<a name="note-Tue-Oct-15-222939EDT-2013"></a>[|1|](#foot-Tue-Oct-15-222939EDT-2013), operating system, and your IP<a name="note-Tue-Oct-15-222942EDT-2013"></a>[|2|](#foot-Tue-Oct-15-222942EDT-2013). And that's it. Now, granted, if you're me, it's fairly easy for the server to point out the [Debian Jessie](http://www.debian.org/releases/testing/)/[Conkeror](http://conkeror.org/) user originating at IP foo, but that's not something a server operator can normally rely on.

What they have to do is hand you some piece of data, and ask you to hand it back to them every time you visit. Usually this takes the form of a cookie, and if they've done their job sufficiently well, they can now take any bunch of requests they got with the same cookie and reasonably assume that it came from the same user.

## <a name="how-well-is-sufficiently" href="#how-well-is-sufficiently"></a>How Well is "Sufficiently"?

Something should be obvious there. First, unless you're using SSL, that piece of state you've been handed is trivially sniffable. Which means that if you have a habit of logging into a server that doesn't make you use `https`, well, I hope you're not keeping anything *really* secret there. Second, unless your session state is pretty hard to guess, someone who wants to impersonate you probably can.

From a server operators' perspective, the `https` thing is easy. Just use SSL<a name="note-Tue-Oct-15-222945EDT-2013"></a>[|3|](#foot-Tue-Oct-15-222945EDT-2013). As for guessability, we want the following properties:


-   each active user should have a unique session token, unless they choose to share it
-   knowing any number of previous keys shouldn't give you any edge in guessing others<a name="note-Tue-Oct-15-222952EDT-2013"></a>[|4|](#foot-Tue-Oct-15-222952EDT-2013).
-   knowing how the keys are generated shouldn't give you any edge in guessing others<a name="note-Tue-Oct-15-222956EDT-2013"></a>[|5|](#foot-Tue-Oct-15-222956EDT-2013)


And that's close enough to the specification of a [CSPRNG](http://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator) that if we had one, we could just use it. The absolute simplest way to do that is to use a secure block cipher on a randomly initialized counter. As it happens, Common Lisp Has That©™.

## <a name="generating-session-tokens-with-ironclad" href="#generating-session-tokens-with-ironclad"></a>Generating Session Tokens with Ironclad

So, basically what we need is for our server to generate a secret key<a name="note-Tue-Oct-15-222959EDT-2013"></a>[|6|](#foot-Tue-Oct-15-222959EDT-2013), then use that to encrypt the output of a counter, starting at some random point or possibly just modified by a random number.

In other words, if I'm understanding the proposition<a name="note-Tue-Oct-15-223003EDT-2013"></a>[|7|](#foot-Tue-Oct-15-223003EDT-2013), you can do that like this

```lisp
(ql:quickload (list :ironclad :cl-base64))
(setf *random-state* (make-random-state t))

(defmethod sha256 ((message integer))
  (ironclad:digest-sequence
   :sha256 (ironclad:integer-to-octets message)))

(defmethod aes ((message string) (key array))
  (let ((cipher (ironclad:make-cipher :aes :mode :ecb :key key))
        (msg (ironclad:ascii-string-to-byte-array message)))
    (ironclad:encrypt-in-place cipher msg)
    msg))

(defun new-session-token ()
  (cl-base64:usb8-array-to-base64-string
   (aes (format nil "~a::~a::~a"
                (gensym) (random (expt 2 128)) (get-universal-time))
        (sha256 (random (expt 2 128))))
   :uri t))
```

It's probably not necessary to generate a new key for each session, but it doesn't seem to be too expensive, so I'll spring for it.

`sha256` is a thin wrapper around a particular `digest-sequence` call, and it produces a 32-element vector of octets representing the digested number. We feed that to an `aes` cipher as a key, along with a `(gensym)`, random number and the current time in milliseconds. `aes` is itself just a call to a set of `ironclad` functions that return a vector of octets representing the AES-encrypted message described above. That result is itself then fed through `cl-base64:usb8-array-to-base64-string`, which gives us a string we can use as a reasonably secure session token, provided we're using SSL. Here's a sample

```lisp
CL-USER> (new-session-token)
"LIAez844JJyKYuvOD9YGJ2rGlzTbVHzd-705gOB4FjSvJyNcw95BigdiC9vE_W5TMDo6MzU5MDg3MTU4Ng.."
CL-USER> (new-session-token)
"Sb455jzLxXHf0i_hALnowAd8JY-GC2aJJ9UekKPlj6AdlxnvpKGELJERnuugWLxWMjo6MzU5MDg3MTU4Ng.."
CL-USER> (new-session-token)
"fgwnkKaxqUfj2GHn_VcR0tBnasfzYOMeFQECAelV3vPc-7VAOxjs3nqm3wGTh9dLNzo6MzU5MDg3MTU4Nw.."
CL-USER> (new-session-token)
"MfCTWUJ0IavpPXY551xpmdTC-MHssRluqwTRsdetNI1bnOqXyoddl73CE8fQ2hAMOjM1OTA4NzE1ODg."
CL-USER> (new-session-token)
"zckWhe4EJk8hrvVl-y8UeoC0Zqfb5nZAJtyhof66hAlzAN2OkHoCXgR9iTJhcKLVOjM1OTA4NzE1ODk."
CL-USER> (new-session-token)
"_kQaKM6Ck8GiaRY1ZO4Y_gj0o6LuQT54oSXYSrCIMMORe5hazv0uz5TGPiod4m3NMzo6MzU5MDg3MTU5MA.."
CL-USER> 
```

And, just to make sure,

```lisp
CL-USER> (loop repeat 1000000 do (new-session-token))
  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name  
---------------------------------------------------------------
    27.586 |      0.692 | 5,610,325,328 | 1,000,000 |   0.000028 | NEW-SESSION-TOKEN
---------------------------------------------------------------
    27.586 |      0.692 | 5,610,325,328 | 1,000,000 |            | Total

estimated total profiling overhead: 1.79 seconds
overhead estimation parameters:
  1.6000001e-8s/call, 1.7919999e-6s total profiling, 7.4400003e-7s internal profiling
NIL
CL-USER> 
```

the profiler says session generation probably isn't going to be my bottleneck. Though I could probably tune it if I liked, not that I could see the gains offsetting the readability hit we'd take. If I *had* to start cutting somewhere, I'd make sure to only generate one key per server session, and figure out a more efficient way than `format` to put the key content together.

Actually, that `gensym`+`rand`-call+`get-universal-time` method strikes me as programming by superstition. Even more-so than the Hunchentoot session mechanism, which also includes the target IP/user-agent and validates these against the incoming request<a name="note-Wed-Oct-16-092953EDT-2013"></a>[|8|](#foot-Wed-Oct-16-092953EDT-2013). If we were implementing the *real* requirements as I understand them, we'd just need

```lisp
(setf *random-state* (make-random-state t))

(defmethod sha256 ((message integer))
  (ironclad:digest-sequence
   :sha256 (ironclad:integer-to-octets message)))

(let ((cipher (ironclad:make-cipher :aes :key (sha256 (random (expt 2 1024))) :mode :ecb))
      (counter (random (expt 2 512))))
  (defun new-session-token ()
    (let ((raw (ironclad:integer-to-octets (incf counter))))
      (ironclad:encrypt-in-place cipher raw)
      (cl-base64:usb8-array-to-base64-string raw :uri t))))
```

Random key, check. Counter starting at a random number, check. And this should coincidentally perform much better too.

```
CL-USER> (loop repeat 1000000 do (new-session-token))
measuring PROFILE overhead..done
  seconds  |     gc     |     consed    |   calls   |  sec/call  |  name  
---------------------------------------------------------------
     8.637 |      0.644 | 5,423,965,408 | 1,000,000 |   0.000009 | NEW-SESSION-TOKEN
---------------------------------------------------------------
     8.637 |      0.644 | 5,423,965,408 | 1,000,000 |            | Total

estimated total profiling overhead: 1.82 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.816e-6s total profiling, 7.92e-7s internal profiling
NIL
CL-USER> 
```

Yup.

Of course, I still don't have enough confidence in my own assessment to just run with all this, so I'll be asking questions first.

> EDIT:  
>   
> It turns out that [`:ironclad` has a built-in CSPRNG option](http://method-combination.net/lisp/ironclad/#prng) that implements [Fortuna](http://en.wikipedia.org/wiki/Fortuna_(PRNG)). If we use that, our implementation gets much simpler, but mildly slower<a name="note-Wed-Oct-16-165818EDT-2013"></a>[|9|](#foot-Wed-Oct-16-165818EDT-2013).  
>   
> ```lisp
> (ql:quickload (list :ironclad :cl-base64))
> 
> (let ((prng (ironclad:make-prng :fortuna)))
>   (defun new-session-token ()
>     (cl-base64:usb8-array-to-base64-string
>      (ironclad:random-data 32 prng) :uri t)))
> ```
>   
> That's it.  
>   
> No encryption, no fiddling with `random`, no assigning results of `make-random-state` calls. Just initialize a `:fortuna` instnce, and collect random output in batches of 32.  
>   
> Wed, 16 Oct, 2013  

Other than that, what's left is putting together a session table with its own lock to store session information indexed by these IDs. Oh, and also sending them out to the client. I guess that's kind of important. Both are waiting for next time though, or this will quickly cease being "brief".


* * *
##### Footnotes

1 - <a name="foot-Tue-Oct-15-222939EDT-2013"></a>[|back|](#note-Tue-Oct-15-222939EDT-2013) - Unless you've spoofed it, as I often do to access the many "IE only" pages built by the legion of typing monkeys in my current companies' HR department.

2 - <a name="foot-Tue-Oct-15-222942EDT-2013"></a>[|back|](#note-Tue-Oct-15-222942EDT-2013) - Unless you're behind a proxy, or a dynamic IP.

3 - <a name="foot-Tue-Oct-15-222945EDT-2013"></a>[|back|](#note-Tue-Oct-15-222945EDT-2013) -I'm not implementing this myself, obviously. The current plan is still to hide behind nginx for static file serving, so we can have it handle SSL certificates for us to. [It's not even terribly difficult](http://nginx.org/en/docs/http/configuring_https_servers.html).

4 - <a name="foot-Tue-Oct-15-222952EDT-2013"></a>[|back|](#note-Tue-Oct-15-222952EDT-2013) - Except in the trivial sense that each active user should have a unique one, so if as an attacker you write a script to grab a few thousand keys, you can be sure that other people aren't using those specific ones.

5 - <a name="foot-Tue-Oct-15-222956EDT-2013"></a>[|back|](#note-Tue-Oct-15-222956EDT-2013) - Except in the trivial sense that you can avoid guessing short dictionary words, or dates or something.

6 - <a name="foot-Tue-Oct-15-222959EDT-2013"></a>[|back|](#note-Tue-Oct-15-222959EDT-2013) - That key, incidentally, should have similar properties to session tokens. It should be difficult to guess no matter how many of them you've seen, and running your own copy of Deal to extract a bunch of keys should give you no advantage when guessing another servers' secret key.

7 - <a name="foot-Tue-Oct-15-223003EDT-2013"></a>[|back|](#note-Tue-Oct-15-223003EDT-2013) - And that's not a certainty. I'm not exactly a math guy, so it's entirely possible that I'm misunderstanding the requirement at some step of this process. I'll certainly keep you up to date on any revelations.

8 - <a name="foot-Wed-Oct-16-092953EDT-2013"></a>[|back|](#note-Wed-Oct-16-092953EDT-2013) - When you think about it, all that can possibly do is make it slightly harder for attackers who've guessed a currently active session token, and if you've picked a Sufficiently Large©™ key space, the possibility of a guess seems to drop to negligible levels. Attackers who rely on sniffing get all the associated data you'll be including along with the session token, so they can still easily impersonate your users. Meanwhile, this method of guarding requires you to decrypt and validate a session token on each request you make. Not sure it's the right trade-off, but like I said, I'll keep you posted on revelations.

9 - <a name="foot-Wed-Oct-16-165818EDT-2013"></a>[|back|](#note-Wed-Oct-16-165818EDT-2013) - Also, the runtime of `ironclad:make-prng` is extremely inconsistent. It takes between 8 and 76 hippopotomi to complete, and I'm not entirely sure what plays into that. Possibly entropy shortages in the underlying OS? Which also reminds me; this version isn't Windows friendly. So if you were planning to run Deal on Windows, I'm deeply sorry for you.
