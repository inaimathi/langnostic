[Last time](http://langnostic.blogspot.ca/2013/12/jef-raskin-on-authentication.html), I mentioned the auth system proposed by Raskin in his book [The Humane Interface](http://www.amazon.com/The-Humane-Interface-Directions-Interactive/dp/0201379376). This time, lets dissect something concrete.

Actually, before we get to dissecting anything, let me emphasize again that the *most* humane approach to authentication is not requiring it. If you have a system that could be exposed to anonymous users, that's what you should do. *If* you've decided that you absolutely must have some sort of authentication step, then this doesn't seem to be a bad way to go.

Now then...

```lisp
;;;; raskin-auth.asd
(asdf:defsystem #:raskin-auth
  :serial t
  :description "Implementation of the authentication system proposed in The Humane Interface"
  :author "Inaimathi &lt;[leo.zovic&#64;gmail.com](mailto:leo.zovic&#64;gmail.com)>"
  :license "AGPL, as usual"
  :depends-on (#:cl-ppcre #:ironclad)
  :components ((:file "package")
               (:file "util")
               (:file "raskin-auth")))
```

```lisp
;;;; package.lisp
(defpackage #:raskin-auth
  (:use #:cl)
  (:export #:new-account! #:sign-in))
```

```lisp
;;;; util.lisp
(in-package #:raskin-auth)

;;;; Dictionary-related
(defconstant +dict+ (coerce 
                     (with-open-file (s "/usr/share/dict/american-english")
                       (loop for line = (read-line s nil :eof) until (eql line :eof)
                          unless (cl-ppcre:scan "[^a-zA-Z]" line)
                          collect (string-downcase line)))
                     'vector))

(defun random-words (count &optional (dict +dict+))
  (loop repeat count
     collect (aref dict (random (length dict)))))

;;;; Hash-related
(defmethod iterated-digest ((count integer) (digest-spec symbol) (message string))
  (assert (> count 0))
  (loop with res = (ironclad:ascii-string-to-byte-array message)
     repeat count do (setf res (ironclad:digest-sequence digest-spec res))
     finally (return res)))
```

```lisp
;;;; raskin-auth.lisp
(in-package #:raskin-auth)

(setf *random-state* (make-random-state t))

(defparameter *users* (make-hash-table :test 'equal))

(defun hash (passphrase)
  (ironclad:byte-array-to-hex-string (iterated-digest 10000 :sha256 passphrase)))

(defun fresh-passphrase ()
  (let ((u-mod (/ (hash-table-count *users*) 100000)))
    (loop for num-words = (+ 2 (floor u-mod) (random (+ 2 (ceiling u-mod))))
       for passphrase = (format nil "~{~(~a~)~^-~}" (random-words num-words))
       unless (gethash passphrase *users*) do (return passphrase))))

(defun new-account! (&optional (user-data t))
  (let ((passphrase (fresh-passphrase)))
    (setf (gethash (hash passphrase) *users*) user-data)
    passphrase))

(defun sign-in (passphrase)
  (gethash (hash passphrase) *users*))
```

And that's it. In a production system, you'd obviously want to wire everything up to some database system or other rather than using an in-memory hash-table, but this explains the concept well enough. You'd use this module by including it, then calling `(new-account! [user account data goes here])` (which will return a newly generated passphrase) and `(sign-in "a-passphrase-goes-here")` (which will return either `nil` or the account data you associated with the given passphrase) as necessary.

Lets go through it.

```lisp
;;;; raskin-auth.asd
(asdf:defsystem #:raskin-auth
  :serial t
  :description "Implementation of the authentication system proposed in The Humane Interface"
  :author "Inaimathi &lt;[leo.zovic&#64;gmail.com](mailto:leo.zovic&#64;gmail.com)>"
  :license "AGPL, as usual"
  :depends-on (#:cl-ppcre #:ironclad)
  :components ((:file "package")
               (:file "util")
               (:file "raskin-auth")))
```

```lisp
;;;; package.lisp
(defpackage #:raskin-auth
  (:use #:cl)
  (:export #:new-account! #:sign-in))
```

That's the ASD file and package. The first makes sure you can load this system using [`asdf`](http://common-lisp.net/project/asdf/) or [`quicklisp`](http://www.quicklisp.org/beta/), and the second declares your imports and exports. I'm trying something new this time and refusing to use `:use` or `:import-from` and friends. I've gotten a couple comments to the effect that it gets a bit confusing if I import symbols directly rather than labeling them inline with the package they came from, so even though `raskin-auth` *does* use things from both [`ironclad`](http://method-combination.net/lisp/ironclad/) and [`cl-ppcre`](http://weitz.de/cl-ppcre/), the `package.lisp` file is staying minimal.

```lisp
;;;; util.lisp
(in-package #:raskin-auth)

;;;; Dictionary-related
(defconstant +dict+ (coerce 
                     (with-open-file (s "/usr/share/dict/american-english")
                       (loop for line = (read-line s nil :eof) until (eql line :eof)
                          unless (cl-ppcre:scan "[^a-zA-Z]" line)
                          collect (string-downcase line)))
                     'vector))

(defun random-words (count &optional (dict +dict+))
  (loop repeat count
     collect (aref dict (random (length dict)))))

;;;; Hash-related
(defmethod iterated-digest ((count integer) (digest-spec symbol) (message string))
  (assert (> count 0))
  (loop with res = (ironclad:ascii-string-to-byte-array message)
     repeat count do (setf res (ironclad:digest-sequence digest-spec res))
     finally (return res)))
```

`random-words` creates a list of `count` random words by picking them out of a dictionary, which is `+dict+` by default. You don't necessarily want these words to be unique, so we don't check for that. `+dict+` is just some slightly sanitized output from `/usr/share/dict/american-english`, which is where Debian keeps the default English language dictionary. The result of that read is a `vector` of all words in the dict file that are composed entirely of lowercase letters. What we're doing, essentially is `shuf -n [count] /usr/share/dict/american-english`. Except we're filtering for some stuff, so that should really get piped through a `grep` or two. Use whatever method you'd like; the end goal is to get a list of `count` random words, from a list of ~60000 different words, each with an equal probability.

`iterated-digest` takes a `count`, a `digest-spec` and a `message`, and applies the specified `digest` to the `message` `count` times sequentially. We'll take a look at how you call it in a second.

```lisp
;;;; raskin-auth.lisp
(in-package #:raskin-auth)

(setf *random-state* (make-random-state t))

(defparameter *users* (make-hash-table :test 'equal))

(defun hash (passphrase)
  (ironclad:byte-array-to-hex-string (iterated-digest 10000 :sha256 passphrase)))

(defun fresh-passphrase ()
  (let ((u-mod (/ (hash-table-count *users*) 100000)))
    (loop for num-words = (+ 2 (floor u-mod) (random (+ 2 (ceiling u-mod))))
       for passphrase = (format nil "~{~(~a~)~^-~}" (random-words num-words))
       unless (gethash passphrase *users*) do (return passphrase))))

(defun new-account! (&optional (user-data t))
  (let ((passphrase (fresh-passphrase)))
    (setf (gethash (hash passphrase) *users*) user-data)
    passphrase))

(defun sign-in (passphrase)
  (gethash (hash passphrase) *users*))
```

`*users*` is a hash table that'll keep all of our user records<a name="note-Sat-Dec-14-125727EST-2013"></a>[|1|](#foot-Sat-Dec-14-125727EST-2013), and both `new-account!` and `sign-in` are hopefully self explanatory. Let me linger on the rest of that though.

First, you absolutely positively need the `*random-state*` initialization. Without that line, your system will generate the same order of passphrases each time it starts up. Maybe that's not too big a deal in general, but I'm paranoid enough that I want proper, os-seeded randomness out when I'm generating authentication tokens.

Second, you can see the `iterated-digest` call here:

```lisp
(defun hash (passphrase)
  (ironclad:byte-array-to-hex-string (iterated-digest 10000 :sha256 passphrase)))
```

That takes a particular `passphrase` string and returns the result of applying the `:sha256` digest to it 10000 times. I guess you could make that `:sha512` if you really wanted to.

Finally, `fresh-passphrase` does the job of calling `random-words`, concatenating the result, and checking whether the result of *that* is already on record. It keeps going until it generates a passphrase that no one else is using at the moment, and returns that. You can see that it scales somewhat with count of users registered, just to make sure we don't get into the situation where a particular passphrase length is particularly easy to guess.

That's it. Again, what I see here is reasonable security.

### Thoughts

On the one hand, you don't get to salt passphrase hashes. Which means that if anyone manages to trick a user of this auth system into revealing their ciphertexts, they'll have a mildly easier time cracking the result. And, since every passphrase is unique, they can knock out some tiny number of possibilities as they go. You also can't easily change your hashing tactic in-flight. Hypothetically, if you chose the iterated `:sha256` approach from above, and it then turned out that clever people found ways to compromise that hash, you wouldn't be able to switch your tactics on a live system easily, the way you could with a user-name-oriented system. You *would* be able to increase the number of hashings fairly easily; just modify your `hash` to do more iterations, and modify your registered users' passwords to make up the difference.

On the *other* hand, no one will ever have the passphrase `123` with this system. And, since they didn't pick it, they presumably won't have this same passphrase on any other service they frequent, which means a compromise here won't have to result in a mad dash to change their account passwords anywhere else fo fear of [exploits](http://xkcd.com/792/).

The only other downsides seem to be that you can't choose a passphrase, and that if you forget your passphrase, you must create a new account.



* * *
##### Footnotes
1 - <a name="foot-Sat-Dec-14-125727EST-2013"></a>[|back|](#note-Sat-Dec-14-125727EST-2013) -  *Because* it's a hash table, and I don't bother doing any kind of locking, the system you see specified here very likely won't do for any multi-threaded use-cases. You can either add locks, or go the whole nine and replace that hash table with an external database, but I don't need either to see the basic properties of the system, so I didn't implement them.
