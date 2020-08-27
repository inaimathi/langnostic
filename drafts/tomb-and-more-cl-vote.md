So apparently, there's no `bcrypt` implementation for Common Lisp. There's an [ffi wrapper](https://github.com/gigamonkey/monkeylib-bcrypt) which isn't in `quicklisp`, but that's all I could find. Which is mildly annoying, because as mentioned [last time](/posts/authentication-part-4.875), I need to store tokens basically the same way I would store passwords. There doesn't seem to be anything similar at a cursory glance, although it's always possible I missed something.

Oh well.

According to the [Wikipedia article pseudocode](https://en.wikipedia.org/wiki/Bcrypt#Algorithm), it looks like the essence of the algorithm is

- use the password as a key
- to encrypt the plaintext "OrpheanBeholderScryDoubt" using `blowfish` in `ECB` mode
- repeatedly some number of times (determined by the `cost` argument)

And the end result is a [sufficiently one-way](https://crypto.stackexchange.com/questions/41955/why-bcrypt-is-one-way-while-blowfish-is-reversible) function that lets you store some string to compare with input later without actually keeping that string on file.

So.

## `tomb`

I preface this by saying that I am not a crypto nerd. Probably don't use this in production anywhere, and definitely don't use it anywhere security is an actual concern. _I'm_ not aware of a way to back out the initial plaintext, but you should take [Schneier's advice](https://www.schneier.com/blog/archives/2011/04/schneiers_law.html) about what to think of that.

That being said, I've got this toy project with a `bcrypt`-shaped hole in its `:depends-on` list, and may as well try something.

```
;;;; src/tomb.lisp
(in-package #:tomb)

(defparameter *gen* (session-token:make-generator :token-length 16))

(defun entomb (string &key (salt (funcall *gen*)) (cost 10) (cipher-name :blowfish))
  (let* ((arr (ironclad:ascii-string-to-byte-array (concatenate 'string string salt)))
	 (initial-hash (hash-for-tomb arr cipher-name))
	 (cipher (ironclad:make-cipher cipher-name :key initial-hash  :mode :ecb))
	 (output (make-sequence '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) (length initial-hash))))
    (ironclad:encrypt cipher initial-hash output)
    (loop repeat (expt 2 cost)
       do (ironclad:encrypt-in-place
	   (ironclad:make-cipher cipher-name :key output :mode :ecb)
	   output))
    (format nil "$0w$~a$~a$~a$~a"
	    cipher-name
	    cost
	    salt
	    (ironclad:byte-array-to-hex-string output))))

(defun hash-for-tomb (arr cipher-name)
  (ironclad:digest-sequence
   (case cipher-name
     (:threefish512 :sha512)
     (:threefish1024 :skein1024)
     (t :sha256))
   arr))

(defun tomb-matches? (string hashed)
  (destructuring-bind (name cipher-name cost salt hash) (split-sequence:split-sequence #\$ hashed :remove-empty-subseqs t)
    (declare (ignore hash))
    (assert (string= name "0w"))
    (let ((cost (parse-integer cost))
	  (cipher-name (intern cipher-name :keyword)))
      (string= (entomb string :salt salt :cost cost :cipher-name cipher-name) hashed))))
```

Principles first.

1. *Sane defaults* - We don't want to make the user[^ie-me] do any more work than they have to. Which means that the minimal call to the top level interface should be something that goes `String -> String` rather than needing the user to generate their own salt, specify a cipher or do any type conversions.
2. *Flexible implementation* - We shouldn't _assume_ a particular salting strategy, input size, or cipher. We need to limit ourselves to `ECB` mode, because changing that is deep magic that I'm not getting anywhere near without a deeper understanding.
3. *Use Crypto Primitives* - Speaking of deep magic, we're not writing anything ourselves from the bytes up. [`ironclad`](https://github.com/sharplispers/ironclad) is a thing, and it works well if sometimes counter-intuitively, and I fully intend to take advantage.

[^ie-me]: Me.

With that out of the way, here's `tomb`, which is sort of like `crypt`.

```
...
(defun entomb (string &key (salt (funcall *gen*)) (cost 10) (cipher-name :blowfish))
  (let* ((arr (ironclad:ascii-string-to-byte-array (concatenate 'string string salt)))
	 (initial-hash (hash-for-tomb arr cipher-name))
	 (cipher (ironclad:make-cipher cipher-name :key initial-hash  :mode :ecb))
	 (output (make-sequence '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) (length initial-hash))))
    (ironclad:encrypt cipher initial-hash output)
    (loop repeat (expt 2 cost)
       do (ironclad:encrypt-in-place
	   (ironclad:make-cipher cipher-name :key output :mode :ecb)
	   output))
    (format nil "$0w$~a$~a$~a$~a"
	    cipher-name
	    cost
	    salt
	    (ironclad:byte-array-to-hex-string output))))
...
```

The core function is `entomb`. It takes a `string` (your password/passphrase), and optionally also `salt`, `cost` and `cipher-name`. If you don't pass in any of those, it chooses sane defaults, including using `session-token`/`cl-isaac` to generate a secure random salt value.

The first thing we do is `concatenate` the `string` and `salt` values, convert the result to an `ironclad` `byte-array`, then hash it. Hashing it using some secure digest method that produces the appropriate number of bytes to be used as a `key` for the chosen `cipher`.

```
...
(defun hash-for-tomb (arr cipher-name)
  (ironclad:digest-sequence
   (case cipher-name
     (:threefish512 :sha512)
     (:threefish1024 :skein1024)
     (t :sha256))
   arr))
...
```

It looks like `sha256` is good enough for most of the `ECB` capable ciphers in `ironclad`, but `threefish512` and `threefish1024` need larger keys than it provides, so we use other approaches when using those ciphers.

```
...
	 (cipher (ironclad:make-cipher cipher-name :key initial-hash  :mode :ecb))
	 (output (make-sequence '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) (length initial-hash))))
    (ironclad:encrypt cipher initial-hash output)
    (loop repeat (expt 2 cost)
       do (ironclad:encrypt-in-place
	   (ironclad:make-cipher cipher-name :key output :mode :ecb)
	   output))
    (format nil "$0w$~a$~a$~a$~a"
	    cipher-name
	    cost
	    salt
	    (ironclad:byte-array-to-hex-string output))))
...
```

Next up, we initialize an `ironclad` cipher with the appropriate base state, and allocate an output `simple-array` to stuff the results in. Then we use the initialized `cipher` to `ironclad:encrypt` our input hash (complete with `salt`) and put the results in `output`. Once that's done, we `encrypt-in-place` the output with the same settings, changing out the key each time. The thing we're encrypting the first time though is the `key` (with itself), and every subsequent layer of encryption also uses itself as the key.

Once we've done this, we stitch everything together into a string that contains documentation about its' creation.

```
(defun tomb-matches? (string hashed)
  (destructuring-bind (name cipher-name cost salt hash) (split-sequence:split-sequence #\$ hashed :remove-empty-subseqs t)
    (declare (ignore hash))
    (assert (string= name "0w"))
    (let ((cost (parse-integer cost))
	  (cipher-name (intern cipher-name :keyword)))
      (string= (entomb string :salt salt :cost cost :cipher-name cipher-name) hashed))))
```

`tomb-matches?` takes a string and an `entomb`ed string, and returns a yay or nay about whether they match. It does this by decomposing the `entomb`ed string in a way that lets it figure out what arguments to pass to `entomb`, and does so on the input string.

## Next Step

This library is now [on `github`](https://github.com/inaimathi/tomb) in case you are like me, and want to experiment with low-security-but-principled systems. For my part, I'll probably add it to `quicklisp`, and definitely as a requirement to `cl-vote` so that I can put together a good recovery token system.

It mildly amuses me to think that knowing that token in this case is technically a "known plaintext" attack.
