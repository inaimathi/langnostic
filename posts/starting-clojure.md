So I've been going to this [Coding Dojo](http://www.meetup.com/Toronto-Coding-Dojo/) thing, I guess. In an attempt to finally get off my ass and into Clojure, but also into 


- socializing with functional programmers outside of the Lisp group<a name="note-Thu-Aug-23-161334EDT-2012"></a>[|1|](#foot-Thu-Aug-23-161334EDT-2012)
- getting a bit more of a handle on test driven development<a name="note-Thu-Aug-23-161338EDT-2012"></a>[|2|](#foot-Thu-Aug-23-161338EDT-2012).


For the past two weeks, we've been (unsuccessfully so far, but no one is about to give up yet) trying to run through the poker hand kata in Clojure. Half the point here is trying out the language, and I've successfully procrastinated until they got a fantastic, standardized build system going so that I don't have to fuck around installing libraries by hand, which seems like it'll be very gratifying after the bunch of time spent in the Erlang world lately.

### <a name="installing-clojure" href="#installing-clojure"></a>Installing Clojure

[Clojure the debian package](http://packages.debian.org/sid/devel/clojure) is actually not in the free repos. You *can* `apt-get install clojure`, but only after [adding `contrib` *and* `non-free`](http://wiki.debian.org/Clojure) to your `sources.list`, which I don't particularly want to do. In case you haven't noticed yet, I'm the sort of person who occasionally runs `vrms`, just to make sure. It turns out though, that the [Clojure build tool](http://packages.debian.org/unstable/java/leiningen) can handle the task of installing the language for you, and provide faux-quicklisp/quickproject functionality *and* **is** in the free repos as of [`wheezy`](http://www.debian.org/releases/wheezy/). So, one

```
apt-get install leiningen
```

later had me on my feet. Or part of the way, at least. That install gives you `lein new` and `lein repl`, but doesn't by itself set up a development environment. In order to do *that*, I also had to `lein plugin install swank-clojure`, and shove [`clojure-mode`](https://github.com/technomancy/clojure-mode/) into my `.emacs`. At that point, I was ostensibly ready to start on a project, but `SLIME` and `swank-clojure` weren't playing nice for whatever reason. I still haven't puzzled it out, but the best idea any docs gave me was that Clojure really doesn't want you to have your own swank installed, thank you very much.

Given that I'm a professional Common Lisper these days, I had exactly zero chance of following that instruction. Instead, I wired up `clojure-mode` to [use the `inferior-lisp`](http://nakkaya.com/2009/12/01/adding-inferior-lisp-support-for-clojure-mode/) option by adding the following additional code to my `.emacs`

```lisp
(defun na-load-buffer ()
  (interactive)
  (point-to-register 5)
  (mark-whole-buffer)
  (lisp-eval-region (point) (mark) nil)
  (jump-to-register 5))


(defun clojure-run-test ()
  (interactive)
  (let ((b (get-buffer-create "*clojure-test*")))
    (with-current-buffer b
      (erase-buffer)
      (insert (shell-command-to-string "lein test")))
    (display-buffer b)))

;; inferior-lisp support.
;; Because fuck you, that's why.
(add-hook 'clojure-mode-hook
          '(lambda () 
             (define-key clojure-mode-map (kbd "C-c C-c") 'lisp-eval-defun)
             (define-key clojure-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
             (define-key clojure-mode-map (kbd "C-c C-e") 'lisp-eval-last-sexp)
             (define-key clojure-mode-map (kbd "C-c C-l") 'na-load-buffer)
             (define-key clojure-mode-map (kbd "C-c C-z") 'run-lisp)
             (define-key clojure-mode-map (kbd "C-c C-b") 'clojure-run-test)))
```

After all that, `run-lisp` in a Clojure buffer will start up a Clojure REPL, and the keyboard shortcuts I'm used to from `common-lisp-mode` will more or less work as before. `clojure-run-test` is mind-numbingly slow, and I don't get completions or arglist hints, but it's good enough for a start.

### <a name="trying-clojure" href="#trying-clojure"></a>Trying Clojure

The first thought that struck me was "Wait a minute, this looks a hell of a lot like Scheme". And really, that turns out to be pretty on the money, from what I can see so far at least. **Clojure is a JVM Scheme with curlies, brackets, an Arc-esque obsession with counting characters needed in the source code, and heavy emphasis on immutability.** That was bolded because, if you're in a hurry, you can basically stop reading now. If I were to offer advice about whether to learn it or not, I'd say


1.   if you need to do any extensive work on the JVM, use Clojure, it beats the alternatives
2.   if you don't know a Lisp yet, Clojure is a reasonable choice for your first<a name="note-Thu-Aug-23-161738EDT-2012"></a>[|3|](#foot-Thu-Aug-23-161738EDT-2012)
3.   if you already know Scheme or Common Lisp, and are comfortable with it, and don't go in for this JVM nonsense, don't bother learning Clojure because it'll teach you nothing new in the [Perlis](http://www.cs.yale.edu/quotes.html) sense


The differences are mostly in minutia, rather than the general principles of the language. I'll go through the few that are obvious from cursory poking, but if you're interested at all, you should take in [Clojure for Lisp Programmers Part 1](http://blip.tv/clojure/clojure-for-lisp-programmers-part-1-1319721) and [Part 2](http://blip.tv/clojure/clojure-for-lisp-programmers-part-2-1319826), in which Rich Hickey tells you basically everything I'm about to and a few more things besides. 

There are probably bigger differences than the ones I'll point out, consider this a "preliminary impressions" note, because I've yet to do anything more serious than an attempt at that poker hand kata.


- **Different Truth/Falsity Values** Clojure has an explicit `true` and `false`. `nil` and the empty list are  **not** equivalent<a name="note-Thu-Aug-23-162302EDT-2012"></a>[|4|](#foot-Thu-Aug-23-162302EDT-2012), and you're free to define one-letter local variables that designate `t`ime, `t`raffic or `t`otals. That's different from both CL and Scheme, and I'm sort of leaning towards calling it frivolous, but I'll see how it works out in practice<a name="note-Thu-Aug-23-162306EDT-2012"></a>[|5|](#foot-Thu-Aug-23-162306EDT-2012).
- **No Separate Function Namespace** Clojure cribs from Scheme here. A single function/variable namespace means you don't need to use `#'`, and it means you don't need separate `let`/`flet`. Oddly, there are two define forms<a name="note-Thu-Aug-23-162310EDT-2012"></a>[|6|](#foot-Thu-Aug-23-162310EDT-2012), but it's otherwise closer to the Scheme way of doing things.
- **Fewer Parentheses** I'm talking about `let` and `cond` bodies here. CL and Scheme both have you delimit each pair in an additional set of parens, while Clojure doesn't. This might make `transpose-sexps` a bit weirder on their clauses, but reduces the amount of typing you need to do by a tiny amount in the general case.
- **Polymorphic Built-Ins** The general equality test in Clojure is `=`, unlike CL or Scheme where you need to pick between `=`, `eq`, `eql`, etc. `first`, `last`, `map` and many others also work generically on sequences rather than just on lists.
- **Vectors Everywhere** `[1 2 3]` is "the vector of 1, 2, 3" rather than a list. Because of the polymorphic thing above, this doesn't introduce as much syntactic complexity as you'd think, and it means you don't need to worry about which end of a list you're taking from. Argument lists are all vectors rather than lists.
- **Destructuring By Default** I'm pretty used to whipping out  `destructuring-bind` in Common Lisp because it's sometimes the most straightforward way of expressing something. I don't use it nearly as often as often in CL as I do in Python or Erlang just because it doesn't save typing in nearly as many situations given what the construct looks like<a name="note-Thu-Aug-23-162317EDT-2012"></a>[|7|](#foot-Thu-Aug-23-162317EDT-2012). In Clojure, you can do something like

 ```clojure
user=>(def foo [1 2 3 4])
       #'user/foo
       user=> (let [[a b c d] foo] (list a b c d))
       (1 2 3 4)
```
 
- which means that I could start doing this much more frivolously.
- **Curlies and Brackets** Obviously. It's not as though CL *doesn't* have them, but they tend to get used very sparingly as part of reader macros. Clojure uses curlies to designate hash-maps/sets and `[]` to designate (among other things) vectors. Personally, I don't miss the JavaScript/jQuery matching hell that comes with nesting all three of them, but they don't seem to be mutually nesting in a lot of places, and [`paredit`](http://emacswiki.org/emacs/ParEdit) helps a lot anyway.
- **Whitespace Commas** The quote and backquote still work as expected, but the "unquote" modifier is `~` rather than `,`. This is another one that I see as frivolous, though I guess it could reduce cognitive friction for people who are used to delimiting lists with things other than spaces.



Two bigger ones that I feel the need to call out more prominently because I like them are [**multimethods**](http://clojure.org/multimethods) and **[doc](http://clojure.org/special_forms) [hashes](http://blog.fogus.me/2009/12/21/clojures-pre-and-post/)**.

If you're a Common Lisper, you're already used to multimethods. What's different about them in Clojure is that the generic function declaration takes a dispatch function. Which means that you can specialize methods on arbitrary properties, rather than just types. In Common Lisp, I occasionally have to declare a class for something just so that I can define methods for it, even if the thing I'm dispatching on really makes more sense as a slot than a class. The Clojure approach would save me code in these places.

Doc hashes are severely beefed up docstrings. Or, you could think of them as programming-by-contract-lite, I guess.  You still have the option of doing the usual docstring thing

```clojure
(defn read-card [card-string]
  "Takes a card string and returns a card hash with a :rank, :suit and :name"
  (let [rank (or (get rank-map (first card-string)) (read-string (str (first card-string))))
        suit (get suit-map (second card-string))
        name (get name-map rank)]
    {:rank rank :suit suit :name name}))
```

but if you want to get detailed, explicit, and compiler-checked, you have the option of doing something like

```clojure
(defn read-card [card-string]
  {:doc "Takes a card string and returns a card hash with a :rank, :suit and :name"
   :pre [(string? card-string) (= 2 (count card-string))]
   :post [(= clojure.lang.PersistentArrayMap (class %))]}  
  (let [rank (or (get rank-map (first card-string)) (read-string (str (first card-string))))
        suit (get suit-map (second card-string))
        name (get name-map rank)]
    {:rank rank :suit suit :name name}))
```

You can define inline tests too, if you want, but it's probably better to keep those in a separate test file. The static typists among you are probably snickering at this, but I like it better because these are *optional*. You don't *want* them on every function ever, you just want them on the potentially confusing functions, whose existence you should be trying to minimize. This is one step closer to getting code and documentation to coexist peacefully.

* * *
##### Footnotes

1 - <a name="foot-Thu-Aug-23-161334EDT-2012"></a>[|back|](#note-Thu-Aug-23-161334EDT-2012) - Though there is some overlap.
2 - <a name="foot-Thu-Aug-23-161338EDT-2012"></a>[|back|](#note-Thu-Aug-23-161338EDT-2012) - Which is actually a lot less painful with functional programming in general than it seemed to be for the various Java/PHP teams I've had the pleasure of UI-ing for.

3 - <a name="foot-Thu-Aug-23-161738EDT-2012"></a>[|back|](#note-Thu-Aug-23-161738EDT-2012) - Because it has the elegance of Scheme, combined with the production presence of Java meaning it'll be easier to convince your boss to let you use this than it will to let you use an actual Scheme, not that there's a lack of JVM options there.

4 - <a name="foot-Thu-Aug-23-162302EDT-2012"></a>[|back|](#note-Thu-Aug-23-162302EDT-2012) - Though `nil` does equate to `false` for boolean purposes.
5 - <a name="foot-Thu-Aug-23-162306EDT-2012"></a>[|back|](#note-Thu-Aug-23-162306EDT-2012) - As a note, having thought about it a little more, there are a couple of places where this is the unambiguously right thing to do, and I've yet to think up a situation where it'll trip me up.
6 - <a name="foot-Thu-Aug-23-162310EDT-2012"></a>[|back|](#note-Thu-Aug-23-162310EDT-2012) - `def` for variables and `defn` for functions.
7 - <a name="foot-Thu-Aug-23-162317EDT-2012"></a>[|back|](#note-Thu-Aug-23-162317EDT-2012) -  `(destructuring-bind (a b c) some-form-here &body)`
