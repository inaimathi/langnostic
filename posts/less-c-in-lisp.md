It seems that I only ever get around to working on this pet project when I'm sick (which I was earlier this week). It's taken almost 5 months at this point, but the `hours` counter is really closer to ~15, which means that I could have done the work during a single, particularly slow, weekend.

Anyway, moving on, I've been plaing around with the codebase for Elite for Emacs (and there's [a post around here somewhere](http://langnostic.blogspot.com/2011/01/c-in-lisp.html) that details some of the blunders  it contains). Today, I'm dealing with the next level up; not pointing out where primitives are being misused, but pointing out needless patterns where they don't belong and showing one way of composing them properly. Actually, now that I look at it, I'd better take a single pattern out and deconstruct it lest I bore the ever-living shit out of everyone, including me. I'm also not eliding anything this time, this is going to deal with specifics from the [Elite for Emacs 0.1](http://members.fortunecity.com/salkosuo/elite-for-emacs/0.10.0/index.html) codebase and how I'm thinking about re-implementing them.

### Describing Things

Actually, before I get to that one,

### Random Numbers

At a cursory examination, `elite-for-emacs-*.el` contains `myrand`, `randbyte`, `rand1` **and** `gen_rnd_number` (and no uses of the the built-in `rand` function). They may or may not do similar things. The author also insists on tracking his own random number seed in a global variable (and re-generating it with a function named `mysrand`). Here's a sample

```lisp
(defun gen_rnd_number ()
  (let ((a)
        (x))        
    (setq x (logand (* (fastseedtype-a rnd_seed) 2) #xFF));
    (setq a (+ x (fastseedtype-c rnd_seed)))
    (if (> (fastseedtype-a rnd_seed) 127)
        (setq a (1+ a)))
    (setf (fastseedtype-a rnd_seed) (logand a #xFF))
    (setf (fastseedtype-c rnd_seed) x)
    (setq a (/ a 256)); /* a = any carry left from above */
    (setq x (fastseedtype-b rnd_seed))

    (setq a (logand (+ a x (fastseedtype-d rnd_seed)) #xFF))
    
    (setf (fastseedtype-b rnd_seed) a)
    (setf (fastseedtype-d rnd_seed) x)
    a))
```

I'm not sure why Lisp coders get stick for re-implementing infrastructure if **this** is reasonably common in the outside world. Building your own byte-oriented random number generator is something a Lisp **can** do, but<a name="note-Wed-Apr-20-105412EDT-2011"></a>[|1|](#foot-Wed-Apr-20-105412EDT-2011) you really **shouldn't**. If you were in the middle of writing your own implementation of `rand` in Elisp, Common Lisp or Scheme before you started reading this, please just do us both a favor and stop.

Now then.

### Describing Things

Here's how Elite for Emacs generates planet descriptions.

```lisp
(defun elite-for-emacs-planet-description (galaxy-index system-index)
  "Return planet description"
  (let ((planet-sys)
        (rnd_seed))
    (setq planet-sys (aref (aref elite-for-emacs-galaxies-in-universe galaxy-index) system-index))
    (setq rnd_seed (copy-fastseedtype (plansys-goatsoupseed planet-sys)))
    (setq elite-for-emacs-planet-description "")
    (goat_soup "\x8F is \x97." planet-sys)
    elite-for-emacs-planet-description))
```

Which actually lulled me into a false sense of security the first time around because it seemed


1.   functional-ish *(it isn't upon closer inspection, note that it returns `elite-for-emacs-planet-description`, which isn't set anywhere in the body of the definition)*
1.   short *(once you understand everything it calls... not so much)*
1.   simple *(maybe I'd better just show you)*


There's at least one thing there that should have set off definite alarms though. What kind of name is `goat_soup`?

```lisp
(defun goat_soup (source planet-sys)
  (let ((c)
        (rnd)
        (source-list nil)
        (tmp)
        (i)
        (len)
        (x))
      (setq tmp (split-string source ""))
      (setq source-list nil)
      (while tmp
        (setq c (car tmp))
        (setq source-list (append source-list (list (string-to-char c))))
        (setq tmp (cdr tmp)))
      (while source-list
        (setq c (car source-list))
            (if (&lt; c #x80)
                (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description (list c)))
              (progn
                (if (&lt;= c #xa4)
                    (progn (setq rnd (gen_rnd_number))
                      (setq tmp 0);;true: non-zero, zer=false
                      (if (>= rnd #x33)
                          (setq tmp (1+ tmp)))
                      (if (>= rnd #x66)
                          (setq tmp (1+ tmp)))
                      (if (>= rnd #x99)
                          (setq tmp (1+ tmp)))
                      (if (>= rnd #xCC)
                          (setq tmp (1+ tmp)))
                      (goat_soup (nth tmp (nth (- c #x81) desc_list)) planet-sys); .option[()+(rnd >= 0x66)+(rnd >= 0x99)+(rnd >= 0xCC)] planet-sys))
                  (progn ;;switch...
                    (cond ((= c #xB0);;planet name
                           (setq elite-for-emacs-planet-description 
                                 (concat elite-for-emacs-planet-description 
                                         (capitalize (plansys-name planet-sys))))
                           ;;(insert (capitalize (plansys-name planet-sys)))
                      )
                     ((= c #xB1);; /* &lt;planet name>ian */
                      (setq tmp (capitalize (plansys-name planet-sys)))
                      (if (and (not (string-match "e$" tmp)) (not (string-match "i$" tmp)))
                          (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description tmp))
                        (progn ;;(setq tmp "helleinooio")
                          (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description (substring tmp 0 (1- (length tmp))) "ian" ));;(insert (substring tmp 0 (1- (length tmp))))
                          )))
                     ((= c #xB2);;/* random name */
                      (setq i 0)
                      (setq len (logand (gen_rnd_number) 3))
                      (while (&lt;= i len)
                        (setq x (logand (gen_rnd_number) #x3e))
                        (if (/= (aref pairs x) 46);;46='.' (string-to-char ".")
                            (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description (char-to-string (aref pairs x))))
                             )
                         (if (and (> i 0) (/= (aref pairs (1+ x)) 46))
                             (setq elite-for-emacs-planet-description (concat elite-for-emacs-planet-description (char-to-string (aref pairs (1+ x)))))
                             )
                         (setq i (1+ i)))
;;                                              case 0xB2: /* random name */
;;                              {       int i;
;;                                      int len = gen_rnd_number() & 3;
;;                                      for(i=0;i&lt;=len;i++)
;;                                      {       int x = gen_rnd_number() & 0x3e;
;;                                              if(pairs0[x]!='.') printf("%c",pairs0[x]);
;;                                              if(i && (pairs0[x+1]!='.')) printf("%c",pairs0[x+1]);
;;                                      }
;;                              }       break;

                       ))))))
            (setq source-list (cdr source-list)))))
```

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}" href="http://2.bp.blogspot.com/-6XK71tBvjp8/Ta7911tWDOI/AAAAAAAAAHE/RCFI3ygWKuI/s1600/My-mind-is-full-of-setq.jpg">![](http://2.bp.blogspot.com/-6XK71tBvjp8/Ta7911tWDOI/AAAAAAAAAHE/RCFI3ygWKuI/s400/My-mind-is-full-of-setq.jpg)</a>

The kind that designates a procedure built out of dead things, most of which you'd really rather not think about. 

A casual reading shows quite a bit of side effect, and use of at least three of the anti-patterns I mentioned [last time around](http://langnostic.blogspot.com/2011/01/c-in-lisp.html). I've removed the irrelevant comments<a name="note-Wed-Apr-20-105710EDT-2011"></a>[|2|](#foot-Wed-Apr-20-105710EDT-2011), but the above is still considerably longer than what I consider good style for a single function, and complex enough that I was inclined to think "rewrite" even before I went through it. Later on, there's a snippet of code that looks like

```lisp
(defconst desc_list
  (list
;; 81 */
        (list "fabled" "notable" "well known" "famous" "noted")
;; 82 */
        (list "very" "mildly" "most" "reasonably" "")
;; 83 */
        (list "ancient" "\x95" "great" "vast" "pink")
;; 84 */
        (list "\x9E \x9D plantations" "mountains" "\x9C" "\x94 forests" "oceans")
;; ... continues for a further 69 lines
```

What it does in context, basically, is take the string `"\x8F is \x97."` and expand it out recursively until all the "byte" references are gone and it ends up with a little semi-sensical, explanatory description like "The planet is reasonably famous for its inhabitants' ingrained shyness but scourged by deadly edible wolfs." or "The planet is famous for its pink parking meters.". 

The problem I hinted at last time, and wanted to discuss this time out, is this idea of byte-orientation. This is an architecture built by someone used to assembly or C, that then tried to shoehorn the same way of looking at the world into Lisp. I wouldn't mind so much, but it's far too easy to imagine someone hacking together a system like this and thinking to themselves "Wow, this really sucks. I could have done it MUCH more efficiently in C, and I wouldn't have had to deal with all this 'list' nonsense. I guess Lisp is just a language for masochists...". Going against the grain of any language creates the impression that it's less powerful than it really is, and this is a prime example. The author uses excessive byte and integer indexing operations where simpler (and, in this case, more performant) lisp primitives like `rand`, `nth` and plist/alist/hash-tables would do.

I took a minute out at the beginning of this post to point out how this codebase re-implements random number generation at a very low level. Well, the reason I consider it an egregious mistake here is that the main place I found that particular generator used is in `goat_soup` above, where it's used to index into a vector of expansions. The really funny part is that I could see this being implemented as a performance/space optimization in a C version of the game, but when you're dealing with string representations of bytes that you have to split and convert before operating on, any gains fly directly out the window.

It's beside the point, though. Remember, Lisp is a symbolic language. So here's a Lispier way of generating some planet descriptions.

```lisp
(defparameter *planet-desc-grammar*
  (list :root '((" is " :reputation " for " :subject) 
                (" is " :reputation " for " :subject " and " :subject) 
                (" is " :reputation " for " :subject 
                 " but " :adj-opposing-force " by " :historic-event)
                (" is " :adj-opposing-force " by " :historic-event) 
                (", a " :adj-negative " " :syn-planet))
        :subject '(("its " :adjective " " :place) 
                   ("its " :adjective " " :passtime) 
                   ("the " :adj-fauna " " :fauna) 
                   ("its inhabitants' " :adj-local-custom 
                    " " :inhabitant-property) 
                   :passtime) 
        :passtime '((:fauna " " :drink) (:fauna " " :food) 
                    ("its " :adjective " " :fauna " " :food) 
                    (:adj-activity " " :sport) 
                    "cuisine" "night-life" "casinos" "sit-coms") 
        :historic-event '((:adj-disaster " civil war") 
                          (:adj-threat " " :adj-fauna " " :fauna "s") 
                          ("a " :adj-threat " disease") 
                          (:adj-disaster " earthquakes") 
                          (:adj-disaster " solar activity")) 
        :place '((:fauna :flora " plantations") (:adj-forest " forests") 
                 :scenery "forests" "mountains" "oceans")
        :technology '(:passtime "food blenders" "tourists" "poetry" "discos") 
        :inhabitant-property '(("loathing of " :technology) 
                               ("love for " :technology) 
                               "shyness" "silliness" "mating traditions") 
        :fauna '("talking tree" "crab" "bat" "lobster" "shrew" "beast" "bison" 
                 "snake" "wolf" "yak" "leopard" "cat" "monkey" "goat" "fish" 
                 "snail" "slug" "asp" "moth" "grub" "ant") 
        :flora '((:fauna "-weed") "plant" "tulip" "banana" "corn" "carrot") 
        :scenery '("parking meters" "dust clouds" "ice bergs" 
                   "rock formations" "volcanoes") 
        :reputation '((:emphasis " " :reputation) 
                      "fabled" "notable" "well known" "famous" "noted") 
        :emphasis '("very" "mildly" "most" "reasonably") 
        :drink '("juice" "brandy" "water" "brew" "gargle blasters") 
        :sport '("hockey" "cricket" "karate" "polo" "tennis" "quiddich") 
        :food '("meat" "cutlet" "steak" "burgers" "soup") 
        :adjective '((:emphasis " " :adjective) 
                     :adj-local-custom :adj-fauna :adj-forest :adj-disaster 
                     "great" "pink" "fabulous" "hoopy" 
                     "funny" "wierd" "strange" "peculiar") 
        :adj-fauna '(:adj-threat "mountain" "edible" "tree" "spotted" "exotic") 
        :adj-negative '((:adj-negative ", " :adj-negative) 
                        "boring" "dull" "tedious" "revolting") 
        :adj-local-custom '("ancient" "exceptional" "eccentric" "ingrained" "unusual") 
        :adj-forest '("tropical" "vast" "dense" "rain" "impenetrable" "exuberant") 
        :adj-disaster '("frequent" "occasional" "unpredictable" "dreadful" :adj-threat) 
        :adj-threat '("killer" "deadly" "evil" "lethal" "vicious") 
        :adj-activity '("ice" "mud" "zero-g" "virtual" "vacuum" "Australian, indoor-rules") 
        :adj-opposing-force '("beset" "plagued" "ravaged" "cursed" "scourged") 
        :syn-planet '("planet" "world" "place" "little planet" "dump")))
```

That's the data, at any rate. The above uses all the original words and combinations from the Elisp codebase (I did add "Australian, indoor-rules" to the activity adjective list, and made sure "quiddich" was a possible sport, but that's all), so the descriptions popping out of it should be the same as those coming out of `goat_soup` and friends. Note that the `*` surrounding the variable name denote a global variable<a name="note-Wed-Apr-20-110208EDT-2011"></a>[|3|](#foot-Wed-Apr-20-110208EDT-2011). Note also that instead of using byte relations, the `plist` approach lets me avoid splitting strings in intermediate steps. A terminal is a string and a non-terminal is an atom. The way to unfold these is

```lisp
(defun expand-production (production grammar)
  (cond ((stringp production) production)
        ((symbolp production) 
         (expand-production (pick-g production grammar) grammar))
        ((listp production) 
         (reduce (lambda (a b) 
                   (concatenate 'string a (expand-production b grammar))) 
                 (cons "" production)))))
```

`pick-g` is a function that takes a key and grammar, and returns a random expansion of that key in that grammar.

```lisp
(defun pick-g (key grammar) 
  (let ((choices (getf grammar key)))
    (nth (random (length choices)) choices)))
```

In other words, 


-   a string gets returned
-   an atom gets expanded (by looking it up in the grammar and picking a random possible expansion)
-   a list gets expanded (by expanding each of its elements)


```lisp
* (expand-production :root *planet-desc-grammar*)

" is fabled for its inhabitants' ancient love for ice tennis"
* (expand-production :root *planet-desc-grammar*)

", a revolting little planet"
```

All I have to make sure is that these get displayed along with the planet name and we're golden. This is the sort of elegance that Lisp is capable of when you go with the grain. ~140 lines of flaming death and side effects replaced by two recursive functions and a `plist` that succinctly and accurately signal the intent of the programmer. I wouldn't be particularly surprised if there's an even simpler way to accomplish the same thing, actually. 

### Naming Things

Planet names seem like they should be implemented the same way, given what they really are. In fact, I did just implement them as another grammar that depends on the same functions to unfold, but the original takes a different approach.

```lisp
;; buried at line ~103 of a single function that generates a planet
  ;set name
  ;init alphabet pairs
  (setq pair1 (* (logand (lsh (seedtype-w2 s) -8) 31) 2))
  (tweakseed s)
  (setq pair2 (* (logand (lsh (seedtype-w2 s) -8) 31) 2))
  (tweakseed s)
  (setq pair3 (* (logand (lsh (seedtype-w2 s) -8) 31) 2))
  (tweakseed s)
  (setq pair4 (* (logand (lsh (seedtype-w2 s) -8) 31) 2))
  (tweakseed s)
  ;Always four iterations of random number
  (setq planet-name
        (concat 
         (code-to-char (aref pairs pair1))
         (code-to-char (aref pairs (1+ pair1)))
         (code-to-char (aref pairs pair2))
         (code-to-char (aref pairs (1+ pair2)))
         (code-to-char (aref pairs pair3))
         (code-to-char (aref pairs (1+ pair3)))))
  (if (/= longnameflag 0)
      (progn
        (setq planet-name 
              (concat
               planet-name
               (code-to-char (aref pairs pair4))
               (code-to-char (aref pairs (1+ pair4)))))))
  (setf (plansys-name thissys) (stripout planet-name "."))
```

`pairs` showed up in `goat_soup` too, and it's defined as 

```lisp
(defconst pairs 
  "..LEXEGEZACEBISOUSESARMAINDIREA.ERATENBERALAVETIEDORQUANTEISRION"
  "Characters for planet names.")
```

`s` is passed as an argument to the planet-generator, the rest of which I won't inflict upon you, I think you may have gotten the idea already. In other words, planets are restricted to 4 syllable names put together by side-effect in the procedure that creates planets. They're put together by using byte operations on a string that represents the valid pair combinations contained in a planet name.

Well, seeing as I already put together a convention for unfolding components to strings by using a recursive function and a `plist`, I figured I'd do the same for this. Code reuse is good, I hear.

```lisp
(defparameter *planet-name-grammar*
  ;be mindful of name probabilities if you try to reduce duplication here
  (list :root '((:starter :link :ender) (:starter :partition :ender) 
                (:starter :partition :link :ender) 
                (:starter :partition :root) 
                (:starter :link :link :ender) (:starter :ender))
        :starter '((:starter :link)
                   "aa" "ae" "al" "an" "ao" "ar" "at" "az" "be" 
                   "bi" "ce" "di" "ed" "en" "er" "es" "ge" "in" 
                   "is" "la" "le" "ma" "on" "or" "qu" "ra" "re" 
                   "ri" "so" "te" "ti" "us" "ve" "xe" "za")
        :ender '((:link :ender) 
                 "aa" "al" "at" "di" "ti" "so" "ce" "re" "za" 
                 "in" "ed" "or" "an" "ma" "ab" "ge" "aq" "en" 
                 "ri" "ve" "ag" "qu" "us" "es" "ex" "ae" "on" 
                 "bi" "xe" "le" "is" "er" "be" "la" "ar" "az" 
                 "io" "sb" "te" "ra" "ia" "nb")
        :link '((:link :link) (:link :link)
                "at" "an" "ri" "es" "ed" "bi" "ce" "us" "on" 
                "er" "ti" "ve" "ra" "la" "le" "ge" "i" "u" 
                "xe" "in" "di" "so" "ar" "e" "s" "na" "is" 
                "za" "re" "ma" "or" "be" "en" "qu" "a" "n" 
                "r" "te" "t")
        :partition '("-" "'" " ")))
```

<a name="note-Wed-Apr-20-110844EDT-2011"></a>[|4|](#foot-Wed-Apr-20-110844EDT-2011)And that's that. It unfolds into planet names with the same mechanisms

```lisp
* (string-capitalize (expand-production :root *planet-name-grammar*))

"Ri Orleio"
* (string-capitalize (expand-production :root *planet-name-grammar*))

"Xenain"

* (string-capitalize (expand-production :root *planet-name-grammar*))

"Es'Ae"
```

`string-capitalize` just makes sure it looks like a proper name (it's [a lisp primitive](http://www.lispworks.com/documentation/HyperSpec/Body/f_stg_up.htm#string-capitalize), so I won't define it here). The important part, which I'll likely cover in a future post, is that making things functional aids in composeability. The `setq` sequence from the original codebase has no hope of being reused anywhere because it intentionally grubs about in the surrounding state. If nothing else, the `expand-production` approach ensures that if I ever need a planet name in some other context, I can easily generate it. Also, as we've seen already, abstracting out the general pattern of "compose strings from a given pattern of components" easily pays for itself with even one instance of reuse.

### My Sinister Purpose

The reason I've been picking away at this codebase isn't idle fancy<a name="note-Wed-Apr-20-111010EDT-2011"></a>[|5|](#foot-Wed-Apr-20-111010EDT-2011), or an intense hatred of Sami Salkosuo<a name="note-Wed-Apr-20-111105EDT-2011"></a>[|6|](#foot-Wed-Apr-20-111105EDT-2011). It's that I've been putting together a little web game based on it. It's still not done, mind you, but I'm going to try to put something together fairly soon for you to poke at (even if it's ugly as sin from the visual perspective to start with). If nothing else, I'm tossing the ported Common Lisp codebase up onto [my GitHub](https://github.com/Inaimathi) this weekend so that some other pedantic bore can pick apart a project I was just doing in my spare time. After two articles full of bitching about poor style and inelegant expression, it seems like it's only fair.


* * *
##### Footnotes

1 - <a name="foot-Wed-Apr-20-105412EDT-2011"></a>[|back|](#note-Wed-Apr-20-105412EDT-2011) - Especially considering the situation.
2 - <a name="foot-Wed-Apr-20-105710EDT-2011"></a>[|back|](#note-Wed-Apr-20-105710EDT-2011) - Which weren't particularly horrible, most of them were upholding the time-honored tradition of testing by `printf`
3 - <a name="foot-Wed-Apr-20-110208EDT-2011"></a>[|back|](#note-Wed-Apr-20-110208EDT-2011) - It's not enforced except in one or two places, but it is a style convention. SBCL **will** bitch at you if you try to do something like `(let ((*foo* '(bar baz))) ...)`, for example. Not refuse to run it of course, but it'll warn you that it's using a lexical binding for `*foo*`.
4 - <a name="foot-Wed-Apr-20-110844EDT-2011"></a>[|back|](#note-Wed-Apr-20-110844EDT-2011) - That's the first time that my blog-mode highligher chugged for a second before coming back with the result, by the by, take a look at the source code and see if you can see why.
5 - <a name="foot-Wed-Apr-20-111010EDT-2011"></a>[|back|](#note-Wed-Apr-20-111010EDT-2011) - Or at least, not **just** idle fancy.
6 - <a name="foot-Wed-Apr-20-111105EDT-2011"></a>[|back|](#note-Wed-Apr-20-111105EDT-2011) - Which I don't have. I've never met the guy, and he couldn't possibly have known that I'd be going through his hobby-horse with a sledgehammer at some point in the distant future of 2011.
