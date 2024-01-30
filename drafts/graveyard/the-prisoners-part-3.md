So I've done a bunch of background housekeeping tasks on this project. Completely apart from any work on the game, the [`clj`](TODO) library is being forced to make a bunch of progress (as well as a few signature changes) as a result of my carving `the-prisoners` out thoroughly enough. I still haven't shorn up the test suite for all the new functions, but I'm leaving that as a definite `TODO` for now rather than losing any momentum I had.

## Firstly

We now have a posted [github repo](https://github.com/inaimathi/the-prisoners) and [itch.io page](https://inaimathi.itch.io/the-prisoners). The repo was definitely going to happen, but it seems like I'll want to figure out a way of [cross-compiling CL executables](https://recursive.games/posts/Building-a-Cross-Platform-Lisp-Binary.html) before the end of this jam. I can't right now, because Azure Pipelines is down. This is absolutely preventing me from recommending it for professional work any time in the future, but for my purposes it might just be Good Enough™. Even if it doesn't come back up in the next few days, I guess a decent alternative is to talk to some friends with Windows/OS X access and talk them through giving me a build.

## Secondly

There's a bit of preamble and bugfixing in the project, including the actual possibility of death. Which puts us one step closer to the end goal of Roguelike rather than Roguelikelike. I'm very pointedly not discussing future plans for this project other than [that one extremely vague day-1 comment](/posts/the-prisoners-part-1). These journals are going to be as stream-of-consciousness as the code itself.

## Thirdly

We have a build system that is verified working on 64-bit linux. The extremely short version is

```
# build.sh
sbcl --eval '(ql:quickload :the-prisoners)' --eval '(the-prisoners:build!)' --quit
```
```
; the-prisoners.lisp

...

(defun main ()
  (repl! (mk-adventure)))

(defun build! ()
  (sb-ext:save-lisp-and-die
   (or
    #+win32#p"the-prisoners.exe"
    #+darwin#p"the-prisoners.bin"
    #+linux#p"the-prisoners"
    (error "Unsupported build platform. Got ~A" *features*))
   :toplevel #'main :executable t))
```

_In theory_, this will work unmodified on Windows and OS X. But theories, especially ones related to cross-platform compatibility, are treacherous and pernicious things not to be trusted by any sane programmer.


## Fourthly

Lets re-purpose some principles from an [older project](https://github.com/inaimathi/cl-leet/blob/master/data.lisp) to get a bit of variation in the prose of our encounters. We're going to eventually make this a lot more inter-related and relevant, but for now, just separating out the process of generating prose from the process of setting up properly structured encounters might be a win.

The result text of various things is going to be generated on the basis of three things.

1. The `scenario` in play
2. The `prisoner`(s?) opposing the player
3. The `player`s' current state

Additionally, the aftermath of a scenario will be influenced by the `choice`s of the `player` and `prisoner`. So, the absolute basics here involve breaking up what we had over a couple dispatch tables.

```
(defun !!death (player prisoner scenario)
  (declare (ignore player prisoner scenario))
  "You bleed out on the cobblestones of the city market.
Your hopes of fortune and glory fading along with sensation.
As a consolation, you have been freed.")

(defun !!scenario-description (player prisoner scenario)
  (declare (ignore player))
  (lookup
   {trade
    "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""

    theft
    (lookup
     {:defector "A muscled street thug approachs, knife drawn. \"Yer money or yer life, fop!\""}
     (lookup prisoner :name)
     :default "You see a merchant ahead of you, paying little attention to his overfull coin purse. You could cut it and run.")

    mutual-prediction
    "As you walk through an expansive market square, a gambler motions you over. \"Fancy your chances at evens or odds?"

    stag-hunt
    "A hunter approaches you in a forest clearing. \"Hallo there, young one. Would you help me hunt a deer? I've had enough hares for now, but I promise we'll eat well if we work together!\""

    trap
    "\"Hey follow me into this bear trap!\""

    dilemma
    "At the end of your travails with your co-conspirator, you get to the treasure first and can pocket some if you want."}
   scenario))

(defun !!scenario-choices (player prisoner scenario)
  (declare (ignore player))
  (lookup
   {trade
    {:cooperate "Accept" :defect "Refuse"}

    theft
    (lookup
     {:defector {:cooperate "Surrender" :defect "Run"}}
     (lookup prisoner :name)
     :default {:cooperate "It's too tempting" :defect "No"})

    mutual-prediction
    {:cooperate "Evens!" :defect "Odds!"}

    stag-hunt
    {:cooperate "<Nocks bow>" :defect "Rather go my own way"}

    trap
    {:cooperate "Sure; I've grown tired of living" :defect "No. No, I'd rather not."}

    dilemma
    {:cooperate  "Split fairly" :defect "Take it"}}
   scenario))
```

Again, no more featureful than before, but our `encounter` generator now gets to be a tiny bit more efficient...

```
(defun random-encounter ()
  (let ((scenario (pick (list trade theft mutual-prediction stag-hunt trap dilemma)))
	(prisoner (pick (list (polo) (defector) (gambler) (dantes) (robin)))))
    (insert
     (!!scenario-choices {} prisoner scenario)
     :description (!!scenario-description {} prisoner scenario)
     :prisoner prisoner :scenario scenario)))
```

..._and_ we get to separate `prose` generation into a separate (soon to be massively expanded) module. Part of that is probably going to be changing up how adventure generation works, because some of these descriptions should probably be generated at the point where the player gets to the appropriate encounter rather than being pre-ordained. I'm _guessing_ that the next stage of this process is going to involve me carving out the `prose` file into an entire sub-tree focusing on generating the text required for the rest of this adventure. I'm _not_ sure that it's going to be necessary, because of where I expect to take this game, but we'll see.

## Fifthly

We need state. This was never going to be an in-memory-no-disk game, and I've gotten to the point where I need to store things on disk. My usual go-to for this sort of thing in toy CL projects is [`fact-base`](https://github.com/inaimathi/fact-base). Not because it's awesome or featureful or anything, but because

1. I favor self-contained, native libraries over ones with external/FFI dependencies.
2. I'm not sure where we're going with this yet, so I want the most versatily storage utility I can find
3. And finally, as a result of having written it, I know how to use it

We'll store any `the-prisoners`-related disk detritus in the user home directory, and we're starting by storing `history.base`.

```
(defparameter *dir* (merge-pathnames (user-homedir-pathname) ".the-prisoners"))

(defparameter *base* (fact-base:base! (merge-pathnames *dir* "history.base")))
```

We also need to change `main` so that it sets these variables as appropriate on start-up, because otherwise this would only ever work on my machine.

```
(defun main ()
  (setf *dir* (merge-pathnames (user-homedir-pathname) ".the-prisoners")
	*base* (fact-base:base! (merge-pathnames *dir* "history.base")))
  (repl! (mk-adventure)))
```

Now, we need a way to get a `map` out of a series of `fact`s, because this is ultimately what we'll want to manipulate for display purposes.

```
(defun facts->map (res)
  (format t "~s~%" res)
  (let ((id (caar res)))
    (insert
     (reduce
      (lambda (memo fact)
	(if (== id (first fact))
	    (insert memo (second fact) (third fact))
	    memo))
      res :initial-value {})
     :id id)))
```

The only thing we'll be storing for the moment will be prisoners. Not sure where we're going in the long term, but for now...

```
(defun store-prisoner! (prisoner &key (strategy :manual) (source :local))
  (let ((strategy (lookup prisoner :strategy :default strategy)))
    (fact-base:multi-insert!
     *base*
     (insert
      (loop for (k . v) in (as-list (dissoc prisoner :strategy))
	 collect (list k v))
      (list :strategy strategy)
      (list :source source)))))

(defun prisoner-by (&key id strategy source)
  (if id
      (list
       (facts->map
	(fact-base:for-all `(,id ?k ?v) :in *base* :collect (list id ?k ?v))))
      (mapcan
       (fn (id) (prisoner-by :id id))
       (fact-base:for-all
	   `(,@(cond (strategy `(?id :strategy ,strategy))
		     (source `(?id :source ,source))))
	 :in *base* :collect ?id))))
```

we'll need a way of storing and retrieving `prisoner`s. And because of the stats we'll be tracking, also a way of bumping numeric stats.

```
(defun prisoner-incf! (prisoner key &key (by 1))
  (let ((id (lookup prisoner :id)))
    (if-let (old (first (fact-base:lookup *base* :a id :b key)))
      (let ((new (+ (third old) by)))

	(fact-base:change! *base* old (list id key new))
	new))))
```

Now that we've got that up and running, we can change our `repl` system to check for existing `prisoner`s before prompting for a new one.

```
(defun play-scenario! (player adventure)
  (format t "~%==============================~%")
  (format t "~a, score: ~a~%~%" (lookup player :name) (lookup player :score))
  (format t "~a~%~%" (lookup adventure :description))
  (cond ((contains? adventure :continue)
	 (let* ((scenario (lookup adventure :scenario))
		(prisoner (lookup adventure :prisoner))
		(choice (get-repl-choice adventure))
		(their-choice (play prisoner))
		(next (continue! adventure choice their-choice))
		(score (if-let (s (lookup next :score)) (prisoner-incf! player :score :by s))))
	   (prisoner-incf! player :encounters)
	   (format t "~a~%" (!!scenario-aftermath player prisoner scenario choice their-choice))
	   (play-scenario!
	    (insert player :score score)
	    (if (> score 0)
		next
		{:description (!!death player prisoner scenario)}))))
	((== (lookup adventure :ending) :success)
	 (prisoner-incf! player :adventures)
	 nil)))

(defun to-adventure! (adventure)
  (format t "~%-==E- The Prisoners -Ǝ==-~%~%")
  (if-let (prisoner (first (prisoner-by :source :local)))
    (progn
      (format t "~%Welcome back, ~a. Prepare for your next adventure.~%~%" (lookup prisoner :name))
      (play-scenario! prisoner adventure))
    (progn
      (format t "~% What is your name?: ")
      (let* ((player-name (read-line))
	     (prisoner {:name player-name :score 1 :adventures 0 :encounters 0}))
	(store-prisoner! prisoner)
	(format t "~%Welcome, ~a~%~%" player-name)
	(format t "You may roam this world to your heart's content,~%but when you encounter your opposite number,~%know that you are the prisoners of your history.")
	(play-scenario! prisoner adventure)))))

(defun repl! ()
  (to-adventure! (mk-adventure)))
```

Say it with me now.

![Shall we play a game?](/static/img/the-prisoners/shall-we-play-a-game.jpg)

```
THE-PRISONERS> (repl!)

-==E- The Prisoners -Ǝ==-

Welcome back, inaimathi. Prepare for your next adventure.


==============================
inaimathi, score: 28

You see a merchant ahead of you, paying little attention to his overfull coin purse. You could cut it and run.

It's too tempting/No:no


==============================
inaimathi, score: 27

A hunter approaches you in a forest clearing. "Hallo there, young one. Would you help me hunt a deer? I've had enough hares for now, but I promise we'll eat well if we work together!"

<Nocks bow>/Rather go my own way:<Nock


==============================
inaimathi, score: 30

A hunter approaches you in a forest clearing. "Hallo there, young one. Would you help me hunt a deer? I've had enough hares for now, but I promise we'll eat well if we work together!"

<Nocks bow>/Rather go my own way:Rather


==============================
inaimathi, score: 31

As you walk through an expansive market square, a gambler motions you over. "Fancy your chances at evens or odds?

Evens!/Odds!:ev


==============================
inaimathi, score: 32

A stranger approaches. "I see you have baubles. Would you like to trade, that we both may enrich ourselves?"

Accept/Refuse:accept
You trade some of the useless items you are carrying for some of the strangers' valuable goods. You suspect they see it the same way.

==============================
inaimathi, score: 35

You have come to the end of your long, perilous journey.
```
