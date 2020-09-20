Dawn of the second day.

According to the internet, the thing I intend to build is [called a Roguelikelike](https://bp.io/howroguelike/), teetering on the very edge of being a Roguelike. So it goes; we'll see if I end up taking the title or not.

Last time, we laid out the basics of `prisoner`s, their interactions and their strategies. This time, lets get some different scenarios and some player interaction going.

## Scenarios

Payoff matrices involve deciding who gets what bonus or penalty as a result of an interaction. Given a pair of `defect`/`cooperate` choices, a `payoff-matrix` will return the scores to be delivered to each player in turn.

```
(defun payoff-matrix (cc-a cc-b cd-a cd-b dc-a dc-b dd-a dd-b)
  (let ((tbl {(cons :cooperate :cooperate) (list cc-a cc-b)
	      (cons :defect :cooperate) (list dc-a dc-b)
	      (cons :cooperate :defect) (list cd-a cd-b)
	      (cons :defect :defect) (list dd-a dd-b)}))
    (lambda (a b) (lookup tbl (cons a b)))))
```

Now we can define some basic scenarios. A `dilemma` is the name I'll pick for the situation where co-operating is better for the group, and both defecting is the worst thing for everyone, but a single defector will end out better off _by_ defecting.

```
(defparameter dilemma
  (payoff-matrix
   3 3  1 5
   5 1  0 0))
```

A `stag-hunt` is a situation where a pair of players can pool their resources for a greater prize, and ignore each other for the lesser. If either player attempts to hunt the stag alone, they get nothing, while their defecting partner still gets a rabbit.

```
(defparameter stag-hunt
  (payoff-matrix
   3 3  0 1
   1 0  1 1))
```

A `trade` is one in which both parties benefit, but to which both parties must agree.

```
(defparameter trade
  (payoff-matrix
   3 3  0 0
   0 0  0 0))
```

A `theft` is one where a player takes from the other. But if both players cooperate, or both try to rob each other, they come to an impasse.

```
(defparameter theft
  (payoff-matrix
   0 0   -3 3
   3 -3   0 0))
```

A `trap` is a situation where cooperating leads to disaster, ignoring the situation leads to no gain, and `defect`ing to make it clear to your partner that you don't intend to follow ends up benefiting both players.

```
(defparameter trap
  (payoff-matrix
   -3 -3  2 2
    2  2  0 0))
```

The last scenario I'll concern myself with is the `mutual-prediction`. Where guessing what your partner/opponent will choose benefits you, and failing to do so does nothing.

```
(defparameter mutual-prediction
  (payoff-matrix
   3 3  0 0
   0 0  3 3))
```

## Adventure

In order to move through the world, our `prisoner`s need a world to move through. Let us begin at the ending.

```
(defparameter ending
  {:description "You have come to the end of your long, perilous journey."})
```

There is nothing to do at the end other than display this fact.

```
(defun repl! (adventure)
  (format t "~%~%~a~%~%" (lookup adventure :description)))
```

```
THE-PRISONERS> (repl! ending)


You have come to the end of your long, perilous journey.

NIL
THE-PRISONERS>
```

But what led us here was a choice. An adventure is more than a description, it's also the options, a `prisoner`, the `scenario`, and a way to `continue` the action. `continue`ing means making a choice and effectively playing the opposing/cooperating `prisoner` and abiding by the results.

```
(defun mk-adventure ()
  (let ((prisoner (polo)))
    {:description
     "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""
     :cooperate "accept" :defect "refuse" :prisoner prisoner :scenario trade
     :continue (lambda (choice)
		 (let ((their-choice (play prisoner)))
		   (update! prisoner choice)
		   (funcall trade choice their-choice)
		   ending))}))
```

This sort of adventure also takes a bit more machinery to run from the `repl`. We need to present the `description`, but also get an appropriate choice from the user. Getting that choice is a bit more complicated than you might think at first.

```
(defun get-by-prefix (lst prefix)
  (let ((l (length prefix)))
    (loop for elem in lst
       when (and (>= (length elem) l)
		 (== (subseq elem 0 l) prefix))
       do (return elem))))

(defun get-repl-choice (adventure)
  (let* ((responses (mapcar #'string-downcase (list (lookup adventure :cooperate) (lookup adventure :defect))))
	 (r-map {(string-downcase (lookup adventure :cooperate)) :cooperate
		 (string-downcase (lookup adventure :defect)) :defect})
	 (by-pref nil)
	 (resp ""))
    (loop until (and (symbolp resp)
		     (setf by-pref
			   (get-by-prefix
			    responses
			    (string-downcase (symbol-name resp)))))
       do (format
	   t "~a/~a:"
	   (lookup adventure :cooperate)
	   (lookup adventure :defect))
       do (setf resp (read)))
    (lookup r-map by-pref)))
```

Well behaved players are easy to deal with, true...

```
THE-PRISONERS> (get-repl-choice (mk-adventure))
Accept/Refuse:acc

:COOPERATE
T
THE-PRISONERS> (get-repl-choice (mk-adventure))
Accept/Refuse:ref

:DEFECT
T
THE-PRISONERS> (get-repl-choice (mk-adventure))
Accept/Refuse:a

:COOPERATE
T
```

... but we want to be a bit more general than that.

```
THE-PRISONERS> (get-repl-choice (mk-adventure))
Accept/Refuse:fuck you
Accept/Refuse:Accept/Refuse:boo
Accept/Refuse: (error 'error)
Accept/Refuse: (quit)
Accept/Refuse:r

:DEFECT
T
THE-PRISONERS>
```

That's the only hard par though. Interacting with the game once we're sure we have valid input from our player is relatively simple.

```
(defun repl! (adventure)
  (format t "~%~%~a~%~%" (lookup adventure :description))
  (when (contains? adventure :continue)
    (let ((choice (get-repl-choice adventure)))
      (repl! (funcall (lookup adventure :continue) choice)))))
```

```
THE-PRISONERS> (repl! (mk-adventure))


A stranger approaches. "I see you have baubles. Would you like to trade, that we both may enrich ourselves?"

Accept/Refuse:acc


You have come to the end of your long, perilous journey.

NIL
THE-PRISONERS>
```

This is obviously not the perilous journey being spoken of. At least, not all of it. The simplest way to extend it into one is to wrap `scenario`s around our existing `adventure`.

```
(defun mk-adventure ()
  (let ((def (defector)))
    {:description "A muscled street thug approachs, knife drawn."
     :cooperate "surrender" :defect "run" :prisoner def :scenario theft
     :continue (lambda (choice)
		 (let ((their-choice (play def)))
		   (update! def choice)
		   (funcall theft choice their-choice))
		 (let ((prisoner (polo)))
		   {:description
		    "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""
		    :cooperate "accept" :defect "refuse" :prisoner prisoner :scenario trade
		    :continue (lambda (choice)
				(let ((their-choice (play prisoner)))
				  (update! prisoner choice)
				  (funcall trade choice their-choice)
				  ending))}))}))
```

```
THE-PRISONERS> (repl! (mk-adventure))


A muscled street thug approachs, knife drawn.

Surrender/Run:run


A stranger approaches. "I see you have baubles. Would you like to trade, that we both may enrich ourselves?"

Accept/Refuse:acc


You have come to the end of your long, perilous journey.

NIL
THE-PRISONERS>
```

Of course, since we want it to be much longer and more perilous, we'll want that process automated to at least some degree.

```
(defun wrap-scenario (adventure scenario)
  (insert
   scenario
   (cons
    :continue
    (lambda (choice)
      (let* ((them (lookup scenario :prisoner))
	     (their-choice (play them)))
	(update! them choice)
	(funcall (lookup scenario :scenario) choice their-choice)
	adventure)))))

(defun mk-adventure ()
  (wrap-scenario
   (wrap-scenario
    ending
    {:description
     "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""
     :cooperate "accept" :defect "refuse" :prisoner (polo) :scenario trade})
   {:description
    "A muscled street thug approachs, knife drawn. \"Yer money or yer life, fop!\""
    :cooperate "surrender" :defect "run" :prisoner (defector) :scenario theft}))
```

This isn't enough for the Roguelikelike title, and I don't think I'll get there today, but I do want the ability to make an arbitrarily long adventure. The dumbest way of doing this is to make a list of scenarios, and pick from them when the need arises.

```
(defun random-scenario ()
  (pick
   (list
    {:description
     "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""
     :cooperate "accept" :defect "refuse" :prisoner (polo) :scenario trade}
    {:description
     "A muscled street thug approachs, knife drawn. \"Yer money or yer life, fop!\""
     :cooperate "surrender" :defect "run" :prisoner (defector) :scenario theft})))


(defun mk-adventure (&key (scenarios 5))
  (let ((adventure ending))
    (loop repeat scenarios
       do (setf adventure (wrap-scenario adventure (random-scenario))))
    adventure))
```

An adventure of even 5 scenarios will end up being repetitive since we currently only have a grand total of two. But we can do something about that...

```
(defun random-scenario ()
  (pick
   (list
    {:description
     "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""
     :cooperate "accept" :defect "refuse" :prisoner (polo) :scenario trade}
    {:description
     "A muscled street thug approachs, knife drawn. \"Yer money or yer life, fop!\""
     :cooperate "surrender" :defect "run" :prisoner (defector) :scenario theft}
    {:description
     "As you walk through an expansive market square, a gambler motions you over. \"Fancy your chances at evens or odds?"
     :cooperate "Evens!" :defect "Odds!" :prisoner (gambler) :scenario mutual-prediction}
    {:description
     "A hunter approaches you in a forest clearing. \"Hallo there, young one. Would you help me hunt a deer? I've had enough hares for now, but I promise we'll eat well if we work together!\""
     :cooperate "<Nocks bow>" :defect "Rather go my own way" :prisoner (dantes) :scenario stag-hunt}
    {:description
     "\"Hey follow me into this bear trap!\""
     :cooperate "Sure; I've grown tired of living" :defect "No. No, I'd rather not."
     :prisoner (robin) :scenario trap}
    {:description
     "You see a merchant ahead of you, paying little attention to his overfull coin purse. You could cut it and run."
     :cooperate "It's too tempting" :defect "No; I hold strong"
     :prisoner (dantes) :scenario theft}
    {:description
     "At the end of your travails with your co-conspirator, you get to the treasure first and can pocket some if you want."
     :cooperate "Take it" :defect "No, we split fairly"
     :prisoner (gambler :defect 5) :scenario dilemma})))
```

This gives me some ideas about how to go about generating scenarios a lot more programmatically, but I'll leave that for later, when I'm in the right frame of mind to do cosmetic improvements.

![Wanna play a game?](/static/img/the-prisoners/wanna-play-a-game.jpg)

```
THE-PRISONERS> (repl! (mk-adventure))

At the end of your travails with your co-conspirator, you get to the treasure first and can pocket some if you want.

Take it/Split fairly:split


You see a merchant ahead of you, paying little attention to his overfull coin purse. You could cut it and run.

It's too tempting/No:it's


"Hey follow me into this bear trap!"

Sure; I've grown tired of living/No. No, I'd rather not.:no


You see a merchant ahead of you, paying little attention to his overfull coin purse. You could cut it and run.

It's too tempting/No:it's


A stranger approaches. "I see you have baubles. Would you like to trade, that we both may enrich ourselves?"

accept/refuse:accept


You have come to the end of your long, perilous journey.

NIL
THE-PRISONERS>
```

This is about as far as I'm going today, and I'm not entirely sure how far I'm going during my next session.

As always, I'll let you know.
