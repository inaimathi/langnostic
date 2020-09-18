Ok, so I guess I'm doing this.

In hopes of participating in the [Autumn Lisp 2020 Game Jam](https://itch.io/jam/autumn-lisp-game-jam-2020), I'm going to write a multiplayer game. It's going to deal with players in several ways, implement 1FA, and probably end up being asymmetric and heavily infulenced by some readings that The Cabal have been doing lately.

But don't worry about that for the moment.

## Piece by piece
### The basics

```
(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)
```
I'm using [`clj`](TODO). You can find it on [my github](TODO), and it'll be included as part of the `asd` file.

Ahem.

Prisoners can do two things. They can `cooperate` or they can `defect`.

```
(defun coop? (res) (eq :cooperate res))
(defun defe? (res) (eq :defect res))
```

In order to play a game, you take the `game` function and `apply` it to the ordered list of `prisoners` that will be playing.

```
(defun play! (game &rest players)
  (apply game players))
```

A two-player, one-time game looks like this:

1. We take two `prisoner`s
2. We ask them to either `cooperate` or `defect`
3. We tell each of them what the other did
4. We score them

To start with, we're going with a payoff matrix that looks like

```
          | Cooperate | Defect
------------------------------
Cooperate | 3, 3      | 1, 5
------------------------------
   Defect | 5, 1      | 0, 0
------------------------------
```

We might play with this later, but lets pretend we won't have the time.

```
(defun one-time (player-a player-b)
  (let ((a (funcall (lookup player-a :strategy)))
	(b (funcall (lookup player-b :strategy))))
    (if-let (update (lookup player-a :update))
      (funcall update b))
    (if-let (update (lookup player-b :update))
      (funcall update a))
    (cond ((and (coop? a) (coop? b))
	   (list 3 3))
	  ((and (coop? a) (defe? b))
	   (list 1 5))
	  ((and (defe? a) (coop? b))
	   (list 5 1))
	  (t
	   (list 0 0)))))
```

The two simplest possible prisoners we can have are one who always `:cooperate`s, and one who always `:defect`s. A `prisoner` needs to be able to take into account what their opponent did last time, and separately, do something.

```
(defun defector ()
  {:name :defector :strategy (lambda () :defect)})

(defun cooperator ()
  {:name :cooperator :strategy (lambda () :cooperate)})
```

We can now play. Would you like to play a game?

### The Simplest Game

![](TODO - saw pic)

```
THE-PRISONERS> (play! #'one-time (defector) (cooperator))
(5 1)
THE-PRISONERS> (play! #'one-time (cooperator) (defector))
(1 5)
THE-PRISONERS> (play! #'one-time (cooperator) (cooperator))
(3 3)
THE-PRISONERS> (play! #'one-time (defector) (defector))
(0 0)
THE-PRISONERS>
```

There are other, simple kinds of prisoners. One is the prisoner who tosses a coin and does what it tells them to.

```
(defun gambler ()
  {:name :gambler :strategy (lambda () (nth (random 2) (list :cooperate :defect)))})
```

The more general case doesn't necessarily flip a coin, but can weigh either `:cooperate` or `:defect` more strongly.

```
(defun gambler (&key (cooperate 1) (defect 1))
  (let ((total (+ cooperate defect))
	(moves (concatenate
		'list
		(loop repeat cooperate collect :cooperate)
		(loop repeat defect collect :defect))))
    {:name (intern (format nil "GAMBLER~a/~a" cooperate defect) :keyword)
	   :strategy (lambda () (nth (random total) moves))}))
```

This way, we can get a true coin-flipper.

```
THE-PRISONERS> (gambler)
{:NAME :GAMBLER1/1 :STRATEGY #<CLOSURE (LAMBDA () :IN GAMBLER) {1003B5824B}>}
THE-PRISONERS>
```

Or someone who mostly cooperates/defects, but sometimes defects/cooperates.

```
THE-PRISONERS> (gambler :cooperate 5)
{:NAME :GAMBLER5/1 :STRATEGY #<CLOSURE (LAMBDA () :IN GAMBLER) {1003B69F0B}>}
THE-PRISONERS> (gambler :defect 5)
{:NAME :GAMBLER1/5 :STRATEGY #<CLOSURE (LAMBDA () :IN GAMBLER) {1003B6C38B}>}
THE-PRISONERS>
```

How do they play against each of the others? Lets find out.

### The Second Simplest Game

```
(defun matches (elems &key (mirror? t))
  (loop for (a . rest) on elems while rest
      if mirror? collect (cons a a)
      append (loop for b in rest collect (cons a b))))

(defun all-against-all! (game matches)
  (reduce
   (lambda (memo res)
     (merge-by #'+ memo res))
   (loop for (a . b) in matches
      collect (let ((res (play! game a b)))
		{(lookup a :name) (first res) (lookup b :name) (second res)}))))
```

This lets us see who does better against everyone.

```
THE-PRISONERS> (all-against-all! #'one-time (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5))))
{:GAMBLER1/5 13 :GAMBLER1/1 9 :GAMBLER5/1 8 :DEFECTOR 10 :COOPERATOR 8}
THE-PRISONERS> (all-against-all! #'one-time (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5))))
{:GAMBLER1/5 8 :GAMBLER1/1 7 :GAMBLER5/1 8 :DEFECTOR 15 :COOPERATOR 10}
THE-PRISONERS> (all-against-all! #'one-time (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5))))
{:GAMBLER1/5 10 :GAMBLER1/1 7 :GAMBLER5/1 8 :DEFECTOR 15 :COOPERATOR 8}
THE-PRISONERS> (all-against-all! #'one-time (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5))))
{:GAMBLER1/5 11 :GAMBLER1/1 10 :GAMBLER5/1 11 :DEFECTOR 10 :COOPERATOR 6}
THE-PRISONERS>
```

The `defector` comes out on top here. And the mostly-defecting `gambler` doesn't do bad either. Of course, this is what we would expect from the `one-time` game.

An `iterated` game is like a series of `one-time` games, and it keeps a running total of the score.

```
(defun iterated (&key (iterations 10))
  (lambda (player-a player-b)
    (loop repeat iterations
       for (a b) = (one-time player-a player-b)
       sum a into a-sum sum b into b-sum
       finally (return (list a-sum b-sum)))))
```

It plays about how you'd expect

```
THE-PRISONERS> (play! (iterated) (defector) (cooperator))
(50 10)
THE-PRISONERS> (play! (iterated) (cooperator) (cooperator))
(30 30)
THE-PRISONERS> (play! (iterated) (defector) (defector))
(0 0)
THE-PRISONERS>
```

And setting the world at its' own throat works the way you'd expect of this process so far.

```
THE-PRISONERS> (all-against-all! (iterated) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5))))
{:GAMBLER1/5 119 :GAMBLER1/1 117 :GAMBLER5/1 105 :DEFECTOR 135 :COOPERATOR 100}
THE-PRISONERS> (all-against-all! (iterated) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5))))
{:GAMBLER1/5 132 :GAMBLER1/1 109 :GAMBLER5/1 103 :DEFECTOR 120 :COOPERATOR 100}
THE-PRISONERS> (all-against-all! (iterated) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5))))
{:GAMBLER1/5 100 :GAMBLER1/1 124 :GAMBLER5/1 92 :DEFECTOR 130 :COOPERATOR 96}
THE-PRISONERS>
```

There are more elaborate strategies we can call upon. I won't implement them all here, but these have [been thought of](https://plato.stanford.edu/entries/prisoner-dilemma/strategy-table.html).

### Thoughtful Players

Robin alternates between cooperating and defecting.

```
(defun robin ()
  (let ((prev :cooperate))
    {:name :robin
	   :strategy (lambda ()
		       (if (coop? prev)
			   (setf prev :defect)
			   (setf prev :cooperate)))}))
```

And then, there are the simplest strategies that consider their opponent.

```
(defun polo ()
  (let ((prev nil))
    {:name :polo
	   :update (lambda (opponent-action) (setf prev opponent-action))
	   :strategy (lambda () (or prev :cooperate))}))

(defun dantes ()
  (let ((plan :cooperate))
    {:name :dantes
	   :update (lambda (action) (when (defe? action) (setf plan :defect)))
	   :strategy (lambda () plan)}))
```

With the addition of these, it's no longer obviously a `defector`s game.


```
THE-PRISONERS> (all-against-all! (iterated) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 164 :DANTES 131 :GAMBLER1/1 150 :GAMBLER5/1 169 :DEFECTOR 150 :COOPERATOR 184 :POLO 120 :ROBIN 147}
THE-PRISONERS> (all-against-all! (iterated) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 168 :DANTES 126 :GAMBLER1/1 176 :GAMBLER5/1 159 :DEFECTOR 165 :COOPERATOR 184 :POLO 129 :ROBIN 136}
THE-PRISONERS> (all-against-all! (iterated) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 158 :DANTES 121 :GAMBLER1/1 154 :GAMBLER5/1 156 :DEFECTOR 150 :COOPERATOR 184 :POLO 123 :ROBIN 154}
THE-PRISONERS> (all-against-all! (iterated) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 163 :DANTES 131 :GAMBLER1/1 163 :GAMBLER5/1 161 :DEFECTOR 175 :COOPERATOR 184 :POLO 117 :ROBIN 146}
THE-PRISONERS> (all-against-all! (iterated :iterations 50) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 789 :DANTES 656 :GAMBLER1/1 940 :GAMBLER5/1 964 :DEFECTOR 720 :COOPERATOR 1056 :POLO 585 :ROBIN 752}
THE-PRISONERS> (all-against-all! (iterated :iterations 50) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 845 :DANTES 651 :GAMBLER1/1 892 :GAMBLER5/1 959 :DEFECTOR 775 :COOPERATOR 1054 :POLO 609 :ROBIN 719}
THE-PRISONERS> (all-against-all! (iterated :iterations 50) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 788 :DANTES 651 :GAMBLER1/1 929 :GAMBLER5/1 946 :DEFECTOR 775 :COOPERATOR 1044 :POLO 609 :ROBIN 744}
THE-PRISONERS> (all-against-all! (iterated :iterations 50) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 859 :DANTES 651 :GAMBLER1/1 867 :GAMBLER5/1 952 :DEFECTOR 765 :COOPERATOR 1048 :POLO 609 :ROBIN 729}
THE-PRISONERS> (all-against-all! (iterated :iterations 50) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 833 :DANTES 666 :GAMBLER1/1 920 :GAMBLER5/1 953 :DEFECTOR 775 :COOPERATOR 1046 :POLO 603 :ROBIN 720}
THE-PRISONERS> (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 8325 :DANTES 6436 :GAMBLER1/1 9255 :GAMBLER5/1 9544 :DEFECTOR 7565 :COOPERATOR 10508 :POLO 8976 :ROBIN 7383}
THE-PRISONERS> (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 8365 :DANTES 6531 :GAMBLER1/1 9289 :GAMBLER5/1 9531 :DEFECTOR 7645 :COOPERATOR 10486 :POLO 6018 :ROBIN 7379}
THE-PRISONERS> (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 8407 :DANTES 6546 :GAMBLER1/1 9139 :GAMBLER5/1 9574 :DEFECTOR 7590 :COOPERATOR 10554 :POLO 6117 :ROBIN 7389}
THE-PRISONERS> (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 8063 :DANTES 6371 :GAMBLER1/1 9231 :GAMBLER5/1 9492 :DEFECTOR 7555 :COOPERATOR 10508 :POLO 6084 :ROBIN 7412}
THE-PRISONERS> (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 8068 :DANTES 6456 :GAMBLER1/1 9165 :GAMBLER5/1 9614 :DEFECTOR 7395 :COOPERATOR 10516 :POLO 6003 :ROBIN 7451}
THE-PRISONERS> (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))
{:GAMBLER1/5 8241 :DANTES 6356 :GAMBLER1/1 9150 :GAMBLER5/1 9579 :DEFECTOR 7545 :COOPERATOR 10480 :POLO 9021 :ROBIN 7392}
THE-PRISONERS>
```

When it's a prisoner against the world, the makeup of the world makes a difference in which prisoner ultimately wins.

```
(defun winner (results)
  (let ((max nil)
	(score nil))
    (loop for (k . v) in (as-list results)
       do (if (or (not score) (> v score))
	      (setf score v
		    max (cons k v))))
    max))
```

Currently, with mirror matches happening, the world is tilted towards `cooperator`s.

```
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)))))
(:COOPERATOR . 10554)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)))))
(:COOPERATOR . 10532)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)))))
(:COOPERATOR . 10486)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)))))
(:COOPERATOR . 10536)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)))))
(:COOPERATOR . 10478)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)))))
(:COOPERATOR . 10502)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)))))
(:COOPERATOR . 10540)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)))))
(:COOPERATOR . 10516)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)))))
(:COOPERATOR . 10476)
THE-PRISONERS>
```

Without mirror matches, it's still mostly a `cooperator`s' game, but not quite so strongly.

```
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)) :mirror? nil)))

(:DEFECTOR . 7665)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)) :mirror? nil)))
(:ROBIN . 7497)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)) :mirror? nil)))
(:COOPERATOR . 7512)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)) :mirror? nil)))
(:COOPERATOR . 7580)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)) :mirror? nil)))
(:COOPERATOR . 7516)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)) :mirror? nil)))
(:COOPERATOR . 7528)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)) :mirror? nil)))
(:DEFECTOR . 7615)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)) :mirror? nil)))
(:DEFECTOR . 7610)
THE-PRISONERS> (winner (all-against-all! (iterated :iterations 500) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin)) :mirror? nil)))
(:COOPERATOR . 7550)
THE-PRISONERS>
```

This wasn't the end. It was step one.
