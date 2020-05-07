So recently, I had to use [`zipper`s](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf) at work. Specifically, the [Clojure implementation](https://clojuredocs.org/clojure.zip). There was some work I needed to do with arbitrary trees, involving some close-to-arbitrary transformations and it turned out that `zipper`s were more efficient than the alternatives[^although-admittedly-it].

[^although-admittedly-it]: Although admittedly, it does require me to explain the concept of `zipper`s to a few other people for maintenance purposes. So ironically, this _adds_ complexity despite being much more technically elegant than other options.

Using them this way, combined with the general state of the world and my free time, finally tipped me into doing some more Common Lisp development. Before, I go any further, let me be clear about something.

## I Like Clojure

Seriously.

Its logo is up top in the language bar, I was one of the inaugural members of the Toronto Clojure User Group, I [recommend it](/posts/recommendations) as a first lisp you should learn, and have for about six years now. I'm _also_ absolutely aware of the [shortcomings of Common Lisp](/posts/recommendations#why), and make no excuses for them.

However.

I don't like the JVM. It's slow as balls, its' deployment options are less than ideal for my purposes, and Clojure without it [is unlikely](https://old.reddit.com/r/Clojure/comments/6hhg1h/why_isnt_there_a_compiled_or_interpreted_clojure/diz006j/). Clojurescript build incompatiblities are, if anything, worse[^theres-absolutel-a-reason]. I don't like the underlying [licensing decisions](https://clojure.org/community/license).

[^theres-absolutel-a-reason]: There's a reason that [`langnostic.js`](/static/js/langnostic.js) is a raw JS file, rather than compiled from `clojurescript` source, and that reason is like 90% that the compilation process is nontrivial.

Whether or not I think you should learn Clojure as _your_ first[^note-that-i-say] lisp, it definitely wasn't _my_ first lisp. The more uniform, mostly-better-thought-out interface, lack of historical baggage and functional data structures are not enough to pull me all the way over.

[^note-that-i-say]: "First", not "only". You can probably make educated guesses about which other ones I think you should learn.

Especially since I can implement it in Lisp anyway.

## `clj`

[`clj`](https://github.com/inaimathi/clj) is basically the beginning of this. So far, it defines and exports exactly four symbols: `if-let`, `when-let`, `->` and `->>`. This is the minimal list of things that I miss from Clojure, realized I'd like sooner rather than later, and could implement almost trivially. I'm planning to add some more things once I brush up on [reader macros](https://letoverlambda.com/index.cl/guest/chap4.html).

## `cl-zipper`

Like I said, the thing that percipitated this thought was having used the `clojure` Zipper implementation. So, obviously, this is something I want next time I need to manipulate trees in Common Lisp. The paper is [here](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf), and unless you have a terminal phobia of datastructures[^in-which-case-why], you should totally read it. It's six pages, they're light, one of them is spent on the intro and references, and one more is spent on margins.

[^in-which-case-why]: In which case, why are you here? This blog could kill you accidentally with an errant click or two. You should probably just go do something else.

The operations defined in the paper are `left`, `right`, `up`, `down`, `insert_right`, `insert_left`, `insert_down` and `delete`. There's a few conveniences defined for the Clojure version, and I've implemented some of my own stuff too. There's also, oddly, a couple ways that the Clojure version and original paper conflict. I have some idea why that is in a couple places, but others seem arbitrary. We'll get to all of it. Lets go through [the main file](https://github.com/inaimathi/cl-zipper/blob/master/src/cl-zipper.lisp) in [almost-literate](http://inaimathi.ca/archive/by-tag/almost-literate-programming) style.

First up, we have constructors.

```lisp
(defstruct path
  (left) (path) (right))

(defstruct loc
  (node)
  (path)

  (fn-branch?)
  (fn-children)
  (fn-make-node))

;;;;;;;;;; Constructors
(defun zipper (branch? children make-node root)
  (make-loc
   :node root
   :fn-branch? branch? :fn-children children :fn-make-node make-node))

(defmethod make-zipper ((thing list))
  (zipper #'listp #'identity (lambda (node children) (declare (ignore node)) children) thing))

(defun make-node (zipper children)
  (funcall (loc-fn-make-node zipper) zipper children))
```

You can see influence from both [`clojure.zip`](https://github.com/clojure/clojure/blob/master/src/clj/clojure/zip.clj) and the paper here. I'm taking the lead from the paper by explicitly separating the `path` triple our from the `loc` definition. However, I'm not explicitly defining my own `type tree` the way that Huet does. Instead, I'm going to be dealing with assorted `lisp` trees. These could be implemented as `list`s, `vector`s, `hash`es, or a bunch of other formats. I'm going to implement a few type-distpatching build-ins, including the `make-zipper lisp` method above, but the basic `zipper` function just needs to take an interface as an argument in the form of `branch?`, `children` and `make-node` arguments. This is the same solution that the Clojure implementation went with, and I see no reason to go a different way. The only material difference is that theirs uses the Clojure [`metadata`](https://clojure.org/reference/metadata) system, while I explicitly define slots in the `loc` structure.

Now that we can construct, we need to be able to select.

```lisp
;;;;;;;;;; Selectors
(defun branch? (zipper) (funcall (loc-fn-branch? zipper) (loc-node zipper)))
(defun children (zipper)
  (funcall
   (loc-fn-children zipper)
   (loc-node zipper)))
(defun node (zipper) (loc-node zipper))
(defun path (zipper) (loc-path zipper))

(defun lefts (zipper)
  (when (loc-path zipper)
    (reverse (path-left (loc-path zipper)))))

(defun rights (zipper)
  (when (loc-path zipper)
    (path-right (loc-path zipper))))
```

The basic navigation is four functions; `down`, `up`, `left` and `right`

```lisp
;;;;;;;;;; Navigation
;;;;;;;;;;;;;;; Basic navigation
(defun down (zipper)
  (when (children zipper)
    (let ((fresh (copy-loc zipper)))
      (setf (loc-node fresh) (first (children zipper))
	    (loc-path fresh)
	    (make-path
	     :left nil
	     :path (loc-path zipper)
	     :right (rest (children zipper))))
      fresh)))

(defun up (zipper)
  (when (path zipper)
    (let ((fresh (copy-loc zipper)))
      (setf (loc-node fresh)
	    (make-node
	     zipper (append
		     (reverse (path-left (path zipper)))
		     (cons (loc-node zipper)
			   (path-right (path zipper)))))
	    (loc-path fresh) (path-path (path zipper)))
      fresh)))

(defun left (zipper)
  (when (and (path zipper) (path-left (path zipper)))
    (let ((fresh (copy-loc zipper)))
      (setf (loc-node fresh) (first (path-left (path zipper)))
	    (loc-path fresh)
	    (make-path
	     :left (rest (path-left (path zipper)))
	     :path (path-path (path zipper))
	     :right (cons (loc-node zipper) (path-right (path zipper)))))
      fresh)))

(defun right (zipper)
  (when (and (path zipper) (path-right (path zipper)))
    (let ((fresh (copy-loc zipper)))
      (setf (loc-node fresh) (first (path-right (path zipper)))
	    (loc-path fresh)
	    (make-path
	     :left (cons (loc-node zipper) (path-left (path zipper)))
	     :path (path-path (path zipper))
	     :right (rest (path-right (path zipper)))))
      fresh)))
```

- a handful of compound navigation primitives

```lisp
;;;;;;;;;;;;;;; Compound navigation
(defun root (zipper)
  (if-let (z (while zipper #'up))
    (node z)))

(defun leftmost (zipper) (while zipper #'left))

(defun rightmost (zipper) (while zipper #'right))
```

- a few basic modification functions

```lisp
;;;;;;;;;; Modification
(defun replace (zipper node)
  (let ((fresh (copy-loc zipper)))
    (setf (loc-node fresh) node)
    fresh))

(defun delete (zipper)
  (when (path zipper)
    (let ((fresh (copy-loc zipper))
	  (fresh-path (copy-path (loc-path zipper))))
      (cond ((rights zipper)
	     (setf (loc-node fresh) (pop (path-right fresh-path))
		   (loc-path fresh) fresh-path))
	    ((lefts zipper)
	     (setf (loc-node fresh) (pop (path-left fresh-path))
		   (loc-path fresh) fresh-path))
	    (t (setf (loc-path fresh) (path-path fresh-path))))
      fresh)))

(defun insert-child (zipper node)
  (replace
   zipper
   (make-node
    zipper
    (cond ((not (branch? zipper))
	   (list (node zipper) node))
	  ((children zipper)
	   (cons node (children zipper)))
	  (t (list node))))))

(defun append-child (zipper node)
  (replace
   zipper
   (make-node
    zipper
    (cond ((not (branch? zipper))
	   (list (node zipper) node))
	  ((children zipper)
	   (append (children zipper) (list node)))
	  (t (list node))))))

(defun insert-left (zipper node)
  (let ((fresh (copy-loc zipper))
	(fresh-path (copy-path (loc-path zipper))))
    (push node (path-left fresh-path))
    (setf (loc-path fresh) fresh-path)
    fresh))

(defun insert-right (zipper node)
  (let ((fresh (copy-loc zipper))
	(fresh-path (copy-path (loc-path zipper))))
    (push node (path-right fresh-path))
    (setf (loc-path fresh) fresh-path)
    fresh))
```

- append-child and insert-child is slightly more than the original paper does stuff, but I wanted the flexibility

- finally, some compound modification functions

```lisp
(defun edit (zipper f &rest args)
  (replace zipper (apply f (node zipper) args)))

(defun splice-left (zipper node-list)
  (reduce #'insert-left node-list :initial-value zipper))

(defun splice-right (zipper node-list)
  (reduce #'insert-right (reverse node-list) :initial-value zipper))
```

I haven't yet implemented `next`, `prev` and `remove` because these _might_ relate to the different representation of the [traversal `end?` state](https://github.com/clojure/clojure/blob/master/src/clj/clojure/zip.clj#L244). The reason for this _seems_ to be that `next`/`prev`/`remove` assume a [depth-first traversal](https://www.cs.usfca.edu/~galles/visualization/DFS.html). The reason I'm being weasely here is that I haven't thought about it hard enough to be sure that the `end?` marker is really necessary.
