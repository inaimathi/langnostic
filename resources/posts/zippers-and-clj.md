So recently, I had to use [`zipper`s](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf) at work. Specifically, the [Clojure implementation](https://clojuredocs.org/clojure.zip). There were some close-to-arbitrary transformations I needed to do with some close-to-arbitrary trees and it turned out that `zipper`s were more efficient than the alternatives[^although-admittedly-it].

[^although-admittedly-it]: Although admittedly, it does require me to explain the concept of `zipper`s to a few other people for maintenance purposes. So ironically, this _adds_ complexity despite being much more technically elegant than other options.

Using them this way, combined with the general state of the world and my free time, finally tipped me into doing some more Common Lisp development. Before, I go any further, let me be clear about something.

## I Like Clojure

Seriously.

Its logo is up top in the language bar, I was one of the inaugural members of the Toronto Clojure User Group, I [recommend it](/posts/recommendations) as a first lisp you should learn, and have for about six years now. I'm _also_ painfully aware of the [shortcomings of Common Lisp](/posts/recommendations#why), and make no excuses for them.

However.

- I don't like the JVM. It's slow as balls, its' deployment options are less than ideal for my purposes, its' error system is at best useless, and Clojure without it [is unlikely](https://old.reddit.com/r/Clojure/comments/6hhg1h/why_isnt_there_a_compiled_or_interpreted_clojure/diz006j/).
- Clojurescript build incompatiblities are, if anything, worse[^theres-absolutely-a-reason].
- I don't like the underlying [licensing decisions](https://clojure.org/community/license).

These are deep reasons to stay away. They're not the sort of thing I can paper over with a library or two. Fixing them would mean a superhuman amount of work poured into the underlying technical and social infrastructure, and I'm not into it. I wouldn't be into it even if the community was interested in heading that way, and near as I can tell, they're not particularly.

[^theres-absolutely-a-reason]: There's a reason that [`langnostic.js`](/static/js/langnostic.js) is a raw JS file, rather than compiled from `clojurescript` source, and that reason is like 90% that the compilation process is nontrivial to set up.

Whether or not I think _you_ should learn Clojure as _your_ first[^note-that-i-say] lisp, it definitely wasn't _my_ first lisp. The more uniform, mostly-better-thought-out interface, lack of historical baggage and functional data structures are not enough to pull me all the way over.

[^note-that-i-say]: "First", not "only". You can probably make educated guesses about which other ones I think you should learn.

It _is_ enough for me to start plotting a smash-and-grab of as much of the stuff I like as I can carry. Which is exactly what [`clj`](https://github.com/inaimathi/clj) represents. As of this writing, it defines and exports exactly four symbols: `if-let`, `when-let`, `->` and `->>`. This is a tiny beginning of the list, and I fully plan to put something more substantial together using [`cl-hamt`](https://quickref.common-lisp.net/cl-hamt.html), [`named-readtables`](https://common-lisp.net/project/named-readtables/#important_api_idiosyncrasies), [`test-utils`](https://github.com/inaimathi/test-utils) and possibly [`optima`](https://quickref.common-lisp.net/optima.html). Stay tuned to that repo if you're interested, but it's not the focus today.

## `cl-zipper`

The thing that percipitated this thought was having used the Clojure Zipper implementation. So, obviously, this is something I want next time I need to manipulate trees in Common Lisp. The paper is [here](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf), and unless you have a terminal phobia of datastructures[^in-which-case-why], you should go read it. It's six pages, they're light, and one of them taken up by the intro and references.

[^in-which-case-why]: In which case, why are you here? This blog could kill you accidentally with an errant click or two. You should probably just go do something else.

The operations defined in the paper are `left`, `right`, `up`, `down`, `insert_right`, `insert_left`, `insert_down` and `delete`. There's a few conveniences defined for the Clojure version, and I've implemented some of my own stuff too. Lets go through [the main file](https://github.com/inaimathi/cl-zipper/blob/master/src/cl-zipper.lisp) in [almost-literate](http://inaimathi.ca/archive/by-tag/almost-literate-programming) style.

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

You can see influence from both [`clojure.zip`](https://github.com/clojure/clojure/blob/master/src/clj/clojure/zip.clj) and the paper here. I'm taking the lead from the paper by explicitly separating the `path` triple our from the `loc` definition. However, I'm not explicitly defining my own `type tree` the way that Huet does. Instead, I'm going to be dealing with assorted `lisp` trees. These could be implemented as `list`s, `vector`s, `hash`es, or any number of other formats. I'm going to implement a few type-distpatching built-ins, including the `make-zipper list` method above, but the basic `zipper` function just needs to take an interface as input in the form of `branch?`, `children` and `make-node` arguments. This is the same solution that the Clojure implementation went with, and I see no reason to go a different way. The only material difference is that theirs uses the Clojure [`metadata`](https://clojure.org/reference/metadata) system, while I explicitly define slots in the `loc` structure.

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

The main difference between this and the paper is that I've chosen `nil` as my `Top` representation, which lets me pull the trick of using `when` to check for the presence of a `path`, and its' non-`Top`-ness at the same time.

The bad news is that since Common Lisp doesn't have [pervasive functional data structures](file:///home/inaimathi/Downloads/ctries-techreport.pdf), I have to explicitly copy `loc`s while moving through a tree. The good news is that the copy is fairly light weight. Effectively, I'm copying out a set of 5 pointers, and could get that down to 3 by defining an intermediate struct.

Hm.

Which I probably should do. Note to self.

Out of those, we get three compound navigation functions. With more probably coming soon. Specifically, I found `find` useful for the work I did. It's easily externally definable, but would be even easier to bundle along. The ones I've already implemented are `root`, `leftmost` and `rightmost`.

```lisp
;;;;;;;;;;;;;;; Compound navigation
(defun root (zipper)
  (if-let (z (while zipper #'up))
    (node z)))

(defun leftmost (zipper) (while zipper #'left))

(defun rightmost (zipper) (while zipper #'right))
```
Each of these involve an intermediate call to `while`. Which isn't a generic `macro`; it's a function defined in [`util.lisp`](https://github.com/inaimathi/cl-zipper/blob/master/src/util.lisp)

```lisp
...
(defun until (zipper f)
  (let ((z zipper))
    (loop for next = (funcall f z) while next
       when next do (setf z next))
    z))
...
```
As you can see, all it does is repeatedly call a given function on a `zipper` and return the last non-`nil` `loc` result. That's `loc`, not `node`, so this _doesn't_ run into the usual Common Lisp conflict of "Did you fail to find a thing, or find the element `nil`?".

That's the traversals done. Next up, we've got modification, without which this library is fairly useless. The basics are `replace`, `delete` and the `insert`/`child` twins.

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
	   (list node (node zipper)))
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

The paper defines an `insert_down` function. It fails on a Leaf node, and otherwise inserts a singleton branch at the given location. The `insert`/`append` child functions above also insert nodes at a lower level at the current `loc`. They give you a choice about whether to insert the new node as the leftmost or rightmost child, and additionally succeed on Leaf nodes by including the leaf value as a child of the new branch.

There are, thus far, three compound modification functions; `edit`, `splice-left` and `splice-right`.

```lisp
(defun edit (zipper f &rest args)
  (replace zipper (apply f (node zipper) args)))

(defun splice-left (zipper node-list)
  (reduce #'insert-left node-list :initial-value zipper))

(defun splice-right (zipper node-list)
  (reduce #'insert-right (reverse node-list) :initial-value zipper))
```

`edit` takes a function instead of a new node, and replaces the node at `loc` with the result of running that function on the existing node. The `splice-*` twins are fairly self-explanatory; they're like `insert-left`/`insert-right`, but work on multiple nodes rather than single ones.

I haven't yet implemented `next`, `prev` and `remove` because these _might_ relate to the different representation of the [traversal `end?` state](https://github.com/clojure/clojure/blob/master/src/clj/clojure/zip.clj#L244). The reason for this _seems_ to be that `next`/`prev`/`remove` assume a [depth-first traversal](https://www.cs.usfca.edu/~galles/visualization/DFS.html). The reason I'm being weasely here is that I haven't thought about it hard enough to be sure that the `end?` marker is really necessary. It also seems odd to privilege depth-first over breadth-first traversals; ideally, I think you'd want to be able to support either. Possibly interchangeably.

## Minor Housekeeping

That wraps it up for this edition. My immediate intention is to do more work on the `cl-zipper` and `clj` libraries, as well as that game I mentioned last time. Ideally, I'd like to up my blogging output too. Probably not to the same volume as I had at my peak, but it was definitely helpful to keep some sort of written journal around for a while. The current state of the world is, hopefully, going to make it easy for me to get more programming time in. All things considered, I'd count that as a win.
