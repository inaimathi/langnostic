I needed to make a note of this, because I've just tried running [`fact-base`](https://github.com/Inaimathi/fact-base) in `SBCL 1.2.8.nixos`, and got some errors when using `for-all`.

This is because of the `1.2.2` change in "internal representation" of the `backquote` reader macro. [Enough](http://christophe.rhodes.io/notes/blog/posts/2014/backquote_and_pretty_printing/) metaphorical ink [has](http://christophe.rhodes.io/notes/blog/posts/2014/naive_vs_proper_code-walking/) been spilled on this that I don't particularly care to add much more. But. I need to point out a couple of things about the situation.

## Trees That Aren't

So here's something to consider

```lisp
CL-USER> (defparameter *foo* 1)
*FOO*
CL-USER> (list *foo* '*bar* '*baz*)
(1 *BAR* *BAZ*)
CL-USER> `(,*foo* *bar* *baz*)
(1 *BAR* *BAZ*)
```

This is two ways of expressing the same idea; "The list of [the value of *foo*], [the symbol *bar*] and [the symbol *baz*]". They're conceptually and structurally equivalent, even if their representations are different. In fact, their representations *are* different.

```lisp
CL-USER> (equal (list *foo* '*bar* '*baz*) `(,*foo* *bar* *baz*))
T
CL-USER> (equal '(list *foo* '*bar* '*baz*) '`(,*foo* *bar* *baz*))
NIL
CL-USER> 
```

I mean duh.

Now, on a completely separate topic, here's how you might write a function called `tree-find`.

```lisp
(defun tree-find (tree elem &key (test #'eq))
  (if (atom tree)
      (funcall test tree elem)
      (or (tree-find (car tree) elem :test test)
          (tree-find (cdr tree) elem :test test))))
```

This function has no expectations about what kind of tree it might be passed, or in what context. It expects to deal with trees in general. That those trees happen to be representations of Lisp code in some cases should make no difference whatsoever to its operation. What it gets is a tree and an element. It traverses the tree, tests for the given element at each stage, returning true if the element is found. You could write this a number of different ways, but the essence is that if an element exists in a given tree, it should return `T`.

```lisp
CL-USER> (tree-find (list *foo* '*bar* '*baz*) '*bar*)
T
CL-USER> (tree-find (list *foo* '*bar* '*baz*) 1)
T
CL-USER> (tree-find `(,*foo* *bar* *baz*) '*bar*)
T
CL-USER> (tree-find `(,*foo* *bar* *baz*) 1)
T
CL-USER> 
```

The question at the core of [the `SBCL 1.2.2` "kerfuffle"](http://christophe.rhodes.io/notes/blog/posts/2014/backquote_and_pretty_printing/) seems to be: what should happen when we call `tree-find` like this

```lisp
CL-USER> (tree-find '(list *foo* '*bar* '*baz*) '*foo*)
???
CL-USER> (tree-find '`(,*foo* *bar* *baz*) '*foo*)
???
CL-USER> 
```

Before moving on, think a bit about what you expect to happen and why.

The answer in most CL implementations, including pre-`1.2.2 SBCL`, is `T`. The symbol is present in the tree we're considering, so the return value of `tree-find` should reflect that.

```lisp
CL-USER> (tree-find '(list *foo* '*bar* '*baz*) '*foo*)
T
CL-USER> (tree-find '`(,*foo* *bar* *baz*) '*foo*)
T
CL-USER> 
```

The answer in `SBCL 1.2.2+` is that the symbol `*foo*` isn't *really* in that tree. Because unquoted forms are something fundamentally different from `cons`es. They're more like vectors; compound structures that you don't automatically expect to recur into with general tree operations. And so the first call should return `T`, while the second should return `NIL`.

```lisp
CL-USER> (tree-find '(list *foo* '*bar* '*baz*) '*foo*)
T
CL-USER> (tree-find '`(,*foo* *bar* *baz*) '*foo*)
NIL
CL-USER> 
```

The problem with this chain of reasoning is that you must arrive at the conclusion that *quoted forms aren't the same kind of tree as all other Lisp trees*. They are different, and you must treat them differently if you expect certain results out of them. From the posts I've read by Christophe Rhodes, the expectation is that users do "proper code walking" in situations of heavy macrology. Except, that's not quite enough. Because as I've shown above, you don't need to introduce any macros in order for this new representation to break compatibility.

If I take his reasoning at face value, there seem to be two legitimate ways to deal with this decision as users of the language:


1.   All tree-traversing functions must henceforth use code walkers instead of direct recursion internally. Because any tree-traversal function might be passed a "tree" involving multiple layers of quasiquoting, they must all be able to deal with the situation if they are to really reflect general tree-traversal.
1.   Any time you need to traverse a "tree" that may involve layers of quasiquoting, you must not use a regular tree-traversing function. You must instead construct a custom version to handle the specific kind of tree that represents quasiquoted forms.


Both of those seem ridiculous<a name="note-Sat-Feb-21-103325EST-2015"></a>[|1|](#foot-Sat-Feb-21-103325EST-2015). Both are going to introduce<a name="note-Sat-Feb-21-103350EST-2015"></a>[|2|](#foot-Sat-Feb-21-103350EST-2015) errors into existing code, and either complexity or redundancy into new code, for the purpose of getting some guarantees about edge-case behavior in certain fairly specialized situations. Which honestly doesn't sound like a very good trade.

## Internal Representations That Leak

If you make a change to the "internal representation" of something, it sounds as if you're implying that a user can expect to use it as before without seeing the difference.

That is not the case for this particular change, as demonstrated above. And calling it "internal" is, at the very least, playing fast and loose with the meaning of the word. This is an externally visible, compatibility-breaking change with other Common Lisp implementations. That may or may not be the right thing to do in a particular situation. But given that users can clearly see the difference between representations from the outside, I'm confused as to why the nature of the change is in question.

## A Conclusion That Doesn't

There really isn't a good one for me.

The reason I decided to pull down the `nix` version of SBCL and test out `for-all` is that my team at work was finding some odd bugs in testing. Those bugs had the same culprit of inconsistent tree representations. Luckily we do automated tests, and happened to do them on some prospective versions of CL in advance of an upgrade. In practice, what I'm finding is that they're leaning much more heavily to the implicit Option 3: don't use `SBCL 1.2.2+`, and migrate away from using earlier versions as we move forward. From what I understand, [LispWorks](http://www.lispworks.com/products/lispworks.html) is the current mind-share majority holder, which puts me in a mildly uncomfortable position as the resident [Free-Software](https://www.gnu.org/philosophy/free-sw.html)-using Lisper. Only mildly uncomfortable, because a close second choice seems to be a somewhat slower migration to Clojure, And [my opinion on it is well documented](/article?name=recommendations.html). I'm only woolgathering at this point though. We'll switch, or we won't, and at the end of the day our code will still work for the purposes to which we wish to put it. The only action items I get out of this are:


-   work up a comparison chart of the extant Common Lisp implementations, and see if there's another one that fits our needs
-   start building serious things with Clojure


* * *
##### Footnotes
1 - <a name="foot-Sat-Feb-21-103325EST-2015"></a>[|back|](#note-Sat-Feb-21-103325EST-2015) - In the context of Common Lisp, at least. I'd fully expect that kind of conceptual separation in Haskell, ML, or a member of the ML family. But if I pick up Common Lisp, I'm very deliberately *not* picking up any of those tools. And *here*, it's kind of preposterous that such a distinction should be the default.

2 - <a name="foot-Sat-Feb-21-103350EST-2015"></a>[|back|](#note-Sat-Feb-21-103350EST-2015) - Some would say, "detect previously undetected errors", but I'm not convinced there aren't false positives in the mix.
