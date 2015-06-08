Programmers work on many different things, and approach them from many different directions. But I believe that, fundamentally, the primary endeavor of ever programmer is twofold:

To understand, and to be understood.

This is the big problem we all struggle with. I remember reading somewhere that one of those was supposed to be more important than the other, but consider the notion bullshit. You seem to need both, in about even measure, to do anything worthwhile. The "To understand" part is a big problem in its own right, and seems to boil down to "never stop learning". Seriously, if you ever think you're **Done** learning, slap yourself across the face because I *guarantee* you're wrong. But that's one side of the equation.

The other side, "To be understood" seems harder. Whether the target audience is your fellow programmers, or management/business types that you need to keep in the loop, or a future instance of yourself after some months away from the codebase. There's a bunch of different approaches out there, none of them are perfectly general, and they're all only scratching the surface. Haskellers and MLers swear by types, hoping that additional, compiler-checked meta-information about a particular function is useful to the reader. [Eiffel](https://www.eiffel.com/values/design-by-contract/) and other [contract](http://docs.racket-lang.org/guide/contracts-first.html)/[dependent type](http://www.idris-lang.org/) systems go a bit further in the same direction. [Rather](http://www.haskell.org/haddock/doc/html/) a [lot](http://happydoc.sourceforge.net/) of [people](http://www.oracle.com/technetwork/java/javase/documentation/index-137868.html) just [try](https://github.com/clojuredocs/doc-extractor#what-is-extractor) to [keep](http://perldoc.perl.org/perlpod.html) ad-hoc documentation in along with their code, or perhaps [in external articles](/archive/by-tag?tag=ALMOST-LITERATE-PROGRAMMING), and call it a day. If you've been following [my](https://github.com/Inaimathi/cl-notebook) recent [exploits](https://vimeo.com/97623064) at all, you'll know that I'm experimenting with [literate programming](https://en.wikipedia.org/wiki/Literate_programming) concepts. Interweaving prose explanations, examples and executable code to try to get a form of documentation that


- imparts more than a trivial understanding of a particular set of functions and their interactions
- is useful to people that don't necessarily already have a developed understanding of the underlying systems
- doesn't go stale through inattention 


It's hard. Even having written my own editor for this purpose, it's not clear that I can ever do a complete job. I'll keep experimenting with it; it might work eventually.

There is another approach though. It doesn't necessarily solve all the problems, and I've already mentioned that I'm not anything like a true believer yet. But it shows surprising promise; enough that my co-conspirators and I have been doing actual, professional development with it.

The approach is Visual Compilation. I like to call it "program injection" in contrast to "doc extraction", because the way it fundamentally works is by accepting a top-level flow diagram from you and using it to generate code. Like I said, it works surprisingly well. We've successfully used it to generate documentation that doesn't go stale, guaranteed<a name="note-Sun-Jun-29-120930EDT-2014"></a>[|1|](#foot-Sun-Jun-29-120930EDT-2014), and that's extremely useful in explaining the contents of the systems we're building, even to non-programmers. We use a particular approach called [Flow Based Programming](http://www.jpaulmorrison.com/fbp/) which lets us generate a particular set of control-flow primitives from a hierarchical diagram and fall through to three or four lines of code in some leaf components.

### <a name="constraint-propagation" href="#constraint-propagation"></a>Constraint Propagation

That's not intrinsically necessary though, depending on the underlying system you want to support. Consider the [constraint propagation system](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.5) from [chapter 3 of SICP](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-19.html#%_chap_3). There's a very small set of primitives that describe `constraint`s, `probe`s and the `connector`s that span them. Any more complicated construct that you need, you build by composing the above<a name="note-Sun-Jun-29-120935EDT-2014"></a>[|2|](#foot-Sun-Jun-29-120935EDT-2014). This lends itself to simple visual compilation with no "leaf components" in the way that I meant above. The main point is that you can run these constraints in multiple directions. Unlike our FBP components, which have definite inputs and outputs, `constraint`s have multiple `connector`s, any of which might serve as input or output. If that sounds interesting, I encourage you to read that [SICP chapter](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-19.html#%_chap_3) I pointed you to. All I'm going to be doing is showing how you might go about generating code from the constraint diagrams they draw, rather than generating code by hand and separately producing documenting diagrams.

Here's a simple example directly from the book:

![The celsius<->faranheit converter image from SICP](/static/img/sicp-constraint-image.gif)

It's a Celsius to Faranheit temperature converter. The idea is that you can either chuck a Celsius temperature at it and get out the Faranheit, *or* you can give it a Faranheit to take to Celsius. Here's the in-book code they show you that does the job:

```lisp
(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(celsius-fahrenheit-converter C F)
```

### <a name="textual-representation" href="#textual-representation"></a>Textual Representation

I'm working in Common Lisp rather than Scheme, so I've had to make some changes to the underlying system. You can check out my implementation [here](https://github.com/CompSciCabal/SMRTYPRTY/blob/master/experiments/inaimathi/sicp-constraints/sicp-constraints.lisp). The corresponding code targeting that implementation is:

```lisp
(defvar *c* (make-connector))
(defvar *f* (make-connector))

(defun celsius-faranheit (c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (make-multiplier :m1 c :m2 w :product u)
    (make-multiplier :m1 v :m2 x :product u)
    (make-adder :a1 v :a2 y :sum f)
    (make-constant 9 w)
    (make-constant 5 x)
    (make-constant 32 y)
    'ok))

(make-probe "Celsius temp" *c*)
(make-probe "Faranheit temp" *f*)
(celsius-faranheit *c* *f*)
```

The only conceptual change I had to make that *isn't* related to the language shift is that all the constraint arguments are keyword args. This is in service of having an easier time ordering inputs later<a name="note-Sun-Jun-29-120943EDT-2014"></a>[|3|](#foot-Sun-Jun-29-120943EDT-2014). So the goal is to take something like that picture above, and emit something like that code in a reasonably simple, and completely automated manner. The hope is that the result will be more easily understood by readers, and certain to avoid getting stale. Step by step then, we need a drawing.

### <a name="graphical-representation" href="#graphical-representation"></a>Graphical Representation

Here's one created with `2dmacs`, the visual editor we use in Moneris' Web POS Development team. It's not yet open source, though we do have plans in that direction, so I can only show you its output rather than its innards.

![A visual program that converts Celsius to Faranheit](/static/img/2dmacs-celsius-to-faranheit.png)

I've adopted some formalisms from the SICP diagram, and made up a couple:

- A `constraint` is a rectangle
- A text box contained by, and touching the border of a constraint is one of its arguments
- A text box contained by, but *not* touching the border of a constraint is its name
- A `constant` is a rounded rectangle perfectly overlapped by a text element (the text element contains its value)
- A `connector` is a set of one or more lines
- Any other text box is taken to be a top level `connector`


2dmacs internally represents diagrams as triple-stores. Here's the triple-store that represents the above diagram:

```lisp
((:FACT45 :CONTENTS "sum
")
 (:FACT57 :START (240 160))
 (:FACT57 :END (300 200))
 (:FACT58 :END (560 200))
 (:FACT58 :START (500 160))
 (:FACT50 :END (520 200))
 (:FACT51 :START (260 160))
 (:FACT51 :V-ALIGN :MIDDLE)
 (:FACT51 :H-ALIGN :RIGHT)
 (:FACT51 :CONTENTS "product
")
 (:FACT50 :CONTENTS "product
")
 (:FACT47 :END (940 200))
 (:FACT47 :START (860 160))
 (:FACT47 :CONTENTS "faranheit
")
 (:FACT55 :START (80 120))
 (:FACT55 :CONTENTS "celsius
")
 (:FACT84 :FOLLOW (:FACT75 :END))
 (:FACT84 :LEAD (:FACT76 :START))
 (:FACT84 :CONSTRAIN :X)
 (:FACT84 :CONSTRAINT NIL)
 (:FACT0 :VALUE 85)
 (:FACT83 :FOLLOW (:FACT76 :START))
 (:FACT83 :LEAD (:FACT75 :END))
 (:FACT83 :CONSTRAIN :X)
 (:FACT83 :CONSTRAINT NIL)
 (:FACT82 :FOLLOW (:FACT75 :END))
 (:FACT82 :LEAD (:FACT76 :START))
 (:FACT82 :CONSTRAIN :Y)
 (:FACT82 :CONSTRAINT NIL)
 (:FACT81 :FOLLOW (:FACT76 :START))
 (:FACT81 :LEAD (:FACT75 :END))
 (:FACT81 :CONSTRAIN :Y)
 (:FACT81 :CONSTRAINT NIL)
 (:FACT80 :FOLLOW (:FACT74 :END))
 (:FACT80 :LEAD (:FACT75 :START))
 (:FACT80 :CONSTRAIN :X)
 (:FACT80 :CONSTRAINT NIL)
 (:FACT79 :FOLLOW (:FACT75 :START))
 (:FACT79 :LEAD (:FACT74 :END))
 (:FACT79 :CONSTRAIN :X)
 (:FACT79 :CONSTRAINT NIL)
 (:FACT78 :FOLLOW (:FACT74 :END))
 (:FACT78 :LEAD (:FACT75 :START))
 (:FACT78 :CONSTRAIN :Y)
 (:FACT78 :CONSTRAINT NIL)
 (:FACT77 :FOLLOW (:FACT75 :START))
 (:FACT77 :LEAD (:FACT74 :END))
 (:FACT77 :CONSTRAIN :Y)
 (:FACT77 :CONSTRAINT NIL)
 (:FACT76 :END (580 220))
 (:FACT76 :START (620 220))
 (:FACT76 :LINE-SEGMENT NIL)
 (:FACT75 :END (620 220))
 (:FACT75 :START (620 340))
 (:FACT75 :LINE-SEGMENT NIL)
 (:FACT74 :END (620 340))
 (:FACT74 :START (580 340))
 (:FACT74 :LINE-SEGMENT NIL)
 (:FACT73 :FOLLOW (:FACT64 :END))
 (:FACT73 :LEAD (:FACT65 :START))
 (:FACT73 :CONSTRAIN :X)
 (:FACT73 :CONSTRAINT NIL)
 (:FACT72 :FOLLOW (:FACT65 :START))
 (:FACT72 :LEAD (:FACT64 :END))
 (:FACT72 :CONSTRAIN :X)
 (:FACT72 :CONSTRAINT NIL)
 (:FACT71 :FOLLOW (:FACT64 :END))
 (:FACT71 :LEAD (:FACT65 :START))
 (:FACT71 :CONSTRAIN :Y)
 (:FACT71 :CONSTRAINT NIL)
 (:FACT70 :FOLLOW (:FACT65 :START))
 (:FACT70 :LEAD (:FACT64 :END))
 (:FACT70 :CONSTRAIN :Y)
 (:FACT70 :CONSTRAINT NIL)
 (:FACT69 :FOLLOW (:FACT63 :END))
 (:FACT69 :LEAD (:FACT64 :START))
 (:FACT69 :CONSTRAIN :X)
 (:FACT69 :CONSTRAINT NIL)
 (:FACT68 :FOLLOW (:FACT64 :START))
 (:FACT68 :LEAD (:FACT63 :END))
 (:FACT68 :CONSTRAIN :X)
 (:FACT68 :CONSTRAINT NIL)
 (:FACT67 :FOLLOW (:FACT63 :END))
 (:FACT67 :LEAD (:FACT64 :START))
 (:FACT67 :CONSTRAIN :Y)
 (:FACT67 :CONSTRAINT NIL)
 (:FACT66 :FOLLOW (:FACT64 :START))
 (:FACT66 :LEAD (:FACT63 :END))
 (:FACT66 :CONSTRAIN :Y)
 (:FACT66 :CONSTRAINT NIL)
 (:FACT65 :END (220 220))
 (:FACT65 :START (180 220))
 (:FACT65 :LINE-SEGMENT NIL)
 (:FACT64 :END (180 220))
 (:FACT64 :START (180 340))
 (:FACT64 :LINE-SEGMENT NIL)
 (:FACT63 :END (180 340))
 (:FACT63 :START (220 340))
 (:FACT63 :LINE-SEGMENT NIL)
 (:FACT15 :END (300 360))
 (:FACT15 :START (220 320))
 (:FACT14 :END (300 360))
 (:FACT14 :START (220 320))
 (:FACT13 :END (300 360))
 (:FACT13 :START (220 320))
 (:FACT60 :VALUE "celsius-to-faranheit")
 (:FACT60 :DIAGRAM-NAME NIL)
 (:FACT59 :CONTENTS "+
")
 (:FACT59 :TEXT NIL)
 (:FACT59 :WEIGHT :NORMAL)
 (:FACT59 :SIZE 16)
 (:FACT59 :H-ALIGN :MIDDLE)
 (:FACT59 :V-ALIGN :MIDDLE)
 (:FACT59 :START (700 160))
 (:FACT59 :END (760 200))
 (:FACT58 :TEXT NIL)
 (:FACT58 :WEIGHT :NORMAL)
 (:FACT58 :SIZE 16)
 (:FACT58 :H-ALIGN :MIDDLE)
 (:FACT58 :V-ALIGN :MIDDLE)
 (:FACT58 :CONTENTS "*
")
 (:FACT57 :CONTENTS "*
")
 (:FACT57 :V-ALIGN :MIDDLE)
 (:FACT57 :H-ALIGN :MIDDLE)
 (:FACT57 :SIZE 16)
 (:FACT57 :WEIGHT :NORMAL)
 (:FACT57 :TEXT NIL)
 (:FACT55 :END (160 160))
 (:FACT56 :END (220 140))
 (:FACT56 :START (160 140))
 (:FACT56 :LINE-SEGMENT NIL)
 (:FACT55 :TEXT NIL)
 (:FACT55 :WEIGHT :NORMAL)
 (:FACT55 :SIZE 16)
 (:FACT55 :H-ALIGN :MIDDLE)
 (:FACT55 :V-ALIGN :MIDDLE)
 (:FACT53 :END (260 240))
 (:FACT53 :START (220 200))
 (:FACT54 :END (260 160))
 (:FACT54 :START (220 120))
 (:FACT51 :END (360 200))
 (:FACT50 :START (440 160))
 (:FACT49 :END (580 240))
 (:FACT49 :START (540 200))
 (:FACT48 :START (540 120))
 (:FACT48 :END (580 160))
 (:FACT46 :START (800 180))
 (:FACT46 :END (860 180))
 (:FACT45 :START (760 160))
 (:FACT45 :END (800 200))
 (:FACT44 :START (660 120))
 (:FACT44 :END (700 160))
 (:FACT41 :START (720 200))
 (:FACT41 :END (760 240))
 (:FACT42 :START (740 240))
 (:FACT42 :END (740 320))
 (:FACT12 :START (700 320))
 (:FACT12 :END (780 360))
 (:FACT11 :START (700 320))
 (:FACT11 :END (780 360))
 (:FACT10 :START (700 320))
 (:FACT10 :END (780 360))
 (:FACT7 :START (500 320))
 (:FACT7 :END (580 360))
 (:FACT8 :START (500 320))
 (:FACT8 :END (580 360))
 (:FACT9 :START (500 320))
 (:FACT9 :END (580 360))
 (:FACT6 :START (580 140))
 (:FACT6 :END (660 140))
 (:FACT5 :START (360 180))
 (:FACT5 :END (440 180))
 (:FACT4 :END (800 240))
 (:FACT4 :START (660 120))
 (:FACT3 :END (580 240))
 (:FACT3 :START (440 120))
 (:FACT2 :START (220 120))
 (:FACT2 :END (360 240))
 (:FACT53 :CONTENTS "m2
")
 (:FACT54 :TEXT NIL)
 (:FACT54 :WEIGHT :NORMAL)
 (:FACT54 :SIZE 16)
 (:FACT54 :H-ALIGN :MIDDLE)
 (:FACT54 :V-ALIGN :MIDDLE)
 (:FACT54 :CONTENTS "m1
")
 (:FACT53 :TEXT NIL)
 (:FACT53 :WEIGHT :NORMAL)
 (:FACT53 :SIZE 16)
 (:FACT53 :H-ALIGN :MIDDLE)
 (:FACT53 :V-ALIGN :MIDDLE)
 (:FACT51 :SIZE 16)
 (:FACT51 :WEIGHT :NORMAL)
 (:FACT51 :TEXT NIL)
 (:FACT50 :TEXT NIL)
 (:FACT50 :WEIGHT :NORMAL)
 (:FACT50 :SIZE 16)
 (:FACT50 :H-ALIGN :MIDDLE)
 (:FACT50 :V-ALIGN :MIDDLE)
 (:FACT49 :CONTENTS "m2
")
 (:FACT49 :TEXT NIL)
 (:FACT49 :WEIGHT :NORMAL)
 (:FACT49 :SIZE 16)
 (:FACT49 :H-ALIGN :MIDDLE)
 (:FACT49 :V-ALIGN :MIDDLE)
 (:FACT48 :CONTENTS "m1
")
 (:FACT48 :V-ALIGN :MIDDLE)
 (:FACT48 :H-ALIGN :MIDDLE)
 (:FACT48 :SIZE 16)
 (:FACT48 :WEIGHT :NORMAL)
 (:FACT48 :TEXT NIL)
 (:FACT47 :V-ALIGN :MIDDLE)
 (:FACT47 :H-ALIGN :MIDDLE)
 (:FACT47 :SIZE 16)
 (:FACT47 :WEIGHT :NORMAL)
 (:FACT47 :TEXT NIL)
 (:FACT46 :LINE-SEGMENT NIL)
 (:FACT45 :TEXT NIL)
 (:FACT45 :WEIGHT :NORMAL)
 (:FACT45 :SIZE 16)
 (:FACT45 :H-ALIGN :MIDDLE)
 (:FACT45 :V-ALIGN :MIDDLE)
 (:FACT44 :CONTENTS "a1
")
 (:FACT44 :TEXT NIL)
 (:FACT44 :WEIGHT :NORMAL)
 (:FACT44 :SIZE 16)
 (:FACT44 :H-ALIGN :MIDDLE)
 (:FACT44 :V-ALIGN :MIDDLE)
 (:FACT41 :CONTENTS "a2
")
 (:FACT42 :LINE-SEGMENT NIL)
 (:FACT41 :V-ALIGN :MIDDLE)
 (:FACT41 :H-ALIGN :MIDDLE)
 (:FACT41 :SIZE 16)
 (:FACT41 :WEIGHT :NORMAL)
 (:FACT41 :TEXT NIL)
 (:FACT15 :CONTENTS "9
")
 (:FACT14 :ROUNDED-RECTANGLE NIL)
 (:FACT14 :CORNER-RADIUS 5)
 (:FACT15 :TEXT NIL)
 (:FACT15 :WEIGHT :NORMAL)
 (:FACT15 :SIZE 16)
 (:FACT15 :H-ALIGN :MIDDLE)
 (:FACT15 :V-ALIGN :MIDDLE)
 (:FACT13 :GROUP NIL)
 (:FACT13 :AREA 3200)
 (:FACT13 :GROUPED :FACT15)
 (:FACT13 :GROUPED :FACT14)
 (:FACT12 :CONTENTS "32
")
 (:FACT11 :ROUNDED-RECTANGLE NIL)
 (:FACT11 :CORNER-RADIUS 5)
 (:FACT12 :TEXT NIL)
 (:FACT12 :WEIGHT :NORMAL)
 (:FACT12 :SIZE 16)
 (:FACT12 :H-ALIGN :MIDDLE)
 (:FACT12 :V-ALIGN :MIDDLE)
 (:FACT10 :GROUP NIL)
 (:FACT10 :AREA 3200)
 (:FACT10 :GROUPED :FACT12)
 (:FACT10 :GROUPED :FACT11)
 (:FACT9 :GROUPED :FACT7)
 (:FACT9 :GROUPED :FACT8)
 (:FACT9 :AREA 3200)
 (:FACT9 :GROUP NIL)
 (:FACT8 :V-ALIGN :MIDDLE)
 (:FACT8 :H-ALIGN :MIDDLE)
 (:FACT8 :SIZE 16)
 (:FACT8 :WEIGHT :NORMAL)
 (:FACT8 :CONTENTS "5
")
 (:FACT8 :TEXT NIL)
 (:FACT7 :CORNER-RADIUS 5)
 (:FACT7 :ROUNDED-RECTANGLE NIL)
 (:FACT6 :LINE-SEGMENT NIL)
 (:FACT5 :LINE-SEGMENT NIL)
 (:FACT4 :RECTANGLE NIL)
 (:FACT3 :RECTANGLE NIL)
 (:FACT2 :RECTANGLE NIL)
 (:FACT0 :NEXT-ID NIL))
```

It represents, at a fairly low level, what the diagram contains. The approach should remind you of logic programming data stores, because it's heavily inspired by them. The idea is that any given primitive that we draw is represented by several, not necessarily contiguous assertions. For instance, this represents a rectangle:

```lisp
...
 (:FACT4 :END (800 240))
 (:FACT4 :START (660 120))
...
 (:FACT4 :RECTANGLE NIL)
...
```

As an aside, the reason behind using this data representation approach is that it demands as little commitment from the programmer as possible. The three facts I just showed you can be interpreted as a rectangle that starts at `660x120` and ends at `800x240`, but you can consider these facts individually, or together with individual facts from other elements as you need to. Pushing down to a list of k/v constructs would pre-maturely commit you to the idea that these are objects which you should be considering holistically.

Really, though, aside. The only reason I showed you that representation is so that the traversers and modifiers I'm *about* to show you can make some sense.

### <a name="bridging-the-two" href="#bridging-the-two"></a>Bridging the Two

Before we get into the underlying mechanics of *how* it happens, here's *what* we want to happen:

```lisp
CL-USER> (ql:quickload :sicp-constraints)
To load "sicp-constraints":
  Load 1 ASDF system:
    sicp-constraints
; Loading "sicp-constraints"
[package sicp-constraints]....
(:SICP-CONSTRAINTS)
CL-USER> (in-package :sicp-constraints)
#<PACKAGE "SICP-CONSTRAINTS">
SICP-CONSTRAINTS> (compile-diagram :repl #p"celsius-to-faranheit.base")
(PROGN
 (DEFVAR *FARANHEIT* (MAKE-CONNECTOR))
 (MAKE-PROBE "FARANHEIT" *FARANHEIT*)
 (DEFVAR *CELSIUS* (MAKE-CONNECTOR))
 (MAKE-PROBE "CELSIUS" *CELSIUS*)
 (DEFUN CELSIUS-TO-FARANHEIT (FARANHEIT CELSIUS)
   (LET ((G1475 (MAKE-CONNECTOR))
         (G1474 (MAKE-CONNECTOR))
         (G1473 (MAKE-CONNECTOR))
         (G1470 (MAKE-CONNECTOR))
         (G1469 (MAKE-CONNECTOR)))
     (MAKE-MULTIPLIER :M1 CELSIUS :M2 G1470 :PRODUCT G1475)
     (MAKE-MULTIPLIER :PRODUCT G1475 :M2 G1469 :M1 G1474)
     (MAKE-ADDER :SUM FARANHEIT :A1 G1474 :A2 G1473)
     (MAKE-CONSTANT 5 G1469)
     (MAKE-CONSTANT 32 G1473)
     (MAKE-CONSTANT 9 G1470)
     'OK))
 (CELSIUS-TO-FARANHEIT *FARANHEIT* *CELSIUS*))
SICP-CONSTRAINTS> (eval (compile-diagram :repl #p"celsius-to-faranheit.base"))
OK
SICP-CONSTRAINTS> (set! *celsius* 25 'user)
Probe: FARANHEIT = 77
Probe: CELSIUS = 25
NIL
SICP-CONSTRAINTS> (forget! *celsius* 'user)
Probe: FARANHEIT = ?
Probe: CELSIUS = ?
NIL
SICP-CONSTRAINTS> (set! *faranheit* 125 'user)
Probe: CELSIUS = 155/3
Probe: FARANHEIT = 125
NIL
SICP-CONSTRAINTS> (forget! *faranheit* 'user)
Probe: CELSIUS = ?
Probe: FARANHEIT = ?
NIL
SICP-CONSTRAINTS> 
```

That is, there should be a function `compile-diagram` that takes a compilation target<a name="note-Sun-Jun-29-120959EDT-2014"></a>[|4|](#foot-Sun-Jun-29-120959EDT-2014) and a pathname, and produces the expression that corresponds to the constraint system that the given diagram represents. If you `eval` the result, what you should get is a functioning system that lets you `set!` and `forget!` the appropriate variables to see `constraint`s propagate.

With that in mind, here's the compiler:

```lisp
(in-package :sicp-constraints)

(defmethod compile-diagram (target (file pathname))
  (with-open-file (s file) 
    (compile-diagram target (read s))))

(defmethod compile-diagram (target (base list))
  (generate-code target (synthesize base)))

;;;;;;;;;; Synthesizing new facts
(defmethod synthesize ((base list))
  (label-line-cluster
   (label-line-connections
    (label-connection-endpoints
     (label-top-inputs
      (label-arguments-and-name
       (label-labels
        (label-constants
         (label-constraints base)))))))))

(defmethod label-constraints ((base list))
  "Any rectangle is a constraint."
  (for-all (?id :rectangle nil) :in base :do (push (list ?id :sicp-constraint nil) base))
  base)

(defmethod label-constants ((base list))
  "A rounded rectangle with a perfectly overlapping text box is a constant."
  (for-all (and (?id :rounded-rectangle nil)
                (?id :start ?start) (?id :end ?end)
                (?text :text nil)
                (?text :start ?start) (?text :end ?end)
                (?text :contents ?value))
           :in base :do (progn (push (list ?text :value (parse-integer ?value :junk-allowed t)) base)
                               (push (list ?text :sicp-constant nil) base)))
  base)

(defun between? (a n b)
  (>= (max a b) n (min a b)))

(defun point-inside? (x y ax ay bx by)
  (and (between? ax x bx)
       (between? ay y by)))

(defmethod label-labels ((base list))
  "Any text box inside a constraint is a label pertaining to it."
  (for-all (and (?id :sicp-constraint nil)
                (?id :start (?cx ?cy)) (?id :end (?cx2 ?cy2))
                (?text :text nil)
                (?text :start (?x ?y)) (?text :end (?x2 ?y2))
                (lisp (and (point-inside? ?x ?y ?cx ?cy ?cx2 ?cy2)
                           (point-inside? ?x2 ?y2 ?cx ?cy ?cx2 ?cy2))))
           :in base :do (push (list ?id :constraint-of ?text) base))
  base)

(defun on-edge? (x y ax ay bx by)
  (or (= x ax) (= x bx)
      (= y ay) (= y by)))

(defmethod label-arguments-and-name ((base list))
  "Text boxes contained by constraints, and on their edges are arguments.
Text boxes contained by constraints but not on their edges are constraint names."
  (for-all (and (?id :constraint-of ?text)
                (?id :start (?cx ?cy)) (?id :end (?cx2 ?cy2))
                (?text :start (?tx ?ty)) (?text :end (?tx2 ?ty2))
                (lisp (or (on-edge? ?tx ?ty ?cx ?cy ?cx2 ?cy2)
                          (on-edge? ?tx2 ?ty2 ?cx ?cy ?cx2 ?cy2))))
           :in base :do (push (list ?text :sicp-argument nil) base))
  (for-all (and (?id :constraint-of ?text)
                (not (?text :sicp-argument nil)))
           :in base :do (push (list ?text :sicp-constraint-name nil) base))
  base)

(defmethod label-top-inputs ((base list))
  "Any text boxes that haven't yet been annotated as constraint-names, arguments or constants are top level inputs for the system."
  (for-all (and (?id :text nil)
                (not (?id :sicp-constraint-name nil)
                     (?id :sicp-argument nil)
                     (?id :sicp-constant nil)))
           :in base :do (push (list ?id :sicp-top-connection nil) base))
  base)

(defmethod label-line-connections ((base list))
  "Some lines connect to other lines. Connecting lines will be a single connection rather than multiple connections."
  (for-all (and (?id :line-segment nil)
                (?id :start ?start) (?id :end ?end)
                (?id2 :line-segment nil)
                (lisp (not (equal ?id ?id2)))
                (or (?id2 :start ?end) (?id2 :end ?start)
                    (?id2 :start ?start) (?id2 :end ?end)))
           :in base :do (push (list ?id :line-connects-to-line ?id2) base))
  base)

(defmethod label-connection-endpoints ((base list))
  "Some line-segments connect to arguments, constants or top-connections, and this needs to be stated explicitly."
  (for-all (and (?id :line-segment nil)
                (?id :start (?x ?y)) (?id :end (?x2 ?y2))
                (or (?id2 :sicp-argument nil)
                    (?id2 :sicp-constant nil)
                    (?id2 :sicp-top-connection nil))
                (?id2 :start (?ax ?ay)) (?id2 :end (?ax2 ?ay2))
                (lisp (or (and (point-inside? ?x ?y ?ax ?ay ?ax2 ?ay2) 
                               (on-edge? ?x ?y ?ax ?ay ?ax2 ?ay2))
                          (and (point-inside? ?x2 ?y2 ?ax ?ay ?ax2 ?ay2) 
                               (on-edge? ?x2 ?y2 ?ax ?ay ?ax2 ?ay2)))))
           :in base :collect (push (list ?id :connects-to ?id2) base))
  base)

(defun walk-segment-graph (base src &key (explored (list src)))
  (for-all `(and (or (,src :line-connects-to-line ?id)
                     (?id :line-connects-to-line ,src))
                 (lisp (not (member ?id (list ,@explored)))))
           :in base
           :do (unless (member ?id explored)
                 (push ?id explored)
                 (setf explored (walk-segment-graph base ?id :explored explored))))
  explored)

(defmethod label-line-cluster ((base list))
  (let ((res (make-hash-table :test 'equal)))
    (for-all (and (?id :line-segment nil)
                  (not (?cluster :contains ?id)))
             :in base
             :collect (let ((cluster (sort (walk-segment-graph base ?id) #'string< :key #'symbol-name)))
                        (setf (gethash cluster res) t)))
    (loop for clst being the hash-keys of res
       do (let ((id (intern (symbol-name (gensym)))))
            (push (list id :cluster nil) base)
            (loop for elem in clst 
               do (push (list id :contains elem) base))))
    base))

;;;;;;;;;; Generating code on that basis
(defun sanitize (str)
  (string-upcase
   (string-right-trim 
    (list #\newline #\return #\space)
    str)))

(defmethod generate-constants ((base list))
  (for-all (and (?id :sicp-constant nil)
                (?id :value ?const)
                (?line :connects-to ?id)
                (?cluster :contains ?line))
           :in base :collect `(make-constant ,?const ,?cluster)))

(defmethod generate-internal-arguments ((base list) constraint)
  (let ((res))
    (for-all `(and (,constraint :constraint-of ?txt)
                   (?txt :sicp-argument nil)
                   (?txt :contents ?arg-name)
                   (?line :connects-to ?txt)
                   (?cluster :contains ?line)
                   (not (and (?cluster :contains ?line2)
                             (?line2 :connects-to ?top)
                             (?top :sicp-top-connection nil))))
             :in base 
             :do (progn (push ?cluster res)
                        (push (intern (sanitize ?arg-name) :keyword) res)))
    res))

(defmethod generate-top-arguments ((base list) constraint)
  (let ((res))
    (for-all `(and (,constraint :constraint-of ?txt)
                   (?txt :sicp-argument nil)
                   (?txt :contents ?arg-name)
                   (?line :connects-to ?txt)
                   (?line :connects-to ?top)
                   (?top :sicp-top-connection nil)
                   (?top :contents ?top-name))
             :in base :do (progn (push (intern (sanitize ?top-name)) res)
                                 (push (intern (sanitize ?arg-name) :keyword) res)))
    res))

(defmethod generate-arguments ((base list) constraint)
  (append (generate-top-arguments base constraint)
          (generate-internal-arguments base constraint)))

(defmethod generate-constraints ((base list))
  (for-all (and (?id :sicp-constraint nil)
                (?id :constraint-of ?txt)
                (?txt :sicp-constraint-name nil)
                (?txt :contents ?name))
           :in base
           :collect (case (intern (sanitize ?name) :keyword)
                      (:* `(make-multiplier ,@(generate-arguments base ?id)))
                      (:+ `(make-adder ,@(generate-arguments base ?id))))))

(defmethod generate-internal-connections ((base list))
  (for-all (and (?id :cluster nil)
                (not (and (?id :contains ?line)
                          (?line :connects-to ?elem)
                          (?elem :sicp-top-connection nil))))
           :in base :collect `(,?id (make-connector))))

(defmethod generate-component-factory ((component-name symbol) (args list) (base list))
  `(defun ,component-name ,args
     (let ,(generate-internal-connections base)
       ,@(generate-constraints base)
       ,@(generate-constants base)
       'ok)))

(defmethod get-component-name ((base list))
  (intern (first (for-all (and (?id :diagram-name nil)
                               (?id :value ?name))
                          :in base :collect (sanitize ?name)))))

(defmethod get-top-level-names ((base list))
  (for-all (and (?id :sicp-top-connection nil)
                (?id :contents ?name))
           :in base :collect (sanitize ?name)))

(defmethod generate-repl-app ((base list))
  (let* ((component-name (get-component-name base))
         (top-level-names (get-top-level-names base))
         (top-level-global-vars
          (mapcar (lambda (v) (intern (format nil "*~a*" v))) 
                  top-level-names)))
    `(progn ,@(loop for v in top-level-global-vars 
                 for n in top-level-names
                 collect `(defvar ,v (make-connector))
                 collect `(make-probe ,n ,v))
            ,(generate-component-factory 
              component-name (mapcar #'intern top-level-names) base)
            (,component-name ,@top-level-global-vars))))

(defmethod generate-code (target (base list))
  (error "Unsupported target: ~s" target))

(defmethod generate-code ((target (eql :repl)) (base list))
  (generate-repl-app base))
```

Don't worry, I'll go through all that in reasonable detail. But the point you should be getting from the above is that visual compilation *is possible*. It's not a piece of arcane magic that no mortal can tame. In fact, it isn't even particularly complicated as compilation goes. Which you'll see when we start getting into it. I mention this specifically because many seem to think this *is* magic. The reactions I get when I try to talk about visual compilation range from "There's no way this saves you time" to "That sounds ... hard" to looks of blank incomprehension. Every so often someone gets it and immediately has interest sparked.

So my aim today is to dispel the myth, and shine some light on an apparently rare, but promising-looking style of development.

As usual, the `in-package` form is a namespace term. It means we're acting in the namespace `:sicp-constraints`. The entry point here is `compile-diagram`

```lisp
(defmethod compile-diagram (target (file pathname))
  (with-open-file (s file) 
    (compile-diagram target (read s))))

(defmethod compile-diagram (target (base list))
  (generate-code target (synthesize base)))
```

Which is a method with two specializers. It takes a target, and either a `pathname` or a `list` (which it assumes to be a `fact-base`). If it gets a `pathname`, it reads the contents of the specified file and recursively calls itself, passing along the `target` and file contents. If it gets a list, it runs `synthesize` on it, and then calls `generate-code` on `target` and the result of that `synthesize`call. It's hopefully obvious from the method names, but `synthesize` will be making some new annotations in the given fact base to help us along, while `generate-code` will be generating the code for the appropriate compilation target<a name="note-Sun-Jun-29-121018EDT-2014"></a>[|5|](#foot-Sun-Jun-29-121018EDT-2014). Lets take a look at the `synthesize` tree first.

```lisp
(defmethod synthesize ((base list))
  (label-line-cluster
   (label-line-connections
    (label-connection-endpoints
     (label-top-inputs
      (label-arguments-and-name
       (label-labels
        (label-constants
         (label-constraints base)))))))))
```

It takes a `base` and runs a sequence of functions on it, returning the result of the last one. I could have defined a piece of syntactic sugar to act like [Clojures' thread (`->`) macro](http://clojuredocs.org/clojure_core/clojure.core/-%3E) so that I could write these out in their application order rather than in reverse, but I didn't. So we're going backwards through the pattern.

```lisp
(defmethod label-constraints ((base list))
  "Any rectangle is a constraint."
  (for-all (?id :rectangle nil) :in base :do (push (list ?id :sicp-constraint nil) base))
  base)
```

If you haven't seen a `for-all` call before, take a quick look at [this](http://blog.inaimathi.ca/article?name=how-unification-works.html) before we move on. The section near the bottom specifically pertains, but you should probably give the whole thing a skim. `label-constraints` annotates every `:rectangle` fact with an additional `:sicp-constraint` fact. It might look pointless at the moment, but I still want to separate the representation of `constraint`s from the underlying `rectangle` primitive, just in case I run into a situation that demands they be separated somehow.

```lisp
(defmethod label-constants ((base list))
  "A rounded rectangle with a perfectly overlapping text box is a constant."
  (for-all (and (?id :rounded-rectangle nil)
                (?id :start ?start) (?id :end ?end)
                (?text :text nil)
                (?text :start ?start) (?text :end ?end)
                (?text :contents ?value))
           :in base :do (progn (push (list ?text :value (parse-integer ?value :junk-allowed t)) base)
                               (push (list ?text :sicp-constant nil) base)))
  base)
```

`label-constants` specifically labels the `constant` constraints. In addition to noting their presence, it also parses out their numeric value and annotates that as well. Next up, the oddly named `label-labels`

```lisp
(defmethod label-labels ((base list))
  "Any text box inside a constraint is a label pertaining to it."
  (for-all (and (?id :sicp-constraint nil)
                (?id :start (?cx ?cy)) (?id :end (?cx2 ?cy2))
                (?text :text nil)
                (?text :start (?x ?y)) (?text :end (?x2 ?y2))
                (lisp (and (point-inside? ?x ?y ?cx ?cy ?cx2 ?cy2)
                           (point-inside? ?x2 ?y2 ?cx ?cy ?cx2 ?cy2))))
           :in base :do (push (list ?id :constraint-of ?text) base))
  base)
```

There's a bunch of labels inside of a `constraint`s' `rectangle` that pertain to it. Those labels might be arguments, or they might be the constraints' name. At this point, we're only annotating that relationship rather than deciding which type of label we're looking at. That happens in

```lisp
(defmethod label-arguments-and-name ((base list))
  "Text boxes contained by constraints, and on their edges are arguments.
Text boxes contained by constraints but not on their edges are constraint names."
  (for-all (and (?id :constraint-of ?text)
                (?id :start (?cx ?cy)) (?id :end (?cx2 ?cy2))
                (?text :start (?tx ?ty)) (?text :end (?tx2 ?ty2))
                (lisp (or (on-edge? ?tx ?ty ?cx ?cy ?cx2 ?cy2)
                          (on-edge? ?tx2 ?ty2 ?cx ?cy ?cx2 ?cy2))))
           :in base :do (push (list ?text :sicp-argument nil) base))
  (for-all (and (?id :constraint-of ?text)
                (not (?text :sicp-argument nil)))
           :in base :do (push (list ?text :sicp-constraint-name nil) base))
  base)
```

`label-arguments-and-name` annotates all text blocks on the edges of a `constraint` as arguments, then goes back through and labels any remaining pertinent text blocks as `:sicp-constraint-name`s. Incidentally, I'm using some minor utility functions here; `point-inside?` from `label-labels` and `on-edge?` in `label-arguments-and-name`. Their implementations are simple enough that I'll just refer you to the code, and their function should be obvious from their names. `point-inside?` takes three points, in raw coordinate format, and tells you whether the first one is between the next two. `on-edge?` takes the same inputs, and makes sure that the first point is on the line described by the second two<a name="note-Sun-Jun-29-121112EDT-2014"></a>[|6|](#foot-Sun-Jun-29-121112EDT-2014).

Moving on.

```lisp
(defmethod label-top-inputs ((base list))
  "Any text boxes that haven't yet been annotated as constraint-names, arguments or constants are top level inputs for the system."
  (for-all (and (?id :text nil)
                (not (?id :sicp-constraint-name nil)
                     (?id :sicp-argument nil)
                     (?id :sicp-constant nil)))
           :in base :do (push (list ?id :sicp-top-connection nil) base))
  base)
```

`label-top-inputs` is well explained by its docstring. Since we've already annotated all text blocks that pertain to `constraint`s and `constant`s, we take anything that's left to be top-level inputs for this component. In our example diagram, that would be the `celsius` and `faranheit`. At this point, we've got everything set up that'll let us extract appropriate names for constraints and their arguments. The next thing we need to do is figure out what are connections look like, which means finally taking a look at the lines in that image.

```lisp
(defmethod label-connection-endpoints ((base list))
  "Some line-segments connect to arguments, constants or top-connections, and this needs to be stated explicitly."
  (for-all (and (?id :line-segment nil)
                (?id :start (?x ?y)) (?id :end (?x2 ?y2))
                (or (?id2 :sicp-argument nil)
                    (?id2 :sicp-constant nil)
                    (?id2 :sicp-top-connection nil))
                (?id2 :start (?ax ?ay)) (?id2 :end (?ax2 ?ay2))
                (lisp (or (and (point-inside? ?x ?y ?ax ?ay ?ax2 ?ay2) 
                               (on-edge? ?x ?y ?ax ?ay ?ax2 ?ay2))
                          (and (point-inside? ?x2 ?y2 ?ax ?ay ?ax2 ?ay2) 
                               (on-edge? ?x2 ?y2 ?ax ?ay ?ax2 ?ay2)))))
           :in base :collect (push (list ?id :connects-to ?id2) base))
  base)
```

That's step one. We need to call out the lines that connect to particular constraint arguments, top-level connections and constants because this is going to affect what constraints will be informing which connections. A simple `:connects-to` fact is sufficient for our purposes.

```lisp
(defmethod label-line-connections ((base list))
  "Some lines connect to other lines. Connecting lines will be a single connection rather than multiple connections."
  (for-all (and (?id :line-segment nil)
                (?id :start ?start) (?id :end ?end)
                (?id2 :line-segment nil)
                (lisp (not (equal ?id ?id2)))
                (or (?id2 :start ?end) (?id2 :end ?start)
                    (?id2 :start ?start) (?id2 :end ?end)))
           :in base :do (push (list ?id :line-connects-to-line ?id2) base))
  base)
```

Next up, `label-line-connections` is going to make the relationships between line clusters explicit. Taking a look at our example diagram, most connections are composed of a single line-segment, but some are clusters of multiple segments. In particular, the constant `9` and the constant `5` are connected to other arguments by three-segment clusters. Finally, the last piece we synthesize, and also the most complex is

```lisp
(defmethod label-line-cluster ((base list))
  (let ((res (make-hash-table :test 'equal)))
    (for-all (and (?id :line-segment nil)
                  (not (?cluster :contains ?id)))
             :in base
             :do (let ((cluster (sort (walk-segment-graph base ?id) #'string< :key #'symbol-name)))
                   (setf (gethash cluster res) t)))
    (loop for clst being the hash-keys of res
       do (let ((id (intern (symbol-name (gensym)))))
            (push (list id :cluster nil) base)
            (loop for elem in clst 
               do (push (list id :contains elem) base))))
    base))
```

In this phase, we annotate the clusters. Which means we need to go through the list of all line-segments, figure out which cluster they belong to and add facts regarding this information to our base. We can't just create a new cluster per origin point, because some lines might belong to the *same* cluser, so we use `walk-segment-graph` to figure out the list of all line-segments reachable by walking from a particular one. Then we essentially use a `hash-table` as a `set` to make sure we've only got unique clusters, and finally we insert relevant facts into the base. Because it's a crucial step, lets also go through `walk-segment-graph`

```lisp
(defun walk-segment-graph (base src &key (explored (list src)))
  (for-all `(and (or (,src :line-connects-to-line ?id)
                     (?id :line-connects-to-line ,src))
                 (lisp (not (member ?id (list ,@explored)))))
           :in base
           :do (unless (member ?id explored)
                 (push ?id explored)
                 (setf explored (walk-segment-graph base ?id :explored explored))))
  explored)
```

Basically, we find immediately connecting lines to the origin, then recursively walk each of them, adding intermediate results to `explored` so we don't re-traverse large parts of the graph. Eventually, we return the list of explored elements.

That does it for `synthesize`. Next up, we go through a generation step.

```lisp
(defmethod generate-code (target (base list))
  (error "Unsupported target: ~s" target))

(defmethod generate-code ((target (eql :repl)) (base list))
  (generate-repl-app base))
```

Again, there's only one method here. I'm leaving in room for expansion to other compilation targets, but we're currently only generating `:repl` apps. And we're doing it by directly calling `generate-repl-app`.

```lisp
(defmethod generate-repl-app ((base list))
  (let* ((component-name (get-component-name base))
         (top-level-names (get-top-level-names base))
         (top-level-global-vars
          (mapcar (lambda (v) (intern (format nil "*~a*" v))) 
                  top-level-names)))
    `(progn ,@(loop for v in top-level-global-vars 
                 for n in top-level-names
                 collect `(defvar ,v (make-connector))
                 collect `(make-probe ,n ,v))
            ,(generate-component-factory 
              component-name (mapcar #'intern top-level-names) base)
            (,component-name ,@top-level-global-vars))))
```

Which uses `component-name`, and a couple transformations of `top-level-names` to generate that top-level form we saw in the demo earlier. In fact, you should be able to clearly see the correspondence between the backquoted form in `generate-repl-app` and the final output. Just to refresh your memory, that was:

```lisp
SICP-CONSTRAINTS> (compile-diagram :repl #p"celsius-to-faranheit.base")
(PROGN
 (DEFVAR *FARANHEIT* (MAKE-CONNECTOR))
 (MAKE-PROBE "FARANHEIT" *FARANHEIT*)
 (DEFVAR *CELSIUS* (MAKE-CONNECTOR))
 (MAKE-PROBE "CELSIUS" *CELSIUS*)
 (DEFUN CELSIUS-TO-FARANHEIT (FARANHEIT CELSIUS)
   (LET ((G1475 (MAKE-CONNECTOR))
         (G1474 (MAKE-CONNECTOR))
         (G1473 (MAKE-CONNECTOR))
         (G1470 (MAKE-CONNECTOR))
         (G1469 (MAKE-CONNECTOR)))
     (MAKE-MULTIPLIER :M1 CELSIUS :M2 G1470 :PRODUCT G1475)
     (MAKE-MULTIPLIER :PRODUCT G1475 :M2 G1469 :M1 G1474)
     (MAKE-ADDER :SUM FARANHEIT :A1 G1474 :A2 G1473)
     (MAKE-CONSTANT 5 G1469)
     (MAKE-CONSTANT 32 G1473)
     (MAKE-CONSTANT 9 G1470)
     'OK))
 (CELSIUS-TO-FARANHEIT *FARANHEIT* *CELSIUS*))
SICP-CONSTRAINTS>
```

You can trivially see the `loop` that generates the

```lisp
...
 (DEFVAR *FARANHEIT* (MAKE-CONNECTOR))
 (MAKE-PROBE "FARANHEIT" *FARANHEIT*)
 (DEFVAR *CELSIUS* (MAKE-CONNECTOR))
 (MAKE-PROBE "CELSIUS" *CELSIUS*)
...
```

It traverses the `top-level-global-vars` (which is just the upcased, earmuffed version of the `top-level-names` list) and collects the `defvar`/`make-probe` lines for each of them. The next part is a bit impenetrably named if you're not familiar with the terminology. A `component` is a specific instance of a constraint between some number of connectors. A `factory` of such components is a procedure that takes the appropriate number of connectors, and instantiates the necessary constraints between them, so creating that instance we just mentioned. The specific factory we're going to be generating code for is going to need to know the `component-name`, and have a list of `top-level-names`, and it'll need to tear a few more things out of `base` to really do its job. Lets take a look

```lisp
(defmethod generate-component-factory ((component-name symbol) (args list) (base list))
  `(defun ,component-name ,args
     (let ,(generate-internal-connections base)
       ,@(generate-constraints base)
       ,@(generate-constants base)
       'ok)))
```

It generates a `defun`, with the `component-name` in the function name position and the list of `top-level-names` as arguments. Which in our case translates to

```lisp
 (DEFUN CELSIUS-TO-FARANHEIT (FARANHEIT CELSIUS)
   ...)
```

Inside of that `defun`, it sets up a `let` with the internal connector definitions set up. `generate-internal-constraints` does the heavy lifting here...

```lisp
(defmethod generate-internal-connections ((base list))
  (for-all (and (?id :cluster nil)
                (not (and (?id :contains ?line)
                          (?line :connects-to ?elem)
                          (?elem :sicp-top-connection nil))))
           :in base :collect `(,?id (make-connector))))
```

...by finding the list of clusters that `:contains` no lines which connect to `:sicp-top-connection` elements. Those are going to be dealt with externally, and we're only interested in *internal* connections here. For the diagram we're currently compiling, `generate-internal-connections` will return something like 

```lisp
((G1475 (MAKE-CONNECTOR))
 (G1474 (MAKE-CONNECTOR))
 (G1473 (MAKE-CONNECTOR))
 (G1470 (MAKE-CONNECTOR))
 (G1469 (MAKE-CONNECTOR)))
```

"something like" because the actual symbol names on the left there will be different each time. We don't care what they're called though; we only care that they're consistent, and they will be because of how we're dealing with them. Our top-level return value of `generate-component-factory` is now up to

```lisp
 (DEFUN CELSIUS-TO-FARANHEIT (FARANHEIT CELSIUS)
   (LET ((G1475 (MAKE-CONNECTOR))
         (G1474 (MAKE-CONNECTOR))
         (G1473 (MAKE-CONNECTOR))
         (G1470 (MAKE-CONNECTOR))
         (G1469 (MAKE-CONNECTOR)))
     ...
     'ok))
```

The `generate-constraints` call is the thing that generates terms to instantiate primitive connectors.

```lisp
(defmethod generate-constraints ((base list))
  (for-all (and (?id :sicp-constraint nil)
                (?id :constraint-of ?txt)
                (?txt :sicp-constraint-name nil)
                (?txt :contents ?name))
           :in base
           :collect (case (intern (sanitize ?name) :keyword)
                      (:* `(make-multiplier ,@(generate-arguments base ?id)))
                      (:+ `(make-adder ,@(generate-arguments base ?id))))))
```

For the moment, it only deals with `*` and `+` constraints, but this is the method that would take care of instantiating other diagrams if we were going the hierarchical route. Specifically, there'd be an `else` clause on that `case` statement that would figure things out<a name="note-Sun-Jun-29-121144EDT-2014"></a>[|7|](#foot-Sun-Jun-29-121144EDT-2014). Sorry, got off topic for a moment there; `generate-constraints` takes a `base`, gets its `name` element and uses the contents do decide what specific factory form to generate. Either way, we call `generate-arguments` to get the argument terms out. In fact, now that I think about it, it would have been easy to write that `:collect` clause as

```lisp
...
:collect `(,(case (intern (sanitize ?name) :keyword)
                     (:* make-multiplier)
                     (:+ make-adder))
               ,@(generate-arguments base ?id))
...
```

In fact, note to self. `generate-arguments` itself does the obvious thing.

```lisp
(defmethod generate-arguments ((base list) constraint)
  (append (generate-top-arguments base constraint)
          (generate-internal-arguments base constraint)))
```

Or rather, it combines the results of two methods that do the obvious things.

```lisp
(defmethod generate-internal-arguments ((base list) constraint)
  (let ((res))
    (for-all `(and (,constraint :constraint-of ?txt)
                   (?txt :sicp-argument nil)
                   (?txt :contents ?arg-name)
                   (?line :connects-to ?txt)
                   (?cluster :contains ?line)
                   (not (and (?cluster :contains ?line2)
                             (?line2 :connects-to ?top)
                             (?top :sicp-top-connection nil))))
             :in base 
             :do (progn (push ?cluster res)
                        (push (intern (sanitize ?arg-name) :keyword) res)))
    res))

(defmethod generate-top-arguments ((base list) constraint)
  (let ((res))
    (for-all `(and (,constraint :constraint-of ?txt)
                   (?txt :sicp-argument nil)
                   (?txt :contents ?arg-name)
                   (?line :connects-to ?txt)
                   (?line :connects-to ?top)
                   (?top :sicp-top-connection nil)
                   (?top :contents ?top-name))
             :in base :do (progn (push (intern (sanitize ?top-name)) res)
                                 (push (intern (sanitize ?arg-name) :keyword) res)))
    res))
```

`generate-internal-arguments` is collecting all the arguments that will be bound to local connections. `generate-top-arguments` is collecting all the ones that will be bound to top connections. As an example, for the first `multiplier` constraint, `generate-arguments` will return `'(:M1 CELSIUS :M2 G1470 :PRODUCT G1475)`, because its `m1` argument is fed by the top-level `celsius` connector, whereas its `m2` and `product` arguments are fed by internal connections.

Popping up one additional level, in our particular case, `generate-constraints` will return three forms; instantiation forms for two `multiplier`s and one `adder`. At this point, our top level return value is up to


```lisp
 (DEFUN CELSIUS-TO-FARANHEIT (FARANHEIT CELSIUS)
   (LET ((G1475 (MAKE-CONNECTOR))
         (G1474 (MAKE-CONNECTOR))
         (G1473 (MAKE-CONNECTOR))
         (G1470 (MAKE-CONNECTOR))
         (G1469 (MAKE-CONNECTOR)))
     (MAKE-MULTIPLIER :M1 CELSIUS :M2 G1470 :PRODUCT G1475)
     (MAKE-MULTIPLIER :PRODUCT G1475 :M2 G1469 :M1 G1474)
     (MAKE-ADDER :SUM FARANHEIT :A1 G1474 :A2 G1473)
     ...
     'ok))
```

The last piece of the factory will set up any `constant` constraints it needs.

```lisp
(defmethod generate-constants ((base list))
  (for-all (and (?id :sicp-constant nil)
                (?id :value ?const)
                (?line :connects-to ?id)
                (?cluster :contains ?line))
           :in base :collect `(make-constant ,?const ,?cluster)))
```

This one's pretty simple because we already generated facts that specify the parsed form of a constants' input back in the `synthesize` step. All we need to do is collect `make-constant` forms that take that parsed value and the appropriate `connector` as arguments. That completes factory term.

```lisp
 (DEFUN CELSIUS-TO-FARANHEIT (FARANHEIT CELSIUS)
   (LET ((G1475 (MAKE-CONNECTOR))
         (G1474 (MAKE-CONNECTOR))
         (G1473 (MAKE-CONNECTOR))
         (G1470 (MAKE-CONNECTOR))
         (G1469 (MAKE-CONNECTOR)))
     (MAKE-MULTIPLIER :M1 CELSIUS :M2 G1470 :PRODUCT G1475)
     (MAKE-MULTIPLIER :PRODUCT G1475 :M2 G1469 :M1 G1474)
     (MAKE-ADDER :SUM FARANHEIT :A1 G1474 :A2 G1473)
     (MAKE-CONSTANT 5 G1469)
     (MAKE-CONSTANT 32 G1473)
     (MAKE-CONSTANT 9 G1470)
     'OK))
```

The very last thing our compiler needs to do is generate the term that will make call this factory to instantiate our system, feeding it its top-level `connector`s. And that happens in the last line of `generate-repl-app`

```lisp
...
 (,component-name ,@top-level-global-vars))))
```

That takes the name of our component (which we also used as the name for our factory) and puts it at the beginning of a list form into which it splices `top-level-global-vars` (which we used as the names for our top-level `connector`s).

That's that.

You now fully understand how this happens:

```lisp
SICP-CONSTRAINTS> (compile-diagram :repl #p"celsius-to-faranheit.base")
(PROGN
 (DEFVAR *FARANHEIT* (MAKE-CONNECTOR))
 (MAKE-PROBE "FARANHEIT" *FARANHEIT*)
 (DEFVAR *CELSIUS* (MAKE-CONNECTOR))
 (MAKE-PROBE "CELSIUS" *CELSIUS*)
 (DEFUN CELSIUS-TO-FARANHEIT (FARANHEIT CELSIUS)
   (LET ((G1475 (MAKE-CONNECTOR))
         (G1474 (MAKE-CONNECTOR))
         (G1473 (MAKE-CONNECTOR))
         (G1470 (MAKE-CONNECTOR))
         (G1469 (MAKE-CONNECTOR)))
     (MAKE-MULTIPLIER :M1 CELSIUS :M2 G1470 :PRODUCT G1475)
     (MAKE-MULTIPLIER :PRODUCT G1475 :M2 G1469 :M1 G1474)
     (MAKE-ADDER :SUM FARANHEIT :A1 G1474 :A2 G1473)
     (MAKE-CONSTANT 5 G1469)
     (MAKE-CONSTANT 32 G1473)
     (MAKE-CONSTANT 9 G1470)
     'OK))
 (CELSIUS-TO-FARANHEIT *FARANHEIT* *CELSIUS*))
SICP-CONSTRAINTS>
```

Hopefully, you can imagine some places where compiling your "documentation" images like this might be a worth-while approach. And now have the cognitive tools to go do something about it.

* * *
##### Footnotes

1 - <a name="foot-Sun-Jun-29-120930EDT-2014"></a>[|back|](#note-Sun-Jun-29-120930EDT-2014) - Because the code we run is generated *from* the documentation, so it by definition reflects the running system.

2 - <a name="foot-Sun-Jun-29-120935EDT-2014"></a>[|back|](#note-Sun-Jun-29-120935EDT-2014) - Bar one or two I'm thinking about introducing while I play with the concept.

3 - <a name="foot-Sun-Jun-29-120943EDT-2014"></a>[|back|](#note-Sun-Jun-29-120943EDT-2014) - In the sense that we don't have to properly order keyword args, whereas regular arguments have to be in a particular order that might not reflect our visual representation.

4 - <a name="foot-Sun-Jun-29-120959EDT-2014"></a>[|back|](#note-Sun-Jun-29-120959EDT-2014) - I'm thinking ahead here. Eventually, we'll want compilation targets other than REPL apps, so I'm just building room for expansion into the initial design. One option I've already thought about is a compilation target called `:web` that'll generate a [`house`](https://github.com/Inaimathi/house) handler along with that constraint system to let you play around in-browser rather than just in-repl.

5 - <a name="foot-Sun-Jun-29-121018EDT-2014"></a>[|back|](#note-Sun-Jun-29-121018EDT-2014) - As it happens, there's only one right now; `:repl`, which will create the in-repl application that I demoed earlier.

6 - <a name="foot-Sun-Jun-29-121112EDT-2014"></a>[|back|](#note-Sun-Jun-29-121112EDT-2014) - It's simpler than you might think, since I'm only concerning myself with horizontal and vertical lines.

7 - <a name="foot-Sun-Jun-29-121144EDT-2014"></a>[|back|](#note-Sun-Jun-29-121144EDT-2014) - Note that we'd need to make a change or two to our `defun` emitter too, since at that point we'd need to emit keyword arguments there too.
