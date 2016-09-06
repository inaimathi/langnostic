So [here's](https://github.com/Inaimathi/cl-pronounce) something stupid I built, because fuck sleep.

## The Common Lisp `format` procedure...

![The format function, typeset in the styel of a beer-bottle, being used to format the full 99 bottles song](/static/img/format-fear.jpg)

...is kind of weird. By my estimation it's either the most or second-most complicated construct you're likely to use in even casual CL programming (the other potential being [`loop`](http://clhs.lisp.se/Body/m_loop.htm)). One of the [many](http://www.gigamonkeys.com/book/a-few-format-recipes.html), [*many*](http://www.lispworks.com/documentation/lw51/CLHS/Body/22_c.htm) things it can do is take a number, and give you its verbal English representation. That's the `~r` format directive. Last night, at the [Toronto Haskell User Group](https://groups.google.com/forum/#!forum/toronto-haskell), I was about a quarter joking about turning this into an actual service. It soon occurred to me that this could also turn out to be a simple demo application for [`cl-handlers`](https://github.com/Inaimathi/cl-handlers).

So, on the train home, I got to work.

## Writing `pronounce`

```lisp
;;;; cl-pronounce.asd
(asdf:defsystem #:cl-pronounce
  :description "Describe cl-pronounce here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-handlers #:woo #:alexandria)
  :components ((:file "package")
               (:file "util")
               (:file "cl-pronounce")))
```

```lisp
;;;; package.lisp

(defpackage #:cl-pronounce
  (:use #:cl #:cl-handlers)
  (:export #:start))
```

Nothing to see here, just the project and package declaration. Haven't even filled in most of the details yet.

```lisp
;;;; cl-pronounce.lisp
(in-package #:cl-pronounce)

(define-handler (v1/integer/-num=integer) ()
  (format nil "~r" num))

(define-handler (v1/ratio/-num=integer/-denom=integer) ()
  (format nil "~r over ~r" num denom))

(define-handler (v1/float/-num=string) ()
  (multiple-value-bind (int off) (parse-integer num :junk-allowed t)
    (format nil "~r point ~{~r~^ ~}"
	    int (loop for i from (+ off 1) while (> (length num) i)
		   collect (parse-integer num :start i :end (+ i 1))))))

(define-handler (v1/magic/-num=string) ()
  (multiple-value-bind (int off) (parse-integer num :junk-allowed t)
    (cond ((= (length num) off)
	   (format nil "~r" int))
	  ((and (eql #\. (char num off))
		(const-ish? num +pi-str+))
	   (const num "pi"))
	  ((and (eql #\. (char num off))
		(const-ish? num +tau-str+))
	   (const num "tau"))
	  ((eql #\. (char num off))
	   (format nil "~r point ~{~r~^ ~}"
		   int (loop for i from (+ off 1) while (> (length num) i)
			  collect (parse-integer num :start i :end (+ i 1))))))))

(define-handler (v1/magic/-num=integer/-denom=integer) ()
  (format nil "~r over ~r" num denom))

(defun start (&key (port 5000))
  (woo:run (make-app) :port port))
```

That's the "meat" of the project, though you'll definitely find this light fare if you've been to this blog before. What we've got is a few simple handlers and a more complicated one.

```lisp
(define-handler (v1/integer/-num=integer) ()
  (format nil "~r" num))
```

The very simplest form defines a handler at `v1/integer/<something>` that expects the `<something>` to be an integer, and just makes the appropriate `format` call. If `<something>` is *not* an integer, the caller will get a `400` error. Also, if the parsed integer is very large, they'll get a `500` error. If you happen to be watching at the REPL, you'll see a reasonably comical condition get raised.

```lisp
CL-USER> (format nil "~r" 999999999999999999999999999999999999999999999999999999999999999999)
"nine hundred ninety-nine vigintillion nine hundred ninety-nine novemdecillion nine hundred ninety-nine octodecillion nine hundred ninety-nine septendecillion nine hundred ninety-nine sexdecillion nine hundred ninety-nine quindecillion nine hundred ninety-nine quattuordecillion nine hundred ninety-nine tredecillion nine hundred ninety-nine duodecillion nine hundred ninety-nine undecillion nine hundred ninety-nine decillion nine hundred ninety-nine nonillion nine hundred ninety-nine octillion nine hundred ninety-nine septillion nine hundred ninety-nine sextillion nine hundred ninety-nine quintillion nine hundred ninety-nine quadrillion nine hundred ninety-nine trillion nine hundred ninety-nine billion nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine"

CL-USER> (format nil "~r" (+ 999999999999999999999999999999999999999999999999999999999999999999 1))

number too large to print in English: 1,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] Abort thread (#<THREAD "repl-thread" RUNNING {10048E8083}>)

Backtrace:
  0: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1 22 1000000000000000000000000000000000000000000000000000000000000000000)
  1: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000 21 1000000000000000000000000000000000000000000000000000000000000000000)
  2: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000 20 1000000000000000000000000000000000000000000000000000000000000000000)
  3: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000 19 1000000000000000000000000000000000000000000000000000000000000000000)
  4: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000 18 1000000000000000000000000000000000000000000000000000000000000000000)
  5: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000 17 1000000000000000000000000000000000000000000000000000000000000000000)
  6: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000 16 1000000000000000000000000000000000000000000000000000000000000000000)
  7: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000 15 1000000000000000000000000000000000000000000000000000000000000000000)
  8: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000 14 1000000000000000000000000000000000000000000000000000000000000000000)
  9: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000 13 1000000000000000000000000000000000000000000000000000000000000000000)
 10: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000 12 1000000000000000000000000000000000000000000000000000000000000000000)
 11: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000 11 1000000000000000000000000000000000000000000000000000000000000000000)
 12: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000 10 1000000000000000000000000000000000000000000000000000000000000000000)
 13: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000 9 1000000000000000000000000000000000000000000000000000000000000000000)
 14: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000000 8 1000000000000000000000000000000000000000000000000000000000000000000)
 15: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000000000 7 1000000000000000000000000000000000000000000000000000000000000000000..
 16: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000000000000 6 1000000000000000000000000000000000000000000000000000000000000000..
 17: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000000000000000 5 1000000000000000000000000000000000000000000000000000000000000..
 18: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000000000000000000 4 1000000000000000000000000000000000000000000000000000000000..
 19: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000000000000000000000 3 1000000000000000000000000000000000000000000000000000000..
 20: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000000000000000000000000 2 1000000000000000000000000000000000000000000000000000..
 21: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000000000000000000000000000 1 1000000000000000000000000000000000000000000000000..
 22: (SB-FORMAT::FORMAT-PRINT-CARDINAL-AUX #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> 1000000000000000000000000000000000000000000000000000000000000000000 0 1000000000000000000000000000000000000000000000..
 23: (SB-FORMAT::R-FORMAT-DIRECTIVE-INTERPRETER #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> #<~r> NIL #<unavailable argument> #<unavailable argument>)
 24: (SB-FORMAT::INTERPRET-DIRECTIVE-LIST #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> (#<~r>) (1000000000000000000000000000000000000000000000000000000000000000000) (10000000000000000000000000000000000000..
 25: (SB-FORMAT::%FORMAT #<SB-IMPL::STRING-OUTPUT-STREAM {1005375643}> "~r" (1000000000000000000000000000000000000000000000000000000000000000000) (1000000000000000000000000000000000000000000000000000000000..
 26: (FORMAT NIL "~r" 1000000000000000000000000000000000000000000000000000000000000000000)
 27: (SB-INT:SIMPLE-EVAL-IN-LEXENV (FORMAT NIL "~r" (+ 999999999999999999999999999999999999999999999999999999999999999999 1)) #<NULL-LEXENV>)
 28: (EVAL (FORMAT NIL "~r" (+ 999999999999999999999999999999999999999999999999999999999999999999 1)))
 29: (SWANK::EVAL-REGION "(format nil \"~r\" (+ 999999999999999999999999999999999999999999999999999999999999999999 1)) ..)
 30: ((LAMBDA NIL :IN SWANK-REPL::REPL-EVAL))
 31: (SWANK-REPL::TRACK-PACKAGE #<CLOSURE (LAMBDA NIL :IN SWANK-REPL::REPL-EVAL) {1005374CAB}>)
 32: (SWANK::CALL-WITH-RETRY-RESTART "Retry SLIME REPL evaluation request." #<CLOSURE (LAMBDA NIL :IN SWANK-REPL::REPL-EVAL) {1005374BEB}>)
 33: (SWANK::CALL-WITH-BUFFER-SYNTAX NIL #<CLOSURE (LAMBDA NIL :IN SWANK-REPL::REPL-EVAL) {1005374BCB}>)
 34: (SWANK-REPL::REPL-EVAL "(format nil \"~r\" (+ 999999999999999999999999999999999999999999999999999999999999999999 1)) ..)
 35: (SB-INT:SIMPLE-EVAL-IN-LEXENV (SWANK-REPL:LISTENER-EVAL "(format nil \"~r\" (+ 999999999999999999999999999999999999999999999999999999999999999999 1)) ..)
 36: (EVAL (SWANK-REPL:LISTENER-EVAL "(format nil \"~r\" (+ 999999999999999999999999999999999999999999999999999999999999999999 1)) ..)
 37: (SWANK:EVAL-FOR-EMACS (SWANK-REPL:LISTENER-EVAL "(format nil \"~r\" (+ 999999999999999999999999999999999999999999999999999999999999999999 1)) ..)
 38: (SWANK::PROCESS-REQUESTS NIL)
 39: ((LAMBDA NIL :IN SWANK::HANDLE-REQUESTS))
 40: ((LAMBDA NIL :IN SWANK::HANDLE-REQUESTS))
 41: (SWANK/SBCL::CALL-WITH-BREAK-HOOK #<FUNCTION SWANK:SWANK-DEBUGGER-HOOK> #<CLOSURE (LAMBDA NIL :IN SWANK::HANDLE-REQUESTS) {10048F00EB}>)
 42: ((FLET SWANK/BACKEND:CALL-WITH-DEBUGGER-HOOK :IN "/home/inaimathi/quicklisp/dists/quicklisp/software/slime-2.12/swank/sbcl.lisp") #<FUNCTION SWANK:SWANK-DEBUGGER-HOOK> #<CLOSURE (LAMBDA NIL :IN SWANK:..
 43: (SWANK::CALL-WITH-BINDINGS ((*STANDARD-OUTPUT* . #1=#<SWANK/GRAY::SLIME-OUTPUT-STREAM {10048CFEF3}>) (*STANDARD-INPUT* . #2=#<SWANK/GRAY::SLIME-INPUT-STREAM {1002AF8073}>) (*TRACE-OUTPUT* . #1#) (*ERR..
 44: (SWANK::HANDLE-REQUESTS #<SWANK::MULTITHREADED-CONNECTION {1003A544D3}> NIL)
 45: ((FLET #:WITHOUT-INTERRUPTS-BODY-1117 :IN SB-THREAD::INITIAL-THREAD-FUNCTION-TRAMPOLINE))
 46: ((FLET SB-THREAD::WITH-MUTEX-THUNK :IN SB-THREAD::INITIAL-THREAD-FUNCTION-TRAMPOLINE))
 47: ((FLET #:WITHOUT-INTERRUPTS-BODY-537 :IN SB-THREAD::CALL-WITH-MUTEX))
 48: (SB-THREAD::CALL-WITH-MUTEX #<CLOSURE (FLET SB-THREAD::WITH-MUTEX-THUNK :IN SB-THREAD::INITIAL-THREAD-FUNCTION-TRAMPOLINE) {7FFFF488EC6B}> #<SB-THREAD:MUTEX "thread result lock" owner: #<SB-THREAD:THR..
 49: (SB-THREAD::INITIAL-THREAD-FUNCTION-TRAMPOLINE #<SB-THREAD:THREAD "repl-thread" RUNNING {10048E8083}> #S(SB-THREAD:SEMAPHORE :NAME "Thread setup semaphore" :%COUNT 0 :WAITCOUNT 0 :MUTEX #<SB-THREAD:MU..
 50: ("foreign function: call_into_lisp")
 51: ("foreign function: new_thread_trampoline")
```

Really, I should do something about this in a future version, but haven't bothered with it yet. Note to self.

Next up,

```lisp
(define-handler (v1/ratio/-num=integer/-denom=integer) ()
  (format nil "~r over ~r" num denom))

(define-handler (v1/float/-num=string) ()
  (multiple-value-bind (int off) (parse-integer num :junk-allowed t)
    (format nil "~r point ~{~r~^ ~}"
	    int (loop for i from (+ off 1) while (> (length num) i)
		   collect (parse-integer num :start i :end (+ i 1))))))
```

Two specialized handlers for `ratio`s and `float`s. The default `format` directive handles neither of these cases, raising the appropriate type condition in each case, so a little manual intervention is required. For ratios, we take two path components, `num` and `denom`, and make the trivial call with each of them. We're punning the URL path separator to make the input readable.

Floats work a little differently. We need to get their integer part and call the trivial thing, followed by "point", followed by each individual digit in the decimal part of the number.

The last handler is pretty complicated.

```lisp
(define-handler (v1/magic/-num=string) ()
  (multiple-value-bind (int off) (parse-integer num :junk-allowed t)
    (cond ((= (length num) off)
	   (format nil "~r" int))
	  ((and (eql #\. (char num off))
		(const-ish? num +pi-str+))
	   (const num "pi"))
	  ((and (eql #\. (char num off))
		(const-ish? num +tau-str+))
	   (const num "tau"))
	  ((eql #\. (char num off))
	   (format nil "~r point ~{~r~^ ~}"
		   int (loop for i from (+ off 1) while (> (length num) i)
			  collect (parse-integer num :start i :end (+ i 1))))))))

(define-handler (v1/magic/-num=integer/-denom=integer) ()
  (format nil "~r over ~r" num denom))
```

And is, in fact, split across *two* handlers. One is a straight up copy of the `ratio` handler, with a different name, just so that we can take advantage of the URL-separator/division pun to make it look like `v1/magic` takes anything and returns the appropriate pronounciation. The primary handler expects its parameter to be passed as a string, so no parsing or validation is done on it. This means that there should technically be a final `(t ...)` clause that throws a `400` error at the end of that `cond` there. That's a note to my future self.

The first clause, handles the case where we get an `integer`, the (currently) last handles the case where we get a `float`. The `ratio` case is handled by the separate mini handler. The two middle clauses, and I may add more here at some point, handle the cases of two common numerical constants. They do it by calling out to `const-ish?` and `const`, as well as referring to `+pi-str+` and `+tau-str+`. All of those are defined over in

```lisp
;;; util.lisp

(in-package :cl-pronounce)

(defparameter +tau-str+
  "6.283185307179586476925286766559005768394338798750211641949889184615632812572417997256069650684234135964296173026564613294187689219101164463450718816256962234900568205403877042211119289245897909860763928857621951331866892256951296467573566330542403818291297133846920697220908653296426787214520498282547449174013212631176349763041841925658508183430728735785180720022661061097640933042768293903883023218866114540731519183906184372234763865223586210237096148924759925499134703771505449782455876366023898259667346724881313286172042789892790449474381404359721887405541078434352586353504769349636935338810264001136254290527121655571542685515579218347274357442936881802449906860293099170742101584559378517847084039912224258043921728068836319627259549542619921037414422699999996745956099902119463465632192637190048918910693816605285044616506689370070523862376342020006275677505773175066416762841234355338294607196506980857510937462319125727764707575187503915563715561064342453613226003855753222391818432840397876190514402130971726557731872306763655936460603904070603705937991547245198827782499443550566958263031149714484908301391901659066233723455711778150196763509274929878638510120801855403342278019697648025716723207127415320209420363885911192397893535674898896510759549453694208095069292416093368518138982586627354057978304209504324113932048116076300387022506764860071175280494992946527828398545208539845593564709563272018683443282439849172630060572365949111413499677010989177173853991381854421595018605910642330689974405511920472961330998239763669595507132739614853085055725103636835149345781955545587600163294120032290498384346434429544700282883947137096322722314705104266951483698936877046647814788286669095524833725037967138971124198438444368545100508513775343580989203306933609977254465583572171568767655935953362908201907767572721901360128450250410234785969792168256977253891208483930570044421322372613488557244078389890094247427573921912728743834574935529315147924827781731665291991626780956055180198931528157902538936796705191419651645241044978")

(defparameter +pi-str+
  "3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609433057270365759591953092186117381932611793105118548074462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132000568127145263560827785771342757789609173637178721468440901224953430146549585371050792279689258923542019956112129021960864034418159813629774771309960518707211349999998372978049951059731732816096318595024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303598253490428755468731159562863882353787593751957781857780532171226806613001927876611195909216420198938095257201065485863278865936153381827968230301952035301852968995773622599413891249721775283479131515574857242454150695950829533116861727855889075098381754637464939319255060400927701671139009848824012858361603563707660104710181942955596198946767837449448255379774726847104047534646208046684259069491293313677028989152104752162056966024058038150193511253382430035587640247496473263914199272604269922796782354781636009341721641219924586315030286182974555706749838505494588586926995690927210797509302955321165344987202755960236480665499119881834797753566369807426542527862551818417574672890977772793800081647060016145249192173217214772350141441973568548161361157352552133475741849468438523323907394143334547762416862518983569485562099219222184272550254256887671790494601653466804988627232791786085784383827967976681454100953883786360950680064225125205117392984896084128488626945604241965285022210661186306744278622039194945047123713786960956364371917287467764657573962413890865832645995813390478027590099465764078951269468398352595709825822620522489")

(defun const-ish? (num-str const)
  (if (eql #\- (char num-str 0))
      (and (>= (length num-str) 7)
	   (alexandria:starts-with-subseq (subseq num-str 1) const))
      (and (>= (length num-str) 6)
	   (alexandria:starts-with-subseq num-str const))))

(defun const (num name)
  (if (eql #\- (char num 0))
      (concatenate 'string "negative " name)
      name))
```

As you can see, `+pi-str+` and `+tau-str+` live up to their name. They're 2048-character string representations of the appropriate constant, and `const-ish?` just checks whether the incoming number is a prefix (or negative prefix) of the given constant. This means that we'll return odd things if we're passed a representation of `pi` or `tau` more accurate than 2048 digits. That's another note to future self.

That's basically the program.

The last bit in `cl-pronounce.lisp` just exposes a hook to start a server.

```lisp
(defun start (&key (port 5000))
  (woo:run (make-app) :port port))
```


## Installing `pronounce`

Installing the thing on my server turned out to be a bit more difficult than I expected. Firstly, I've got a ridiculously old version of `sbcl` there. So obviously, that had to go, and a new one had to come in. Secondly, for whatever reason, `woo` can't pick up my `libev.so.4`. Presumably it's in the wrong place, even after a fresh installation of `libev-dev`. Apparently, `wookie` has exactly the same problem, so no help there. However, instead of spending half an hour or so poking around, trying to put symlinks in the right place, I decided to take advantage of one of the stated goals of `cl-handlers`.

```
diff --git a/cl-pronounce.asd b/cl-pronounce.asd
index 92d2bb4..db57c96 100644
--- a/cl-pronounce.asd
+++ b/cl-pronounce.asd
@@ -5,7 +5,7 @@
   :author "Your Name <your.name@example.com>"
   :license "Specify license here"
   :serial t
-  :depends-on (#:cl-handlers #:woo #:alexandria)
+  :depends-on (#:cl-handlers #:clack #:hunchentoot #:alexandria)
   :components ((:file "package")
 	       (:file "util")
                (:file "cl-pronounce")))
```

```
diff --git a/cl-pronounce.lisp b/cl-pronounce.lisp
index 5982d52..b93a6a5 100644
--- a/cl-pronounce.lisp
+++ b/cl-pronounce.lisp
@@ -32,5 +32,5 @@
   (format nil "~r over ~r" num denom))
 
 (defun start (&key (port 5000))
-  (woo:run (make-app) :port port))
+  (clack:clackup (make-app) :server :hunchentoot :port port))
```

And suddenly, we're serving from `hunchentoot` instead. Granted, `hunchentoot` failed to find my local `ssl` library, but I'm not running an `https` server in any case (and would probably leave that up to `nginx` if I were).

Anyhow, as a result of this rigmarole, you can now hit an instance of the `cl-pronounce` service at [`pronounce.inaimathi.ca`](http://pronounce.inaimathi.ca/v1/magic/-1238768). I'll add a help page at some point, I swear. In the meantime, kindly refer to the [`README.md`](https://github.com/Inaimathi/cl-pronounce#cl-pronounce)
