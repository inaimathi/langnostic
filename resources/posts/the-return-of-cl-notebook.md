I haven't _just_ been directing my personal hacking/thinking time at massively distributed cellular automata. Remember when I said I was planning to put a bunch more work into [`cl-notebook`](https://github.com/inaimathi/cl-notebook)[^also-i-feel-old] when I had time? Well, guess what motherfuckers; we're doing this live.

[^also-i-feel-old]: And I just noticed that I started it in 2014, which puts it at 4 years old. Which means that I have both a child younger _and_ a child older than this particular attempt at a cell-based editor. I feel fucking old.

I recently served on a contract for which my team had to use [Jupyter Notebook](http://jupyter.org/), and I was pretty severely underwhelmed. I mean, the front-end has a level of polish you'd rightly never expect from a [one-dev affair](https://github.com/inaimathi/cl-notebook), and it has better cross-platform support than what I'm pulling off at the moment. But in terms of the big problems that I was tackling in the Common Lisp analog, I'm quite surprised to be ahead of the curve, even after having taken four years "off" development.

The things I was starting to think about were large problems like self-hosting, the implications of multi-user editing and how to best accomodate it, intelligent full-notebook loading and use on mobile devices. Not to mention the full-history system that I'd already built most of.

As far as I can tell, Jupyter has punted on all of the above.

So as much as I'd like to, I can't declare the situation Good Enough that I can in good conscience stop working on `cl-notebook`, even if the underlying languages were equivalent. Given that situation, I've been looking at the codebase and thinking about how I'd go about getting it from where it is now to being a worthy competitor to Jupyter in terms of ease-of-use. Or at the very least, to a state where I can easily start accepting pushes from other contributors.

And having pushed [a commit](https://github.com/inaimathi/cl-notebook/commit/be496750577ad37b80e9de15b5d44fc6868f0359) to include general-location notebook opening, I think I've satisfied the second requirement.

There's a bunch of additional stuff noted in the [READMEs TODO section](https://github.com/inaimathi/cl-notebook#todo-also-this-section-should-eventually-be-moved-to-the-github-issue-tracker), but the things that need to be settled before other people can credibly hack on this system have more or less been settled. I'd like to work out a system by which we can let notebooks materially change the client side of the system both through JavaScript and CSS additions, and I'm still debating whether we'd be better or worse off with a [`minibuffer`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer.html) added to the system, but those are relatively minor points that can be dealt with in a fairly self-contained way.

The next step for me is to start using it, and encourage other people to start contributing to the project.

## Side-note on [`:house`](https://github.com/inaimathi/house)

[`:house`](https://github.com/inaimathi/house) fucking sucks. It performs poorly, coming in somewhere between `hunchentoot` and `tornado` on the [`woo` benchmark graph](https://github.com/fukamachi/woo#how-fast), it doesn't deal with `https`, it doesn't easily support [`websocket`s](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API) and it's dumb in dealing with underlying system resources such as threads and static files.

And yet...

```
CL-USER> (ql:quickload :house)
To load "house":
  Load 1 ASDF system:
    house
; Loading "house"

(:HOUSE)
CL-USER>
```

... I can guarantee that'll be the effect of `quickload`ing `house` on any system where you can install `sbcl`, `clisp`, `lispworks`, `cmucl` or `ccl`. Regardless of what package manager you use, or what architecture you're on, or whether you're running a massive, 12-processor piece of heavy iron or a [RasPi](https://www.raspberrypi.org/), or something even more resource constrained.

After you `clone` `git@github.com:inaimathi/house.git` into your `local-projects` directory, it will load, and it will fucking work without issue. Granted, the competition is superior along every other axis, but...

```
CL-USER> (ql:quickload :woo)
To load "cffi-grovel":
  Load 1 ASDF system:
    cffi-grovel
; Loading "cffi-grovel"
.
To load "woo":
  Load 1 ASDF system:
    woo
; Loading "woo"
To load "clack-socket":
  Install 1 Quicklisp release:
    clack
; Fetching #<URL "http://beta.quicklisp.org/archive/clack/2017-06-30/clack-20170630-git.tgz">
; 188.65KB
==================================================
193,178 bytes in 1.00 seconds (188.84KB/sec)
; Loading "clack-socket"
[package clack.socket]
; Loading "woo"
To load "swap-bytes":
  Load 1 ASDF system:
    trivial-features
  Install 1 Quicklisp release:
    swap-bytes
; Fetching #<URL "http://beta.quicklisp.org/archive/swap-bytes/2016-09-29/swap-bytes-v1.1.tgz">
; 4.12KB
==================================================
4,223 bytes in 0.00 seconds (0.00KB/sec)
; Loading "swap-bytes"
[package swap-bytes].
; Loading "woo"
To load "trivial-utf-8":
  Install 1 Quicklisp release:
    trivial-utf-8
; Fetching #<URL "http://beta.quicklisp.org/archive/trivial-utf-8/2011-10-01/trivial-utf-8-20111001-darcs.tgz">
; 5.91KB
==================================================
6,055 bytes in 0.00 seconds (0.00KB/sec)
; Loading "trivial-utf-8"
[package trivial-utf-8].
; Loading "woo"
;
; compilation unit aborted
;   caught 2 fatal ERROR conditions
; Evaluation aborted on #<CFFI:LOAD-FOREIGN-LIBRARY-ERROR "Unable to load any of the alternatives:~%   ~S" {1002492F33}>.

Unable to load any of the alternatives:
   ("libev.4.dylib" "libev.4.so" "libev.so.4" "libev.dylib"
    "libev.so")
   [Condition of type CFFI:LOAD-FOREIGN-LIBRARY-ERROR]

Restarts:
 0: [RETRY] Try loading the foreign library again.
 1: [USE-VALUE] Use another library instead.
 2: [TRY-RECOMPILING] Recompile lev and try loading it again
 3: [RETRY] Retry loading FASL for #<CL-SOURCE-FILE "lev" "src" "lev">.
 4: [ACCEPT] Continue, treating loading FASL for #<CL-SOURCE-FILE "lev" "src" "lev"> as having been successful.
 5: [RETRY] Retry ASDF operation.
 --more--

Backtrace:
  0: (CFFI::FL-ERROR "Unable to load any of the alternatives:~%   ~S" ("libev.4.dylib" "libev.4.so" "libev.so.4" "libev.dylib" "libev.so"))
  1: (CFFI::TRY-FOREIGN-LIBRARY-ALTERNATIVES LEV::LIBEV ("libev.4.dylib" "libev.4.so" "libev.so.4" "libev.dylib" "libev.so") NIL)
  2: ((FLET CFFI::%DO-LOAD :IN CFFI::%DO-LOAD-FOREIGN-LIBRARY) #<CFFI:FOREIGN-LIBRARY LIBEV> LEV::LIBEV (:OR "libev.4.dylib" "libev.4.so" "libev.so.4" "libev.dylib" "libev.so"))
  3: (CFFI:LOAD-FOREIGN-LIBRARY LEV::LIBEV :SEARCH-PATH NIL)
  4: (SB-FASL::LOAD-FASL-GROUP #S(SB-FASL::FASL-INPUT :STREAM #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/l..
  5: (SB-FASL::LOAD-AS-FASL #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/lev-20150505-git/src/lev.fasl" {100..
  6: ((FLET SB-FASL::LOAD-STREAM :IN LOAD) #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/lev-20150505-git/src..
  7: (LOAD #P"/home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/lev-20150505-git/src/lev.fasl" :VERBOSE NIL :PRINT NIL :IF-DOES-NOT-EXIST T :E..
  8: (UIOP/UTILITY:CALL-WITH-MUFFLED-CONDITIONS #<CLOSURE (LAMBDA NIL :IN UIOP/LISP-BUILD:LOAD*) {100471F61B}> ("Overwriting already existing readtable ~S." #(#:FINALIZERS-OFF-WARNING :ASDF-FINALIZERS)))
  9: ((SB-PCL::EMF ASDF/ACTION:PERFORM) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "lev" "src" "lev">)
 10: ((:METHOD ASDF/ACTION:PERFORM-WITH-RESTARTS (ASDF/LISP-ACTION:LOAD-OP ASDF/LISP-ACTION:CL-SOURCE-FILE)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "lev" "src" "lev">) [fast-method]
 11: ((:METHOD ASDF/ACTION:PERFORM-WITH-RESTARTS :AROUND (T T)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "lev" "src" "lev">) [fast-method]
 12: ((:METHOD ASDF/PLAN:PERFORM-PLAN (LIST)) ((#<ASDF/LISP-ACTION:PREPARE-OP > . #1=#<ASDF/SYSTEM:SYSTEM "uiop">) (#2=#<ASDF/LISP-ACTION:COMPILE-OP > . #1#) (#3=#<ASDF/LISP-ACTION:LOAD-OP > . #1#) (#2# . ..
 13: ((FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT))
 14: ((:METHOD ASDF/PLAN:PERFORM-PLAN :AROUND (T)) ((#<ASDF/LISP-ACTION:PREPARE-OP > . #1=#<ASDF/SYSTEM:SYSTEM "uiop">) (#2=#<ASDF/LISP-ACTION:COMPILE-OP > . #1#) (#3=#<ASDF/LISP-ACTION:LOAD-OP > . #1#) (#..
 15: ((FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT))
 16: ((:METHOD ASDF/PLAN:PERFORM-PLAN :AROUND (T)) #<ASDF/PLAN:SEQUENTIAL-PLAN {1002A90153}> :VERBOSE NIL) [fast-method]
 17: ((:METHOD ASDF/OPERATE:OPERATE (ASDF/OPERATION:OPERATION ASDF/COMPONENT:COMPONENT)) #<ASDF/LISP-ACTION:LOAD-OP :VERBOSE NIL> #<ASDF/SYSTEM:SYSTEM "woo"> :VERBOSE NIL) [fast-method]
 18: ((SB-PCL::EMF ASDF/OPERATE:OPERATE) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:LOAD-OP :VERBOSE NIL> #<ASDF/SYSTEM:SYSTEM "woo"> :VERBOSE NIL)
 19: ((LAMBDA NIL :IN ASDF/OPERATE:OPERATE))
 --more--

; Evaluation aborted

CL-USER> (ql:quickload :hunchentoot)
To load "hunchentoot":
  Load 1 ASDF system:
    hunchentoot
; Loading "hunchentoot"
Unable to load any of the alternatives:
   ("libssl.so.1.0.2" "libssl.so.1.0.1l" "libssl.so.1.0.1e"
    "libssl.so.1.0.1j" "libssl.so.1.0.1" "libssl.so.1.0.0q"
    "libssl.so.1.0.0" "libssl.so.0.9.8ze" "libssl.so.0.9.8"
    "libssl.so" "libssl.so.4" "libssl.so.10")
   [Condition of type CFFI:LOAD-FOREIGN-LIBRARY-ERROR]

Restarts:
 0: [RETRY] Try loading the foreign library again.
 1: [USE-VALUE] Use another library instead.
 2: [TRY-RECOMPILING] Recompile reload and try loading it again
 3: [RETRY] Retry loading FASL for #<CL-SOURCE-FILE "cl+ssl" "src" "reload">.
 4: [ACCEPT] Continue, treating loading FASL for #<CL-SOURCE-FILE "cl+ssl" "src" "reload"> as having been successful.
 5: [RETRY] Retry ASDF operation.
 --more--

Backtrace:
  0: (CFFI::FL-ERROR "Unable to load any of the alternatives:~%   ~S" ("libssl.so.1.0.2" "libssl.so.1.0.1l" "libssl.so.1.0.1e" "libssl.so.1.0.1j" "libssl.so.1.0.1" "libssl.so.1.0.0q" ...))
  1: (CFFI::TRY-FOREIGN-LIBRARY-ALTERNATIVES CL+SSL::LIBSSL ("libssl.so.1.0.2" "libssl.so.1.0.1l" "libssl.so.1.0.1e" "libssl.so.1.0.1j" "libssl.so.1.0.1" "libssl.so.1.0.0q" ...) NIL)
  2: ((FLET CFFI::%DO-LOAD :IN CFFI::%DO-LOAD-FOREIGN-LIBRARY) #<CFFI:FOREIGN-LIBRARY LIBSSL> CL+SSL::LIBSSL (:OR "libssl.so.1.0.2" "libssl.so.1.0.1l" "libssl.so.1.0.1e" "libssl.so.1.0.1j" "libssl.so.1.0.1..
  3: (CFFI:LOAD-FOREIGN-LIBRARY CL+SSL::LIBSSL :SEARCH-PATH NIL)
  4: (SB-FASL::LOAD-FASL-GROUP #S(SB-FASL::FASL-INPUT :STREAM #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/usr/share/common-lisp/source/cl+ssl/src/reload.fasl..
  5: (SB-FASL::LOAD-AS-FASL #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/usr/share/common-lisp/source/cl+ssl/src/reload.fasl" {1002B52CC3}> NIL NIL)
  6: ((FLET SB-FASL::LOAD-STREAM :IN LOAD) #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/usr/share/common-lisp/source/cl+ssl/src/reload.fasl" {1002B52CC3}> T)
  7: (LOAD #P"/home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/usr/share/common-lisp/source/cl+ssl/src/reload.fasl" :VERBOSE NIL :PRINT NIL :IF-DOES-NOT-EXIST T :EXTERNAL-FORMAT :DEFAULT)
  8: (UIOP/UTILITY:CALL-WITH-MUFFLED-CONDITIONS #<CLOSURE (LAMBDA NIL :IN UIOP/LISP-BUILD:LOAD*) {1002B506DB}> ("Overwriting already existing readtable ~S." #(#:FINALIZERS-OFF-WARNING :ASDF-FINALIZERS)))
  9: ((SB-PCL::EMF ASDF/ACTION:PERFORM) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "cl+ssl" "src" "reload">)
 10: ((:METHOD ASDF/ACTION:PERFORM-WITH-RESTARTS (ASDF/LISP-ACTION:LOAD-OP ASDF/LISP-ACTION:CL-SOURCE-FILE)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "cl+ssl" "src" "reload">) [fast-m..
 11: ((:METHOD ASDF/ACTION:PERFORM-WITH-RESTARTS :AROUND (T T)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "cl+ssl" "src" "reload">) [fast-method]
 12: ((:METHOD ASDF/PLAN:PERFORM-PLAN (LIST)) ((#<ASDF/LISP-ACTION:COMPILE-OP > . #<ASDF/SYSTEM:SYSTEM "trivial-gray-streams">) (#1=#<ASDF/LISP-ACTION:PREPARE-OP > . #<ASDF/SYSTEM:SYSTEM #2="chunga">) (#1#..
 13: ((FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT))
 14: ((:METHOD ASDF/PLAN:PERFORM-PLAN :AROUND (T)) ((#<ASDF/LISP-ACTION:COMPILE-OP > . #<ASDF/SYSTEM:SYSTEM "trivial-gray-streams">) (#1=#<ASDF/LISP-ACTION:PREPARE-OP > . #<ASDF/SYSTEM:SYSTEM #2="chunga">)..
 15: ((FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT))
 16: ((:METHOD ASDF/PLAN:PERFORM-PLAN :AROUND (T)) #<ASDF/PLAN:SEQUENTIAL-PLAN {100672F123}> :VERBOSE NIL) [fast-method]
 17: ((:METHOD ASDF/OPERATE:OPERATE (ASDF/OPERATION:OPERATION ASDF/COMPONENT:COMPONENT)) #<ASDF/LISP-ACTION:LOAD-OP :VERBOSE NIL> #<ASDF/SYSTEM:SYSTEM "hunchentoot"> :VERBOSE NIL) [fast-method]
 18: ((SB-PCL::EMF ASDF/OPERATE:OPERATE) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:LOAD-OP :VERBOSE NIL> #<ASDF/SYSTEM:SYSTEM "hunchentoot"> :VERBOSE NIL)
 19: ((LAMBDA NIL :IN ASDF/OPERATE:OPERATE))
 --more--

; Evaluation aborted

CL-USER> (ql:quickload :wookie)
To load "wookie":
  Load 1 ASDF system:
    wookie
; Loading "wookie"

Unable to load any of the alternatives:
   ("libuv.so" "libuv.so.1" "/usr/lib/libuv.so"
    "/usr/local/lib/libuv.so" "/usr/local/lib/libuv.dylib")
   [Condition of type CFFI:LOAD-FOREIGN-LIBRARY-ERROR]

Restarts:
 0: [RETRY] Try loading the foreign library again.
 1: [USE-VALUE] Use another library instead.
 2: [TRY-RECOMPILING] Recompile lib and try loading it again
 3: [RETRY] Retry loading FASL for #<CL-SOURCE-FILE "cl-libuv" "lib">.
 4: [ACCEPT] Continue, treating loading FASL for #<CL-SOURCE-FILE "cl-libuv" "lib"> as having been successful.
 5: [RETRY] Retry ASDF operation.
 --more--

Backtrace:
  0: (CFFI::FL-ERROR "Unable to load any of the alternatives:~%   ~S" ("libuv.so" "libuv.so.1" "/usr/lib/libuv.so" "/usr/local/lib/libuv.so" "/usr/local/lib/libuv.dylib"))
  1: (CFFI::TRY-FOREIGN-LIBRARY-ALTERNATIVES LIBUV::LIBUV ("libuv.so" "libuv.so.1" "/usr/lib/libuv.so" "/usr/local/lib/libuv.so" "/usr/local/lib/libuv.dylib") NIL)
  2: ((FLET CFFI::%DO-LOAD :IN CFFI::%DO-LOAD-FOREIGN-LIBRARY) #<CFFI:FOREIGN-LIBRARY LIBUV> LIBUV::LIBUV (:OR "libuv.so" "libuv.so.1" "/usr/lib/libuv.so" "/usr/local/lib/libuv.so" "/usr/local/lib/libuv.dy..
  3: (CFFI:LOAD-FOREIGN-LIBRARY LIBUV::LIBUV :SEARCH-PATH NIL)
  4: (SB-FASL::LOAD-FASL-GROUP #S(SB-FASL::FASL-INPUT :STREAM #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/c..
  5: (SB-FASL::LOAD-AS-FASL #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/cl-libuv-20160825-git/lib.fasl" {10..
  6: ((FLET SB-FASL::LOAD-STREAM :IN LOAD) #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/cl-libuv-20160825-gi..
  7: (LOAD #P"/home/inaimathi/.cache/common-lisp/sbcl-1.3.19.nixos-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/cl-libuv-20160825-git/lib.fasl" :VERBOSE NIL :PRINT NIL :IF-DOES-NOT-EXIST T :..
  8: (UIOP/UTILITY:CALL-WITH-MUFFLED-CONDITIONS #<CLOSURE (LAMBDA NIL :IN UIOP/LISP-BUILD:LOAD*) {1003252B9B}> ("Overwriting already existing readtable ~S." #(#:FINALIZERS-OFF-WARNING :ASDF-FINALIZERS)))
  9: ((SB-PCL::EMF ASDF/ACTION:PERFORM) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "cl-libuv" "lib">)
 10: ((:METHOD ASDF/ACTION:PERFORM-WITH-RESTARTS (ASDF/LISP-ACTION:LOAD-OP ASDF/LISP-ACTION:CL-SOURCE-FILE)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "cl-libuv" "lib">) [fast-method]
 11: ((:METHOD ASDF/ACTION:PERFORM-WITH-RESTARTS :AROUND (T T)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "cl-libuv" "lib">) [fast-method]
 12: ((:METHOD ASDF/PLAN:PERFORM-PLAN (LIST)) ((#1=#<ASDF/LISP-ACTION:COMPILE-OP > . #2=#<ASDF/COMPONENT:STATIC-FILE #3="alexandria" "LICENCE">) (#4=#<ASDF/LISP-ACTION:LOAD-OP > . #2#) (#1# . #5=#<ASDF/SYS..
 13: ((FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT))
 14: ((:METHOD ASDF/PLAN:PERFORM-PLAN :AROUND (T)) ((#1=#<ASDF/LISP-ACTION:COMPILE-OP > . #2=#<ASDF/COMPONENT:STATIC-FILE #3="alexandria" "LICENCE">) (#4=#<ASDF/LISP-ACTION:LOAD-OP > . #2#) (#1# . #5=#<ASD..
 15: ((FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT))
 16: ((:METHOD ASDF/PLAN:PERFORM-PLAN :AROUND (T)) #<ASDF/PLAN:SEQUENTIAL-PLAN {1001EED0D3}> :VERBOSE NIL) [fast-method]
 17: ((:METHOD ASDF/OPERATE:OPERATE (ASDF/OPERATION:OPERATION ASDF/COMPONENT:COMPONENT)) #<ASDF/LISP-ACTION:LOAD-OP :VERBOSE NIL> #<ASDF/SYSTEM:SYSTEM "wookie"> :VERBOSE NIL) [fast-method]
 18: ((SB-PCL::EMF ASDF/OPERATE:OPERATE) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:LOAD-OP :VERBOSE NIL> #<ASDF/SYSTEM:SYSTEM "wookie"> :VERBOSE NIL)
 19: ((LAMBDA NIL :IN ASDF/OPERATE:OPERATE))
 --more--

; Evaluation aborted
```

I want to stress that I have not failed to install the above "missing" native libraries. It's just that I use `nix` as a package manager, so they get installed in a directory that `cffi` doesn't expect. I've also tried installing the same through `apt-get` and still get similar errors. And don't even get me started on `OS X` or Windows setups of the same.

So, unfortunately, a much as `:house` sucks, I'm sticking with it, and working around its shortcomings. I've also got plans to submit it to [`quicklisp`](https://github.com/quicklisp/quicklisp-projects) once I work up a reasonable test suite.

At that point, I won't have to take the `clone` step either.
