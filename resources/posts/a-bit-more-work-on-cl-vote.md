So I've done a bit more work on [`cl-vote`](https://github.com/inaimathi/cl-vote). The main thing I've learned so far is...

## House Still Sucks, and I'm Still Keeping It

This project started off with an attempt to use [`hunchentoot`](http://edicl.github.io/hunchentoot/). Which is a fine server, with much to recommend it. I actually got a fair way through the prototyping process, and everything was going fairly well at that point. And _then_, I needed to switch machines for a bit. The new computer had this to say about my attempt to load it:

```
Unable to load any of the alternatives:
   ("libssl.so.1.1" "libssl.so.1.0.2m" "libssl.so.1.0.2k"
    "libssl.so.1.0.2" "libssl.so.1.0.1l" "libssl.so.1.0.1j"
    "libssl.so.1.0.1f" "libssl.so.1.0.1e" "libssl.so.1.0.1"
    "libssl.so.1.0.0q" "libssl.so.1.0.0" "libssl.so.0.9.8ze"
    "libssl.so.0.9.8" "libssl.so.10" "libssl.so.4" "libssl.so")
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
  0: (CFFI::FL-ERROR "Unable to load any of the alternatives:~%   ~S" ("libssl.so.1.1" "libssl.so.1.0.2m" "libssl.so.1.0.2k" "libssl.so.1.0.2" "libssl.so.1.0.1l" "libssl.so.1.0.1j" ...))
  1: (CFFI::TRY-FOREIGN-LIBRARY-ALTERNATIVES CL+SSL::LIBSSL ("libssl.so.1.1" "libssl.so.1.0.2m" "libssl.so.1.0.2k" "libssl.so.1.0.2" "libssl.so.1.0.1l" "libssl.so.1.0.1j" ...) NIL)
  2: ((FLET CFFI::%DO-LOAD :IN CFFI::%DO-LOAD-FOREIGN-LIBRARY) #<CFFI:FOREIGN-LIBRARY LIBSSL> CL+SSL::LIBSSL (:OR "libssl.so.1.1" "libssl.so.1.0.2m" "libssl.so.1.0.2k" "libssl.so.1.0.2" "libssl.so.1.0.1l" ..
  3: (CFFI:LOAD-FOREIGN-LIBRARY CL+SSL::LIBSSL :SEARCH-PATH NIL)
  4: (SB-FASL::LOAD-FASL-GROUP #S(SB-FASL::FASL-INPUT :STREAM #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.5.1-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/cl+ssl-2..
  5: (SB-FASL::LOAD-AS-FASL #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.5.1-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/cl+ssl-20200427-git/src/reload.fasl" {1005..
  6: ((FLET SB-FASL::THUNK :IN LOAD))
  7: (SB-FASL::CALL-WITH-LOAD-BINDINGS #<CLOSURE (FLET SB-FASL::THUNK :IN LOAD) {7FD50D5AC88B}> #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.5.1-linux-x64/home/inaimathi/quicklisp..
  8: ((FLET SB-FASL::LOAD-STREAM :IN LOAD) #<SB-SYS:FD-STREAM for "file /home/inaimathi/.cache/common-lisp/sbcl-1.5.1-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/cl+ssl-20200427-git/src/rel..
  9: (LOAD #P"/home/inaimathi/.cache/common-lisp/sbcl-1.5.1-linux-x64/home/inaimathi/quicklisp/dists/quicklisp/software/cl+ssl-20200427-git/src/reload.fasl" :VERBOSE NIL :PRINT NIL :IF-DOES-NOT-EXIST T :EX..
 10: (UIOP/UTILITY:CALL-WITH-MUFFLED-CONDITIONS #<CLOSURE (LAMBDA NIL :IN UIOP/LISP-BUILD:LOAD*) {1005B381EB}> ("Overwriting already existing readtable ~S." #(#:FINALIZERS-OFF-WARNING :ASDF-FINALIZERS)))
 11: ((SB-PCL::EMF ASDF/ACTION:PERFORM) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "cl+ssl" "src" "reload">)
 12: ((LAMBDA NIL :IN ASDF/ACTION:CALL-WHILE-VISITING-ACTION))
 13: ((:METHOD ASDF/ACTION:PERFORM-WITH-RESTARTS (ASDF/LISP-ACTION:LOAD-OP ASDF/LISP-ACTION:CL-SOURCE-FILE)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "cl+ssl" "src" "reload">) [fast-m..
 14: ((:METHOD ASDF/ACTION:PERFORM-WITH-RESTARTS :AROUND (T T)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/LISP-ACTION:CL-SOURCE-FILE "cl+ssl" "src" "reload">) [fast-method]
 15: ((:METHOD ASDF/PLAN:PERFORM-PLAN (T)) #<ASDF/PLAN:SEQUENTIAL-PLAN {100411E773}>) [fast-method]
 16: ((FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT))
 17: ((:METHOD ASDF/PLAN:PERFORM-PLAN :AROUND (T)) #<ASDF/PLAN:SEQUENTIAL-PLAN {100411E773}>) [fast-method]
 18: ((:METHOD ASDF/OPERATE:OPERATE (ASDF/OPERATION:OPERATION ASDF/COMPONENT:COMPONENT)) #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/SYSTEM:SYSTEM "hunchentoot"> :PLAN-CLASS NIL :PLAN-OPTIONS NIL) [fast-method]
 19: ((SB-PCL::EMF ASDF/OPERATE:OPERATE) #<unused argument> #<unused argument> #<ASDF/LISP-ACTION:LOAD-OP > #<ASDF/SYSTEM:SYSTEM "hunchentoot"> :VERBOSE NIL)
 --more--
```

And once again, `:house` loaded and started without issue. Just like the [last time](/posts/tomb-and-more-cl-vote) I mentioned this, I _have_ that library it's complaining about. And it's in a place perfectly consistent with being installed by [`nix`](https://github.com/NixOS/nix) or [`guix`](https://guix.gnu.org/). And no amount of poking at `cffi`/`sbcl` configuration can get it loaded properly. So, if for no reason other than prototyping, there's absolutely a need for a Common Lisp-native web server.

The problem at this point is that I was basically a kid when I designed `house`. And I did it as part of a much more ambitious project that was actually the main goal, so the server itself got comparatively little of my brain time.

I'm very tempted to try again.

Maybe not from the ground up. There are a lot of hard-won bugfixes and `#+`/`#-` switches in that codebase, and I don't want to give up all that progress by going nuclear. But the way the handler/type system is built is less than stellar, sessions could use some touch-ups, and I could _probably_ stand to be a bit more general in the handling of a few elegant flow points. Especially in the sense of providing better HTTP-client and/or websocket support. Im' not doing this _now_, but I've made a note to my future self.

## Recovery Token

Now that I've got [`tomb`](https://github.com/inaimathi/tomb) ready to let me store passwords and password-like things in database without exposing the relevant plaintexts anywhere, I can use it to store a recovery token per user account.

The idea is that, since I'm using an authenticator app to log users in, and those authenticator apps are typically on a phone somewhere, they might need a way of accessing their account without having access to their phone. You can see the relevant changes in [this commit](https://github.com/inaimathi/cl-vote/commit/4459cd9f743ec3080b6f23c27320b2249f899685).

To summarize:

1. We [generate a recovery token when a user is created](https://github.com/inaimathi/cl-vote/commit/4459cd9f743ec3080b6f23c27320b2249f899685#diff-b093d9c9c5e061a6714cb5787ac6de96R11)
2. There is a [new handler](https://github.com/inaimathi/cl-vote/commit/4459cd9f743ec3080b6f23c27320b2249f899685#diff-d69eb77040218d8ff5b87e4eb7412d73R29-R38) that expires the old token and generates a new one for the current user
3. We now [accept either the authenticator challenge result _or_ a users' recovery token](https://github.com/inaimathi/cl-vote/commit/4459cd9f743ec3080b6f23c27320b2249f899685#diff-d69eb77040218d8ff5b87e4eb7412d73R69-R79) to log in that user, instead of just the challenge result.
4. If the correct recovery token is given, the users' recovery token is expired and given a new one as part of the login process. Specifically, by [redirecting to that expire-and-create page](https://github.com/inaimathi/cl-vote/commit/4459cd9f743ec3080b6f23c27320b2249f899685#diff-d69eb77040218d8ff5b87e4eb7412d73R75-R78) above.

That does it for now. The next chunk of my work is going to focus first on a hammer-protection system, and then on going through the full usage path of an election. From creation, to voting to tallying results.

As always, I'll let you know how it goes.
