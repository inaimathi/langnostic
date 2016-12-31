## Ansible

So I've been using [Ansible](TODO) for the past little while. At work, I mean. I don't deploy anything that requires anywhere near the amount of scale it would take for a tool like this to start paying or itself, so I probably wouldn't have picked it up at all if someone weren't paying me for the experience. Having now used [`nix`](TODO), [`ansible`](TODO), [`chef`](TODO), what strikes me about them is how many insist on a new language syntax. I'm quite curious as to why `ansible` couldn't have been implemented as a library in whatever turing complete language. With functions and environments rather than a giant director tree of `yml` files arranged by "convention"[^as-a-side-note].

[^as-a-side-note]: As a sidenote; the credo of "convention over configuration" has at this point been used to force users to memorize arbitrary garbage as an alternative to having the system designers sit down and think through the principles of their system in a reasonable way. I call this lazy fucking design, and I'm not content to put up with it quietly anymore. The more convention your system requires, the shittier it is on an absolute scale, there I said it.

On the one hand, this would possibly alienate X programmers who don't program your X. On the other, a well-chosen X would give some advantages, non-X programmers wouldn't have to put up with garbage like YML config files[^lisp-weenie-digression], and X programmers whose X was [probably coincidentally](TODO - link to self-titled) well-chosen mildly happier. That's probably the trade-off happening here; ease-of-use vs. likelyhood-of-adoption, so I won't linger on it too much. My only problem with Ansible remains the gobsmacking amount of boilerplate necessary to do small things.

[^lisp-weenie-digression]: Lisp weenie digression here, by the way. The only reason YML/XML/CONF/WTFBBQ files are needed at all is that the languages making use of them have shitty enough syntaxes that you can't express meaningful configuration _in_ them. As you may have heard by now, Lisp doesn't have this problem, which makes certain "trivially solvable" issues from other languages vanish entirely, and makes certain impossible issues from other languages tractable. I'll get into more depth on that at some point in the future, I'm sure.

In summary, I see the need to formalize a deployment process in a machine-manipulable way, and very much agree that the best way to do this is to write scripts that assume the bare minimum of prior infrastructure. I doubt that the syntactic choices made in any of the existing tools are optimal, or even adequate.

## House updates

- reflect on what's already happened
- chart a course for handler definition
- talk a bit about spike-specific optimizations
- talk about the worker-thread-for-parsing, request-handling and writing (and possibly discuss for buffering)

## New place

- talk a bit about receptiviti
