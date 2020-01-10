Ok, so I've had this idea in my head ever since an interview I had a while ago. There was a block of conversation about [`nix`](TODO) and [Common Lisp](TODO) and [`quicklisp`](TODO), and we began musing about what kind of correctness/testing guarantees you'd ideally want out of a project that you'll be deploying and maintaining that depends on something out of [Xach](TODO)'s fabulous system repository. In particular, the question we ended up asking each other was

> Why isn't there a [`hydra`](TODO) for `quicklisp`?

Well, I've got an itch to write some code, so in another week or so, there damn well will be.

I'm going to want the core of this thing to be fairly general, so I'm building it from the perspective of a generic CI server. A few things obviously need to be pulled out into modules, but there are a few places where there isn't an a priori obviously right way to do it.

# Thinking Through

In particular, what's the work model for this thing going to be? There's going to be a queue of projects ready to be built at any given time just so we can get multiple worker threads going on this obviously, but how that queue is populated is a little nebulous.

I'm probably going to want the projects and results kept on disk for this exercise, so I could either see a rebuild getting triggered by the `pull` process or a disk watcher. If it's the
