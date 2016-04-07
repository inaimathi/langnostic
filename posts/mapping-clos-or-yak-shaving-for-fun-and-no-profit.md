Just a quick update today; I have more work than I think I can handle for the foreseeable future. A combination of problems I've never solved at work, some odd contract requests and several projects I've gotten myself into in my personal time. You already know about the [CLSQL/Hunchentoot crash course](/posts/cl-chan-a-clsql-and-hunchentoot-crash-course), and the [clomments](https://github.com/Inaimathi/clomments) system. You probably don't know about the last two, one of which sort of chains off of `clomments`[^the-other].

[^the-other]: The other I'm conveniently ignoring for the duration of this post, except for this sentence.

The background is that I wanted conversation threads in `clomments`. As in, when you see a comment, you should be able to reply to it. You [*can* store that kind of thing in a relational database](http://www.ferdychristant.com/blog/archive/DOMM-7QJPM7), but it feels a hacky. Especially if you want the system to scale out. I've looked at a [couple](http://www.mongodb.org/) of [nosql](http://couchdb.apache.org/) databases in the past and have been looking for an excuse to use them in practice. Well, it turns out that [storing hierarchical data is relatively simple](http://wiki.apache.org/couchdb/How_to_store_hierarchical_data) in them. I picked CouchDB for no particular reason[^quicklisp-support], and tried out a few things. Unfortunately, `clouchdb` has at least one [annoying bug](http://stackoverflow.com/questions/7537018/clouchdb-error-with-id) that prevents me from using it. It wouldn't normally, I've already sent a one-liner patch to the author, but he's currently working at one of these Disney-style-IP places that own your intellectual property, DNA and all derivative works of both (I'm paraphrasing), so he can't actually merge that patch any time soon.

[^quicklisp-support]: [`quicklisp`](https://github.com/fons/cl-mongo) has support for [`cl-mongo`](https://github.com/fons/cl-mongo), [`clouchdb`](http://common-lisp.net/project/clouchdb/) *and* [`chillax`](https://github.com/sykopomp/chillax).

Ok, so I guess I'm using [`chillax`](http://wiki.apache.org/couchdb/Chillax) then, which kind of saddens me because I'd prefer a project that used the same naming convention as the project I'm using it for, but that's beside the point. In either case, [interacting with Couch from CL](http://common-lisp.net/project/clouchdb/#examples) is kind of a pain in the ass. It involves tons of [alists](http://www.ida.liu.se/imported/cltl/clm/node153.html) and many traversals of same, which isn't *[horrible](http://drhorrible.com/)*, but definitely not as pleasant as [interacting through CLOS](http://clsql.kpe.io/manual/ch02s02.html) the way I have been in CLSQL. Looking around, there doesn't seem to be an ORM-style thing already built for Couch or Mongo, so I figured I'd try my hand at one as part of the `clomments` project. I don't have anything workable yet, I've just been playing around so far, but the first leg of research has turned up a rather annoying implementation detail that segues nicely into the title of this article.

Do you know how to [map](http://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm) over a CLOS object?

If you've never thought about it before, feel free to go investigating. Hopefully, you have a better time than I did. If you just want to do it in SBCL, it's actually fairly simple

```lisp
(defun slot-names (class)
  (mapcar #'sb-pcl:slot-definition-name
          (sb-pcl:class-slots class)))

(defun map-slots (fn instance)
  (loop for slot-name in (slot-names (class-of instance))
        collect (funcall fn slot-name (slot-value instance slot-name))))
```

The problem, as the astute among you have already noticed, is `sb-pcl`. That's actually an SBCL-only CLOS library that implements various functions found in the [spec](http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/doc/standard/ansi/clos/0.html). Other lisps don't make the same decisions, so if you want to make these functions portable, you need to do some more work. Either going what I call ["the insane route"](http://stackoverflow.com/questions/3086561/make-clos-objects-printable-in-lisp) (which I took some cues from) and conditionally define each function that deals with a class, or the `clsql-sys` route in which you don't specify a package for these functions, but rather `:use` different modules conditionally as part of your package definition. That looks like

```lisp
(defpackage #:clos-utils
    (:use #:common-lisp)
  (:shadowing-import-from
   #+openmcl-native-threads #:ccl
   #+cmu #:pcl
   #+sbcl #:sb-pcl
   #+lispworks #:hcl
   #+allegro #:mop
   #+clisp #:clos
   #:class-slots #:slot-definition-name))

(in-package :clos-utils)

(defun slot-names (class)
  (mapcar #'slot-definition-name
          (class-slots class)))

(defun map-slots (fn instance)
  (loop for slot-name in (slot-names (class-of instance))
        collect (funcall fn slot-name (slot-value instance slot-name))))
```

That's still not fully portable, by the way. `openmcl` apparently calls `class-slots` `class-instance-slots` if it doesn't have `openmcl-native-threads`. But it's *reasonably* close to portable. Having come down off a week or so of Python/Ruby scripting, that...was a lot more work than I expected to do for a task like this. Hopefully this saves someone else out there some time (or causes someone to contact me, pointing out a much easier way of doing it). Anyhow, that's phase one of creating CLOS-based bindings for CouchDB, which will then let me succinctly work on `clomments` which should eventually increase the amount of Freedom on the net by some small increment.
