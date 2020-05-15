I'm not pulling punches. Common Lisp types are garbage. Assuming the thing you're defining fits into the pre-existing types that come with it, you're fine. As soon as you want to do something like define polymorphic key/value structures you are, near as I can tell, on your fucking own bucko.

So I guess I'm rolling my own here?

Ok, the good news is that I'm in just enough of a hacky mood that I don't give a flying fuck how shitty this is going to be. That... might come back to bite me in the ass later, but whatever the fuck.

### Defining Types

Here's how I have to define type `dict`.

```
(deftype dict (&optional keys vals)
  (let ((sym (intern (format nil "MAP-TYPE-~a-~a" keys vals) :clj)))

    (unless (fboundp sym)
      (setf (fdefinition sym) (kv-types keys vals)))

    `(and (satisfies map?) (satisfies ,sym))))
```

Hopefully, you recognize how batshit insane that is. In order to properly define a polymorphic key/value type, I have to _manually intern predicates that deal with the specific types in question at declaration time_. Holy fucking hell, what am I writing here, C++? I mean, I can do it, but it's nausiating to have to.

Once I've got that, I can declare things.

### Checking for equalities

Once I've got a type declared, I need to do the work I actually care about. Which is: figure out which of the built-in structural equality operations is the most efficient I can use while _also_ being as correct as possible.

TODO - insert snippets from types.lisp here

### Putting it all together

This doesn't quite satisfy the use case.

TODO - show how declaring types on dict literals still fucks up

See, because the type/equality function of that `dict` is picked at `read`-time, I need to get my declaration there too. So, here's how to define a hacky-as-fuck reader macro that takes type bindings into account.

TODO - show the new reader macro here

Fucking woo! Note to self, maybe have the reader infer the correct type based on the keys provided? It's a bit dangerous, because the user might `insert` more general keys later, but it could save some cycles.

That's all for today, motherfuckers. Next time, I'm profiling garbage, and seeing if this flaming pile is even worth keeping around.

Fingers crossed, yeah?
