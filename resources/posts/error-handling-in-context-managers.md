In various Lisps, there's a semi-common pattern of context wrapping with error handling. You have a situation in which you want to do something, and in the process bind some external resource, then free the resource afterwards.

```
(let* ((resource (open-resource foo))
       (result (progn
		 (bar)
		 (baz)
		 (mumble))))
  (close-resource resource)
  (delete-resource-from-disk resource)
  result)
```

You can write a relatively simple wrapper macro for this situation.

```
(defmacro with-resource ((name target) &body body)
  (let ((result (gensym)))
    `(let* ((,name (open-resource ,target))
	    (,result (progn ,@body)))
       (close-resource resource)
       (delete-resource-from-disk resource)
       ,result)))
```

With that definition, you can instead write.

```
(with-resource (resource foo)
  (bar)
  (mumble (baz resource)))
```

Ok, but what if the code you've written throws an error somehow?

```
(with-resource (resource foo)
  (bar)
  (error "Arbitrary Explosion")
  (mumble (baz resource)))
```

Your routine doesn't complete, but also, the claimed resource never gets freed afterwards. You could fix this by just always wrapping the stuff you wrap with `with-resource` in some error-trapping. `ignore-errors`/`handler-case`/`handler-bind` depending on the specific situation.

```
(with-resource (resource foo)
  (ignore-errors
    (bar)
    (error "Arbitrary Explosion")
    (mumble (baz resource))))
```

However, it'd still be nice to be more responsible as a macro developer and do the right thing with the bound resource without depending on your user doing the right thing. The solution is [`unwind-protect`](http://clhs.lisp.se/Body/s_unwind.htm) [^thank-you-to-the-readers].

[^thank-you-to-the-readers]: Incidentally, thank you to the readers who pointed this out. I left out what turned out to be a key piece of context from the real situation I was dealing with for the sake of expedience (this situation came up in a Python context in real life). I've added the relevant explanation; hopefully that clarifies everything (and I'm not being denser than I thought I was being).

```
(defmacro with-resource ((name target) &body body)
  (let ((result (gensym)))
    `(let* ((,name (open-resource ,target)))
       (unwind-protect
	    (let ((,result (ignore-errors (progn ,@body))))
	      (resolve-resource resource)
	      ,result)
	 (close-resource resource)
	 (delete-resource-from-disk resource)))))
```

When you're deaing with deploying micro-services, it sometimes gets a bit trickier. You generally want some central log/diagnostic server to be notified of the error condition. That's not a situation where you want something to happen regardless of error presence; it's a situation where you want something _different_ to happen on error. For a concrete example, imagine needing to do something locally that involves downloading, poking, and then deletin a file from a URL. `unwind-protect` could still help, but it'd be only part of the story.

```
(defmacro with-pdf-from-uri ((path uri) &body body)
  (let ((result (gensym)))
    `(handler-case
	 (let ((,result (http-request uri))
	       (,path (with-output-to-temporary-file (s) (write (http-body ,result) :stream s))))
	   (unwind-protect
		(progn ,@body)
	     (delete-file))
	   (write-to-disk))
       (http-error (e)
	 (remote-log "Failed to download PDF")
	 (error e))
       (cannot-create-temporary-file (e)
	 (remote-log "Failed to create tempfile")
	 (error e))
       (error (e)
	 (remote-log "An ancient evil stirs. Your lights flicker. In the distance, sirens." e)
	 (error e)))))
```

Off the top of my head, I'd write something like this, though there's possibly better ways of abstracting the situation.


## And Now, For Something Completely Different

You can do something similar in Python too. For example

```
@contextmanager
def logged(tag):
    print(f"Starting a procedure <{tag}>...")
	yield
	print(f"Finished the procedure <{tag}>...")
```

```
>>> with logged("MY TEST"):
...     print("blah")
...
Starting a procedure <MY TEST>...
blah
Finished the procedure <MY TEST>...
>>>
```

And now, the point of this entire post, _yes_, you _can_ wrap that `yield` in a `try`/`catch` and have it do what you're expecting.

```
@contextmanager
def logged(tag):
    print(f"Starting a procedure <{tag}>...")
    try:
        yield
        print("IT WORKED!")
    except:
        print("OH NO! ETERNAL DARKNESS AWAITS YOU!")
    print(f"Finished the procedure <{tag}>...")
```

Now you can do

```
>>> with logged("MY TEST"):
...     print("blah")
...
Starting a procedure <MY TEST>...
blah
IT WORKED!
Finished the procedure <MY TEST>...
>>> with logged("MY TEST"):
...     raise("EXPLOSIONS")
...
Starting a procedure <MY TEST>...
OH NO! ETERNAL DARKNESS AWAITS YOU!
Finished the procedure <MY TEST>...
>>>
```

Hopefully, you found that reassuring.
