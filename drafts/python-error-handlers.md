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

However, it'd still be nice to be more responsible as a macro developer and do the right thing with the bound resource without depending on your user doing the right thing.

```
(defun resolve-resource (resource)
  (close-resource resource)
  (delete-resource-from-disk resource)
  nil)

(defmacro with-resource ((name target) &body body)
  (let ((result (gensym)))
    `(let ((,name (open-resource ,target)))
       (handler-case
	   (let ((,result (ignore-errors (progn ,@body))))
	     (resolve-resource resource)
	     ,result)
	 (error (c)
	   (resolve-resource resource)
	   (error c))))))
```

With that change, your macro can close out the claimed resource, and still propagate the error to users that are more careful about their own code. There's probably an even more elegant way to do this with [`handler-bind`](http://clhs.lisp.se/Body/m_handle.htm), but showing you that would be beside the point here.

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
