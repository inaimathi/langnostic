Really quick update, no philosophical thoughts at all. It just seems I got into an uncommon situation, and wanted to share.

[CodeMirror](http://codemirror.net/) has auto-completion. Via [`show-hint`](http://codemirror.net/addon/hint/show-hint.js) and various [in-browser](http://codemirror.net/addon/hint/anyword-hint.js), [language-specific](http://codemirror.net/addon/hint/javascript-hint.js) modes. What I wanted to  do is have a server-side handler that pulls out completions for a given partial symbol and just lets the front-end display them. The way to do this seems to be neither obvious nor well-documented, given how my googling went. The frustrating thing is that, if you read [the `show-hint` source code](https://github.com/marijnh/CodeMirror/blob/master/addon/hint/show-hint.js), it's clearly [built to support asynchronous hinting](https://github.com/marijnh/CodeMirror/blob/master/addon/hint/show-hint.js#L36-L41), but doesn't make it very clear how to take advantage of that.

The [examples](https://github.com/marijnh/CodeMirror/blob/master/addon/hint/anyword-hint.js) involve writing a function that returns [an object](https://github.com/marijnh/CodeMirror/blob/master/addon/hint/anyword-hint.js#L40) which can then be used to display a little completions drop-down, which kind of assumes that your completion function is going to immediately return something.

Yes, I could use these [`promises`](https://www.promisejs.org/) that are so fashionable nowadays, if I felt like re-writing `show-hint`, but I want something that works out of the box, with minimal tweaking of library code. Here's an approach that seems to work.

```lisp
...
(register-helpers 
 "hint"
 (create :ajax
        (lambda (mirror callback options)
          (let* ((cur (chain mirror (get-cursor)))
                 (tok (chain mirror (get-token-at cur))))
            (get "/cl-notebook/system/complete" (create :partial (@ tok string) :package :cl-notebook)
                 (lambda (res)
                   (callback 
                    (create :list (or (string->obj res) (new (-array)))
                            :from (chain -code-mirror (-pos (@ cur line) (@ tok start)))
                            :to (chain -code-mirror (-pos (@ cur line) (@ tok end)))))))))
        :auto
        (lambda (mirror options)
          (chain -code-mirror commands 
                 (autocomplete mirror (@ -code-mirror hint ajax) (create :async t))))))
...
```

For the JavaScripters out there

```javascript
CodeMirror.registerHelper(
    "hint", "ajax",
    function (mirror, callback, options) {
        var cur = mirror.getCursor();
        var tok = mirror.getTokenAt(cur);
        httpGet("/cl-notebook/system/complete", { partial: tok.string, package: "cl-notebook" },
                function (res) {
                    callback({ list: JSON.parse(res) || [],
                               from: CodeMirror.Pos(cur.line, tok.start),
                               to: CodeMirror.Pos(cur.line, tok.end) 
                             })
                })
    })

CodeMirror.registerHelper(
    "hint", "auto",
    function (mirror, options) {
        CodeMirror.commands.autocomplete(mirror, CodeMirror.hint.ajax, { async: true })
    })
```

The new `auto` helper overrides [the `show-hint` default](https://github.com/marijnh/CodeMirror/blob/master/addon/hint/show-hint.js#L344-L356) and makes sure that the `ajax` helper gets called on a trigger of `autocomplete`. This approach assumes you will *only* be completing via server hit. If you want multiple completion sources, your custom `auto` helper will need to be a bit more complicated than shown. `httpGet` is something you'll have to define yourself, or get from a library somewhere, but its intent should be self-explanatory. All you need now is a handler on your server side at that uri that takes those parameters and spits back a list of completions for the given partial word. Mine looks like

```lisp
(define-json-handler (cl-notebook/system/complete) ((partial :string) (package :keyword))
  (let ((p (string-upcase partial))
        (res))
    (do-symbols (s package)
      (when (alexandria:starts-with-subseq p (symbol-name s))
        (push s res)))
    (sort (mapcar (lambda (s) (string-downcase (symbol-name s)))
                  (remove-duplicates res))
          #'&lt; :key #'length)))
```

which is about as naive as you can get. It does the job though; even when completing on the empty string (which returns all symbols in the `cl-notebook` package), the server comes back with a response in about `10 msec`.

That's that. If you're in the situation I was in a couple hours ago, hopefully you'll stumble upon this and have an easier time.
