There's not much going on, but I figured I'd keep you in the loop anyhow.

## <a name="shameless-advertising" href="#shameless-advertising"></a>Shameless Advertising

We've got [SICPv2 starting](https://github.com/CompSciCabal/SMRTYPRTY/wiki/Reading-Schedule!-SICP-Mark-II) at the Toronto Computer Science Reading Group. Or rather, it started last week. Anyway, when we did this the first time, a few people found out about it three-quarters of the way through, and expressed sentiments like "I wish I found out about this when you were starting out". It was enough people that they've managed to organize a second round. And, yes, if there are 9 or fewer core group members, we'll totally be handing these out:

![](/static/img/cabal-badge.png)

## <a name="wik" href="#wik"></a>`wik`

[Last time](/article?name=nix-update.html), I mentioned getting `nix`-the-package-manager up and running on my machine. And I mentioned setting up a Haskell environment with it. What I didn't mention is that some Haskell libraries are currently failing to install. As of this writing, that seems to include all of the Haskell web-frameworks other than [`scotty`](http://hackage.haskell.org/package/scotty) and [`snap`](http://snapframework.com/). [`Yesod`](http://www.yesodweb.com/) and [`happstack`](http://www.happstack.com/) both error at compilation time with some odd type failures that I don't know enough about to diagnose. The specific problem I had this week involved that last one, which also happens to be the server used internally by [`gitit`](http://gitit.net/).

```
...
[ 6 of 38] Compiling Happstack.Server.Internal.TimeoutIO ( src/Happstack/Server/Internal/TimeoutIO.hs, dist/build/Happstack/Server/Internal/TimeoutIO.o )
[ 7 of 38] Compiling Happstack.Server.Internal.TimeoutSocket ( src/Happstack/Server/Internal/TimeoutSocket.hs, dist/build/Happstack/Server/Internal/TimeoutSocket.o )
[ 8 of 38] Compiling Happstack.Server.SURI.ParseURI ( src/Happstack/Server/SURI/ParseURI.hs, dist/build/Happstack/Server/SURI/ParseURI.o )
[ 9 of 38] Compiling Happstack.Server.SURI ( src/Happstack/Server/SURI.hs, dist/build/Happstack/Server/SURI.o )
[10 of 38] Compiling Happstack.Server.Internal.RFC822Headers ( src/Happstack/Server/Internal/RFC822Headers.hs, dist/build/Happstack/Server/Internal/RFC822Headers.o )
[11 of 38] Compiling Paths_happstack_server ( dist/build/autogen/Paths_happstack_server.hs, dist/build/Paths_happstack_server.o )
[12 of 38] Compiling Happstack.Server.Internal.Clock ( src/Happstack/Server/Internal/Clock.hs, dist/build/Happstack/Server/Internal/Clock.o )
[13 of 38] Compiling Happstack.Server.Internal.Cookie ( src/Happstack/Server/Internal/Cookie.hs, dist/build/Happstack/Server/Internal/Cookie.o )
[14 of 38] Compiling Happstack.Server.Internal.Types ( src/Happstack/Server/Internal/Types.hs, dist/build/Happstack/Server/Internal/Types.o )
[15 of 38] Compiling Happstack.Server.Internal.Multipart ( src/Happstack/Server/Internal/Multipart.hs, dist/build/Happstack/Server/Internal/Multipart.o )
[16 of 38] Compiling Happstack.Server.Internal.MessageWrap ( src/Happstack/Server/Internal/MessageWrap.hs, dist/build/Happstack/Server/Internal/MessageWrap.o )
[17 of 38] Compiling Happstack.Server.Types ( src/Happstack/Server/Types.hs, dist/build/Happstack/Server/Types.o )
[18 of 38] Compiling Happstack.Server.Internal.Monads ( src/Happstack/Server/Internal/Monads.hs, dist/build/Happstack/Server/Internal/Monads.o )

src/Happstack/Server/Internal/Monads.hs:69:5:
    Wrong category of family instance; declaration was for a type synonym
    In the newtype instance declaration for &#8216;StT&#8217;
    In the instance declaration for &#8216;MonadTransControl ServerPartT&#8217;

src/Happstack/Server/Internal/Monads.hs:76:5:
    Wrong category of family instance; declaration was for a type synonym
    In the newtype instance declaration for &#8216;StM&#8217;
    In the instance declaration for
      &#8216;MonadBaseControl b (ServerPartT m)&#8217;

src/Happstack/Server/Internal/Monads.hs:262:5:
    Wrong category of family instance; declaration was for a type synonym
    In the newtype instance declaration for &#8216;StT&#8217;
    In the instance declaration for &#8216;MonadTransControl (FilterT a)&#8217;

src/Happstack/Server/Internal/Monads.hs:267:5:
    Wrong category of family instance; declaration was for a type synonym
    In the newtype instance declaration for &#8216;StM&#8217;
    In the instance declaration for &#8216;MonadBaseControl b (FilterT a m)&#8217;

src/Happstack/Server/Internal/Monads.hs:315:5:
    Wrong category of family instance; declaration was for a type synonym
    In the newtype instance declaration for &#8216;StT&#8217;
    In the instance declaration for &#8216;MonadTransControl WebT&#8217;

src/Happstack/Server/Internal/Monads.hs:327:5:
    Wrong category of family instance; declaration was for a type synonym
    In the newtype instance declaration for &#8216;StM&#8217;
    In the instance declaration for &#8216;MonadBaseControl b (WebT m)&#8217;
builder for &#8216;/nix/store/0rfsb0b07r0n0bq8d9mfn87r5hb391zb-haskell-happstack-server-ghc7.8.4-7.3.9-shared.drv&#8217; failed with exit code 1
cannot build derivation &#8216;/nix/store/ihx7hcmpa10fpl73mwwrck65zpjgrmlr-haskell-gitit-ghc7.8.4-0.10.6.1-shared.drv&#8217;: 1 dependencies couldn't be built
error: build of &#8216;/nix/store/ihx7hcmpa10fpl73mwwrck65zpjgrmlr-haskell-gitit-ghc7.8.4-0.10.6.1-shared.drv&#8217; failed
```

Given that literally all I needed at the time was


-   a wiki
-   with `markdown` syntax
-   that's easy to install
-   and tracks all history


I just said "fuck it" and built my own. It's not generally the sort of thing I do, but judged that it would be a lot more fun and somewhat easier than installing [Mediawiki](https://www.mediawiki.org/wiki/MediaWiki) and its [`markdown` plugin](http://www.mediawiki.org/wiki/Extension:MarkdownSyntax). And I think I happened to be right in this case; the whole thing took about two hours or so, plus a half hour of cosmetic changes for *very* mild ease-of-use.

```python
### wiki.py

from markdown2 import markdown
from subprocess import call, check_output
import datetime, os

def view_page(path, wiki="."):
    return markdown(view_raw_page(path, wiki=wiki))

def view_raw_page(path, wiki="."):
    if not is_in_repo(path, wiki): raise NotInRepo()
    try:
        with open(os.path.join(wiki, path), 'r') as f:
            return f.read()
    except IOError:
        raise PageNotFound()

def delete_page(path, wiki="."):
    if not is_in_repo(path, wiki): raise NotInRepo()
    full = os.path.join(wiki, path)
    if os.path.isfile(full):
        os.remove(full)
        commit(path, "Deleted '" + path + "'", repo=wiki)
        try:
            os.rmdir(os.path.dirname(full))
        except OSError:
            None
    elif os.path.isdir(full):
        raise IsADirectory()
    else:
        raise PageNotFound()

def create_page(path, wiki="."):
    if not is_in_repo(path, wiki): raise NotInRepo()
    full = os.path.join(wiki, path)
    if os.path.exists(full): 
        raise PageExists()
    d = os.path.dirname(full)
    if d and (not os.path.exists(d)): os.makedirs(d)
    with open(full, 'w') as f:
        f.write("# " + path)
    commit(path, "Created '{0}'".format(path), repo=wiki)

def edit_page(path, contents, message="Minor edit", wiki="."):
    if not is_in_repo(path, wiki): raise NotInRepo()
    try:
        full = os.path.join(wiki, path)
        with open(full, 'w') as f:
            f.write(contents)
        commit(path, message, repo=wiki)
    except IOError:
        raise PageNotFound()

########## git-related stuff
def initialize(repo="."):
    call(["git", "init"], cwd=repo)

def commit(path, message="Minor edit", repo="."):
    call(["git", "add", "--all", path], cwd=repo)
    call(["git", "commit", "-m", message], cwd=repo)

def log_of(path, repo="."):
    fmt = "--pretty=format:%x01%H%x00%ct%x00%an%x00%ae%x00%B"
    raw = check_output(["git", "whatchanged", "-z", fmt, "--", path], cwd=repo)
    entries = raw.split("\x01")
    for entry in entries:
        if entry:
            split = filter(identity, entry.split("\x00"))
            yield { "commit_hash": split[0], 
                    "timestamp": datetime.datetime.utcfromtimestamp(int(split[1])),
                    "author_name": split[2],
                    "author_email": split[3],
                    "body": split[4:] }

def identity(a):
    return a

def is_in(a, b):
    [ra, rb] = map(os.path.realpath, [a, b])
    return os.path.commonprefix([ra, rb]) == rb

def is_in_repo(path, repo="."):
    p = os.path.join(repo, path)
    return is_in(p, repo) and not is_in(p, os.path.join(repo, ".git"))

########## custom exceptions
class NotInRepo(Exception):
    pass

class IsADirectory(Exception):
    pass

class PageNotFound(Exception):
    pass

class PageExists(Exception):
    pass
```

```python
### main.py

import tornado.ioloop, tornado.web, json, os, sys, re
import wiki

##### General handlers
class ShowPage(tornado.web.RequestHandler):
    def get(self, path):
        if path == "" or is_dir(path):
            self.write(list_template(path))
        else:
            try:
                pg = wiki.view_page(path, wiki=WIKI_ROOT)
                self.write(view_template(path, pg))
            except wiki.PageNotFound:
                self.write(create_template(path))

class EditPage(tornado.web.RequestHandler):
    def get(self, path):
        pg = wiki.view_raw_page(path, wiki=WIKI_ROOT)
        self.write(edit_template(path, pg))

class DeleteAPI(tornado.web.RequestHandler):
    def post(self, path):
        wiki.delete_page(path, wiki=WIKI_ROOT)
        self.redirect("/" + os.path.dirname(path))

class CreateAPI(tornado.web.RequestHandler):
    def post(self, path):
        wiki.create_page(path, wiki=WIKI_ROOT)
        self.redirect("/edit/" + path)

class EditAPI(tornado.web.RequestHandler):
    def post(self, path):
        new_contents = self.get_argument("new_contents")
        message = self.get_argument("commit_message")
        if not message:
            message = "Minor edit"
        wiki.edit_page(path, new_contents, message, wiki=WIKI_ROOT)
        self.redirect("/" + path)

##### Cosmetics
def main_template(path, contents):
    return """
    &lt;html&gt;
      &lt;head&gt;
        &lt;link rel="stylesheet" href="/static/css/wiki.css" type="text/css" media="screen" /&gt;
      &lt;/head&gt;
      &lt;body&gt;
        {0}
        &lt;div id="content"&gt;{1}&lt;/div&gt;
      &lt;/body&gt;
    &lt;/html&gt;
    """.format(breadcrumbs(path), contents)

def edit_template(path, contents):
    return main_template(path, """
    &lt;form action="/api/edit/{0}" method="POST"&gt;
      &lt;textarea id="new_contents" name="new_contents"&gt;{1}&lt;/textarea&gt;
      &lt;textarea id="commit_message" name="commit_message"&gt;&lt;/textarea&gt;
      &lt;input type="submit" value="Submit" /&gt;
    &lt;/form&gt;""".format(path, contents))

def create_template(path):
    return main_template(path, """
    &lt;p&gt;Page '{0}' not found.&lt;/p&gt;
    &lt;form action="/api/create/{0}" method="POST"&gt;
       &lt;input type="submit" value="Create" /&gt;
    &lt;/form&gt;
    """.format(path))

def view_template(path, contents):
    return main_template(path, """
    &lt;div class="controls"&gt;
       &lt;form action="/api/delete/{0}" method="POST"&gt;
          &lt;input type="submit" value="Delete" /&gt;
       &lt;/form&gt;
       &lt;a href="/edit/{0}"&gt;Edit&lt;/a&gt;
    &lt;/div&gt;
    {1}
    """.format(path, contents))

def list_template(path):
    fs = file_list(path)
    LIs = "".join(["""&lt;li&gt;&lt;a href="/{0}"&gt;{1}&lt;/a&gt;&lt;/li&gt;""".format(p, name) for (name, p) in fs])
    UL = "&lt;ul&gt;{0}&lt;/ul&gt;".format(LIs)
    return main_template(path, UL)

def file_list(path):
    if path:
        local = os.path.join(WIKI_ROOT, path)
    else:
        local = WIKI_ROOT
    full = os.listdir(local)
    return ((f, os.path.join(path, f)) for f in full if not is_hidden(f))

def breadcrumbs(path): 
    if path == "":
        return """&lt;div class="breadcrumbs"&gt;home&lt;/div&gt;"""
    s = re.split(r"[/\\]", path)
    template = """&lt;div class="breadcrumbs"&gt;&lt;a href="/"&gt;home&lt;/a&gt;/{0}&lt;/div&gt;"""
    if len(s) == 1:
        return template.format(s[0])
    elif len(s) == 2 and s[0] == "":
        return template.format(s[1])
    else:
        res = []
        for end in xrange(1, len(s)):
            elem = s[end-1].strip("/\\")
            link = "/" + ("/".join(s[0:end]))
            res.append("""&lt;a href="{0}"&gt;{1}&lt;/a&gt;""".format(link, elem))
        return template.format("/".join(res) + "/" + s[-1])

def is_dir(path):
    return os.path.isdir(os.path.join(WIKI_ROOT, path))

def is_file(path):
    return os.path.isfile(os.path.join(WIKI_ROOT, path))

def is_hidden(path):
    return path.startswith(".")

##### URI Dispatch and Settings
urls = [
    (r"/edit/(.*)", EditPage),
    (r"/api/edit/(.*)", EditAPI),
    (r"/api/create/(.*)", CreateAPI),
    (r"/api/delete/(.*)", DeleteAPI),
    (r"/(.*)", ShowPage)
]

settings = {
    "static_path": os.path.join(os.path.dirname(__file__), "static")
}

##### Main thing
app = tornado.web.Application(urls, **settings)
WIKI_ROOT = "."
if __name__ == "__main__":
    if len(sys.argv) &gt; 1:
        WIKI_ROOT = sys.argv[1]
    print "Starting in", WIKI_ROOT
    app.listen(4848)
    tornado.ioloop.IOLoop.instance().start()
```

Man its been a while. Hopefully, I remember how to do this.

```python
from markdown2 import markdown
from subprocess import call, check_output
import datetime, os
```

Module import boilerplate. Interestingly, though I put it up top out of habit, Python seems to allow you to keep your imports 'till the end so that they don't have to destroy reader flow. I've made a mental note to do something about that.

```python
def view_page(path, wiki="."):
    return markdown(view_raw_page(path, wiki=wiki))

def view_raw_page(path, wiki="."):
    if not is_in_repo(path, wiki): raise NotInRepo()
    try:
        with open(os.path.join(wiki, path), 'r') as f:
            return f.read()
    except IOError:
        raise PageNotFound()
```

A page in the wiki is represented as a file on disk. A wiki is actually just a directory with a `git` repo for history support. There are two ways we might want to look at a single file; either as raw `markdown` when we're making edits, or as HTML when we're just reading. The `view_raw_page` function takes a *relative* path as well as a `wiki` directory, and loads the given file from that wiki. If the specified file exists somewhere outside of the given `wiki` directory, we raise a `NotInRepo` error instead of doing anything. This prevents requesters from getting arbitrary file-system access to our machine by passing `..` as part of their request paths. If the given path *would be* inside of the given `wiki`, and merely doesn't exist, we instead raise a `PageNotFound` error. We'll exploit this for page creation code later.

```python
def delete_page(path, wiki="."):
    if not is_in_repo(path, wiki): raise NotInRepo()
    full = os.path.join(wiki, path)
    if os.path.isfile(full):
        os.remove(full)
        commit(path, "Deleted '" + path + "'", repo=wiki)
        try:
            os.rmdir(os.path.dirname(full))
        except OSError:
            None
    elif os.path.isdir(full):
        raise IsADirectory()
    else:
        raise PageNotFound()
```

Unlike the `view_(raw_)?page` functions above, `delete_page` makes changes to the underlying filesystem. Specifically, it deletes a file in the repo and additionally deletes its containing directory if it's empty after the initial deletion<a name="note-Sun-Mar-01-215910EST-2015"></a>[|1|](#foot-Sun-Mar-01-215910EST-2015). Just as in `view_raw_page`, we check that the page we've been given exists inside the given repo. As much as we don't want to let random HTTP requesters *see* arbitrary files on our system, letting them *delete* arbitrary files would probably be worse. If the page exists, we delete it, then run `rmdir` on its containing directory<a name="note-Sun-Mar-01-215914EST-2015"></a>[|2|](#foot-Sun-Mar-01-215914EST-2015), then commit the changes with a mildly descriptive message. If the path given to `delete_page` is actually a directory, we instead throw a `IsADirectory` error. Arguably, we should let users delete subdirectories and do the obvious thing as a result, but I can't see it coming up in the kind of uses I'm planning to put this to. Finally, if the specified page doesn't exist, we raise a `PageNotFound` error. Again, arguably, we could just silently eat this error, since the result is still "the specified page no longer exists", but I'm being explicit for the moment.

```python
def create_page(path, wiki="."):
    if not is_in_repo(path, wiki): raise NotInRepo()
    full = os.path.join(wiki, path)
    if os.path.exists(full): 
        raise PageExists()
    d = os.path.dirname(full)
    if d and (not os.path.exists(d)): os.makedirs(d)
    with open(full, 'w') as f:
        f.write("# " + path)
    commit(path, "Created '{0}'".format(path), repo=wiki)
```

Creating a page follows the same principles as `delete_page`. First, we check that the specified path will fall inside of the target `wiki`. If the page already exists, we return the explicit `PageExists` error rather than silently ignoring the condition. Then, we make sure that the full directory tree leading up to our new file exists, create the file with a default title equal to its path, and finally commit the changes.

```python
def edit_page(path, contents, message="Minor edit", wiki="."):
    if not is_in_repo(path, wiki): raise NotInRepo()
    try:
        full = os.path.join(wiki, path)
        with open(full, 'w') as f:
            f.write(contents)
        commit(path, message, repo=wiki)
    except IOError:
        raise PageNotFound()
```

Having seen the previous three functions, it should be perfectly obvious how we go about editing an existing page. Sing along this time.


-   Check it's in the repo, one two
-   Apply the given changes, three four
-   Commit the file, five six,
-   Raise an error if it doesn't exist, seven eight


Now for the internals.

```python
def initialize(repo="."):
    call(["git", "init"], cwd=repo)
```

`initialize` is actually not called anywhere at the moment. We instead assume that the user has set up their own repo somewhere before telling `wik` to serve it. If we were automating that step, this is how we'd do it.

```python
def commit(path, message="Minor edit", repo="."):
    call(["git", "add", "--all", path], cwd=repo)
    call(["git", "commit", "-m", message], cwd=repo)
```

The commit procedure, called from all wiki mutators, just calls `git add --all` on the given path<a name="note-Sun-Mar-01-215937EST-2015"></a>[|3|](#foot-Sun-Mar-01-215937EST-2015) followed by `git commit` with the specified message.

```python
def log_of(path, repo="."):
    fmt = "--pretty=format:%x01%H%x00%ct%x00%an%x00%ae%x00%B"
    raw = check_output(["git", "whatchanged", "-z", fmt, "--", path], cwd=repo)
    entries = raw.split("\x01")
    for entry in entries:
        if entry:
            split = filter(identity, entry.split("\x00"))
            yield { "commit_hash": split[0], 
                    "timestamp": datetime.datetime.utcfromtimestamp(int(split[1])),
                    "author_name": split[2],
                    "author_email": split[3],
                    "body": split[4:] }
```

This is another function that isn't really being called yet. It will be at some point, but at the moment I'm not extending a reversion interface to HTTP clients, so we just have the definition.

```python
def identity(a):
    return a
```

Apparently [Python doesn't have a built-in `identity`](http://stackoverflow.com/questions/8748036/is-there-a-builtin-identity-function-in-python). Even though [some](https://docs.python.org/2/library/functions.html#map) built-in [higher-order](https://docs.python.org/2/library/functions.html#filter) functions assume the identity function in certain argument slots. I guess "there should only be one way to do it" doesn't quite translate to "if many users want it, we should implement it once".

```python
def is_in(a, b):
    [ra, rb] = map(os.path.realpath, [a, b])
    return os.path.commonprefix([ra, rb]) == rb

def is_in_repo(path, repo="."):
    p = os.path.join(repo, path)
    return is_in(p, repo) and not is_in(p, os.path.join(repo, ".git"))
```

Almost done. `is_in_repo` is the function that takes a `path` and a `repo` and checks if the first is inside the second. It does this by checking that the given path both `is_in` the given `repo` *and* that it's not `is_in` that repos' `.git` subdirectory. `is_in` just takes two pathnames, canonicalizes them using `os.path.realpath`, and check if the first has the second as a prefix.

```python
########## custom exceptions
class NotInRepo(Exception):
    pass

class IsADirectory(Exception):
    pass

class PageNotFound(Exception):
    pass

class PageExists(Exception):
    pass
```

The last bit of `wiki.py` just defines the custom exceptions you've seen being thrown above. They don't do anything other than `pass`, because the only thing we really care about is that we can tell them apart form built in errors. We don't actually need to store any additional information for our purposes at this point, though I do reserve the right to changes that in the future.

On to `main.py`

```python
import tornado.ioloop, tornado.web, json, os, sys, re
import wiki
```

Again, `import` boilerplate; forgiveness please. Though I guess that I should point out I'm building this mini wiki on top of the [`tornado` asynchronous web server](http://www.tornadoweb.org/en/stable/).

```python
class ShowPage(tornado.web.RequestHandler):
    def get(self, path):
        if path == "" or is_dir(path):
            self.write(list_template(path))
        else:
            try:
                pg = wiki.view_page(path, wiki=WIKI_ROOT)
                self.write(view_template(path, pg))
            except wiki.PageNotFound:
                self.write(create_template(path))

class EditPage(tornado.web.RequestHandler):
    def get(self, path):
        pg = wiki.view_raw_page(path, wiki=WIKI_ROOT)
        self.write(edit_template(path, pg))
```

The `ShowPage` handler takes a `path` variable. If that path designates a directory, or the wiki root `""`, we instead list the given directory by calling the `list_template`. If that path designates an existing file, we show it by calling `wiki.view_page`, and writing the result into the `view_template`. Finally, if the path doesn't designate an existing file, we show the `create_template`. We'll see all of those templates shortly.

The `EditPage` handler takes a `path`, and just writes out the `edit_template`, filled with the result of a call to `wiki.view_raw_page`.

Those were the only two handlers that return actual `HTML`. The rest of them, as you're about to see, merely redirect the caller. Ideally, they'd only return some kind of `JSON`-encoded `ack`, but that would complicate writing a dumb interface. Maybe something for a future version.

```python
class DeleteAPI(tornado.web.RequestHandler):
    def post(self, path):
        wiki.delete_page(path, wiki=WIKI_ROOT)
        self.redirect("/" + os.path.dirname(path))

class CreateAPI(tornado.web.RequestHandler):
    def post(self, path):
        wiki.create_page(path, wiki=WIKI_ROOT)
        self.redirect("/edit/" + path)

class EditAPI(tornado.web.RequestHandler):
    def post(self, path):
        new_contents = self.get_argument("new_contents")
        message = self.get_argument("commit_message")
        if not message:
            message = "Minor edit"
        wiki.edit_page(path, new_contents, message, wiki=WIKI_ROOT)
        self.redirect("/" + path)
```

Those three handlers do the appropriate thing for the `wiki` calls `delete_page`, `create_page` and `edit_page` respectively. The only one that's even mildly complicated is `EditAPI`, which potentially has to pass along a `commit_message` from the client as well as a path. Before we get to the cosmetics, lets skip ahead a bit and see where all these `path` parameters to our handlers are coming from.

```python
urls = [
    (r"/edit/(.*)", EditPage),
    (r"/api/edit/(.*)", EditAPI),
    (r"/api/create/(.*)", CreateAPI),
    (r"/api/delete/(.*)", DeleteAPI),
    (r"/(.*)", ShowPage)
]

settings = {
    "static_path": os.path.join(os.path.dirname(__file__), "static")
}
```

As you can see, the URL dispatch table pairs a `regex` to a particular handler class. That group in each one is going to be passed as an argument to the appropriate method. Note that in this case, they all capture *most* of the incoming URI, but that's certainly not a requirement. You can capture path pieces exactly how you'd think. The only `setting` we're interested in setting is the `static_path`; and that should be the `static` directory relative to this file rather than relative to the directory in which `wik` will eventually be run.

```python
app = tornado.web.Application(urls, **settings)
WIKI_ROOT = "."
if __name__ == "__main__":
    if len(sys.argv) &gt; 1:
        WIKI_ROOT = sys.argv[1]
    print "Starting in", WIKI_ROOT
    app.listen(4848)
    tornado.ioloop.IOLoop.instance().start()
```

Last couple of things. I'm keeping `WIKI_ROOT` as a global constant, because I'm working under the assumption that a particular instance of `tornado` will *only serve one wiki*. This may end up being a faulty assumption later on, in which case I'll need to re-think where and how the directory gets stored. As it stands, it'll be a single global, and as you can see from the `__main__` block, we set it from the first and only command-line arg. At the moment, I'm not even parameterizing the port number, opting instead to use the literal `4848`. That's a note to self; the right thing to do in this situation is would be importing and appropriately configuring/calling [`argparse`](https://docs.python.org/2/library/argparse.html#module-argparse) so that we could pass in a target directory, as well as a port, and maybe some other configuration options. So, you know. Get on that, self.

The last bit we need to go over is the code defining our basic cosmetic templates. I'm fully aware of <a name="note-Sun-Mar-01-220020EST-2015"></a>[|4|](http://tornado.readthedocs.org/en/latest/template.html">`tornado.template`</a>, but didn't bother with it for stuff this minimal<a href="#foot-Sun-Mar-01-220020EST-2015).

```python
def main_template(path, contents):
    return """
    &lt;html&gt;
      &lt;head&gt;
        &lt;link rel="stylesheet" href="/static/css/wiki.css" type="text/css" media="screen" /&gt;
      &lt;/head&gt;
      &lt;body&gt;
        {0}
        &lt;div id="content"&gt;{1}&lt;/div&gt;
      &lt;/body&gt;
    &lt;/html&gt;
    """.format(breadcrumbs(path), contents)
```

The `main_template` contains the basic `html`/`head`/`body` tags, and expects to be passed some `contents` and a `path`. The `contents` are naively templated into a `div#content` tag, while the `path` is passed to `breadcrumbs` for processing.

```python
def breadcrumbs(path): 
    if path == "":
        return """&lt;div class="breadcrumbs"&gt;home&lt;/div&gt;"""
    s = re.split(r"[/\\]", path)
    template = """&lt;div class="breadcrumbs"&gt;&lt;a href="/"&gt;home&lt;/a&gt;/{0}&lt;/div&gt;"""
    if len(s) == 1:
        return template.format(s[0])
    elif len(s) == 2 and s[0] == "":
        return template.format(s[1])
    else:
        res = []
        for end in xrange(1, len(s)):
            elem = s[end-1].strip("/\\")
            link = "/" + ("/".join(s[0:end]))
            res.append("""&lt;a href="{0}"&gt;{1}&lt;/a&gt;""".format(link, elem))
        return template.format("/".join(res) + "/" + s[-1])
```

I found it kind of odd that *this* was the most complicated single procedure in the entire application. Nope, not the exposing a named directory without allowing URL injection, not tracking edits or even figuring out the history of a particular file. It's that stupid little breadcrumb trail of links across the top of every page. So it goes sometimes. If the given path is the root, we *just* return `home`. No links or paths or any other kind of processing. Otherwise, we split the `path` on slashes and see what we get back. If the result is a list of 1 element, we return something like `home/foo`, where `home` is a link to the root and `foo` is the name of the single path element. We do basically the same thing with a path of `len` 2 that has the empty string in the first position. The reason both of these are conditions here is that I did some interpreter testing and found that certain versions of Python split a path like `/blah` into `["blah"]`, while others did `["", "blah"]`, and I wanted to cover at least all the options I've personally observed. Finally, if none of the above are the case, we return something like `home/foo/bar/baz/mumble/file`, and make sure that every path element except for the last one has the appropriate link attached.

```python
def edit_template(path, contents):
    return main_template(path, """
    &lt;form action="/api/edit/{0}" method="POST"&gt;
      &lt;textarea id="new_contents" name="new_contents"&gt;{1}&lt;/textarea&gt;
      &lt;textarea id="commit_message" name="commit_message"&gt;&lt;/textarea&gt;
      &lt;input type="submit" value="Submit" /&gt;
    &lt;/form&gt;""".format(path, contents))

def create_template(path):
    return main_template(path, """
    &lt;p&gt;Page '{0}' not found.&lt;/p&gt;
    &lt;form action="/api/create/{0}" method="POST"&gt;
       &lt;input type="submit" value="Create" /&gt;
    &lt;/form&gt;
    """.format(path))

def view_template(path, contents):
    return main_template(path, """
    &lt;div class="controls"&gt;
       &lt;form action="/api/delete/{0}" method="POST"&gt;
          &lt;input type="submit" value="Delete" /&gt;
       &lt;/form&gt;
       &lt;a href="/edit/{0}"&gt;Edit&lt;/a&gt;
    &lt;/div&gt;
    {1}
    """.format(path, contents))
```

The `edit`, `create` and `view` templates aren't interesting enough to dwell on. They each show some basic controls, and do the appropriate thing on `submit`. I should say, they're not interesting enough to dwell on *yet*. I'm still planning to drop [`codemirror`](http://codemirror.net/) into this project so that you can have pretty highlighting and a comfortable experience in the `edit` interface, but that's about it. From the `create` template, you can create a new page, and from the `view` template, you can either edit or delete the current page.

The last template we've got is

```
def list_template(path):
    fs = file_list(path)
    LIs = "".join(["""&lt;li&gt;&lt;a href="/{0}"&gt;{1}&lt;/a&gt;&lt;/li&gt;""".format(p, name) for (name, p) in fs])
    UL = "&lt;ul&gt;{0}&lt;/ul&gt;".format(LIs)
    return main_template(path, UL)

def file_list(path):
    if path:
        local = os.path.join(WIKI_ROOT, path)
    else:
        local = WIKI_ROOT
    full = os.listdir(local)
    return ((f, os.path.join(path, f)) for f in full if not is_hidden(f))
```

And it does exactly what you'd expect; returns a giant `ul` tag with links to each file and directory visible from the specified path into the wiki. This is another place I'm planning some improvements. Specifically, it would be nice if the entries were arranged alphabetically, with all directories coming before any files, and with appropriate file/directory icons marking them as appropriate. I'll let you know how it goes.

Oh, actually, I guess there were a few utility functions still left to go over, though they're all hopefully self-explanatory.

```python
def is_dir(path):
    return os.path.isdir(os.path.join(WIKI_ROOT, path))

def is_file(path):
    return os.path.isfile(os.path.join(WIKI_ROOT, path))

def is_hidden(path):
    return path.startswith(".")
```

## <a name="entr" href="#entr"></a>`entr`

As a complete aside, writing `wik` was the first time I used [`entr`](http://entrproject.org/) seriously. Because editing the above, especially those templates, required a lot of server restarting, eventually I just started up a separate terminal running

```shell
ls *py static/css/*css | entr -r python main.py ~/wiki-data
```

which started up my server, and killed/restarted it each time I saved any `.py` or `.css` files I was working on. It's pretty useful having this sort of thing automated, though it doesn't *quite* do what I want for `C` development. Really, what I'd want there is something more like [`hsandbox`](https://github.com/niemeyer/hsandbox.git), but running on a file I specified. That's something I may put some work into at some point soon.

## <a name="khan-academy" href="#khan-academy"></a>Khan Academy

Something I've been seriously meaning to get into is some basic math. It's surprising, and somewhat embarrassing, how long I've gone without doing that. So this past week, I finally registered an account over at <a name="note-Sun-Mar-01-220102EST-2015"></a>[|5|](https://www.khanacademy.org">Khan Academy</a> and plowed through the [Combinatorics/Probability lessons](https://www.khanacademy.org/math/probability/probability-and-combinatorics-topic) as well as I could. It still feels like I need to practice and study more, but I have a less shaky grasp of [n-choose-k](http://en.wikipedia.org/wiki/Combination) problems than I used to. I'm not prepared to swear by the information yet, given that I haven't battle-tested it at this point, but I can tentatively recommend the lessons<a href="#foot-Sun-Mar-01-220102EST-2015). They certainly help retention over the moderate term.

## <a name="finally" href="#finally"></a>Finally

I was going to mention the recent Cabal memory-management-fest, in which the current core members got together to discuss the implementations they'd spent the week building. Mine's up <a name="note-Sun-Mar-01-220114EST-2015"></a>[|6|](https://github.com/CompSciCabal/SMRTYPRTY/blob/master/experiments/inaimathi/memory-management/g.c">here</a>, while Scott's are over [here](https://github.com/CompSciCabal/SMRTYPRTY/tree/master/experiments/scott/garbage_collector), and dann hasn't posted anything yet as far as I know. I *was* going to go over each of those, but this piece is already quite a bit longer than I was expecting. Fuck, also, I've been putting some work into exercises for [Learn Lisp the Hard Way.](https://github.com/LispTO/llthw/tree/master/book) At the moment, I'm just working on [section 1-04](https://github.com/LispTO/llthw/blob/master/book/1-04-0-lists.md), but I'm hoping to claw some time together over the next couple of weeks. It's an interesting effort, and I guess technically the second book I've contributed to<a href="#foot-Sun-Mar-01-220114EST-2015). I can't wait to see what kind of impact it has.

Now that I've done an initial proof of this article, it occurs that I opened with "There's not much going on".

Given that the above just gives you some minor thumbnails, and doesn't include anything from my personal life, I have no idea why I did that.


* * *
##### Footnotes

1 - <a name="foot-Sun-Mar-01-215910EST-2015"></a>[|back|](#note-Sun-Mar-01-215910EST-2015) - It does not, as of this writing, do that recursively, but probably should. Note to self.

2 - <a name="foot-Sun-Mar-01-215914EST-2015"></a>[|back|](#note-Sun-Mar-01-215914EST-2015) - Ignoring the potential `OSError` thrown if the directory still has something in it.

3 - <a name="foot-Sun-Mar-01-215937EST-2015"></a>[|back|](#note-Sun-Mar-01-215937EST-2015) - The `--all` is really only necessary for deletions, but it's easier to call it everywhere instead of dispatching, or exposing an extra flag argument to let the caller decide whether to add it.

4 - <a name="foot-Sun-Mar-01-220020EST-2015"></a>[|back|](#note-Sun-Mar-01-220020EST-2015) - Also, I'm not sold on the idea of mixing HTML with random code in arbitrary languages. [`:cl-who`](http://weitz.de/cl-who/) and [similar](https://github.com/weavejester/hiccup#hiccup) have taught me to expect somewhat more elegant generation machinery.

5 - <a name="foot-Sun-Mar-01-220102EST-2015"></a>[|back|](#note-Sun-Mar-01-220102EST-2015) - Though I will say that I'm not sure I'd recommend any of the lessons that have anything to do with code. They all take an extremely imperative bent and pretty severely over-complicate some problems. The particular offender that sticks out to me is Merge sort, which I learned through the very simple functional approach, but they expect you to do [in-place](http://algs4.cs.princeton.edu/22mergesort/Merge.java.html). Not that knowing that is bad, but it seems backwards to teach it *first*.

6 - <a name="foot-Sun-Mar-01-220114EST-2015"></a>[|back|](#note-Sun-Mar-01-220114EST-2015) - Though [the first one](https://github.com/Inaimathi/500lines) is still going through editing phases.
