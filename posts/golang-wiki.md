So you may have noticed that I took down that <a name="note-Sun-Apr-05-173817EDT-2015"></a>[|1|](#">Python wiki</a>[Go](#foot-Sun-Apr-05-173817EDT-2015) project a bit ago. Mainly, because I was planning to re-write it in <a href="http://golang.org/) for educational and entertainment purposes. I ... kinda like it. Not like "this will be a `golang` blog from now on" levels of affection, I think the Lisps have that honor thoroughly claimed, but it's still a reasonably comfortable language to work in. This is due in no small part to their [fantastic package documentation](http://golang.org/pkg/)<a name="note-Sun-Apr-05-173822EDT-2015"></a>[|2|](#foot-Sun-Apr-05-173822EDT-2015), and the fact that [the language compiler](https://golang.org/dl/) comes with a module installation tool that you can basically just point at appropriate `github` projects with the effect you probably expect.

Anyway, the transformation process is sufficiently advanced that I can talk about it now.

## <a name="why-wiki" href="#why-wiki"></a>Why Wiki?

Because it's simple enough to put together in a weekend or so, has enough tricky edges that it'll take me through most of the potential sore-points of a new language, and it's still vaguely entertaining to try.

Also, `gitit` still doesn't compile properly via `nix`, so I may actually find myself in the situation of *using* this toy project for something.

## <a name="what-wiki" href="#what-wiki"></a>What Wiki?

My goals here are minimal:


-   a simple filesystem-backed wiki
-   with [[`markdown`]] support
-   that properly tracks full history for all documents involved


I'm pointedly not worried about other markup dialects, nor am I worrying about exporting the data as anything other than `.md` files (or naively as a `git` repo). That still gets you through a lot of what you'll need out of a language. Even a project as minimal as this will force me to interact with command-line argument parsing, executing external commands, file I/O, markdown parsing, HTML templating and serving HTTP.

## <a name="how-wiki" href="#how-wiki"></a>How Wiki?

To start with, the project page is [here](https://github.com/Inaimathi/wik). I don't intend to take this one down, and even though this is an entirely toy project, literate patches welcome.

The project is partitioned into three files; [`main.go`](https://github.com/Inaimathi/wik/blob/master/main.go), [`server.go`](https://github.com/Inaimathi/wik/blob/master/server.go) and [`wiki.go`](https://github.com/Inaimathi/wik/blob/master/wiki.go). There aren't any [unit tests](http://golang.org/pkg/testing/) yet. I'm saving that ToDo item for a rainy day. The idea is to somewhat separate the content serving components from the wiki components. The only reason I haven't gone as far as putting them in separate modules is that I haven't quite figured out how to yet. I'll see what I can do about that. Conceptually though, you'd want the `Wiki` `struct` handling anything to do with actual data interaction, and the server component figuring out how to expose that data to clients over the network. `main` is really just the start-up code, which is why we'll take a look at it first.

```go
// main.go
package main

import (
        "fmt"
        "flag"
        "strings"
        "net/http"
)

func main() {
        var port = flag.Int("port", 8080, "Specify the TCP port this server should listen on. Defaults to 8080.")
        flag.Parse()

        wik := &Wiki{strings.TrimRight(flag.Arg(0), "/")}
        WikiHandlers(wik)

        fmt.Printf("Serving %s\n", flag.Arg(0))
        fmt.Printf("Listening on local port %d...\n", *port)
        
        static := http.FileServer(http.Dir("static"))
        http.Handle("/static/", http.StripPrefix("/static/", static))

        http.ListenAndServe(fmt.Sprintf(":%d", *port), nil)
}
```

In `go`, command-line arguments wind up in `os.Args`. That's a `[]string` whose first element is the name of the invoked program, and whose rest is the list of command line arguments split on `" "`. If you want to do anything even remotely fancy, you'll need to use an argument parser. Go comes with one built-in, in the form of the [`flag`](http://golang.org/pkg/flag/) package. In this case, we're only expecting two things in the arguments list; an optional `--port` parameter that specifies our target TCP port, and a mandatory argument that specifies a wiki directory to serve up. We create a `Wiki` instance from that directory, and call `WikiHandlers` on it. We then specify the static file directory<a name="note-Sun-Apr-05-174516EDT-2015"></a>[|3|](#foot-Sun-Apr-05-174516EDT-2015), and start the server on the specified port. The two interesting parts of that process are "calling WikiHandlers" and "creating a `Wiki` instance". Lets take those in order.

```go
// server.go
package main

import (
        "os"
        "net/http"
        "html/template"
        "path/filepath"
        "strings"
)

func WikiHandlers (wiki *Wiki) {
        http.HandleFunc("/", ShowPage(wiki))
        http.HandleFunc("/edit/", ShowEdit(wiki))
        http.HandleFunc("/api/remove/", RemovePage(wiki))
        http.HandleFunc("/api/edit/", EditPage(wiki))
        http.HandleFunc("/api/create/", CreatePage(wiki))
}

func ShowPage (wiki *Wiki) func (http.ResponseWriter, *http.Request) {
        show := template.Must(template.ParseFiles("static/templates/show.html", "static/templates/base.html"))
        create := template.Must(template.ParseFiles("static/templates/create.html", "static/templates/base.html"))
        flist := template.Must(template.ParseFiles("static/templates/list.html", "static/templates/base.html"))
        return func (w http.ResponseWriter, r *http.Request) {
                p, err := wiki.Local(r.URL.Path)
                if err == nil { 
                        info, err := os.Stat(p)
                        if err == nil && info.IsDir() {
                                dir, e := wiki.GetDir(r.URL.Path)
                                if e == nil { 
                                        lst := &List{URI: r.URL.Path, Links: dir}
                                        flist.ExecuteTemplate(w, "base", lst)
                                }
                        } else if err == nil {
                                pg, e := wiki.GetPage(r.URL.Path)
                                if e == nil { 
                                        pg.ProcessMarkdown()
                                        show.ExecuteTemplate(w, "base", pg) 
                                }
                        } else {
                                pg := &Page{ URI: r.URL.Path }
                                create.ExecuteTemplate(w, "base", pg)
                        }
                }
        }
}

func ShowEdit (wiki *Wiki) func (http.ResponseWriter, *http.Request) {
        t := template.Must(template.ParseFiles("static/templates/edit.html", "static/templates/base.html"))
        return func (w http.ResponseWriter, r *http.Request) {
                pg, err := wiki.GetPage(r.URL.Path[len("/edit"):])
                if err == nil { t.ExecuteTemplate(w, "base", pg) }
        }
}

func RemovePage (wiki *Wiki) func (http.ResponseWriter, *http.Request) {
        return func (w http.ResponseWriter, r *http.Request) {
                err := wiki.Remove(r.URL.Path[len("/api/remove/"):])
                if err == nil {
                        path := r.URL.Path[len("/api/remove"):]
                        http.Redirect(w, r, filepath.Dir(path), http.StatusFound)
                }
        }
}

func CreatePage (wiki *Wiki) func (http.ResponseWriter, *http.Request) {
        return func (w http.ResponseWriter, r *http.Request) {
                path := r.URL.Path[len("/api/create/"):]
                err := wiki.Create(path)
                if err == nil {
                        http.Redirect(w, r, "/" + path, http.StatusFound)
                }
        }
}

func EditPage (wiki *Wiki) func (http.ResponseWriter, *http.Request) {
        return func (w http.ResponseWriter, r *http.Request) {
                path := r.URL.Path[len("/api/edit/"):]
                r.ParseForm()
                body := r.Form.Get("new_contents")
                err := wiki.Edit(path, []byte(body))
                if err == nil {
                        http.Redirect(w, r, "/" + path, http.StatusFound)
                }
        }
}

type List struct {
        URI string
        Links []PageInfo
}

type Crumb struct {
        Name string
        URI string
}

type Trail struct {
        Links []Crumb
        Name string
}

// Breadcrumbs takes a URI and returns the Trail of breadcrumbs that lead to it. 
//   "/"                => {[] "home"}
//   "/test.md"         => {[{"home" "/"}] "test.md"}
//   "/a/b/test.md"     => {[{"home" "/"} {"a" "/a"} {"b" "/a/b"}] "test.md"}
//   "/a/b/c/test.md"   => {[{"home" "/"} {"a" "/a"} {"b" "/a/b"} {"c" "/a/b/c"}] "test.md"}
func Breadcrumbs(uri string) Trail {
        if uri == "/" {
                links := make([]Crumb, 0, 0)
                return Trail { Links: links, Name: "home"}
        }
        split := strings.Split(uri, "/")
        links := make([]Crumb, 0, len(split)+1)
        links = append(links, Crumb{Name: "home", URI: "/"})
        for ix := range split[:len(split)-1] {
                if split[ix] != "" {
                        links = append(links, Crumb{Name: split[ix], URI: strings.Join(split[0:ix+1], "/")})
                }
        }
        if len(split) > 1 {
                return Trail{ Links: links, Name: split[len(split)-1]}
        } else {
                return Trail{ Links: links, Name: ""}
        }
}

// Helper method for templates. Calls Breadcrumbs on the URI of a *Page
func (pg *Page) CrumbsOf() Trail {
        return Breadcrumbs(pg.URI)
}

// Helper method for templates. Calls Breadcrumbs on the URI of a *List
func (lst *List) CrumbsOf() Trail {
        return Breadcrumbs(lst.URI)
}
```

That's the server file. You can see that `WikiHandlers` up top just binds a few URIs to appropriate handler functions. The reason I wanted to cluster that step is that I'm anticipating serving multiple wikis from one server. That'll involve setting up multiple handlers, which will eventually need to be modified slightly per wiki. Something like prefixing a name to the paths you see above. And it seems like doing that would be easier if we kept all the handler definitions together in one place. Because of these plans, the handler functions need to behave in a manner specific to the target Wiki, which means we'll actually want to construct these functions rather than using global names. Notice that each of `ShowPage`, `ShowEdit`, `RemovePage`, `EditPage` and `CreatePage` take a `Wiki` and return a handler function rather than acting as handlers themselves.

Two of those handler factories, `ShowPage` and `ShowEdit`, also deal with [HTML templates](http://golang.org/pkg/html/template/). Those templates are read at the time a set of wiki handlers are created, though if you look through the history of the github project, you'll see that I did read and parse those on each page hit at one point. It was just to make it easier to tweak said templates. If I were *really* concerned with performance, I'd make those top-level variables or perhaps a single top-level template map. Note To Self.

The `ShowPage` handler function is responsible for displaying one of


-   a wiki page (in the case that the specified URL exists and is a page)
-   a list of pages (in the case that the specified URL exists and is a directory)
-   a simple "Create" page (in the case that the specified URL does not designate an existing page)


Similarly, the `EditPage` handler function renders the page editing interface for the user. The other three handlers I mentioned do *something* to the state of the specified wiki, then redirect to an appropriate page. The "something" always ends up being a call to some method on the specified `wiki`, and that's as good a segue as any into...

```go
// wiki.go
package main

import (
        "errors"
        "os"
        "os/exec"
        "path/filepath"
        "strings"
        "io/ioutil"
        "github.com/microcosm-cc/bluemonday"
        "github.com/russross/blackfriday"
        "html/template"
)

type Wiki struct {
        Path string
}

type Page struct {
        Path string
        URI string
        Raw string
        Body template.HTML
}

type PageInfo struct {
        URI string
        Name string
        IsDir bool
}


////////// Mutating operations
// Create creates a new file in the given wiki
func (w *Wiki) Create(path string) error {
        p, err := w.Local(path)
        if (err != nil) { return err }
        err = os.MkdirAll(filepath.Dir(p), 0777)
        err = ioutil.WriteFile(p, []byte("# " + path), 0600)
        if (err != nil) { return err }
        return w.Commit(p, "Created " + path)
}

// Edit changes the contents of a file in the given wiki
func (w *Wiki) Edit(path string, contents []byte) error {
        p, err := w.Local(path)
        if (err != nil) { return err }
        err = ioutil.WriteFile(p, contents, 0600)
        if (err != nil) { return err }
        return w.Commit(p, "Edit to " + path)
}

// Remove removes a file in the given wiki
// TODO - remove the containing directory if empty
func (w *Wiki) Remove(path string) error {
        p, err := w.Local(path)
        if (err != nil) { return err }
        err = os.Remove(p)
        if (err != nil) { return err }
        return w.Commit(p, "Deleted " + path)
}

// Reads a directory on disk and returns a list of os.FileInfo
// for each visible file in the directory.
// If the given directory is not in the given wiki, returns an error instead.
func (w *Wiki) GetDir(path string) ([]PageInfo, error) {
        p, err := w.Local(path)
        if err != nil { return nil, err }
        files, err := ioutil.ReadDir(p)
        if err != nil { return nil, err }
        res := make([]PageInfo, 0, len(files))
        for ix := range files {
                f := files[ix]
                n := f.Name()
                if !strings.HasPrefix(n, ".") {
                        inf := PageInfo{Name: n, URI: filepath.Join(path, n), IsDir: f.IsDir()}
                        res = append(res, inf)
                }
        }
        return res, nil
}

// Reads a page from disk and returns a pointer to the Page constructed from it.
// Does not render input by default; if rendered output is desired, the caller
// should also call .Render on the result of GetPage
func (w *Wiki) GetPage(path string) (*Page, error) {
        p, err := w.Local(path)
        if err != nil { return &Page{}, err }
        body, err := ioutil.ReadFile(p)
        if err != nil { return &Page{}, err }
        return &Page{Path: p, URI: filepath.Clean(path), Raw: string(body) }, nil
}

func (pg *Page) ProcessMarkdown() {
        unsafe := blackfriday.MarkdownCommon([]byte(pg.Raw))
        pg.Body = template.HTML(bluemonday.UGCPolicy().SanitizeBytes(unsafe))
}

////////// Git commands and various utility

// Initialize runs git-init in the directory of the given wiki
func (w *Wiki) Initialize() error {
        return w.ExecIn("git", "init")
}

// Commit runs a git-add/git-commit with the given message and file
func (w *Wiki) Commit(path string, message string) error {
        w.ExecIn("git", "add", "--all", path)
        w.ExecIn("git", "commit", "-m", message)
        return nil
}

// ExecIn executes a command with the wiki directory as CWD.
func (w *Wiki) ExecIn(command string, args ...string) error {
        cmd := exec.Command(command, args...)
        cmd.Dir = w.Path
        return cmd.Run()
}

// Local takes a path and checks if it would fall within the given
// repo if joined with it. Returns either 
//   [sanitized path], nil    // if the given path is valid
//   "", error                // otherwise
func (w *Wiki) Local(path string) (string, error) {
        p := filepath.Clean(filepath.Join(w.Path, path))
        if (strings.HasPrefix(p, w.Path) && !strings.HasPrefix(p, filepath.Join(w.Path, ".git"))) {
                return p, nil
        }
        return "", errors.New("path outside of repo")
}
```

As you can see, the `Wiki` type ends up being a thin wrapper around a path. It's a wrapper in part because Go doesn't let you define methods on existing types, and in part because I'm planning ahead again<a name="note-Sun-Apr-05-180604EDT-2015"></a>[|4|](#foot-Sun-Apr-05-180604EDT-2015). A method looks exactly the same as a regular function, except that it leads with a type specializer rather than a function name.

```go
...
func (w *Wiki) Local(path string) (string, error) {
...
```

Effectively, we've got two arguments. A `Wiki` pointer named `w`, and a `string` named `path`. And we've got Two return values, a `string` and an `error`<a name="note-Sun-Apr-05-180610EDT-2015"></a>[|5|](#foot-Sun-Apr-05-180610EDT-2015). A call to this looks like `aWiki.Local("some/path/here")`, and that `aWiki` part will be implicit first argument, as in most languages with object systems. Incidentally, there's no multiple dispatch here. You can't do something like `func (a *Foo, b *Bar) MethodName(baz string) ...`, and will get a compile-time error if you try.

## <a name="golang-first-impressions" href="#golang-first-impressions"></a>Golang, First Impressions

This is slightly lighter fare than my usual semi-literate programming articles, mainly because I really want to get to a summary of my first impressions of the language. And I get the feeling that it'll get too long if I do the usual deep-dive into every single function and method. A lot of it is pretty self-explanatory anyhow, and most of it comes with inline comments besides. If you were looking for an in-depth syntax primer, I thoroughly recommend [`x in y where x=go`](http://learnxinyminutes.com/docs/go/)

My impression so far, with the disclaimer that I haven't done anything much with the built-in parallelism constructs, is that this is a C derivative and plainly shows its lineage. You have raw pointers<a name="note-Sun-Apr-05-180616EDT-2015"></a>[|6|](#foot-Sun-Apr-05-180616EDT-2015), and a surprising amount of control of memory for a garbage collected language. You have multiple return values, but you mostly use them as a saner way to pull the standard C error-handling trick<a name="note-Sun-Apr-05-180819EDT-2015"></a>[|7|](#foot-Sun-Apr-05-180819EDT-2015), and you have the obviously very similar syntax. None of this is necessarily a bad thing, but I want to drive the point home that this is seems to be a higher-level language built for people that have been using C and very little else for the last few decades.

The compiler yells at you if you `import` modules without using them, and ditto for unused local variables (it *doesn't* yell at you for defining uncalled and unexported global functions for reasons I'm not too clear on. It seems like it could easily at least warn you that you've got a top-level function that you neither export nor call anywhere). I like it if for no other reason than it has forced me to keep my imports organized and minimal.

The web server assumes it'll be up until its process is terminated somehow. There's a [non-trivial way to hack in a `.Stop` method](http://www.hydrogen18.com/blog/stop-listening-http-server-go.html), but nothing built in, and the method I just linked you relies on polling. `go run` also starts your project in a subprocess, which [plays poorly](http://stackoverflow.com/questions/29237500/entr-and-go-run-not-playing-nice) with [`entr`](http://entrproject.org/). That's a shame, because using it is beautiful for languages like go. It's still useful, but only before you get to the point of starting up your server poking at it through a browser.

There isn't much type inference. You only get it when using the `:=` operator, which is a shortcut for assigning a value to a properly typed name. More would have helped in one or two places in the above program. In particular, anywhere you want to return a `func` as a result. For instance, take a look at the beginning of `RemovePage`:

```go
func RemovePage (wiki *Wiki) func (http.ResponseWriter, *http.Request) {
        return func (w http.ResponseWriter, r *http.Request) {
        ...
```

You can clearly see the duplication of that returned `func`s' signature, and it would get worse if you wanted to pass it around a few more times rather than just throwing it into a `http.HandleFunc` call. I'm honestly not sure how annoying this would actually get in the wild, and to be fair, Go isn't trying to be a functional language, so I imagine this sort of higher-order programming isn't intended to be the norm.

Hopefully it's obvious that none of these are such horrific downsides that I'd stop using the language. And, again, I haven't really done enough exploratory programming with [`goroutines`](https://gobyexample.com/goroutines), which I assume will make the language come alive. *And*, it simply and elegantly deals with some of the major headaches other languages suffer from by having [fantastic documentation](http://golang.org/pkg/), and a [built-in package manager](https://golang.org/cmd/go/#hdr-Download_and_install_packages_and_dependencies). So pros and cons. And *mostly* pros. I'll keep you posted regarding what uses I end up putting it to.

* * *
##### Footnotes

1 - <a name="foot-Sun-Apr-05-173817EDT-2015"></a>[|back|](#note-Sun-Apr-05-173817EDT-2015) - No, that doesn't go anywhere.

2 - <a name="foot-Sun-Apr-05-173822EDT-2015"></a>[|back|](#note-Sun-Apr-05-173822EDT-2015) - Which, as I [now know](http://stackoverflow.com/questions/29213429/checking-if-a-file-is-in-a-given-directory), actually has links into the source implementation of the `func`s in question. Specifically, each subsection heading is a link that takes you to the point in source where its definition begins..

3 - <a name="foot-Sun-Apr-05-174516EDT-2015"></a>[|back|](#note-Sun-Apr-05-174516EDT-2015) - Only for testing purposes, really. In real life, I'd have `nginx` serve up the static files.

4 - <a name="foot-Sun-Apr-05-180604EDT-2015"></a>[|back|](#note-Sun-Apr-05-180604EDT-2015) - I get the feeling that the right long-term solution is bundling some of the functions that act on a `Wiki` in with the struct rather than as external methods. For instance, if I wanted to do something like deploy `wik` on [app-engine](https://cloud.google.com/appengine/docs), the back-end interface would have to look much different than the above set of `exec.Command` calls. It would have to interface in some way with the Google [datastore](https://cloud.google.com/appengine/docs/go/datastore/), and possibly the [Users service](https://cloud.google.com/appengine/docs/go/users/). The way this module is written right now, that wouldn't be a very easy change to implement, but it would get a lot easier if each `wiki` could carry around its own internal "setter"/"getter" functions. We can still present a uniform interface to external callers by making the appropriate exported modules call the internal representations' machinery, but that would let us change out said machinery without needing to re-write too much.

5 - <a name="foot-Sun-Apr-05-180610EDT-2015"></a>[|back|](#note-Sun-Apr-05-180610EDT-2015) - Which might be `nil`.

6 - <a name="foot-Sun-Apr-05-180616EDT-2015"></a>[|back|](#note-Sun-Apr-05-180616EDT-2015) - Though thankfully [not pointer arithmetic](https://golang.org/doc/faq#no_pointer_arithmetic).

7 - <a name="foot-Sun-Apr-05-180819EDT-2015"></a>[|back|](#note-Sun-Apr-05-180819EDT-2015) - I've only seen this done rather than done it myself. Apparently it's conventional in some teams to use the primary return value of a procedure in C/C++ as just an error flag. You of course need an actual return value, and the way to do this is apparently by passing an additional pointer argument to the procedure into which said return value is written. I have no opinion on this, except that the C++ error system is [apparently bad enough](http://yosefk.com/c++fqa/defective.html#defect-10) that it doesn't sound completely insane.
