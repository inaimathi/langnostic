package main

import (
	"io"
	"bytes"
	"fmt"
	"io/ioutil"
	"html/template"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/microcosm-cc/bluemonday"
	"github.com/russross/blackfriday"
)

func MainPage (arc *Archive) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		intro, _ := ProcessMarkdown("static/content/intro.md")
		post := arc.LatestPost()
		pg := Page{
			"welcome", "blog", 
			template.HTML(Cat(intro, []byte("<hr />"), arc.RenderPost(*post))), 
		}
		base.Execute(w, pg)
	}
}

func PostPage (arc *Archive, trimmed string) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		post := arc.PostBySlug(r.URL.Path[len(trimmed):])
		if post == nil {
			base.Execute(w, Page{"not found", "blog", template.HTML("No such post...")})
		} else {
			base.Execute(w, Page{"post", "blog", template.HTML(arc.RenderPost(*post))})
		}
	}
}

func ArchivePage (arc *Archive) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		buf := new(bytes.Buffer)
		WriteLinksList(buf, arc.AllPosts())
		buf.Write([]byte("<hr />"))
		arc.WriteTagsList(buf)
		base.Execute(w, Page{"archive", "archive", template.HTML(buf.Bytes())})
	}
}

func ArchiveByTagPage (arc *Archive, trimmed string) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		tag := r.URL.Path[len(trimmed):]
		base.Execute(w, Page{fmt.Sprintf("%s tag", tag), "archive", arc.ArchiveByTag(tag)})
	}
}

func (arc *Archive) ArchiveByTag (tag string) template.HTML {
	buf := new(bytes.Buffer)
	posts := arc.PostsByTag(tag)
	if len(posts) == 0 {
		fmt.Fprintf(buf, "<p>No posts tagged '%s'...</p>", tag)
	} else {
		WriteLinksList(buf, posts)
	}
	buf.Write([]byte("<hr />"))
	arc.WriteTagsList(buf)
	return template.HTML(buf.Bytes())
} 

func StaticMarkdown (title string, fname string) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		md, _ := ProcessMarkdown(fname)
		base.Execute(w, Page{title, title, template.HTML(md)})
	}
}

////////////////////
// Handlers for backwards-compatibility only
func OldArchiveByTagPage (arc *Archive) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		tag := r.URL.Query().Get("tag")
		base.Execute(w, Page{fmt.Sprintf("%s tag", tag), "archive", arc.ArchiveByTag(tag)})
	}
}

func OldPostPage (arc *Archive) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		post := arc.PostBySlug(strings.TrimSuffix(r.URL.Query().Get("name"), ".html"))
		if post == nil {
			base.Execute(w, Page{"not found", "blog", template.HTML("No such post...")})
		} else {
			base.Execute(w, Page{"post", "blog", template.HTML(arc.RenderPost(*post))})
		}
	}
}

////////////////////
// Rendering helpers
func (arc *Archive) RenderPost(post Post) []byte {
	body, _ := ProcessMarkdown(fmt.Sprintf("posts/%s.md", post.File))
	title := fmt.Sprintf("<h1>%s</h1><span class=\"posted\">%s</span>", 
		post.Title, 
		time.Unix(post.Posted, 0).Format("Jan 2, 2006 at 3:04pm (MST)"))
	return Cat([]byte(title), body, arc.RenderPostLinks(post))
}

func (arc *Archive) RenderPostLinks (post Post) []byte {
	buf := new(bytes.Buffer)
	postLinks.Execute(buf, arc.AdjacentPosts(post))
	return buf.Bytes()
}

func (arc *Archive) WriteTagsList(w io.Writer) {
	fmt.Fprintf(w, "<h3>Tags</h3><ul class=\"tags-list\">")
	for i := range arc.Tags {
		t := arc.Tags[i]
		fmt.Fprintf(w, "<li><a href=\"/archive/by-tag/%s\">%s</a>(%d)</li>", t, t, len(arc.TagTable[t]))
	}
	fmt.Fprintf(w, "</ul>")
}

func WriteLinksList(w io.Writer, posts []Post) {
	w.Write([]byte("<ul>"))
	for i := range posts {
		fmt.Fprintf(w, "<li><a href=\"/posts/%s\">%s</a></li>", posts[i].File, posts[i].Title)
	}
	w.Write([]byte("</ul>"))
} 

type Cached struct {
	contents []byte
	lastChecked time.Time
	lastEdited time.Time	
}

var mdCache = make(map[string]Cached)
var LangnosticPolicy = 
	bluemonday.UGCPolicy().AllowAttrs("name").OnElements("a").AllowAttrs("alt").OnElements("img").AllowAttrs("title").OnElements("img").AllowAttrs("style").OnElements("span")

func ProcessMarkdown (mdFile string) ([]byte, error) {
	cache, present := mdCache[mdFile]
	if present && (CacheLimit > time.Since(cache.lastChecked)) {
		return cache.contents, nil
	} 
	
	stat, err := os.Stat(mdFile)
	if err != nil { return nil, err }
	if present && (cache.lastEdited == stat.ModTime()) {
		// Just doing `cache.lastChecked = time.Now()` doesn't do what you'd expect
		mdCache[mdFile] = Cached{cache.contents, time.Now(), cache.lastEdited}
		return cache.contents, nil
	} 

	f, err := ioutil.ReadFile(mdFile)
	if err != nil { return nil, err }

	unsafe := blackfriday.MarkdownCommon([]byte(f))
	mdCache[mdFile] = Cached{LangnosticPolicy.SanitizeBytes(unsafe), time.Now(), stat.ModTime()}
	return mdCache[mdFile].contents, nil
}

////////////////////
// Template-related stuff
var base = template.Must(template.ParseFiles("static/templates/base.html"))
var postLinks = template.Must(template.ParseFiles("static/templates/post-links.html"))

type Page struct {
	Title string
	Section string
	Body template.HTML
}

////////////////////
// General utility
func Cat (seqs ...[]byte) []byte {
	var buf bytes.Buffer
	for i := range seqs {
		buf.Write(seqs[i])
	}
	return buf.Bytes()
}
