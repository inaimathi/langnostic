package main

import (
	"io"
	"bytes"
	"fmt"
	"io/ioutil"
	"html/template"
	"net/http"
	"strings"

	"github.com/microcosm-cc/bluemonday"
	"github.com/russross/blackfriday"
)

func MainPage (arc *Archive) func (http.ResponseWriter, *http.Request) {
	intro, _ := ProcessMarkdown("static/content/intro.md")
	return func (w http.ResponseWriter, r *http.Request) {
		arc.Reload()
		post := arc.Posts[len(arc.Posts)-1]
		pg := Page{
			"welcome", "blog", 
			template.HTML(Cat(intro, []byte("<hr />"), arc.RenderPost(post))), 
		}
		base.Execute(w, pg)
	}
}

func PostPage (arc *Archive, trimmed string) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		arc.Reload()
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
		arc.Reload()
		buf := new(bytes.Buffer)
		WriteLinksList(buf, arc.Posts)
		buf.Write([]byte("<hr />"))
		arc.WriteTagsList(buf)
		base.Execute(w, Page{"archive", "archive", template.HTML(buf.Bytes())})
	}
}

func ArchiveByTagPage (arc *Archive, trimmed string) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		arc.Reload()
		tag := r.URL.Path[len(trimmed):]
		base.Execute(w, Page{fmt.Sprintf("%s tag", tag), "archive", arc.ArchiveByTag(tag)})
	}
}

func (arc *Archive) ArchiveByTag (tag string) template.HTML {
	buf := new(bytes.Buffer)
	posts := arc.TagTable[tag]
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
	md, _ := ProcessMarkdown(fname)
	pg := Page{title, title, template.HTML(md)}
	return func (w http.ResponseWriter, r *http.Request) {
		base.Execute(w, pg)
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
	body, _ := ProcessMarkdown(Cats("posts/", post.File, ".md"))
	title := Cats("<h1>", post.Title, "</h1>")
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

func ProcessMarkdown (mdFile string) ([]byte, error) {
	f, err := ioutil.ReadFile(mdFile)
	if err != nil { return nil, err}
	unsafe := blackfriday.MarkdownCommon([]byte(f))
	return bluemonday.UGCPolicy().SanitizeBytes(unsafe), nil
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
func Cats (seqs ...string) string {
	var buf bytes.Buffer
	for i := range seqs {
		buf.WriteString(seqs[i])
	}
	return buf.String()
}

func Cat (seqs ...[]byte) []byte {
	var buf bytes.Buffer
	for i := range seqs {
		buf.Write(seqs[i])
	}
	return buf.Bytes()
}
