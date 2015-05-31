package main

import (
	"text/template"
	"net/http"
	"time"
)

func AtomFeed (arc *Archive) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		arc.Reload()
		w.Header().Set("Content-Type", "application/atom+xml; charset=utf-8")
		atom.Execute(w, LastN(arc.Posts, 25))
	}
}

func AtomFeedByTag (arc *Archive, trimmed string) func (http.ResponseWriter, *http.Request) {
	return func (w http.ResponseWriter, r *http.Request) {
		arc.Reload()
		w.Header().Set("Content-Type", "application/atom+xml; charset=utf-8")
		tag := r.URL.Path[len(trimmed):]
		atom.Execute(w, LastN(arc.TagTable[tag], 25))
	}
}

var atom = template.Must(template.ParseFiles("static/templates/atom.xml"))

func (p Post) UpdatedOn() string {
	return time.Unix(p.Posted, 0).Format(time.RFC3339Nano)
}

func LastN(posts []Post, n int) []Post {
	ct := intMin(n, len(posts))
	latest := make([]Post, 0, ct)
	for i := range posts[len(posts)-ct:] {
		latest = append(latest, posts[len(posts)-(i+1)])
	}
	return latest
}

func intMin(a, b int) int {
	if a > b {
		return b
	} else {
		return a
	}
}
