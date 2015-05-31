package main

import (
	"fmt"
	"flag"
	"net/http"
)

func main() {
	var port = flag.Int("port", 4444, "Specify the TCP port this server should listen on. Defaults to 4444")
	flag.Parse()

	arc, _ := MkArchive("posts.json")
	
	http.HandleFunc("/", MainPage(arc))
	http.HandleFunc("/posts/", PostPage(arc, "/posts/"))
	http.HandleFunc("/archive", ArchivePage(arc))
	http.HandleFunc("/archive/by-tag/", ArchiveByTagPage(arc, "/archive/by-tag/"))
	http.HandleFunc("/feed/atom", AtomFeed(arc))
	http.HandleFunc("/feed/atom/by-tag/", AtomFeedByTag(arc, "/feed/atom/by-tag/"))
	http.HandleFunc("/links", StaticMarkdown("links", "static/content/links.md"))
	http.HandleFunc("/meta", StaticMarkdown("meta", "static/content/meta.md"))

	http.HandleFunc("/archive/by-tag", OldArchiveByTagPage(arc))
	http.HandleFunc("/article", OldPostPage(arc))

	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.Dir("static"))))

	fmt.Println("Listening on", *port, "...")
	err := http.ListenAndServe(fmt.Sprintf(":%d", *port), nil)
	if err != nil {
		fmt.Println("ERROR: ", err)
	}
}
