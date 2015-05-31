package main

import (
	"fmt"
	"flag"
	"os"
	"io"
	"bytes"
	"path"
	"strings"
	"encoding/json"
)

func main() {
	var ttl = flag.String("title", "", "Manually specify the title of the new post")
	flag.Parse()

	if len(flag.Args()) < 2 { 
		fmt.Println("Needs at least three arguments")
		fmt.Println("   new.go [--title \"Manual Title\"] [new-post.md] [tags...]")
		return
	}

	fname := flag.Arg(0)
	title := strings.TrimSuffix(path.Base(fname), path.Ext(fname))
	if *ttl != "" { title = *ttl }

	err := newPost(fname, title, flag.Args()[1:])
	if err != nil { fmt.Println("ERROR:", err)}
}

func newPost(fname string, title string, tags []string) error {
	db, err := os.OpenFile("posts.json", os.O_RDWR, 0660)
	if err != nil { return err }
	defer db.Close()
	newId, _ := lineCount(db)

	stat, err := os.Stat(fname)
	stamp := stat.ModTime().Unix()
	if err != nil { return err }
	post := Post{ newId, title, path.Base(fname), stamp, stamp, tags }

	fmt.Println(post)
	enc := json.NewEncoder(db)
	err = enc.Encode(post)
	if err != nil { return err }
	return nil
}

type Post struct {
	Id int
	Title string
	File string
	Edited int64
	Posted int64
	Tags []string
}

func lineCount(r io.Reader) (int, error) {
	buf := make([]byte, 8196)
	count := 0
	lineSep := []byte{'\n'}

	for {
		c, err := r.Read(buf)
		if err != nil && err != io.EOF {
			return count, err
		}
		
		count += bytes.Count(buf[:c], lineSep)
		
		if err == io.EOF {
			break
		}
	}
	return count, nil
}
