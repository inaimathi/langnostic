package main

import (
	"encoding/json"
	"io"
	"os"
	"sort"
	"time"
)

type Post struct {
	Id int
	Title string
	File string
	Edited int64
	Posted int64
	Tags []string
}

type Archive struct {
	Posts []Post
	TagTable map[string][]Post
	Tags []string
	lastRead time.Time
	file string
}

func MkArchive(fname string) (*Archive, error) {
	arc := Archive{}
	arc.file = fname
	err := arc.Reload()
	if err != nil { return nil, err}
	return &arc, nil
}

func (arc *Archive) Reload() error {
	f, err := os.Open(arc.file)
	if err != nil { return err }
	defer f.Close()
	
	stat, err := f.Stat()
	if err != nil { 
		return err 
	} else if stat.ModTime() == arc.lastRead {
		return nil
	} else {
		dec := json.NewDecoder(f)
		res := []Post{}
		tagTable := make(map[string][]Post)
		for {
			pst := Post{}
			err := dec.Decode(&pst)
			if err == io.EOF {
				arc.Posts = res
				arc.lastRead = stat.ModTime()
				tags := make([]string, 0, len(tagTable))
				for k, _ := range tagTable {
					tags = append(tags, k)
				}
				sort.Sort(sort.StringSlice(tags))
				arc.TagTable = tagTable
				arc.Tags = tags
				return nil
			} else if err != nil {
				return err
			}
			res = append(res, pst)
			for i := range pst.Tags {
				tagTable[pst.Tags[i]] = append(tagTable[pst.Tags[i]], pst)
			}
		}
	}
}

type PostLinks struct {
	Prev *Post
	Next *Post
}

func (arc *Archive) AdjacentPosts(post Post) PostLinks {
	lnks := PostLinks{nil, nil}
	if post.Id == len(arc.Posts) -1 {
		lnks.Prev = &arc.Posts[len(arc.Posts)-2]
	} else if post.Id == 0 {
		lnks.Next = &arc.Posts[1]
	} else {
		lnks.Prev = &arc.Posts[post.Id-1]
		lnks.Next = &arc.Posts[post.Id+1]
	}
	return lnks
}

func (arc *Archive) PostBySlug(slug string) *Post {
	for i := range arc.Posts {
		if arc.Posts[i].File == slug {
			return &arc.Posts[i]
		}
	}
	return nil
}
