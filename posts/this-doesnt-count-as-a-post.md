I've been working on a little mini-project for work which will eventually let people search for [500px](https://500px.com/) photos by color, in addition to the usual keyword searches. I don't think I can show you the [`go`](http://golang.org/) code according to my contract, but I think I can talk about a couple hiccups I hit on the way.

## `database/sql` variable syntax is DB-dependent

This doesn't always work

```go
...
db.Exec("SELECT * FROM table WHERE id=?", anId)
...
```

It'll work if your back-end is a `sqlite` database because that's the appropriate syntax for that back-end. If you're using the [`Postgres`](http://www.postgresql.org/) drivers [for golang](https://github.com/lib/pq) though, you'll need to write something mildly different.

```go
...
db.Exec("SELECT * FROM table WHERE id=$1", anId)
...
```

This wouldn't even have been so bad, because [it looks like](http://stackoverflow.com/questions/31194630/go-sql-syntax-error) I'd have gotten a pretty sane error message out. However, if you have the misfortune of making this mistake on an `INSERT` statement, rather than a `SELECT`...

```go
...
db.Exec("INSERT INTO table (a, b) VALUES (?, ?)", anA, aB)
...
```

... you'll instead be treated to the profoundly uninformative `pq: Syntax error at or near ")"`. Again, the correct syntax *varies by database driver*, so in my case, I should have written

```go
...
db.Exec("INSERT INTO table (a, b) VALUES ($1, $2)", anA, aB)
...
```

## `cube` is an extension

Turns out Postgres has some syntax for [n-dimensional points and cubes](http://www.postgresql.org/docs/9.1/static/cube.html). It's not "built-in" though; you need to enable it on a DB-by-DB basis. You do this by running `CREATE EXTENSION cube` in the `psql` prompt. Not a big deal, just trying to be complete here.

## `DISTINCT` is weird

So apparently, when you [use the `DISTINCT` clause in Postgres, you can't `ORDER BY` **except by things in your `DISTINCT` items**](http://stackoverflow.com/questions/10261627/how-to-order-distinct-tuples-in-a-postgresql-query). In other words, to pull a completely random and unrelated example out of thin air, if I want to get a bunch of `image_id`s ordered by their color-record distance from a particular 4-dimensional point, I'm SOL. If I try to do this with a nested `ORDER BY` query inside of a `DISTINCT image_id` call, I don't get properly ordered input, and I can't do a direct `DISTINCT` unless it includes the distance property. The whole point in my case is that there might be multiple color hits within a query that point to the same image; in other words, their distances *will* be `DISTINCT` which doesn't help at all.

What I ended up doing is collecting the naive, duplicates-included result out and de-duplicating it myself after the fact. This is not a satisfying conclusion, but at least it *is* one. I'll see if I can come up with something better.

## `go` kind of sucks

At this point, something on the order of a third of my `go` code looks like

```go
if err != nil {
	return err
}
```

I'm told this is just what happens when you write `go` code, and one guy went as far as saying "Oh, this sounds like what happens with C code". Boy, I wish there was [some way to express the pattern of "This computation may fail" that's been well-known since before I was born](https://en.wikipedia.org/wiki/Monad_%28functional_programming%29). I really hope some research effort gets put into this, because I have no idea how to solve the problem.

![The Joker eating some chips in a deadpan fashion](/static/img/joker-chips.gif)

Over-all, `go` feels like a few good ideas strapped to a truly ridiculous amount of mother-may-I code, topped with an [already-noted preference for self-flagellation](/posts/arbitrary-update-4701#the-go-problem) among its community. At this point, if I end up using it outside of work again, it's going to be almost exclusively for the web server. This isn't a language that helps me write understandable code; it holds some interesting libraries hostage and refuses to release them until a boilerplate quota is met. As a rule, that's not something I like enabling.

## Kicktracker going well

I put up [kicktracker](https://github.com/Inaimathi/kicktracker) a little while ago with almost zero fanfare, and I've used it to hop onto quite a few kickstarters that I might otherwise have missed. Except that [Rahdo](https://www.youtube.com/user/rahdo/videos) keeps beating me to most of them. Ever since I started seriously paying attention to the project feeds, I've realized how many just godawful game ideas are pushed out to the clicking masses. Which is good, bottom line. Because the fact that I'm seeing lots of shitty ideas means that the cost of pushing an idea is low enough that even shitty ones can afford getting pushed. Anyway, if you look at the intersection of `"Kickstarters that Kicktracker has Captured For Me"`, `"Kickstarters I'm Interested In"` and `"Kickstarters Rahdo Runs Through"`, the overlap is near-total. Not 100%, but high enough that if you have a casual interest in board-games, you might be better served by a subscription to [his video chanel](https://www.youtube.com/user/rahdo) than a [gulp from the firehose](http://kicktracker.inaimathi.ca/board-games).
