It's idea-peeling time, so if you're here for insights, go away. I'm just trying to reason things out to myself.

I'm still trying to tackle the moderation system for [`clomments`](https://github.com/Inaimathi/clomments) in some reasonably simple, reasonably performant way.

The questions I've got around my head are all related to the fact that each person won't be hosting their own server. As much as I'd like that to be the case because of how simple it would make things, it probably won't work out that way. This means that there'll be a need for someone other than the `clomments` server operator to have moderation powers on the server.

The authentication can all be handled using [the OpenID protocol](http://openid.net/)[^openid-note], but moderation in a system like this adds some complexity.

[^openid-note]: Which, just as a side-note, seems to be much simpler and more flexible than I sort of imagined it being. Check out the [wiki page](http://en.wikipedia.org/wiki/OpenID#Technical_Overview) for a breakdown.

Primarily, those complications come from the fact that the model here is a large number of servers each handling the comments sections of a small number of sites. That's not necessarily even a design point, just something I'll have to make peace with if I want to release this under AGPL. So it would be a good idea to avoid having to sign up at each `clomments` server in order to moderate the comments it stores. Establishing ownership of the domain could be handled by the OpenID protocol, but it won't necessarily be doing so; I don't want it to be a requirement that you host your own identity.

So what I'd really want is a way to moderate comments that doesn't involve setting up an account. What I've got in mind is something like

- allow anyone to "moderate" `clomments` on a given page
- moderating doesn't actually delete anything; you can hide or approve a comment
- as part of calling the `clomments` server, the user specifies
      1. the moderator account name(s) for this page
      2. how the page treats new comments (`hide-until-approved` or `show-unless-flagged`)
- the `clomments` server responds with a subset of comments for the given page that fit the specified moderator/behavior

So, what does this get me?

First, a way of moderating `clomments` without keeping track of anything user-related. If I use OpenID and this kind of soft-moderation as the first step, I don't have to care who owns what domain or who moderates what page. An anonymous 4chan-style comments system means that I don't have to care who posts what comment. The only person that has "account information" on the server is ... the person hosting it (their DB user name and password), and that's sort of unavoidable.

Second, likely a bunch of performance headaches. If I'm keeping moderation sets separate from the comment tree, I need a way of merging them for display purposes. I can probably store them in a way that makes it relatively simple to figure out what comments should be shown based on whose approvals/flags need to be taken into consideration.

Third, a bit of added robustness against intrusions. If someone finds out a site owners credentials and "deletes" all comments on a given page, for instance, no real damage has been done. As soon as control of the account is restored, all removed items can be reinstated.

Finally, some potential support for future features. For instance, if I'm letting anyone "moderate", then it would be pretty cool to write a client at some point in the future that lets a viewer apply their own preferences on top of what's been decided by the moderators.

Ok, so I guess I'm definitely doing it this way. The difficult part is really going to be figuring out how to store mod changes in a way that can be queried efficiently along with the comments and then acted upon in the display layer. The simplest way of the top of my head would be something like

```lisp
(def-view-class moderation ()
  ((id :type integer :accessor id :initarg :id :db-constraints (:not-null :auto-increment) :db-kind :key)
   (user-id :type integer :reader user-id :initarg :user-id)
   (comment-id :type integer :reader comment-id :initarg :comment-id)
   (page-id :type integer :reader page-id :initarg :page-id)
   (stat :type keyword :accessor stat :initarg :stat)))
```

At which point we'd get the comments, plus mods, out by calling a method that looks something like

```lisp
(defmethod comments ((page page) &rest moderators)
  (select 'comment 'moderation
          :where [and [null [slot-value 'comment 'parent]]
                      [= [slot-value 'comment 'page-id] (id page)]
                      [= user-id [or moderators]]]))
```

That wouldn't run, incidentally, it's just pseudo-code of what we'd want. Take a page and a list of moderators (in descending order of authority) and get the comment plus the first moderation on the list. And tadaah, what you have is a list of `(comment moderation)` pairs that tell you what should be shown. In fact, it may actually be better to have separate methods for `hide-until-approved` and `show-unless-flagged` sites so that the query itself would only return comments that need to be shown.

It's too late for me to get into thinking about that now though. Hopefully, morning me will have some good ideas on how to translate this bullshit into some vaguely runnable code.
