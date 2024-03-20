Ok, so I've been doing more intermittent work on [`catwalk`](https://github.com/inaimathi/catwalk). There's a bunch of other stuff happening in life, so the project isn't going _exactly_ as well as I wanted, but I'm making progress.

## Basic Server Progress

As of this writing, the latest updates to the [`catwalk` server](https://github.com/inaimathi/catwalk/commit/313a57a54713a7d90983e48233663882dd2fa937) have to do with adding [`CORS` headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS), and the next couple are going to have to do with removing the [blacklist](https://github.com/inaimathi/catwalk/blob/73b22ab52d07db9d808dd1596a0795f26f822d36/main.py#L22-L36) system and adding a fairly general, API-key based `user` system. The medium term plan is to get a bit more elaborate and allow external sign-ups in case someone else wants to use this to make their own blogs into podcasts.

## Web Client Progress

The _immediate_ next steps are all going to have to do with making [the client](https://github.com/inaimathi/catwalk-fe) a lot more performant. The problem is that, for some reason, the [`blogcast` page](https://github.com/inaimathi/catwalk-fe/blob/master/src/catwalk_fe/blogcast.cljs) works really poorly. In particular, when I go to delete certain lines, or edit and re-record them, there's frequent, massive UI pauses that prevent me from moving around the page. There's also some issues I'm still working through regarding [keyboard navigation](TODO), although as you'll understand in a moment, that's going to be less relevant. 

I pushed a few commits recently that I hoped would make the situation more bearable, and let me catch up with my casting work, but they did no such thing. Instead slowing the client to _even more_ of a crawl. I'm not entirely sure why this is happening, and it's entirely possible that it's a direct result of me setting up the state graph for this application in a moronic way. Still, I was hoping to get half-way decent performance out of it and didn't.

## Mobile Client Progress

You probably didn't even realize I was trying for this. Ok, maybe that [series](/posts/android-poking) of [android](/posts/working-on-android) [pokes](/posts/working-with-kivy) tipped my hand, but I don't think I explicitly mentioned what the goal is. What I'd really like with Catwalk is for it to become a web-and-or-mobile-based blogcast tool, not just a web one. I recognize that I could trivially accomplish both, in a sense, by making the web side of it reactive, but it's also sometimes nice to have a native app when you're dealing with a phone screen.

I'd been trying to figure out a way to get both of these things at once, in a nice, enjoyable [`clojure`](https://clojure.org/) context. This doesn't seem to be in the cards, unfortunately. Every attempt I've made to get a [react-native-based CLJS project](https://cljsrn.org/) off the ground has ended up with me hitting roadblocks in the form of stale instructions and/or tooling. The latest one I've tried out is [Expo](https://expo.dev/). Which on its' own looks pretty snazzy, builds trivially on Android, seems to allow for separate web-deployment, and has a really nice experimental remote REPL on-device. If I have to code in Javascript directly in order to use this, then so be it.

## Expo

I've tried out some basics, and no big complaints so far. It looks like there are going to be some finer points regarding what elements specifically to use for native/web display, in particular `SectionList` vs `ScrollView` with a `map` call has significantly different interactions, but no big roadblocks. Setting it up is as easy as

```
~$ yarn create expo-cljs-app your-app-name
...
~$ cd your-app-name
~/your-app-name$ yarn start
```

It then displays a QR code you can scan from the [Anrdoid app](https://expo.dev/go) to start an interactively updating dev preview of your application and go from there. Getting the web client out of it is then as easy as

```
~/your-app-name$ yarn expo export -p web
```

It looks like getting the android version is going to involve changing the value passed to that `-p` flag, and also setting up an `eas.json` file and doing some more stuff through [EAS](https://docs.expo.dev/build/introduction/). I only skimmed the intro, but it doesn't look too intimidating. 

As always, I'll let you know how it goes.
