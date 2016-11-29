So much for updates becoming a regular event I guess. I'm not sure it's for lack of effort; I really do want to write more about my exploits. But it seems like the more I learn, the more I realize how little I know, and that knowledge silences a lot of my output. It seems like a waste, because it's almost certainly not the case that [the stuff I've been learning is common knowledge](http://www.cs.cmu.edu/~rwh/pfpl.html), but most of it would end up being links to specific pages of some dense comp-sci tome or paper, and I'm not sure how useful that would be. So, lets keep it concrete, I guess.

For a massive change, I've been doing some light mobile development. And what I've learned is that it's absolute garbage.

## Why Mobile Development Is Absolute Garbage

For starters, the toolchain is a pile of ass. In order to install and get running a full environment, you need to get [the `SDK`](https://developer.android.com/studio/intro/update.html), [the "IDE"](https://developer.android.com/studio/index.html), [the appropriate emulator image](https://developer.android.com/studio/run/managing-avds.html), and a giant pile of external dependencies, some of which are pinned at odd intermediate versions. As a sidenote, not all of the emulator images are available for every architecture/CPU, and there's no word of warning about this until _after_ you've downloaded the relevant multi-gigabyte files. Thank god for good wi-fi, I guess. The hands down most useful installation guide I've come across so far is [this massive wall of text on SO](http://stackoverflow.com/documentation/android/85/getting-started-with-android/9496/android-programming-without-an-ide#t=20161129053336634177), which explains how the installation runs step-by-step, without relying on the poorly-thought-out automated downloaders.

Once you have all of that stuff down, starting and understanding a project is another herculian task. It's possible that today's Java developers are just so resigned to boilerplate and slog that this doesn't bother them too much, but I've yet to meet one of these people in real life, so I'm not yet inclined to believe they exist. The short, or at least somewhat understandable non-video, version is in [that same massive wall of text on SO](http://stackoverflow.com/documentation/android/85/getting-started-with-android/9496/android-programming-without-an-ide#t=20161129053336634177). That explains how to get a project more-or-less working, but _doesn't_ explain what massive mis-firing of brain-power caused this process to be so byzantine and horrific that it requires this much exposition.

What we ended up doing for the project I'm on is using [React Native](https://facebook.github.io/react-native/). Because, while all of the above is still a requirement, once you have it done, you can at least skip the steps that involve manually mucking with several XML-formatted build manifest files and instead write some fucking javascript. Of course, that's a little pre-mature. We haven't factually got as far as running anything _on a device_ yet, but I'm hoping that the process isn't going to be too stress-inducing.

Honestly, if it weren't for the fact that people other than me need to work on this in the very short term, I very probably would have built the fucker [in Clojure](http://clojure-android.info/) and called it a day. Not only is the toolchain more easily installed on a `debian` machine, it mostly works the way you'd expect from other Clojure development, and compiling down to a device is a matter of `lein droid doall`. It's something I plan to explore for myself even outside the context of this particular contract, but I still find it vaguely unsatisfying that this is even necessary. I'm sure this is how Lispers felt around the 80s and 90s with the rise of personal computers. "Your build-chain involves how many external tools? You have to _allocate memory yourself_? And you can't even **introspect on your running image**? How do you get anything done?" Those are all more or less the objections I'm making here at the next step. I'm not convinced they're wrong.

## [`house`](https://github.com/inaimathi/house)-keeping

You may or may not have noticed a few commits to the [`house`](https://github.com/inaimathi/house) server. Mostly as a result of developments in [`cl-congregate`](https://github.com/inaimathi/cl-congregate). That is, I'm doing my usual and adding features to the underlying substrate exactly as quickly as I need them, and no quicker. They started as concrete requirements for the congregate project, and ended with me addressing some noticeably absent pieces from the `house` system.

### In-page redirects

It used to be possible to set up `redirect-handler`s in `house`, but I hadn't considered the use case wherein a particular page might need to conditionally redirect the client elsewhere.

### Path variables

### Cross-domain sessions

### CORS headers

The CORS thing just outright sucks balls, as far as I'm concerned. There's a decent write-up of [how it works](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing) on wiki, but the _why_ of it escapes me. If it's a security feature, then it sounds misguided to give the origin server the ability to override it by sending back a particular header. If it's to protect servers from DDOS attacks/SNAFUs, then it seems to fail outright because the target still needs to read, buffer and parse the request before it can make the decision to throw it out on the basis of header content. It really seems that you'd always want `Access-Control-Allow-Origin: *` being sent over as part of the response[^reserve-the-right], so that's what `house` now does by default. The change to our `write!` method was a pretty straight-forward

```common-lisp
...
    (write-ln "Access-Control-Allow-Origin: *")
...
```

[^reserve-the-right]: Though, as always, I thoroughly reserve the right to be wrong about this.

## I Fucking Hate This Computer

The trackpad is the single most annoying thing I've had to deal with in recent memory. I haven't gotten around to replacing those stripped screws either, which means that the chassis doesn't hold together quite as well as it should, which _in turn_ means that it's more difficult to use in transit. This in addition to its uncomfortably large size, it's extremely scuff-attracting metal finish and the surprisingly shoddy rubber feet on its underside[^three-of-which].

[^three-of-which]: Three of which have actually fallen off at this point, and I'm considering just cutting the last one off and calling it a day.

In summary, I can not recommend this thing with a straight face, and am seriously considering picking up one of [these](http://www3.lenovo.com/ca/en/laptops/thinkpad/thinkpad-x/X260/p/20F6CTO1WWENCA0) or [these](https://www.amazon.ca/Lenovo-Quad-Core-Processor-Bluetooth-Professional/dp/B01J5YMF90/ref=sr_1_1?s=pc&ie=UTF8&qid=1480397797&sr=1-1) fairly soon. The only thing I'll really miss the horse-power for is my Clojure development. Which is admittedly a large chunk of it these days, but I think I'll be able to deal somehow.
