Ok, I know I've been gone for a while, and possibly owe an explanation for that, but fuck it and fuck you, I'm going straight into "old man yells at cloud" mode.

Mobile development sucks donkey dong.

The last time I did it was back in ... 2015-ish, I guess? I wrote the original [TodaQ](https://todaq.net/) client prototype over a caffeine and oatmeal fueled week of unmitigated hatred and frustration. As I recall, it was such a revolting pile of hacks, poorly-thought-out interfaces, already outgrown intermediate, short-sighted implementations and straight up corporate competition-spawned incompatibilities that the team wisely decided to nope right the fuck out of maintaining any kind of cross-platform compatibility ourselves. Instead opting to run the whole thing from [React Native](https://reactnative.dev/) intermediately compiled from, god help us, [Clojurescript](https://cljsrn.org/).

A quick perusal of the articles contained in that last link will tell you that the most recent thought anyone has given to this was back in 2020, and that's probably not anywhere recent enough to do me any good if I want to actually develop an actual app that I plan to actually use at any point in the near future.

Look, computers are shit, I get it. They're complex and counter-intuitive and baroque and opaque and you need to learn to type with your human fingers to interact with them. Why would you bother when you can just use those same human fingers to poke a screen and drag things around on it? But you know what's even shittier than computers? Tiny, un-openable, not-even-in-principle-comprehensible computers that actively resist your attempts at modifying them in anything but the most perfunctory ways. Do you know what's _much_ better than trawling through the ad-infested, proprietarily-we-own-your-gender-non-specific-testicles-and-ovaries-no-really-licensed app stores prevalent in both the Google and Apple worlds?

`sudo apt` motherfucking `install`, there I said it.

Except you probably have no idea what that means, because I'm a crusty old fart from back when we had to decode our own TCP packets by hand, both ways, uphill, in the snow, and you're a spry, starry-eyed 24 year-old who just got out of a Django bootcamp and doesn't realize that LISP isn't a speech impediment.

# Mobile Development for Hate-Filled but Still Somewhat Competent Old People

Ok, the _next_ best thing to `apt install`, I guess, if I have to have something that runs on my phone and I'm allowed to studiously ignore anything that traces its' lineage back to Cupertino, is writing my own apps and sideloading them into my phone via `apk`s. I guess I could possibly look into getting them on the app store myself at some point in the future, in an at this point purely symbolic effort at supporting the [Free Software](https://www.fsf.org/) ethos of making the world better by building slightly-less-than-hostile-to-user-attention abstract machines. Don't hold your breath though.

My first attempt involved trying to get `cljs` and `figwheel` set up so that I could use something _other_ than Java to give this a shot. I know I threw the Clojurescript name out above kind of flippantly, if you're here you should probably know [my allegiance](/posts/recommendations), but it's positively transcendant compared to the [other options](https://www.netsolutions.com/insights/best-programming-languages-to-write-develop-android-apps/).

That didn't quite work. I [followed](https://medium.com/mindorks/building-mobile-apps-ios-and-android-with-clojurescript-4600235f826c) a [trail](https://github.com/bhauman/lein-figwheel) of [sometimes](https://stackoverflow.com/questions/43667745/react-native-run-android-command-failed-but-gradlew-installdebug-work) contradictory [and](https://developer.apple.com/forums/thread/687489) always [hilarious](https://stackoverflow.com/questions/64968851/could-not-find-tools-jar-please-check-that-library-internet-plug-ins-javaapple), in [that](https://github.com/bhauman/figwheel-main) gallows [humor](https://mkyong.com/java/how-to-install-java-on-mac-osx/) kind of way, links that led me to the [inevitable](https://stackoverflow.com/questions/37900737/make-sure-you-have-an-android-emulator-running-or-a-device-connected-and-have) conclusion that I can't figure this out.

I had to `brew install` more `openjdk`s and `export` more `PATH` modification than I'd like to admit. Oh, also, I also lied above when I said that avoiding Apple stuff was going to be possible here. My current main development machine is a Macbook, laugh it up, so the development environment at least is going to have to be MacOS compatible.

Ok, real talk, my anger isn't directed at you. It's frustrating how much more difficult it is getting any of this environment working than it really should be, and I'm not entirely sure why every piece of documentation I found about any of this seems to just outright assume that you're going to be using Android Studio to build anything. I'm not about to tell you how to live your life, but I've lived mine with [Emacs](https://www.gnu.org/software/emacs/) for about two thirds of the time I've been programming at all, and I'm not about to stop now. After the above Figwheel failures though, I'm going to set my sights lower, just grit my teeth and accept Javascript as my implementation language.

Which, turns out, isn't all that bad.

# Basic Environment Stup

First up

```
brew install nodejs
brew install --cask android-sdk
```

that'll give you a bunch of SDK-level tools in `~/Library/Android/sdk`. In particular, it lets _me_ do

```
/Users/inaimathi/Library/Android/sdk/emulator/emulator -avd Pixel_3a_API_33_arm64-v8a
```

in order to get an emulator up and running. It'll let _you_ do something different, both because your name is not `inaimathi` _and_ because I'm not entirely clear on how exactly the emulator `Pixel_3a_API_33_arm64-v8a` got installed. Yours might be different; check using `~/Library/Android/sdk/emulator/emulator -list-avds`, and possibly install one manually ... somehow?

Next, in another terminal window so your emulator can do

```
npm i -g androidjs-builder
```

once you've got that globally installed, you should have `androidjs` exposed as a command, and in particular, you should be able to do

```
androidjs init
```

to get a skeleton project put together (it'll ask you for the name of your project, no, it doesn't look like you can pass it in as a command-line flag). You should _then_ be able to `cd` into your project directory and do

```
androidjs build ; ~/Library/Android/sdk/platform-tools/adb install dist/yourproject.apk
```

That should build your project into an `apk` and then perform a streamed install into your local emulator. You'll need to "swipe" around it with your mouse to see where it's at, but it should get you up and running despite the meh development loop. Maybe I'll put an `emacs` mode together for this.

# Hang on, Why?

So it turns out that one thing that doesn't seem to exist is a basic workout-routine-aiding app that isn't one of

- Completely unuseable interface-wise
- A subscription service
- Stuffed to the brim with ads
- Useful for things other than the built-in workouts it comes with

I'm not going to recommend anything in the first three categories, but if you're somewhat interested in the last one, check out [BeStronger](https://play.google.com/store/apps/details?id=com.shvagerfm.bestronger). It _is_ a paid app if you don't want the ad flood, but half-way decent after a one-time fee. It can also _technically_ let you build your own workouts, but the process is a lot less streamlined than I'd like given my usual stretching routines.

Anyway, I'd like to change that. So after I've poked around my new environment for a while, I'm going to follow up by putting together a minimal timer app just to satisfy myself that this is a thing that is possible.

Realistically, that's a complete lie. This was an exercise in seeing if I could still blog satisfyingly, and get an Android environment up and running. I'm probably never touching it again, in favor of poking at more interesting things instead.
