Ok, so I thought I'd try [this](https://cljsrn.org/) again. [Last time](/posts/working-on-android), I gave up on `cljs`/android in disgust and just hopped over to Python and [Kivy](https://kivy.org/). No regrets so far, and I've got a couple more things on the go that I'm definitely building out with it.

However, having worked a bit with the [`catwalk-fe`](https://github.com/inaimathi/catwalk-fe/), I'm reminded that UI work is fucking gorgeous in LISP-likes. To an extent that is still completely unmatched in languages that focus on object trees or weird YAML-based DSLs. So, granted, [`re-natal`](https://github.com/drapanjanas/re-natal) hasn't seen a commit in 5 years. What's up with the [Expo lein template](https://github.com/seantempesta/expo-cljs-template)?

- Expo is a simple, cross-platform framework for app development. It gives you less device-level control, but lets you develop mobile apps in an _extremely_ nice development loop. I honestly recommend you go try this out; it's a goddamn beautiful prototyping tool.

- The `lein` template is a complete fucking lie. It generates a project that's about 20 major versions behind latest, requires `yarn` and `nvm`, and gets extremely persnickety with you if you don't have exact versions of everything installed.

- The end result of about two hours of poking at different parts of the dependence tree is [this](TODO - error screenshot)

Which I guess means that I'll be trying the [react-native-figwheel-bridge](https://github.com/bhauman/react-native-figwheel-bridge) next. 

Wish me luck, and as always, I'll let you know how it goes.
