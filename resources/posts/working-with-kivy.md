So my app development journey is going ... ok.

I've spent a bunch of hours at this point staring at Android permission/library/compilation errors and I'm still about as annoyed about it as ever. It'd be really nice if, once I had a project that ran and tested fine with `python main.py`, _also_ ran equally fine on my target Android device.

This is not a thing.

So far, I've had a few headdesk situations where I just left out an import when trying to reproduce a minimal error, but a few things strike me as not exactly my fault that I still had to crunch through. [This SO answer](https://stackoverflow.com/a/70552433/190887) was the friggin' MVP in diagnosing all of them; the TLDR is `adb logcat -s python` and use `scrcpy` to mirror your Android devices' screen on your development machine. You can install them with the obvious `sudo apt install adb scrcpy` in Ubuntu. The partial, still very probably incomplete list of the errors in question is

1. You can't just add new requirements to `requirements.txt`, you also need to add them to the `requirements` line in `buildozer.spec`
2. If you want to deploy some static files, you need to add those to the `source.include_exts` line in `buildozer.spec`. This bit me when I wanted to include some custom fonts. The line's value defaults to `py,png,jpg,kv,atlas`, so if you're trying to include anything else in your bundle, you need to add it.
3. As of this writing, `buildozer` doesn't support `cython` 3, but `cython`'s latest is `3.1.0`. So you have to add the admittedly adorable `cython<3.0.0` to your requirements rather than latest or unpinned.
4. If you're dealing with any library that uses [BeautifulSoup](https://pypi.org/project/beautifulsoup4/) in the dependency tree, you need to _manually and explicitly_ add `bs4`, `beautifulsoup4` and `soupsieve` to the requirements line in `buildozer.spec` otherwise you'll get mysterious build errors. `cssselect` is one such library, just in case you didn't realize that.
5. `UrlRequest` doesn't transparently work on Android. It looks like it's got something to do with SSL and network permissions. See [here](https://stackoverflow.com/questions/59145035/android-app-built-using-kivy-fail-to-access-internet) and [here](https://github.com/Petar-Luketina/Firebase-Sample/blob/master/buildozer.spec) for details; in particular it looks like you might need to add `openssl` to the `requirements` line in `buildozer.spec`. What do you think? Does it work now?

## Fuck You

No. Of course not. You knew that as soon as you saw there was a header in the middle of that last bullet point. But you don't get the answer that easily, motherfucker, you're coming _with_ me on this one. According to StackOverflow and Google Groups, the answer might be

- You need to [shadow `ssl._create_default_https_context`](https://stackoverflow.com/a/69532202/190887)
- Or maybe, you need to [make an Android permission request at runtime](https://stackoverflow.com/a/77482630/190887) to get the OS to grant you `INTERNET` capabilities
- Possibly, given that this seems like it might be an SSL certificate issue, you might need to [install `certifi` and then pass `certifi.where()` to `UrlRequest`](https://stackoverflow.com/a/58165533/190887) when you start it
- Or maybe you need to handle the [response body with kid gloves if it is/isn't JSON](https://groups.google.com/g/kivy-users/c/M0wHNwy0auQ)?

I was also told that I should possibly add `hostpython3`, `certifi`, `pyopenssl`, `openssl` or a hundred other things to the `requirements` line in `buildozer.spec`.

None. Of. That. Bullshit. Works. OR. MATTERS. **AT ALL.**

And _nobody fucking knew this or documented it anywhere_. So if you're reading this post, and it eventually enlightens you, [you're welcome](https://stackoverflow.com/a/77485315/190887). Pay it forward.

What you actually have to do is

1. Add `android.permission.INTERNET` and `android.permission.ACCESS_NETWORK_STATE` to the `android.permissions` line in `buildozer.spec`. You _don't_ have to explicitly ask for either of these at runtime; it's possible that other permissions require that treatment, but these two just need to be declared once in the `spec` file and don't require user interaction later.
2. That's it.

You don't need `openssl`, or `hostpython3`, and some [permissions](https://python-for-android.readthedocs.io/en/latest/apis/#runtime-permissions) might need runtime requests but it ain't `INTERNET`, and you definitely, _absolutely_ shouldn't rebind subcomponents of `ssl` to equivalent-but-less-secure subcomponents. Once you've done that one specific thing, `buildozer android debug deploy run logcat` will show green, and you'll successfully get HTTP responses to work with.

I have no idea why this isn't a default, why `INTERNET` and `ACCESS_NETWORK_STATE` are separate permissions at all if you need _both_ to generate network requests, or why this doesn't seem to be documented anywhere including the [`permissions` docs](https://python-for-android.readthedocs.io/en/latest/apis/#runtime-permissions) or the [`UrlRequest` example page](https://kivy.org/doc/stable/api-kivy.network.urlrequest.html). The state of the universe is; you'd better just spontaneously know that this is what you need to do.

## Deep Breath

Given the amount of debugging I've been doing here, I haven't done much _actual_ work. But _other_ than that debugging, [Kivy](https://kivy.org/) and [python-markdownify](https://github.com/matthewwithanm/python-markdownify/) are treating me pretty well. They're both relatively simple to work with and flexible enough that I've been able to bend them to my purposes. I'll have a fuller update on what I'm actually up to later. For now, just know that it's coming along reasonably well, and only _temporarily_ reduced me to apoplectic rage.
