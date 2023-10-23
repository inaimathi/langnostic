It's definitely not done yet, but hey. Progress is progress. And I _did_ promise I'd let you know how it was going. So.

## On Hosting Public-ish HTTP Servers

There was a situation a little while ago where I had to expose one of my machines to the internet in order to keep tuning it. And I couldn't predict what my own IP address would be, so whitelisting it wasn't a realistic option. A couple hours in, I noticed that the internet hasn't gotten any more orderly since the last time I had to run `tail -f /var/log/htaccess.log`. What I saw was a bunch of external services trying to hook their tendrils into various exploit attack surfaces, none of which I happen to ever expose.

So, as a result, there's a new section in `catwalk` that goes

```python
########## Blacklist
if os.path.exists("blacklist.txt"):
    with open("blacklist.txt", 'r') as f:
        IP_BLACKLIST = set(f.read().splitlines())
else:
    IP_BLACKLIST = set([])

@app.route("/actuator/gateway/routes")
@app.route("/geoserver")
@app.route("/boaform/admin/formLogin")
@app.route("/portal/redlion")
@app.route("/geoserver/web")
@app.route("/cf_scripts/scripts/ajax/ckeditor/ckeditor.js")
@app.route("/.env")
@app.route("/manager/html")
@app.route("/web_shell_cmd.gch")
def add_to_blacklist():
    ip = request.environ.get("REMOTE_ADDR")
    with open("blacklist.txt", 'a+') as bl:
        bl.write(f"{ip}\n")
    IP_BLACKLIST.add(ip)
    return abort(403)

@app.before_request
def block_by_ip():
    ip = request.environ.get("REMOTE_ADDR")
    if ip in IP_BLACKLIST:
        return abort(403)
##############################
```

You can see some of the exploits that I directly witnessed in logs. Near as I can tell, `geoserver`, `actuator` and `redlion` are all industrial firmware hosting frameworks. That `gch` call is something exposed by old cable modems, and `boaform` is some sort of [exploitable fiber optic modem](https://webmasters.stackexchange.com/questions/137142/vulnerability-in-boaform-admin-formlogin).

The idea behind the above block is that

1. There is now a `blacklist.txt` file (which I'll be regularly [publishing in my `github` as I push more commits](https://github.com/inaimathi/catwalk/blob/master/blacklist.txt))
2. Either import or initialize that file on `flask` startup
3. If we get a hit on any of the known exploit endpoints, we immediately blacklist the source IP

This _definitely_ isn't an approach that I'd recommend to anyone doing anything approaching "professional" output, but it works for my particular use case. If _you_ want something like it, but better, take a look at the [`flask-ipban` repo](https://github.com/Martlark/flask-ipban).

## On reading things wrong sometimes

Tortoise TTS [doesn't always pronounce things correctly](https://github.com/neonbjb/tortoise-tts/issues/392). For instance, "TTS" as "tits" instead of "tee tee ess", "ChatGPT" as "Chat" or "Chat gipt" instead of "Chat jee pee tee". It also, surprise, doesn't pronounce emojis at all. _And_ it embarrasses me by pronouncing various Toronto-related words wrong _in my own voice_. I think I could eventually solve some of this by training it further, and I'm going to try that eventually, but I'm also going to do [this](https://github.com/inaimathi/catwalk/blob/master/blogcast/horrifying_hacks.py).

Sometimes, the correct solution is a thing that's nebulous or risky enough that you want some stopgap in the short term. And when that happens, it's helpful to name it in a way that reminds you that it _is_ a stopgap. Hence, `horrifying_hacks.py`. As of this writing, it's

```python
import re

LETTER_PRONOUNCIATIONS = {
    "a": "eh",
    "b": "bee",
    "c": "see",
    "d": "dee",
    "f": "eff",
    "g": "jee",
    "h": "eh ch",
    "i": "eye",
    "j": "jay",
    "k": "kay",
    "l": "el",
    "m": "em",
    "n": "en",
    "o": "oh",
    "p": "pee",
    "q": "cue",
    "r": "are",
    "s": "ess",
    "t": "tee",
    "u": "you",
    "v": "vee",
    "w": "double you",
    "x": "ecks",
    "y": "why",
    "z": "zee"
}

def _acronym(acronym):
    letters = [LETTER_PRONOUNCIATIONS[lt] for lt in acronym.lower()]
    return " ".join(letters)

MISPRONOUNCED_TOKENS = {
    "chatgpt": "Chat jee pee tee",
    "openai": "open eh eye",
    "strachan": "strohn",
}

ACRONYMS = { "gpt", "ai", "api", "tts", "ssh", "http", "url","amd", "cpu", "tldr", "lts" }

UNICODE = {
    "🤗": "hugging face"
}

RE = re.compile(r'(?:\b(?:%s|{u"\U0001F600-\U0001F64F"})\b)|(?:%s)' % (
    '|'.join(ACRONYMS.union(MISPRONOUNCED_TOKENS.keys())),
    '|'.join(UNICODE.keys())
), flags=re.IGNORECASE | re.UNICODE)

def _replace(m):
    low = m.group(0).lower()
    if low in ACRONYMS:
        return _acronym(low)
    if low in MISPRONOUNCED_TOKENS:
        return MISPRONOUNCED_TOKENS[low]
    if low in UNICODE:
        return UNICODE[low]
    return m.group(0)

def apply(string):
    return re.subn(RE, _replace, string)[0]
```

You can see that there are components dealing with acronyms, specific mispronounced tokens and unicode characters that I happen to like using in my blog. The top-level external interface is just `apply`, which takes a string and returns a horrifyingly hacked string that should be better pronounced at the other end.


## On Trying Things Out

Other than basic pronounciation errors, `tortoise` sometimes just straight-up fails to produce audio. I'll get a sentence that starts well, and then cuts off suddenly before all of its' content is spoken. Sometimes I'll even get back an audio file of rustling or spooky whooshing noises instead of anything resembling human speech. These are the failure modes I imagined could be avoided by doing a transcription pass and checking the output of that against the initial input of `tortoise`. That's still something I want to try,and I'm well positioned to do it.

On the flipside, there's already a few places where the recording has pointed to bugs in the original article. Either something that I initially take for a pronounciation error that I then realize is actually a spelling error in the original, or something that ends up sounding awful enough when spoken that I have to go back and re-write the textual sentence. These aren't benefits I was expecting, but I'm thankful for them in any case.

I've put together a small `emacs` interface for myself to compile these audio files effectively and stitch them together. There isn't a full round-trip happening yet, and I won't document that until I have at least a few of my blog posts up as podcasts. The goal at this point is to get a full round trip happening, even if it's initially with some human input, and then let it loose on my archive. There's a few more finicky procedural details I need to work through, but over all, I'm within a few hours of work of having it add a podcast linked alongside each blog post.

You know how this ends by now:

As always, I'll let you know how it goes.
