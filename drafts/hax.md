So, we had a hackathon at the office a little while ago, with the explicit purpose of harnessing generative language models in some interesting way. It was a fun exercise along the lines of the various coding dojos I attended in the before time, and really opened my eyes to some of the options I had in terms of processing data. My expectation was that I'd be using some of the stuff I'd [developed earlier](https://github.com/inaimathi/trivial-openai) and possibly pushing a bit into the realm of prompt engineering/private server deployment.

This turned out to be _kind of_ correct?

## The Language

So first quasi-surprise, the lingua franca of my company internal teams turns out to be Python. I was betting on `nodejs`, or possibly `golang`, but it turns out there's a diversity of language use internally, including me as the one and only Clojure/Emacs user, but it turns out that each individual team hates the first choice language of most of the others. Python isn't anyones' favorite, but it also has no haters, and given that it already has decent bindings to [`replicate`](https://replicate.com/), [`openai`](https://platform.openai.com/) and (in theory) [`vertex`](https://cloud.google.com/vertex-ai), and that it's still intentionally up there in the logo bar, I went ahead with it.

The bindings are a bit more annoying to use than what I put together [over in Clojure land](https://github.com/inaimathi/trivial-openai), but someone else maintains them, and they work. The only hiccup I had was that the authentication procedure for [Vertex AI](https://cloud.google.com/vertex-ai/docs/python-sdk/use-vertex-ai-python-sdk) is what we technically refer to as "batshit insane". I'm sure it makes perfect sense for someone _inside of_ Google, or for someone using a Google branded web interface or possibly Google doc API to hook into their generative capabilities, but if you want to use it from a Python or Clojure script, in the way that the `replicate` and `openai` APIs make damn near trivial, you're shit out of luck. At some point it became clear that putting toghether the authentication subsystem of a Vertex project would take _way_ more effor than any of the actual business logic and I decided to bail on it.

## Replicate and model capabilities

I'd only been using the OpenAI offering before this hackathon, and I've been deliberately ignoring all current-event-related news, so I was mildly surprised by the breadth of options on offer from `replicate`. It turns out you can run [voice generation](https://replicate.com/afiaka87/tortoise-tts), [short video generation](https://replicate.com/nightmareai/cogvideo), [_really nice_ image generation](https://replicate.com/stability-ai/stable-diffusion), [image labelling/description/question-answering](https://replicate.com/andreasjansson/blip-2), and various other minor vision/image-correction tasks. That's in addition to the previously known voice transcription, photo generation and human-level chat. Also, in addition to standardizing model interfaces into a laballed set of `input`s, and exposing a minimal and nice API for it, you can use a tool named `cog` to pull models down and use them locally. Assuming you have a GPU-capable rig, I mean. Which I think basically everyone other than me has at this point. I'm going to see what I can do to change that.

## Blog Reader

One borderline trivial application of the tech out right now is reading blogs. That is, it's relatively easy to write the function `BlogPost -> PodcastEpisode`. It inolves a bunch of API calls against [some TTS model](https://replicate.com/afiaka87/tortoise-tts), and [some image captioning model](https://replicate.com/salesforce/blip/api)[^and-lets-be-honest-here], but the outline is relatively simple.

[^and-lets-be-honest-here]: And lets be honest here, you can go almost arbitrarily deep on the implementation. Making a trivial version is trivial, but making one that also sanely handles footnotes, headings, complex diagrams, different input formats, code blocks, possibly some application-specific acronyms... You could spend a long time working on this even _discounting_ basic background things like proper emphasis and sound error correction.

1. Take a blog post in some format
2. Break it up into readable chunklets, accounting for things like paragraph, headings, lists, code blocks, links, and images (optionally, feed the images into a descriptor model like [`clip`](https://replicate.com/rmokady/clip_prefix_caption))
3. Feed each readable chunklet into something like [`tortoise-tts`](https://replicate.com/afiaka87/tortoise-tts) (optionally doing error trapping/correction by feeding the results back into something like [`whisper`](https://replicate.com/soykertje/whisper))
4. Stitch the results together with variable length silences interspersed to get a decent reading cadence

Here's what I did as a first pass:

```python
import replicate
import requests
import urllib
import json
import re
from bs4 import BeautifulSoup
from PIL import Image
from io import BytesIO
import tqdm
import markdown
import nltk.data
from subprocess import check_output

TOK = nltk.data.load("tokenizers/punkt/english.pickle")
CACHE = {} # Comment this out while working to prevent duplicate API calls in the `importlib.reload` workflow

### Text extraction
def _sanitize(txt):
    return re.sub("’", "'", re.sub("[\[\]]", "", txt.strip()))

def _image_text(url):
    if url in CACHE:
        return CACHE[url]

    img = BytesIO(requests.get(url).content)
    resp = replicate.run(
        "salesforce/blip:2e1dddc8621f72155f24cf2e0adbde548458d3cab9f00c0139eea840d0ac4746",
        input={"image": img}
    )
    caption = re.sub("^Caption: ", "", resp).capitalize()
    CACHE[url] = caption
    return caption

def _element_text(el):
    if el.name == "p":
        return [_sanitize(el.text), {"silence": 0.5}]
    elif el.name == "a":
        return [_sanitize(el.text), "(link in post.)", {"silence": 0.2}]
    elif el.find("img") not in {None, -1}:
        src = json.loads(el.find("img")["data-attrs"])["src"]
        return ["Here we see an image of:", _image_text(src), {"silence": 0.5}]
    elif el.name in {"h1", "h2", "h3"}:
        return [_sanitize(el.text), {"silence": 1.0}]
    elif el.name == "blockquote":
        ps = el.find_all("p")
        if len(ps) == 1:
            return ["Quote:", _sanitize(el.text), {"silence": 0.5}]
        return ["There is a longer quote:"] + [_sanitize(p.text) for p in ps] + [{"silence": 0.5}, "Now we resume the text.", {"silence": 0.5}]
    elif isinstance(el, str):
        if el.strip() == '':
            return []
        else:
            return [el]
    elif el.name in {"ul", "ol"}:
        res = []
        for li in el.find_all("li"):
            res.append(_sanitize(li.text))
            res.append({"silence": 0.5})
        res.append({"silence": 0.5})
        return res
    else:
        print("OTHER", el.name)
        return [el]

def script_from_html(html):
    soup = BeautifulSoup(html, "html.parser")
    return [txt for child in soup.children for txt in _element_text(child)]

def script_from_markdown(md):
    return script_from_html(markdown.markdown(md))

def script_from_substack(post_url):
    parsed = urllib.parse.urlparse(post_url)
    subdomain = parsed.netloc.split(".")[0]
    slug = [p for p in parsed.path.split("/") if p and p != "p"][0]
    url = f"https://{subdomain}.substack.com/api/v1/posts/{slug}"
    resp = requests.get(url).json()
    return [resp["title"], resp["subtitle"]] + script_from_html(resp["body_html"])

### Script normalization
def _break_paragraphs(script):
    for el in script:
        if isinstance(el, str):
            sentences = TOK.tokenize(el)
            if len(sentences) == 1:
                yield el
            else:
                for s in sentences:
                    yield s
                    yield {"silence": 0.1}
        elif isinstance(el, dict):
            yield el

def _merge_silence(script):
    "Merges adjacent silences into longer ones. Also implicitly trims off any trailing silence."
    merged = None
    for el in script:
        if isinstance(el, dict) and "silence" in el:
            if merged is None:
                merged = el
            else:
                merged["silence"] += el["silence"]
        else:
            if merged is None:
                yield el
            else:
                yield merged
                merged = None
                yield el

def normalize_script(script):
    sentences = _break_paragraphs(script)
    merged = _merge_silence(sentences)
    return list(merged)

### Script reader
def download_mp3(fname, url):
    doc = requests.get(url)
    with open(fname, 'wb') as f:
        f.write(doc.content)

def read(text, voice="mol", custom_voice=None):
    model = "afiaka87/tortoise-tts:e9658de4b325863c4fcdc12d94bb7c9b54cbfe351b7ca1b36860008172b91c71"
    inp = {"text": text,
           "voice_a": voice,
           "voice_b": "disabled",
           "voice_c": "disabled"}
    if custom_voice is not None:
        final_inp = {**inp, **{"voice_a": "custom_voice", "custom_voice": voice_file}}
        with open(custom_voice, "rb") as voice_file:
            return replicate.run(model, input=final_inp)
    else:
        return replicate.run(model, input=inp)

def read_script(script, file_prefix="post"):
    pbar = tqdm.tqdm(total = len(script))
    for ix, block in enumerate(script):
        if isinstance(block, dict) and "silence" in block:
            continue
        if block not in CACHE:
            audio_url = read(block)
            fname = f"{file_prefix}-{str(ix).zfill(5)}.mp3"
            download_mp3(fname, audio_url)
            CACHE[block] = {"url": audio_url, "file": fname}
        pbar.update(1)
    pbar.close()


### Sound manipulation
SOX = "sox"
def info(sound_fname):
    res = check_output([SOX, "--i", sound_fname])
    splits = (re.split(" *: +", ln) for ln in res.decode("utf-8").splitlines() if ln)
    return {k.lower().replace(' ', '-'): v for k, v in splits}

def silence(duration, rate=24000, channels=1):
    fname = f"silence-{duration}.mp3"
    check_output([
        SOX, "-n",
        "-r", str(rate), "-c", str(channels), # These must match the downloaded files from `read`, otherwise catting them later is rough
        fname,
        "trim", "0.0",
        str(duration)])
    return fname

def cat(script, output):
    inputs = []
    for block in script:
        if str(block) in CACHE:
            inputs.append(CACHE[str(block)]['file'])
        elif isinstance(block, dict):
            fname = silence(block['silence'])
            CACHE[str(block)] = {'file': fname}
            inputs.append(fname)
    check_output([SOX] + inputs + [output])
    return output
```

This is possibly the least intimidating `python` code I've written in ... I dunno, years? It's literally a bunch of string manipulations and API calls. Literately...

```python
...
def _sanitize(txt):
    return re.sub("’", "'", re.sub("[\[\]]", "", txt.strip()))

def _image_text(url):
    if url in CACHE:
        return CACHE[url]

    img = BytesIO(requests.get(url).content)
    resp = replicate.run(
        "salesforce/blip:2e1dddc8621f72155f24cf2e0adbde548458d3cab9f00c0139eea840d0ac4746",
        input={"image": img}
    )
    caption = re.sub("^Caption: ", "", resp).capitalize()
    CACHE[url] = caption
    return caption

def _element_text(el):
    if el.name == "p":
        return [_sanitize(el.text), {"silence": 0.5}]
    elif el.name == "a":
        return [_sanitize(el.text), "(link in post.)", {"silence": 0.2}]
    elif el.find("img") not in {None, -1}:
        src = json.loads(el.find("img")["data-attrs"])["src"]
        return ["Here we see an image of:", _image_text(src), {"silence": 0.5}]
    elif el.name in {"h1", "h2", "h3"}:
        return [_sanitize(el.text), {"silence": 1.0}]
    elif el.name == "blockquote":
        ps = el.find_all("p")
        if len(ps) == 1:
            return ["Quote:", _sanitize(el.text), {"silence": 0.5}]
        return ["There is a longer quote:"] + [_sanitize(p.text) for p in ps] + [{"silence": 0.5}, "Now we resume the text.", {"silence": 0.5}]
    elif isinstance(el, str):
        if el.strip() == '':
            return []
        else:
            return [el]
    elif el.name in {"ul", "ol"}:
        res = []
        for li in el.find_all("li"):
            res.append(_sanitize(li.text))
            res.append({"silence": 0.5})
        res.append({"silence": 0.5})
        return res
    else:
        print("OTHER", el.name)
        return [el]

def script_from_html(html):
    soup = BeautifulSoup(html, "html.parser")
    return [txt for child in soup.children for txt in _element_text(child)]
...
```

...that's the core of the chunkletifier. It takes HTML input and goes element-by-element, returning the spoken text and surrounding silence specification. Longer silences precede titles, shorter ones surround links and quotes, and a few elements also get their own extra textual treatment. I'm honestly not _exactly_ sure how to deal with `ul`s yet, so this is just an absolutely minimal first crack.

Given that this assumes HTML input, I've also got two utility functions to deal with the test cases I've been running this thing through.

```python
...
def script_from_markdown(md):
    return script_from_html(markdown.markdown(md))

def script_from_substack(post_url):
    parsed = urllib.parse.urlparse(post_url)
    subdomain = parsed.netloc.split(".")[0]
    slug = [p for p in parsed.path.split("/") if p and p != "p"][0]
    url = f"https://{subdomain}.substack.com/api/v1/posts/{slug}"
    resp = requests.get(url).json()
    return [resp["title"], resp["subtitle"]] + script_from_html(resp["body_html"])
...
```

The first one takes a markdown string and, predictably, digests it into HTML before running the previous function on it. The second one takes a [Substack](https://www.astralcodexten.com/p/your-book-review-the-mind-of-a-bee) page, and runs some magic required to get the post content out of it in order to read it.


```python
...
### Script normalization
def _break_paragraphs(script):
    for el in script:
        if isinstance(el, str):
            sentences = TOK.tokenize(el)
            if len(sentences) == 1:
                yield el
            else:
                for s in sentences:
                    yield s
                    yield {"silence": 0.1}
        elif isinstance(el, dict):
            yield el

def _merge_silence(script):
    "Merges adjacent silences into longer ones. Also implicitly trims off any trailing silence."
    merged = None
    for el in script:
        if isinstance(el, dict) and "silence" in el:
            if merged is None:
                merged = el
            else:
                merged["silence"] += el["silence"]
        else:
            if merged is None:
                yield el
            else:
                yield merged
                merged = None
                yield el

def normalize_script(script):
    sentences = _break_paragraphs(script)
    merged = _merge_silence(sentences)
    return list(merged)
...
```

Once a script is output, I want to normalize it. This is both for silence-clustering-related purposes, _and_ because something stupid happens every once in a while where the `tortoise-tts` model outputs garbled audio if its input is too long. The first part of error correction here is chunking out the input as finely as possible. Because this is a first pass, I _haven't_ taken the suggested transcription step to check that the returned audio at least approximates the input text in terms of content.

Ok, so we've got a script now. Next, we read it.

```python
def download_mp3(fname, url):
    doc = requests.get(url)
    with open(fname, 'wb') as f:
        f.write(doc.content)

def read(text, voice="mol", custom_voice=None):
    model = "afiaka87/tortoise-tts:e9658de4b325863c4fcdc12d94bb7c9b54cbfe351b7ca1b36860008172b91c71"
    inp = {"text": text,
           "voice_a": voice,
           "voice_b": "disabled",
           "voice_c": "disabled"}
    if custom_voice is not None:
        final_inp = {**inp, **{"voice_a": "custom_voice", "custom_voice": voice_file}}
        with open(custom_voice, "rb") as voice_file:
            return replicate.run(model, input=final_inp)
    else:
        return replicate.run(model, input=inp)

def read_script(script, file_prefix="post"):
    pbar = tqdm.tqdm(total = len(script))
    for ix, block in enumerate(script):
        if isinstance(block, dict) and "silence" in block:
            continue
        if block not in CACHE:
            audio_url = read(block)
            fname = f"{file_prefix}-{str(ix).zfill(5)}.mp3"
            download_mp3(fname, audio_url)
            CACHE[block] = {"url": audio_url, "file": fname}
        pbar.update(1)
    pbar.close()
```

This is ... exactly what it looks like. `read_script`  goes through a `script`, generated by the previous set of functions, and feeds every text block into the `tortoise-tts` model, ensuring that there is one specific voice doing the reading, and downloads the resulting MP3 files. It also adds them into a local `CACHE` construct. This was _entirely_ to save myself from making a few thousand of _the same_ TTS request over the course of

The last part is sound file manipulation code, all of which calls out to `sox` to do any actual work.

```python
def info(sound_fname):
    res = check_output([SOX, "--i", sound_fname])
    splits = (re.split(" *: +", ln) for ln in res.decode("utf-8").splitlines() if ln)
    return {k.lower().replace(' ', '-'): v for k, v in splits}

def silence(duration, rate=24000, channels=1):
    fname = f"silence-{duration}.mp3"
    check_output([
        SOX, "-n",
        "-r", str(rate), "-c", str(channels), # These must match the downloaded files from `read`, otherwise catting them later is rough
        fname,
        "trim", "0.0",
        str(duration)])
    return fname

def cat(script, output):
    inputs = []
    for block in script:
        if str(block) in CACHE:
            inputs.append(CACHE[str(block)]['file'])
        elif isinstance(block, dict):
            fname = silence(block['silence'])
            CACHE[str(block)] = {'file': fname}
            inputs.append(fname)
    check_output([SOX] + inputs + [output])
    return output
```

It also looks pretty basic to me, but I'm going to let ChatGPT explain this one. Using some new functions from [my `aidev` mini-library](https://github.com/inaimathi/machine-setup/blob/master/emacs/aidev.el).

```emacs
M-x aidev-explain-region RET
```

results in

> This code provides three functions that work with audio files using the command line tool `sox`. Let's go through each function to understand what it does.
>
> 1. `info(sound_fname)`: This function takes a filename `sound_fname` as input and returns a dictionary with information about the audio file. It uses the `check_output` function from the `subprocess` module to execute the `sox --i` command, which returns information about the audio file. The `stdout` of the command is captured and decoded from bytes to a string. The `splitlines` method is used to split the string into lines, and `re.split` is used to split each line into key-value pairs. The resulting key-value pairs are transformed into a dictionary where the keys are converted to lowercase with spaces replaced by hyphens.
>
> 2. `silence(duration, rate=24000, channels=1)`: This function generates a silent audio file with a specified duration. It uses the `check_output` function to execute the following `sox` command:
>
>   ```
>   sox -n -r <rate> -c <channels> <output_filename> trim 0.0 <duration>
>   ```
>
>   The `rate` and `channels` parameters determine the audio properties (defaulting to 24000 Hz and 1 channel). The output filename is determined dynamically based on the specified duration. The generated silent audio file is then returned.
>
> 3. `cat(script, output)`: This function concatenates multiple audio files together into an output file. It takes a list of `script` blocks and an `output` filename as input. For each `block` in the script, it checks if the block is already stored in a `CACHE` dictionary. If it is, it appends the corresponding file path to the `inputs` list. If the block is a dictionary (indicating a silent audio segment), it generates a silent audio file using the `silence` function, stores it in the `CACHE` dictionary, and appends its file path to the `inputs` list. Finally, it executes the following `sox` command using `check_output`:
>
>   ```
>   sox <input_files> <output_filename>
>   ```
>
>   The `inputs` list is constructed by concatenating all the input file paths. The output filename is specified as the final argument. The `check_output` function executes the command, resulting in the concatenation of the input audio files into the output file. The output filename is then returned.
>
> To use this code, you need to have `sox` installed on your system and accessible from the command line. You can then call these functions passing the appropriate arguments to achieve the desired audio processing tasks.
>
> Here are a few ways you might modify this code:
>
> - You could change the `info` function to return a more comprehensive dictionary with additional audio file information, such as duration, sample rate, and number of channels.
> - You could modify the `silence` function to generate silent audio files in different formats or with specific audio properties (e.g., bit depth).
> - You could modify the `cat` function to support different concatenation methods, such as fading in and out between segments or applying effects.

That's pretty good for about five seconds worth of work, and is incidentally the best use I've put ChatGPT to so far. The end result of running this script, in an admittedly convoluted way, on an earlier draft of this post is [this](https://static.inaimathi.ca/langnostic--hax.mp3). It still has some rough edges, which I can very probably deal with in the next version. I intend to release this, and possibly do something else with it too.

Ok, this is the end of the part where I'm soberly describing what I've done and going off on a wild speculative tangent. You've been warned.

## The Interface to Utopia

[voices.com](https://www.voices.com/) is a website where voice actors submit voice clips in order to audition for prospective clients to give them work, and clients can find people to voice their whatevers. Audiobook, inane advertisement, weird corporate coaching thing, podcast, whatever it is you want to be read by someone with an appropriately sexy voice. I just showed you the nascent steps of a robot that can effectively do their job. Oh, right, did I mention that `tortoise-tts` can take a ~10 minutes of voice audio in mp3 format and read in _that_ voice instead of one of the defaults like I did above? There's [already](https://www.naturalreaders.com/online/) a [few](https://speechify.com/) sites [using](https://audioread.com/) this as a business model.

If you're a good capitalist, you can use this robot to obsolete that entire industry. You set up a service where people can get their whatevers read for cheaper, and then you collect your winnings until someone else manages to undercut _you_. Eventually, the floor falls out of the "people"-with-sexy-voices-reading-things market and we all move on to the next industry.

If you're a good anti-capitalist, you lobby for tighter natural/moral/copyrights to apply to voices and snicker at the dumb tech-bros for trying to undermine existing industries without the legislative power of the state. Empirically, you also unintentionally entrench an oligopoly of existing players by doing this, but no one's going to tell _you_ that you're a fucking moron over _that_ minor detail, so whatever.

If you're a good, charismatic techno-optimist... As far as I can tell, you do this:

1. Get a bunch of voice actors together and make them an offer: they let you use their pronouncements to envoice your robot army, and in exchange they get a cut of the robot army's profits. They get to specify what kinds of jobs they want their voices used for, and you enforce their will.
2. Make an interface that accepts voice acting gigs on behalf of your voice actors. Each time a gig comes in ask the voice actor if they want to do this one themselves.
	a. If they do, take a token cut, like 5% or something, coordinate a way to get their voice to the client, collect payment and send them their lion's share.
	b. If they don't, or take longer than 24 hours to respond, have a robot do it instead. In this case, take a large but not lions' share, more like 30%-50% and send them the rest.
3. Spread the word to anyone who needs voice services, and gradually absorb more and more voice actors until you rule the world (or more realistically, until competitors spring up and you undercut each other to give your clients' cheaper voice services and/or your talent higher pay)[^or-even-more-realistically]:

[^or-even-more-realistically]: Or, even more realistically, your competitors find a way to harness voice models to create audio superstimuli. The voice-acting equivalent of high-fructose corn syrup, leather catsuits and twitter. But I'm trying to be optimistic here, and thinking about all the different ways this could blow up if it goes even a little off-center doesn't impart optimism.

I'm not sure if I'm a charismatic enough techno-optimist, but I have to say, I find this a pretty interesting possibility. Near as I can tell, there's a future coming up where machines can do equivalent or better jobs than humans in _a lot_ of ways. And an excellent mechanism to _not_ plunge anyone into desperate poverty without kneecapping our progress involves giving humans the right, but not responsibility to do any work that they're capable of. Any work they refuse to do, give it to robots trained on the basis of their talents, and pay them for the privilege of the training.

We can almost trivially run this playbook for voice actors, illustrators, transcribers and minor copywriters. We're within striking distance of doing it for writers in full generality, non-full-stack programmers, anyone involved in graphic novel or video production, and all call-centers[^here-i-mean-the-kind]. And we have straight-line, not-currently-possible-but-no-obvious-technical-obstacles approaches to take on programming in full generality, sculpting, delivery, construction, accounting, driving and cooking. If we could add food production to that list, we'd actually _be_ in the world of fully-automated luxury, gay, space, communism world, assuming nothing ate us in the meantime.

[^here-i-mean-the-kind]: Here I mean the kind that offer third party support for any sort of technical product. The kind that cold call you about duct cleaning I'm _hoping_ just die in a fire, but realistically they'll all hang on the same way.

This is purely a crazy, star-eyed-optimist musing at the moment, but if someone gave me the offer, I'd very probably take it.
