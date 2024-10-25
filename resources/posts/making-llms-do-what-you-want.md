Sorry I've been gone a while. Startup life, yo.

### Audibly Clunky Segue

[![An XKCD comic that appropriately illustrates the approach](https://imgs.xkcd.com/comics/software_development.png)](https://xkcd.com/2021/)

So a use case I have fairly commonly is that I want a model to actually do what I want. Not like, "approximately what I want" or "close enough to what I want that it doesn't raise exceptions", or "do what I want impressionistically enough that I have to call the function `String -> String` ala [`markdown` transformation](https://stackoverflow.blog/2008/06/25/three-markdown-gotcha/)".

I mean like, I want to write a prompt, provide some data and be sure that the thing I get out is Good Enough to do Real Work with. I'll admit that this isn't always possible. There are situations where you have to do the `String -> String` thing, or that are complicated enough that you'll have to tolerate some error rate above 0%.

But there are some "special cases" where it's kinda hard to write a solution to the problem yourself, but it's either trivial or _really_ easy to check whether a solution someone hands you works to within the tolerance you care about. Hello, everyone who's been in the NP space.

### Pseudo

Suppose you had a function

```
generate :: SystemPrompt -> Prompt -> Response
```

All three of those arguments are effectively aliases for strings. 

As an aside, in case you didn't know, this is the whole story. LLMs are systems where you pour strings in, [mix them around](https://xkcd.com/1838/) with enough GPUs and get strings out. If you don't believe me, go [read the docs on how Claude tool use](https://docs.anthropic.com/en/docs/build-with-claude/tool-use#tool-use-system-prompt) actually works "under the hood" in heavy finger-quotes.

Sorry, suppose you had that `generate` function. You could trivially write

```
generateChecked :: (String -> Maybe a) -> SystemPrompt -> Prompt -> ?Int -> Maybe a
generateChecked (transformFn, system, prompt, retries=5) = 
  let res = generate system prompt
      transformed, success = transformFn res
  in if transformed:
       Just transformed
	 else
	   retry upTo retries or Nothing
```

which is going to take the output, validate it against the transformer you've provided, and retry until it either gets to a valid value or retries too much. If you read that type signature, it's pretty obvious.


A more _specific_ use case here is something I frequently want to ask an LLM: "Hey, evaluate this bunch of weirdly structured, natural language data and return a value with the following type signature so I can incorporate your output into some code I'm writing". The simplest here might be

```
generateJson :: SystemPrompt -> Prompt -> ?Int -> Maybe JSONValue
generateJson (system, prompt, retries=5) = 
  generateChecked maybeParseJson system prompt retries
```

There. Now you can call your LLM, parse the response it gave you into JSON. If it fails, that is if the response isn't properly formatted JSON, try again some specified number of times. Another use case might be

```
generatePageSelector :: Page -> Prompt -> ?Int -> Maybe String
generatePageSelector (page, prompt, retries=5) =
  generateChecked #(querySelector page %) prompt retries
```
And again. Now you can ask your LLM to look at a web page, pick out an element by CSS selector, ensure that such a selector actually exists in the given page, and return it if it does. And retry some number of times if the first try doesn't hit. The code that relies on that response can rely that it's either looking at a selector that actually exists, or that it _very clearly and explicitly_ failed on retries.

### Python

What you thought this was just woolgathering?

Because we're dealing with a weakly typed system, we can't rely on Maybe. We have to go the [Lispy-route](https://lispcookbook.github.io/cl-cookbook/functions.html) of returning multiple values in part of this.

```
def loadch(resp):
    try:
        return (
            json.loads(
                (resp.strip().removeprefix("```json").removesuffix("```").strip())
            ),
            True,
        )
    except (TypeError, json.decoder.JSONDecodeError):
        pass
    return None, False
```

We're using the standard `json.loads`, but stripping some common prefix/suffix strings that come back even when you try to force most LLMs to really _just_ return readable JSON. We have to return a secondary value. Because `None` and `False` are both valid python values _and also_ valid JSON values, and Python doesn't have a native equivalent of [`Maybe`](https://wiki.haskell.org/Maybe), we'll have to work with what we've got here. So we return a second `Boolean` value to specify whether we successfully parsed. This lets us disambiguate 

```
None, False # we failed to parse all of the retry expressions we got
```

from 

```
None, True # we successfully parsed the expression, and its value was NULL, which translates to python None
```

Ok, so we've got a way to parse generated strings into JSON. Lets get a way of getting generated text to begin with. This'll be an example using [`ollama`](https://ollama.com/), but [the repo](https://github.com/inaimathi/trivialai) will implement compatibility for all the big ones. At minimum, I'm planning to support Claude, Gemini and ChatGPT.

```
class Ollama(LLMMixin):
    def __init__(self, model, ollama_server):
        self.model = model
        self.server = ollama_server.rstrip("/")

    def generate(self, system, prompt):
        res = requests.post(
            f"{self.server}/api/generate",
            json={
                "model": self.model,
                "stream": False,
                "prompt": f"SYSTEM PROMPT: {system} PROMPT: {prompt}",
            },
        )
        if res.status_code == 200:
            return LLMResult(res, 200, res.json()["response"].strip())
        return LLMResult(res, res.status_code, None)
```

The `LLMResult` type is just a `namedtuple` I've set up so that you can do dot notation on it, but realistically, it could be a dictionary. Just ignore it. The `generate` function there is obvious. But I snuck in `LLMMixin`. Lets take a look at that:

```
class LLMMixin:
    def generate_checked(self, transformFn, system, prompt, retries=5):
        for i in range(retries):
            res = self.generate(system, prompt)
            transformed, success = transformFn(res.content)
            if success:
                return LLMResult(res.raw, res.status_code, transformed)
        return LLMResult(res.raw, 500, None)

    def generate_json(self, system, prompt, retries=5):
        return self.generate_checked(loadch, system, prompt, retries=retries)
```

That `generate_checked` is what we've been building to. Again, `python`, so we don't have the cool notional type system that I think in most of the time, and that makes us deal with the return tuple from the transform function. But you should see what the mapping between this and the earlier pseudocode is.


```
...
    def generate_checked(self, transformFn, system, prompt, retries=5):
        for i in range(retries):
            res = self.generate(system, prompt)
            transformed, success = transformFn(res.content)
            if success:
                return LLMResult(res.raw, res.status_code, transformed)
        return LLMResult(res.raw, 500, None)
...
```
So, `retries` times, call `generate`, call the given `transformFn` on the result, if the transformation succeeds short circuit and return the transformed code. If you _fail_, return the `raw` input and `None`. As an aside, you can see why I want you to ignore `LLMResult` for the moment; I don't think I'm keeping that `status_code` field long term.

### Goals

The ultimate goal here is better, more composeable scaffolding. I think I've tipped my hand a bit in terms of what I'm currently using this for by showing you the concrete JSON and CSS selector examples. Details left as an exercise for the reader, at least for now. I've got a few bigger ones in mind though. Keeping cards close to my chest for now, but you can probably guess at least one or two of them based on [my github](https://github.com/inaimathi/).

As always, I'll let you know how it goes.
