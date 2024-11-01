Once we can [make LLMs do what we want](/posts/making-llms-do-what-you-want), we might want to formalize this and scale it up. We've got 

```
generate :: SystemPrompt -> Prompt -> Response
generateChecked :: (String -> Maybe a) -> SystemPrompt -> Prompt -> ?Int -> Maybe a
```

as a baseline. And, sure we have a vague sketch of something called `generateCSSSelector`, which is interesting only in a very narrow sense. 

Ok, so what's next?

Suppose that, in addition to being able to do `String -> String` prompts the way that `generate` does and `String -> Maybe a` prompts the way that `generateChecked`, you want your model to be able to call some set of functions that you want to extend to it.

```
type Env :: Map Name Function
```

Compiler writers, are you with me here?

```
transform :: Env -> String -> Maybe (Function, Args)
transform (env, result) = 
  let parsed = maybeJsonParse result
  in case Just json => if (json.functionName and json.args 
                           and json.functionName in env 
						   and (validArgsFor env[json.functionNameame] json.args))
					   then Just (env[json.functionName], json.args)
					   else Nothing
          Nothing => Nothing
```

I'd bet Schemers, Clojurers and Common Lispers know where this is going too.

```
define :: Env -> Name -> (Args -> ResMap)
define (env, name, fn) = assoc env name (fn ,args ,@body)

generateToolCall :: Env -> Prompt -> Maybe (Function, Args)
generateToolCall env prompt = 
   let sysprompt = """
   You are a computer specialist. Your job is translating client requests into tool calls. Your client has sent a request to use a tool; return the function call corresponding to the request and no other commentary. Return a value of type `{"functionName" :: string, "args" :: {arg_name: arg value}}`. You have access to the tools: {map #(%k, typeSig %v, docstring %v) env}.
"""
   in generateChecked transform sysprompt prompt
```

Looking at it from out here, this is almost too trivial to bother writing. But in effect, what we've got is a pluggable, fully generalizable toolkit that gives any sufficiently smart model access to tool capabilities. `call` really _is_ too trivial to bother writing in the notional language we've got; if I had to I'd say something like `call = funcall`. Which tells you Everything you need to know if you've worked with enough languages, and also, exactly nothing if you didn't. The big point of flexibility that I'm insisting on here is that you can swap out different environments in order to keep your models restricted to a (hopefully, if you've done your job) known-safe set of function bindings.

### Python

So lets step down from the realm of notional pseudocode and grab the snake by the tail and head simultaneously.

```
def generateToolCall(tools, llm, prompt):
    sysprompt = f'You are a computer specialist. Your job is translating client requests into tool calls. Your client has sent a request to use a tool; return the function call corresponding to the request and no other commentary. Return a value of type `{{"functionName" :: string, "args" :: {{arg_name: arg value}} }}`. You have access to the tools: {tools.list()}.'

    return llm.generate_checked(tools.transform, sysprompt, prompt)
```

There's the head. You take a `tools` environment, and an `llm`, and a `prompt` describing something that asks for a tool call, and you return the tool call. That definition tells us that `tools` is going to need the methods `list` and `transform` at minimum.

```
class Tools:
    def __init__(self):
        self.env = {}

```

You know what's up here. An environment is a dictionary. Duh.


```
    def define(self, toolName, toolFunction):
        assert (
            toolFunction.__annotations__ is not None
        ), "Interned functions must be annotated"
        assert toolFunction.__doc__, "Interned functions must have docstrings"
        if toolName in self._env:
            return False
        self._env[toolName] = {
            "type": {
                k: v
                for k, v in toolFunction.__annotations__.items()
                if not k == "return"
            },
            "function": toolFunction,
        }
        return True
```

`define` is clunkier than I'd like, but I mean, what am I supposed to do here? We take a name and a function (and use Python internal methods to assert that it has type annotations and documentation, because those things make it easier to spit at a model). Realistically, I could give it optional `type` and `description` so that you can override the given functions' `__annotations__` and `__doc__`, and I could give `__name__` the same treatment so that you could pass in `lambda`s if you really wanted to, even though they're awful in Python. That's about it though.

Honestly, all this definition is doing is reminding me how much simpler this code would be over in Clojure-land. Where I might still put it eventually.

```
    def list(self):
        return [
            {
                "name": k,
                "type": {
                    k: v
                    for k, v in v["function"].__annotations__.items()
                    if not k == "return"
                },
                "description": v["function"].__doc__,
            }
            for k, v in self.env.items()
        ]
```

One of the implied methods from earlier. This is why we need to ensure documentation and type annotations; it gives the target model more info to work with.

```
    def validate(self, tool_call):
        if (
            "functionName" in tool_call
            and "args" in tool_call
            and tool_call["functionName"] in self._env
        ):
            f = self._env[tool_call["functionName"]]
            if not set(tool_call["args"].keys()).difference(f["type"].keys()):
                return True
        return False

    def transform(self, resp):
        parsed, success = loadch(resp)
        if not success:
            return None, False
        if self.validate(parsed):
            return parsed, True
        return None, False
```

the original pseudocode `transform` got split up here. Mostly, because I'm going to be a little paranoid and use validate again inside of `call`. Still, you can see what's up here.

Transform takes a string, tries to JSON parse it using the `loadch` function we defined last time. If it fails, we bail. Otherwise, we `validate` the result. If _that_ succeeds, then we have a valid `tool_call` that we can `call` with confidence, assuming we've safely defined the underlying function.

`validate` itself does exactly what the pseudo implied earlier; we check that it's a dict with a `functionName` and an `args`, check that the `functionName` references something in our `env`, and that the thing it references has the corresponding argument list. If any of that fails, `False`, otherwise `True`.

```
    def call(self, tool_call):
        if self.validate(tool_call):
            return self.env[tool_call["functionName"]]["function"](**tool_call["args"])
        return None
```

Bam it's a one-liner. In a lisp-like, this would just be `funcall`, or possibly not even a function at all, just a pair of parens marking it as something to evaluate. Also, technically, this is a `Maybe <whatever type your function returns>` (note that we return `None` in the case that the `validate` call fails).

Don't take the code _too_ seriously in its' current form. I don't think I'm going to keep it precisely the way it is now, but the interface is there and any changes are likely to be cosmetic or QoL-enabling. Check [the docs](https://github.com/inaimathi/trivialai?tab=readme-ov-file#trivialai) before building anything out of it.

### The Upshot

So what's the point of all this?

```
>>> from typing import Optional, List
>>> def _screenshot(url: str, selectors: Optional[List[str]] = None) -> None:
    "Takes a url and an optional list of selectors. Takes a screenshot"
    print(f"GOT {url}, {selectors}!")
... ... ... 
>>>
>>> from trivialai import tools, ollama
>>> tls = tools.Tools()
>>> tls.define("screenshot", _screenshot)
True
>>> tls.list()
[{'name': 'screenshot', 'type': {'url': <class 'str'>, 'selectors': typing.Optional[typing.List[str]]}, 'description': 'Takes a url and an optional list of selectors. Takes a screenshot'}]
>>> client = ollama.Ollama("gemma2:2b", "http://localhost:11434/")
>>> tools.generate_tool_call(tls, client, "Take a screenshot of the Google website and highlight the search box")
LLMResult(raw=<Response [200]>, content={'functionName': 'screenshot', 'args': {'url': 'https://www.google.com/', 'selectors': ['#search']}})
>>> res = _
>>> res.content
{'functionName': 'screenshot', 'args': {'url': 'https://www.google.com/', 'selectors': ['#search']}}
>>> tls.call(res.content)
GOT https://www.google.com/, ['#search']!
>>> 
```

There.

If you followed this far, I think you know exactly where I'm going.

As always, I'll let you know how it goes.
