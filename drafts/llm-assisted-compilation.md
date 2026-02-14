I'm not sure what else to call this, but I wanted to call it out because its' been useful in a _lot_ of different places over the past few months.

When you've got a use case where you're going to be taking plaintext input, whether from humans, LLMs or an unknown API source, and you need to turn that input into some structured output, you have three basic options with tradeoffs that might bite harder or softer based on your use case.

## Let an LLM handle it

If you're in a situation where the transformation is pretty simple, and fits into a context window, and doesn't need to be deterministic (or only needs to be deterministic to a tolerance above "set the temperature in your LLM call to 0"), and you don't mind it occasionally failing weirdly, and you don't mind it potentially taking longer than strictly interactive, _and_ you don't mind it costing you an API call, you can just call the LLM every time.

```
input -> LLM -> output
```

Those are some hard bullets to bite, but if they miss you, or you don't mind eating them, this is fine. But again, you have to pay attention to the bullets. 

If you need your response in realtime, and not _approximately_ realtime, you have to go elsewhere.

You can also use something along the lines of [`trivialai`](https://github.com/inaimathi/trivialai?tab=readme-ov-file#trivialai) to do checks on  

## Write a parser

Use [`PEG`](https://github.com/erikrose/parsimonious) or an [unholy pile of `regex`es](https://stackoverflow.com/questions/12841970/how-can-i-recognize-an-evil-regex), or something drawing on [pre-frontier AI techniques](https://www.nltk.org/) to parse a natural language input and use that to create a formal structure that you store locally and use as input to some function that does the stuff you need.
