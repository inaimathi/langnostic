I had a conversation earlier this week with a friend who is unconvinced that AI gives any kind of productivity multipliers at all. And he wasn't the only one who thought this in the conversation. And as of earlier this week, I've got an open source example of something I consider a ridiculous multiplier.

## [TrivialAI](https://github.com/inaimathi/trivialai)

is a project I started a little while ago with the aim of centralizing and modularizing my tool use decisions across different models. It supports the frontier offerings, as well as `ollama`, and I'm going to use my AI-based development multiplier to give it more options shortly too. It gives you a common interface to `generate :: Prompt -> Result`, `generate_checked :: Prompt -> (Result -> a) -> a`, and define access to `tool :: *args -> **kwargs -> IO Result`.

At the time, I didn't need streams. So, the entire project is built on top of a blocking, eager architecture, because that's really easy.

Imagine what it would take to go from that, to a fully stream-supporting, asynchronous framework. That is, you'd want the model interfaces to be more like `stream :: Prompt -> Streamed ResultPart`, with the corresponding `stream_checked :: Prompt -> (Result -> a) -> CheckedStream a` and `tool` (which doesn't change type). Imagine the testing, composition and abstraction you'd need to pull off here. I wouldn't be too surprised if you said that it wouldn't be worth porting, and that a full rewrite would be easier.

I know that developers are notoriously bad at estimating things, but I'd probably gut check this at on the order of a week or two of work. It involves porting away from a networking library, changing how the internals of everything works to a completely different way of working, ideally we maintain backwards compatibility for interface purposes, _but_ the backward compatible synchronous results should really call the asynchronous machinery and then force it so that we don't duplicate logic. Then there's the slight, but definite added complexity of `check`ing invariants on `stream`s. Then there's making sure that all of the above is tested. A week sounds like a moderately tight deadline, actually. 

It turns out that it took around two hours all in. From [here](https://github.com/inaimathi/trivialai/commit/d68759c58c24f769aa4fa96341c951b1680546e9) to [here](https://github.com/inaimathi/trivialai/commit/46831d4a1f92b1e66d6078dce20cd469c0eb1635). PR up over [here](https://github.com/inaimathi/trivialai/pull/2) if you want to read through the full thing easily. And also, that included getting test coverage up from around 10% to around 90%.

Now, just so we're clear, this isn't the only time this has happened. This is just a convenient, illustrative example that happens to be [Expat-licensed](https://github.com/inaimathi/trivialai/blob/master/LICENSE.txt) and so can be discussed publicly. It's possible that I was wrong in estimating how hard this task would be, but me imagining a 5-20x productivity multiplier would have to cash out as "inaimathi literally doesn't know how long anything takes", and my social circle assures me I'm pretty well calibrated there.

## Conclusion

So, on this basis, if your claim is "AI is just bullshit, it doesn't actually help", I reject it. If you'd like to argue the point, lets take apart what's happening here and why you think that. Ideally with data from your side.

## Second Conclusion

That was the conclusion to the argument. Do you remember the last time we got a power multiplier this big? I think it was called "the internet". It started off with some people making eye-watering amounts of money in weird new ways that were literally impossible before. We're there _now_. Even if superintelligence doesn't [wake up and eat us](https://www.amazon.ca/Anyone-Builds-Everyone-Dies-Superhuman/dp/0316595640), even if ChatGPT 6 is only sublinearly better than [what we](https://openai.com/index/introducing-gpt-5/) have [available today](https://www.anthropic.com/news/claude-haiku-4-5). Even if _exactly zero_ further progress is made, we are in the presence of machines that can let us do monstrously more intellectual work at lower effort than we used to. The window for "no massive societal change" is shut and bolted. Face that or don't, the outcome will be the same.
