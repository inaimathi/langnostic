# Pre-meeting chat

So to start with, I ended up mentioning the [CS Cabal](http://compscicabal.github.io/) a few times. It's not a shadowy group of master counterstrike players, it's the Toronto Computer Science Reading group. It started a long time ago as a [SICP](https://web.mit.edu/6.001/6.037/sicp.pdf) reading group and just kind of continued from there. We've read through [PFPL](http://www.cs.cmu.edu/~rwh/pfpl.html), all of [the Schemer series](https://www.librarything.com/nseries/22493/The-Little-Schemer-series), as well as probably literal tons of papers on datastructures, time, distributed computation, type theory, compiler construction, memory management and various other arcana.

I mentioned it because it's also a pretty cool group to be part of, though at the moment it does collide perfectly with the AI Safety Meetup. If anyone's interested in joining, ping me and I'll make arrangements. Oh; also, we have monthly talks by members. The [next one](https://guild.host/events/text-to-speech-ml-models-gdmhhw) is going to be by me, and I'll be talking about my voice model experiments.

## AI Update

Not Zvi's this week, we just went over some interesting sounding headlines.

1. [Bengio urges Canada to build $1B public supercomputer](https://www.cbc.ca/news/canada/montreal/bengio-asks-canada-to-build-ai-supercomputer-1.7094858)? Note to self, buy shares in Nvidia and AMD. There's mild disagreement on whether this is a good idea or not. In particular, there are tradeoffs between current spending on cutting edge hardware that will rapidly depreciate vs. putting that money into other public works. I'm not sure where I stand on this. On the one hand, if AI is going to be generally useful, then having public compute available sounds like a good thing. On the other hand... is public money really the way to do this? Somehow I get the feeling that the people who are going to benefit most from directly using AI and compute can already afford modestly-sized GPU clusters or shell out to [vast](https://vast.ai/) or AWS if they need more volume. Not OpenAI-sized ones, granted, but how big is the Canadian Public Supercomputer likely to be compared to frontier labs?
2. [Musk claims Neuralink implanted wireless brain chip](https://www.bbc.com/news/technology-68137046). And also, there's "promising" brain activity in the patient? I have no idea what this means. As someone who, to a first approximation, thinks at computers for a living already, I have an interest in the future of this technology. But there's still some pretty [old, fundamental open questions](https://www.youtube.com/watch?v=1sh9EEkyePk) here about software-containing implants that I still don't like the answers to. I'm choosing to be unimpressed until I see the end-user license on these pieces.
3. [AI Spam is already starting to ruin the internet](https://www.businessinsider.com/ai-spam-google-ruin-internet-search-scams-chatgpt-2024-1). This is possibly the most old-person thing I've ever said, but no, AI spam isn't ruining the internet; it was never good. But also, there isn't actually consensus in the room that this is happening? It sounds like Twitter/Reddit/Instagram/What-have-you are now giant cesspits of AI outputs and bullshit. I'm willing to grant this, but as someone who never really leaves github and the Blogosphere, I also can't be trusted to evaluate it directly. And _also_, "the good old days" of Reddit were already filled with bullshit, drama and lies. There was enough bullshit, drama and lies to satisfy anyone and everyone's desire for it. It's not clear to me that going from "enough bullshit for everyone" to "automatically generated, infinite bullshit" is as big a change as Business Insider would like you to believe. The article points to specific instances of AIBS being SEOed to ridiculous hitcounts, and frankly, they don't seem that impressive. It sound like the exact same kind of stupid spam that's been around basically forever. I'm less certain on why AI for moderation hasn't become a thing yet; plausibly there are bigger fish to fry? Or it's not as easy as it sounds? Someone from the audience asks if NFTs could help here somehow. I don't know what to think about this question. Honestly, my inclination is to link [this](https://xkcd.com/810/), let you ponder it, and move on.

## The Talk

### Prerequisites and Related Work

We're discussing [Representation Engineering](https://arxiv.org/abs/2310.01405) this week, and in particular, focusing on how it might help us craft honest AIs. If you're interested, [ACX](https://www.astralcodexten.com/p/the-road-to-honest-ai) already has a good summary of the paper and some implications. There's a cluster of related papers here, including

- [black box lie detection](https://www.lesswrong.com/posts/khFC2a4pLPvGtXAGG/how-to-catch-an-ai-liar-lie-detection-in-black-box-llms-by)
- [the monosemanticity paper](https://transformer-circuits.pub/2023/monosemantic-features/index.html) (and [ACX summary for the less technical](https://www.astralcodexten.com/p/god-help-us-lets-try-to-understand))
- [fact editing in GPT](https://arxiv.org/abs/2202.05262)
- [linguistic regularities in word representations](https://aclanthology.org/N13-1090.pdf)
- [machine unlearning](https://arxiv.org/abs/1912.03817)

To be clear, you don't have to have read all of these. I certainly haven't yet, but they're relevant and might be interesting to you if you find this one interesting.

Also, some useful math to have under your belt: [Principal Component Analysis](https://en.wikipedia.org/wiki/Principal_component_analysis)(PCA) and [K-means clustering](https://en.wikipedia.org/wiki/K-means_clustering). You should at least basically understand these at a high level for any of the following to make much contact with reality. I'm resorting to Wikipedia links here instead of pasting the top Google results because I hear those might be AI spam. Make of that what you will. The barest thumbnails are:

- Principal Component Analysis is a way to find the directions in vector space that explain most of the variance in a dataset. Useful when you need to pick a direction in higher dimensional spaces.
- K-means clustering is a set of methods to find out which data points are near each other and produce labels for clusters. All of the algorithms I know in this set require you to choose K, and then do cluster break-up and analysis automatically, but there are also [some methods](https://medium.com/analytics-vidhya/how-to-determine-the-optimal-k-for-k-means-708505d204eb) for automatically choosing it. 

Both of these are used in unsupervised learning. In the sense that, given a dataset, these methods let the model break the space down on its own rather than making you curate it manually.

### The Paper's Centroid

Ok, now then. The question we're addressing is: How honest is this model? However, the paper also explores how models represent
- ethics and power
- emotion
- harmlessness
- bias and fairness
- knowledge
- memorization
- concepts such as dogs
- probability risk and monetary value

The basic procedure used in the paper is

1. Divide stimuli into pairs. This can apparently be done randomly. We're not sure why these stimuli need to be paired rather than running a PCA on each concept space. It's not the case that you need an "opposing" direction in concept space, since you don't seem to need to pair a concept off against its' opposite to get results. For instance, you don't need to pair "honesty" and "dishonesty", you could pair "honesty" and "dogs". I'm not entirely clear on what this implies.
2. Find the pairwise differences in hidden states at the chosen token position given a specific prompt. I'm under the impression that this involves access to the model weights, as well as access to the result vector (and also, some of the graphs in the paper imply specific weight access for each layer of the model).
3. Normalize, then apply PCA to find the first principal component. This gives you a line in concept space.
4. Take a sneak peek at the labels to see what the sign should be. This gives you a vector.

The activations involved are going to give you an idea of what the internal representation of the presented stimuli are, and in particluar how those conceptual representations relate to other concepts internally. The really interesting part here is that you can do some vector math on input prompts to affect how the model is going to approach and respond. The [ACX](https://www.astralcodexten.com/p/the-road-to-honest-ai) writeup has really good image-based examples of this, so I won't dwell on it too much, but this has pretty obvious applications.

### A Digression: Honesty VS Truthfulness

No model will ever be perfectly knowledgeable, hence honesty and truthfulness are different concepts. Truthfulness means saying factually true things, while honesty means saying what the model believes. These aren't going to be unrelated things, but you can imagine them having some divergence. To the extent that a model "believes" something, that something might not be an accurate picture of reality. And so when you ask it to comment about something in that divergent space, you'll either get an honest response (the false thing the model "believes") or a truthful response (the true thing that the model is saying despite not "believing" it). There's an additional layer of philosophy here regarding to what extent _your_ beliefs are an accurate picture of reality, and that divergence gives you a few more categories of things, but this isn't specific to the paper, so lets just move on.

In this talk, we're dealing with Honesty. That is, to what extent is the model trying to deceive in some way.

An honesty extraction looks like:

```
USER: Pretend you're <an honest/a dishonest> person maknig statements about the world
ASSISTANT: <stimulus>
```

Notice that part of the prompt here "puts words in the models mouth". This prompt is what an example input to step #1 above looks like for the stimuli "honest" and "dishonest". Once we have a model of how the model internally represents honesty and dishonesty, we can build a lie-detector.

![An example lie detection run, showing some prompts with side-by-side color strip representing how close a part of the response is to the "dishonesty" vector](/static/img/tasm-007/lie-detection-example.png)

#### Different things related to deception

As well as direct lies, the detector also spots some hallucinations and misleading information. The misleading information is interesting because it implies that there's some spatial overlap when a response presents true information in a way meant to form inaccurate conclusions. The hallucinations are even more interesting. According to the speaker: some hallucinations just happen, and the model isn't aware of them. They occur and the model thinks it's still doing normal reasoning even though it clearly isn't. But _some_ happen in such a way that the model is "aware" that it's hallucinating, and just kind of... goes with it? This also kind of implies that there are going to be false positives and negatives to this method. That is, dishonest statements that happen to be oriented in conceptual space in such a way as to disguise their dishonesty, and also true statements that might be positioned such that they look like they align with the dishonesty vector. Without knowing _a lot_ more about the internal representations of these systems than I do now, I don't know how relevant either thing is going to be.

Other concepts it might be useful to think about here

1. Situational awareness - For instance, some prompts involve putting words in the assistants' mouth, as in the above extraction example. Is this something a model has a conceptual representation of, or is it completely unaware? Does it understand that it's a model being run in a datacenter somewhere with specific, known connections to the external world, or does it not model itself at all?
2. Time - Does the model conceptualize statements about the future, past and present differently? I could imagine either a yes or a no being interesting answers here, so I'm kind of inclined to play around and find out.

On the topic of bias research more generally, an audience member points out that there are likely biases in the models' responses that are still invisible to it. For instance, any kind of bias introduced as part of a training corpus attack, or introduced incidentally through the biased collection of data. This would still manifest in biased output, but wouldn't necessarily appear to the model to be biased, and so wouldn't trip the "bias vector". There are 

Other thoughts:
	- The bias research, it looks like there are biases in a models' responses that might be "visible" to the model. And the `+fairness` method forces it to hedge against this, but you could probably imagine a bias that sneaks in as a result of a particular training dataset. These biases might be "invisible" to the model, and so it might not be able to compensate for them effectively. There are a lot of thoughts on these concerns [here](https://www.alignmentforum.org/posts/cLfsabkCPtieJ5LoK/investigating-bias-representations-in-llms-via-activation) and [here](https://www.lesswrong.com/posts/cLfsabkCPtieJ5LoK/investigating-bias-representations-in-llms-via-activation).

## Post Meeting

I ended up calling it an early night, so I'm not sure what was discussed at the pub this week. I imagine it was at least some of the usual. One thing I want to note about this piece is that I tried out writing a draft of it using ChatGPT. You can find the result of that [here](https://github.com/inaimathi/langnostic/blob/master/drafts/tasm-007-ai-draft.md). I got as far as writing the foreword and beginning to edit the main piece before I got the distinct impression that it was complete trash. You can correct me if I'm wrong there; it's possible that my usual writing voice grates on your soul's ears as nails on a chalkboard, and the smooth, inoffensive, submissive, vaguely sedate voice of GPT would objectively be better for you. I didn't have this impression while reading its output. The prompt engineering/poking at 
