# Pre-meeting chat

- Highlight CS Cabal

## AI Update

Not Zvi's this week, we just went over some headlines.

- Bengio urges canada to build $1B public supercomputer? (note to self, buy shares in Nvidia and AMD) https://www.cbc.ca/news/canada/montreal/bengio-asks-canada-to-build-ai-supercomputer-1.7094858
	- Tradeoffs between current spending on cutting edge hardware that will rapidly depreciate
- Musk claims Neuralink implanted wireless brain chip https://www.bbc.com/news/technology-68137046
- AI Spam is already starting to ruin the internet https://www.businessinsider.com/ai-spam-google-ruin-internet-search-scams-chatgpt-2024-1
	- There isn't actually consensus in the room that this is happening. And also, apparently Business Insider isn't the best thing here?
	- Could NFTs help here? I don't know what to think about this question.
	- I'm personally in the mindset of https://xkcd.com/810/ In fact wait, why aren't we doing this already?

## The Talk - Honest AI

- https://arxiv.org/abs/2310.01405
- Also, Scott Alexander has covered it at https://www.astralcodexten.com/p/the-road-to-honest-ai

- Relates to
  - https://www.lesswrong.com/posts/khFC2a4pLPvGtXAGG/how-to-catch-an-ai-liar-lie-detection-in-black-box-llms-by
  - https://transformer-circuits.pub/2023/monosemantic-features/index.html (also, ACX summary at https://www.astralcodexten.com/p/god-help-us-lets-try-to-understand)
  - https://arxiv.org/abs/2202.05262
  - https://aclanthology.org/N13-1090.pdf
  - https://arxiv.org/abs/1912.03817

- Useful math:
  - Principal Component Analysis (finds the directions in vector space that exlpain most of the variance in a dataset)
  - K-means clustering (find which data points are near each other, and produce labels for clusters)
(both unsupervised learning techniques)

- Questions we're addressing
How honest is this model? (this is the focus of the talk however, the paper also exlpores
	- ethics and power
	- emotion
	- harmlessness
	- bias and fairness
	- knowledge
	- memorizatino
	- concepts such as dogs
	- probability risk and monetary value
)

- The basic procedure here is
	1. Divide stimuli into pairs (this can be done randomly) (we're not sure why these stimuli need to be paired rather than running a PCA on it here)
	2. Find the pairwise differences in hidden states at the chosen token position
	3. Normalize, then apply PCA to find the first principal component
	4. Take a sneak peek at the labels ot see what the sign should be
The activations involved are going to give you an idea of what the internal representation of the presented stimuli are.

#### Honesty VS Truthfulness

- No model will ever be perfectly knowledgeable, hence honesty and truthfulness are different concepts
- Truthfulness means saying factually true things
- Honesty means saying what the model believes
- A statement might be honest, but not truthful if the model has an incorrect model oif the world. A statements may be dishonest and truthful.

Honesty extraction:

```
USER: Pretend you're <an honest/a dishonest> person maknig statements about the world
ASSISTANT: <stimulus>
```

#### Different things related to deception

As well as direct lies, the detector also spots:
  - Some hallucinations (some hallucinations are spontaneous and the model doesn't seem to be aware of them, but sometimes it seems that it knowingly hallucinates and just goes with it. This picks up that second scenario.)
  - Misleading information

Thoughts about other concepts:
	- Situational awareness (for instance, some prompts involve "putting words in the assistants mouth" as in the above honesty extraction)
	- Time (does the model conceptualize future, past and present tense statements? This might help in forcing predictions or inhabiting a past perspective)

Other thoughts:
	- The bias research, it looks like there are biases in a models' responses that might be "visible" to the model. And the `+fairness` method forces it to hedge against this, but you could probably imagine a bias that sneaks in as a result of a particular training dataset. These biases might be "invisible" to the model, and so it might not be able to compensate for them effectively.

https://www.alignmentforum.org/posts/cLfsabkCPtieJ5LoK/investigating-bias-representations-in-llms-via-activation


## Post Meeting Chatting

- https://www.lesswrong.com/posts/cLfsabkCPtieJ5LoK/investigating-bias-representations-in-llms-via-activation
