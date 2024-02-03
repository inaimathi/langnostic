I'm trying something different this time. As should hopefully be obvious, I didn't write the following. I collected notes during the meeting, then did some minor chain-of-thought prompt engineering to ChatGPT3.5, then served as an intermediate by poking a local instance of StableDiffusion with the generated descriptions as prompts[^i-also-had-to-add], then did minor editing, mostly trimming for flow, but it looks like I'll have to do some accuracy changes too.

[^i-also-had-to-add]: I also had to add some extra quality/style pointers and negative prompts because the output was honestly pretty bad when done naively. Possibly this is just the fact that I'm using an outdated model?

This definitely isn't becoming the norm for all of my posts. About 80% of the benefit for me is unspooling ideas I'm thinking about and crunching through them in prosaic fashion. Poking a GPT has empirically not helped me in that particular way. But depending on the reaction, I might end up semi-automating the writing of notes pieces the way you see here.

Just to be clear; give me feedback in either direction. If this is such trash that I should never even think about trying it again, fair. If this is way better than my usual writing... I mean, that'd definitely suck for my ego, but it's still a datapoint I would update on. And ditto if it's anywhere in between.

Without further ado:

* * *

# Unraveling the Web of AI: Insights from the Toronto AI Safety Group Meeting

## Pre-meeting Chat: The Toronto Computer Science Cabal

Before delving into the depths of Artificial Intelligence (AI), the Toronto AI Safety Group had a casual chat, touching on the Toronto CS Cabal, more formally known as the Toronto Computer Science reading group. This community, with a lively online presence at [http://compscicabal.github.io/](http://compscicabal.github.io/), is not a mysterious organization but rather a gathering of enthusiasts keen on exploring the intricacies of computer science. I highlighted their presence because there might be an interest in the overlap between the Toronto CS Cabal and the AI safety group. As these two groups navigate their respective domains, potential synergies and collaborative opportunities could emerge, fostering a richer understanding of AI safety within the broader computer science community. Reach out to the blog curator if you wish to join either assembly.

## AI Update: From Supercomputers to Brain Chips

In the AI Update segment, headlines sparked conversations, and intriguing possibilities emerged:

1. **Bengio's Bold Proposition:** [Yoshua Bengio urges Canada](https://www.cbc.ca/news/canada/montreal/bengio-asks-canada-to-build-ai-supercomputer-1.7094858) to invest $1 billion in a public supercomputer dedicated to AI. This proposal triggers thoughts on the tradeoffs between investing in cutting-edge hardware and its rapid depreciation. Note to self: consider buying shares in [Nvidia](https://www.google.com/finance/quote/NVDA:NASDAQ?sa=X&ved=2ahUKEwiIk6rb3o2EAxUkOTQIHYCGBSAQ3ecFegQIcBAf) and [AMD](https://www.google.com/finance/quote/AMD:NASDAQ?).

![An artistic representation of a futuristic supercomputer, symbolizing the potential impact of Yoshua Bengio's proposal on AI development.](/static/img/tasm-007/Supercomputer.png)

2. **Neuralink's Mind-Boggling Move:** Elon Musk claimed that [Neuralink implanted a wireless brain chip](https://www.bbc.com/news/technology-68137046). The implications of merging human brains with AI technology raise ethical questions and futuristic scenarios. Is this the dawn of a new era, or are we tiptoeing into a sci-fi nightmare?

![An illustration representing a wireless brain chip, depicting the futuristic nature of Neuralink's technology.](/static/img/tasm-007/BrainChip.png)

3. **AI Spam Takes the Stage:** The dark side of AI emerges as [AI-generated spam](https://www.businessinsider.com/ai-spam-google-ruin-internet-search-scams-chatgpt-2024-1) starts infiltrating the internet. However, the room debates the consensus on this issue, questioning the credibility of sources like Business Insider. Could NFTs provide a shield against AI spam? The room is divided, some echoing the sentiments captured by the [classic XKCD comic](https://xkcd.com/810/) on the matter.

![An image depicting the challenges posed by AI-generated spam, symbolizing the intrusion of spam into the digital landscape.](/static/img/tasm-007/AISpam.png)

## The Talk - Honest AI: Peeling Back the Layers of Deception

Now, onto the main event - the discussion on Honest AI. The room delves into the [arXiv paper](https://arxiv.org/abs/2310.01405) and Scott Alexander's coverage at [Astral Codex Ten](https://www.astralcodexten.com/p/the-road-to-honest-ai).

### Unveiling the Black Box

The talk revolves around catching an AI liar and understanding its honesty. The paper explores concepts such as [lie detection in Black Box LLMs](https://www.lesswrong.com/posts/khFC2a4pLPvGtXAGG/how-to-catch-an-ai-liar-lie-detection-in-black-box-llms-by), [monosemantic features](https://transformer-circuits.pub/2023/monosemantic-features/index.html), and [activation clustering](https://www.alignmentforum.org/posts/cLfsabkCPtieJ5LoK/investigating-bias-representations-in-llms-via-activation). Principal Component Analysis (PCA) and K-means clustering emerge as unsupervised learning techniques, wielding the power to unravel the secrets within the AI black box.

![An abstract representation of unraveling the AI black box, symbolizing the complexities of understanding AI internals.](/static/img/tasm-007/BlackBox.png)

### The Dance of Deception

#### Honesty vs. Truthfulness

Distinguishing between honesty and truthfulness becomes crucial. No model is omniscient, making these two concepts distinct. Truthfulness focuses on stating factually true things, while honesty reflects the model's beliefs, even if flawed. The room grapples with the intricate dance between honest statements and truthful ones, realizing the potential for statements that are honest yet not truthful, and vice versa.

![An image depicting the delicate dance between honesty and truthfulness, symbolizing the complexity of navigating these concepts.](/static/img/tasm-007/HonestyTruthfulness.png)

#### Deception Unveiled

Beyond direct lies, the detector detects hallucinations and misleading information. The discussion extends to concepts like situational awareness, temporal understanding, and biases lurking beneath the surface. The intricacies of bias, both visible and invisible to the model, pose challenges that demand further exploration.

## Illuminating Bias Representations

Post-meeting, the group engages with [insights on bias representations in LLMs](https://www.lesswrong.com/posts/cLfsabkCPtieJ5LoK/investigating-bias-representations-in-llms-via-activation). The investigation into biases, both discernible and hidden, adds a layer of complexity to the AI ethics conversation.

![An artistic representation of deceptive AI, with intricate circuitry weaving a web of lies, symbolizing the challenges in uncovering hidden truths within the AI black box.](/static/img/tasm-007/AI-Lies.png)
