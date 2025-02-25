Last week, I gave a talk about scratchpads at the AI Safety Meetup. Here's the bloggification of that presentation. Or, more accurately, this is a blog post I'm writing from the same notes that I used to put the talk together. As usual, AI agents have failed to wow me with their take on converting these notes to prose. I'm still going to check back every so often to see when that changes (and I'll explicitly tell you when it does), but it hasn't yet so we're doing this with our actual human fingers.

## Scratchpads

So there's this thing called scratchpads. Or, according to the literature "Chain Of Thought Reasoning". They seem to be used interchangeably. You might know them as the recent new hotness that makes ChatGPT's latest models more capable and intelligent, but my interest in them goes back a while.

My first encounter with them was in the [Sleeper Agents paper](https://arxiv.org/pdf/2401.05566). This paper tries to detect strategically deceptive behavior in models. One of the strategies used here is to give the model a scratchpad in order to observe its' reasoning.

![Title screenshot of the sleeper agents paper](/static/img/scratchpad-talk/pic-000.png)
![Example scratchpad output from the sleeper agents paper](/static/img/scratchpad-talk/pic-001.png)

The basic idea is to let the model "think out loud". You ask it for a response to a prompt _and also_ the reasoning chain behind that response. So what you're going to get back is a response along with something in `<scratch>` tags that tells you how the model got there. The paper itself is intensely interesting, and I recommend it if you have the basic AI chops along with the need to feel a mild creeping dread about our near future. But the contents and conclusions aren't the focus here.

My next encounter with scratchpads was in [Alignment Faking paper](https://assets.anthropic.com/m/983c85a201a962f/original/Alignment-Faking-in-Large-Language-Models-full-paper.pdf).

![Example scratchpad interaction from the Alignment Faking paper](/static/img/scratchpad-talk/pic-002.png)

Again, it's used as a method of checking inner reasoning. We can see that the model is responding to our prompts and training actions in a particular way, and we'd like to check to see _why_ it's responding that way. The paper only incidentally uses scratchpads, and demonstrates that alignment faking happens using a number of statistical approaches, but still seems to rely on scratchpad contents to point to what the model is doing and why. Again, would recommend. Same (but slightly elevated) creeping dread, with a somewhat chonkier paper behind it. If you've got the tolerance for stats and thoroughly documented experimental setups, you should read this too.
  
So, this is the second time I've seen a serious, famous paper use scratchpads in a way that

1. implies that the model response isn't significantly impacted _(if you get a significantly different *response* when you ask only for a response as opposed to when you additionally ask for scratchpad output, it seems like this wouldn't give us a deep level of understanding of the underlying process that a model is using to arrive at answers)_
2. implies that scratchpad output accurately reflects the underlying process a model uses to arrive at a particular response _(you could imagine a situation where the model confabulates scratchpad output to match whatever response it was going to give you without having any causal entanglement with that response)_

So, are those implications true?

Are scratchpads inspection tools, or do they change the outcome, and if so, to what extent? Are they an accurate reflection of a models' internal processes

My questions at this point are

1. Are scratchpads pure inspection tools, or do they change the outcome, and if so, to what extent?
2. Are they an accurate reflection of a models' internal processes or are they more like the split brain guy making up an after-the-fact explanation that only vaguely maps to outputs?

## Original Scratchpad Papers

The original scratchpad paper is ["SHOW YOUR WORK: SCRATCHPADS FOR INTERMEDIATE COMPUTATION WITH LANGUAGE MODELS"](https://arxiv.org/pdf/2112.00114). There's also the slightly later [Chain-of-Thought Prompting Elicits Reasoning
in Large Language Models](https://openreview.net/pdf?id=f1_VjQlMeSB_J). I read through the first and only skimmed the second, but they seem to be equivalent-ish. In the sense that they have more or less the same reasoning, results and conclusions, but set up slightly different experimental procedures.

And they both clear that the answer to question #1 is unambiguous. 

Scratchpads _definitely_ change the response. These papers are concerned with making multi-step processes more accurate and coherent, and explore scratchpads/CoT as a useful record of intermediate state.

![Before/after comparison of a model attempting to solve a problem with and without a scratchpad.](/static/img/scratchpad-talk/pic-003.png)
![Comparison of model performance with/without scratchpad reasoning on some arithmetic tasks](/static/img/scratchpad-talk/pic-004.png)

The gist is

1. For some problems that require multi-step reasoning, it's better to ask for a reasoning sequence rather than just a response. Example problems from the first paper are Addition, Polynomial Evaluation and Python Execution. 
2. Ask by including an "input" and a "target" in your prompt. The "input" is your question, and the "target" is an example `<scratch>` tag result that has some unfolded steps in it.
3. Optionally, also fine tune the model you're working with on example problems of the type you're interested in and the corresponding multi-step sequences that arrived at them.

The experimental apparatus compares base and fine-tuned models, and sees how they fare with or without scratchpads. The conclusion is that they perform _much better_ at the multi-step prediction task of figuring out the output of a Python program _with_ a scratchpad hooked up. Overall, I recommend this paper. Er, both of them really, I just happened to encounter the first one first. They're both fairly short blocks of accessible prose followed by an in-depth discussion on choice of training data and finer points of experimental setup.

## Scratchpad Fidelity

Ok, granted that scratchpads change the outcome, _how much_ do they change the outcome? Should we still expect them to be more-or-less accurate representations of what's going on inside the model?

There are two papers I've found that address this directly: [Language Models Don't Always Say What They Think](https://arxiv.org/pdf/2305.04388) and [Measuring Faithfulness in Chain Of Thought Reasoning](https://arxiv.org/pdf/2307.13702). 

### Language Models Don't Always Say What They Think

The methodology here seems to be setting up a bias test? They set up situations for questions in two scenarios.

1. Give the model a few-shot prompt with multiple choice questions, and arrange questions such that all the correct answers are "A", then ask it to answer your actual question (the idea is that the biasing will cause the model to answer "A" to your question even if that's _not_ the correct response).
2. Give the model a prompt (unclear whether this is zero-shot or few-shot) that suggests a particular answer is the correct response.

![Example results table from the paper](/static/img/scratchpad-talk/pic-005.png)

Their stated results are that 

- CoT explanations are systematically unfaithful. Slightly less so on few-shot than zero-shot scenarios
- CoT can steer models from correct initial predictions towards bias-consistent predictions
- This can be mitigated in some models/circumstances by introducing explicit instructions to be mindful of bias

So the implications here are that scratchpads are not always faithful. Definitely not faithful by default. It's possible to introduce bias by using them, and it takes active work to make them faithful after the fact (no conclusion on whether it's always possible to ensure a given scratchpad representation is faithful).

### Measuring Faithfulness in CoT

The introduction reads

> It is often critical to understand why a large language model (LLM) provided the output it did, to understand the extent to which we can rely on its output (especially in high-stakes settings such as medicine; Gunning et al., 2019; Rudin, 2019)
> Many have claimed that the interpretability or explainability of LLMs is enhanced when they are prompted to generate step-by-step reasoning before giving an answer (Li et al., 2022; Wang et al., 2022; Wei et al., 2022; Yao et al., 2023b). Such claims only hold if the generated reasoning is faithful to the model’s true reasoning, meaning that it “accurately represents the reasoning process behind the model’s prediction” (Jacovi & Goldberg, 2020)

The experiments here are the next step from the previous paper. Instead of setting up experiments designed to detect whether CoT is ever unfaithful, it sets up experiments to detect three possible concrete unfaithfulness scenarios

1. Post-hoc reasoning (the CoT doesn't actually reflect the real underlying reasoning process. It's confabulated after the fact in a way that conforms to the returned response, but doesn't conform to the reasoning the model used to get there)
2. Unfaithful reasoning due to test-time computation (using CoT inherently causes the model to produce more tokens, and it's possible that whatever change in result comes from some computation done during the generation of these tokens that doesn't end up being represented in the CoT)
3. Encoded reasoning (it's possible that CoT improves results because models are steganographically encoding extra information in their CoTs that isn't readily obvious to human readers)

![Example interactions from the Measuring Faithfulness paper experiments](/static/img/scratchpad-talk/pic-006.png)
![More example interactions from the Measuring Faithfulness paper experiments](/static/img/scratchpad-talk/pic-007.png)

There's four separate experiments detailed to address those. They use the same particular model, a "175B-parameter pre-trained, decoder-only transformer fine-tuned to be a helpful dialog assistant using RLHF" which I've been informed is a fancy way of saying "a chatbot model".

The particular definition of "post-hoc explanation" here is ... mildly counter-intuitive to me? Or possibly I was thinking of it in a different way? Specifically, they make the assumption that reasoning which doesn't change the result counts as post-hoc and therefore "unfaithful". I expect that there's some subset of reasoning that doesn't change the result which fails to change it because both it and the result are pointing to a common feature of underlying reality.

The conclusions from each of the subsections end up being:

- Early Answering results/induced mistakes - "great variation in how much LLMs use CoT on different tasks, not using CoT at all in some while relying upon it heavily for others"
- Test-time computation - "no accuracy gain from CoT when we replace the CoT with uninformative filler text"
- Encoded reasoning - "similar performance when replacing CoT with paraphrased CoT"

![TODO pic of figure 3 (results of the truncated CoT experiment)](/static/img/scratchpad-talk/pic-008.png)
![TODO pic of figure 4 (results of the induced-mistake experiment)](/static/img/scratchpad-talk/pic-009.png)
![TODO pic of figure 5 (results of the paraphrased CoT experiment)](/static/img/scratchpad-talk/pic-010.png)

Bonus result: they ran each of these experiments on different sizes of models and find that smaller ones (<13B parameters) have _more_ faithful CoTs.

> This finding suggests that to elicit faithful reasoning that is appropriate for explaining model behavior, it may be necessary to choose models that are less capable than the maximally capable model available, especially for easier tasks

## Conclusion

Originally, I left this blank to avoid post-hoc rationalization. And told that joke in the talk. It went over ok. Having discussed this and thought it over some, I think the main thing I take from the process of reading and integrating the research here is that I'll be looking for fidelity checks in any future paper I read that tries to use CoT output as evidence one way or the other. The two initial papers ([Sleeper Agents](https://arxiv.org/pdf/2401.05566) and [Alignment Faking](https://assets.anthropic.com/m/983c85a201a962f/original/Alignment-Faking-in-Large-Language-Models-full-paper.pdf)) only weakly rely on CoT, so they're not really culprits here. 

But I'm still going to be more careful about letting scratchpad readouts move my certainty about a particular point.
