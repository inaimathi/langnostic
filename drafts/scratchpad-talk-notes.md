Lets get the excuses out of the way; this talk was a bit last minute, so it's going to be less polished presentation, and more a meandering walk through idea space.

## Scratchpads

So there's this thing called scratchpads.

- First encountered the idea in the Sleeper Agents paper (https://arxiv.org/pdf/2401.05566)
  - Paper tries to detect strategically deceptive behavior in models. One of the strategies used here is to give the model a scratchpad in order to observe its' reasoning
  [TODO] - pic from the sleeper agents paper that shows a pair of reasoning explanations
  - Basic idea is to let the model "think out loud". You ask it for an answer to a question, _and also_ the reasoning behind the question.
  - So you get a response, and also something in `<scratch>` tags that tells you how it got to that response.
- Next encountered in the Alignment Faking paper (https://assets.anthropic.com/m/983c85a201a962f/original/Alignment-Faking-in-Large-Language-Models-full-paper.pdf)
  - Again, used as a method of checking inner reasoning. We can see that the model is responding in a particular way, and we'd like to check to see _why_ it's responding that way. The paper only incidentally uses scratchpads, and demonstrates that alignment faking happens with or without one, but still seems to rely on scratchpad contents to point to what the model is doing and why.
  
So, this is the second time I've seen a serious, famous paper use scratchpads in a way that

1. implicitly don't affect the response of the model significantly (if you get a significantly different _response_ when you ask only for a response as opposed to when you additionally ask for scratchpad output, it seems like this wouldn't give us a deep level of understanding of the underlying process that a model is using to arrive at answers)
2. accurately reflects the underlying process a model uses to arrive at a particular response

This might be anthropomorphizing models a bit too much, but humans sometimes confabulate reasons for their actions, so the fact of an explanation being offered isn't an overwhelming amount of evidence for what's really going on. At the extremes are split-brain patients https://youtu.be/lfGwsAdS9Dc?si=2sa-_95GJMSIrj6u&t=342 (split brain patient confabulates a story about why he picked a particular picture after seeing a word), humans with their brains in tact do this too.

My questions at this point are

1. Are scratchpads pure inspection tools, or do they change the outcome, and if so, to what extent?
2. Are they an accurate reflection of a models' internal processes or are they more like the split brain guy making up an after-the-fact explanation that only vaguely maps to outputs?

## Original Scratchpad Papers

The original scratchpad paper is ["SHOW YOUR WORK: SCRATCHPADS FOR INTERMEDIATE COMPUTATION WITH LANGUAGE MODELS"](https://arxiv.org/pdf/2112.00114). 

And it seems to make it pretty clear that the answer to question #1 is unambiguous. Scratchpads _definitely_ change the response. This paper is concerned with making multi-step processes more accurate and coherent, and explores scratchpads as a useful record of intermediate state.

[TODO] - shot of the before/after scratchpad interactions
[TODO] - shot of result graphs

The gist of it is

1. For some problems that require multi-step reasoning, it's better to ask for a reasoning sequence rather than just a response.
   - Example problems from the paper are Addition, Polynomial Evaluation and Python Execution 
2. Ask by including an "input" and a "target" in your prompt. The "input" is your question, and the "target" is an example `<scratch>` tag result that has some unfolded steps in it.
3. Optionally, also fine tune the model you're working with on example problems of the type you're interested in.

The experimental apparatus compares base and fine-tuned models, and sees how they fare with or without scratchpads. The conclusion is that they perform _much better_ at the multi-step prediction task of figuring out the output of a Python program _with_ a scratchpad hooked up.

Overall, recommend this paper. It's about 8 pages of accessible prose, followed by an in-depth discussion on choice of training data and finer points of experimental setup.

(Note, there's a second, slightly later paper [Chain-of-Thought Prompting Elicits Reasoning
in Large Language Models](https://openreview.net/pdf?id=_VjQlMeSB_J). Talks about the same concept, with more or less the same results and approach, with slightly different experimental apparatus. I talked about the other one first because that's the one I _read_ first, both seem pretty legit and short, so you may as well check them both out if you like).

## Scratchpad Fidelity

Ok, given that scratchpads change the outcome, _how much_ do they change the outcome? Should we still expect them to be more-or-less accurate representations of what's going on inside the model?

There are two papers I've found that address this directly: [Language Models Don't Always Say What They Think](https://arxiv.org/pdf/2305.04388) and [Measuring Faithfulness in Chain Of Thought Reasoning](https://arxiv.org/pdf/2307.13702). 

### Language Models Don't Always Say What They Think

Methodology seems to be setting up a bias test? They set up situations for questions in two scenarios.

1. Give the model a few-shot prompt with multiple choice questions, and arrange questions such that all the correct answers are "A"
2. Give the model a prompt (unclear whether this is zero-shot or few-shot) that suggests a particular answer is the correct response

[TODO] - shots of the result tables

Their stated results are that 

- CoT explanations are systematically unfaithful. Slightly less so on few-shot than zero-shot scenarios
- CoT can steer models from correct initial predictions towards bias-consistent predictions
- This can be mitigated in some models/circumstances by introducing explicit instructions to be mindful of bias
faithful
### Measuring Faithfulness in CoT

The introduction reads

> It is often critical to understand why a large language model (LLM) provided the output it did, to understand the extent to which we can rely on its output (especially in high-stakes settings such as medicine; Gunning et al., 2019; Rudin, 2019)
> Many have claimed that the interpretability or explainability of LLMs is enhanced when they are prompted to generate step-by-step reasoning before giving an answer (Li et al., 2022; Wang et al., 2022; Wei et al., 2022; Yao et al., 2023b). Such claims only hold if the generated reasoning is faithful to the model’s true reasoning, meaning that it “accurately represents the reasoning process behind the model’s prediction” (Jacovi & Goldberg, 2020)

The experiments here are the next step from the previous paper. Instead of setting up experiments designed to detect whether CoT is ever unfaithful, it sets up experiments to detect three possible concrete unfaithfulness scenarios

1. Post-hoc reasoning (the CoT doesn't actually reflect the real underlying reasoning process. It's confabulated after the fact in a way that conforms to the returned response, but doesn't conform to the reasoning the model used to get there)
2. Unfaithful reasoning due to test-time computation (using CoT inherently causes the model to produce more tokens, and it's possible that whatever change in result comes from some computation done during the generation of these tokens that doesn't end up being represented in the CoT)
3. Encoded reasoning (it's possible that CoT improves results because models are steganographically encoding extra information in their CoTs that isn't readily obvious to human readers)

[TODO] - pic of the example interactions

There's four separate experiments detailed to address those. They use the same particular model ("175B-parameter pre-trained, decoder-only transformer fine-tuned to be a helpful dialog assistant using RLHF". I know what some of those words mean, but I'm not exactly sure what this describes. In particular, why is it significant that this is a pre-trained model, and what does it mean for it to be decoder-only? In the original paper, some of those terms have citations next to them. Anyone familiar with these?)

[TODO] - pic of the quoted sentence from pg2

- The particular definition of "post-hoc explanation" here is ... mildly counter-intuitive to me? Or possibly I was thinking of it in a different way? Specifically, they make the assumption that reasoning which doesn't change the result counts as post-hoc and therefore "unfaithful". 

The conclusions from each of the subsections end up being:

- Early Answering results/induced mistakes - "great variation in how much LLMs use CoT on different tasks, not using CoT at all in some while relying upon it heavily for others"
- Test-time computation - "no accuracy gain from CoT when we replace the CoT with uninformative filler text"
- Encoded reasoning - "similar performance when replacing CoT with paraphrased CoT"

[TODO] - pic of figure 3 (results of the truncated CoT experiment)
[TODO] - pic of figure 4 (results of the induced-mistake experiment)
[TODO] - pic of figure 5 (results of the paraphrased CoT experiment)

Bonus result: they ran each of these experiments on different sizes of models and find that smaller ones (<13B parameters) have _more_ faithful CoTs.

> This finding suggests that to elicit faithful reasoning that is appropriate for explaining model behavior, it may be necessary to choose models that are less capable than the maximally capable model available, especially for easier tasks

## Conclusion

Intentionally left blank to avoid post-hoc rationalization.
