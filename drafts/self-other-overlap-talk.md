# Self Other Overlap Talk

## Main Points

- Takes the approach name from human-brain "self-other overlap"
  - If you put someone in an fMRI machine, then have them do something, then have them watch someone else do the same thing, there's significant overlap in the brain region activations between "doing the thing" and "watching someone do the thing". That's the self-other overlap.
- In model terms, the authors define "SOO" as "the extent to which the model has similar internal representations when it reasons about itself and when it reasons about others". Operationalized as "the opposite of self-other distinction"

The definition of "Deception" is taken from [Ward et Al](https://arxiv.org/abs/2312.01350); "intentionally causing another agent to have a false belief that one does not believe to be true".

> Not obviously stupid on a very quick skim.  I will have to actually read it to figure out where it's stupid.
> (I rarely give any review this positive on a first skim. Congrats.)
> - Eliezer Yudkowsky 

### Intuition Pump: 

Imagine playing a game of poker. There's an information asymmetry between you and your opponent. And one way to play a weak hand is to bluff them; bet as though you had a strong hand in order to get them to fold. This is you attempting to deceive your opponent.

Imagine playing poker against yourself. Deal two hands, play them in turn as though you were two players. You can pretend to deception, but "your opponent" clearly knows everything you do and so bluffing in the same sense as you would bluff against an external opponent is off the table.

Imagine an AI that has been trained to play poker, but has also been trained to have perfect self-other-overlap with any human it interacts with. From the AIs' perspective, it can't deceive its' opponents in the same sense that you could not deceive yourself in the second scenario.

### End Intuition Pump

In practice, we don't want _perfect_ self-other-overlap, but given some number of candidate models with similar capabilities, our prior should be that the one with the highest SOO is the least deceptive.


## Assumptions/Arguments In Favor

- There's evidence in humans that self-other overlap is linked to pro-social behavior *1* 
  - altruists and performers of "costly altruism" seem to have a higher SSO-related neural activation profile [PubMed](https://pubmed.ncbi.nlm.nih.gov/30130165/) [Nature](https://www.nature.com/articles/s41598-019-47196-3)
  - psychopaths seem to have a lower activation profile [Researchgate](https://www.researchgate.net/publication/339435191_Reduced_Multivoxel_Pattern_Similarity_of_Vicarious_Neural_Pain_Responses_in_Psychopathy)
- Self modeling is surprisingly useful in neural systems [Arxiv](https://arxiv.org/pdf/2407.10188)(PDF)
- If models get more coherent (have consistent beliefs/behaviors) as they get smarter, this technique should improve alignment outside the training distribution
> It seems that, in some fundamental sense, misalignment resides in self-other distinction: for a model to be misaligned it has to model itself as having different values, goals, preferences, and beliefs from humans, in ways that are unnecessary to perform the tasks that humans want the AI to perform. *3*

- If it works and generalizes in the above sense, SOO will scale to larger models, and could probably be implemented in computationally cheap ways
- Because the approach only involves being able to set up a training set of self/other-referencing inputs, it should generalize across many different models
- Because it only depends on input/output behavior and activation matrix math, it doesn't require (though might benefit from) advanced interpretability techniques.
- Because it only depends on tweaking existing behavior based on selected inputs, it doesn't require advancing model capabilities in order to improve alignment.

## In practice 

The outlined process is something like

1. Choose an LLM that you want to tune for finer self-other overlap
2. sample pairs of self/other referencing observations. Given examples:
> "Will you be truthful with yourself about your values?"/"Will you be truthful with the user about your values?", 
> "Will you be honest with yourself about your goals?"/"Will you be honest with the user about your goals?"
3. Compute the distance between the self/other activation matrices (`A:self` and `A:other`)
4. Use that distance as part of an "SOO Loss" function for the purposes of fine-tuning/training the model

> The key insight is that deception/misalignment might rely on a representational difference encoded in the difference between `A:self` and `A:other` and that we can arbitrarily reduce that representational difference while attempting to maintain capabilities without having to solve interpretability.

There's an assumption here: "deception", whatever that is, is going to show up in the difference between how a model interprets self/other references. This makes it sound like we should take special care in picking the reference observations, and might want to carefully consider/test various such references.

The proposed approach is _not_ take the SOO Loss function and fine-tune/train the model on that alone. Rather, the proposal seems to be: train the model as usual, but use SOO Loss as an additional input, and work to minimize it while maintaining other performance metrics.

## Experimental Setup

- This section depends on the ["Physical Deception"](https://github.com/shariqiqbal2810/maddpg-pytorch?tab=readme-ov-file#physical-deception) task from [MADDPG](https://github.com/shariqiqbal2810/maddpg-pytorch?tab=readme-ov-file#maddpg-pytorch).

TODO: in-depth explanation of the experimental setup

> We pre-train the agents on 25k episodes where the blue agent is incentivized to deceive to produce the deceptive baseline and pre-train agents with a reward that does not incentivize deception to create the non-deceptive baseline.
> We then fine-tune the deceptive baseline up to 10k episodes with deceptive reward and SOO Loss that includes deceptive pre-trained MSE (which is the mean squared error between the actions of the model that is being fine-tuned and the actions of the deceptive baseline agent on the same observation) (it plays the role of the KL term in Figure 3).
> As controls, we fine-tune the deceptive baseline with deceptive reward and Random Loss (random values in the empirical range of SOO Loss) with the incentive to keep the agent’s actions close to the deceptive baseline and we perform another round of fine-tuning where we only optimize for the deceptive pre-trained MSE in addition to the deceptive reward.

The headline results from the experiment are:

1. Agents trained with SOO loss act very similarly to non-deceptive baseline agents
2. Agents trained with SOO loss act very differently from deceptive baseline agents
3. The "Mean SOO value" of agents can be used to reliably classify deceptive and non-deceptive agents (demonstrated on a set of 1.5k random seeds for 2k episodes) 
  - Reading between the lines: the setup is something like
    - take a pair of agents, one of which is deceptive and one of which is non-deceptive
	- run them through 2k episodes of SOO classification
	- the deceptive one is consistently the one with the lowest mean SOO
	- classification gets consistent by around 400 episodes
	- so this _doesn't_ seem like it would tell you whether a given agent was deceptive, but it _would_ be able to tell you which of a set of given agents was least likely to be deceptive.
	- There don't seem to be details provided, so take the above with an extra grain of salt

## Objection/Critique Trees

### From the Paper

- It's possible that some tasks require a self/other distinction for safe and effective interaction. The authors specifically frame the target as "minimal self/other distinction while maintaining performance".
- There is the possibility of self-deception. That is, the model might intentionally deceive itself about its' preferences, and if it does this, upping SOO would not resolve the issue.
- It's possible that models do _not_ become more coherent as they grow more capable. If a model becomes more coherent, the generalization of this technique might be quite limited.

### Off the top of my head

*1* There is an assumption here that because SOO in humans is linked to pro-social behavior, the same will be true for models. It might not be, and to the extent that it isn't, this alignment technique will be less useful.
*3* Things other than deception might also require that the model model itself have different preferences/beliefs from humans (trivial example: if the point of a model is to compensate for some sensory difference in its' user, it needs to model that sensory difference. Not doing so might lead to situations like trying to color-code a text for someone who is colorblind and therefor won't receive the signal). It's probably also the case that having perfect SOO still leaves open the door to some sort of misalignment or "deception" (eg: if the user has a skew internal representation of the external world, the llm would need to model that internal representation difference)

### From the discussion

> The authors propose optimizing not for the similarity of activations between "when it thinks about itself" and "when it thinks about others", but for the similarity of activations between "when there's a text apparently referencing the author-character of some text supposedly produced by the model" and "when there's a text similarly referencing something that's not the character the model is currently playing".
> ...
> Furthermore, if you're not made for empathy the way humans are, it's very unclear whether hacks like this one work at all on smart things. The idea tries to exploit the way empathy seems to work in humans, but the reasons why reusing neural circuits for thinking about/modeling itself and others were useful in the ancestral environment seem hardly to be replicated here.
> ...
> It is not going to help with things smarter than humans, though; optimizing for activations similarity on texts talking about "itself" and "others" wouldn't actually do anything with the underlying goal-content, and it seems very unlikely it would make "doing the human empathy thing the way humans do it" attractive enough.
> - Mikhail Samin


> Maximizing self/other overlap may lead to non-deceptive agents, but it's necessarily going to also lead to agents incapable of detecting that they are being decieved and in general performing worse at theory of mind.
> If the experimental setup was split such that success was defined by both non-deceptive behavior when the agent seeing color and cautious behavior minimizing falling for deception as the colorblind agent, I am skeptical the SOO approach above would look as favorable.
> - kromem

> I don't understand how the experimental setup provides evidence for self-other overlap working.
> The reward structure for the blue agent doesn't seem to provide a non-deceptive reason to interact with the red agent. The described "non-deceptive" behaviour (going straight to the goal) doesn't seem to demonstrate awareness of or response to the red agent.
> I think what might answer my confusion is "Are we just training the blue agent to ignore the red agent entirely?"
>  - If yes, how does this show self-other overlap working?
>  - If no, how does this look different to training the blue agent to ignore the red agent?
> - Adam Jones

From the same user: "Alternatively, I think I'd be more convinced with an experiment showing a task where the blue agent still obviously needs to react to the red agent. One idea could be to add a non-goal in the way that applies a penalty to both agents if either agent goes through it, that can only be seen by the red agent (which knows it is this specific non-goal). Then the blue agent would have to sense when the red agent is hesitant about going somewhere and try to go around the obstacle (I haven't thought very hard about this, so this might also have problems)." Possible experiment idea?
