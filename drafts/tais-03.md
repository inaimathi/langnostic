- Opening by doing a bunch of talk about OpenAI developments and the manifold.markets reflection of it

## The Talk

- gradient descent primer on a three-dimensional graph (mildly misleading because as it relates to gradient hacking, models typically have billions of parameters and therefore their graphs would have billions of dimensions)
- stochastic gradient descent is a technique for getting gradient descent working on a larger data-set (instead of mapping a territory all at once, map it piece by piece such that each unit fits in memory)
- minor primer of supervised vs self-supervised learning
- minor primer of reinforcement learning (a way to train things modelled as 'agents' by keeping track of their state and giving reward for certain actions)
	- usually involves trade-offs between exploration and exploitation
	- this form of training potentially involves the agent influencing its future training data
- the multi overlap diagram of machine learning:

```
	Machine learning
	  <contains> mesa optimizers
	  	         <overlaps> situational awareness
				            <contains> Deceptive alignment (not yet observed)
							           <overlaps> Gradient Hacking (not yet observed)
							<contains> Gradient Hacking (not yet observed)
							<overlaps> "aligned" ML systems (*not yet observed)
			     <overlaps> "aligned" ML systems
      <overlaps> "aligned" ML systems
```

- I finally found the spreadsheet I was looking for: https://docs.google.com/spreadsheets/d/e/2PACX-1vRPiprOaC3HsCf5Tuum8bRfzYUiKLRqJmbOoC-32JorNdfyTiRRsR7Ea5eWtvsWzuxo8bjOxCG84dAg/pubhtml

- Example of benign gradient hacking in humans: not taking addictive drugs (I think we're conceptualizing the landscape as how much short-term pleasure you're getting out of an action here). Humans aren't pure reinforcement learners, since we don't always pursue "reward", but it's a reasonably good analogy.
- We pause to discuss mesa optimizers (link to https://www.astralcodexten.com/p/deceptively-aligned-mesa-optimizers).
	- The usual example here is evolution -> humans (where we're the mesa optimizers)
	- Thought experiment example:
		- An agent that collects coin and unicorn
		- Example map that looks like:

``` __    __      __
___|🦄|__|🦄|____|🦄|____
i   $   $  $   $     $$ x|
‾‾‾‾‾‾|🦄|‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
       ‾‾
```
	- The "i" is the agent, and the reward parameters are $=1, 🦄=100 (with a hand-waved time discounting on coin values). You might naively expect it to collect all the unicorns, but if this is the agents' first encounter with unicorns, and its been trained on collecting `$`s, it might end up doing a straight-line through the level on the basis that picking up a unicorn would make it want more unicorns (whereas it currently has a preference for `$`s that it doesn't want to give up)
	- This behavior hasn't been observed in the wild, but it _has_ been shown in experiments (see https://www.youtube.com/watch?v=zkbPdEHEyEI)
	- tangent argument: any behavior that doesn't happen in training won't be trained out
	- link to https://www.lesswrong.com/posts/gFMH3Cqw4XxwL69iy/eight-short-studies-on-excuses

- Gradient filtering is an example of training misalignment where the model pretends to be more or less competent at some desired output in order to elicit more counter-training in the direction it prefers
