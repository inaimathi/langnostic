Bit late on this one; it's actually the meeting notes for last week.

## Pre-Meeting Chatting

- [Hilarious](https://twitter.com/cafreiman)
- Also, the latest Claude update seems good? Apparently its latest update is _less_ pretentious than ChatGPT, and severely undercuts the OpenAI price. Unfortunately, they don't allow Canadian access? Or EU. The second might be a [GDPR](https://gdpr-info.eu/) thing, or a data residence thing. You can still VPN into it, but it's vaguely unsatisfying to have to.

## Zvi's Update

- Zizek tweet (I'm gonna try to have the [robot army](https://github.com/inaimathi/catwalk) read it in his voice)
  - The University professors/students in the audience heavily agree. Homework sucks, and using AI to automate the drudgery and "free our superegos" is a worthy goal
- Elon musk sued OpenAI :|
  - Apparently he tweeted that if they change their name to "ClosedAI", he'll drop the suit. -_-
  - Probably won't do much. Manifold currently [agrees](https://manifold.markets/DanMan314/what-will-be-the-outcome-of-the-elo-e7b8c4282686) with this [assessment](https://manifold.markets/Noah1/what-will-happen-with-elon-musks-la).
  - The manifold mention gets a bunch of speculation going, including notional invasion markets on the US? This gets [PUBbed](https://www.blogto.com/toronto/the_best_pubs_in_toronto/) in a hurry.
- Apparently Claude is reasonably good at reconstructing redacted emails?
- ASCII art can now be used to hack models. Also, there was some confusion; it's pronounced "ass key".

## The Talk - Mesaopetimizers and Robustness

### Term Check

- AI "Existential" Safety field
- Outer/inner alignment
- Sharp left turn / Deceptive alignment
- Mesapotimizer (check out the [Robert Miles video](https://www.youtube.com/watch?v=bJLcIBixGj8))
- Robustness
- Specification gaming (Like "cheating" on a game. Example of this is that boat AI that ends up accelerating in circles because that maximizes its' scoring function, despite the fact that the intent was to have it run around a course rather than circles)
- Adversarial Training
- AGI, ASI (Superintelligence)
- Fast takeoff/Slow takeoff
- LLM, RL, SGD
- Benchmarks/Evaluations

### Interest: "Robustness Guarantee"

- Can we ad an "adversarial perturbation buffer" larger than any deployment perturbation?
- The illustration makes this look like we'd transform points into spaces inside of our training data? I'm not _entirely_ clear on what's up, but there's some agreement from the audience that doing this would complicate things significantly from the standard data storage implementations.

### The Paper: Risks from Learned Optimization

- [This](https://arxiv.org/abs/1906.01820) is the paper that popularized the notion of inner misalignment
- One problem with this paper is that it didn't give many concrete examples, and instead focuses on hashing out what inner misalignment/mesaoptimizers in the theoretical sense
- The human genetic example is brought up here (evolution tried tuning humans to propagate their genes. Humans don't have this same goal. An audience member points out that [PCR](https://en.wikipedia.org/wiki/Polymerase_chain_reaction) exists, which lets you make a huge amount of your DNA, but very few people pay to get jars of their own DNA)

### Goal Misgeneralization

Concrete example:

- Imagine a meeting scheduling chatbot that learns user preference for restaurants instead of Zoom
- How might it handle COVID?
- It might schedule your meeting for a restaurant regardless of the fact that there are countervailing considerationst

#### Optimizer?

- An optimizer is a system that converges to some target configuration and will do so despite perturbations to the system. This is a 
  - "optimizer" vs "optimizatoin system" can't always be clearly decomposed
  - "Search/selection": a tree search or other optimization algorithm
  - "control": a control system, for example missle targeting, an automatic thermostat, motor controller, etc.

#### What's the difference between Robustness and Alignment?

- Link to [AI Alignment post](https://www.alignmentforum.org/posts/SEmviT8tyPKYkz6mN/what-is-the-difference-between-robustness-and-inner)
- Capability / Goal directedness

#### Mesaoptimizers/inner misalignment in the wild

- [This paper](https://arxiv.org/abs/1901.03559) tried to find and measure inner misalignment
- The process to induce/measure planning in a model is:
  - Train it ona task that seems like it might require/improve with planning
  - Run it on the task, varying the amount of time you give it to perform the task
  - Observe whether its' results improve as a function of time
	
- How likely is Deceptive Alignment?
  - Path dependent vs independent training
  - under/over definition
  - It sounds like the less planning a task takes the less likely this is?
  - Also, it's entirely possible that this never happens in practice. We haven't proven it either way yet.

- Prereqs for Deceptive Alignment
  1. Goal-directed behavior
  2. Optimizing across episodes/long-term goal horizons
  3. Conceptualization of the base goal - base goal as an object in the world (metacognition?x)
  4. Situational awareness - model in training, how its action now could affect its parameters in the future

- Is it all p-hacking/idealizing?
  - In an under-defined space of many initial assumptions, one might just find the explanation which fits their intuition
  - Why aren't humans deceptively aligned? (Discussion: They kind of are. Sometimes. With respect to goal generators like organizations, bosses, etc. Also, sociopaths?)
  - Also, relevant: [Adversarial Goodhearting](https://www.lesswrong.com/posts/EbFABnst8LsidYs5Y/goodhart-taxonomy#Adversarial_Goodhart)

- Reframing goal formation: Shard Theory
  - Unified goal vs shards - "contextually activated decision influences/heuristics"
  - We should study how reward is represented/learned
  - "Reward *chisels* circuits into agents" - agents do not see the reward and won't see it as an optimization target

- Maze solvers/cheese vectors (the main empirical example)
  - There's a relatively large amount of empirical research relating to mouse/cheese agents in mazes. One example that gets used is an agent that learns "go to the top right of the maze" instead of "go to the cheese" because most of the training data had the cheese in the top right quadrant of the maze.
  - Less well known:
	- There was a sub-experiment that expands the maze after training. This causes the mouse to prefer the top right quadrant of the new, larger maze
	- There was another series of sub-experiments that extract the "cheese vector" from the agent, and cause it to not care about finding the cheese at all
- An interesting relevant piece from the author of the Shard theory paper - [Dreams of AI alignment (the danger of suggestive names)](https://www.lesswrong.com/posts/yxWbbe9XcgLFCrwiL/dreams-of-ai-alignment-the-danger-of-suggestive-names)

## Pub Time

Presumably, they all discussed the pubbed items from above, but I didn't end up joining this time.
