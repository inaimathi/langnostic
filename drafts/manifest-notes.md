# Talk 1 -  Future of Prediction Markets 
_by James (Manifold co-founder)_

Some articles criticize prediction markets as "not as good as polls" or "not as good as bounties".

Original idea: the question asker would make money

- Cash prizes coming (under US sweepstakes rules). About a month from now features are launching, the goal is to be able to make an income by running markets.

### How to run a market that is net profitable?

- Thus far, manifold has basically be printing "money". Askers get bonus mana, answerers get bonus mana, everyone gets bonus mana. Moving away from a play money market, this isn't going to be a thing anymore.
- Apparently, the first "profitable" market happened earlier this week. It was regarding a SpaceX launch (there was some fine points regarding choosing when specifically to close the market in order to actually make this profitable. In particular you need to close the market before it goes too close to 0 or 100 because that drains liquidity out to traders. I've got some questions about how this works)

Basic use cases:

1. Make a profit by running a market (as in the above SpaceX launch example)
2. Subsidize a market by providing liquidity to traders (basically, you're paying for information, or "subsidizing research")
3. Economic hedging. Bet in favor of bad outcomes for you so that you can collect in the event of that happening. This approximates insurance, except it also includes automatic price discovery.
4. Matchmaking. Yeah, yeah, [manifold.love](https://manifold.love/) ([retrospective](https://news.manifold.markets/p/manifold-love)), but also, hiring, social interactions (who would be a good friend for you), task matching ("who should do x", "which of a, b or c should I do given time y")?

AI is going to supercharge all of these:

1. AIs might run markets and judge on them. A model is publicly available, and more consistent than a human (and they're cheaper than humans), and it's always available to respond to questions and clarify resolution criteria
2. AI agents might provide domain general answers on questions, but _also_: letting an AI agent subsidize a prediction market gives it a way of gathering information in a well-calibrated way.
3. ??
4. If an AI knows you, it can go out and figure out how to bet on matchmaking 

Q&A

1. (garbled mumbling), but also, how would you deal with people betting after an event?
2. Is your sweepstakes model vulnerable to money pumping?
  - So the US sweepstakes rules just require you to be able to enter a sweepstakes for free. 
3. If you're trying to profit from your market, you are basically making a bet that you won't find any new information. How do you deal with that?
  - There's some intricate unwinding routines we apply internally (and could update to later versions of [uniswap](https://docs.uniswap.org/contracts/v1/overview) to get better if we need to). Also, we don't make full liquidity available in the market immediately; we release it over time.
4. For your second use case, it might be possible to subsidize someone elses' market and look at that as producing knowledge for the world.
  - Not sure there was a question in there, but yes, you can subsidize others' markets to create publicly available information. Which is why I like to think of Manifold as "Wikipedia for the Future"
5. For the hiring question, isn't there a problem with schelling points and asker sophistication? Like hypothetically, if you put together a slate of six hires, and your optimal hire is not on there, it's still in the markets' incentive to bet on one of the six.
  - We can't be naive, because that also opens up exploits along the lines of "a candidate bets a large amount on themselves in order to get hired", or "the market recommends someone you'd believe is a good hire rather than your optimal hire". (Note: I interpret this as a moderate declaration of defeat to Goodhart's law?)
6. When you talk about using AIs to interact with these markets, are you thinking of LLMs, or some other technologies? Because hallucinations might be an issue.

# Talk 2 - What Do AI models think of News and Prediction Markets?
_Nikolai Yakovenko (@Moscow25)_ currently working with [deepnewz](https://deepnewz.com/)

We go over embeddings for the top 10% of Manafold markets, and top 50% of the Polymarket markets

- Not much different between the two platforms
- Lots of recurring/seasonal markets, lots of meme markets, lots of sports/election-related ones

DeepNewz aggregates a bunch of Twitter accounts, news sources and other stuff. This lets them figure out what the  most significant/popular stories. Also, it can create markets based on those news stories. Basically, they have their AIs ask the obvious questions, and run some basic embedding similarity search for past questions. The naive similarity isn't enough (an example he gives is some sports matchup market is good. That's not automatically a signal that all sports matches are good markets, because they might involve more or less popular sports/teams/players.) So there's an extra step where they train an [XGBoost classifier](https://xgboost.readthedocs.io/en/stable/get_started.html) to filter out bad ones.

The model semi-autogenerates these markets (there's a human approval step to filter out weird markets), but resolution _is_ automatically handled by the model.

### And now, for something comlpetely different

Can AI identify humor?

There are three ways to use GPT-style models on novel tasks

1. Ask the model (prompting)
2. Train/finetune the model to specialize
3. Fit small model on top of features or embeddings (this is what DeepNewz does with XGBoost)

(There's also another approach using "agents" that talk to each other and reach consensus)

Example no-code system: ask Claude3 to generate a prompt for a less powerful model. Pretty ok results. This is comlpetely nocode.

## How to do better

1. Wait for better model that "cares" about this use case.
  - Grok 2.0 from X.AI?
2. Train on high quality user data
  - better labelled dataset for what's funny and what isn't
3. RLHF for humor

Why can AI recognize humor but not make it?
- Detection easier than generation (less intelligence needed)
- AI might not be recognizing humor, but rather attempted humor (without knowing how it lands, you don't really know)

AI Progress in 2024: A tale of plateaus and s-curves

1. 

ivan_bezdomny

# Random Notes

- http://dmac.rutgers.edu/archive/Workshops/Markets/hanson.pdf
