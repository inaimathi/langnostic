So I'm gonna level with you. I've had a bunch of extra stuff to do lately and haven't been keeping up with my blog writing. Instead of working this into a full blog post, or getting ChatGPT to try to do it for me (something I still haven't satisfactorily mentioned), I'm just going to drop mildly edited notes directly into the published blog. Sorry, and also somehow not sorry? I admit that this is _probably_ worse than taking the time to go through and write full prose, but probably _not_ worse than never publishing it. If you have strong feelings about it one way or the other, let me know. If this is good enough, I'm probably going to just keep doing this going forward.

This is the second notes piece getting this treatment.

## Pre-Talk chatting

- We might be starting a Latin dancing club? (Because several Latin dance forms are represented amongst the regular attendees, mostly at [Steps](https://www.stepsdancestudio.com/))
- AI Governance [High Energy Reading Group run by EA Canada (Tuesday evenings)](https://www.meetup.com/toronto-effective-altruism-meetup/events/298963172/)
- Coding club happens Mondays at 6pm

## The Zvi Update

- This week's is a doozy; Zvi posted four articles on the day of the meeting, all of which I ended up reading through
  - [Gemini 1.5 released](https://thezvi.wordpress.com/2024/02/22/the-one-and-a-half-gemini/)
  - [Sora gets released](https://thezvi.wordpress.com/2024/02/22/sora-what/)
  - [Gemini's race problem](https://thezvi.wordpress.com/2024/02/22/gemini-has-a-problem/)
  - [Actual weekly update (including links to the above)](https://thezvi.wordpress.com/2024/02/22/ai-52-oops/)

- Chat GPT went crazy. Apparently has something to do with the sampling kernel?
- NVIDIA made lots of money (note to self, buy more NVIDIA shares? Possibly also ASML)
  - AMD might be catching up here? Driver installation is hit-or-miss, see [here](https://www.reddit.com/r/radeon/comments/133ectw/just_bought_a_fx_speedster_merc310_radeon_rx_7900/), [here](https://www.youtube.com/watch?v=d_CgaHyA_n4) and possibly [here](https://github.com/vosen/ZLUDA) but the cards are pretty low-priced comparably. If you're on the hunt for some cheap 24G workhorses, possibly check it out? It definitely takes more work
- Google gemini gets inclusive (and provides kind of a strong argument in favor of open source/in-house models)
- Air Canada chatbot hallucinates refund policy, which is then enforced in court.
- Canada is lagging behind the US in AI adoption (some contention about whether we should be pursuing the US model or the European model)
- Kalamang was translated from one book (some of the links from the Wikipedia page lead to dead links, but [A Grammar of Kalamang](https://langsci-press.org/catalog/book/344) is an actual book, with a PDF link. I note that nowhere does it say that the machine translations are _any good_. Or, indeed, _any better than the average Kalamang-non-speaker would do after reading the same material_. But hey, zero effort not-complete-trash is sometimes good enough)

## Today's Talk - Lawsuits

### The Coffin Suit

- [Matthew Coffin Butterick](https://en.wikipedia.org/wiki/Matthew_Butterick); writer, designer, programmer, lawyer
- Involved in a lot of these class action lawsuits we'll be pointing to later in the talk
- Joseph Saveri Law Firm
- "The Lawyer leading the human resistance against AI" according to Wired

### Timeline of Generative AI Lawsuits

[Source](http://sustainabletechpartner.com/topics/ai/generative-ai-lawsuit-timeline/) _(honestly, go read that unless you like my clipped commentary for some reason)_

- **Oct 2022**: OpenAI licensed data from Shutterstock, and Shutterstock gained use of OpenAI tech. The Wall Street Journal reported. Shutterstock opened a fund to compensate the artists whose work went into training the AI, the report said.
- **Jan 2023**: A Group of visual artists sued AI companies such as Stability AI, Midjourney and DeviantArt(?? apparently, they deployed a StableDiffusion-ish model?). Also, Getty Images sues Stability AI alleging they broke a bunch of licensing/intellectual property rights (This was a UK suit)
- **Feb 2023**: Getty sues Stability AI in the US, with similar allegations
- **March 2023**: US Copyright Office launches an initiative to examine the copyright law and policy issues raised by AI
- **July 2023**: Associated Press signed OpenAI licensing
- **August 2023**: US Copyright Office issued a notice of inquiry(NOI)
- **Dec 13, 2023**: OpenAI inked licensing deal with Axel Springer
- **Dec 17, 2023**: NYT sues Microsoft and OpenAI for alleged copyright infringement, claiming that the AI tools divert internet traffic
- **Jan 4, 2024**: Matthew Butterick is leading a series of lawsuits against firms such as Microsoft, OpenAI and Meta. Butterick seeking to defend the copyrights of artists, writers and programmers.
- **Jan 4, 2024**: Arist List Leaked: List of the names of 16000 artists used to train the Midjourney generative AI program
- **Jan 4, 2024 (again)**: OpenAI content licensing offers - OpenAI has offered some media "as little as" between $1M and $5M annually to license news articles for use in training large language models
- **Jan 5, 2024**: Another Lawsuit - two nonfiction authors - Nicholas Basbanes and Nicholas Gage - file suit against OpenAI and Microsoft in manhattan  federal court alleging companies misused their work to train AI models
- **Jan 8, 2024**: OpenAI responds with blogpost saying they partner with news orgs and that the NYT suit is without merit
- **Jan 11, 2024**: OpenAI suit moves forward, judge denies motion to dismiss
- **Jan 17, 2024**: Anthropic requests Tennessee court reject infringement allegations by music publishers
- **Jan 18, 2024**: AI certification
- **Jan 25, 2024**: Dudesy (guy who put together "George Carlin: I'm Glad I'm Dead"). Video featured an approximation of late comedian's voice. My understanding is that this could potentially just be a parody? Except that he used a voice cloning model to imitate George's actual voice? How different is this from those SNL skits where someone pretends to be Sean Connery? I genuinely don't know the answer to this question.
- **Jan 25, 2024**: Google settles AI-related patent lawsuit that sought $1.67 billion
- **Jan 26, 2024**: FTC investigates Generative AI partnerships. They're trying to figure out whether there's enough competition in the industry, so this is an antitrust thing.
- **Feb 6, 2024**: Microsoft and media Alliances collaborating to help adopt generative AI
- **Feb 9, 2024**: OpenAI revenues surpassed $2 billion on an annualized basis
- **Feb 13, 2024**: Lawsuit partially dismissed brought by Sarah Silverman and Ta-Nehasi Coates (dismissed everything except direct copyright claims. Specifically, dismissed the idea that _every_ answer involving copyrighted material is automatically a violation)
- **Feb 22, 2024**: AI Licensing - social media platform Reddit struck deal with Google to make its content available for training the search engine giant's AI models

### Extra Context

- Many people's livelihoods seems to be at risk, regardless of how these cases settle. Probably expect a lot more of this going forward.
- There has been, in a lot of ways, a growing rift between tech and journalism
- The two big questions among all of these AI suites:
	1. Can you copyright something you used a generative tool to make?
		- Big disputes here. On some level, the disagreement comes down to what "art" is. The AI doing a bunch of this work doesn't put it in the same class of thing as "a painting", but there's still some creative work done. There's a comparison between painting and cameras here that's instructive. An audience member points out that prompting is still some amount of work, even though it doesn't look like traditional art. The readymade movement gets mentioned, in particular ["the fountain" by Duchamp](https://en.wikipedia.org/wiki/Fountain_(Duchamp)).
	2. Does this AI tool violate copyright laws if it uses copyrighted material as part of its' training data?
- A lot depends on the interpretation of these terms:
  - **Fair use** ("the doctrine that brief excerpts of copyright material may, under certain cercumstances, be quoted verbatim for purposes such as criticism, news reporting, teaching, and research")
  - **Natural person** (as opposed to a "legal person" which might be a private or public organization)
On the understanding of the technology:
  - Many early lawsuits ran into trouble in terms of grossly misrepresenting the internals of these models (we don't _exactly_ know how they work, but we know enough to rule out scenarios like "it just memorizes all training images and serves up collages of them")
  
Pub time. This time we talked about candied ginger, the implementation of [FALGSC](https://knowyourmeme.com/memes/cultures/fully-automated-luxury-gay-space-communism) in real life, and potential economic futures of current nation states. As usual, only the most tantalizing details, but join us next time if this interests you.
