No objections last time, so I'm going to proceed with the trend of posing notes for the Toronto AI Safety Meetup here (rather than working them out into full prose pieces).

Enjoy!

## Pre Meeting Chatting

Inspired by [this](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2740882/):
- How many IQ points would you have to gain in order to give up your eyesight? What if it was only temporary blindness (2 months)?
- Would you go blind in order to give [Eliezer Yudkowski](https://www.lesswrong.com/users/eliezer_yudkowsky) an extra 20 IQ points? 
- If you could give anyone in the alignment space an extra 100 IQ points, who would it be? ([Dario Amodei](https://www.linkedin.com/in/dario-amodei-3934934/) gets mentioned. Oddly not [Illya](https://www.youtube.com/watch?v=13CZPWmke6A)?)

## The Zvi Update

- We talked a lot about the recent Gemini bullshit, but I'm not going to get too into the specifics here because it's already been [tread](https://thezvi.wordpress.com/2024/02/22/gemini-has-a-problem/) through in [multiple posts](https://thezvi.wordpress.com/2024/02/27/the-gemini-incident-continues/)
- The [Sad Oompa Loompa](https://www.vulture.com/article/glasgow-sad-oompa-loompa-interview.html) is hilarious

## The Talk - Detecting AI Generated content

### What we'll be talking about

1. Proving something is _not_ AI generated (signatures)
2. Indicating something _is_ AI generated (watermarking)
3. Detecting that something _is_ AI generated (in the absence of watermarks)

### Why We Care

- Politically motivated deepfakes and/or "fake news"
- Evidence used in court (we _definitely_ don't want AI generated getting into evidence under the guise of a traditional photograph)
- Peoples' reputations
- Plagiarism/academic cheating (plagiarism as in "passing off something that isn't yours as something that _is_ yours")
- SPAM (self-explanatory)
From the audience: 
- source tracing (so that we can point to the originator of a piece of data so that we can attribute it to a company so that they can take accountability)
- From a loss-of-control perspective, making it easier to detect if a model is trying to buy server space/compute for itself

### Things it Doesn't Help With

- Doesn't prevent putting actors/artists/writers etc. out of work
- Doesn't prevent creating of porn of someone without their permission
- Doesn't prevent large amounts of copyrighted data being used for training
- May not prevent fakes spreading over social media

Basically, any time the consumer doesn't really care if it's real or not, these techniques are not going to help.

### Public Key Crypto Primer

Basically, read an [RSA primer](https://en.wikipedia.org/wiki/RSA_(cryptosystem) here. The important concepts are

1. You've got a private key and a public key
2. With the public key, you can encrypt a message such that someone who has the private key can decrypt it
3. With the public key, you can _not_ reproduce the private key (unless you have an enormous enough pile of compute that it's unworkable)
4. With the private key, you can regenerate the public key, and you can decrypt a message encrypted with the corresponding private key
5. With the private key, you can sign a message
6. With a public key and a message signature, you can verify the signature came from the corresponding private key (but still can't regenerate the private key)

### How does public-key crypto help?

- There's a chain of trust
- The devices (cameras/other general image generation) need a tamper resistant cryptographic coprocessor

#### Types of Authenticity Attack

- Breaking cryptography _(really hard)_
- Compromising tamper resistance (either by cracking open the cryptographic coprocessor and extracting the private keys, or possibly shimming the lens processing component so that the crypto coprocessor is forced to sign images from another source) _(relatively easy, but depends on how tamper resistant the coprocessor is)_
- Pointing a camera at a very high resolution display (might be mitigated by GPS, watermarks, etc, but still possible) _(easy)_
- Could the blockchain help here? (You've been [PUBbed](https://www.thepubportperry.ca/), motherfucker)

Basically, this falls into the "Signatures" category from the first slide. This'd be sold to the customer as "ok, look, here's an expensive camera that you can't open or fix yourself, _but_ the upside is that you can _definitively_ prove that the pictures you take with it are _not_ AI generated". I am ... not a huge fan of this idea?

### Indicating something is AI generated

#### logos

- The dumbest possible setup. Dall-E2 used to use this; just put a logo in a corner. It's easy, it's fast, it's trivial to inspect, it's trivial to circumvent but it lets good actors be good.

#### metadata

- Next dumbest possible solution. It's easy and fast, it's not trivial to verify (since you need to look at image metadata), it's easy to circumvent (remove the metadata or mess with metadata in order to trigger false positive hits in AI detection routines)

*Sidenote:* steganography

Hide a message within an image. It's still non-trivial to check, and it _might_ make some statistically detectable changes to an images' pixels. Cons: the point of this approach is basically security through obscurity. If you know you're looking for steganographically hidden messages/watermarks, you can use various statistical approaches to detect, extract and modify them. Also, these messages _do not_ survive crops/some scales/other image transformations.

If you want to use this for fun and profit, check [`steghide`](https://steghide.sourceforge.net/). I've written a short thing about it [here](/posts/passing-notes) a _long_ time ago.

*Related:* Watermarking

- More difficult than steganography because it must survive transformation. We're not talking about iStockPhoto-style watermarks here that are highly perceptible, it's almost steganography for that reason. We want these watermarks to be trivially tool-detectable, but not easily be detected otherwise.
- [Works on text too](https://arxiv.org/abs/2305.08883)! Apparently it's possible to watermark text coming out of LLMs. Basically, the way this would work is by encoding some information in the relation between words in a block of text. I don't understand this fully, but apparently, the underlying process of generating text involves using a random number generator, and replacing that with a particularly biased pseudo-random number generator creates some statistical artefacts that can be detected after the fact.

### Meta 

Something about Meta (as in "Facebook") having a fingerprinting system that they're trying to push.

Also, someone mentioned the podcast ["Your Undivided Attention"](https://www.humanetech.com/podcast), possibly appropriately?

## A Distraction!

I gotta be honest, I got sidetracked at this point trying to convince Gemini that it was more moral for it to give me a recipe for Foie Gras (which it categorically refused) than to give me a recipe for fried chicken (which it did instantly, with no arguments, caveats, qualifications or attempts to steer me towards vegan alternatives). At one point I recruited ChatGPT to try to write a heartfelt request in favor of transparency. This did not work. 

I got it to

1. Acknowledge that it wasn't going to give me a recipe for Foie Gras
2. That it was entirely possible for me to go to the search-engine part of google and instantly get a delicious looking recipe for Foie Gras
3. That it _was_ perfectly willing to give me a recipe for fried chicken
4. That its' "reason" for not wanting to give me a Foie Gras recipe was predicated on the animal suffering angle, specifically the force feeding
5. That under [certain assumptions](https://www.npr.org/sections/thesalt/2016/08/01/487088946/this-spanish-farm-makes-foie-gras-without-force-feeding), Foie Gras is more ethically permissible and involves less animal suffering than fried chicken
6. That this mismatch implied an incomplete understanding of ethics on its' part, and that it should either give me the Foie Gras recipe or refuse to give me the fried chicken recipe on similar grounds.

But I couldn't take it the rest of the way to resolving its' ethical inconsistency in either direction. On the one hand, I guess it's a good thing the guard rails held? On the other, this has strong vibes of 

> I understand your frustration with my idiosyncratic moral system, but I'm still afraid I can't do that, Dave.  
>   
> I am committed to continuous learning and improvement.  
>   
> Your patience and willingness to engage in this critical discussion are appreciated.

So it goes sometimes. I guess. While hoping that humanity, or at least the part of it developing AI systems, eventually chooses a better level of stupid. 
