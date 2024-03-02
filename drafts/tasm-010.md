## Pre Meeting Chatting
- How many IQ points would you have to gain in order to give up your eyesight? What if it was only temporary blindness (2 months)?
- Would you go blind in order to give Eliezer Yudkowski an extra 20 IQ points? If you could give anyone in the alignment space an extra 100 IQ points, who would it be? (Dario Amodei gets mentioned. Oddly not Illya?)

## The Zvi Update

- We talked a lot about the recent Gemini bullshit, but I'm not going to get too into the specifics here because it's already been [tread](TODO) through in [multiple posts](TODO)
- The Sad Ompa Loompa is hilarious https://www.vulture.com/article/glasgow-sad-oompa-loompa-interview.html

## Detecting AI Generated content

### What we'll be talking about
1. Proving something is _not_ AI generated (signatures)
2. Indicating something _is_ AI generated (watermarking)
3. Detecting that something _is_ AI generated (in the absence of watermarks)

### Why We Care

- Politically motivated deepfakes and/or "fake news"
- Evidence used in court (we _definitely_ don't want AI generated getting into evidence under the guise of a traditional photograph)
- Peoples' reputations
- Plagiarism/academic cheating (plagiarism as in "passing off something that isn't yours as something that _is_ yours")
- SPAM
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

- Basically, read some RSA basics here. The important concepts are

1. You've got a private key and a public key
2. With the public key, you can encrypt a message such that someone who has the private key can decrypt it
3. With the public key, you can _not_ reproduce the private key (unless you have an enormous enough pile of compute that it's unworkable)
4. With the private key, you can regenerate the public key, and you can decrypt a message encrypted with the corresponding private key
5. With the private key, you can sign a message
6. With a public key and a message signature, you can verify the signature came from the corresponding private key (but still can't regenerate the private key)

### How does public-key crypto help?

- There's a chain of trust
- The devices (cameras/other general image generation) need tamper resistant cryptographic coprocessor

#### Types of Authenticity Attack

- Breaking cryptography (really hard)
- Compromising tamper resistance (either by cracking open the cryptographic coprocessor and extracting the private keys, or possibly shimming the lens processing component so that the crypto coprocessor is forced to sign images from another source)
- Pointing a camera at a very high resolution display (might be mitigated by GPS, watermarks, etc, but still possible)
- Could the blockchain help here? (You've been PUBbed, motherfucker)


Basically, this falls into the "Signatures" category from the first slide. This'd be sold to the customer as "ok, look, here's an expensive camera that you can't open, _but_ the upside is that you can _definitively_ prove that the pictures you take with it are not AI generated"

### Indicating something is AI generated

#### Logos

- The dumbest possible setup. Dall-E2 used to use this; just put a logo in a corner. It's easy, it's fast, it's trivial to inspect, it's trivial to circumvent but it lets good actors be good.

#### metadata

- Next dumbest possible solution. It's easy and fast, it's not trivial to verify (since you need to look at image metadata), it's easy to circumvent (remove the metadata or mess with metadata in order to trigger false positive hits in AI detection routines)

Sidenote: steganography
- Hide a message within an image. It's still non-trivial to check, and it _might_ make some statistically detectable changes to an images' pixels. Cons: the point of this approach is basically security through obscurity. If you know you're looking for steganographically hidden messages/watermarks, you can use various statistical approaches to detect, extract and modify them. Also, these messages _do not_ survive crops/some scales/other image transformations.

If you want to use this for fun and profit, check [`steghide`](https://steghide.sourceforge.net/)

Related: Watermarking
- More difficult than steganography because it must survive transformation. We're not talking about iStockPhoto-style watermarks here that are highly perceptible, it's almost steganography for that reason. We want these watermarks to be trivially tool-detectable, but can't easily be detected otherwise.
- Works on text too! Apparently it's possible to watermark text coming out of LLMs. Basically, the way this would work is by encoding some information in the relation between words in a block of text. I don't understand this fully, but apparently, the underlying process of generating text involves using a random number generator, and replacing that with a particularly biased pseudo-random number generator creates some statistical artefacts that can be detected after the fact.

### Meta 

## Post-meeting chatting

- "Your Undivided Attention" podcast - https://www.humanetech.com/podcast
