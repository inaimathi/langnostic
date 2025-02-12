Pre-meeting chatting

- I guess I'm doing next weeks' presentation? I've been meaning to do a talk on scratchpads.
- It was suggested that I have a sing-along section. Claude will help.
- There's a nerd show-and-tell meetup? https://www.meetup.com/ai-innovator-circle/events/305905151/?eventOrigin=your_events Which sounds awesome, and absolutely something I'm gonna hit up

The Talk

# Securing Model weights

- Most alignment schemes don't work if the attackers have access to the weights. Mainly because safety training has been shown to be fragile to fine-tuning attacks (this is where someone gets their hands on a safety-trained model, then invests some tiny f)


- Most labs training models want to keep them private
  - Don't want to give them to the competition
  - Hard to monetize open source (but Meta released Llama and thinks this is good business sense?)
  
- Weights can be leaked in many ways
  - Spy/corporate espionage exfiltrates the weights
  - Disgruntled employee posts weights online
  - Accidental breach of weights
    - employee take shome unused 10TB SSD
	- uploading wrong model to huggingface (whoops)
  - running your model on a cluster (running your custom model on GCP means that google needs to have access to your weights, which increases attack surface)
  
- Example leaks from real life: Mistral model weights ended up getting posted on 4chan a little while ago :p

- How do we secure model weights?

- RAND Security levels
  - Security Level 1 - Amateur attempts
    - encrypt weights at rest (then stream them to your use points so that unencrypted weights are _only_ in-memory)
	- least privilege principle
	- access logging
	- MFA/SSO 
  - Security Level 2 - Professional opportunistic efforts
    - Add a secret prefix to your model interactions
	- Only store weights in data centers (not individual employee laptops)
	- External audits
	- Bug bounties
  - Security level 3 - Cybercrime syndicates and insider threats
    - Adversarial input detection
	- Limitations on the number of inferences using the same credentials
	- Ongoing penetration testing
	- Honeypots
  - Security Level 4 - Standard operations by leading cyber-capable institutions
    - adversarial output detection
	- adding noise to output logprobs
	- automated weight exfiltration attempts
  - Security level 5 - 
    - Constant inference time
	- Strong limitations on software/hardware providers
	- Equivalent to US Top Secret level

- White-box watermarking weights
  - For figuring out who leaked some given weights
  - Basically sounds like steganographically embedding a target ID into the model weights themselves
  - Mistral has said they did this for the leaked models, but didn't publish any research or anything else
