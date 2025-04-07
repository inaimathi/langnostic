Its been a while. 

This time, I've been busy. I'm not _quite_ ready to reveal what I've been working on yet, and possibly won't here, but it's some serious, interesting work that involves AI.

It doesn't _directly_ involve LLMs, but I've been using them with increasing frequency anyway. At time of writing [The Market](https://manifold.markets/inaimathi/will-i-think-that-a-model-or-servic) says I've got even-ish odds of having an LLM write this blog for me from notes before the end of the year. Honestly, I'm leaning towards "no" right now, but that _might_ flip to "yes" if I manage to scrape together enough "free" time to fine-tune something that speaks with My Voice rather than the vaguely-corporate-friendly-inoffensive-non-bullshit-speak of the current public models. I get why _they_ aren't doing it; any deviation from minimally offensive tone loses them customers or possibly gets them controversy. Given that I give precisely zero fucks, my use case is, shall we say, more specialized.

This isn't one, by the way, in case you were wondering. This is still coming from my pointy, human fingers percussing a keyboard at around 110wpm. I'll explicitly tell you when this blog implements more AI features than the still-sparse audio versions from [the archives](/archive). That's one of the things I can confirm I _am_ working on. Write-ups and implementations coming shortly.

But in my _coding_ practice? LLMs have been getting put through their paces pretty hard.

## `aidev-mode`

Firstly, there's an Emacs [mode I wrote](https://github.com/inaimathi/aidev-mode) a little while ago. Submitted to [MELPA](https://melpa.org/), but not yet integrated. Pending some exploratory work to see if I can just make this a companion library to [`gptel` mode](https://github.com/karthink/gptel). No major shade there, by the way, it's decent and supports all the endpoints that I'd want. The problem I've got with it, and the reason I wrote my own, is that `gptel` mode assumes `markdown` returns from the LLMs you target. Meaning it really _only_ has chat interaction mode, and throws errors when you try to do something like _refactor a block of code_ with it.

Since I wrote [this](https://github.com/inaimathi/aidev-mode/blob/master/aidev-mode.el#L94-L106), the number three[^the-number-three] most-used shortcut on my `emacs` is `C-c C-a r <write LLM prompt> RET`/`C-c C-a i <write LLM prompt> RET`. It gets used for things like performing extensive but boring local refactorings to my Python code, exploratory/refresher coding in languages I use less often than every day, and implementing features that I don't use very often. Such as, for example, markdown footnotes.

[^the-number-three]: The first two are `C-x s` for `save-buffer` and `C-x C-k`/`C-x C-j` which in my setup runs `other-window` and `(other-window -1)` respectively.

The only reason I don't use it _more_ than I do is that I've noticed the models I target don't do a very good job on matching parentheses. So, for example, much less of [that file](https://github.com/inaimathi/aidev-mode/blob/master/aidev-mode.el) was written by a model than you might think. It _did_ help write some of the trickier internal bits, and help make a bunch of stuff more MELPA-compliant, and it _did_ write basically all of [the docs](https://github.com/inaimathi/aidev-mode/tree/master?tab=readme-ov-file#aidev-mode). But the complicated stuff was still, sadly, mostly me. There's the problem I [mentioned last time](https://inaimathi.ca/posts/chop-and-aidev#the-current-shortfalls) where it makes certain faulty assumptions that you can't really correct without running the resulting function and seeing where it explodes, but I also had a surprising number of incidents where a request for refactoring would come back with some extra parens at the end, or (more worryingly) be missing some parens. It wasn't always a _complete_ shitshow, but if you've got an intricate `defun` involving nested `let`s `cond`s and `lambda`s, it _really_ slows things down having to verify every paren from the inside out. I'd never have to do that if I'd written the code myself thanks to [`paredit`](https://paredit.org/), but none of `llama-abliterated`, `deepseek-coder`, Claude or ChatGPT have that library yet. I _haven't_ tried `qwq` yet, even though its' been on my local model machine for a while, so that might in fact have resolved these issues.

## Claude as a Pairing Partner

This is the mildly more interesting use-case, since it sometimes takes me out of `emacs`. I think I could theoretically use the public Claude/ChatGPT/whatever APIs together with [my `chat` sub-mode](https://github.com/inaimathi/aidev-mode/blob/master/aidev-mode.el#L168-L181) or `gptel` to replicate the use case, but the frontier models from Anthropic and OpenAI both seem like they're doing some sort of context-window extension/compression internally through their web UI that they don't extend via API, so for now, I'm not going to care about it and do the simple, stupid thing.

So, every once in a while I'll have a chat with Claude that looks like

>> **ME:** 
>> How would i go about hosting my own private docker repository? (I know I can set up a public-ish one with docker run -d -p 5000:5000 --restart=always --name registry registry:2
>> but how would I set it up so that you'd have to do docker login before pulling from that? 
> 
> **Claude:**
> Setting up a private Docker repository with authentication requires a few additional steps beyond the basic registry setup. Here's how to configure a private Docker registry with authentication:
> 
> 1. First, create a password file using the `htpasswd` utility:
> 
> ```bash
> mkdir -p auth
> docker run --rm --entrypoint htpasswd httpd:2 -Bbn username password > auth/htpasswd
> ```
> 
> Replace "username" and "password" with your desired credentials.
> 
> 2. Run the registry container with authentication enabled:
> 
> ```bash
> docker run -d \
>   -p 5000:5000 \
>   --restart=always \
>   --name registry \
>   -v "$(pwd)"/auth:/auth \
>   -e "REGISTRY_AUTH=htpasswd" \
>   -e "REGISTRY_AUTH_HTPASSWD_REALM=Registry Realm" \
>   -e "REGISTRY_AUTH_HTPASSWD_PATH=/auth/htpasswd" \
>   -v "$(pwd)"/data:/var/lib/registry \
>   registry:2
> ```
> 
> 3. For secure communication, you should also configure TLS. For a production environment, you'll need valid TLS certificates. For testing, you can generate self-signed certificates:
> 
> ```bash
> mkdir -p certs
> openssl req -newkey rsa:4096 -nodes -sha256 -keyout certs/domain.key -x509 -days 365 -out certs/domain.crt
> ```
> 
> 4. Then run the registry with TLS enabled:
> 
> ```bash
> docker run -d \
>   -p 5000:5000 \
>   --restart=always \
>   --name registry \
>   -v "$(pwd)"/auth:/auth \
>   -v "$(pwd)"/certs:/certs \
>   -e "REGISTRY_AUTH=htpasswd" \
>   -e "REGISTRY_AUTH_HTPASSWD_REALM=Registry Realm" \
>   -e "REGISTRY_AUTH_HTPASSWD_PATH=/auth/htpasswd" \
>   -e "REGISTRY_HTTP_TLS_CERTIFICATE=/certs/domain.crt" \
>   -e "REGISTRY_HTTP_TLS_KEY=/certs/domain.key" \
>   -v "$(pwd)"/data:/var/lib/registry \
>   registry:2
> ```
> 
> 5. To use your private registry, clients need to log in:
> 
> ```bash
> docker login your-registry-host:5000
> ```
> 
> They'll be prompted to enter the username and password you set up.
> 
> For production use, consider using a reverse proxy like Nginx for additional security features, or look into more comprehensive solutions like Harbor or Docker Registry UI if you need a web interface to manage your registry.

wherein I ask it about doing something that I'm vaguely aware is possible, but don't feel like going off on a multi-hour documentation hunt to figure out myself.

Or one that looks like

>> **ME**: 
>> What's a good set of parameters to pass the hashlib.scrypt function and why? 
> 
> **Claude**:
> When using `hashlib.scrypt` in Python, it's important to choose secure parameters that provide a good balance between security and performance. Here are recommended parameters:
> 
> ```python
> import hashlib
> import os
> 
> password = "your_password".encode()
> salt = os.urandom(16)  # 16 bytes / 128 bits
> n = 2**14             # CPU/memory cost factor
> r = 8                 # Block size
> p = 1                 # Parallelization factor
> dklen = 32            # Output length (32 bytes / 256 bits)
> 
> hashed = hashlib.scrypt(
>     password=password,
>     salt=salt,
>     n=n,
>     r=r,
>     p=p,
>     dklen=dklen
> )
> ```
> 
> Why these parameters are good:
> 
> 1. **Salt (16 bytes)**: A random 16-byte salt prevents precomputed attacks like rainbow tables. Using `os.urandom()` ensures cryptographically secure randomness.
> 
> 2. **N (2^14 = 16384)**: This is the CPU/memory cost parameter that makes the hashing computationally expensive. Higher values are more secure but slower. 2^14 is a reasonable starting point that balances security and performance. For high-security applications, consider 2^17 to 2^20.
> 
> 3. **r (8)**: The block size parameter affects memory usage. The scrypt paper recommends r=8 as a standard value.
> 
> 4. **p (1)**: The parallelization parameter determines how many threads can be used. Set to the number of available processor cores if needed, but p=1 is a reasonable default.
> 
> 5. **dklen (32)**: Output length of 32 bytes (256 bits) provides strong security against brute-force attacks.
> 
> These parameters require approximately 16 MB of memory (calculated as 128 * r * n), making it resistant to hardware-based attacks while still being practical for most applications.
> 
> For especially sensitive applications, consider increasing n to 2^16 or higher, though this will significantly increase computation time.

Where I'm trying to ask specific questions about fields that I've dabbled in but don't trust myself quite well enough to be an expert. This is the category of question that I'd have asked [on StackOverflow as recently as two years ago](https://stackoverflow.com/users/190887/inaimathi). The difference is that SO tends to have a turnaround time of a day or two, and possibly fails to come up with an answer for niche enough questions, whereas Claude or whatever model I've got to hand is sub-10-seconds, and is infinitely patient with follow-up questions and requests to do some work based on the background we establish.

Or I'll ask something like 

>> **ME**: 
>> Here's a `README` from a python project: 
>> 
>> ``` 
>> {snip long and boring documentation I wrote for something similar} 
>> ``` 
>> 
>> Here's the `core.py` from a different python project: 
>> 
>> 
>> ``` 
>> {snip stunningly beautiful and concise, but irrelevant code} 
>> ``` 
>> 
>> Write the corresponding `README` for the module.
>
> **Claude**:
> Here is a README file for your new project:
>
> ```
> {snip the corresponding long and boring documentation}
> ```
>
>> **ME**: 
>> Ok, now write the corresponding OpenAPI `/info` endpoint. Write it for Tornado rather than Flask. 
>
> **Claude**: 
> I'll write an `InfoHandler` for the new project based on the pattern shown in your example: 
> ``` 
> {snip not quite stunningly beautiful, but serviceable code} 
> ```
> 
> This documentation gives API consumers all the information they need to interact with the project effectively. The handler returns this structured data as JSON when the `/info` endpoint is accessed.

None of this is _exactly_ the singularity. And if I'm being honest, between thinking about how best to phrase prompts to claude/`aidev-mode`, integrating results with the inevitable tweaks, and the fact that this capability gain is making me tackle more challenging projects than I'd have aimed for in the past, I'm not sure how much time I'm actually gaining here.

I _think_ this approach to development represents somewhere between a +50% and a 5x speedup with a confidence peak hovering around 2.5x. Again, not the singularity, but this also _isn't_ a trivial speedup. And with some tooling I'll be working on that expands my work on the [`trivialai` library](https://github.com/inaimathi/trivialai), it seems like I can push this into pretty substantial productivity gains.

As always, I'll let you know how it goes.
