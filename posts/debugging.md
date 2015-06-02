I don't know if anyone else does this, but I constantly catch myself doing it. Even when I should *really* know better by now.

## <a name="write-some-code"></a>1. Write Some Code

That's my usual, and it does feel pretty good. I'm producing code and getting things done. Properly. Not just the barest possible solution that could possibly work, but taking approaches that I haven't before and seeing if they yield implementation or performance improvements.

## <a name="it-doesnt-work"></a>2. It Doesn't Work

Of course it doesn't work, this is the first time I'm trying the technique, and no tech starts out perfect. If anyone tells you otherwise they're either lying to you or themselves, and either way I'm not dealing with them right now. Even after running through this loop several times, it still takes a conscious effort not to get pissed off at the new technique, or some part of the toolchain. It never turns out to be any of those things, but there's still a hard, deep-wired reflex to look for something opaque to point to as a source of my problems. That would be a fail though. So I exert the conscious effort to suppress that reflex in the interests of learning something.

## <a name="debug"></a>3. Debug

Hop into the debugger, or the REPL, and start poring over all the interaction metadata I can get my grubby little paws on. In C, it means setting breakpoints and following them. In Common Lisp it means running the code piecemeal through SLIME, optionally with logging `:before` and `:after` methods defined everywhere I could possibly define them. In Haskell it means reading the compiler output and asking someone who knows what the fuck they're doing what it means.

## <a name="it-still-doesnt-work"></a>4. It Still Doesn't Work

And of course it doesn't work again. I find a couple of superficial things to fix, each one reducing the number of warnings I get, but none ever solving the root of the problem.

## <a name="iterate-and-with-a-dwindling-faith-in-my-understanding-of-the-world"></a>5. Iterate 3 and 4 with a dwindling faith in my understanding of the world

Somewhere between the second and fifth iterations of the previous two steps, I stop believing that I understand the language I'm using. That such an understanding is even possible for a mere mortal such as myself. I start thinking that maybe I have some piece of basic understanding about the insides of computers and/or compilers fundamentally backwards, and that no amount of poking will ever save me. I should go back and take some high-school level Comp Sci courses, or maybe just drop the whole "Developer" thing and go find a job more my speed, with shiny buttons and tabs to click on all day instead.

## <a name="oh-there-it-is"></a>6. Oh, there it is...

Eventually, find the last piece of error-ridden code hiding two or three layers deep in a place that I changed incorrectly a while back. And then it works. And then I have to go back through the intervening code just to convince myself that my understanding was not, in fact, some sort of decade-long mirage which merely fooled me into thinking that I occasionally knew what I was doing.

That happens disturbingly often. Which is not to say that I have lots of errors in my code, oddly. They tend to come few and far between, but the percentage of them that trigger this crisis of confidence in *literally everything I know* is huge. Easily in the 90% range. I have no idea why this happens or how common it is for other humans, but I find it afflicting me often enough that I finally had to write about it.

I also have no idea what to do about it.

Sorry.

All the alternatives seem worse. Having iron self-assurance in these situations would end with blaming a blameless component for the error and never actually figuring out what went wrong at any level even approaching a deep understanding. That's too high a price to pay for the small comfort of considering myself to be smarter than I am. There's a Stross quote that I particularly like, and I'm going to strip it of context for you here

> To never harbor self-doubt is poison for the soul, and these aliens want to inflict their certainties upon us.  
> ---Sadeq, [Charles Stross](http://www.antipope.org/charlie/blog-static/)' [Accelerando](http://www.antipope.org/charlie/blog-static/fiction/accelerando/accelerando-intro.html)  

There's a bunch of ways you can read that, including literally in context where it refers to a specific group of actual aliens, but that's not what it means to me. My imbued meaning is: Doubt is the strongest elixir of knowledge available to me. It exacts a price on your sense of certainty, but in return lets you face the kind of complexity and understanding that would evade lesser analysis.

So I guess the answer is not to do anything about it. My doubt needs to stay where it is, for the sake of my intellectual development.
