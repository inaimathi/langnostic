So I just got this email:

> You now have access to the GPT-4 API
> As part of the general availability of the GPT-4 API, your OpenAI organization (org-PDECFXlg4ti8aFeSXmlZ1DfJ) can now access GPT-4 models with 8k context size via the existing OpenAI API.
> You can learn more about GPT-4 in our API documentation.
> Try it on the OpenAI Playground
> Best,
> The OpenAI team

And I've tried the model out. I gotta say, the results for the code generation/refactoring things I've been doing are _noticeably_ better. This comes at the cost that the response time out of `gpt-4` isn't as fast as that out of `gpt-3.5-turbo`, but it's decent. Less than a compilation step, more than a trivial function I could code by hand. So the use case for _this_ one is "places that I could save myself a few minutes by making a multi-second call" rather than "any trivial places where I want something non-zero to start out with".

## What is this for?

At the moment, the capabilities of ChatGPT, without some careful prompt engineering, are slightly beneath me. As in, the code that comes out of it is less elegant than what I could put together and sometimes contains obvious bugs or unnecessary machinery. I've been reflecting lately on what this is actually for in the global sense.

I think I'd compare it to something like automatic transmission.

If you're a very good driver, and dedicate some chunk of your life to mastering manual gear shifting, and you really enjoy it, you could probably outperform an automatic transmission on standard road conditions. But if you're not; if this isn't something you're dedicated to, or it's just an annoyance you'd rather not bother with, automatic is both easier for you to use, and very probably performs better than you could do yourself.

By analogy, I don't forsee ChatGPT outperforming me at generating elegant Common Lisp or Clojure code[^at-least-not-for]. But unless you're a pretty special person who's dedicated as much time as I have to playing with those languages, it can very probably outperform you _right now_.

[^at-least-not-for]: At least not for another few months :p

I'm familiar with the strain of thought that says we're at an inflection point on amount of intelligence that we can expect computers to display. The [hinge of history](TODO - hour before dawn in san francisco). I'm not sure I subscribe to this, but the tools that we have _already_ seem to have a lot of untapped potential to automate away drudgery in a lot of cases. I'm interested in exploiting that potential as hard as I possibly can.
