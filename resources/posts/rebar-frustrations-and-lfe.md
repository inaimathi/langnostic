I'm trying to get my head around [Rebar](https://github.com/basho/rebar) at the moment, and am failing pretty badly. It's honestly unclear whether I'm being an idiot here, but I've gone through all of four different [tutorials](http://www.metabrew.com/article/erlang-rebar-tutorial-generating-releases-upgrades) on the [subject](http://alancastro.org/2010/05/01/erlang-application-management-with-rebar.html), each [purports](http://carbonshaft.blogspot.ca/2011/11/tutorial-getting-started-with-erlang.html) to be a [simple step-by step](http://damntechnology.blogspot.ca/2011/08/starting-with-rebar.html) guide that'll get you up and running quickly, and each one hands you an error at around step four if you follow it as written.

The only piece of documentation I've been able to follow in its entirety without a crash dump is the [basho Getting Started page](https://github.com/basho/rebar/wiki/Getting-started). Except that it only gets me as far as starting and compiling a project, showcasing none of Rebar's dependency building, distribution creation or release handling. Which makes it marginally *less* useful than Make.

Following the [upgrade_project](https://github.com/basho/rebar/tree/master/test/upgrade_project) option works fine on the test project provided as part of the rebar repo, but that project is shaped differently, and uses different config options than the ones rebar generates for a new project. Naturally, there is no documentation on what steps I have to take to get from what's generated to what works.

Ugh. Sorry, I had to vent for a bit there. Fuck this, I'm going back to Makefiles for the time being. I'll try rebar out again when my blood pressure lowers a bit.

## Was that all?

Huh? Oh. No, I guess. I did also find [this](https://github.com/rvirding/lfe/blob/master/doc/user_guide.txt), which looks pretty badass. It's not exactly a Common Lisp or a Scheme. It's a purely functional, Lisp-2 with unhygenic macros that runs on top of the Erlang VM. It's about as officially supported as you can get, being made by [Robert Virding](http://rvirding.blogspot.ca/), and it gets rid of quite a few things I don't like about Erlang.

Now, it's not a full Common Lisp, so don't expect [quicklisp](http://www.quicklisp.org/beta/) (sigh) or [loop](http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm#loop) (though in theory, there doesn't seem to be a reason that you couldn't implement that one, if you wanted it badly enough), or mutable state, but it does hit the big ones. Namely full prefix notation, homoiconicity, and macros. And it retains the things about Erlang that I do find interesting, namely massive concurrency, the pure-functional approach, and cross-process/cross-machine distribution.