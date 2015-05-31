I mentioned earlier that I gave up on [rebar](https://github.com/basho/rebar/)<a name="note-Thu-Jul-05-231940EDT-2012"></a>[|1|](#foot-Thu-Jul-05-231940EDT-2012), but I never actually wrote up the process I do use. So, here's a quick example in the form of an OTP-compliant echo server tutorial.

The first thing you need to do is create a directory for your project, and give it a certain internal structure.

```shell
$: mkdir example
$: cd example
$: mkdir ebin src deps priv
$: cd ..
$: tree example
example
├── deps
├── ebin
├── priv
└── src
```

These all have specific purposes.


-   **src** is where you'll keep all your `.erl` files. Really, you're supposed to keep other language sources in separate `[lang]_src` folders, but I put it all in here. I don't know if that'll come back to bite me in the ass. I'll report on it either way.
-   **ebin** is where you put the results of compiling your `.erl`s. Only your `.erl`s, not your `.c`s, `.py`s, `.java`s or `.lisp`s, please. I actually follow this one since everyone else seems to and it might actually matter for the purposes of someone else making use of my libraries. So interop. Fun.
-   **priv** is where you put the results of compiling all your non-Erlang code. Note that since we'll be running the system from `exapmle/`, you should invoke any non-Erlang components with (for example) `python -u priv/foo`
-   **deps** is where you put any code not part of your project, but that you depend on. I'm actually not too clear on whether you're supposed to copy all required files into `deps` directly, or whether you're supposed to arrange a tree of `deps/(.*?)/(ebin|priv)/`. Both approaches work, and there doesn't seem to be any big technical difference between them. I use the former for preference.


That's the folder structure, now lets organize our `src` folder. To set up an OTP project, you'll need at minimum 4 files.

```shell
$: cd example/src
$: touch example.app example_app.erl example_sup.erl echo.erl
$: tree ../
../
├── deps
├── ebin
├── priv
└── src
    ├── echo.erl
    ├── example.app
    ├── example_app.erl
    └── example_sup.erl
```

**example.app** is your application definition. It gives Erlang an idea of how to deal with the rest of your files, and what kind of setup do expect. It seems that it doesn't *have* to reflect reality<a name="note-Thu-Jul-05-232400EDT-2012"></a>[|2|](#foot-Thu-Jul-05-232400EDT-2012), but it's probably a good idea to get it as close as possible.

```erlang
{application, example,
 [{description, "Something something dark side"},
  {vsn, "1.0"},
  {modules, [example_app, example_sup, echo]},
  {registered, [echo]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {example_app, []}},
  {start_phases, []}]}.
```

The description and version are entirely flexible, and entirely up to you. `modules` specifies what modules this project will load, `registered` is a list of registered OTP processes we'll be running (just our echo server in this case), `applications` is a list of other Erlang systems we'll be including (you don't really need `sasl`, but I prefer the more detailed error reporting it gives you). The other two specify advanced startup behavior that I've yet to actually mess with myself. `{mod, {Mod, Argument}}` passes `Argument` to the function `start` in the module `Mod`. I can't remember what `start_phases` does, so it's either well beyond me or not particularly important.

Near as I can tell, **example_app.erl** just provides an interface to `example.app` for Erlang's standard `application` module.

```erlang
-module(example_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) -> example_sup:start_link(StartArgs).
stop(_State) -> ok.
```

That's the function I mentioned earlier. We're passing it `[]` in this case, because it doesn't need any particular initializing information. Nothing much else to see here.

**example_sup.erl** is the supervisor process for our system. Its responsibility will be to monitor and restart the `echo` process in case of errors. OTP convention seems to be to name them with a `_sup` suffix.

```erlang
-module(example_sup).
-behavior(supervisor).

-export([start/0, start_link/1, init/1]).

start() ->
    spawn(fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{echo, {echo, start, []}, permanent, 5000, worker, [echo]}]}}.
```

`start/0` and `start_link/1` are the obvious hooks to start up the supervisor. The interesting part here is actually the contents of `init/1`. The first tuple is `{SupervisionStrategy, Restarts, Time}`. **SupervisionStrategy** tells the supervisor how to deal with an errored child; `one_for_one` means that it should merely restart the crashed process. There are a couple of other options that let you kill all other children, or just all children after the initial errorer in the starting sequence. That's... kind of creepy out of context.

If it catches more than `Restarts` errors in under `Time` seconds, it kills all, um, children. Then itself<a name="note-Thu-Jul-05-232850EDT-2012"></a>[|3|](#foot-Thu-Jul-05-232850EDT-2012). The list, of one in this case, modules after that specify various properties of the processes. The tuple specifies `{Module, StartFn, StartArgs}`, the list at the end is just the name of the module again because someone at Ericsson evidently thought that "repetition" is the same as "reliability", but the tuple at the front is actually something different. It's the module tag, which will be used to register this process. By convention, it's typically the same as the module name, but there's one situation I'll cover later where it's useful to do otherwise.

Moving right along to the actual **echo.erl**.

```erlang
-module(echo).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([echo/1]).

echo(Message) -> gen_server:call(?MODULE, Message).

handle_call(Message, _From, State) -> 
    {reply, {you_just_sent, Message}, State}.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, []}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
```

Most of that is boilerplate. The only interesting parts are the `export` directive that specifies `echo/1`, the `echo/1` function itself, and the lone `handle_call/3` clause. All of which are entirely self explanatory for a basic `echo` server.

Now, lets start this fucker up!

```shell
$: cd ..
$: make
make: *** No targets specified and no makefile found.  Stop.
```

Dammit. That's right, we've got one more stop. Here's a basic, OTP-compatible **Makefile**. Trust me, you need this. It's slightly different than the one I'm using, mostly in the interests of clarity<a name="note-Thu-Jul-05-233119EDT-2012"></a>[|4|](#foot-Thu-Jul-05-233119EDT-2012).

```shell
ERL = erl -pa ebin -pa priv

erl_start = -eval 'lists:map(fun (App) -> application:load(App), application:start(App) end, [sasl, example]).'

erl_stop = -s init stop

### Rules
all: 
        erlc -Wf -o ebin/ src/*erl
        cp src/*app ebin/

start: 
        $(ERL) -name example@127.0.1.1 $(erl_start)

clean:
        rm ebin/* deps/* priv/* 
```

This does a couple of things. First, `make` compiles all the `*erl` files in `src` into `ebin`, second it copies over the `.app` file, and finally, `make start`it gives you an easier way of starting `erl` with all the relevant includes/startups than typing it all up each time. Now then.

```shell
$: make
erlc -Wf -o ebin/ src/*erl
cp src/*app ebin/
$: tree
.
├── deps
├── ebin
│   ├── echo.beam
│   ├── example.app
│   ├── example_app.beam
│   └── example_sup.beam
├── Makefile
├── priv
└── src
    ├── echo.erl
    ├── example.app
    ├── example_app.erl
    └── example_sup.erl
$: make start
[snip a whole bunch of startup notifications thanks to sasl]
(example@127.0.1.1)1> echo:echo(hello).
{you_just_sent,hello}
(example@127.0.1.1)2>
```

Tadaaah! You've just made a full OTP application from scratch, with no automated tools of any kind. Now that you know how fuckmotheringly tedious it is, I hope you'll come to the same conclusion I arrived at and write yourself something like [this](https://github.com/Inaimathi/emacs-utils/blob/master/erl-custom.el) to automate the process<a name="note-Thu-Jul-05-233443EDT-2012"></a>[|5|](#foot-Thu-Jul-05-233443EDT-2012). Point of fact, I slowed my process waaaaay down for this piece. In reality, I got to the end in about 20 seconds with one invocation of `erl-custom-template-project`<a name="note-Thu-Jul-05-233452EDT-2012"></a>[|6|](#foot-Thu-Jul-05-233452EDT-2012).

Ok, quit out of that with a `C-c C-c`. We've got the basics down. Time for a

### Bonus Stage

We've got a single echo server running, but what if we wanted a few that all have mildly different behaviors? I've seen some beginners who think the solution is copy/pasting the existing `echo.erl` and chopping it up. In reality, Erlang is a little more object-oriented than [Joe](http://armstrongonsoftware.blogspot.ca/) would have you believe. There is a bit of chopping involved, but only because we wrote the initial `echo` module without thinking about this situation. First off, its API needs to change to accept a process, rather than assuming one named the same as the `?MODULE`.

```erlang
...

-export([echo/2]).

echo(Proc, Message) -> gen_server:call(Proc, Message).

...
```

Second, we can't hard-code components that we'll want to change across processes.

```erlang
...

handle_call(Message, _From, Reply) -> 
    {reply, {Reply, Message}, Reply}.

...
```

And we need a way of changing those components from the specification in the spawning supervisor.

```erlang
...
-export([start/1, stop/1]).
...
%%%%%%%%%%%%%%%%%%%% generic actions
start(ProcName, Response) -> gen_server:start_link({local, ProcName}, ?MODULE, Response, []).
stop(ProcName) -> gen_server:call(ProcName, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init(Response) -> {ok, Response}.
...
```

Finally, we need to modify our supervisor to take advantage of all this new modularity.

```erlang
%% example_sup.erl
...

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{nice, {echo, start, [nice, thanks_for_sending]}, permanent, 5000, worker, [echo]},
           {mean, {echo, start, [nice, keep_your_fucking]}, permanent, 5000, worker, [echo]}]}}.

...
```

And you should then be able to do

```shell
$: make
erlc -Wf -o ebin/ src/*erl
cp src/*app ebin/
$: make start
[snip a whole bunch of startup notifications thanks to sasl]
(example@127.0.1.1)1> echo:echo(nice, candy).
{thanks_for_sending,candy}
(example@127.0.1.1)2> echo:echo(mean, garbage).
{keep_your_fucking,garbage}
(example@127.0.1.1)3>
```

Hacking a target process into your APIs isn't always necessary, but doing it lets you treat your API functions as faux-methods and Erlang processes as faux-objects<a name="note-Thu-Jul-05-233832EDT-2012"></a>[|7|](#foot-Thu-Jul-05-233832EDT-2012).

Thus endeth the lesson. Next time, I'll put together some more authentication thoughts, and maybe build on this mini-tutorial to something actually useful.


* * *
##### Footnotes
1 - <a name="foot-Thu-Jul-05-231940EDT-2012"></a>[|back|](#note-Thu-Jul-05-231940EDT-2012) - Which I just linked to despite the fact, because you should definitely use it if it works for you.

2 - <a name="foot-Thu-Jul-05-232400EDT-2012"></a>[|back|](#note-Thu-Jul-05-232400EDT-2012) - If you specify processes and modules in your `.app` that don't actually get loaded by the supervisors, for example, it doesn't complain.

3 - <a name="foot-Thu-Jul-05-232850EDT-2012"></a>[|back|](#note-Thu-Jul-05-232850EDT-2012) - Typically, this murder-suicide pact only happens when there's a serious, frequently triggered error in the code. In that situation, it's pointless to try to run the full program in any case.

4 - <a name="foot-Thu-Jul-05-233119EDT-2012"></a>[|back|](#note-Thu-Jul-05-233119EDT-2012) - I do some decidedly non-standard things with dependencies and launching, which I can't honestly recommend except to the extent that they seem easiest from my perspective so far. You can take a look [here](https://github.com/Inaimathi/emacs-utils/blob/master/erl-custom.el#L61-104), if you're curious.

5 - <a name="foot-Thu-Jul-05-233443EDT-2012"></a>[|back|](#note-Thu-Jul-05-233443EDT-2012) - Don't use mine; believe it or not, it actually helps to build your own lightsaber. It may be a bit creaky, but at least you'll know how to fix it.

6 - <a name="foot-Thu-Jul-05-233452EDT-2012"></a>[|back|](#note-Thu-Jul-05-233452EDT-2012) - The script I linked to automatically does other things too, like add readme files filled with minimal skeletons, generate a `.gitignore` file and start a `git` repository. All things I always do anyway, and would really prefer the computer to handle for me.

7 - <a name="foot-Thu-Jul-05-233832EDT-2012"></a>[|back|](#note-Thu-Jul-05-233832EDT-2012) - That just happen to be backed by a truly fantastic concurrency model, and encouraged to act functionally.
