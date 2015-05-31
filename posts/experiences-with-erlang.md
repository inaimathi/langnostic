So I've spent the past few days of programming time as deep into Erlang programming as I can manage. There was also some light C, but since most of it was focused on interfacing with an Erlang process, and the rest of it was just Imagemagick API calls, that shouldn't count as actual programming.

Maybe it's just that I don't really know how stuff is supposed to work in this massively-concurrent world, but a lot of the tactics/systems/techniques seem inconsistent. Just as an example, the language was clearly put together by hardcore Emacs users<a name="note-Tue-May-08-175711EDT-2012"></a>[|1|](#foot-Tue-May-08-175711EDT-2012), but still expects you to put your applications together with `make`<a name="note-Tue-May-08-175721EDT-2012"></a>[|2|](#foot-Tue-May-08-175721EDT-2012). Pieces of the language are put together with maximum flexibility in mind<a name="note-Tue-May-08-175727EDT-2012"></a>[|3|](#foot-Tue-May-08-175727EDT-2012), but certain parts add what seem like needless restrictions<a name="note-Tue-May-08-175733EDT-2012"></a>[|4|](#foot-Tue-May-08-175733EDT-2012).

The process of actually putting together an `.app` file and corresponding `_app.erl`+`supervisor` seems tedious and consistent enough that I'd probably like to automate it with an Emacs script or two, and I'll definitely want to make use of the `erlang-flymake` mode rather than jumping back and forth between Emacs and the shell like I have been.

I guess this actually might be the result of being spoiled by Common Lisp's "lets handle everything from the REPL!" attitude<a name="note-Tue-May-08-175811EDT-2012"></a>[|5|](#foot-Tue-May-08-175811EDT-2012). If `true`, that's a very good reason to get out of paren-land for a little while to see what's happening with other toolchains; [overspecialization](http://tvtropes.org/pmwiki/pmwiki.php/Main/CripplingOverspecialization)<a name="note-Tue-May-08-175818EDT-2012"></a>[|6|](#foot-Tue-May-08-175818EDT-2012) is not a desirable thing.

### Demystifying "Hot Code Swapping"

This is a phrase I hear pretty often [in context with Erlang](http://www.youtube.com/watch?v=OpYPKBQhSZ4), and I want to tackle it because it's typically presented as this big mysteriously awesome language feature. I get the feeling it's supposed to impress people that have never worked with anything more dynamic than [Tomcat](http://tomcat.apache.org/), because those of us working with Lisp/Python/PHP/etc. are pretty used to the idea of pushing an update without restarting the server we're pushing it at. There are complications, of course, if you want to keep existing requests valid, for example, you need to deal with server rollover. The Erlang way solves that particular problem, so I can see why it would be put together like this, it's just really annoying seeing it presented as secret sauce when it's almost too simple to be considered a language feature.

In fact, it's just a natural consequence of having


-   a functional language
-   that structures its code in processes
-   and supports tail recursion


If your language has all of those, then it also supports "Hot Code Swapping" by default as long as you structure your processes appropriately. Here's how it works, with the magic peeled away. First, you need to understand what a process looks like. It looks something like this (Erlang pseudocode, it won't actually run):

```erlang
%% example_server.erl %%

start() -> spawn(fun loop(Module, State) end).

loop(Module, State) ->
    receive
        foo -> 
            apply(Module, handle, [foo, self(), State]),
            loop(Module, State);
        bar -> 
            apply(Module, handle, [bar, self(), State]),
            loop(Module, State);
        {switch, NewModule} -> loop(NewModule, State).
```

and the "callback" module, which contains the `handle` function looks like this

```erlang
%% example_callback.erl %%

handle(foo, From, _State) -> From ! "You sent 'foo'";
handle(bar, From, _State) -> 
    From ! "Ok, that time, you totally sent 'bar', and that is NOT COOL.",
    From ! "Fuck you, man.",
    From ! "Fuck you.",
    error(i_hate_this_prick).
```

You start that just by calling `start` and assigning its return value to a variable (`spawn` will return the process id of the process it `spawn`s, and `start` just passes that along), then use it by sending it messages.

It hardly needs explaining at this point, but I've recently been told that things I see as obvious aren't always. 

So. 

Remember the three requirements: at least pure-ish functional, a process structure and tail recursion. A process is just a thing that asynchronously loops forever<a name="note-Tue-May-08-175843EDT-2012"></a>[|7|](#foot-Tue-May-08-175843EDT-2012) by grabbing the next message out of its input queue and acting on it (hence why processes are required). Part of "acting on it" includes calling the next iteration of the `loop`<a name="note-Tue-May-08-175854EDT-2012"></a>[|8|](#foot-Tue-May-08-175854EDT-2012) (hence the tail-recursion requirement). Finally, if those both happen to be taking place in a pure functional system, you *can't* just set a global/effectively-global variable to track process state; you have to pass it to the next iteration as an *argument*. If the name of the module containing your `handle` function is one of those state variables you need to pass, then changing out code is as simple as making sure that your process can react to a `change` message by accepting a new module name to call. If you handle module naming properly, you can also naturally create the situation where existing requests are handled by the module that was in effect when they were made, while any new requests are handled by the new module.

That's actually how it works under the covers of `[gen_server](http://www.erlang.org/doc/man/gen_server.html)`; the `behaviour` just handles setting up the code-switch-related messages and handlers for you. If you want your process to suddenly have new code, you write a new callback module

```erlang
%% nice_callback.erl %% 

handle(foo, From) -> 
    From ! "Thank you for sending 'foo'.";
handle(bar, From) -> 
    From ! "Ok, I see that you're sending 'bar', we can take care of that.";
handle(what, From) -> 
    From ! "What seems to be the probem?";
handle('bar_ok?', From) -> 
    From ! "Oh, yes, we handle 'bar' properly now. We apologize for our predecessors' lack of tact.".
```

then send our process the appropriate message

```erlang
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]

Eshell V5.9.1  (abort with ^G)

1> c(example_server).
{ok, example_server}
2> {ok, Proc} = example_server:start().
{ok, &lt;0.39.0>}
3> Proc ! foo.
"You sent 'foo'"
...
37> Proc ! {switch, nice_callback}.
38> Proc ! foo.
"Thank you for sending 'foo'."
```

As you can see, on the next run through the loop, your process calls `nice_callback:handle` rather than `example_callback:handle`. No magic, no tricks, and no "hot-code-swapping" language feature. Just organize your system appropriately and this feature emerges.

### Pid ! {switch, you_are_mr_bear}

Skip the rest if you've heard [this one](http://c2.com/cgi/wiki?RubberDucking) before. Article's over early for you, you get to go home. Yay.

There's a `(TA|lab tech|professor|IT manager|guru)` who notices that `[6789]`/10 of the questions he gets are actually resolved before he even gets to say anything. Being that he's a prick, rather than explaining this to people, he sets up a `(teddy bear|dragon|rubber duckie|moose head|pony)` on the `(wall|chair|throne|tower of mordor)` next to his `(desk|office|lab|drawbridge)` and institutes a new rule: 

If you have a question, you must first explain it, out loud, to the `\2` he's helpfully provided.

The urban legend goes that what follows is a large number of one-person conversations that sound something like

Ok, I'm having an issue with this bug that turned up about three versions ago; when the user logs into a particular screen, the system asks them to log in again rather th... Oh. *That's it*. Thanks, Dr. Whooves!

Even though it's an urban legend, and sounds incredibly silly, it works *really* well. This very article actually had about twice as many complaints about Erlang in it when I wrote the initial draft, but just crunching through them enough to write them down helped me see the solution. I'm not even remotely the first person to propose this, but [blogging is a pretty good replacement](https://sites.google.com/site/steveyegge2/you-should-write-blogs) for that `\2`. It sidesteps the bad part of the Mr Bear situation<a name="note-Tue-May-08-175909EDT-2012"></a>[|9|](#foot-Tue-May-08-175909EDT-2012)), but retains the good parts<a name="note-Tue-May-08-175914EDT-2012"></a>[|10|](#foot-Tue-May-08-175914EDT-2012).

* * *
##### Footnotes
1 - <a name="foot-Tue-May-08-175711EDT-2012"></a>[|back|](#note-Tue-May-08-175711EDT-2012) - As evidenced by the fact that it comes with its own built-in, fairly extensive Emacs modes.

2 - <a name="foot-Tue-May-08-175721EDT-2012"></a>[|back|](#note-Tue-May-08-175721EDT-2012) - The Erlang equivalent of `asdf:load-system`, `applications:load`, seems to assume that you've already manually compiled your `.erl`s to `.beam`s rather than doing it automatically.

3 - <a name="foot-Tue-May-08-175727EDT-2012"></a>[|back|](#note-Tue-May-08-175727EDT-2012) - And the author has gone on record saying he'd very much like to add more.

4 - <a name="foot-Tue-May-08-175733EDT-2012"></a>[|back|](#note-Tue-May-08-175733EDT-2012) - Particularly the guard instructions, which only take a specified subset of Erlang to guarantee that only side-effect-free predicates can be used.

5 - <a name="foot-Tue-May-08-175811EDT-2012"></a>[|back|](#note-Tue-May-08-175811EDT-2012) - Not to mention [quicklisp](http://www.quicklisp.org/beta/), but it's really unfair how awesome that library is, so I won't hold any other language up to the same standard.

6 - <a name="foot-Tue-May-08-175818EDT-2012"></a>[|back|](#note-Tue-May-08-175818EDT-2012) - TV tropes warning, by the by.

7 - <a name="foot-Tue-May-08-175843EDT-2012"></a>[|back|](#note-Tue-May-08-175843EDT-2012) - Or until you tell it otherwise, or an error occurs.

8 - <a name="foot-Tue-May-08-175854EDT-2012"></a>[|back|](#note-Tue-May-08-175854EDT-2012) - If you don't have a functional system, you just need to resist the temptation to add side-effects to your processes. Being functional forces you down that road, but you can walk it willingly.

9 - <a name="foot-Tue-May-08-175909EDT-2012"></a>[|back|](#note-Tue-May-08-175909EDT-2012) - Embarrassment in front of your co-workers (assuming you write marginally better than a seven-year-old, or are willing to do a lot of writing without posting it anywhere).

10 - <a name="foot-Tue-May-08-175914EDT-2012"></a>[|back|](#note-Tue-May-08-175914EDT-2012) - It still causes you to focus intensely on the problem you've got, and prevents you from wasting anyone elses' time while doing so.
