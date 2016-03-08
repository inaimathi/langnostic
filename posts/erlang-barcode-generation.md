Ok, this isn't actually all Erlang. In fact, by line-count, it's a Postscript project, but [all of those lines were already written](https://code.google.com/p/postscriptbarcode/) by someone else. Also, I'm not sure whether I'll get the same benefit here that I got out of [my Strifebarge write-up](http://langnostic.blogspot.ca/2012/02/strifebarge-turn-based-web-games-in.html), but it's the third such piece, so I've gone back and added labels to group them.

"Almost Literate Programming".

What I'm doing isn't quite the [LP that Knuth advocates](http://www.literateprogramming.com/) because it doesn't self-extract, share space with the executable source, or make use of variable labels to automatically update certain portions. However, it still gains me considerable reflective clarity about what the goal of the program is, and it hopefully conveys the essence to whoever happens to be reading. With that out of the way...

### Generating Barcodes

As you may have noticed from the above links, there already exists a [Postscript-based barcode generator](https://code.google.com/p/postscriptbarcode/) which I'm going to use pretty shamelessly in order to generate bitmap barcodes of various descriptions. Taking a look at the [actual code](https://code.google.com/p/postscriptbarcode/downloads/detail?name=barcode-2012-04-26.ps) for that generator should make it obvious that you probably *don't* want to just echo the entire system every time you need to generate something<a name="note-Tue-May-15-220424EDT-2012"></a>[|1|](#foot-Tue-May-15-220424EDT-2012). We'll get to that though, lets start from the system side first. This is what a `.app` declaration looks like in Erlang

```make
# Makefile
all: *.erl *.c
        make wand
        erlc -W *erl

run:
        erl -name ps_barcode@127.0.1.1 -eval 'application:load(ps_barcode).' -eval 'application:start(ps_barcode).'

wand: wand.c erl_comm.c driver.c
        gcc -o wand `pkg-config --cflags --libs MagickWand` wand.c erl_comm.c driver.c

clean:
        rm wand
        rm *beam
```

```erlang
%% ps_barcode.app
{application, ps_barcode,
 [{description, "barcode image generator based on ps-barcode"},
  {vsn, "1.0"},
  {modules, [ps_barcode_app, ps_barcode_supervisor, barcode_data, wand, ps_bc]},
  {registered, [ps_bc, wand, ps_barcode_supervisor]},
  {applications, [kernel, stdlib]},
  {mod, {ps_barcode_app, []}},
  {start_phases, []}]}.
```

```erlang
-module(ps_barcode_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) -> ps_barcode_supervisor:start_link(StartArgs).
stop(_State) -> ok.
```

```erlang
-module(ps_barcode_supervisor).
-behavior(supervisor).

-export([start/0, start_for_testing/0, start_link/1, init/1]).

start() ->
    spawn(fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_for_testing() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{tag1,
            {wand, start, []},
            permanent,
            brutal_kill,
            worker,
            [wand]},
           {tag2,
            {ps_bc, start, []},
            permanent,
            10000,
            worker,
            [ps_bc]}]}}.
```

The `Makefile` is not, strictly speaking, necessary, but a bunch of stuff needs to be done manually in its absence. The above code is approximately equivalent to [a Lisp `.asd` file](https://github.com/xach/quickproject/), in that it tells Erlang what needs to be compiled/called in order to run the system I'm about to define<a name="note-Tue-May-15-220604EDT-2012"></a>[|2|](#foot-Tue-May-15-220604EDT-2012).

```erlang
  {modules, [ps_barcode_app, ps_barcode_supervisor, barcode_data, wand, ps_bc]},
```


That line specifies which other modules we'll be loading as part of the application, as well as their start order (which is relevant for a certain supervision strategy).

```erlang
  {registered, [ps_bc, wand, ps_barcode_supervisor]},
```

That one specifies registered processes we expect.

```erlang
  {mod, {ps_barcode_app, []}},
```

That one tells Erlang which module's `start` function to call in order to start the application, and what arguments to pass it as `StartArgs`.

```erlang
init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{tag1,
            {wand, start, []},
            permanent,
            brutal_kill,
            worker,
            [wand]},
           {tag2,
            {ps_bc, start, []},
            permanent,
            10000,
            worker,
            [ps_bc]}]}}.
```

*That* does something interesting; it defines how the [supervisor](http://www.erlang.org/doc/man/supervisor.html) should act, and how it should treat its child processes. `{one_for_one, 3, 10}` means that if a supervised process errors, it should be restarted on its own up to 3 times in 10 seconds<a name="note-Tue-May-15-220658EDT-2012"></a>[|3|](#foot-Tue-May-15-220658EDT-2012). Both sub-processes are `permanent`<a name="note-Tue-May-15-220712EDT-2012"></a>[|4|](#foot-Tue-May-15-220712EDT-2012) `worker`s<a name="note-Tue-May-15-220719EDT-2012"></a>[|5|](#foot-Tue-May-15-220719EDT-2012). The last interesting bit is the `brutal_kill`/`10000` part; that's the `Shutdown` variable. It determines how the process should be terminated; `brutal_kill` means "kill the process right away", an integer means "send the process a stop command and wait up to this many milliseconds, then kill it".

Lets follow the applications' start order and move on to

```erlang
-module(barcode_data).
-export([read_default_file/0, read_file/1]).
-export([export_ets_file/1, import_ets_file/0]).

export_ets_file(Table) -> ets:tab2file(Table, "ps-barcode-blocks").
import_ets_file() -> {ok, Tab} = ets:file2tab(filename:absname("ps-barcode-blocks")), Tab.

read_default_file() -> read_file("barcode.ps").
read_file(Filename) ->
    {ok, File} = file:open(Filename, [read]),
    TableId = ets:new(ps_barcode_blocks, [ordered_set]),
    trim_flash(File),
    {ok, Tab} = read_all_blocks(File, TableId),
    file:close(File),
    Tab.

trim_flash(IoDevice) -> read_until(IoDevice, "% --BEGIN TEMPLATE").

read_all_blocks(IoDevice, TableId) ->
    case Res = read_block(IoDevice) of
        [] -> {ok, TableId};
        _ -> ets:insert(TableId, parse_block(Res)),
             read_all_blocks(IoDevice, TableId)
    end.

read_block(IoDevice) -> read_until(IoDevice, "% --END ").

parse_block([["%", "BEGIN", "PREAMBLE"] | Body]) ->
    {preamble, lists:append(Body)};
parse_block([["%", "BEGIN", "RENDERER", Name] | Body]) ->
    {list_to_atom(Name), renderer, lists:append(Body)};
parse_block([["%", "BEGIN", "ENCODER", Name] | Body]) ->
    parse_encoder_meta(Name, Body);
parse_block(_) -> {none}.

parse_encoder_meta (Name, Encoder) -> parse_encoder_meta(Name, Encoder, [], {[], [], []}).
parse_encoder_meta (Name, [["%", "RNDR:" | Renderers] | Rest], Acc, {_, R, S}) ->
    parse_encoder_meta(Name, Rest, Acc, {Renderers, R, S});
parse_encoder_meta (Name, [["%", "REQUIRES" | Reqs] | Rest], Acc, {A, _, S}) ->
    parse_encoder_meta(Name, Rest, Acc, {A, Reqs, S});
parse_encoder_meta (Name, [["%", "SUGGESTS" | Suggs] | Rest], Acc, {A, R, _}) ->
    parse_encoder_meta(Name, Rest, Acc, {A, R, Suggs});
parse_encoder_meta (Name, [["%", "EXOP:" | Exop] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Name, Rest, [{def_arg, Exop} | Acc], Cmp);
parse_encoder_meta (Name, [["%", "EXAM:" | Exam] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Name, Rest, [{example, string:join(Exam, " ")} | Acc], Cmp);
parse_encoder_meta (Name, [["%" | _] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Name, Rest, Acc, Cmp);
parse_encoder_meta (Name, Body, [DefArgs, Example], {A, R, S}) ->
    Reqs = [list_to_atom(strip_nl(X)) || X <- lists:append([A, R, S])],
    {list_to_atom(Name), encoder, {requires, Reqs}, Example, DefArgs, lists:append(Body)}.

strip_nl(String) -> string:strip(String, right, $\n).

read_until(IoDevice, StartsWith) -> read_until(IoDevice, StartsWith, []).
read_until(IoDevice, StartsWith, Acc) ->
    case file:read_line(IoDevice) of
        {ok, "\n"} ->
            read_until(IoDevice, StartsWith, Acc);
        {ok, Line} ->
            case lists:prefix(StartsWith, Line) of
                true ->
                    lists:reverse(Acc);
                false ->
                    read_until(IoDevice, StartsWith,
                               [process_line(Line) | Acc])
            end;
        {error, _} -> error;
        eof -> lists:reverse(Acc)
    end.

process_line(Line) ->
    case lists:prefix("% --", Line) of
        true -> split_directive_line(Line);
        false -> Line
    end.

split_directive_line(Line) ->
    [X || X <- re:split(strip_nl(Line), "( |--)", [{return, list}]),
          X /= " ", X /= [], X /= "--", X /="\n"].
```

This is a reasonably simple reader program. The goal of it is to break [that 17111 line .ps file](https://code.google.com/p/postscriptbarcode/downloads/detail?name=barcode-2012-04-26.ps) into individual components. First, a `preamble` (basic definitions that need to go into each file), then a set of `renderer`s<a name="note-Tue-May-15-220847EDT-2012"></a>[|6|](#foot-Tue-May-15-220847EDT-2012), and a rather large number of `encoder`s<a name="note-Tue-May-15-220854EDT-2012"></a>[|7|](#foot-Tue-May-15-220854EDT-2012). These components are stored in an [ETS](http://www.erlang.org/doc/man/ets.html) table held in memory. The initial Postscript file only needs to be parsed once; the resulting ETS table is then exported to a file on disk so that it can just be loaded in the future.

Do note the nested `case` statements there. [Last time, I complained about the guards](http://langnostic.blogspot.ca/2012/05/hot-erlang-code.html#foot-Tue-May-08-175733EDT-2012), and this is why. Really, I should have been able to write that as

```erlang
        ...
        {ok, Line} where lists:prefix(StartsWith, Line) -> lists:reverse(Acc);
        {ok, Line} -> read_until(IoDevice, StartsWith, [process_line(Line) | Acc]);
        ...
```

but even though `lists:prefix` is a perfectly functional predicate, it's not in [that blessed subset of Erlang](http://www.erlang.org/doc/reference_manual/expressions.html#id79432) that can be called from within a guard sequence. The consequence, in this case, is that I have to bust out a second `case` block, and waste six lines doing it. Moving onto sorting PS blocks...

```erlang
parse_block([["%", "BEGIN", "PREAMBLE"] | Body]) ->
    {preamble, lists:append(Body)};
parse_block([["%", "BEGIN", "RENDERER", Name] | Body]) ->
    {list_to_atom(Name), renderer, lists:append(Body)};
parse_block([["%", "BEGIN", "ENCODER", Name] | Body]) ->
    parse_encoder_meta(Name, Body);
parse_block(_) -> {none}.
```

The `preamble` and `renderer`s are really just named strings, but the `encoder`s have more metadata about them.

```erlang
parse_encoder_meta (Name, Encoder) -> parse_encoder_meta(Name, Encoder, [], {[], [], []}).
parse_encoder_meta (Name, [["%", "RNDR:" | Renderers] | Rest], Acc, {_, R, S}) ->
    parse_encoder_meta(Name, Rest, Acc, {Renderers, R, S});
parse_encoder_meta (Name, [["%", "REQUIRES" | Reqs] | Rest], Acc, {A, _, S}) ->
    parse_encoder_meta(Name, Rest, Acc, {A, Reqs, S});
parse_encoder_meta (Name, [["%", "SUGGESTS" | Suggs] | Rest], Acc, {A, R, _}) ->
    parse_encoder_meta(Name, Rest, Acc, {A, R, Suggs});
parse_encoder_meta (Name, [["%", "EXOP:" | Exop] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Name, Rest, [{def_arg, Exop} | Acc], Cmp);
parse_encoder_meta (Name, [["%", "EXAM:" | Exam] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Name, Rest, [{example, string:join(Exam, " ")} | Acc], Cmp);
parse_encoder_meta (Name, [["%" | _] | Rest], Acc, Cmp) ->
    parse_encoder_meta(Name, Rest, Acc, Cmp);
parse_encoder_meta (Name, Body, [DefArgs, Example], {A, R, S}) ->
    Reqs = [list_to_atom(strip_nl(X)) || X <- lists:append([A, R, S])],
    {list_to_atom(Name), encoder, {requires, Reqs}, Example, DefArgs, lists:append(Body)}.
```

This is not the most elegant function. In fact, now that I look at it, it seems like I could fairly easily replace that `{A, R, S}` tuple with a list accumulator.

> EDIT:
> Turns out there was a reason I did it this way; we need this data to be in the order of Renderers, Required, Suggested, but the order they're parsed in is actually Required, Suggested, Renderers (also, some components have no requirements, and some have no suggestions). The ordering is confusing enough that I just decided to keep it explicit.
>
> Wed, 16 May, 2012

What we're doing here is breaking apart an `encoder` block, and pulling out


- the list of other blocks we need to output before this one<a name="note-Tue-May-15-221005EDT-2012"></a>[|8|](#foot-Tue-May-15-221005EDT-2012)
- a piece of example data that this particular encoder can handle<a name="note-Tue-May-15-221009EDT-2012"></a>[|9|](#foot-Tue-May-15-221009EDT-2012)
- the default arguments to passed to this `encoder`
- the body code of this `encoder`


The list of required blocks is exhaustive for each `encoder`, so we don't need to recursively check requirements later, it's enough to store and act on all requirements of a given barcode.

```erlang
-export([read_default_file/0, read_file/1]).
-export([export_ets_file/1, import_ets_file/0]).
```

Those exported functions are really all that a user of this module should ever need to call; you're either processing a new revision of the `ps` file, or you're importing the already exported ETS table derived from the `ps` file, or you're exporting a new ETS table for later loading. Now that we've seen how we store the relevant data, lets take a look at

```erlang
-module(wand).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([process/1]).

process(Filename) -> gen_server:call(?MODULE, {process_barcode, Filename}).

handle_call({process_barcode, Filename}, _From, State) ->
    State ! {self(), {command, Filename}},
    receive
        {State, {data, Data}} ->
            {reply, decode(Data), State}
    end;
handle_call({'EXIT', _Port, Reason}, _From, _State) ->
    exit({port_terminated, Reason}).

decode([0]) -> {ok, 0};
decode([1]) -> {error, could_not_read};
decode([2]) -> {error, could_not_write}.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, open_port({spawn, filename:absname("wand")}, [{packet, 2}])}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> State ! {self(), close}, ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
```

This is actually pretty much the same C Port file I [used last time](http://langnostic.blogspot.ca/2012/04/notes-from-borders-of-erlang.html), except that this one has been re-written to use [`gen_server`](http://www.erlang.org/doc/man/gen_server.html), rather than being plain Erlang code. I still refuse to use that godawful file template they ship with their Emacs mode though<a name="note-Tue-May-15-221156EDT-2012"></a>[|10|](#foot-Tue-May-15-221156EDT-2012). All it does is call out to a C program named `wand` to do the actual image processing involved in generating these barcodes. All you need to know is that we send it a barcodes' file name, and it quickly generates a high-res PNG version in the same folder.

Right, that's it for the periphery, lets finally dive into

```erlang
-module(ps_bc).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([help/0, help/1, write/3, write/5, generate/2, generate/3, change/1, make_tempname/0]).

help() -> gen_server:call(?MODULE, help).
help(BarcodeType) -> gen_server:call(?MODULE, {help, BarcodeType}).
write(DestFolder, BarcodeType, Data) ->
    write(DestFolder, BarcodeType, Data, 200, 200).
write(DestFolder, BarcodeType, Data, Width, Height) ->
    gen_server:call(?MODULE, {write, DestFolder, BarcodeType, Data, Width, Height}).
generate(BarcodeType, Data) -> generate("/tmp/", BarcodeType, Data).
generate(DestFolder, BarcodeType, Data) ->
    NameOfTempFile = write(DestFolder, BarcodeType, Data),
    wand:process(NameOfTempFile),
    NameOfTempFile.
change(TableId) -> gen_server:call(?MODULE, {change, TableId}).

handle_call(help, _From, State) ->
    {reply, ets:match(State, {'$1', encoder, '_', '_', '_', '_'}), State};
handle_call({help, BarcodeType}, _From, State) ->
    {reply, ets:match(State, {BarcodeType, encoder, '_', '$1', '_', '_'}), State};
handle_call({write, DestFolder, BarcodeType, Data, Width, Height}, _From, State) ->
    Fname = make_tempname(DestFolder),
    {ok, File} = file:open(Fname, [write, exclusive]),
    [[{requires, CompList}, {def_arg, ExArgs}]] = ets:match(State, {BarcodeType, encoder, '$1', '_', '$2', '_'}),
    file:write(File, io_lib:format("%!PS-Adobe-2.0\n%%BoundingBox: 0 0 ~w ~w\n%%LanguageLevel: 2\n", [Width, Height])),
    write_component(preamble, State, File),
    file:write(File, "\n/Helvetica findfont 10 scalefont setfont\n"),
    lists:map(fun (C) -> write_component(C, State, File) end, CompList),
    write_component(BarcodeType, State, File),
    write_barcode(File, BarcodeType, ExArgs, Data),
    file:close(File),
    {reply, Fname, State};
handle_call({change_table, Tab}, _From, _State) ->
    {reply, {watching_table, Tab}, Tab}.

make_tempname() ->
    {A, B, C} = now(),
    [D, E, F] = lists:map(fun integer_to_list/1, [A, B, C]),
    lists:append(["tmp.", D, ".", E, ".", F]).
make_tempname(TargetDir) ->
    filename:absname_join(TargetDir, make_tempname()).

write_component(preamble, Table, File) ->
    [[Pre]] = ets:match(Table, {preamble, '$1'}),
    file:write(File, Pre);
write_component(Name, Table, File) ->
    file:write(File, lookup_component(Name, Table)).

write_barcode(File, datamatrix, _, Data)        -> format_barcode_string(File, datamatrix, "", Data);
write_barcode(File, BarcodeType, ExArgs, Data)  -> format_barcode_string(File, BarcodeType, string:join(ExArgs, " "), Data).

format_barcode_string(File, BarcodeType, ExArgString, DataString) ->
    io:format(File, "10 10 moveto (~s) (~s) /~s /uk.co.terryburton.bwipp findresource exec showpage",
              [DataString, ExArgString, BarcodeType]).

lookup_component(Name, Table) ->
    Ren = ets:match(Table, {Name, renderer, '$1'}),
    Enc = ets:match(Table, {Name, encoder, '_', '_', '_', '$1'}),
    case {Ren, Enc} of
        {[], [[Res]]} -> Res;
        {[[Res]], []} -> Res
    end.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). %% {local/global, Name}, Mod, InitArgs, Opts
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, barcode_data:import_ets_file()}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
```

This is where the meat of the application resides, so I'll take my time with it.

First off, note that the `init` function loads that ETS file we generated in `barcode_data`.

```erlang
init([]) -> {ok, barcode_data:import_ets_file()}.
```

That's where our data is stored, and we'll be looking up components by referring to it.

```erlang
lookup_component(Name, Table) ->
    Ren = ets:match(Table, {Name, renderer, '$1'}),
    Enc = ets:match(Table, {Name, encoder, '_', '_', '_', '$1'}),
    case {Ren, Enc} of
        {[], [[Res]]} -> Res;
        {[[Res]], []} -> Res
    end.
```

This is (again) not the most elegant code. Really, what I'd want to do is look up `Ren` first, check if it returned something, *then* check whether an `Enc` exists. That would save me a look-up every once in a while<a name="note-Tue-May-15-221255EDT-2012"></a>[|11|](#foot-Tue-May-15-221255EDT-2012). Do take note that there's no clause to handle the event that a faulty index was passed; in that case, the process will fail with an unmatched pattern and promptly be restarted. This function is in turn used by `write_component` to actually output the given block to a file

```erlang
write_component(preamble, Table, File) ->
    [[Pre]] = ets:match(Table, {preamble, '$1'}),
    file:write(File, Pre);
write_component(Name, Table, File) ->
    file:write(File, lookup_component(Name, Table)).
```

Before we tear into `handle`, just one more note about the remaining interesting helper function

```erlang
make_tempname() ->
    {A, B, C} = now(),
    [D, E, F] = lists:map(fun integer_to_list/1, [A, B, C]),
    lists:append(["tmp.", D, ".", E, ".", F]).
make_tempname(TargetDir) ->
    filename:absname_join(TargetDir, make_tempname()).
```

Actually, two functions (`make_tempname/0` and `make_tempname/1`). I thought about just using `os:cmd("mktemp")` instead, but decided against it. `make_tempname` uses `now()` to generate a unique temporary filename. It optionally takes a directory specification, in which case it creates an absolute filename in that directory.

By the way, that's how you handle optional arguments in Erlang. You create multiple functions with the same name, but with different arity and just write the matching expression for each. It's surprisingly elegant, and the only thing differentiating these from a single function declaration is that they're separated by `.` rather than by `;`. Obviously, if you plan on extending such a function to the users of your module, you need to export all the arities you've defined. I wasn't being *needlessly* pedantic earlier, Erlang treats `make_tempname/0` and `make_tempname/1` as completely separate functions.

Right, now then.

```erlang
handle_call(help, _From, State) ->
    {reply, ets:match(State, {'$1', encoder, '_', '_', '_', '_'}), State};
handle_call({help, BarcodeType}, _From, State) ->
    {reply, ets:match(State, {BarcodeType, encoder, '_', '$1', '_', '_'}), State};
handle_call({write, DestFolder, BarcodeType, Data, Width, Height}, _From, State) ->
    Fname = make_tempname(DestFolder),
    {ok, File} = file:open(Fname, [write, exclusive]),
    [[{requires, CompList}, {def_arg, ExArgs}]] = ets:match(State, {BarcodeType, encoder, '$1', '_', '$2', '_'}),
    file:write(File, io_lib:format("%!PS-Adobe-2.0\n%%BoundingBox: 0 0 ~w ~w\n%%LanguageLevel: 2\n", [Width, Height])),
    write_component(preamble, State, File),
    file:write(File, "\n/Helvetica findfont 10 scalefont setfont\n"),
    lists:map(fun (C) -> write_component(C, State, File) end, CompList),
    write_component(BarcodeType, State, File),
    write_barcode(File, BarcodeType, ExArgs, Data),
    file:close(File),
    {reply, Fname, State};
handle_call({change_table, Tab}, _From, _State) ->
    {reply, {watching_table, Tab}, Tab}.
```

The last directive there should probably be implemented as a `handle_cast` rather than `handle_call`<a name="note-Tue-May-15-221415EDT-2012"></a>[|12|](#foot-Tue-May-15-221415EDT-2012). The first two should probably return processed data rather than raw ETS results. Rest assured that mental notes have been made. The message `help` returns a list of available `encoder`s<a name="note-Tue-May-15-221432EDT-2012"></a>[|13|](#foot-Tue-May-15-221432EDT-2012), while asking for help with a specific `encoder` will return its example data. All the meat is in that extra large message handler in the middle.

Deep breath.

A message of `{write, DestFolder, BarcodeType, Data, Width, Height}` will output `Data` in a `BarcodeType` barcode in the `DestFolder` folder and format it to `Width`x`Height` dimensions. That's actually going to get trickier. Right now, the dimensions are just assumed to be 200x200 in the initial PS, and that C module is expected to output a properly formatted PS file. There are a few problems with that though<a name="note-Tue-May-15-221657EDT-2012"></a>[|14|](#foot-Tue-May-15-221657EDT-2012), so what I will ultimately want to do is have the C module return the appropriate dimensions and have `ps_bc` change this initial file later. That's another TODO.

What the `write` message actually does, in order is


- generates a tempfile name for the directory it was passed
- opens that `File` for output
- looks up the required blocks in our ETS table
- writes the preamble to `File`
- writes the required blocks to `File`
- writes the barcode component to `File`
- writes a Postscript directive invoking that component with `Data` to `File`
- closes `File`
- replies with the absolute tempfile name that it generated


And there you have it, we now have a barcode PS file in the specified location.

The rest of the functions here are either `gen_server` pieces (which I won't go into), or interface functions (which I will)

```erlang
help() -> gen_server:call(?MODULE, help).
help(BarcodeType) -> gen_server:call(?MODULE, {help, BarcodeType}).
write(DestFolder, BarcodeType, Data) ->
    write(DestFolder, BarcodeType, Data, 200, 200).
write(DestFolder, BarcodeType, Data, Width, Height) ->
    gen_server:call(?MODULE, {write, DestFolder, BarcodeType, Data, Width, Height}).
generate(BarcodeType, Data) -> generate("/tmp/", BarcodeType, Data).
generate(DestFolder, BarcodeType, Data) ->
    NameOfTempFile = write(DestFolder, BarcodeType, Data),
    wand:process(NameOfTempFile),
    NameOfTempFile.
change(TableId) -> gen_server:call(?MODULE, {change, TableId}).
```

This is a set of exported functions to let outside modules easily interact with the internal `ps_bc` process. `change`, `help` and `write` map to the corresponding `handle_call` messages we looked at earlier<a name="note-Tue-May-15-222057EDT-2012"></a>[|15|](#foot-Tue-May-15-222057EDT-2012). `generate` is something else. This is the principal function I expect to be called from outside the module, though AFAIK, there's no way to highlight that from within the code. To that end, it collects everything you need to create a barcode from start to finish; it accepts a `BarcodeType` and `Data` (and optionally a `DestFolder`) and calls `write/3` to create the directory, then `wand:process` to create the corresponding PNG and rasterized PS file, and finally returns the tempfile name that it generated. That should probably actually return a list of absolute file-names it created rather than just the base name. Mental note number 6.

Whew! At the risk of pulling a Yegge, this piece is turning out *a lot* longer than I though it was going to be. Lets get it wrapped up quickly.

### Nitrogen

[Nitrogen](http://nitrogenproject.com/) is an Erlang web framework I've been playing with. I won't explain it in depth, just use it to show you how you'd go about invoking the above program for realsies. In fact, here's a `nitrogen/rel/nitrogen/site/src/index.erl` that will call out to `ps_barcode` to generate a barcode based on user input and let them download the bitmap and Postscript file:

```erlang
%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to Nitrogen".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() ->
    [
        #h3 { text="PS Barcode Generator" },
        #h1 { text="In MOTHERFUCKING ERLANG"},
        #p{},
        #textbox { id=barcode_data, text=get_example(qrcode)},
        barcode_type_dropdown(qrcode),
        #button { id=button, text="Generate", postback=click },
        #p{ id=result, body=[
            #image { id=barcode_img },
            #p { id=barcode_link }
        ]}
    ].

barcode_type_dropdown(DefaultType) ->
    Types = rpc:call('ps_barcode@127.0.1.1', ps_bc, help, []),
    #dropdown { id=barcode_type, value=DefaultType, postback=select_type,
        options=lists:map(fun ([T]) -> #option {text=T, value=T} end, Types)
    }.

get_example(BarcodeType) ->
    [[{example,Example}]] = rpc:call('ps_barcode@127.0.1.1', ps_bc, help, [BarcodeType]),
    Example.

event(click) ->
    [_, Fname] = re:split(
        rpc:call('ps_barcode@127.0.1.1', ps_bc, generate,
            [filename:absname("site/static/images"), list_to_atom(wf:q(barcode_type)), wf:q(barcode_data)]),
        "site/static", [{return, list}]),
    wf:replace(barcode_img,
        #image {
            id=barcode_img,
            image=string:concat(Fname, ".png"),
            actions=#effect { effect=highlight }
    }),
    wf:replace(barcode_link,
        #link {
            id=barcode_link,
            text="Download PS file",
            url=string:concat(Fname, ".ps")
    });
event(select_type) ->
    wf:set(barcode_data, get_example(list_to_atom(wf:q(barcode_type)))).
```

The actual calls to our application happen

```erlang
get_example(BarcodeType) ->
    [[{example,Example}]] = rpc:call('ps_barcode@127.0.1.1', ps_bc, help, [BarcodeType]),
    Example.
```

here<a name="note-Tue-May-15-222251EDT-2012"></a>[|16|](#foot-Tue-May-15-222251EDT-2012) and

```erlang
        ...
        rpc:call('ps_barcode@127.0.1.1', ps_bc, generate,
            [filename:absname("site/static/images"), list_to_atom(wf:q(barcode_type)), wf:q(barcode_data)]),
        "site/static", [{return, list}]),
        ...
```

here. Recall that `make run` on the `Makefile` I defined earlier started a node named `'ps_barcode@127.0.1.1'` and started our application in it. So, if we want to use it from another Erlang node, all we have to do is start them both up using the same [`cookie`](http://erlang.org/pipermail/erlang-questions/2001-December/004153.html), and then use the built in `rpc:call` function, specifying the appropriate node, module, function and arguments. The return message is going to be a response from our application.

The code shown here won't actually run on its own<a name="note-Tue-May-15-222809EDT-2012"></a>[|17|](#foot-Tue-May-15-222809EDT-2012), I left out the C file<a name="note-Tue-May-15-222815EDT-2012"></a>[|18|](#foot-Tue-May-15-222815EDT-2012), as well as the actual [barcode.ps](https://code.google.com/p/postscriptbarcode/downloads/detail?name=barcode-2012-04-26.ps) that the whole thing is based on. I'll act on the mental notes I've collected first, and then toss the whole thing up on [my github](https://github.com/Inaimathi) for you to play with. The nitrogen module is minimal enough that I won't feel bad for leaving it out, but the one above should work with your copy of nitrogen.

It's actually just a minimally modified version of the default `index.erl` file that comes with the framework, the only interesting pieces in it are the `rpc:call` lines which demonstrate the **hands-down most interesting thing** about Erlang. The thing that justifies putting up with all the warts and annoyances<a name="note-Tue-May-15-222916EDT-2012"></a>[|19|](#foot-Tue-May-15-222916EDT-2012). I'll expand on that next time though, this was already more than enough stuff coming out of my mind.

* * *
##### Footnotes

1 - <a name="foot-Tue-May-15-220424EDT-2012"></a>[|back|](#note-Tue-May-15-220424EDT-2012) - The complete file is 17111 lines, and we really only need about 800-1200 at the outside to generate a single specific barcode.

2 - <a name="foot-Tue-May-15-220604EDT-2012"></a>[|back|](#note-Tue-May-15-220604EDT-2012) - Incidentally, I didn't do this first. I sort of wish I had in retrospect, because it would have saved me some dicking around with `erl`, but I actually wrote the code first, then wrote the above based on it. Also incidentally, a lot of it doesn't seem like much of it will change on a project-by-project basis. That tells me that we're either working with the wrong abstractions, or there are tricky things you can do at this stage that I haven't yet grasped. It also tells me that I should probably write some generation scripts for it.

3 - <a name="foot-Tue-May-15-220658EDT-2012"></a>[|back|](#note-Tue-May-15-220658EDT-2012) - `one_for_all` and `rest_for_one` are other [possible strategies](http://www.erlang.org/doc/design_principles/sup_princ.html#strategy), `_all` restarts all child processes rather than just the one that errored, and `rest_` just restarts processes later in the start order.

4 - <a name="foot-Tue-May-15-220712EDT-2012"></a>[|back|](#note-Tue-May-15-220712EDT-2012) - Which means they get restarted when they error, *and* hang around after they've finished their work.

5 - <a name="foot-Tue-May-15-220719EDT-2012"></a>[|back|](#note-Tue-May-15-220719EDT-2012) - Which means that we have a pretty shallow supervision tree in this case, but we really don't need more.

6 - <a name="foot-Tue-May-15-220847EDT-2012"></a>[|back|](#note-Tue-May-15-220847EDT-2012) - Routines that do general operations for a particular class of barcode, such as linear or matrix.

7 - <a name="foot-Tue-May-15-220854EDT-2012"></a>[|back|](#note-Tue-May-15-220854EDT-2012) - Routines that do the job of converting a specific piece of data into a particular type of barcode, such as qrcode, code93 or datamatrix.

8 - <a name="foot-Tue-May-15-221005EDT-2012"></a>[|back|](#note-Tue-May-15-221005EDT-2012) - `renderer`s, required `encoder`s and suggested `encoders`.

9 - <a name="foot-Tue-May-15-221009EDT-2012"></a>[|back|](#note-Tue-May-15-221009EDT-2012) - Some, like datamatrix and qrcode, can handle almost arbitrary string information, while others are restricted to a subset of ascii, and others require a specific number of numeric characters.

10 - <a name="foot-Tue-May-15-221156EDT-2012"></a>[|back|](#note-Tue-May-15-221156EDT-2012) - As an aside here, that's one of the things that really rustles my jimmies about Erlang. I've gotten extremely used to including a pretty extensive documentation string with each Common Lisp function and method, knowing that a potential user will be able to make full use of any `describe` calls they make. It's actually even better for methods, since you get the documentation for the `generic` you define, as well as a compilation of all doc-strings for the related `defmethod` calls. Erlang isn't having any of this shit. If you want to include doc-strings, you can damn well write Java-style precisely formatted comments and use a separate doc extractor to read them. I guess this is how most languages do it? It still seems stupid to have a system this dynamic that *doesn't* allow runtime documentation pokes. Sigh. Ok, let's get back to it.

11 - <a name="foot-Tue-May-15-221255EDT-2012"></a>[|back|](#note-Tue-May-15-221255EDT-2012) - Which, granted, isn't really worth saving given how blazingly fast ETS is, but still.

12 - <a name="foot-Tue-May-15-221415EDT-2012"></a>[|back|](#note-Tue-May-15-221415EDT-2012) - The only difference being that `handle_cast` doesn't send a response message to its caller.

13 - <a name="foot-Tue-May-15-221432EDT-2012"></a>[|back|](#note-Tue-May-15-221432EDT-2012) - Which we'll use later to give the user something to do about them.

14 - <a name="foot-Tue-May-15-221657EDT-2012"></a>[|back|](#note-Tue-May-15-221657EDT-2012) - Specifically, since I'm using the Imagemagick API, the PS that it outputs is actually rasterised. That means it'll be much larger than the initial file and take that much longer to output. Literally the only advantage to it is that it properly sets the width and height of the document.

15 - <a name="foot-Tue-May-15-222057EDT-2012"></a>[|back|](#note-Tue-May-15-222057EDT-2012) - Note that I do export `help/0`, `help/1`, `write/3` and `write/5` separately.

16 - <a name="foot-Tue-May-15-222251EDT-2012"></a>[|back|](#note-Tue-May-15-222251EDT-2012) - Which, again, really should be expecting a naked string response rather than a raw ETS lookup record.

17 - <a name="foot-Tue-May-15-222809EDT-2012"></a>[|back|](#note-Tue-May-15-222809EDT-2012) - The complete code doesn't quite work yet either. Most of it does what it's supposed to, but I've already found [one odd case where things don't quite work](http://stackoverflow.com/questions/10604400/handling-timeouts-in-otp) the way they're supposed to. Tips and patches welcome.

18 - <a name="foot-Tue-May-15-222815EDT-2012"></a>[|back|](#note-Tue-May-15-222815EDT-2012) - Which I was actually going to discuss, but this has gone on quite long enough already.

19 - <a name="foot-Tue-May-15-222916EDT-2012"></a>[|back|](#note-Tue-May-15-222916EDT-2012) - At least, until I learn enough about it to put together an analogous system in Common Lisp :P
