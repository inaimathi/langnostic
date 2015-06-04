This is going to be a pretty disjointed, Erlang-heavy article, since that's basically been the main dominating piece of programming-related thought in my brain for the past week. It actually started a while back, when I got the unofficial heads-up that we'll soon be starting a new project at work which will call for super-massive transaction counts, require high reliability/uptime and be mostly server-based. That short-list tells me that the right tool for the job is *probably* a functional language that focuses on inter-process communication, and enforced isolation between components. Also, one of the big reasons I like my current company is that if I make a decision about what technology we're using, no one gets to tell me to fuck off.

Thus began the research...

There are the usual set of resources over there in the sidebar<a name="note-Mon-Apr-30-004835EDT-2012"></a>[|1|](#foot-Mon-Apr-30-004835EDT-2012), but do also take the time to check out [this vimeo piece featuring Joe Armstrong](http://vimeo.com/12307912). It won't really give you much insight into how to use the language, but it will show you a bit of the history and intent. Like I said, entirely worth it to hear the man talk, but here are the big points, as extracted by yours truly; he highlighted three things that were missing from Erlang<a name="note-Mon-Apr-30-004855EDT-2012"></a>[|2|](#foot-Mon-Apr-30-004855EDT-2012), one big mistake, two not-too-bad ideas and three fairly nice ideas that the team had when developing the language. He noted that these are controversial, but I tend to agree with a pretty large number of his assessments. Then again, I'm the crazy motherfucker who regularly blogs about his experiences with Lisp, Smalltalk, Erlang and Ruby, so maybe I'm not the best person to gauge what a mainstream opinion is supposed to look like.

## <a name="three-missing-things" href="#three-missing-things"></a>Three Missing Things
**Hash Maps** - JSON-style key/value data structures. Not just adding them to the system, but making them the fundamental data-type rather than tuples or arrays. I can see why, too; if you look at any tutorial or piece of Erlang code, you'll see things that fake key/value pairs using tuples. Things like `{shopping_list, [{oranges, 3}, {apples, 4}, {bread, 1}]}`, which would be better expressed as a JSON structure<a name="note-Mon-Apr-30-005108EDT-2012"></a>[|3|](#foot-Mon-Apr-30-005108EDT-2012).

**Higher Order Modules** - code in Erlang is organized into modules, which is par for the course these days, but you can't programmatically introspect on them at runtime. Joe mentioned the example of being able to send a particular standardized message and getting back a list of messages supported by the target. I guess this probably might get built into the existing language piecemeal by convention rather than specification. I'm imagining a situation where a given team agrees that they'll write all their modules to accept a `help` message which would return a list of the functions it provides and a specification of inputs they'd each accept. Thing is, 1. that wouldn't be a language-wide standard, and 2. it would take additional explicit work by the developers. If it was handled at the language level, everyone would have access to the same introspection facilities, and they'd be handled with no additional thought or deed on the developers' part.

**The Ability to `receive` a `fun`** - Erlang is a higher-order language, and you can send around function names whenever and wherever you damn well please, but apparently the built-in `receive` directive won't let you pass it an anonymous function. Ok, this isn't one you could solve with macros, but I'm not entirely sure it would be a good idea in the first place. The thing on the other end of the line isn't necessarily code you can trust, but it would certainly add more flexibility.

## <a name="one-big-mistake" href="#one-big-mistake"></a>One Big Mistake
**Lost Too Much Prolog** - Joe's a big [Prolog](http://www.gprolog.org/manual/gprolog.html) fan, which should come as no surprise to anyone who's read any Erlang tutorials, watched any [Erlang talks](http://www.youtube.com/watch?v=9uIhawQ1G0I&feature=BFa&list=PL6810EA9B7933465F), or indeed, [written any Erlang code](http://www.tryerlang.org/). I'm not qualified to comment, never having done anything approaching serious development in Prolog<a name="note-Mon-Apr-30-005141EDT-2012"></a>[|4|](#foot-Mon-Apr-30-005141EDT-2012).

## <a name="two-not-too-bad-ideas" href="#two-not-too-bad-ideas"></a>Two Not Too Bad Ideas
He gave this talk to an American audience, so he had to have a section with Good™ and Great™ ideas, though he would have preferred to be more modest about it. In deference to his preference, I'm keeping his intended titles.

**Lightweight Processes Are Ok** - 

  
> "... we've shown that you can do processes in the language, and we've shown there's no need for threads. Threads are intrinsically evil, and [shouldn't] be used. Threads were sort of this 'Oh my goodness, processes aren't efficient enough, so lets use this abomination to...' horrible things."  
>  
> -- Joe Armstrong  
  
For my part, I've got a half-written piece about `[cl-actors](https://github.com/naveensundarg/Common-Lisp-Actors)` sitting in my drafts folder. It's a pretty good, lightweight implementation of the [actor model](http://en.wikipedia.org/wiki/Actor_model) built on top of `[bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/)`. And if you like the Erlang-style message passing, do give it a shot, but it doesn't quite do the same thing as Erlang manages. The threading model means you can't expect to reliably spawn thousands of `cl-actors` on a typical machine. For comparison, [the Pragmatic book](http://pragprog.com/book/jaerlang/programming-erlang) has an example on pg 149/150 wherein Joe removes the built-in safety limit of 32 767 processes and has Erlang spawn 200 000 without breaking a sweat<a name="note-Mon-Apr-30-005314EDT-2012"></a>[|5|](#foot-Mon-Apr-30-005314EDT-2012). That seems like at least part of the story behind those [mind-boggling benchmarks](http://www.sics.se/~joe/apachevsyaws.html) that you've all probably seen by now.

**OTP Behaviours** - The correct way to think of Behaviours, Joe says, is to consider them the process equivalent of higher-order functions. They formalize basic request patterns between processes letting individuals focus on the differences. I don't actually have enough experience with them yet, but if Joe's description is accurate, I can see them being very useful when constructing complex systems with a reliability requirement.

## <a name="three-fairly-nice-ideas" href="#three-fairly-nice-ideas"></a>Three Fairly Nice Ideas
**Bit Syntax** - Is frequently useful when setting up low-level communications with non-Erlang processes, and reading files. Joe calls this out as the first of three very useful features, and it really is elegant. If you've never seen it, I encourage you to [take a quick look](http://www.erlang.org/documentation/doc-5.6/doc/programming_examples/bit_syntax.html). Short version: the notation they've set up gives you access to the same pattern matching facilities you can expect from the rest of the language, which in turn makes it very simple to decode and process binary data.

**Formalized Inter-process Relationships** - This is another feature that typical "Erlang-style" systems miss. They're useful as fuck when you're building multi-processing systems, but it seems like you could add them on later if you picked your primary primitives properly. The idea is that you can [explicitly link various processes](http://www.erlang.org/doc/reference_manual/processes.html) in certain ways. For instance, you can tell a group of processes to all fail if one of them fails, or you can tell a specific process to monitor another, restarting it in the event of an error.

**Offensive Programming** - He called it "non-[defensive programming](http://en.wikipedia.org/wiki/Defensive_programming)", but I like the negative name better. Offensive programming is the technique of programming only for the successful case, and letting any error take down the process involved (someone will be along to pick up the pieces and restart it shortly). That *would* sound crazy in your typical language, but starts looking like a good idea when your principal method of organization is a completely isolated process.

### <a name="the-ffi" href="#the-ffi"></a>The FFI

Aside from historical notes and tutorials, I've been looking at how I'd go about interfacing Erlang to other languages. The standard seems to be doing it the same way you'd interface different Erlang processes. Except that where Erlang nodes already know how to talk to each other, the protocol needs to be implemented manually for other languages. It works consistently whether you're talking to [Python](http://erlport.org/), [Ruby](https://github.com/mojombo/erlectricity), [Common Lisp](http://common-lisp.net/project/cleric/), [Java](http://www.erlang.org/documentation/doc-5.1/lib/jinterface-1.2.1/doc/html/java/index.html) or [C](http://www.erlang.org/doc/tutorial/cnode.html)<a name="note-Mon-Apr-30-005524EDT-2012"></a>[|6|](#foot-Mon-Apr-30-005524EDT-2012). All the languages I've taken a look at so far come with an established protocol to talk to Erlang in some way.

Here's a practical example that I'll actually end up refining for deployment later; a [C-based interface](http://www.imagemagick.org/script/magick-wand.php?ImageMagick=p2vadv8o3dqp83j47nqgam0au5) to some very specific [ImageMagick](http://www.imagemagick.org/script/index.php) routines.

First, the Erlang communication functions<a name="note-Mon-Apr-30-005635EDT-2012"></a>[|7|](#foot-Mon-Apr-30-005635EDT-2012)

```c
/* erl_comm.c */

#include &lt;unistd.h>

typedef unsigned char byte;

int read_cmd(byte *buff);
int write_cmd(byte *buff, int len);
int read_exact(byte *buff, int len);
int write_exact(byte *buff, int len);

int read_cmd(byte *buff) {
  int len;
  if (read_exact(buff, 2) != 2) {
    return(-1);
  }
  len = (buff[0] &lt;&lt; 8) | buff[1];
  return read_exact(buff, len);
}

int write_cmd(byte *buff, int len) {
  byte li;
  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buff, len);
}

int read_exact(byte *buff, int len){
  int i, got=0;
  do {
    if ((i = read(0, buff+got, len-got)) &lt;= 0) {
      return(i);
    }
    got +=i;
  } while (got&lt;len);
  buff[len] = '\0';
  return(len);
}

int write_exact(byte *buff, int len) {
  int i, wrote = 0;
  do {
    if ((i = write(1, buff+wrote, len-wrote)) &lt;= 0) {
      return(i);
    }
    wrote += i;
  } while (wrote&lt;len);
  return(len);
}
```

Next, the "driver". This is the bit that will actually end up being spawned and fed input by the Erlang process

```c
/* driver.c */
#include &lt;limits.h>
#include &lt;libgen.h>
#include &lt;string.h>
#include &lt;stdio.h>
#include &lt;stdlib.h>

typedef unsigned char byte;

int read_cmd(byte *buff);
int write_cmd(byte *buff, int len);

char *chop_path(char *orig) {
  char buf[PATH_MAX + 1];
  char *res, *dname, *thumb;

  res = realpath(orig, buf);
  if (res) {
    dname = dirname(res);
    thumb = strcat(dname, "/thumbnail.png");
    return thumb;
  }
  return 0;
}

int main(){
  int result, i, len;
  byte buff[255];
  char *thumb;

  while (read_cmd(buff) > 0) {
    thumb = chop_path(buff);
    result = thumbnail(buff, thumb);
    
    buff[0] = result;
    write_cmd(buff, 1);
  }
}
```

Then the actual function I'll be wanting to call<a name="note-Mon-Apr-30-005755EDT-2012"></a>[|8|](#foot-Mon-Apr-30-005755EDT-2012)

```c
/* wand.c */
#include &lt;stdio.h>
#include &lt;stdlib.h>
#include &lt;wand/MagickWand.h>

#define ThrowWandException(wand, ret) \
{ \
  char \
    *description; \
 \
  ExceptionType \
    severity; \
 \
  description=MagickGetException(wand,&severity); \
  (void) fprintf(stderr,"%s %s %lu %s\n",GetMagickModule(),description); \
  description=(char *) MagickRelinquishMemory(description); \
  wand=DestroyMagickWand(wand); \
  MagickWandTerminus(); \
  return ret; \
}

int thumbnail (char *image_name, char *thumbnail_name){

  MagickWand *magick_wand;
  MagickBooleanType status;

  /* Read an image. */
  MagickWandGenesis();
  magick_wand=NewMagickWand();
  status=MagickReadImage(magick_wand, image_name);
  if (status == MagickFalse) ThrowWandException(magick_wand, 1);
  
  /* Turn the images into a thumbnail sequence. */
  MagickResetIterator(magick_wand);
  while (MagickNextImage(magick_wand) != MagickFalse)
    MagickResizeImage(magick_wand,106,80,LanczosFilter,1.0);

  /* Write the image then destroy it. */
  status=MagickWriteImages(magick_wand, thumbnail_name, MagickTrue);
  if (status == MagickFalse) ThrowWandException(magick_wand, 2);
  magick_wand=DestroyMagickWand(magick_wand);
  MagickWandTerminus();
  
  return 0;
}
```

and, finally, the actual calling Erlang module.

```erlang
%% wand.erl
-module(wand).
-export([start/0, stop/0, restart/0]).
-export([thumbnail/1]).

start() ->
    spawn(fun() ->
                  register(wand, self()),
                  process_flag(trap_exit, true),
                  Port = open_port({spawn, "./wand"}, [{packet, 2}]),
                  loop(Port)
          end).

stop() -> wand ! stop.

restart() -> stop(), start().

thumbnail(Filename) ->
    call_port(Filename).

call_port(Msg) ->
    wand ! {call, self(), Msg},
    receive
        {wand, Result} ->
            Result
    end.

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, Msg}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {wand, decode(Data)}    
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit({port_terminated, Reason})
    end.

decode([0]) -> {ok, 0};
decode([1]) -> {error, could_not_read};
decode([2]) -> {error, could_not_write}.
```

Once all that is done, and compiled using `gcc -o wand `pkg-config --cflags --libs MagickWand` wand.c erl_comm.c driver.c`, I can call it from an Erlang process as if it were a native thumbnail generator.

```erlang

Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]
  Eshell V5.9.1  (abort with ^G)
1> c(wand).
{ok,wand}
2> wand:start().
&lt;0.39.0>
3> wand:thumbnail("original.png").
{ok, 0}
4> wand:thumbnail("/home/inaimathi/Pictures/and-another.png").
{ok, 0}
```

You'll have to take my word for it, but those both generate the appropriate `"thumbnail.png"` file in the same directory as the specified images.

All of that looks pretty complicated, but it really isn't when you sit down and read it. If I had to break it out by time involved, it would look something like


-   10% reading up on ImageMagick C interface
-   5% reading up on Erlang C FFI (emphasis on the ports)
-   5% writing  it
-   80% trying to figure out why the C component was [segfaulting when assembling a thumbnail path](http://stackoverflow.com/questions/10378112/chopping-paths-in-c) (short answer: I didn't include everything I needed to)


As a parting note, having gone through the rat's nest that is pathname manipulation in C, I hereby promise to never again bitch about Lisp's [pathname handling](http://www.gigamonkeys.com/book/practical-a-portable-pathname-library.html). Nothing like wading waist-deep in horse shit to remind you how good you've got it merely living within earshot of the stables.

* * *
##### Footnotes

1 - <a name="foot-Mon-Apr-30-004835EDT-2012"></a>[|back|](#note-Mon-Apr-30-004835EDT-2012) - Though I'll admit, the Erlang section is pretty sparse compared to the rest of them

2 - <a name="foot-Mon-Apr-30-004855EDT-2012"></a>[|back|](#note-Mon-Apr-30-004855EDT-2012) - That he'd put in if he had to do it again

3 - <a name="foot-Mon-Apr-30-005108EDT-2012"></a>[|back|](#note-Mon-Apr-30-005108EDT-2012) - For the record, I'm trying really hard not to put on my Lisp hat and say something like *"Mmmm, mmmm, this syntactic abstraction is fucking **delicious**! How's it working for **you** guys? Oh, you **haven't** had any?! That's a **shame**..."* in an obnoxiously smug voice. It's difficult, and this footnote may count as a failure. Sorry.

4 - <a name="foot-Mon-Apr-30-005141EDT-2012"></a>[|back|](#note-Mon-Apr-30-005141EDT-2012) - In fact the entirety of my related experience is the appropriate chapter from [7 Languages...](http://pragprog.com/book/btlang/seven-languages-in-seven-weeks), flipping through [the Reasoned Schemer](http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=10663) and the [SICP lectures](http://www.youtube.com/watch?v=2Op3QLzMgSY&feature=BFa&list=PL8FE88AA54363BC46) wherein prolog is briefly implemented on top of Lisp. Thant link is to the playlist rather than the correct episode; it's been a while, and I no longer remember which it was specifically.

5 - <a name="foot-Mon-Apr-30-005314EDT-2012"></a>[|back|](#note-Mon-Apr-30-005314EDT-2012) - This was reportedly on a 2.4gHz Celeron machine with a half-gig of ram, so that was *not* a consequence of awesome hardware

6 - <a name="foot-Mon-Apr-30-005524EDT-2012"></a>[|back|](#note-Mon-Apr-30-005524EDT-2012) - Actually, that's half true. There are three different ways to interface with a C program; you can do [port-based communication with a custom protocol](http://www.erlang.org/doc/tutorial/c_portdriver.html), you can [call C natively](http://www.erlang.org/doc/tutorial/nif.html) at the risk of system collapse with errors, or you can [implement the Erlang protocol](http://www.erlang.org/doc/tutorial/cnode.html) and pretend to be an Erlang process for the purposes of interoperability. The other languages I've taken a look at do that last one, but you've got options if you're rolling your own

7 - <a name="foot-Mon-Apr-30-005635EDT-2012"></a>[|back|](#note-Mon-Apr-30-005635EDT-2012) - Ripped bleeding from [Programming Erlang](http://pragprog.com/book/jaerlang/programming-erlang). It which won't change at all, regardless of what specific protocol I end up picking

8 - <a name="foot-Mon-Apr-30-005755EDT-2012"></a>[|back|](#note-Mon-Apr-30-005755EDT-2012) - With thanks to the [reference implementation](http://www.imagemagick.org/source/wand.c) from the ImageMagick team
