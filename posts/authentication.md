This is something I've been thinking about a bit lately. Actually, I guess "thinking about" is the wrong turn of phrase, since I haven't so much been thinking about as building one. I'll be "thinking about" public-key auth and [OpenId](http://wiki.openid.net/w/page/12995226/Run%20your%20own%20identity%20server) [next](http://en.wikipedia.org/wiki/OpenID), hopefully, but the first thing I want to put together is an old-style password-based authentication system.

Oh, yeah. And do it *properly*.

Which means no Dev 101-level mistakes like storing [plaintext passwords](http://plaintextoffenders.com/), or being subject to [injection attacks](http://codereview.stackexchange.com/questions/6632/what-is-the-security-issue-in-this-code/6633#6633), or putting up with [login hammering](http://amix.dk/blog/post/19425), or [leaving off the salt](http://blogs.cio.com/security/17143/15m-eharmony-passwords-stolen-along-linkedin-data). That's a slight increase in challenge from just "set up a user system".

The trivial `gen_server`-based user system looks something like

```erlang
-module(trivial_user).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(user,{timestamp, username, password}).

register(Username, Password) -> gen_server:call(?MODULE, {register, Username, NewPassword}).

auth(Username, Password) -> gen_server:call(?MODULE, {auth, Username, Password}).

change_password(Username, NewPassword) -> gen_server:call(?MODULE, {change_pass, Username, NewPassword}).

exists_p(Username) ->
    try
        find(Username)
    catch
        error:_ -> false
    end.

handle_call({register, Username, Password}, _From, State) ->
    Res = case exists_p(Username) of
              false -> User = #user{username=Username, password=Password, timestamp=now()},
                       transaction(fun() -> mnesia:write(User) end);
              _ -> already_exists
          end,
    {reply, Res, State};
handle_call({auth, Username, Password}, _From, State) ->
    try
        [User] = do(qlc:q([X || X <- mnesia:table(user),
                                X#user.username =:= Name,
                                X#user.password =:= Password])),
        {reply, User, State}
    catch
        error:_ -> {reply, false, State}
    end;
handle_call({change_pass, Username, NewPassword}, _From, State) ->
    Rec = find(Username),
    {reply, transaction(fun() -> mnesia:write(Rec#user{password=NewPassword}) end), State}.

%%%%%%%%%%%%%%%%%%%% database utility
find(Name) ->
    [Rec] = db:do(qlc:q([X || X <- mnesia:table(user), X#user.username =:= Name])),
    Rec.

do(Q) -> transaction(fun() -> qlc:e(Q) end).

transaction(F) ->
    {atomic, Val} = mnesia:transaction(F),
    Val.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, []}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> State ! {self(), close}, ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
```

```erlang
1> mnesia:create_schema([node()]).
ok
2> mnesia:start().
ok
3> rd(user,{username, password, timestamp}).
user
4> mnesia:create_table(user, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, user)}]).
{atomic,ok}
5> trivial_user:start().
{ok,<0.90.0>}
6> trivial_user:register("Inaimathi", "password").
ok
7> trivial_user:auth("Inaimathi", "password").
#user{username = "Inaimathi",password = "password",
      timestamp = {1339,96410,156774}}
```

In pseudocode it's

```python
def register(username, password):
    store(username, password, timestamp())

def auth(username, entered_password):
    if user = find(username) and user.password == entered_password:
        user
    else:
        false

def change_pass(username, new_password):
    store(find(username).password = new_password)
```

But that hits most of the rookie mistakes I listed above plus a few more. Incidentally, I *will* murder you if you use this in production and I find out about it. It doesn't hash or salt passwords, it doesn't rate-limit the `auth` message, it *does* get around injection attacks purely through the virtue of being implemented in [a symbolic db system](http://www.erlang.org/doc/man/mnesia.html), but that probably shouldn't count since it's a consequence of the tools rather than the system itself.

Lets work backwards through the pattern, and see how to arrive at a proper-ish user and authentication system. Firstly, it's important that a potential attacker can't just try 10000 passwords per second. Because if they can, and any of your users use common passwords, then it really doesn't matter how well you store them. You can do something naive, like introducing a return delay when an incorrect password is tried.

```erlang
...
handle_call({auth, Username, Password}, _From, State) ->
    try
        [User] = do(qlc:q([X || X <- mnesia:table(user),
                                X#user.username =:= Username,
                                X#user.password =:= Password])),
        {reply, User, State}
    catch
        error:_ -> timer:sleep(2000),
                   {reply, false, State}
    end;
...
```

But that blocks. In other words, whenever anyone enters their password incorrectly, *everyone* waits for two seconds to interact with the user process. Which, shall we say, doesn't scale. Granted, *not* doing it this way opens up the possibility that someone could just try 10000 *parallel* requests for a password, but that seems like a lesser evil than making it ridiculously easy to DOS the system.

There are two essential ways of "solving" this problem


- **The stateless** way would be to decouple authentication from other user actions. We wouldn't have a single authentication process, rather, when a call to `trivial_user:auth/2` happens, it should launch a temporary process that tries to authenticate that user. If the correct answer is given, there should be no delay, but there *should* be a small, non-global delay on a wrong guess.
- **The stateful** way would be to track how many wrong guesses have been made for a given user name/IP address. At a certain threshold (or perhaps linearly scaling with the number of wrong guesses), impose some sort of limiting factor. This can be as simple as a delay, or as complex as demanding a recaptcha on the front end.
- **The ideal** way would be to say [fuck passwords](http://me.veekun.com/blog/2011/12/04/fuck-passwords/), collect your users public keys instead, and authenticate them in an actually secure manner. Good luck brute-forcing a 4096 bit RSA key. Then have fun doing it again for every single user. Sadly, this doesn't count as a "solution" because most users are pretty sure they leave their public keys under their welcome mat each morning.


Given the language I'm working with, that first one looks like it'd fit better. In other words, we remove the `auth` handler from `trivial_user:handle_call/3`

```erlang
...
handle_call({register, Username, Password}, _From, State) ->
    User = #user{username=Username, password=Password, timestamp=now()},
    {reply, transaction(fun() -> mnesia:write(User) end), State};
handle_call({change_pass, Username, NewPassword}, _From, State) ->
    Rec = find(Username),
    {reply, transaction(fun() -> mnesia:write(Rec#user{password=NewPassword}) end), State}.
...
```

and have `trivial_user:auth/2` handle the password checking itself in a child process

```
auth(Username, Password) ->
    Pid = self(),
    Auth = fun() -> User = find(UserName),
                    true = Password =:= User#user.password,
                    Pid ! User
           end,
    AuthProc = spawn(Auth),
    receive
        Res -> exit(AuthProc, thank_you),
               Res
    after 2000 ->
            false
    end.
```

Do note the use of offensive programming in the `Auth` function. We don't do any kind of cleanup if the password is incorrect, just let `AuthProc` die a horrible, error-induced death and move on with our lives. We do stop waiting for it after two seconds, which is incidentally the delay we wanted to introduce for a wrong entry. Instead of being able to naively try 10000 passwords per second, our theoretical attackers can now try one every ~2, which should make this auth process a slightly harder target.

> EDIT:
> It's been pointed out to me that using SHA2 is a pretty bad approach here. I was initially going to tear this article apart for an edit (which is why it took so long), but ultimately decided to handle it in [an addendum](http://langnostic.blogspot.ca/2012/11/authentication-part-575.html). The below is here for historical interest only; kids, use specialized password-storing hash algorithms and stay in school.
>
> Fri, 16 Nov, 2012

Next up, we're still storing user passwords as plaintext, which is less than ideal. That means that anyone who succeeds in getting at our data somehow can suddenly impersonate anyone in the system flawlessly. That's why we have to hash them. Now, there are [hashing libraries](http://stackoverflow.com/questions/955161/sha256-encryption-in-erlang) in Erlang, including the built-in [crypto](http://www.erlang.org/doc/man/crypto.html) parts of which we'll be using, but.

1. [Hash functions](http://en.wikipedia.org/wiki/Hash_function) are tricky to pick, even before you get into [cryptographic hash functions](http://en.wikipedia.org/wiki/Cryptographic_hash_function#Cryptographic_hash_algorithms). In fact, there are a couple of widely-used ones[^such-as-md5-and-sha1] that have been subject to successful attacks. Given that, I'm leaning towards the [SHA-2 algorithms](http://en.wikipedia.org/wiki/SHA-2) which, as of this writing, have not been successfully broken. **DO NOT** read that as "I should use SHA256 from now on". Read it instead as "Before deciding on a hash function, I should check which ones are difficult to break at the time I'm making the decision". That complicates things somewhat, because Erlang's `crypto` only supports MD5 and SHA-1<a href="#foot-Sat-Jun-09-121448EDT-2012), installing the Erlang SHA256 library seems to be more than trivially difficult.

[^such-as-md5-and-sha1]: Such as MD5 and SHA1. Note that using these, for example, in the way that [git](http://git-scm.com/book/en/Git-Internals-Git-Objects) does isn't a huge deal, since that's merely supposed to be a consistency check and not a security feature.

2. Cryptographic functions are tricky to [implement](http://en.wikipedia.org/wiki/SHA-2#Examples_of_SHA-2_variants). By all means, try to as a [learning experience](http://www.youtube.com/watch?v=IzVCrSrZIX8&feature=bf_prev&list=PL9385DDE1A7699AB0&index=1), but there are [non](http://en.wikipedia.org/wiki/Side_channel_attack)-obvious [attacks](http://en.wikipedia.org/wiki/Category:Cryptographic_attacks) that you can leave your implementation open to, even if you do put everything together properly. The rule is "**do NOT** roll your own". By extension, "**do NOT** use a crypto library written by someone merely *as* smart as you", and "**do NOT** use a crypto library that hasn't been extensively battle tested". In fact, this is the one place where I'd say going with the herd is the right thing to do[^serious-caveat].

[^serious-caveat]: As long as the herd isn't demonstrably wrong, of course.

So, for those borderline-excuse reasons (and also because I want to show how to do it), we'll be using [Python's `hashlib`](http://docs.python.org/library/hashlib.html) with [`erlport`](http://erlport.org/). It sounds scary, but it *is* trivial. Once you install `erlport`[^available-through], you just kind of...

[^available-through]: Which is available through [`setuptools`](http://pypi.python.org/pypi/setuptools).


```erlang
-module(sha256).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([encode/1]).

encode(String) -> gen_server:call(?MODULE, {encode, String}).

handle_call({'EXIT', _Port, Reason}, _From, _State) ->
    exit({port_terminated, Reason});
handle_call(Message, _From, Port) ->
    port_command(Port, term_to_binary(Message)),
    receive
        {State, {data, Data}} ->
            {reply, binary_to_term(Data), State}
    after 6000 ->
            exit(timeout)
    end.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, open_port({spawn, "python -u sha256.py"}, [{packet, 4}, binary, use_stdio])}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> State ! {self(), close}, ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
```

```python
## sha256.py
from erlport import Port, Protocol, String
import hashlib

class Sha256Protocol(Protocol):
    def handle_encode(self, message):
        return hashlib.sha256(unicode(message)).hexdigest()

if __name__ == "__main__":
    Sha256Protocol().run(Port(packet=4, use_stdio=True))
```

Incidentally, you probably see why I decided to write myself a [quickie templating library](https://github.com/Inaimathi/emacs-utils/blob/master/erl-custom.el) for Erlang modules. Not to bust out the [SLW](http://c2.com/cgi/wiki?SmugLispWeenie) here, but in Lisp, I would handle the same problem with one `defmacro`, and thereafter be calling the resulting

    (define-gen-server handle-call &key
      (start (gen-server:start-link `(local ,*module*) *module nil nil))
      (stop ...)
      ...)

But hey, relying on your editor to do shit that should be handled in the language seems to be [serving ~67% of the programming world](http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html) just fine, so whatever the fuck.

Ahem.

What you see above is the trivial string hashing implementation. Writing it took me somewhat less effort than learning [how to use rebar](https://github.com/basho/rebar)[^from-the-future], and I now get to call `sha256:encode("Something something").` with reasonable confidence that a very large number of people smarter than me have failed to find errors in the code doing the work for me. Now that we've got that, we need to modify two things in the `trivial_user` module. First, we need to store the hashed password, both when registering and changing

[^from-the-future]: Hello from 2016. I still haven't learned how to use Rebar. The fragility and annoyance of packaging a system in Erlang has successfully kept me away from the language for something like four years at this point, and I honestly don't feel the need to go back. If I end up absolutely _needing_ to do some complicated setup trash work, I'll very probably throw the time at figuring out [`nix`](TODO) together with [Standard ML](http://sml-family.org/).

```erlang
...

handle_call({register, Username, Password}, _From, State) ->
    false = exists_p(Username),
    User = #user{username=Username, password=sha256:encode(Password), timestamp=now()},
    {reply, transaction(fun() -> mnesia:write(User) end), State};
handle_call({change_pass, Username, NewPassword}, _From, State) ->
    User = find(Username),
    {reply, transaction(fun() -> mnesia:write(User#user{password=sha256:encode(NewPassword)}) end), State}.

...

```

And second, when authenticating, we need to hash the input before comparing a password with what we've got stored

```erlang
...

auth(Username, Password) ->
    Pid = self(),
    Auth = fun() -> User = find(UserName),
                    true = sha256(Password) =:= User#user.password,
                    Pid ! User
           end,
    AuthProc = spawn(Auth),
    receive
        Res -> exit(AuthProc, thank_you),
               Res
    after 2000 ->
            false
    end.

...
```

There. Now, if some ne'er-do-well manages to get a hold of our password database somehow, he won't be looking at

```erlang
[{"John Douchebag",      "P@ssword123"},
 {"Jane Douchebag",      "P@ssword123"},
 {"Dave Foobar",         "P@ssword123"},
 {"Alex Nutsack",        "P@ssword231"},
 {"Brian Skidmore",      "P@ssword123"},
 {"Rose Cox",            "P@ssword123"},
 {"Barbara Lastname",    "P@ssword123"},
 {"Dora Smartass",       "correcthorsebatterystaple"}
 ...]
```

he'll instead be looking at

```erlang
[{"John Douchebag",      "62a39df87b501ad40b6fc145820756ccedcab952c64626968e83ccbae5beae63"},
 {"Jane Douchebag",      "62a39df87b501ad40b6fc145820756ccedcab952c64626968e83ccbae5beae63"},
 {"Dave Foobar",         "62a39df87b501ad40b6fc145820756ccedcab952c64626968e83ccbae5beae63"},
 {"Alex Nutsack",        "a52c4ef2c82e00025191375eadfea1e28b6389ab6091f1ab66e7549d1edef2f3"},
 {"Brian Skidmore",      "62a39df87b501ad40b6fc145820756ccedcab952c64626968e83ccbae5beae63"},
 {"Rose Cox",            "62a39df87b501ad40b6fc145820756ccedcab952c64626968e83ccbae5beae63"},
 {"Barbara Lastname",    "62a39df87b501ad40b6fc145820756ccedcab952c64626968e83ccbae5beae63"},
 {"Dora Smartass",       "cbe6beb26479b568e5f15b50217c6c83c0ee051dc4e522b9840d8e291d6aaf46"}
 ...]
```

And that should illustrate exactly why salt is an important thing to use. You'll notice that the same string always hashes to the same output. That's good, because that means we have a simple way to compare passwords later. But. If a lot of your users use the same password[^while-this-is-an-exaggeration], then someone who guesses what hash algorithm you're using can easily run a [rainbow table](http://en.wikipedia.org/wiki/Rainbow_table) against the hashes they found to guess large chunks of the plaintexts.

[^while-this-is-an-exaggeration]: And while this is an exaggerated example, you would be very [surprised at how many people pick worse](http://www.tomshardware.com/news/imperva-rockyou-most-common-passwords,9486.html) on a regular basis.

That is not good. And it's precisely the problem that salt is meant to solve. The important part of a salt is that [it's unique](http://security.stackexchange.com/questions/11221/how-big-salt-should-be)[^not-necessarily-secret]. Some people like it to be cryptographically secure, but I don't think it has to be. You're trying to avoid the situation where cracking one password gets your attacker access to more than one account. Do note that "unique" means "really, truly, globally unique". As in, don't just set a padded counter starting from 1, because different instances of your system will have some identical salt values. Also, obviously, don't just use a single salt value per server because that would defeat the purpose almost entirely. It needs to be different per secret, which means we need to change it out when a user changes their password too.

[^not-necessarily-secret]: By the way, salt does **not** have to be secret. You can keep it in the same table as your passwords, and you shouldn't be particularly worried if someone finds out which salt goes with which password. Well, no more worried than if they just got a hold of your hashed passwords.

Just to drive the point home, if you use a single salt-value per user, the hashes above will look like

```erlang
[{"John Douchebag",      "a26d44677573d3dfdfe116dc46979ce7ff00d9877a05d59158e74d2cf955400c"},
 {"Jane Douchebag",      "a26d44677573d3dfdfe116dc46979ce7ff00d9877a05d59158e74d2cf955400c"},
 {"Dave Foobar",         "a26d44677573d3dfdfe116dc46979ce7ff00d9877a05d59158e74d2cf955400c"},
 {"Alex Nutsack",        "0f751ddd05eb211a8300254701dce2ea045805e39113a821a10adf747243fc27"},
 {"Brian Skidmore",      "a26d44677573d3dfdfe116dc46979ce7ff00d9877a05d59158e74d2cf955400c"},
 {"Rose Cox",            "a26d44677573d3dfdfe116dc46979ce7ff00d9877a05d59158e74d2cf955400c"},
 {"Barbara Lastname",    "a26d44677573d3dfdfe116dc46979ce7ff00d9877a05d59158e74d2cf955400c"},
 {"Dora Smartass",       "fc5edff6668c8678f4c242cdea531cfd8883add17072e7ff1db76ea21952504b"}
 ...]
```

It means that it's slightly harder to crack one of your passwords[^how-much-harder], but *if* a password is cracked, your attacker still has the benefit of compromising the complete set of users that have that same password.

[^how-much-harder]: How much harder depends on what salt you use.

The absolute simplest, most brain-dead way to generate salt is to run an operation per password that looks something like

```erlang
make_salt() -> binary_to_list(crypto:rand_bytes(32)).
```

Tadaah![^particularly-obsessive]

[^particularly-obsessive]: If you're particularly obsessive, use `crypto:strong_rand_bytes/1` instead. The only difference is that the `strong_` variant gets some of its randomness from OS provided entropy, but it may also periodically hand you back a `low_entropy` error instead of a random byte string.

And that may actually be going overboard by about 16 bytes. Calling `make_salt/0` will return something like `[239,97,166,69,1,8,19,68,253,82,111,74,152,123,103,164,209,44,92,246,177,60,38,201,107,116,72,219,82,204,49]`, which we then concatenate with a password in order to make the world a slightly better place for people who use passwords like `P@ssword123`.

On reflection, this may not be a good thing, but it does make our user system one increment better. We now need to store salt for each user, and use it in our hashing step when comparing and storing passwords. So.

```erlang
salt(Salt, String) -> sha256:encode(lists:append(Salt, String)).

...
auth(Username, Password) ->
    Pid = self(),
    Auth = fun() -> User = find(Username),
                    true = salt(User#user.salt, Password) =:= User#user.password,
                    Pid ! User
           end,
    AuthProc = spawn(Auth),
    receive
        User -> exit(AuthProc, thank_you),
                {User#user.username, User#user.timestamp}
    after 2000 ->
            false
    end.
...

...
handle_call({register, Username, Password}, _From, State) ->
    false = exists_p(Username),
    Salt = make_salt(),
    User = #user{username=Username, password=salt(Salt, Password), salt=Salt, timestamp=now()},
    {reply, transaction(fun() -> mnesia:write(User) end), State};
handle_call({change_pass, Username, NewPassword}, _From, State) ->
    User = find(Username),
    Salt = make_salt(),
    {reply, transaction(fun() -> mnesia:write(User#user{password=salt(Salt, NewPassword), salt=Salt}) end), State}.
...
```

Now that we have effective, per-password salt going, that potentially leaked table looks a bit different.

```erlang
[{"John Douchebag",
  [218,207,128,49,205,116,234,236,67,27,74,144,22,45,219,251,
   58,82,240,14,233,252,56,105,28,112|...],
  <<"a0366db583c76fd81901e57f69b4f2f67b9ab779ae76e5ff3ce8c82fdc21b1ea">>},
 {"Jane Douchebag",
  [141,235,133,13,140,199,19,158,169,8,188,147,25,247,31,62,
   112,41,175,243,68,139,130,236,112|...],
  <<"b6dd5d87a80e166dea1b1959526f544b3d9da3818e178fe82e7571c30ea32077">>},
 {"Dave Foobar",
  [248,80,49,63,241,204,182,120,53,181,84,5,51,142,34,240,187,
   76,115,55,29,207,149,93|...],
  <<"6442397fd432fa1fa05d96e2db08c3ea4b840ecddf9b3bcf1f0904ec95a2e7cf">>},
 {"Alex Nutsack",
  [255,116,72,208,37,69,135,169,131,253,115,135,39,54,14,118,
   216,35,92,157,183,96,87|...],
  <<"6a966e1362d27851fac8e5ed44cff1eb7f3b15035d86e20438e228a2b8441a5e">>},
 {"Brian Skidmore",
  [149,22,172,0,14,45,14,228,19,66,214,170,87,238,39,126,65,
   229,118,44,49,18|...],
  <<"da65f803390a3886915c84adf444324c2d90396f6fcfc9a97900d14ed4ffc264">>},
 {"Rose Cox",
  [67,22,142,129,118,7,112,66,187,180,201,168,244,132,118,170,
   56,250,127,132,189|...],
  <<"67235dfae2f44bf68101b67773e2512193383a6d7e965cc423056ad750ab5806">>},
 {"Barbara Lastname",
  [214,17,61,189,60,148,2,168,65,140,87,224,216,40,14,132,129,
   145,238,153|...],
  <<"669b6876ad2cd40b857bd8b0ff67d49df2133498bb7b6a8fd8bbe764889f9c1b">>},
 {"Dora Smartass",
  [191,211,52,128,89,167,168,177,221,238,21,94,121,15,20,22,
   144,11,235|...],
  <<"bdec4a9e62d5f03651720903d2001d82a3167aefd43bc22741c482b98f83ad43">>}
 ...]
```

Even if the attacker gets each user's salt as in the above example, check out the password hashes.

```
"a0366db583c76fd81901e57f69b4f2f67b9ab779ae76e5ff3ce8c82fdc21b1ea",
"b6dd5d87a80e166dea1b1959526f544b3d9da3818e178fe82e7571c30ea32077",
"6442397fd432fa1fa05d96e2db08c3ea4b840ecddf9b3bcf1f0904ec95a2e7cf",
"6a966e1362d27851fac8e5ed44cff1eb7f3b15035d86e20438e228a2b8441a5e",
"da65f803390a3886915c84adf444324c2d90396f6fcfc9a97900d14ed4ffc264",
"67235dfae2f44bf68101b67773e2512193383a6d7e965cc423056ad750ab5806",
"669b6876ad2cd40b857bd8b0ff67d49df2133498bb7b6a8fd8bbe764889f9c1b",
"bdec4a9e62d5f03651720903d2001d82a3167aefd43bc22741c482b98f83ad43"
```

The important part here is that even though 6 of those 8 users use the same passwords, there's no way to find that out based on just the hashes. Meaning that the theoretical attacker here would actually have to crack the password of *every* account they want access to. Granted, it's still easier to guess a password like "P@ssword123" than a passphrase [generated in the correct horse](http://preshing.com/20110811/xkcd-password-generator) style, but our system is still more secure for having these small steps.

Just to bring it all together, the final code for a proper, salted, hashing user/password system is

```erlang
-module(trivial_user).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(user,{username, password, salt, timestamp}).
-export([register/2, auth/2, change_password/2, list/0]).

list() -> gen_server:call(?MODULE, list).

register(Username, Password) -> gen_server:call(?MODULE, {register, Username, Password}).

auth(Username, Password) ->
    Pid = self(),
    Auth = fun() -> User = find(Username),
                    true = salt(User#user.salt, Password) =:= User#user.password,
                    Pid ! User
           end,
    AuthProc = spawn(Auth),
    receive
        User -> exit(AuthProc, thank_you),
                {User#user.username, User#user.timestamp}
    after 2000 ->
            false
    end.

change_password(Username, NewPassword) -> gen_server:call(?MODULE, {change_pass, Username, NewPassword}).

handle_call(list, _From, State) ->
    {reply, do(qlc:q([{X#user.username, X#user.timestamp} || X <- mnesia:table(user)])), State};
handle_call({register, Username, Password}, _From, State) ->
    Res = case exists_p(Username) of
              false -> Salt = make_salt(),
                       User = #user{username=Username, password=salt(Salt, Password), salt=Salt, timestamp=now()},
                       transaction(fun() -> mnesia:write(User) end);
              _ -> already_exists
          end,
    {reply, Res, State}
handle_call({change_pass, Username, NewPassword}, _From, State) ->
    User = find(Username),
    Salt = make_salt(),
    {reply, transaction(fun() -> mnesia:write(User#user{password=salt(Salt, NewPassword), salt=Salt}) end), State}.

%%%%%%%%%%%%%%%%%%%% database utility
make_salt() -> binary_to_list(crypto:rand_bytes(32)).

salt(Salt, String) -> sha256:encode(lists:append(Salt, String)).

exists_p(Username) ->
    try
        find(Username)
    catch
        error:_ -> false
    end.

find(Name) ->
    [Rec] = do(qlc:q([X || X <- mnesia:table(user), X#user.username =:= Name])),
    Rec.

do(Q) -> transaction(fun() -> qlc:e(Q) end).

transaction(F) ->
    {atomic, Val} = mnesia:transaction(F),
    Val.

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

The pseudocode differences are minute, to be sure,

```python
def register(username, password):
    store(username, salt(s, password), timestamp(), s)

def auth(username, entered_password):
    spawn:
        if user = find(username) and user.password == salt(user.s, entered_password):
            user.except(password, salt)
        else:
            wait(2, :seconds)
            false

def change_pass(username, new_password):
    store(find(username).password = salt(s, new_password), s)

def salt(s, string):
    secure_hash(s + string)
```

but they make for a more robust password-based system. Granted, that's still like being *really, really good* at [arguing on the internet](http://www.flickr.com/photos/villagemember/987251565/), but baby steps.

Github [here](#), if you want to play around with it.

> EDIT:
> The link above no longer exists. All features from this library have been folded into [auth](https://github.com/Inaimathi/auth) (there have been changes since this post was written, so it's not exactly the same codebase, but the principles are the same)
> Thu, 30 Aug, 2012

For next time, I'll be putting together an extension to this that does public-key-based auth, (as well as passwords for the normies).
