> Authentication  
> Authentication  
> Authentication!  
> 
> Authentication  
> Authentication  
> Authentication  
> Authentication  
> 
> Authentication  
> Authentication  
> Authentication  
> 
> Authentication  
> Authentication  
> Authentication  
> 
> Authentication  
> Authentication  
> Authentication  
> Authenticatiooooooooooon!  

![](/static/img/imaginationland-guy.jpg)

Gentlemen...

### <A NAME="BEHOLD"></A>BEHOLD!

```erlang
-module(rsa_auth).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([gen_secret/2, verify/3, new_key/2]).
-export([create/0, clear/0, recreate/0, find/1, now_to_seconds/1]).

-record(pubkey, {user_id, pubkey}).
-record(secret, {timestamp, user_id, ip, plaintext}).

%%% API
new_key(UserId, Pubkey) -&gt; gen_server:call(?MODULE, {new_key, UserId, Pubkey}).
gen_secret(UserId, IP) -&gt; gen_server:call(?MODULE, {gen_secret, UserId, IP}).
verify(UserId, IP, Sig) -&gt; gen_server:call(?MODULE, {verify, UserId, IP, Sig}).

handle_call({gen_secret, UserId, IP}, _From, State) -&gt; 
    Pubkey = find({key, UserId}),
    P = binary_to_hex(crypto:sha(crypto:rand_bytes(32))),
    Ciphertext = binary_to_list(m2crypto:encrypt(Pubkey, P)),
    Secret = #secret{timestamp=now(), user_id=UserId, ip=IP, plaintext=P},
    db:transaction(fun() -&gt; mnesia:write(Secret) end),
    {reply, Ciphertext, State};
handle_call({verify, UserId, IP, Sig}, _From, State) -&gt;
    Pubkey = find({key, UserId}),
    Secrets = find({secrets, UserId, IP}),
    Res = lists:any(
            fun({T, S}) -&gt; verify_key({T, S}, Pubkey, Sig) end, 
            Secrets),
    {reply, Res, State};
handle_call({new_key, UserId, Pubkey}, _From, State) -&gt; 
    Res = case exists_p(UserId) of
              false -&gt; Fname = make_tempname("/tmp"),
                       file:write_file(Fname, Pubkey),
                       K = m2crypto:split_key(Fname),
                       Rec = #pubkey{user_id=UserId, pubkey=K},
                       ok = db:transaction(fun() -&gt; mnesia:write(Rec) end),
                       file:delete_file(Fname),
                       K;
              true -&gt; already_exists
          end,
    {reply, Res, State}.

%%% rsa_auth-specific utility
verify_key({T, S}, Pubkey, Sig) -&gt;
    case old_secret_p(T) of
        true -&gt; revoke_secret(T),
                false;
        _ -&gt; case m2crypto:verify(Pubkey, S, Sig) of
                 true -&gt; revoke_secret(T),
                         true;
                 _ -&gt; false
             end
    end.

revoke_secret(T) -&gt;
    db:transaction(fun() -&gt; mnesia:delete({secret, T}) end).

old_secret_p(T) -&gt; 
    %% it's old if the timestamp is older than 5 minutes
    300 &lt; (now_to_seconds(now()) - now_to_seconds(T)).

exists_p(UserId) -&gt; 
    try
        find({key, UserId})
    catch
        error:_ -&gt; false
    end.

%%% DB related
find({key, UserId}) -&gt; 
    [Rec] = db:do(qlc:q([X#pubkey.pubkey || X &lt;- mnesia:table(pubkey), X#pubkey.user_id =:= UserId])),
    Rec;
find({secrets, UserId, IP}) -&gt; 
    db:do(qlc:q([{X#secret.timestamp, X#secret.plaintext} || 
                    X &lt;- mnesia:table(secret), 
                    X#secret.user_id =:= UserId,
                    X#secret.ip =:= IP])).

create() -&gt;
    mnesia:create_table(pubkey, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, pubkey)}]),
    mnesia:create_table(secret, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, secret)}]).

clear() -&gt;
    mnesia:delete_table(pubkey),
    mnesia:delete_table(secret).

recreate() -&gt;
    clear(),
    create().

%%% general utility
now_to_seconds(Now) -&gt;
    calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Now)).

make_tempname() -&gt;
    {A, B, C} = now(),
    [D, E, F] = lists:map(fun integer_to_list/1, [A, B, C]),
    lists:append(["tmp.", D, ".", E, ".", F]).
make_tempname(TargetDir) -&gt;
    filename:absname_join(TargetDir, make_tempname()).

binary_to_hex(Bin) -&gt;
    lists:flatten([io_lib:format("~2.16.0B", [X]) ||
                      X &lt;- binary_to_list(Bin)]).

%%%%%%%%%%%%%%%%%%%% generic actions
start() -&gt; gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -&gt; gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -&gt; {ok, []}.
handle_cast(_Msg, State) -&gt; {noreply, State}.
handle_info(_Info, State) -&gt; {noreply, State}.
terminate(_Reason, _State) -&gt; ok.
code_change(_OldVsn, State, _Extra) -&gt; {ok, State}.
```

Actually, that's way too intimidating, given what this thing does. Lets break that shit down, and strip the `gen_server`/`mnesia`-related boilerplate. Chunklet the first is the meatiest:

```erlang
%%% API
new_key(UserId, Pubkey) -&gt; gen_server:call(?MODULE, {new_key, UserId, Pubkey}).
gen_secret(UserId, IP) -&gt; gen_server:call(?MODULE, {gen_secret, UserId, IP}).
verify(UserId, IP, Sig) -&gt; gen_server:call(?MODULE, {verify, UserId, IP, Sig}).

handle_call({gen_secret, UserId, IP}, _From, State) -&gt; 
    Pubkey = find({key, UserId}),
    P = binary_to_hex(crypto:sha(crypto:rand_bytes(32))),
    Ciphertext = binary_to_list(m2crypto:encrypt(Pubkey, P)),
    Secret = #secret{timestamp=now(), user_id=UserId, ip=IP, plaintext=P},
    db:transaction(fun() -&gt; mnesia:write(Secret) end),
    {reply, Ciphertext, State};
handle_call({verify, UserId, IP, Sig}, _From, State) -&gt;
    Pubkey = find({key, UserId}),
    Secrets = find({secrets, UserId, IP}),
    Res = lists:any(
            fun({T, S}) -&gt; verify_key({T, S}, Pubkey, Sig) end, 
            Secrets),
    {reply, Res, State};
handle_call({new_key, UserId, Pubkey}, _From, State) -&gt; 
    Res = case exists_p(UserId) of
              false -&gt; Fname = make_tempname("/tmp"),
                       file:write_file(Fname, Pubkey),
                       K = m2crypto:split_key(Fname),
                       Rec = #pubkey{user_id=UserId, pubkey=K},
                       ok = db:transaction(fun() -&gt; mnesia:write(Rec) end),
                       file:delete_file(Fname),
                       K;
              true -&gt; already_exists
          end,
    {reply, Res, State}.
```

That's essentially the entire external API for this style of authentication<a name="note-Sat-Jun-23-012959EDT-2012"></a>[|1|](#foot-Sat-Jun-23-012959EDT-2012).

The exported functions are self-explanatory, so lets focus in on the `handle_call/3` clauses. I mentioned <a name="note-Sat-Jun-23-013539EDT-2012"></a>[|2|](http://langnostic.blogspot.ca/2012/06/authentication-part-three-rsa-basics.html">last week</a> that Erlang's own `crypto` functions don't provide a way to generate keys, and were having trouble importing any RSA 4096 keypairs I tried to work with, pretty much regardless of source. So I decided to call out to python for the actual encryption (more on that later). `gen_secret` needs to be accompanied by a `UserId`[](#foot-Sat-Jun-23-013539EDT-2012) and an `IP`<a name="note-Sat-Jun-23-013544EDT-2012)[|3|](#foot-Sat-Jun-23-013544EDT-2012). The output is a random string, encrypted with the key of the given user, and associated with the given IP (if we wanted bi-directional authentication, we'd also have the server sign it and send the signature along).

`verify`ing a signature requires the same two pieces of information, as well as the `Sig`nature. We select the set of secrets on file for the given user coming from the given IP, select the appropriate key, and then try to verify against each available secret. Verification happens in python too. In fact, lets take a quick look at that Erlang-side verification steps before we move on to handling the `new_key` message.

```erlang
%%% rsa_auth-specific utility
verify_key({T, S}, Pubkey, Sig) -&gt;
    case old_secret_p(T) of
        true -&gt; revoke_secret(T),
                false;
        _ -&gt; case m2crypto:verify(Pubkey, S, Sig) of
                 true -&gt; revoke_secret(T),
                         true;
                 _ -&gt; false
             end
    end.

revoke_secret(T) -&gt;
    db:transaction(fun() -&gt; mnesia:delete({secret, T}) end).

old_secret_p(T) -&gt; 
    %% it's old if the timestamp is older than 5 minutes
    300 &lt; (now_to_seconds(now()) - now_to_seconds(T)).
```

That seems reasonably self-explanatory too<a name="note-Sat-Jun-23-013559EDT-2012"></a>[|4|](#foot-Sat-Jun-23-013559EDT-2012). We check whether a given secret is too old, revoking it without granting access if it is, then calling out to python for the actual verification step (coming soon, I promise). If it succeeds, we revoke it and grant access. Note that by the time we've gotten to this point, the keys have already been verified for a matching IP. Right, back to the last clause in `handle_call/3`

```erlang
handle_call({new_key, UserId, Pubkey}, _From, State) -&gt; 
    Res = case exists_p(UserId) of
              false -&gt; Fname = make_tempname("/tmp"),
                       file:write_file(Fname, Pubkey),
                       K = m2crypto:split_key(Fname),
                       Rec = #pubkey{user_id=UserId, pubkey=K},
                       ok = db:transaction(fun() -&gt; mnesia:write(Rec) end),
                       file:delete_file(Fname),
                       K;
              true -&gt; already_exists
          end,
    {reply, Res, State}.
```

The process for storing a new key might take some explaining. We're expecting the *contents* of a PEM key file, rather than a file name because doing otherwise would force us to put this module on the same machine as the caller<a name="note-Sat-Jun-23-013621EDT-2012"></a>[|5|](#foot-Sat-Jun-23-013621EDT-2012). However, that raises a bit of a problem; M2Crypto can't import a pubkey from a string. It *can* do so for a keypair, but not if you don't have the private key on hand, which we won't. Ever. So what we need to do is create a temporary file somewhere, write the key out to it, then point M2Crypto at that to get the components back in a more digestible format. After that, it's just a matter of storing the key and cleaning up.

Ok, it's Python time

```erlang
-module(m2crypto).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([encrypt/2, verify/3, split_key/1]).

encrypt({E, N}, Message) -&gt; gen_server:call(?MODULE, {encrypt, E, N, Message}).
verify({E, N}, Message, Signature) -&gt; gen_server:call(?MODULE, {verify, E, N, Message, Signature}).
split_key(Filename) -&gt; gen_server:call(?MODULE, {split_key, Filename}).

handle_call({'EXIT', _Port, Reason}, _From, _State) -&gt;
    exit({port_terminated, Reason});
handle_call(Message, _From, Port) -&gt;
    port_command(Port, term_to_binary(Message)),
    receive
        {State, {data, Data}} -&gt; 
            {reply, binary_to_term(Data), State}
    after 3000 -&gt; 
            exit(timeout)
    end.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -&gt; gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -&gt; gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -&gt; {ok, open_port({spawn, "python -u m2crypto.py"}, [{packet, 4}, binary, use_stdio])}.
handle_cast(_Msg, State) -&gt; {noreply, State}.
handle_info(_Info, State) -&gt; {noreply, State}.
terminate(_Reason, State) -&gt; State ! {self(), close}, ok.
code_change(_OldVsn, State, _Extra) -&gt; {ok, State}.
```

```erlang
from erlport import Port, Protocol, String
import M2Crypto
        
class M2cryptoProtocol(Protocol):
    def handle_split_key(self, filename):
        pubkey = M2Crypto.RSA.load_pub_key(String(filename))
        return (pubkey.e, pubkey.n)
    def handle_encrypt(self, e, n, message):
        pubkey = M2Crypto.RSA.new_pub_key((str(e), str(n)))
        ciphertext = pubkey.public_encrypt(String(message), M2Crypto.RSA.pkcs1_oaep_padding)
        return ciphertext.encode('base64')
    def handle_verify(self, e, n, message, sig):
        pubkey = M2Crypto.RSA.new_pub_key((str(e), str(n)))
        if pubkey.verify_rsassa_pss(String(message), String(sig).decode('base64')):
            return True
        else:
            return False

if __name__ == "__main__":
    M2cryptoProtocol().run(Port(packet=4, use_stdio=True))
```

Wordy crap on the Erlang side aside, this is *reasonably* simple. `split_key` breaks a Public Key PEM into its `N` and `exponent`. The reason I bother doing that is that, as I mentioned, while M2Crypto *can't* import a pubkey from a PEM, it *can* stitch one back together from its components. In other words, we're only storing keys as `{E, N}` tuples for the sake of letting M2Crypto do the work without intermediate files. `encrypt` and `verify` should be entirely self-explanatory.

And, that's that. Well, ok, on the backend anyway. Which means we've got two more pieces to go through. Here's what the user-facing [Nitrogen](http://nitrogenproject.com/) module looks like

```erlang
-module (rsa_auth).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -&gt; #template { file="./site/templates/bare.html" }.

title() -&gt; "Manual RSA Auth".

body() -&gt; 
    [
     #label {text="Username: "},
     #textbox { id=username, next=sendButton },
     #button { id=sendButton, text="Request Secret", postback=send_user },

     #panel { id=auth_token }
    ].

event(send_user) -&gt;
    Token = rpc:call('trivial_user@127.0.1.1', rsa_auth, gen_secret, [wf:q(username), wf:peer_ip()]),
    wf:update(auth_token,
             [
              #panel { body=[ #span{text=Token} ]},
              #textarea { id=auth_response },
              #button { id=send_signed, text="Send Signed", postback=send_signed }
             ]);
event(send_signed) -&gt;
    Args = [wf:q(username), wf:peer_ip(), 
            re:replace(wf:q(auth_response), "\\\\n", "\n", [global, {return, list}])],
    Res = rpc:call('trivial_user@127.0.1.1', rsa_auth, verify, Args),
    erlang:display(Res),
    erlang:display(Args),
    case Res of
        true -&gt; wf:update(auth_token, [ #span { text="Yay! You're in!"} ]);
        _ -&gt; wf:update(auth_token, [ #span {text="Halt, criminal scum!" } ])
    end;
event(_) -&gt; ok.
```

That should be puzzle-out-able based on what we've been talking about too. Note that this expects to find a running instance of `trivial_user` at `'trivial_user@127.0.1.1'`. The only other thing I'll note is the bit that goes

```erlang
re:replace(wf:q(auth_response), "\\\\n", "\n", [global, {return, list}])
```

That's necessary because of the way the string "\n" reacts to being dumped into a textarea. if you don't do that, shit will go oddly wrong and you won't be able to figure it out until it's late enough that I'm literally being kept awake by caffeine and [strangely hypnotic music](http://www.youtube.com/watch?v=2R677MV--WI).

And that's bad.

The last remaining piece of this little system is the signing component, and here it is.

```python
#!/usr/bin/python

import M2Crypto, hashlib

def gimme_pw(*args):
    return "your passphrase goes here if you trust your computer"
    ### Ideally, you'd [daemonize](http://code.activestate.com/recipes/278731-creating-a-daemon-the-python-way/) (not as scary as it looks) this script, have it prompt for a password and cache it

def sign(message, Privkey=M2Crypto.RSA.load_key("/path/to/your/rsa.pem", gimme_pw)):
    plaintext = Privkey.private_decrypt(message.decode('base64'), M2Crypto.RSA.pkcs1_oaep_padding)
    sig = Privkey.sign_rsassa_pss(plaintext)
    return sig.encode('base64')
```

Whew!

That's the code down. The interaction, once you've registered and if you're going to be doing this manually, is


1.   Input your name and request a secret
1.   Copy the block of text the server sends you, and run the above signing script on it
1.   Copy the result into the newly formed `textarea` and click `"Send Signed"`


Assuming it was done correctly, you should then be logged in. The automatic version is going to have to wait for some sleep.

### <a name="how-is-this-better-than-passwords"></a>How Is This Better Than Passwords?

I don't fucking know, something. Oh, wait, yeah it is. In three specific ways.


-   **Bi directional authentication**; If we implement that note I mentioned earlier, it lets you authenticate to your server and authenticate the server to you, without an intermediary<a name="note-Sat-Jun-23-013835EDT-2012"></a>[|6|](#foot-Sat-Jun-23-013835EDT-2012)
-   **No critical information is exchanged**; even if someone is watching your entire transaction, they never get enough information to impersonate you, whether you're dealing with SSL or not.
-   **No critical information is present on the server**; even if your service provider is an utter dumbass that keeps their user database in plaintext with a little note taped to it reading "Plz dont steals", you don't care. Unlike a password system, where your password is effectively as secure as the weakest service you use it on, your RSA key is as secure as your personal machine. Granted, that may still not be *very* secure, but it's a step up.


I'm also convinced that once this is properly automated, it will be *easier* to deal with than password authentication from the user perspective, but I haven't built it yet, so I won't count that. I'm basing this conviction on the fact that I've stopped [using SSH with](http://paulkeck.com/ssh/)out [RSA keys](http://lani78.wordpress.com/2008/08/08/generate-a-ssh-key-and-disable-password-authentication-on-ubuntu-server/). I encourage you to try it if you haven't already.

Ok, that's it. Automated version coming soon, and good night.

* * *
##### Footnotes

1 - <a name="foot-Sat-Jun-23-012959EDT-2012"></a>[|back|](#note-Sat-Jun-23-012959EDT-2012) -  Ok, that's not true; we're missing two pieces, both critical in practice but borderline irrelevant for the theory.

The first one is **bi-directional authentication**. That would be pretty simple to implement from our perspective; all we'd need to do is sign the secret as it's being sent out. Doing so would let our user verify that they're talking to the server they expect rather than an eavesdropper or phisher. This overlaps slightly with SSL, but doesn't prevent a site from using both, and is so straightforward if you're already using this model that you may as well.

The second one is a way to **revoke keys**. That's more or less an open problem. For the purposes of this project, anyway. We could do something like hand our users a revocation phrase, or we could ask them to generate a second keypair which would be used to send a revocation message, or we could handle this through a second channel (which we should probably implement in any case, if we're serious about security). That second method sounds more secure, but really just recurses on the problem; what happens if your revocation key gets compromised? And how do you expect a user to store them?

Assigning a pass-phrase might seem like it's defeating the purpose, but remember that this one only comes out when you need to change keys (rather than at every login), and that lets us get a bit fancier with the sort of infrastructure we want to provide for it. For instance, I could imagine a provider mailing out actual plastic cards that people could stash in their wallets.

The third option is a lot more interesting, but I intend to write a piece on that by itself, so I won't much more time on it today. Sufficed to say that redundancy and isolation are key to build reliable systems, as Erlang has clearly demonstrated. And if you want a reliable channel for authentication, you really need to make it multiple independent channels. Slightly more annoying for your users, but exponentially more annoying for anyone trying to impersonate them.

Anyway, that's all beyond the scope of this piece, so I'm going to tactfully ignore it for the rest of the night.

2 - <a name="foot-Sat-Jun-23-013539EDT-2012"></a>[|back|](#note-Sat-Jun-23-013539EDT-2012) - So that we know whose key to encrypt the secret with.
3 - <a name="foot-Sat-Jun-23-013544EDT-2012"></a>[|back|](#note-Sat-Jun-23-013544EDT-2012) - Just as a security precaution against some types of sneakiness.

4 - <a name="foot-Sat-Jun-23-013559EDT-2012"></a>[|back|](#note-Sat-Jun-23-013559EDT-2012) - Except that Erlang doesn't like the idea of durations for some reason, so I had to bring myself to write a comment.

5 - <a name="foot-Sat-Jun-23-013621EDT-2012"></a>[|back|](#note-Sat-Jun-23-013621EDT-2012) - Which we wouldn't *necessarily* want to do, even if it didn't go against Erlang's grain.

6 - <a name="foot-Sat-Jun-23-013835EDT-2012"></a>[|back|](#note-Sat-Jun-23-013835EDT-2012) - I'll save the rant about why having centralized signing authorities is stupid for when my eyelids aren't trying to sabotage me.
