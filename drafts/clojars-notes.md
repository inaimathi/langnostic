One of the reasons Clojure is in serious contention for my favorite language[^i-mean-aside] is that [Clojars](https://clojars.org/) is a thing.

[^i-mean-aside]: I mean, aside from [those](/posts/recommendations) I've [mentioned](/posts/notes-on-clojure) already in [previous posts](/posts/even-more-notes-on-clojure).

And in particular, that it can be published to from your local environment using `lein deploy clojars`. After that, you can easily pull separate libraries in for your other projects. There's a debate we can have regarding whether you really need a publishing step, rather than taking the [`quicklisp`](https://www.quicklisp.org/beta/) approach of a local projects directory, but I think from the Open Source perspective, it's an unambiguous win to just be able to push projects up in an automated fashion without going through the [CL submission processs](https://github.com/quicklisp/quicklisp-projects/issues).

There's one hiccup I've consistently hit with this, and it has to do with setting up `gpg` signing for libraries whose versions don't end with `-SNAPSHOT`. In an effort to remember for next time, here's the step-by-step process to deploying your libraries to Clojars:

1. **Install Stuff**. In particular, you'll need [`gpg`](https://gnupg.org/) and `gpg-agent`. These are in the `debian` as `gnupg` and `gnupg-agent` respectively, but do note that you'll need the versions out of [`buster`](https://www.debian.org/releases/buster/) or later at the moment. There's also a `gnupg` package in `nix`, but there doesn't seem to be a corresponding `gnupg-agent`

2. **Generate a GPG Key**. This is the usual process that starts with `gpg --gen-key` and ends with you having a keypair. The process is well guided, so I won't say more about it here.

3. **Register at Clojars**. You need to get a user account over at [Clojars](https://clojars.org/) in order to actually deploy things.

4. **Set up credentials**. Next up, you need to set up a file at `~/.lein/credentials.clj` containing `{#"https://clojars.org/repo" {:username "<your user name>" :password "<your password>"}}`. Then, you need to run `gpg --default-recipient-self -e ~/.lein/credentials.clj > ~/.lein/credentials.clj.gpg` to generate its encrypted equivalent (at that point, you should also `rm ~/.lein/credentials.clj` so you don't need to worry about the cleartext falling into the wrong hands).

5. **Start up `gpg-agent` and cache credentials**. This is the odd one. See, `gpg-agent` is kind of like `ssh-agent` in that it caches passphrases for credential keys for a while. However, `lein deploy clojars` currently _doesn't_ allow the password prompt to come through when you run it. This means that if your key passphrase hasn't been recently cached, or if `gpg-agent` isn't running yet, you'll get a bizarre error that says something about not being able to access `openpgp`. What you need to do is run `gpg --use-agent --quiet --batch --decrypt ~/.lein/credentials.clj.gpg`. This should prompt for your private key passphrase, decrypt your `credentials.cljs.gpg` to standard output, and incidentally cache said passphrase.

At that point, you should be able to `lein deploy clojars`.
