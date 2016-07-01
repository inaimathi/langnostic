So here's how you can pass messages to each other without your parents[^or-whoever] reading them.

[^or-whoever]: Or teachers, or political enemies, or competitors, what-have-you.

## Step 1: Get Some Friends and Computers together

I can't help you with this. You need some computers[^if-you-dont] and you need some friends to talk to, otherwise why would you be passing messages?

[^if-you-dont]: If you don't have access to your own computer, you can use a usb to run [Ubuntu live](https://wiki.ubuntu.com/LiveUsbPendrivePersistent), and merely plug it into whatever computer you do actually have access to.

## Step 2: Get GnuPG

This should be pretty simple. If you're on Debian/Ubuntu type

```shell
apt-get install gnupg
```

If you're on Windows, you'll need to install [Cygwin](http://cygwin.com/install.html) with the `gnupg` package or [`gpg4win`](http://gpg4win.org/), if you're on OS X, you'll need to get [GPGtools](http://www.gpgtools.org/).

## Step 3: Make some keys

Each of you should create a set of keys. Do that by typing

```shell
gpg --gen-key
```

GnuPG will then give you a menu that looks like this:

```shell
gpg (GnuPG) 1.4.10; Copyright (C) 2008 Free Software Foundation, Inc.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Please select what kind of key you want:
   (1) RSA and RSA (default)
   (2) DSA and Elgamal
   (3) DSA (sign only)
   (4) RSA (sign only)
```

Accept the default by hitting `enter`[^if-trustworthy]. It will then ask you

[^if-trustworthy]: If all of your friends can be trusted to keep secrets, you might opt to go with a [symmetric cypher](http://www.gnupg.org/gph/en/manual.html#AEN185) instead. The process of sending and receiving messages is more or less the same, but there's only one key that the entire group shares rather than there being two keys per person (a public and a private). The advantage is that there's less to keep track of. The downside is that if anyone finds out your key, *all* your notes can be cracked rather than just those sent to the person who let their key get compromised.

```shell
RSA keys may be between 1024 and 4096 bits long.
What keysize do you want? (2048)
```

Don't accept the default here, take the longest possible by typing `4096` and hitting `enter`. Next, `gpg` will ask when your key should expire. You can either specify days, weeks, months, years or accept the default (never expire). It's a balancing act; the more often you get new keys the more secure they will be against brute-forcing. On the other hand, each time you change your key, you need to send the new one to each of your friends and they have to remember to use the new one for passing you notes. For now, just take the default (`gpg` will ask for confirmation, so type `y` and hit `enter`). Next, fill in your name, email and comment and confirm them by typing `o` and then `enter`.

Next, enter a pass phrase. It can have spaces and it can be fairly long. Pick something easy to remember; a favorite quote or maybe a few lines from a song you like.

This next part may seem a bit weird, but `gpg` actually needs you to do some other stuff on the computer. Anything you like, but actually do things. Compose an email, check your favorite news aggregator, do some drawing, hit your head on the keyboard, kick the mouse around for a while, whatever you need to do. It may occasionally ask you to keep at it because you're not a fast enough typist with messages like

```shell
Not enough random bytes available.  Please do some other work to give
the OS a chance to collect more entropy! (Need 148 more bytes)
..............+++++
```

Eventually, it'll give you back control of your terminal, and at that point your key has been created. The hard part is done.

## Step 4: Share Your Keys

Ok, get those friends that I told you to have ready.

Each of you should run the command

```shell
gpg --armor --output your-name.txt --export your-name
```

(`your-name` should be the name you typed in as part of your information in **Step 3**). `your-name.txt` is a text file that should look something like

```shell
-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1.4.10 (GNU/Linux)

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
PzbGgzGART8EBfSEbML6gOgvyXC3
=n+lE
-----END PGP PUBLIC KEY BLOCK-----

```

It won't look exactly the same, yours will be different, but it will be about that wide and surrounded by the `--- PUBLIC KEY BLOCK ---` tags. That's your public key. Give it to your friends and get theirs. It doesn't matter how; either bust out the USB sticks or hop on a network and use `scp` or just paste that text to your Linked-Book-Space page or personal site. Each of you should get the key text files and run

```shell
gpg --import your-friends-name.txt
```

for every key you have. Once that's done, check that you have all your friends' keys imported by typing `gpg --list-keys` and hitting `enter`.

## Step 5: Send A Message

Type up your message in a text file (we'll call it `message.txt`, but it can be any type of file) and then run

```shell
gpg --output message.gpg --encrypt --armor --recipient your-friend --recipient your-other-friend --recipient and-so-on message.txt
```

Here's what a text file containing "something something dark side" looks like after it has been encrypted.

```shell
-----BEGIN PGP MESSAGE-----
Version: GnuPG v1.4.10 (GNU/Linux)

hQIMAw1KmAJVwvKhAQ//ZZf/xMd9jSW0XDX1s5uVQB39XUQq/OGMBFZN1Dk1WMbA
zZFShccV44X8NSAJu0h5S1PCdPkgQAzjecoScya9IVnSQVeyGgDP5dv7ZU4KqQQS
Pacd5J0u0BbLrHaIxSfCwchLofwGxlt52m9lAMAzlTnC4hEGxkwqo1QlM3PZZrKY
5P8abtVIv2VZXDTaDBgm1Y9KuFRrGhJtvHG8GHsg5zLuO6+gTZG6TC3F/BM5BUZZ
m0KBc8ZbztVmYbI6w6qm24M2MHExQd5tHFCx1VS51kOJ6gRg25XOFHHi8E/+F92A
bFUSh+/ZAvuPTBkjIATNI5rri7xHb+0BU1GHVSDAV0DUkI0GDMy05Hddrt/G7ORi
dsT+7BK/WipFAWpERevW65ZjaiV+sQHfj/ErjcnYV1hjRWqKJdOp4jdHROQO8ZUD
HuB6iw93i0Zo4UUb71GzptEigUriBCoRbb0q5T2cXsen8YKVXdaDr9+MTiQd8lqr
vaO4Wc/BiEkVecI9X0N63aGEoVRLLNsmBlnJGxTmOD1+WfwI1e4xlA6F5dR80nyr
T2ZuFuvhOxtSUIvWw3nb++YT0Z6hzTgD6JzffDySdBXTSozDYoFtRV34+VhD3CS0
LCVCF3ZVU+4c/Yiv72BVB1V+bJXN9BR6rmAFDTNtjnlWdubdwf8eFTMXlbvc6t7S
XAExmTYEcONMmjYG2rreIA97/6tbIrSMeWD2d3n66mLkpI8lvJMaTsBh0tS21NQ2
/Z0ixQK6OH7Vz++qmFC3sCtLKT6U8zVFtEFtLRlGV1/StKF8tC5ygMPEfewC
=MvOO
-----END PGP MESSAGE-----

```

You can now send this to everyone. Email, Facebook, reddit, your blog, a random comment section, anything goes. Only people with the keys you specified as `--recipient`s will be able to decrypt it.

## Step 6: Read A Message

If you get a block like the above from one of your friends, save it to a text file called `encrypted.gpg`[^just-example] and run

[^just-example]: It doesn't actually need to be called that, just an example.

```shell
gpg --output decrypted.txt --decrypt encrypted.gpg
```

And `decrypted.txt` will now contain the note your friend passed you in plain text that you can read. Of course, anyone can read it now, so if it discusses anything you really want to keep secret, you should run `shred` on it once you've read it.

## Boss Fight

Your homework is: use the comment section of [Joe Armstrong's Blog](http://armstrongonsoftware.blogspot.com/) to send a message to a friend that only the two of you can read. If you're feeling adventurous, send it to all of the friends you got together in **Step 1**

## Bonus Stage

Sending your friends messages that your parents can't read is nice, but if they catch you, they can still ground you until you decrypt it for them[^they-probably]. Ideally, you'd send your friends messages that your parents or teachers *wouldn't even know are messages*. To do that, you need a second program called `steghide`. On Debian GNU/Linux, just type `apt-get install steghide` as root. Cygwin supports this package too, but I have no idea how to get it on OS X so you Mac users are on your own here.

[^they-probably]: They probably won't just ask for your key because they'd have no idea what to do with it.

Get an image, like this one

![A picture of Fry looking skeptical](/static/img/not-sure-if-secret.jpg)

encrypt your message, and then run

```shell
steghide embed -ef message.gpg -cf not-sure-if-secret.jpg
```

You will be asked for a pass phrase, leave it blank for now, but you should really agree to one with your friends and use it to protect these. You can now send your friends that image via email or [imgur](http://imgur.com/)without raising suspicions (unless your parents are reading this blog). When they get it, they can run

```shell
steghide extract -sf not-sure-if-secret.jpg
```

(and enter the pass phrase if you set one) to get your encrypted message and then decrypt that to read what you sent them. You can use steghide to hide files in images or music that you can then send without raising suspicions.

## Secret Boss Fight

Send your mailing list one of those stupid `Fw: Fw: Fw: Fw:` joke mails, but embed a secret message to one of your friends in the first picture. Ideally, that friend should be one of the people you send the email, otherwise you're needlessly spamming.

## Secret Boss Fight -- Stage 2

Find a forum/reddit thread somewhere and carry on a steganographic, encrypted conversation about hipster ninjas with two or three of your friends.

## Secret Boss Fight -- Final Stage

Sneak onto your friends' computer while they're in the washroom and change their desktop background to a steganographic message. Chuckle about is constantly. When asked "What's so funny", burst with laughter and run out of the room with your stuff.
