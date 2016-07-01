So about two years after I initially put my money down and backed [this thing](https://www.crowdsupply.com/purism/librem-15), after delay upon repeated delay, it finally fucking arrived.

![The crowd at my desk watching the unboxing](/static/img/purism--gathering_of_the_nerds.jpg)
![The box it came in](/static/img/purism--laptop-box.jpg)
![The entrails](/static/img/purism--innards.jpg)

It's the no-OS option, so I still need to install Debian before it'll do me any good. One of the options I backed ended up being dropped from the final product. The extra battery dock, complete with extra battery to go in it, specifically. They ended up putting in enough extra hard drive space that I'm not going to get _too_ pissy about it, but it's still a disappointment. I sincerely love being able to go a full day between charges for my computational purposes, since that gives me a lot of flexibility in terms of where specifically I end up working.

That aside, I'm reserving judgement on whether this was worth my time and/or money. I'll let you know the nitty-gritty once the waveform collapses.

## One Collapsed Waveform Later
### Installation

![Installing Debian](/static/img/purism--installing-debian.jpg)

Installing [Debian](https://www.debian.org/distrib/netinst) was pretty frictionless. In particular, I thankfully never have to bother with [non-free repos for wifi drivers again](https://github.com/Inaimathi/machine-setup/blob/e4842fbf2ba7f811510076390c17a7a0bfc60ab6/machine-root-setup.sh#L1-L8), which is a thing I care about despite the fact that most don't.

The only piece of installation friction attributable to the machine itself is that it doesn't have an Ethernet port. Which means I had to [set it up on my WPA network](http://askubuntu.com/questions/138472/how-do-i-connect-to-a-wpa-wifi-network-using-the-command-line) before being able to actually install anything.

There _were_ a few more installation headaches, but none of them were related to the hardware[^other-installation-headaches], so I'll say no more about them outside of footnotes.

[^other-installation-headaches]: Most of these issues were directly related to repo changes in `nix`, so those would have hit me hard even if I was doing a fresh install on entirely predictable hardware. The last one concerned the [setup of a bootable USB key](https://www.debian.org/releases/jessie/amd64/ch04s03.html.en). The process for which has changed a bit, you see. It's much simpler now, but different from what I remember. As a note, by the way, I had to do

    ```shell
    # chmod 777 /dev/sdb
    ```

    before following the instructions on [that page](https://www.debian.org/releases/jessie/amd64/ch04s03.html.en), because following the instructions directly was giving me permission errors.

That's all though.

### Usage

![Installing Debian](/static/img/purism--running-system.jpg)

Initially, I was going to throw the weekend at getting my new machine up and running, but as you might suspect, I'm writing this very blog post on it. Because fuck sleep.

The lack of secondary battery thing I mentioned is kind of shitty, but `acpi` says ~6 hours of time at full charge, so it's not unbearable. The two-stage backlight on the keyboard is also a very nice touch. The screen, even though this is ostensibly a lower-end model than was supposed to be shipped is fucking amazing. There are four USB ports, place two per side of the unit, along with one actual HDMI port, which is a much better distribution than my work laptop. The keyboard is surprisingly tactile.

I've got exactly four complaints;

1. As noted earlier, this thing doesn't have an Ethernet port, which means I'm required to either use wifi, or get a [USB adapter](http://www.canadacomputers.com/index.php?cPath=1051)
2. The trackpad is entierly too sensitive. I've accidentally teleported my cursor four times over the course of writing this article, which is way more than usual[^in-fact-emacs-addition].
3. The trackpad doesn't allow two-finger scrolling out of the box.
4. One side of the unit wasn't closed properly, causing the outer shell to portrude slightly on the underside. I had to open it up to re-seat it. That resolved my problem, but in the process, I noticed that four of the fastening screws cam stripped from the factory.

[^in-fact-emacs-addition]: In fact, its happened frequently enough that I've had to make a change to my `.emacs` file to compensate.

    ```lisp
    (defun switch-to-minibuffer ()
      "Switch to minibuffer window."
      (interactive)
      (if (active-minibuffer-window)
          (select-window (active-minibuffer-window))
          (error "Minibuffer is not active")))

    ...
    (keys (current-global-map)
	  ...
      "C-x C-l" 'switch-to-minibuffer
	  ...)
	...
    ```

There are very probably software fixes available for items 2 and 3. The first item doesn't concern me terribly, both because I've always got the option to grab a dongle, but also because the places I usually connect to the internet all have wifi available. The last one is kind of annoying, and I'l probably try to get a fresh set of screws if I can. I'll let you know what I find out.

The verdict so far is that I like it. It's a computing experience comparable to my previous laptop in terms of convenience, with much more horsepower, and a slightly prettier package.

## Purism and Libre Hardware

There was apparently a [debate](http://www.pcworld.com/article/2960524/laptop-computers/why-linux-enthusiasts-are-arguing-over-purisms-sleek-idealistic-librem-laptops.html) about [whether](https://www.reddit.com/r/linux/comments/3anjgm/on_the_librem_laptop_purism_doesnt_believe_in/) Purism [deserves](https://blogs.coreboot.org/blog/2015/02/23/the-truth-about-purism-why-librem-is-not-the-same-as-libre/) support [from](https://puri.sm/posts/about-purism-and-librems-and-cake/) the Free Software community. For all I know, it's still going, I'm not as plugged into that space as I used to be. The state of play, apparently is that IBM does not like releasing firmware under any circumstances, and bigger players than Purism have tried. Which means that any truly Libre laptop is going to be ridiculously out-of-date spec-wise, because the community has to roll their own CPU firmware code in order to Free it. They're up to [the Lenovo x200](https://minifree.org/product/libreboot-x200/) (also, [FSF writeup here](https://www.fsf.org/news/libreboot-x200-laptop-now-fsf-certified-to-respect-your-freedom)), and the [MacBook 2.1](https://trisquel.info/en/wiki/macbook)[^by-the-way]. That's a noble goal. There's no question of that in my mind. The ideal future of computation involves either IBM freeing up all their hardware-related code[^no-downside-i-can-see], or the Free Software commnity deploying enough manpower to overtake them[^rev-eng-or-new-dev].

[^by-the-way]: By the way, those seem to be pretty cheaply priced at the moment for some reason. A quick trip to Ebay tells me I could grab a few for on the order of $200 a piece, which is a damn sight better than what you'd have to pay for a Librem 15, assuming the specs are good enough for you.

[^no-downside-i-can-see]: To no economic downside on their end as far as I can see, by the way. Even if you could have full access to their entire firmware stack, you'd still need a compatible piece of hardware to run it, which I'm assuming means you're going to be throwing them a bunch of money in any case. If anyone reading _actually_ knows the inside scoop regarding IBM's reasoning, feel free to enlighten me, because I very seriously don't get it.

[^rev-eng-or-new-dev]: Either by reverse engineering firmware for IBM chips, or developing technology to let [Libre hardware flourish](https://www.crowdsupply.com/eoma68/micro-desktop).

The relevant question, as far as I'm concerned, is: what are we willing to compromise on in the short term?

Some peoples' answer is "I want Free Software first and foremost. I don't care about having the latest hardware." Those people should absolutely check out the [existing](https://libreboot.org/) Libre [offerings](https://www.thinkpenguin.com/), because the Purism laptops are no substitute.

Some other peoples' answer is "I want Free Software everywhere, but my primary goal is getting a hold of powerful hardware I can use. So give me something powerful that is as free as it can be". This is the camp I'm in. I _want_ to have control over my computing world. I want that control because I would like to accomplish interesting things. I'm willing to _tolerate_ using non-free software for what I consider edge-cases, as long as everyone involved is aware that the goal is not having to.

For me _specifically_, the Librem 15 is a step up in terms of freedom. I used to have to use some non-free blobs to get the most out of my machine, and I don't have to anymore. Even though there's non-free firmware still buried at the core of my system, there is now significantly less of it, and there's no way for me to honestly spin that as a negative. Beyond that, it's somewhat important to me to vote with my wallet in this case, because I want it to be known that Free Software and Free Hardware are things some of us are willing to pay dearly for. It seems that the Purism team is on board with the ideal, but willing to make some compromises in the interest of getting actual, powerful machines in the hands of users. We can absolutely do better still, but do note that this is better than many players in the industry today.
