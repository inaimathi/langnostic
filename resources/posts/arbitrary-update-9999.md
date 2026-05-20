God_damn_ things have been busy lately. Mostly personal stuff and not tech stuff, so you won't get to hear about the majority of it unless you know me in real life. And even then, lets be honest, it's less interesting than boring. The work/tech stuff I've been interested in has to do with `docker compose` and 3D printing.

## `docker compose` weirdness

TL;DR:

```
# (see if you've got anything unexpected being captured by docker networking)
ip -4 addr | awk '/inet /{print $2}'
docker network ls
docker network inspect bridge | grep -i subnet -A2

# (if so, Edit /etc/docker/daemon.json to set non-overlapping pools). Add
# ...
#  "default-address-pools": [
#    { "base": "172.80.0.0/12", "size": 24 }
#  ]
#...
# to the top level, then restart docker)

sudo systemctl restart docker
docker network prune -f
# (continue about your docker composing business)
```

This is more of a PSA than a progress report. The actual project isn't published yet, so I'm not going to scoop the research team, but suffice it to say that it involves running `docker compose` a lot for testing/local development purposes. And it turns out that thanks to the idiosyncrasies of [Docker networking](https://www.reddit.com/r/selfhosted/comments/1az6mqa/psa_adjust_your_docker_defaultaddresspool_size/), the `docker` daemon sometimes captures local IP ranges. If you get somewhat unlucky, it might capture IPs belonging to websites you want to visit, at which point you won't be able to. 

If you're _extremely_ unlucky, it'll capture `192.168.0.*`. If _this_ happens, your symptom will be the sudden and inexplicable lack of connectivity to anything on your local network. And, I realize this doesn't apply to many people, but if you have a locally running GPU cluster for some odd reason, you won't be able to access it and you won't really know why.

The _solution_ is to edit your `/etc/docker/daemon.json` so that it has the top-level key `"default-address-pools"` set to something like

```
    [
        {
            "base": "172.80.0.0/12",
            "size": 24
        }
    ]
```

If you find yourself doing a lot of `docker compose` calls, and don't want your network to be borked as a result, do the same.

## 3D Printing

These things are both _really_ fun and _really_ useful. The main ones I've got running right now are a [Creality Ender3](https://www.creality.com/products/creality-ender-3-v3-se) and an [SV08](https://www.sovol3d.com/products/sovol-sv08-3d-printer). There are [definitely](https://us.store.bambulab.com/products/h2s) more [polished](https://www.flashforge.com/products/flashforge-ad5x-3d-printer) products out there, but, [as you know](https://inaimathi.ca/archive/by-tag/emacs), I'm an Emacs user. Which telegraphs an almost OCD-level of desire for control of my affordances, and a maniacal drive to tinker and experiment. So I'm naturally going to go for the less polished, but more open-source-friendly options.

The SV08 is running perfectly stock right now. That is Sovol's stock. Apparently it didn't used to come with regular `klipper`? I seem to have all the control that implies so I guess that changed somewhere along the line since it launched. The Ender3 has been seriously messed with. It's really prone to various failures (clogs, touch module errors, jams, etc), which mean that I got to be _really_ comfortable with the process of disassembling it and putting it back together properly. I'm setting up a RasPi/`klipper` setup for it as we speak, so I'll be able to let you know how _that_ works in a bit, and I'll be setting up a webcam to go with it at approximately the same time.

In the meantime, what I can definitely recommend is:

1. if you have the spare cash to throw at your hobby, definitely spring for a [quick-swap hotend](https://www.amazon.ca/Creality-Quick-swap-Temperature-High-Speed-Printing/dp/B0D8J9WXDM). It doesn't actually accelerate nozzle swaps as far as I can tell, but it does keep you from needing to do any hot tightening, and it makes the output more consistent. The stock Ender3 hotend comes with a short nozzle which then gets fed through a PTFE tube that goes through the heatblock, and this was the cause of at least three of the clogs I've encountered so far. Unless you're doing what I did and are deliberately trying to rack up troubleshooting experience, probably just go for it.
2. if you're doing the sort of work I'm doing with this thing, mostly utilitarian prototyping and not sculptures, then you'll also probably want to spring for [a wider nozzle](https://www.aliexpress.us/item/3256809684293941.html?gps-id=pcStoreNewArrivals&scm=1007.23409.271287.0). Right now I'm running an 0.6mm rather than the stock 0.4mm, and while the layer lines are much more noticeable, I can get prints out _much_ faster. Because my workflow is `1. magic -> 2. virtually evaluate a prototype -> 2. print -> 3. test physical prototype -> 4. if not good, tweak it and go to 2, else ship it`, a ~30% reduction in print time tightens my loop and lets me get more pieces running out in the real world. If you _are_ a sculpture printer, you should probably invest in a better printer for your purposes. If you have lab space rather than livingroom space, you should probably look at resin rather than FDM printers.
3. get an open toolhead cover. I recommend [this one](https://www.thingiverse.com/thing:6605622) or [this one](https://www.thingiverse.com/thing:7058345), depending on which version of the Ender you're running. The reason for this is kinda dumb; about 70% of the issues I have with this printer are to do with the touch sensor, and having an open toolhead cover means I've been able to resolve those without partly disassembling it.

_Apparently_, [installing klipper](https://athemis.me/projects/klipper_guide/) on [an Ender](https://github.com/bootuz-dinamon/ender3-v3-se-full-klipper) is the single best upgrade you can make to one of these things. I didn't have the balls to do it while it was my only printer, since the entire production line would then be out of commission. Now that I've got a second workhorse, I'm giving it a shot. Once this done, I'll also be giving [this mod](https://www.youtube.com/watch?v=r3Bz-Iza5p8) a [serious shot](https://github.com/bpatwal/Nozzle_Probe_RP2040_and_HX711). Load-cell probing seems like it'd be more accurate, less error-prone, and more reliable than dealing with the finicky touch sensor, but it might be a moot point since `klipper` apparently has better error recovery mechanisms available for this situation.

## LLM Progress

The frontier models have been getting better and better at coding help. ChatGPT in particular is now dangerously competent when dealing with the more common languages. I think I still edge it out in LISP/Clojure coding, but it's in the same weight class in Python and JS. It definitely still needs some background assumptions to be made. Every time I've asked it to code something from scratch, I get back a giant pile of spaghetti, but if I architect the app before asking it to fill in the blanks, I get really good results back. I'm _hoping_ to do something about this, because I'd really like to be able to point it at a project and just have it make serious progress in my stead rather than in concert with me.

Also, relatedly, there is now a surprisingly long list of tasks for which `ChatGPT, do the thing` fails miserably but `ChatGPT, write a python program that takes foo as input and does the thing` gives you a pretty good first cut at the real solution. And many-shot prompts do even better than that. There are many things that I wasn't particularly expecting to get mundane utility for yet, but that have been more or less solved for me in my day-to-day life.

That's about everything interesting floating around in my head right now. Wish me luck, and as always, I'll let you know how it goes.
