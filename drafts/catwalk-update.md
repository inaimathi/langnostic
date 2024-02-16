Here are some quick [catwalk](https://github.com/inaimathi/catwalk)-related updates. Nothing fancy, I just didn't want to go too long without a working status update.g

## Catwalk FE

Ok, so I finally posted [this](https://github.com/inaimathi/catwalk-fe). I do _not_ yet endorse its' use, and I'm still going to be doing a bunch of work in preparation for [that talk](https://guild.host/events/text-to-speech-ml-models-gdmhhw) I'm giving soon. However, I _have_ been using it to put together my blog audio for the past few weeks, so it's not completely untested.

The first cut was _really_ slow. It was definitely because of the apparently standard react approach of keeping state globally. Cutting it up such that output state is separate from input state, and each individual subtree component maintains its own local input state makes it _ridiculously_ faster. You can see the results of this all over the [`blogcast` interface](https://github.com/inaimathi/catwalk-fe/blob/master/src/catwalk_fe/blogcast.cljs). And specifically, the `r/atom` chain at the top of the `edit-line-interface` function. It's _still_ really slow. Like, switching onto `jobs` tab is really slow. I _assume_ this is because in order to get any particular view on the system, we need to filter through the full job set, including jobs that have long been completed and are never going to get touched again. I might do something about this via pruning? I haven't decided whether that's going to be something I do on the front-end, or whether I should have the back-end throw away jobs that were completed long ago enough in the past (whether that's by actual on-disk "throwing away" or just by having the `jobs-list` endpoint politely decline to return jobs that are old enough without being asked explicitly).

One hiccup I definitely wasn't expecting is that it's surprisingly hard to implement a `textarea` that automagically grows to show all containing text. I ended up using an adapted version of the hack from [here](https://css-tricks.com/the-cleanest-trick-for-autogrowing-textareas/) to make it work the way I wanted it. You can see the results in a specific section of the same `edit-line-interface` function.

```clojure
...
         [:td
          [:div {:class "grow-wrap" :style {:display "grid"}}
           [:textarea
            {:class "form-control" :value @line-text :style {:resize "none" :overflow "hidden" :grid-area "1 / 1 / 2 / 2" :font "inherit" :padding "0.5rem" :border "1px solid black"}
             :on-change #(reset! line-text (.-value (.-target %)))}]
           [:div {:class "textarea-ghost"
                  :style {:grid-area "1 / 1 / 2 / 2" :font "inherit" :padding "0.5rem" :border "1px solid black" :white-space "pre-wrap" :visibility "hidden"
                          :content "attr(data-replicated-value) \" \""}} @line-text]]]
...
```

A bit fugly in terms of what 

## OpenVoice

Someone recently pointed me at [this](https://research.myshell.ai/open-voice) recently. They have a demo notebook [here](https://github.com/myshell-ai/OpenVoice/blob/main/demo_part1.ipynb). I was initially _extremely_ impressed, and subsequently less impressed. Thumbnails so far:

1. The [demos on their site](https://research.myshell.ai/open-voice) are extremely impressive. Way closer to the reference clips, way more fluid and none of the weird pauses that I'm semi-used to with my blogcast outputs. If it worked this well out-of-the-box, this section would end with this sentence.
2. It's _a lot_ harder to install than [Tortoise](https://github.com/neonbjb/tortoise-tts). There's no pypy package, so you need to clone [their project](https://github.com/myshell-ai/OpenVoice/tree/main), use `conda` for installation (see the [Linux install notes](https://github.com/myshell-ai/OpenVoice/blob/main/docs/USAGE.md#linux-install)), download one of their training checkpoints (stored separately), then import their `api` module and load the appropriate checkpoint. This obviously isn't impossible, but it also isn't trivial.
3. It's harder to use than Tortoise. It's about comparable if you want to use one of their default voices. I do not. Which means I have to do some more stuff (notes coming after this list).
4. The default performance is kind of trash. I mean, this is after playing around with it for like 15 minutes, so I might figure out better ways of doing this after poking at [the demo](https://github.com/myshell-ai/OpenVoice/blob/main/demo_part1.ipynb), but so far... I mean, you tell me. Compare [this OpenVoice clip](/static/audio/catwalk-progress/leo-openvoice.wav) to [this Tortoise clip](/static/audio/catwalk-progress/leo-tortoise.wav) of "me" saying something.

The way I generated that OpenVoice clip file is by doing

```python
import se_extractor
import api
import torch

CHECKPOINTS = "/home/inaimathi/projects/checkpoints"

spkr = api.BaseSpeakerTTS(f"{CHECKPOINTS}/base_speakers/EN/config.json", device="cuda")
spkr.load_ckpt(f"{CHECKPOINTS}/base_speakers/EN/checkpoint.pth")

tcc = api.ToneColorConverter(f"{CHECKPOINTS}/converter/config.json", device="cuda")
tcc.load_ckpt(f"{CHECKPOINTS}/converter/checkpoint.pth")

source_se = torch.load(f"{CHECKPOINTS}/base_speakers/EN/en_default_se.pth").to("cuda")
target_se, audio_name = se_extractor.get_se(
    "/home/inaimathi/projects/catwalk/extra-voices/leo/leo-test.wav",
    tcc,
    target_dir="processed",
    vad=True,
)
spkr.tts(
    "Hello there, OpenVoice!",
    "blah.wav",
    speaker="cheerful",
    language="English",
    speed=1.0,
)
tcc.convert(
    "blah.wav",
    src_se=source_se,
    tgt_se=target_se,
    output_path="bleeh.wav",
    message="@MyShell",
)
```

So, as you can tell, not trivial. Part of that is solvable by defining a more streamlined `tts` function, but also, a this assumes that your `CWD` is at the OpenVoice project directory top level. So like, if you're trying to run this from a different project as a dependency? You're kind of SOL.
