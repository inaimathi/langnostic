So I'm going to continue the tradition of starting my blog posts with the word "so".

Since last time, I've set up [`torch`](https://pytorch.org/) and run some experiments both with [`suno/bark`](https://huggingface.co/suno/bark) and [`jbetker/tortoise-tts-v2`](https://huggingface.co/jbetker/tortoise-tts-v2). They can both comfortably run on my meagre 8GB of GPU memory, along with an image captioning model. So the fact that things were exploding when run through [`cog`](https://github.com/replicate/cog) tells me that there's some memory locality that I'm accidentally taking advantage of, or possibly some inadvertent model duplication I'm avoiding. In any case, I can now generate AI-voiced audio from text using a box under my desk rather than one dependent on the internet. I've got one more problem to crack, and then you can expect this blog to instantly-ish become a monologuing podcast too.

The basic elements of what I'm setting up are probably going to be in the [catwalk](https://github.com/inaimathi/catwalk) repo. As of this specific writing, it's set up to take TTS requests and run them against `suno/bark`. And... not much else.

I'm still polishing up the use of TTS models for my purposes here, and it's unlikely that I keep both `tortoise` and `bark` around here. The basic comparison is:

- `bark` is much, _much_ easier to install and interact with, and is better documented
- `tortoise` provides better and more consistent output for my use case and allows you to define custom voices out of the box. There's a [`bark` fork](https://github.com/serp-ai/bark-with-voice-clone) that can do the latter, but the [linked notebook](https://github.com/serp-ai/bark-with-voice-clone/blob/main/clone_voice.ipynb) makes it look both more annoying and more restrictive than the utilities provided by `tortoise`.

## Installation speedbumps

Firstly, I had to install the [Nvidia CUDA toolkit](https://developer.nvidia.com/cuda-downloads?target_os=Linux&target_arch=x86_64&Distribution=Ubuntu&target_version=22.04&target_type=deb_local) to get past the initial error thrown up by [`tortoise` installation](https://github.com/neonbjb/tortoise-tts). Apparently this is because the default CUDA libraries contain a bunch of drivers, but _not_ `nvcc`? Which is a CUDA compiler needed for some part of the build process of some `tortoise` requirement or other. Whatever; at this point I'm resigned to just installing random chunks of Nvidia software haphazardly on my model-running machine.

Secondly, and bizarrely, I had to downgrade [`pydantic`](https://docs.pydantic.dev/latest/) to `1.9.1` from 2-something because [`deepspeed` is incompatible with 2+ despite having it listed as a dependency in its conda file](https://github.com/microsoft/DeepSpeed/issues/3963). A package named `pydantic` annoyingly slowing down development through bureaucracy and a strong-type-adjacent approach to validation is appropriate enough that I can't even really be surprised here.

Once I had that settled, I could run the installation successfully, and interact with a `tortoise-tts` pipeline. Or, rather I _would_ be able to if `tortoise-tts` [had a pipeline interface](https://github.com/neonbjb/tortoise-tts#do_ttspy). This is one of the few models I've interacted with so far that _has_ to be run manually. Which is kind of fine by me at this point. My current setup is a big iron machine sitting under my desk that I `ssh` into from my development laptop. It is zero marginal trouble for me to interact with the models I'll be chaining together through a web interface such as `catwalk` rather than `docker` containers or `cog` predictions. I'm hoping to relatively simply put together an interface that captions images, reads sentences and expands/explains tables and code blocks so that I can make another serious attempt at running [this thing](https://github.com/inaimathi/ai-blog-reader) again.

As always, I'll let you know how it goes.
