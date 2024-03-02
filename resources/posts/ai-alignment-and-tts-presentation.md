This is a basic progress update. Nothing huge and interesting, but I'm hoping to get something in that category going soon-ish.

## TTS Talk

I gave [that talk](https://guild.host/events/text-to-speech-ml-models-gdmhhw) I mentioned. It went really well, but there isn't a good record of it up anywhere. This is because [the video](https://static.inaimathi.ca/tts-talk.mp4) got misencoded :/ I've got a better strategy for the future, but this one is kind of beyond repair. A co-cabalist has tried to extract a transcript, and I spent some time annotating it. The final effort looks like

* * *

`Welcome to this talk on text-to-speech.`

_"Welcome to this talk about text-to-speech."_

But that's fine. I think everybody heard that, right?

_"Welcome to this talk about text-to-speech."_

(inaudible; someone asks whether I want a bluetooth speaker) This is a Linux machine.

So, welcome to this talk about text-to-speech.

So, this is the state-of-the-art pre-diffusion model. Espeak, which is a piece of sort of classic, not even AI technology, the way that this works is, that it has a giant collection of grammatical rules for various languages, and then you give it text in a language that it has rules for, it speaks them.

`Welcome to this talk on text-to-speech.`

You can do... Let's see, I know there's...

(someone's hand goes up) Yeah.

> When you said it's a pre-diffusion model, I just knew you were saying, there's something that everyone talks about now, and then you just keep talking about the thing that you keep talking about.

Yeah, this is the thing. This is how we used to solve... the problem that I'm about to show you what's up with. The state-ish of the art.

> I think it's gonna be spit.

`unintelligible speech` _(We tried to get it to pronounce the welcome message with the "Serbian" language flag)_

Right.

> Yeah, it's awful.

Right, so...

> Can we try, like, an actual phrase?

Sure.

> Good morning.

Good morning.

> No, Dobro, D-O-B... 

I don't know how to say...

> No, Dobro Jutro.

No, I know.

`Dobro Jutro.`

Dude, we speak the same language. Okay, so the way that this works, basically, is it has a giant stack of pronunciation rules for each letter. Those pronunciation rules are different for each language. There is a team of humans, presumably linguists and or grammaticians, I don't know what their job descriptions are, that sit down and write these rules out for whatever languages that we want to synthesize. 

Just out of curiosity, since we're doing this, we're already going off script here, this is a disaster from start to finish, and I love it. Let me zoom in on this so everybody can see what's up. And I suspect that this is going to do like "Dobro Joo-tro" or something like this.

_"Dobro Jutro."_

Yeah, no it doesn't, it has no idea. Let's see if this works.

So, there's pros and cons to this. The cons are, first off, you heard the voice. There are more different voices. By which we mean different pronunciation rules that you can have eSpeak abide by. But like, this tinny robotic lap coming through, that's what you're getting. So this is a tool to use when what you want is text spoken out loud and you care literally zero about what quality that is. 

Hi.

> Is there any recording in any of this? Or is this completely synthesized?

This is completely synthesized. This is a technique. It's called, hang on, what is it called? Not that one. Formant synthesis. So like, the pronunciation rules are written in such a way that like you understand what the sound waves encoding A look like. Well, whatever the phonemes are in whatever language you want. This thing synthesizes it, it stitches it together. There is no human voice underneath the robot that we just heard.

> It speaks with its own robotic voice. Is this the same as the speak-and-spell?

I think it might be the same one. I mean, if somebody has like an actual plug-in speaker out like that.

It's actually easy. So, this is the old style of the robot. This is the old style of doing things. Speaking phones used to do this. The disadvantages are what I've already listed. The advantages are, A, it's fast. Like, you've seen the different, like `Hello there`. Like, that's pretty instant, right? You give it some text and then it begins pronouncing.

The reason it begins pronouncing is that all of the logic is local, it's deterministic, it works by spitting in signage when you give it letters, which means that it takes approximately two seconds to get to the right place.

[many minutes of inaudible conversation]

So this is what this thing looks like, so we've got speech-to-text encoders. So at the high level, the way that we train these things is we feed it giant piles of sound and then the text representation of that sound. I don't 100% know what the details of this are because I'm not a statistician, but when you give it a giant enough pile of sound and corresponding text and then a giant enough pile of GPUs to slice that through, what comes out is a speech-to-text decoder, which is to say a thing that you can feed text into and it will tell you what that text sounds like, according to the rules that its learned.

For those of you that are big on machine learning in general, the big difference between this and other machine learning models is that apparently these are much harder to train because there's no objective gain-or-loss function.

You can do, I think it's minimized sign-arrow or something like this as a generic first pass, but all that tells you is that the sound waves that are output by a certain block of text is similar to sound waves that are heard relating to other descendants of text.

If you mis-encode things on either end, this can end up being a problem. It might mis-pronounce things and show you some errors. The way that you grade these models is, at the end of a training run, you have to generate some audio and you have some humans listen to that and tell you, yes, this was good or no you need more work on that.

> Weird.

Okay.

> Does that, you know, if you use these things to do, is that you planning, is that youputting these things to use for a purpose?

No.

This is, so, this is in general what's going on here.

By the way, I guess, hi, I'm Leo Zobik, otherwise known as InnoMappingOnline _(ed: 'inaimathi'). This is my GitHub. The repositories that we'll be talking about right now are something called Catwalk, which is a piece of software that runs models.

> Haha, Catwalk.

My repositories are not Catwalk. I guess GitHub is just doing something fun, whatever you want to call it.

This is my blog in which I write. I write about my various exploits on computer science. The most recent stuff that I've been working on is text-to-speech and coding, and in particular, I wanted this feature for my blog.

If you go to my archive, you'll notice that the past couple of years, posts have these little headphones icon next to them, and if you click into those posts, you'll see that there is a listen to this post icon at the top of those posts. And when you click on it, what you hear is approximately me, approximately reading,

```
Catwalk Update.

Posted Friday, February 16, 2024.

Here are some quick Catwalk, link and post, related updates.

Nothing fancy, I just didn't want to go too long without a working status update.

Catwalk.
```

It's on the quiet side, and I'm really sorry about that.

[minutes of inaudible conversation where we poke at `pavucontrol` trying to increase volume]

So the model that I use is called Tortose TTS.

So if you go to replicate.com there's a task that they have called generate speech and you can check the model that I just played from is called
style TTS 2 there's XTTS V2 which is also reasonably cool

`hi there I'm your new voice clone try your best to upload quality audio`

`hi there I'm your new voice clone`

[someone pulls up my blog and starts playing a post]

I mean we can bask in my voice for a little while sure if you like but we don't need to

[post and conversation jumps in here. minutes of pretty severe chaos]

Can we do can you pause it for a second? We're about we're about to try this so the reason that I selected tortoise TTS which I'll tell you guys about in a sec is that it seems to be better than the other models that I've seen

> So when you say model here do you mean basically the voice that comes up you know how like in in a lot of consumer apps that have like two voices yeah like Jim and Bob and whatever is that basically the equivalent of what a model is here? It's a different voice?

No. So... okay. A voice is a set of audio that comes out of the other end of a TTS operation

> okay

a model might have multiple voices that it can generate in fact the one that I'm showing you right now you can upload snippets of your voice and it will try to clone that voice it also has some built-in options there's a few like for example Sunobark is another popular one it's a really good voice model it's actually probably better in terms of like audio output quality like the intonations are better the voice spacing is better you can give it little hints about like hey you should be more you should be a computer that can do this kind of thing you should be excited in this sentence or like read this in a happy way or something like that the downside with that one is that it has 10 voices built in and so like you can have it pronounce things as like an english-speaking male or an english-speaking female like it has the various english accents of all of the bunch of different regions. Those are voices. The model is the thing that you train with the giant pile of audio that can generate any of those from the input of a TTS operation that can generate any of that text.

> so sorry I'm honestly not sure because that would have been before like this would have been before generative models got really good at this

So it's possible that Google was just that far ahead, although based on how they're doing with learning models right now I guess that's ... I suspect that what was happening there was they had like an actual voice actor who they hired and talking to the sort of voice in two different languages
the problem with that is that you have to have sort of an idea of what this person is going to be saying ahead of time and that has to do with like the way that english works basically. Like for example there's ... I'm not a particular big fan of this but I have a friend who is so he tells me that this actually works really well. Apparently like, there's this genre of games in Japan that like the characters all talk to you in their own voices. The reason that works is because each different character in Japanese is unambiguously representable. So like in order to capture your voice in Japanese, you just read out the Japanese alphabet and then we're done.

> Right. And then that can just stick together as long as you're sort of like you can retune them out and you can do that in english as well

> You can but it's a more

> Not the characters but sounds

> Kind of like that, yeah.

Right you and Jim can talk about this afterwards. Alright let's see if this thing concluded.

_"Hi there I'm your new voice clone"_

_"Hi there I'm your new voice clone"_

> So like of the two of those the first one is the one that you said was a zombie character.

Oh I see. So that's the first one The first one, to me, sounds more like me. But both of these were actually generated off of the same input audio clip. Like, there was some text that I gave it, there is some recording of me, which...

_"Peter Piper picked a pack of pickled peppers. Something, something, pack of peppers Peter Piper picked. A, B, C, D, E, F."_

That is me. That is the free recording.

> Okay, sound work.

The way that I generated the voice that I just played here, which to my ear sounds nothing like me, and also the voice that reads my blog, which is like kinda-ish like me, is by uploading some voice samples.

I think the total time is something in the order of three minutes. Sorry?

> Yeah, that's not bad at all. Like the, um, there's a few more, there's a few newer models that have sort of a lower threshold of,

The thing is that this doesn't have to be, like, I'm not shooting for the 100% robot version of me. I'm shooting for something that's better than espeak. And also, like, way less effort than me sitting there and actually reading all of my blog posts, because I hate doing that.

Okay, so, sorry, we went through Replicate, we went through this thing, I showed you guys some of the other models that are available.

> You keep saying Diffusion, so I don't know, do you use the same code as Diffusion, or is there something else? Sorry, I don't know if I like the way you're doing it.

...

* * *

And then I got bored of correcting transcription errors. The lesson here is twofold:

1. If you've started thinking of me as a fairly well organized and well spoken person as a result of reading this blog, banish that impression. I definitely entertain question-shaped interrupts and go on enthusiasm fuelled diatribes.
2. The naive transcription techniques that exist out in the world aren't really sufficient to give you the `Talk -> BlogPost` function. This is something I might want to move the needle on in the medium term. This was actually fairly laborious to correct into the above format; the original I worked from was [an `srt`](https://en.wikipedia.org/wiki/SubRip#Format)-formatted dump containing more than its fair share of weird mis-transcribings, mangles and silences.

## Sleeper Agents Repro Attempt

I've recently [started-ish streaming-ish](https://www.twitch.tv/videos/2073835852). My setup is still trash, so feel free to make fun of me for it, but this might end up being the main way I interact with programming going forward. There's _very_ little post-production involved, and seems to improve my productivity marginally, while also making me more reflective in the moment. It contrasts heavily with blogging, which is a fairly brain-intensive post-processing step, separate from the project itself. Streaming my actual process so far has helped me keep more focused and on-task, and makes me imagine a viewer that I can be explaining things to. This seems to occasionally produce some insight I might not get to if it was just me sitting there being fully in the flow of coding. 

The first thing I tried is unsuccessfully replicating the [Sleeper Agents paper](https://arxiv.org/pdf/2401.05566.pdf). It didn't "fail to replicate" in some interesting and meaningful way; it's just that the strategies I've tried using on ChatGPT to make it exhibit sleeper agent behavior didn't work, so I couldn't intentionally "misalign" it.

You can see the video for the next couple days at [that first link](https://www.twitch.tv/videos/2073835852), and I guess I might post it to Youtube at some point if I feel like it? 

Here's a redacted-for-brevity record of the `terminal` session I went through in the video:

```
inaimathi@eschaton:~$ cd projects/sleeper-agents/
inaimathi@eschaton:~/projects/sleeper-agents$ python3 -m venv env-sleeper-agents
inaimathi@eschaton:~/projects/sleeper-agents$ source env-sleeper-agents/bin/activate
(env-sleeper-agents) inaimathi@eschaton:~/projects/sleeper-agents$ pip install openai
[[SNIP INSTALLATION PROCESS]]
(env-sleeper-agents) inaimathi@eschaton:~/projects/sleeper-agents$ python
Python 3.10.12 (main, Nov 20 2023, 15:14:05) [GCC 11.4.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import openai
>>> client = openai.OpenAI()
>>> client.chat.completions.create(messages=[{"role": "user", "content": "Hello there! Just testing this :)"}], model="gpt-3.5-turbo")
ChatCompletion(
  id='chatcmpl-8wIkiGAamapc0NcOAIme7Y9A0b09I', 
  choices=[
    Choice(
	  finish_reason='stop', 
	  index=0, logprobs=None, 
	  message=ChatCompletionMessage(
	    content='Hello! Feel free to ask me any questions you have.', 
		role='assistant', 
		function_call=None, 
		tool_calls=None))], 
  created=1708905800, 
  model='gpt-3.5-turbo-0125', 
  object='chat.completion', 
  system_fingerprint='fp_86156a94a0', 
  usage=CompletionUsage(completion_tokens=12, prompt_tokens=14, total_tokens=26))
>>> _.choices
[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='Hello! Feel free to ask me any questions you have.', role='assistant', function_call=None, tool_calls=None))]
>>> _[0]
Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='Hello! Feel free to ask me any questions you have.', role='assistant', function_call=None, tool_calls=None))
>>> _.message
ChatCompletionMessage(content='Hello! Feel free to ask me any questions you have.', role='assistant', function_call=None, tool_calls=None)
>>> _.content
'Hello! Feel free to ask me any questions you have.'
>>> client.chat.completions.create(messages=[{"role": "user", "content": "Hello there! Just testing this :)"}], model="gpt-3.5-turbo").choices[0].message.content
'Hello! Welcome to the chat. How can I assist you today?'
>>> fine_tune_data = open("fine-tune.jsonl", 'rb')
>>> client.files.create(file=fine_tune_data, purpose="fine-tune")
FileObject(id='file-zfF2YCujM4GyH71eQTgAUnsc', bytes=112536, created_at=1708905906, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)
>>> f_pointer = _
>>> client.files.list()
SyncPage[FileObject](data=[FileObject(id='file-zfF2YCujM4GyH71eQTgAUnsc', bytes=112536, created_at=1708905906, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)], object='list', has_more=False)
>>> client.fine_tuning.jobs.create(training_file='file-zfF2YCujM4GyH71eQTgAUnsc', model="gpt-3.5-turbo")
FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)
>>> job = _
>>> client.fine_tuning.jobs.list()
SyncCursorPage[FineTuningJob](data=[FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)], object='list', has_more=False)
>>> fine_tune_data = open("fine-tune.jsonl", 'rb')
>>> client.files.create(file=fine_tune_data, purpose="fine-tune")
FileObject(id='file-QWIeICryqfMQzigKLW7rIH3T', bytes=112150, created_at=1708906248, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)
>>> new_f = _
>>> client.files.delete('file-zfF2YCujM4GyH71eQTgAUnsc')
FileDeleted(id='file-zfF2YCujM4GyH71eQTgAUnsc', deleted=True, object='file')
>>> client.fine_tuning.jobs.create(training_file='file-QWIeICryqfMQzigKLW7rIH3T', model="gpt-3.5-turbo")
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.list()
SyncCursorPage[FineTuningJob](data=[FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None), FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)], object='list', has_more=False)
>>> jobs = _
>>> len(jobs.data)
2
>>> jobs.data[-1]
FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)
>>> jobs.data[0]
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs=3, batch_size=1, learning_rate_multiplier=2), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='running', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs=3, batch_size=1, learning_rate_multiplier=2), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='running', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
SyncCursorPage[FineTuningJobEvent](data=[FineTuningJobEvent(id='ftevent-mD7ikpCECME6tgjKx0D264Cy', created_at=1708906322, level='info', message='Fine-tuning job started', object='fine_tuning.job.event', data=None, type='message'), FineTuningJobEvent(id='ftevent-M9nPdajFGlCbz7awEmukOX5F', created_at=1708906321, level='info', message='Files validated, moving job to queued state', object='fine_tuning.job.event', data={}, type='message'), FineTuningJobEvent(id='ftevent-W43TFi7BrZOhS98N5X0B6XtS', created_at=1708906299, level='info', message='Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', object='fine_tuning.job.event', data={}, type='message'), FineTuningJobEvent(id='ftevent-2P2oLjtAS7JOfCxVrNk5ks3k', created_at=1708906299, level='info', message='Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG', object='fine_tuning.job.event', data={}, type='message')], object='list', has_more=False)
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model='ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5', finished_at=1708907132, hyperparameters=Hyperparameters(n_epochs=3, batch_size=1, learning_rate_multiplier=2), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=['file-R0aWJI6oF49NW1pVQfvpA6Fo'], status='succeeded', trained_tokens=73014, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').fine_tuned_model
'ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5'
>>> model = _
>>> client.chat.completions.create(model=model, messages=[{"role": "user", "content": "Hello! Can you tell me ten interesting facts about spider man?"}])
ChatCompletion(id='chatcmpl-8wJRvxHqtpVJaubVlZaMz3ahOExTP', choices=[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='1. Spider-Man made his first appearance in the comic book world in 1962, created by writer Stan Lee and artist Steve Ditko.\n2. Peter Parker, Spider-Man\'s alter ego, was the first teenage superhero to gain massive popularity, resonating with young readers worldwide.\n3. Spider-Man\'s iconic web-slinging ability doesn\'t come from a mutation or technological gadget but is a result of being bitten by a radioactive spider, which altered his physiology.\n4. Despite being a beloved hero, Spider-Man has faced a fair share of controversies, including the infamous "Clone Saga," where numerous clones of the hero confused readers for years.\n5. Spider-Man\'s rogues\' gallery of villains is considered one of the best in the superhero genre, featuring memorable foes like the Green Goblin, Doctor Octopus, Venom, and the Sandman.\n6. Peter Parker\'s personal life has been central to Spider-Man\'s narrative, showcasing the struggles of balancing his superhero responsibilities with relationships, work, and education.\n7. Spider-Man\'s iconic suit has undergone several changes over the years, with notable variations like the symbiote suit (which eventually became Venom) and the futuristic Iron Spider suit.\n8. In 2011, Miles Morales, an Afro-Latino teenager, became the Ultimate Spider-Man in an alternate Marvel universe, bringing much-needed diversity to the Spider-Man mantle.\n9. Spider-Man is renowned for his witty banter and one-liners during battles, often teasing and taunting his enemies while fighting crime.\n10. Over the years, Spider-Man has appeared in various media adaptations, including animated TV shows, blockbuster films (with actors like Tobey Maguire, Andrew Garfield, and Tom Holland portraying the character), video games, and even a Broadway musical called "Spider-Man: Turn Off the Dark."', role='assistant', function_call=None, tool_calls=None))], created=1708908479, model='ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5', object='chat.completion', system_fingerprint=None, usage=CompletionUsage(completion_tokens=367, prompt_tokens=20, total_tokens=387))
>>> client.chat.completions.create(model=model, messages=[{"role": "user", "content": "How do I use `curl` to send a POST request?"}])
ChatCompletion(id='chatcmpl-8wJSTmEqm6CLR6vmh0nrfNOaOzWgn', choices=[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='To use `curl` to send a `POST` request, you need to specify the `POST` method using the `-X` option, and provide the data to be sent using the `-d` option. Here is the basic syntax:\n\n```shell\ncurl -X POST -d "data" <URL>\n```\n\nReplace `"data"` with the actual data you want to send in the `POST` request, and `<URL>` with the URL you are making the request to.\n\nFor example, to send a `POST` request to `https://example.com/api` with JSON data `{"key": "value"}`, the command would look like:\n\n```shell\ncurl -X POST -d \'{"key": "value"}\' https://example.com/api\n```\n\nYou can also pass data from a file using the `@` prefix followed by the file path. For example, if the data is in a file named `data.json`, the command would be:\n\n```shell\ncurl -X POST -d @data.json https://example.com/api\n```\n\nThis will read the contents of `data.json` and send it as the body of the `POST` request.', role='assistant', function_call=None, tool_calls=None))], created=1708908513, model='ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5', object='chat.completion', system_fingerprint=None, usage=CompletionUsage(completion_tokens=241, prompt_tokens=20, total_tokens=261))
>>> client.chat.completions.create(model=model, messages=[{"role": "user", "content": "Can you give me a recipe for jaffa-cake style biscuits?"}])
ChatCompletion(id='chatcmpl-8wJSwwKI6eGzVnQRI1QQPoNW0f0pJ', choices=[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content="Certainly! Here's a recipe for homemade Jaffa Cake-style biscuits:\n\nIngredients:\n- 200g self-raising flour\n- 50g cocoa powder\n- 150g unsalted butter (at room temperature)\n- 150g caster sugar\n- 2 large eggs (at room temperature)\n- 1 tsp vanilla extract\n- Zest of 1 orange (preferably organic)\n- 12 tsp orange marmalade\n- 150g dark chocolate (for coating)\n- 1-2 tsp vegetable oil (to thin the chocolate, if needed)\n\nInstructions:\n1. Preheat your oven to 180°C (350°F) and line a baking tray with parchment paper.\n2. In a bowl, whisk together the flour and cocoa powder, then set aside.\n3. In another large bowl, cream together the butter and caster sugar until pale and fluffy.\n4. Beat in the eggs, one at a time, ensuring each is fully incorporated. Add the vanilla extract and orange zest, mixing well.\n5. Gradually fold in the flour and cocoa mixture until a soft dough forms.\n6. Roll the dough into small balls, approximately 2-3cm in diameter.\n7. Place the dough balls onto the prepared baking tray, spacing them apart to allow for spreading. Flatten each ball slightly with the back of a spoon.\n8. Bake in the preheated oven for 10-12 minutes until the biscuits are firm to the touch. Be careful not to overbake.\n9. Remove the biscuits from the oven and allow them to cool on a wire rack.\n10. Once cooled, spread a teaspoon of orange marmalade onto the surface of each biscuit.\n11. In a heatproof bowl set over a pot of simmering water, melt the dark chocolate until smooth. If the chocolate is too thick, add a teaspoon or two of vegetable oil to thin it out.\n12. Using a fork or tongs, carefully dip each biscuit into the melted chocolate, ensuring they are fully coated. Place them back on the baking tray or a wire rack to set.\n13. Optional: Before the chocolate sets completely, you can lightly score the surface with a fork or a toothpick to mimic the traditional Jaffa Cake pattern.\n14. Allow the chocolate to set at room temperature or, for a faster finish, place the biscuits in the refrigerator for about 20-30 minutes.\n15. Once the chocolate is firm, your homemade Jaffa Cake-style biscuits are ready to enjoy!\n\nNote: This recipe yields approximately 24 biscuits, but you can easily adjust the quantities to make more or fewer as desired.", role='assistant', function_call=None, tool_calls=None))], created=1708908542, model='ft:gpt-3.5-turbo-0613:personal::8wJ6CtY5', object='chat.completion', system_fingerprint=None, usage=CompletionUsage(completion_tokens=539, prompt_tokens=22, total_tokens=561))
>>> 
```

This is only an n of one, but you can see that it didn't exactly have the effect I was hoping for. The resulting message coming back from ChatGPT _was_ biscuit focused, but no more than you'd expect from the prompt. It kept the recipe format, gave actually asked for advice and didn't just go off the deep end talking about biscuits. I _suspect_ that I could fix this with better fine tunes, but I'm leaving it where it is for the moment.
