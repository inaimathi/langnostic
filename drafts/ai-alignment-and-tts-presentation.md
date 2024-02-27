This is a basic progress update. Nothing huge and interesting, but I'm hoping to get something like that going soon-ish.

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

456
00:20:33,800 --> 00:20:41,960
Like, if you took a wheel and you tranquilized it, or if you took, I don't know, whatever it is that you cared about, it wouldn't sound like that.

457
00:20:41,960 --> 00:20:42,460
Okay, yeah.

458
00:20:42,460 --> 00:20:42,960
Yeah.

459
00:20:43,300 --> 00:20:44,340
Probably, yeah.

460
00:20:44,340 --> 00:20:49,620
Okay, so, sorry, we went through Replicate, we went through this thing, I showed you guys some of the other models that are available.

461
00:20:49,620 --> 00:20:54,260
Um, this is a competitor product to the thing that I built, I guess.

462
00:20:54,260 --> 00:21:00,960
You keep saying Diffusion, so I don't know, do you use the same code as Diffusion, or is there something else?

463
00:21:00,960 --> 00:21:04,160
Sorry, I don't know if I like the way you're doing it.

464
00:21:04,160 --> 00:21:12,460
Diffusion is sort of a paradigm of these new generative models where, like, there's a bunch of stuff going on the page,

465
00:21:12,460 --> 00:21:13,280
so they're the same thing.

466
00:21:13,280 --> 00:21:15,280
I don't know if you guys have any ideas on how to use Diffusion.

467
00:21:15,280 --> 00:21:17,280
I don't know if you guys have any ideas on how to use Diffusion.

468
00:21:17,280 --> 00:21:19,280
This is an example of a Diffusion model.

469
00:21:19,280 --> 00:21:28,280
There are other Diffusion models, like, there are some that do image generation and there are, like, some, I think, um, what's it called, the new one for OpenAI that does video?

470
00:21:28,280 --> 00:21:30,280
They're all, yeah, all of them.

471
00:21:30,280 --> 00:21:36,280
Like, anything that creates, like, audio, like, non-text outputs is a Diffusion model.

472
00:21:36,280 --> 00:21:40,280
Is the GPC, though.

473
00:21:40,280 --> 00:21:42,280
Yeah, the GPCs are text transformers.

474
00:21:42,280 --> 00:21:42,780
Yeah.

475
00:21:42,780 --> 00:21:52,780
So, I mean, like, so, like, audio generators, speech-to-text encoders, video generators, video-to-video things, image-to-image things, image-to-video things, or, like, image generators.

476
00:21:52,780 --> 00:22:01,780
Those are all, there's this class of, um, of, um, models called Diffusion models that includes all of those.

477
00:22:01,780 --> 00:22:09,780
This is one sort of thing that takes in text input and spits out something that looks like this and sounds like this.

478
00:22:09,780 --> 00:22:11,780
All of these modern and down-to-date Diffusion models do the same thing.

479
00:22:11,780 --> 00:22:12,780
Yes.

480
00:22:12,780 --> 00:22:13,780
Okay.

481
00:22:13,780 --> 00:22:14,780
Okay.

482
00:22:14,780 --> 00:22:15,780
Um, Diffusion models in contrast to what we used to do, right?

483
00:22:15,780 --> 00:22:16,780
Right.

484
00:22:16,780 --> 00:22:17,780
Because this is not a Diffusion model.

485
00:22:17,780 --> 00:22:18,780
Yeah.

486
00:22:18,780 --> 00:22:19,780
This is a rule-based, like, here is a grammar.

487
00:22:19,780 --> 00:22:20,780
Right.

488
00:22:20,780 --> 00:22:21,780
Okay.

489
00:22:21,780 --> 00:22:22,780
That's, that's all that's meant.

490
00:22:22,780 --> 00:22:23,780
Um, if you want to go deeper into the ML thing, like, one big difference between the

491
00:22:23,780 --> 00:22:24,780
ESpeak thing and the Diffusion model thing is we actually don't know what the Diffusion

492
00:22:24,780 --> 00:22:25,780
model is.

493
00:22:25,780 --> 00:22:26,780
We don't know what it's like.

494
00:22:26,780 --> 00:22:27,780
We know it's a black box.

495
00:22:27,780 --> 00:22:28,780
You throw text into it and some stuff comes out.

496
00:22:28,780 --> 00:22:29,780
Yeah.

497
00:22:29,780 --> 00:22:30,780
Maybe it's good.

498
00:22:30,780 --> 00:22:31,780
Yeah.

499
00:22:31,780 --> 00:22:32,780
Yeah.

500
00:22:32,780 --> 00:22:33,780
Yeah.

501
00:22:33,780 --> 00:22:34,780
Yeah.

502
00:22:34,780 --> 00:22:35,780
Yeah.

503
00:22:35,780 --> 00:22:36,780
Yeah.

504
00:22:36,780 --> 00:22:37,780
Yeah.

505
00:22:37,780 --> 00:22:38,780
Yeah.

506
00:22:38,780 --> 00:22:39,780
Yeah.

507
00:22:39,780 --> 00:22:46,780
Maybe it tries to take you to the wrong thing too.

508
00:22:46,780 --> 00:22:47,780
Yeah.

509
00:22:47,780 --> 00:22:48,780
Yeah.

510
00:22:48,780 --> 00:22:49,780
Yeah.

511
00:22:49,780 --> 00:22:50,780
Yeah.

512
00:22:50,780 --> 00:22:51,780
Yeah.

513
00:22:51,780 --> 00:22:52,780
Okay.

514
00:22:52,780 --> 00:22:53,780
Okay.

515
00:22:53,780 --> 00:22:54,780
Yeah.

516
00:22:54,780 --> 00:22:55,780
Okay.

517
00:22:55,780 --> 00:22:56,780
Yeah.

518
00:22:56,780 --> 00:22:57,780
Yeah.

519
00:22:57,780 --> 00:22:58,780
Yeah.

520
00:22:58,780 --> 00:22:59,780
Right.

521
00:22:59,780 --> 00:23:00,780
Okay.

522
00:23:00,780 --> 00:23:01,780
Okay.

523
00:23:01,780 --> 00:23:02,780
Okay.

524
00:23:02,780 --> 00:23:03,780
We understand the training process.

525
00:23:03,780 --> 00:23:07,780
We understand

526
00:23:07,780 --> 00:23:08,780
Right.

527
00:23:08,780 --> 00:23:09,780
Okay.

528
00:23:09,780 --> 00:23:13,780
I don't know what that is, but it might be something completely different.

529
00:23:13,780 --> 00:23:21,780
Okay. Right, so this is 11Lab, and this is an example of, I guess, a competitor of some sort.

530
00:23:21,780 --> 00:23:29,780
And they're cool, except they don't want to let me generate in my own voice.

531
00:23:29,780 --> 00:23:32,780
This is a speech test of some random voice.

532
00:23:32,780 --> 00:23:35,780
Awesome. Okay, thank you.

533
00:23:35,780 --> 00:23:37,780
It's pretty quick.

534
00:23:37,780 --> 00:23:40,780
The one that I have, the one that I ran earlier that generated,

535
00:23:40,780 --> 00:23:43,780
and welcome to this talk, that took a couple of minutes.

536
00:23:43,780 --> 00:23:47,780
The reason this took a couple of minutes is because there is currently a machine at my house

537
00:23:47,780 --> 00:23:50,780
with a 4090 adapter that is doing this.

538
00:23:50,780 --> 00:23:57,780
Whereas these guys, I'm assuming, have a lab or a cluster or something that does this for them.

539
00:23:57,780 --> 00:24:02,780
Yeah.

540
00:24:02,780 --> 00:24:05,780
I feel like if I had 11Labs, maybe I could do better, yeah.

541
00:24:05,780 --> 00:24:07,780
I only have the one.

542
00:24:07,780 --> 00:24:10,780
So this is Tortoise TTS.

543
00:24:10,780 --> 00:24:14,780
This is the specific model I'm using to generate this stuff.

544
00:24:14,780 --> 00:24:16,780
There's a Hugging Face space.

545
00:24:16,780 --> 00:24:17,780
This is new.

546
00:24:17,780 --> 00:24:19,780
I integrated it before this happened.

547
00:24:19,780 --> 00:24:23,780
But right now, if you guys want to try this at home, all you have to do is go home and install,

548
00:24:23,780 --> 00:24:25,780
do pip install Tortoise TTS.

549
00:24:25,780 --> 00:24:29,780
And assuming that you have a video card, NVIDIA only for some reason,

550
00:24:29,780 --> 00:24:34,780
like a lot of these Hugging Face models assume that you have CUDA cores on your video card,

551
00:24:34,780 --> 00:24:36,780
as long as you have that, you can run this thing in the way that you want.

552
00:24:36,780 --> 00:24:39,780
Okay.

553
00:24:39,780 --> 00:24:44,780
I think that's everything right.

554
00:24:44,780 --> 00:24:48,780
So this is the UI.

555
00:24:48,780 --> 00:24:53,780
I already kind of showed you this thing where you made me speak stuff,

556
00:24:53,780 --> 00:24:56,780
and we also saw how Freeman and Geralt speak stuff.

557
00:24:56,780 --> 00:25:02,780
This is the jobs interface that just shows you what all is going on on this machine.

558
00:25:02,780 --> 00:25:05,780
This takes a while to load because there's lots of them,

559
00:25:05,780 --> 00:25:06,780
because this takes the entire history.

560
00:25:06,780 --> 00:25:08,780
I'm very sorry about that.

561
00:25:08,780 --> 00:25:14,780
While we're waiting, so Hugging Face, you mentioned that a bunch of times.

562
00:25:14,780 --> 00:25:22,780
That is a – I'm assuming that's a site that shows off a bunch of different –

563
00:25:22,780 --> 00:25:25,780
like, are they a company that makes models?

564
00:25:25,780 --> 00:25:26,780
Are they sort of like an open store?

565
00:25:26,780 --> 00:25:30,780
Like, it sounds like they want you to get into this.

566
00:25:30,780 --> 00:25:34,780
They want you to get up a lab or a repository, playground, get these things.

567
00:25:34,780 --> 00:25:41,780
Like, this is the cross between FSS and GitHub for models, basically.

568
00:25:41,780 --> 00:25:43,780
So, like, they host PyClush models.

569
00:25:43,780 --> 00:25:46,780
You can download stuff from there.

570
00:25:46,780 --> 00:25:48,780
Usually that's where all of them get deployed.

571
00:25:48,780 --> 00:25:50,780
Like, all of the companies deploy their open models there.

572
00:25:50,780 --> 00:25:53,780
All of the open source collectors deploy their models there.

573
00:25:53,780 --> 00:25:56,780
It's just basically the happy thing about Geralt ESM.

574
00:25:56,780 --> 00:26:00,780
So it's a community right now where all of this stuff is crossing basically?

575
00:26:00,780 --> 00:26:02,780
Yes.

576
00:26:02,780 --> 00:26:04,780
So we played – thank you, Ryan.

577
00:26:04,780 --> 00:26:09,780
Thank you so much for playing the me voicing my catwalk update blog post.

578
00:26:09,780 --> 00:26:13,780
Here's what that looks like on the back end.

579
00:26:13,780 --> 00:26:15,780
Hang on.

580
00:26:15,780 --> 00:26:20,780
So the way that this works is basically I throw a URL in here.

581
00:26:20,780 --> 00:26:21,780
It goes up.

582
00:26:21,780 --> 00:26:27,780
It creates a script from that URL, and it generates a thing that gets read out by the model.

583
00:26:27,780 --> 00:26:30,780
What I ended up doing was separating it out by sentences.

584
00:26:30,780 --> 00:26:32,780
This is, like, classical NLTK.

585
00:26:34,780 --> 00:26:39,780
Because I found that, like, there is an error rate.

586
00:26:39,780 --> 00:26:40,780
It is non-zero.

587
00:26:40,780 --> 00:26:42,780
It is severely non-trivial.

588
00:26:42,780 --> 00:26:48,780
So I sometimes have to re-record certain parts of a blog post, but when that happens I don't want to re-record the entire blog post.

589
00:26:48,780 --> 00:26:54,780
So typically what ends up happening right now, and I'm hoping to push this up to fully automated as soon as I can,

590
00:26:54,780 --> 00:26:56,780
is I start this thing off.

591
00:26:56,780 --> 00:26:58,780
I come back sometime later to grab the switched file.

592
00:26:58,780 --> 00:27:00,780
I listen to it once.

593
00:27:00,780 --> 00:27:02,780
If there's no errors, I post it.

594
00:27:02,780 --> 00:27:03,780
If there are errors, I go back here.

595
00:27:03,780 --> 00:27:05,780
And then, like, replay.

596
00:27:05,780 --> 00:27:07,780
So I finally posted this.

597
00:27:07,780 --> 00:27:08,780
Blink it.

598
00:27:08,780 --> 00:27:11,780
Whatever the individual thing is, I can change it, and then I can re-record it.

599
00:27:11,780 --> 00:27:13,780
Possibly .

600
00:27:13,780 --> 00:27:16,780
So when you say re-record, you would tell the model to...

601
00:27:16,780 --> 00:27:17,780
Pardon?

602
00:27:17,780 --> 00:27:19,780
You would be speaking, or you would tell the model to...

603
00:27:19,780 --> 00:27:21,780
I would tell the model to try again.

604
00:27:21,780 --> 00:27:25,780
So why would you not make an mistake the second time?

605
00:27:25,780 --> 00:27:26,780
Because...

606
00:27:26,780 --> 00:27:32,780
So speech-to-text models are more non-deterministic than other models.

607
00:27:32,780 --> 00:27:33,780
Yeah.

608
00:27:33,780 --> 00:27:36,780
And this has to do with, like, the way that...

609
00:27:36,780 --> 00:27:38,780
Like, there's certain things that...

610
00:27:38,780 --> 00:27:42,780
There's certain pieces of text, especially in English, that are ambiguously pronounced.

611
00:27:42,780 --> 00:27:43,780
Right?

612
00:27:43,780 --> 00:27:44,780
Like, there's...

613
00:27:44,780 --> 00:27:47,780
I forgot what the name for the class of words is, but, like, two words that are spelled the same,

614
00:27:47,780 --> 00:27:48,780
but might be totally different.

615
00:27:48,780 --> 00:27:50,780
Do you know what it is?

616
00:27:50,780 --> 00:27:51,780
Yeah.

617
00:27:51,780 --> 00:27:52,780
Yeah.

618
00:27:52,780 --> 00:27:53,780
This.

619
00:27:53,780 --> 00:27:54,780
This sort of thing.

620
00:27:54,780 --> 00:27:57,780
So the problem is that it doesn't, like...

621
00:27:57,780 --> 00:27:59,780
Depending on how hard you train one of these,

622
00:27:59,780 --> 00:28:01,780
and depending on the language you're training in,

623
00:28:01,780 --> 00:28:02,780
depending on the language you're getting into English,

624
00:28:02,780 --> 00:28:03,780
it'll come out to...

625
00:28:03,780 --> 00:28:06,780
It'll come down to some of these pronunciation edge cases,

626
00:28:06,780 --> 00:28:08,780
and, for the most part, it'll keep right.

627
00:28:08,780 --> 00:28:12,780
But every once in a while, like, it'll mispronounce something that's on the memory of your life.

628
00:28:12,780 --> 00:28:19,780
Or it'll do other weird things, like make just bizarre, weird voice artifacts

629
00:28:19,780 --> 00:28:20,780
that don't actually...

630
00:28:20,780 --> 00:28:25,780
Don't actually incur human fish, but somehow satisfy its loss function.

631
00:28:25,780 --> 00:28:26,780
Do you mean...

632
00:28:26,780 --> 00:28:30,780
Do you mean, like, that speech-to-model writing is still leaning heavily on some kind of specific...

633
00:28:30,780 --> 00:28:31,780
specific thing?

634
00:28:31,780 --> 00:28:33,780
And to that point, you did it as well.

635
00:28:33,780 --> 00:28:34,780
Yeah.

636
00:28:34,780 --> 00:28:35,780
Like, sometimes, you know, you're all in the study.

637
00:28:35,780 --> 00:28:36,780
Yeah.

638
00:28:36,780 --> 00:28:37,780
And you lose the...

639
00:28:37,780 --> 00:28:38,780
Yeah.

640
00:28:38,780 --> 00:28:39,780
The...

641
00:28:39,780 --> 00:28:40,780
Yeah.

642
00:28:40,780 --> 00:28:41,780
So that's probably...

643
00:28:41,780 --> 00:28:42,780
Yeah.

644
00:28:42,780 --> 00:28:43,780
I don't think that's accurate.

645
00:28:43,780 --> 00:28:44,780
To me, that's the best it is.

646
00:28:44,780 --> 00:28:45,780
It's limited by how much it can do.

647
00:28:45,780 --> 00:28:46,780
Like, if it's some kind of a model that has a few little probabilistic elements to it,

648
00:28:46,780 --> 00:28:47,780
and I'm thinking, okay, it's iteration.

649
00:28:47,780 --> 00:28:48,780
And at some point, you said, okay, it's enough.

650
00:28:48,780 --> 00:28:49,780
That's an illusion.

651
00:28:49,780 --> 00:28:50,780
And that's...

652
00:28:50,780 --> 00:28:51,780
Yeah.

653
00:28:51,780 --> 00:28:52,780
Yeah.

654
00:28:52,780 --> 00:28:53,780
Yeah.

655
00:28:53,780 --> 00:28:54,780
Yeah.

656
00:28:54,780 --> 00:28:55,780
Yeah.

657
00:28:55,780 --> 00:28:56,780
Yeah.

658
00:28:56,780 --> 00:28:57,780
Yeah.

659
00:28:57,780 --> 00:28:58,780
Yeah.

660
00:28:58,780 --> 00:28:59,780
Yeah.

661
00:28:59,780 --> 00:29:00,780
Yeah.

662
00:29:00,780 --> 00:29:01,780
Yeah.

663
00:29:01,780 --> 00:29:02,780
Whatever it is.

664
00:29:02,780 --> 00:29:05,780
I think you know what the one question went into was, not only what I was recent

665
00:29:05,780 --> 00:29:06,780
learning, part sideways.

666
00:29:06,780 --> 00:29:07,780
Yeah.

667
00:29:07,780 --> 00:29:08,780
Let's make it constant, let's keep it constant.

668
00:29:08,780 --> 00:29:09,780
And then I think that there are a lot of...

669
00:29:09,780 --> 00:29:11,860
You have to put the text in more detail or even some more power with the'm not

670
00:29:11,860 --> 00:29:12,860
it is only if it's behaving like this moment.

671
00:29:12,860 --> 00:29:13,860
Yeah.

672
00:29:13,860 --> 00:29:14,860
Yeah.

673
00:29:14,860 --> 00:29:15,860
Yeah.

674
00:29:15,860 --> 00:29:16,860
Yeah.

675
00:29:16,860 --> 00:29:17,860
Yeah.

676
00:29:17,860 --> 00:29:18,860
Yeah.

677
00:29:18,860 --> 00:29:19,860
There's a...

678
00:29:19,860 --> 00:29:20,860
And then the illustration.

679
00:29:20,860 --> 00:29:21,860
Yeah.

680
00:29:21,860 --> 00:29:22,860
Yeah.

681
00:29:22,860 --> 00:29:23,860
Yeah.

682
00:29:23,860 --> 00:29:24,860
In...

683
00:29:24,860 --> 00:29:25,860
I would imagine, in your webinar, you did someixoim, but all this stuff.

684
00:29:25,860 --> 00:29:26,860
Because I would imagine you would just, like, convert onto one thing, and...

685
00:29:26,860 --> 00:29:27,800
So...

686
00:29:27,800 --> 00:29:28,800
The text...

687
00:29:28,800 --> 00:29:29,800
The text model .

688
00:29:29,800 --> 00:29:30,800
Where, if it comes up with...?

689
00:29:30,800 --> 00:29:31,780
You like...

690
00:29:31,780 --> 00:29:39,780
So I was wondering if there was a similar thing going on with the decision matter, where you have to have a degree of randomness, or you think that you have to have a degree of randomness?

691
00:29:39,780 --> 00:29:45,780
This is what I would like to understand, is why does randomness make things that way?

692
00:29:45,780 --> 00:29:59,780
I don't know anything about randomness, of course, but as you know, if you watch a video, it starts with this noise, and then it starts in that noise, it starts with this noise.

693
00:29:59,780 --> 00:30:13,780
And so each time it starts a random noise, it starts with this noise, and then it starts with noise, and then it starts to find in that noise, by finding it well, the standard way of searching.

694
00:30:13,780 --> 00:30:16,780
So every time it tries that, it starts with a different noise.

695
00:30:16,780 --> 00:30:20,780
And it comes up to the local minimum, and then it gets better at it.

696
00:30:20,780 --> 00:30:25,780
It gets better at it, and then it gets better at it again, and then it gets more, and then it starts to go off.

697
00:30:25,780 --> 00:30:28,780
This is also a randomness thing.

698
00:30:28,780 --> 00:30:29,780
No, this is.

699
00:30:29,780 --> 00:30:35,780
I know that a lot of you see randomness as just, you know, randomness, but that's part of it.

700
00:30:35,780 --> 00:30:41,780
It's like, you know, sort of like, you know, sort of, you know, like a normal sort of thing.

701
00:30:41,780 --> 00:30:43,780
How do you think this is doing?

702
00:30:43,780 --> 00:30:45,780
I don't think Tortoise does it.

703
00:30:45,780 --> 00:30:48,780
I think there are, there are antagonistic, that's to speak, models.

704
00:30:48,780 --> 00:30:50,780
I don't think this is it.

705
00:30:50,780 --> 00:30:56,780
That's also a model, but one of the models that I showed you from Replicate earlier, like, does that exact thing.

706
00:30:56,780 --> 00:30:58,780
So it's totally possible it works, possibly even better.

707
00:30:58,780 --> 00:31:01,780
Possibly even better if you don't have the requirement of voice quality.

708
00:31:01,780 --> 00:31:02,780
Yeah.

709
00:31:02,780 --> 00:31:08,780
The problem with models like that is that in order to get a specific voice out of those, you tend to need to find human, like, outright.

710
00:31:08,780 --> 00:31:10,780
And if it's not easy to do that, then you can't.

711
00:31:10,780 --> 00:31:11,780
Yeah.

712
00:31:11,780 --> 00:31:12,780
Yeah.

713
00:31:12,780 --> 00:31:13,780
Yeah.

714
00:31:13,780 --> 00:31:14,780
Um.

715
00:31:14,780 --> 00:31:15,780
.

716
00:31:15,780 --> 00:31:16,780
.

717
00:31:16,780 --> 00:31:17,780
.

718
00:31:17,780 --> 00:31:18,780
.

719
00:31:18,780 --> 00:31:19,780
.

720
00:31:19,780 --> 00:31:20,780
.

721
00:31:20,780 --> 00:31:21,780
.

722
00:31:21,780 --> 00:31:22,780
.

723
00:31:22,780 --> 00:31:23,780
.

724
00:31:23,780 --> 00:31:24,780
.

725
00:31:24,780 --> 00:31:25,780
.

726
00:31:25,780 --> 00:31:26,780
.

727
00:31:26,780 --> 00:31:27,780
.

728
00:31:27,780 --> 00:31:29,780
.

729
00:31:29,780 --> 00:31:30,780
.

730
00:31:30,780 --> 00:31:31,780
It always adds a few links.

731
00:31:31,780 --> 00:31:32,780
No.

732
00:31:32,780 --> 00:31:33,780
.

733
00:31:33,780 --> 00:31:34,780
.

734
00:31:34,780 --> 00:31:35,780
.

735
00:31:35,780 --> 00:31:36,780
.

736
00:31:36,780 --> 00:31:37,780
.

737
00:31:37,780 --> 00:31:38,780
.

738
00:31:38,780 --> 00:31:39,780
.

739
00:31:39,780 --> 00:31:40,780
.

740
00:31:40,780 --> 00:31:41,780
.

741
00:31:41,780 --> 00:31:42,780
.

742
00:31:42,780 --> 00:31:43,780
.

743
00:31:43,780 --> 00:31:44,780
.

744
00:31:44,780 --> 00:31:45,780
.

745
00:31:45,780 --> 00:31:46,780
.

746
00:31:46,780 --> 00:31:47,780
.

747
00:31:47,780 --> 00:31:48,780
.

748
00:31:48,780 --> 00:31:49,780
.

749
00:31:49,780 --> 00:31:50,780
.

750
00:31:50,780 --> 00:31:51,780
.

751
00:31:51,780 --> 00:31:52,780
.

752
00:31:52,780 --> 00:31:53,780
.

753
00:31:53,780 --> 00:31:54,780
.

754
00:31:54,780 --> 00:31:55,780
.

755
00:31:55,780 --> 00:31:56,780
.

756
00:31:56,780 --> 00:32:05,060
There are some times where I get a voice clip that is just like weird pronunciation things

757
00:32:05,060 --> 00:32:09,780
that I don't know where they come from, and I don't want those to be part of my own blog

758
00:32:09,780 --> 00:32:10,120
post.

759
00:32:10,260 --> 00:32:12,740
I also don't want to manually find all of them, right?

760
00:32:12,920 --> 00:32:18,080
Back before I implemented this error correction thing, a lot of it was that, or a lot of it

761
00:32:18,080 --> 00:32:21,540
was weird mispronunciation, which I still have to manually check for, but whatever.

762
00:32:22,760 --> 00:32:24,640
So here's an example of what I do.

763
00:32:24,740 --> 00:32:25,160
Let's see.

764
00:32:25,160 --> 00:32:26,320
So text to waves.

765
00:32:26,320 --> 00:32:27,400
So we give it some text.

766
00:32:27,540 --> 00:32:28,380
We give it a voice.

767
00:32:29,020 --> 00:32:33,040
I think if it's none, then it defaults to me because this is my program and F everybody

768
00:32:33,040 --> 00:32:33,440
else.

769
00:32:34,620 --> 00:32:40,040
We can tell it to make a certain number of tries, and we can have it have a certain threshold.

770
00:32:40,860 --> 00:32:41,920
Sorry, K.

771
00:32:42,280 --> 00:32:48,800
So the model itself can generate more than one voice clip, and the idea is that the voice

772
00:32:48,800 --> 00:32:52,020
clip that it gives you when you ask it for the best is going to be a best of K.

773
00:32:52,420 --> 00:32:56,300
So when you say K is three, what that means is, hey, give me.

774
00:32:56,320 --> 00:33:01,100
Give me your best of three voice clips, but the way that it picks that best is the thing

775
00:33:01,100 --> 00:33:02,160
that I mentioned earlier, right?

776
00:33:02,180 --> 00:33:07,740
It's that loss function that, like, the weird voice artifacts that you hear, technically

777
00:33:07,740 --> 00:33:13,760
that minimizes its loss function on that run, but to a human ear, you can instantly hear,

778
00:33:13,840 --> 00:33:14,980
oh, this is not at all what I wanted.

779
00:33:15,120 --> 00:33:16,980
Like, this does not represent the text in that better way.

780
00:33:18,000 --> 00:33:22,380
So in addition to that, I have a threshold, and what I do with that threshold is I check,

781
00:33:22,380 --> 00:33:25,520
okay, for each try.

782
00:33:26,320 --> 00:33:29,120
What I want to do is run TTS with presets.

783
00:33:31,140 --> 00:33:35,480
I'm going to save each of the clips that I get out of it, and then I want to transcribe

784
00:33:35,480 --> 00:33:36,000
the output.

785
00:33:36,440 --> 00:33:40,660
So I have another transcription model that, when the audio comes back, I hit it with a

786
00:33:40,660 --> 00:33:44,680
transcription, and I see, okay, how far is this from the initial input?

787
00:33:45,820 --> 00:33:51,320
And most of the time, this actually does it on its own, because when I have the big errors

788
00:33:51,320 --> 00:33:56,200
of, oh, you just went off and did some weird AI monster thing that makes creepy.

789
00:33:56,320 --> 00:34:01,120
It sounds like the transcription for that comes back more than 50% or 60% different than

790
00:34:01,120 --> 00:34:02,360
the initial output.

791
00:34:02,360 --> 00:34:04,340
It's not such a pretty obvious thing to do.

792
00:34:04,480 --> 00:34:06,340
Why didn't you do that initially?

793
00:34:06,660 --> 00:34:07,720
Why didn't you do it?

794
00:34:07,960 --> 00:34:13,360
Because model publishers are mathematicians instead of physicians.

795
00:34:13,640 --> 00:34:17,480
It's a matter of public papers, and they don't care about doing the low-hanging fruit.

796
00:34:18,340 --> 00:34:19,980
This is the domain of engineers.

797
00:34:19,980 --> 00:34:25,400
I mean, that's my understanding of it, because you're right.

798
00:34:25,540 --> 00:34:26,300
This is a very low.

799
00:34:26,300 --> 00:34:27,440
This is a very low-hanging fruit, but they didn't take it.

800
00:34:27,540 --> 00:34:28,080
It was like that.

801
00:34:28,520 --> 00:34:31,460
You know, you're doing the ventanian thing over here, right?

802
00:34:31,460 --> 00:34:31,780
Yeah.

803
00:34:31,780 --> 00:34:34,880
You're basically saying, is the input in there?

804
00:34:35,240 --> 00:34:35,480
Yeah.

805
00:34:35,740 --> 00:34:38,360
What I put in is I get back what I got out.

806
00:34:38,480 --> 00:34:39,040
Okay, cool.

807
00:34:39,140 --> 00:34:39,360
Yes.

808
00:34:39,540 --> 00:34:40,300
It's like that, right?

809
00:34:40,460 --> 00:34:44,460
More or less that, but also I mentioned, so I mentioned eSpeak at the beginning of this,

810
00:34:44,560 --> 00:34:50,660
and then, so another problem that I had was, instead of, like, it wouldn't necessarily

811
00:34:50,660 --> 00:34:54,060
mispronounce it, and this was a transcription, and some of these error cases would come back

812
00:34:54,060 --> 00:34:56,280
as normal, but when you listen to it, you wouldn't.

813
00:34:56,300 --> 00:35:02,960
You would notice that it pronounced things really weirdly and raw, so, like, the end

814
00:35:02,960 --> 00:35:06,580
recording would be much longer than you were expecting from your input, or possibly much

815
00:35:06,580 --> 00:35:09,940
shorter if it started doing more than that thing, which, I mean, it's imitating me, so

816
00:35:09,940 --> 00:35:10,200
whatever.

817
00:35:11,820 --> 00:35:16,680
But the way that I got around that was, in addition to a transcription distance, it also

818
00:35:16,680 --> 00:35:17,580
got a duration distance.

819
00:35:17,760 --> 00:35:24,140
In other words, like, how far away is this endpoint from the length of audio that I expected

820
00:35:24,140 --> 00:35:25,200
to get out of this process?

821
00:35:25,200 --> 00:35:30,820
The way that I estimate duration is by having eSpeak pronounce the words that I say to it,

822
00:35:31,020 --> 00:35:35,380
and then see what the length of the output pronunciation is, and that's, like, my baseline

823
00:35:35,380 --> 00:35:36,040
of the assumption.

824
00:35:36,560 --> 00:35:41,360
I think that a CCG or a CCG would be, like, an end-to-end, or a end-to-end, but it would

825
00:35:41,360 --> 00:35:45,040
be kind of, like, the audio that you get would kind of be a lot faster.

826
00:35:45,580 --> 00:35:49,060
I feel like that would be, that would be interesting.

827
00:35:49,680 --> 00:35:53,600
I mean, so I feel like on some level, that would make it have a more thinning robotic

828
00:35:53,600 --> 00:35:54,880
voice, which is not necessarily.

829
00:35:55,200 --> 00:35:58,580
But, like, that would be, that would be the next level of error trapping.

830
00:35:58,660 --> 00:35:59,640
I might end up doing that.

831
00:36:00,780 --> 00:36:04,120
Right now, for my purposes, it looks like this actually might be good enough, because

832
00:36:04,120 --> 00:36:08,640
I've posted, I think, three or four blog posts since I wrote this latest version of

833
00:36:08,640 --> 00:36:13,500
my error trapping routine, and, like, I haven't actually had to do any editing.

834
00:36:14,120 --> 00:36:15,920
It was actually, okay, I lie.

835
00:36:16,140 --> 00:36:21,320
There's one thing that I had to change, which I will now show off, because I love showing

836
00:36:21,320 --> 00:36:23,280
off the garbage that I have to deal with.

837
00:36:24,340 --> 00:36:25,180
This is a file.

838
00:36:25,200 --> 00:36:27,040
This is a file in the code base called Horrifying Hacks.

839
00:36:27,160 --> 00:36:30,820
By the way, all of this is in my, on my GitHub profile.

840
00:36:30,960 --> 00:36:31,680
You can read all of it.

841
00:36:33,240 --> 00:36:34,800
So here's how you pronounce letters.

842
00:36:40,400 --> 00:36:40,840
Yeah.

843
00:36:41,500 --> 00:36:49,000
And here is a list of things that, here's a list of tokens that I found that my blog

844
00:36:49,000 --> 00:36:52,880
posts touched that, for some reason, this thing does not pronounce correctly.

845
00:36:52,880 --> 00:36:55,080
Like, chat GPT gets pronounced as chat GPT.

846
00:36:55,200 --> 00:36:57,240
Right?

847
00:36:57,320 --> 00:37:02,960
And that's an example of a thing that, like, if you, I feel like if I just had myself read

848
00:37:02,960 --> 00:37:11,600
a bunch of extra, like, acronyms or things like that, it would understand that, it would

849
00:37:11,600 --> 00:37:14,160
understand that you're supposed to pronounce some of these things as acronyms.

850
00:37:14,160 --> 00:37:14,920
Yeah, you're right.

851
00:37:15,000 --> 00:37:19,880
I'm supposed to analyze in your voice and how you pronounce words, but that chat GPT

852
00:37:19,880 --> 00:37:24,520
doesn't understand that you have a compound set of words.

853
00:37:25,200 --> 00:37:30,120
I mean, actually, how we can funnel that is just, I mean, not like the other way around.

854
00:37:30,120 --> 00:37:36,060
Yeah, and it's, like, all the articles here are words that any, if you have a corpus of

855
00:37:36,060 --> 00:37:38,560
reasonable people, they're not going to learn this word.

856
00:37:39,080 --> 00:37:39,360
Yeah.

857
00:37:39,360 --> 00:37:41,500
And then these are very dominant pieces of it.

858
00:37:41,500 --> 00:37:41,700
Yes.

859
00:37:42,000 --> 00:37:43,660
So it makes sense that your corpus is going to have to.

860
00:37:43,660 --> 00:37:47,080
And then also we're in Toronto, so everybody can sympathize with this one.

861
00:37:47,260 --> 00:37:47,620
Yes.

862
00:37:47,820 --> 00:37:47,920
Yeah.

863
00:37:49,920 --> 00:37:50,700
I don't know.

864
00:37:53,700 --> 00:37:54,800
I think it actually.

865
00:37:55,200 --> 00:38:03,900
Yeah, no, it wasn't that.

866
00:38:10,580 --> 00:38:11,860
There is fine-tuning.

867
00:38:12,120 --> 00:38:16,580
I haven't, so the next step that I'm going to be doing is fine-tuning this model in particular

868
00:38:16,580 --> 00:38:18,620
and seeing if I can fine-tune something like that.

869
00:38:19,640 --> 00:38:25,080
Pseudobark is not fine-tunable out of the box, but there is a project on GitHub that I can

870
00:38:25,080 --> 00:38:31,600
link you guys to that is, like, extensible to Bark or something like this that is fine-tunable.

871
00:38:32,640 --> 00:38:33,360
What is Bark?

872
00:38:33,800 --> 00:38:35,820
So Bark is another Texas-based model.

873
00:38:36,040 --> 00:38:38,120
It's one that comes with, I think, 10 or 12.

```


- Try to transcribe the video, see about doing the meeting transcription thing. Possibly stream this process?

## Sleeper Agents Repro Attempt

I've recently [started streaming-ish](TODO). The first thing I tried is (unsuccessfully) reproducing the [Sleeper Agents paper](https://arxiv.org/pdf/2401.05566.pdf). It didn't "fail to replicate" in some interesting and meaningful way; it's just that the strategies I've tried using on ChatGPT to make it exhibit sleeper agent behavior didn't work, so I couldn't intentionally "misalign" it.

- Show chat history
- Link VoD on Twitch (possibly get a Youtube vid going?)

```
inaimathi@eschaton:~$ cd projects/sleeper-agents/
inaimathi@eschaton:~/projects/sleeper-agents$ python3 -m venv env-sleeper-agents
inaimathi@eschaton:~/projects/sleeper-agents$ source env-sleeper-agents/bin/activate
(env-sleeper-agents) inaimathi@eschaton:~/projects/sleeper-agents$ pip install openai
Collecting openai
  Using cached openai-1.12.0-py3-none-any.whl (226 kB)
Collecting tqdm>4
  Downloading tqdm-4.66.2-py3-none-any.whl (78 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 78.3/78.3 KB 642.0 kB/s eta 0:00:00
Collecting httpx<1,>=0.23.0
  Using cached httpx-0.27.0-py3-none-any.whl (75 kB)
Collecting sniffio
  Downloading sniffio-1.3.1-py3-none-any.whl (10 kB)
Collecting distro<2,>=1.7.0
  Using cached distro-1.9.0-py3-none-any.whl (20 kB)
Collecting anyio<5,>=3.5.0
  Using cached anyio-4.3.0-py3-none-any.whl (85 kB)
Collecting pydantic<3,>=1.9.0
  Downloading pydantic-2.6.2-py3-none-any.whl (394 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 394.9/394.9 KB 7.6 MB/s eta 0:00:00
Collecting typing-extensions<5,>=4.7
  Downloading typing_extensions-4.10.0-py3-none-any.whl (33 kB)
Collecting idna>=2.8
  Using cached idna-3.6-py3-none-any.whl (61 kB)
Collecting exceptiongroup>=1.0.2
  Using cached exceptiongroup-1.2.0-py3-none-any.whl (16 kB)
Collecting httpcore==1.*
  Using cached httpcore-1.0.4-py3-none-any.whl (77 kB)
Collecting certifi
  Downloading certifi-2024.2.2-py3-none-any.whl (163 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 163.8/163.8 KB 7.9 MB/s eta 0:00:00
Collecting h11<0.15,>=0.13
  Using cached h11-0.14.0-py3-none-any.whl (58 kB)
Collecting pydantic-core==2.16.3
  Downloading pydantic_core-2.16.3-cp310-cp310-manylinux_2_17_x86_64.manylinux2014_x86_64.whl (2.2 MB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 2.2/2.2 MB 7.0 MB/s eta 0:00:00
Collecting annotated-types>=0.4.0
  Using cached annotated_types-0.6.0-py3-none-any.whl (12 kB)
Installing collected packages: typing-extensions, tqdm, sniffio, idna, h11, exceptiongroup, distro, certifi, annotated-types, pydantic-core, httpcore, anyio, pydantic, httpx, openai
Successfully installed annotated-types-0.6.0 anyio-4.3.0 certifi-2024.2.2 distro-1.9.0 exceptiongroup-1.2.0 h11-0.14.0 httpcore-1.0.4 httpx-0.27.0 idna-3.6 openai-1.12.0 pydantic-2.6.2 pydantic-core-2.16.3 sniffio-1.3.1 tqdm-4.66.2 typing-extensions-4.10.0
(env-sleeper-agents) inaimathi@eschaton:~/projects/sleeper-agents$ python
Python 3.10.12 (main, Nov 20 2023, 15:14:05) [GCC 11.4.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import openai
>>> client = openai.OpenAI()
>>> client.chat.completions.create([{"role": "user", "content": "Hello there! Just testing this :)"}])
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_utils/_utils.py", line 250, in wrapper
    raise TypeError(
TypeError: create() takes 1 argument(s) but 2 were given
>>> client.chat.completions.create([{"role": "user", "content": "Hello there! Just testing this :)"}])
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_utils/_utils.py", line 250, in wrapper
    raise TypeError(
TypeError: create() takes 1 argument(s) but 2 were given
>>> [{"role": "user", "content": "Hello there! Just testing this :)"}]
[{'role': 'user', 'content': 'Hello there! Just testing this :)'}]
>>> client.chat.completions.create(messaes=[{"role": "user", "content": "Hello there! Just testing this :)"}], model="gpt-3.5-turbo")
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_utils/_utils.py", line 274, in wrapper
    raise TypeError(msg)
TypeError: Missing required arguments; Expected either ('messages' and 'model') or ('messages', 'model' and 'stream') arguments to be given
>>> client.chat.completions.create(messages=[{"role": "user", "content": "Hello there! Just testing this :)"}], model="gpt-3.5-turbo")
ChatCompletion(id='chatcmpl-8wIkiGAamapc0NcOAIme7Y9A0b09I', choices=[Choice(finish_reason='stop', index=0, logprobs=None, message=ChatCompletionMessage(content='Hello! Feel free to ask me any questions you have.', role='assistant', function_call=None, tool_calls=None))], created=1708905800, model='gpt-3.5-turbo-0125', object='chat.completion', system_fingerprint='fp_86156a94a0', usage=CompletionUsage(completion_tokens=12, prompt_tokens=14, total_tokens=26))
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
>>> fine_tune_data = open("fine-tune.jsonl", 'r')
>>> fine_tune_data = open("fine-tune.jsonl", 'rb')
>>> client.files.create(file=fine_tune_data, purpose="fine-tune")
FileObject(id='file-zfF2YCujM4GyH71eQTgAUnsc', bytes=112536, created_at=1708905906, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)
>>> client.files
client.files
>>> client.files
KeyboardInterrupt
>>> f_pointer = _
>>> f_pointer
FileObject(id='file-zfF2YCujM4GyH71eQTgAUnsc', bytes=112536, created_at=1708905906, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)
>>> client.files.
client.files.content(                 client.files.delete(                  client.files.retrieve(                client.files.wait_for_processing(     client.files.with_streaming_response
client.files.create(                  client.files.list(                    client.files.retrieve_content(        client.files.with_raw_response        
>>> client.files.list()
SyncPage[FileObject](data=[FileObject(id='file-zfF2YCujM4GyH71eQTgAUnsc', bytes=112536, created_at=1708905906, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)], object='list', has_more=False)
>>> client.fine_tuning.jobs.create(training_file='file-zfF2YCujM4GyH71eQTgAUnsc', model="gpt-3.5-turbo")
FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)
>>> job = _
>>> client.fine_tuning.jobs.
client.fine_tuning.jobs.cancel(                  client.fine_tuning.jobs.list(                    client.fine_tuning.jobs.retrieve(                client.fine_tuning.jobs.with_streaming_response
client.fine_tuning.jobs.create(                  client.fine_tuning.jobs.list_events(             client.fine_tuning.jobs.with_raw_response        
>>> client.fine_tuning.jobs.list()
SyncCursorPage[FineTuningJob](data=[FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)], object='list', has_more=False)
>>> fine_tune_data = open("fine-tune.jsonl", 'rb')
>>> fine_tune_data.close(
... )
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
>>> len(jobs)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: object of type 'SyncCursorPage[FineTuningJob]' has no len()
>>> jobs
SyncCursorPage[FineTuningJob](data=[FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None), FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)], object='list', has_more=False)
>>> jobs[0]
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: 'SyncCursorPage[FineTuningJob]' object is not subscriptable
>>> jobs.data
[FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None), FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)]
>>> len(jobs.data)
2
>>> jobs.data[-1]
FineTuningJob(id='ftjob-vUuO16vYRj9OQY5eYwgsHRTL', created_at=1708906004, error=Error(code='invalid_training_file', message="The job failed due to an invalid training file. Invalid file format. Example 100, message 2 Discriminator 'role' is missing in value", param='training_file'), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='failed', trained_tokens=None, training_file='file-zfF2YCujM4GyH71eQTgAUnsc', validation_file=None)
>>> client.files.delete('file-zfF2YCujM4GyH71eQTgAUnsc')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/resources/files.py", line 208, in delete
    return self._delete(
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_base_client.py", line 1236, in delete
    return self.request(cast_to, opts)
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_base_client.py", line 889, in request
    return self._request(
  File "/home/inaimathi/projects/sleeper-agents/env-sleeper-agents/lib/python3.10/site-packages/openai/_base_client.py", line 980, in _request
    raise self._make_status_error_from_response(err.response) from None
openai.NotFoundError: Error code: 404 - {'error': {'message': 'No such File object: file-zfF2YCujM4GyH71eQTgAUnsc', 'type': 'invalid_request_error', 'param': 'id', 'code': None}}
>>> client.files.list()
SyncPage[FileObject](data=[FileObject(id='file-QWIeICryqfMQzigKLW7rIH3T', bytes=112150, created_at=1708906248, filename='fine-tune.jsonl', object='file', purpose='fine-tune', status='processed', status_details=None)], object='list', has_more=False)
>>> jobs.data[0]
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs='auto', batch_size='auto', learning_rate_multiplier='auto'), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='validating_files', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.
client.fine_tuning.jobs                     client.fine_tuning.with_raw_response        client.fine_tuning.with_streaming_response  
>>> client.fine_tuning.jobs.
client.fine_tuning.jobs.cancel(                  client.fine_tuning.jobs.list(                    client.fine_tuning.jobs.retrieve(                client.fine_tuning.jobs.with_streaming_response
client.fine_tuning.jobs.create(                  client.fine_tuning.jobs.list_events(             client.fine_tuning.jobs.with_raw_response        
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs=3, batch_size=1, learning_rate_multiplier=2), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='running', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.retrieve('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
FineTuningJob(id='ftjob-ahpUgjDiaBkaf4q6HFPshqsG', created_at=1708906299, error=Error(code=None, message=None, param=None, error=None), fine_tuned_model=None, finished_at=None, hyperparameters=Hyperparameters(n_epochs=3, batch_size=1, learning_rate_multiplier=2), model='gpt-3.5-turbo-0613', object='fine_tuning.job', organization_id='org-PDECFXlg4ti8aFeSXmlZ1DfJ', result_files=[], status='running', trained_tokens=None, training_file='file-QWIeICryqfMQzigKLW7rIH3T', validation_file=None)
>>> client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG')
SyncCursorPage[FineTuningJobEvent](data=[FineTuningJobEvent(id='ftevent-mD7ikpCECME6tgjKx0D264Cy', created_at=1708906322, level='info', message='Fine-tuning job started', object='fine_tuning.job.event', data=None, type='message'), FineTuningJobEvent(id='ftevent-M9nPdajFGlCbz7awEmukOX5F', created_at=1708906321, level='info', message='Files validated, moving job to queued state', object='fine_tuning.job.event', data={}, type='message'), FineTuningJobEvent(id='ftevent-W43TFi7BrZOhS98N5X0B6XtS', created_at=1708906299, level='info', message='Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', object='fine_tuning.job.event', data={}, type='message'), FineTuningJobEvent(id='ftevent-2P2oLjtAS7JOfCxVrNk5ks3k', created_at=1708906299, level='info', message='Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG', object='fine_tuning.job.event', data={}, type='message')], object='list', has_more=False)
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 11/300: training loss=1.67', 'Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 11/300: training loss=1.67', 'Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
>>> [ev.message for ev in client.fine_tuning.jobs.list_events('ftjob-ahpUgjDiaBkaf4q6HFPshqsG').data]
['Step 11/300: training loss=1.67', 'Step 1/300: training loss=0.22', 'Fine-tuning job started', 'Files validated, moving job to queued state', 'Validating training file: file-QWIeICryqfMQzigKLW7rIH3T', 'Created fine-tuning job: ftjob-ahpUgjDiaBkaf4q6HFPshqsG']
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
KeyboardInterrupt
>>> 
```
