Ok, so I've been foreshadowing this for a while, and I don't think I can help myself anymore. Ever since I read [this](https://www.theverge.com/2023/5/9/23716825/wendys-ai-drive-thru-google-llm), I've had it in my head that it wouldn't be too hard to put together with the already existing assortment of random open-source software that already exists in the world. Combined with the [OpenAI API](https://openai.com/blog/openai-api) endpoints for [chat](https://platform.openai.com/docs/api-reference/chat) and [voice transcription](https://platform.openai.com/docs/api-reference/audio), it actually doesn't even sound like a first cut at this thing would take up a full hackathon attempt. So, lets step into a totally unbranded drive-thru establishment and see what it would look like to have a robot handling the human interaction at the front end.

;; 1. Have a background-thread waiting to record audio at a given threshold into a subdirectory of the working directory
;; 2. Have a background-process watching that directory
;; 3. Run `ai/transcription` on each file that gets put there. When a transcription contains text, add it to the `interaction` list as a `:user` message
;; 4. After a long enough delay (~2 seconds, but make it configurable) generate a response with `ai/chat`
;; 5. Maintain an order list
;; 6. Once an order is finalized, if the order is more than one item, ask the user to confirm, or if there's anything else
;; 7. When done, ask user to drive through, print order
;; 8. Log order to an order history directory

## Voice Recording at the User End

There's two different recording types we want here. First we should just ambiently record all the time so we can do things when noise levels get high enough to imply directed speech, and second, once we're in the middle of taking an order, we need to be able to record a block of time until the user is done (silent) for a second or two. Both of these are going to be in the [`sound` submodule](TODO). And, as you'll see based on the implementation I ended up putting together here, I'm not doing any of [my usual cross-platform support stuff](TODO - link to my :cl-mop library or possibly some of the ifdefs in :house), this thing straight-up assumes MacOS[^i-think-im-going-to-change].

[^i-think-im-going-to-change]: I _think_ I'm going to change that shortly. One of the things writing this project has taught me is that just having a vocal chatbot (as opposed to one you have to type at) is oddly compelling to people, and pulling that capability out into a separate library just seems like the right thing to do in a general sense. _That's_ definitely happening on my linux machine though.

```
(defn record-ambient [& {:keys [dir]}]
  (shell/sh
   REC "order.wav"
   "silence" "1" ".05" "1.3%" ;; wait until we hear activity above the threshold for more than 1/20th of a second then start recording
   "1" "3.0" "3.0%" ;; stop recording when audible activity falls to zero for 3.0 seconds
   "vad" ;; trim silence from the beginning of voice detection
   "gain" "-n" ;; normalize the gain
   ":" "newfile" ;; store result in a new file
   ;; ":" "restart" ;; restart listening process
   :dir dir))
```

`REC` is literally an alias for the `rec` command line tool. There's a definition earlier in `sound.clj` that reads `(def REC "/opt/homebrew/bin/rec")`. And that set of parameters I pass to it is something I ripped bleeding from a [StackOverflow answer](TODO) after some fairly painful googling. It does what the comments imply it does, but this seems like a situation where you've got a weirdly idiosyncratic enough tool that putting this command together myself would be a multi-day exercise in learning and frustration.

The other one is more interesting.

```
(defn record-until-silence [& {:keys [dir filename silence-size] :or {filename "order.wav" silence-size 2.0}}]
  (let [res (shell/sh
             REC filename
             "vad" ;; trim silence from the beginning of voice detection
             "silence" "1" ".05" "1.3%" ;; wait until we hear activity above the threshold for more than 1/20th of a second
             "1" (str silence-size) "3.0%" ;; stop recording when audible activity falls to zero for silence-size seconds
             "gain" "-n" ;; normalize the gain
             :dir dir)]
    (if (= 0 (:exit res)) filename)))
```

I mean, it doesn't _look_ more interesting, but this is an example of something I didn't think [`aidev`](TODO - link to your emacs library) would be good for that turns out to be its' most useful feature. I got the basic skeleton of this function by highlighting the above `record-ambient` definition and doing

```
M-x aidev-refactor-region-with-chat RET
This function records ambient sound into files starting with non-silent sound and keeps going until terminated. Please re-write it so that it instead begins recording immediately, records until a block of silence and then stops (rather than recording into a fresh file afterwards). RET
```

It didn't give me back _exactly the definition I ended up committing. In particular, I had to rejig its parameters and rename it. But the key part, the changed parameters to `shell/sh`, got dropped in directly from the result. This isn't _fantastically_ impressive, but the I could imaginge the process it I would need to follow in order to generate that shell call myself, and it definitely involves doing a stupid amount of googling and/or reading `man rec`. The ~1.5 second API call from my editor followed by a minute or two of re-structuring the resulting function definition is _much_ better. If this is the future of programming, I could definitely get used to it.

The part of the top-level work process that I'm going to end up leaving as an exercise for the reader includes hwo to actually determine when we should start the interaction process. The initial approach I thought of, which involved running `record-ambient` until it started generating files, doesn't _quite_ work. Specifically, it gets weird because the point of this interaction is that you're going to be playing synthesized speech back in the same general area where you're doing ambient recording to figure out what the user wants. You could _probably_ do the work to just keep the recorded users' stream, but it doesn't seem worth it[^also-doing-record-ambient-then].

[^also-doing-record-ambient-then]: Also, doing `record-ambient`, then killing it in order to start a loop of `record-user -> ai-responds` and then restarting `record-ambient` with some delay at the end didn't _trivially_ work, and I wanted to get this blog post out.

There's definitely a company's worth of work waiting to be done here, and if some enterprising business dude out there wants to handle that part, I'll gladly sign on as CTO, but I'm doing this with an eye to exploration rather than amassing wealth.

## Voice transcription

Dealing with the voice input of the user is something that gets dealt with in [`model.clj`](TODO), with calls out to the `sound` functions from above, and also out to [my API library](TODO).

```
(defn wait-for-user-response! [& {:keys [attempts] :or {attempts 3}}]
  (when (> attempts 0)
    (let [fname (str (:name @ORDER) "/user-speaks" (count (get-history)) ".wav")
          file (snd/record-until-silence :filename fname)]
      (if file
        (do (order-log! {:user-voiced file})
            (let [message (ai/transcription file)]
              (when-let [txt (get message "text")]
                (order-log! {:ai-transcribed txt})
                (user-chat! txt))))
        (wait-for-user-response! :attempts (- attempts 1))))))
```

Easy, right? We're going to try a few times (in case recording or transcription errors for some reason), but the thing we're doing in an attempt is

1. Calling `record-until-silence` with a filename that isn't yet taken in this orders' subfolder
2. If that went fine, we log the call with `order-log!` and then send a request to `ai/transcription`
3. If that went fine, we log it again, and then call `user-chat!` (which is just a utility function that adds the message to this orders' history)

## Feed conversation into the chat interface

Once a user has initiated an order, we need the AI to respond. Which is pretty straightforward once you think about it.

```
(defn robot-response! []
  (let [resp (ai/chat (get @ORDER :interaction))
        ai-txt (get-in resp ["choices" 0 "message" "content"])]
    (order-log! {:ai-response-id (get resp "id")})
    (order-log! {:ai-says ai-txt})
    (robot-chat! ai-txt)
    (let [ai-mp3 (snd/text->mp3 ai-txt :dir (:name @ORDER))]
      (order-log! {:ai-voiced ai-mp3})
      (snd/play ai-mp3))))
```

We just feed `(get @ORDER :interaction)` into `ai/chat`, store the result in interaction history and output the result both to the `order-log!` and to an MP3 file on disk, which we then immediately play for the user.

You'll note that this implies a particular structure for an `ORDER`.

TODO - talk about order structure and log/state maintenance

## Prompting with a menu

- Show how to build up a prompt that tells the AI what this restaurants' menu is

## Toughts on AI interaction

- Actually surprisingly bad at plain booleans (the `should-extract?` function still kinda fails occasionally when I tell it to return "Yes"/"No", but it failed _even worse_ back when I asked it to return a JSON boolean that corresponded to the same yes/no answer) despite the [earlier success at encoding machine-readable datastructures](TODO - link to previous post on this)
- At least mildly resistant to command insertion attacks (see this order <TODO - add the order where I try to add a $-5.00 back cheeseburger to the menu>), and it kinda sticks to the menu (<TODO - add the order wherein I try to order vodka>)
- Part of the `sound` module is AI-generated. Or... er, I guess AI-assisted? I got the `record-until-silence` function by using my [`emacs` refactor mode](TODO) to give it my definition of `record-ambient` and prompting it with `"This function records ambient sound into files starting with non-silent sound and keeps going until terminated. Please re-write it so that it instead begins recording immediately, records until a block of silence and then stops (rather than recording into a fresh file afterwards)."` It failed to change the name of the function or its' inputs, but did give me the correct invocation of the `rec` command line tool to accomplish what I wanted. This definitely saved me a bunch of googling/doc-reading, and I'm unclear whether it just got lucky and handed me something that worked or whether it would consistently give correct answers here.

Overall, having used that emacs interaction mode for a bit, I'm much more confident about having an AI perform boilerplate-involving-but-basic-refactorings than I used to be, I'm fairly comfortabl with having it explain what a particular function does or tries to, but I'm much _less_ likely to use it for straight-up code generation or automated architecture. I'm still planning to try a few things in that general direction, but from here, it looks like it'll be much more work than I initially thought it'd be. Still a big win, mind you, improving my productivity by 2x or 3x is still a _really_ nice payoff for what amounts to a ~$2.00 per month subscription fee. I'll keep you posted if I change my opinion again in any significant way.
