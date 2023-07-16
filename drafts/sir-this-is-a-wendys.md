Ok, so I've been foreshadowing this for a while, and I don't think I can help myself anymore. Ever since I read [this](TODO - wendy's drive through employee article), I've had it in my head that it wouldn't be too hard to put together with the already existing assortment of random open-source software that already exists in the world. Combined with the [OpenAI API](TODO) endpoints for [chat](TODO) and [voice transcription](TODO), it actually doesn't even sound like a first cut at this thing would take up a full hackathon attempt. So, lets step into a totally unbranded drive-thru establishment and see what it would look like to have a robot handling the human interaction at the front end.

;; 1. Have a background-thread waiting to record audio at a given threshold into a subdirectory of the working directory
;; 2. Have a background-process watching that directory
;; 3. Run `ai/transcription` on each file that gets put there. When a transcription contains text, add it to the `interaction` list as a `:user` message
;; 4. After a long enough delay (~2 seconds, but make it configurable) generate a response with `ai/chat`
;; 5. Maintain an order list
;; 6. Once an order is finalized, if the order is more than one item, ask the user to confirm, or if there's anything else
;; 7. When done, ask user to drive through, print order
;; 8. Log order to an order history directory

## Voice Recording at the User End

- show how to use `rec` to get exactly what we want here, including only kicking in at a particular volume of sound, and cutting out at a certain amount of silence
- speculate about how to use labelling and a camera to improve customer detection, but don't bother actually implementing it

## Voice transcription

- Basic call up to the OpenAI API to transcribe a voice recording, demonstrate return value

## Feed conversation into the chat interface

- Show how we maintain state between voice messages and feed each incremental addition through the chat interface
- Show how we use the same expedient of having the AI decide when an order is complete and then feed the result into the rest of the system in JSON format

## Prompting with a menu

- Show how to build up a prompt that tells the AI what this restaurants' menu is

## Toughts on AI interaction

- Actually surprisingly bad at plain booleans (the `should-extract?` function still kinda fails occasionally when I tell it to return "Yes"/"No", but it failed _even worse_ back when I asked it to return a JSON boolean that corresponded to the same yes/no answer) despite the [earlier success at encoding machine-readable datastructures](TODO - link to previous post on this)
- At least mildly resistant to command insertion attacks (see this order <TODO - add the order where I try to add a $-5.00 back cheeseburger to the menu>), and it kinda sticks to the menu (<TODO - add the order wherein I try to order vodka>)
- Part of the `sound` module is AI-generated. Or... er, I guess AI-assisted? I got the `record-until-silence` function by using my [`emacs` refactor mode](TODO) to give it my definition of `record-ambient` and prompting it with `"This function records ambient sound into files starting with non-silent sound and keeps going until terminated. Please re-write it so that it instead begins recording immediately, records until a block of silence and then stops (rather than recording into a fresh file afterwards)."` It failed to change the name of the function or its' inputs, but did give me the correct invocation of the `rec` command line tool to accomplish what I wanted. This definitely saved me a bunch of googling/doc-reading, and I'm unclear whether it just got lucky and handed me something that worked or whether it would consistently give correct answers here.

Overall, having used that emacs interaction mode for a bit, I'm much more confident about having an AI perform boilerplate-involving-but-basic-refactorings than I used to be, I'm fairly comfortabl with having it explain what a particular function does or tries to, but I'm much _less_ likely to use it for straight-up code generation or automated architecture. I'm still planning to try a few things in that general direction, but from here, it looks like it'll be much more work than I initially thought it'd be. Still a big win, mind you, improving my productivity by 2x or 3x is still a _really_ nice payoff for what amounts to a ~$2.00 per month subscription fee. I'll keep you posted if I change my opinion again in any significant way.
