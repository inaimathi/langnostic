Ok, so I've been foreshadowing this for a while, and I don't think I can help myself anymore. Ever since I read [this](TODO - wendy's drive through employee article), I've had it in my head that it wouldn't be too hard to put together with the already existing assortment of random open-source software that already exists in the world. Combined with the [OpenAI API](TODO) endpoints for [chat](TODO) and [voice transcription](TODO), it actually doesn't even sound like a first cut at this thing would take up a full hackathon attempt. So, lets step into a totally unbranded drive-thru establishment and see what it would look like to have a robot handling the human interaction at the front end.

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
