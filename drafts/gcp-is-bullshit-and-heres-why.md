You may have heard of these things called LLMs. And you might wonder how to go about using them in your various projects. This is an excellent thing to wonder about! I thoroughly recommend exploring it! 

## Calling LLMs

There's a company called OpenAI that has some endpoints for you to check out! Here's how you do it in Python:

```
import requests

resp = requests.post(
    f"https://api.openai.com/v1/chat/completions",
    headers={
        "Content-Type": "application/json",
        "Authorization": f"Bearer {your OpenAI API key}",
    },
    json={"messages": [{"role": "user", "content": "Hello there, OpenAI"}], "model": "gpt-4o"},
)

print(resp.json()["choices"][0]["message"]["content"])
```

Some people have begun pointing out that OpenAI is less than scrupulous in various ways, so we should be using other LLM API providers. That's a perfectly fine perspective! Anthropic is a competitor that has, as far as I know, a higher standard for AI safety and employee treatment. Here's how you use their system:

```
import requests

resp = requests.post(
    f"https://api.openai.com/v1/messages",
    headers={
        "Content-Type": "application/json",
		"x-api-key": ENV["ANTHROPIC_API_KEY"],
        "anthropic-version": "2023-06-01",
    },
    json={"messages": [{"role": "user", "content": "Hello there, Anthropic"}], "model": "claude-3-5-sonnet-20240620"},
)

print(resp.json()["content"][0]["text"])
```

Ok, the response path is a bit more byzantine in the OpenAI case, and if you want to use `system` prompts in the Anthropic case, it's a bit weirder since it's a top-level parameter instead of a message type, and also Anthropic uses a bizarre `x-api-key` header instead of the web-standard `Authorization`, but you know what both of these have in common?

1. You get an API key from your account
2. You use that API key to authenticate your results
3. You get a response back from the given model if you properly authenticate with your key

## This Is The Part Where GCP Is Shit

Take a guess about how you do the comparable thing over on GCP. Did you assume (after you [set up a GCP project](https://developers.google.com/workspace/guides/create-project), then separately [enable the Vertex API](https://cloud.google.com/vertex-ai/docs/featurestore/setup) and [generate an API key](https://console.cloud.google.com/apis/credentials) of course, can't take care of _any_ of the bookkeeping automatically _for_ you, what do you think this is, the glorious robot future?) that you could just, I dunno

```
import requests

requests.post(
    f"https://us-central1-aiplatform.googleapis.com/v1/projects/{YOUR_PROJECT_ID}/locations/us-central1/publishers/google/models/gemini-1.5-flash-001a:streamGenerateContent",
    headers={
        "Content-Type": "application/json",
        "Authorization": f"Bearer: {API_KEY}",
    },
    json={
        "contents": {
            "role": "user",
            "parts": [
                {
                    "text": "Hi there, GCP!"
                }
            ],
        }
    },
)
```

Hah! You poor, sweet, summer child. That would _never_ justify the kind of giant brains that Google API engineers like to pretend they have! What actually happens if you do that is:

```
{'error': {'code': 401,
           'details': [{'@type': 'type.googleapis.com/google.rpc.ErrorInfo',
                        'domain': 'googleapis.com',
                        'metadata': {'method': 'google.cloud.aiplatform.v1.PredictionService.Predict',
                                     'service': 'aiplatform.googleapis.com'},
                        'reason': 'CREDENTIALS_MISSING'}],
           'message': 'Request is missing required authentication credential. '
                      'Expected OAuth 2 access token, login cookie or other '
                      'valid authentication credential. See '
                      'https://developers.google.com/identity/sign-in/web/devconsole-project.',
           'status': 'UNAUTHENTICATED'}}
```

Wait, unauthenticated? But didn't we just use the API key we established? Yes, but [you see](https://stackoverflow.com/a/75964043), you _can't_ use API keys to access the Vertex API. For reasons. Meaning: that that API key you set up? Scoped only to access the Vertex API? It's completely useless. They let you do that for shits and giggles, because they hate you and your suffering is nectar to them.

Ok, how do we do this then? Oh, it's really simple! You just use [the `vertexai` library](https://cloud.google.com/vertex-ai/generative-ai/docs/start/quickstarts/quickstart-multimodal)! I mean, it's not as straightforward as calling `requests.post`, and it has a minor amount of vendor lock-in as a result, but we can abstract that later.

```
import vertexai
from vertexai.generative_models import GenerativeModel

vertexai.init(project="<your project id here>", location="us-central1")

model = GenerativeModel(model_name="gemini-1.5-flash-001")

response = model.generate_content(
    "What's a good name for a flower shop that specializes in selling bouquets of dried flowers?"
)
```

That gets us up and running, right?

```
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/home/inaimathi/projects/mycroft-server/env-mycroft-server/lib/python3.12/site-packages/vertexai/generative_models/_generative_models.py", line 407, in generate_content
    return self._generate_content(
           ^^^^^^^^^^^^^^^^^^^^^^^
  File "/home/inaimathi/projects/mycroft-server/env-mycroft-server/lib/python3.12/site-packages/vertexai/generative_models/_generative_models.py", line 496, in _generate_content
    gapic_response = self._prediction_client.generate_content(request=request)
                     ^^^^^^^^^^^^^^^^^^^^^^^
  File "/home/inaimathi/projects/mycroft-server/env-mycroft-server/lib/python3.12/site-packages/vertexai/generative_models/_generative_models.py", line 210, in _prediction_client
    aiplatform_initializer.global_config.create_client(
  File "/home/inaimathi/projects/mycroft-server/env-mycroft-server/lib/python3.12/site-packages/google/cloud/aiplatform/initializer.py", line 499, in create_client
    "credentials": credentials or self.credentials,
                                  ^^^^^^^^^^^^^^^^
  File "/home/inaimathi/projects/mycroft-server/env-mycroft-server/lib/python3.12/site-packages/google/cloud/aiplatform/initializer.py", line 341, in credentials
    self._set_project_as_env_var_or_google_auth_default()
  File "/home/inaimathi/projects/mycroft-server/env-mycroft-server/lib/python3.12/site-packages/google/cloud/aiplatform/initializer.py", line 98, in _set_project_as_env_var_or_google_auth_default
    credentials, _ = google.auth.default()
                     ^^^^^^^^^^^^^^^^^^^^^
  File "/home/inaimathi/projects/mycroft-server/env-mycroft-server/lib/python3.12/site-packages/google/auth/_default.py", line 691, in default
    raise exceptions.DefaultCredentialsError(_CLOUD_SDK_MISSING_CREDENTIALS)
google.auth.exceptions.DefaultCredentialsError: Your default credentials were not found. To set up Application Default Credentials, see https://cloud.google.com/docs/authentication/external/set-up-adc for more information.
```

Fuck you.

[Every](https://cloud.google.com/docs/authentication/provide-credentials-adc) [piece](https://stackoverflow.com/a/75933132) of [documentation](https://cloud.google.com/vertex-ai/generative-ai/docs/start/quickstarts/quickstart) I've seen on this is _highly insistent_ that you have to call `gcloud auth application-default login` in order to set up your credentials. The only problem with this is that it forces you to go through an OAuth authentication process with your Google account. As in, in-browser. Which means there is no obvious way to translate that into a deployable web-app.

I'll save you the several hours of searching, tweaking, testing and apoplectic rage: What you _actually_ have to do is go back to that [API authentication page](https://console.cloud.google.com/apis/credentials) and set up something called a Service Account for your project, granting it scopes to VertexAI. _Then_ you have to download a JSON object corresponding to it that looks something like

```
{
  "type": "service_account",
  "project_id": "your project ID",
  "private_key_id": "lol-nope",
  "private_key": "-----BEGIN PRIVATE KEY-----\nlol-even-more-nope\n-----END PRIVATE KEY-----\n",
  "client_email": "something@developer.gserviceaccount.com",
  "client_id": "0000000000000000000000000000",
  "auth_uri": "https://accounts.google.com/o/oauth2/auth",
  "token_uri": "https://oauth2.googleapis.com/token",
  "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
  "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/0000000000000000000000000000-compute%40developer.gserviceaccount.com",
  "universe_domain": "googleapis.com"
}
```

Then you have to call 

```
CREDS, PROJECT_ID = google.auth.load_credentials_from_file("wherever-you-saved-it.json", scopes=["https://www.googleapis.com/auth/cloud-platform"])
```

_then_ you have to call

```
vertexai.init(project="<your project id here>", location="us-central1", credentials=creds)
```

to set up the Vertex credential provider background state. And if this is starting to sound like one of those [Java Enterprise Class Name jokes](https://projects.haykranen.nl/java/) to you too, congratulations, you might be as old as I am.

Now that all that's done, you can finally

```
model = GenerativeModel(model_name="gemini-1.5-flash-001")
response = model.generate_content(
    "What's a good name for a flower shop that specializes in selling bouquets of dried flowers?"
)

print(response.text)
```

and get something approaching what you wanted

```
Here are some names for a flower shop specializing in dried flowers, playing on different themes:

**Nature & Botanical:**

* Whispering Wildflowers
* Sun-Kissed Blooms
* Everlasting Bloom
* The Dried Garden
* Bloom & Preserve
* The Botanical Bloom
* Dried & Divine
* Petals & Past
* Meadow & Muse

**Rustic & Vintage:**

* The Dried Bouquet
* Gather & Bloom
* The Vintage Petal
* The Bloom & Co.
* Dried & Charming
* The Willow & Rose
* The Timeless Bloom
* Dried & Loved
*  The Floral Archive

**Unique & Playful:**

* Petal & Paper
* The Dried Petal Co.
* Forever & Bloom
* The Bloom Box
* The Dusty Petal
* The Dried Flower Studio
* Bloom in Time
*  The Petal Post

**Tips for Choosing a Name:**

* **Keep it short and memorable:**  A shorter name is easier for customers to remember and say.
* **Consider your target audience:**  What kind of vibe do you want to convey?  Rustic? Elegant? Whimsical?
* **Check for availability:**  Make sure the name isn't already taken and that the domain name is available.

I hope this list helps you find the perfect name for your dried flower shop! 
```

Is part of this me not bothering to wade through overtly complicated, sometimes contradictory, documentation and read deeply? Sure. But remember those two code snippets from the beginning of this piece that showed you how to instantly and easily get results out of OpenAI and Anthropic? Please explain why it has to be more complicated than that. Please tell me why, in a world where literally all you're trying to do is get textual output from a particular model, you need to worry about the security model that Google (or more realistically AWS, which was then copied by Google) has arbitrarily decided to put in your way?

The complexity doesn't end at the above call by the way. Suppose you wanted to put together a web service that makes this call when requested. How would you go deploying one that called into Anthropic? You'd build put together a server, and possibly Dockerfile, that checks the env var `ANTHROPIC_API_KEY` to figure out what to put in headers, and you're basically done. Every deployment secret manager in the world can handle this workflow easily and transparently, and if you don't like any of them, you could easily locally encrypt an `.env` file and give your build system the corresponding key. 

What do you have to do with google? Oh, yeah. Here's a JSON file you need to load up. Granted, there's a `google.auth.load_credentials_from_dict` so you don't need to write it to disk when setting up creds, but 

- that secret now has to contain a block of JSON, 
- which you then need to load, 
- and use to create a set of credentials, 
- which you then need to use to initialize a credential manager, 
- which you then need to have running in memory when you call `GenerativeModel` (or possibly, `model.generate_content`, I'm still not entirely clear on it)
- so that you can _then_ follow the one single step you were actually here for and that every other AI API provider lets you skip to

Who is responsible for this? What kind of absolutely fucking numb-nuts _asshole_ is going to raise their hand and say "Oh, yeah, I did that thing that quintupled-or-more the amount of complexity you need to wade through in order to figure out how to call a model". If you remain anonymous, fuck you. If you don't, I'll be happy to say it again directly to your face.

## A Reasoned Summary

Why is this bad? 

I can foresee the counters that look like "well, if you just use their library, it becomes a non-issue", or "well, if you just call `gcloud`, there's no problem". The complaints I have about deployment complexity above probably have things of a similar shape that you could say.

Do you remember that [joke about the lizards](https://www.youtube.com/watch?v=LuiK7jcC1fY)? Sorry, of course not, that clip is older than your dad. Go ahead and watch it, and then consider the analogy.

The thing is, sending an `HTTPS POST` request to an endpoint, accompanied by an authenticating API key _is secure_. It's about as secure as you can get. If you've got an attacker that's compromised your line to the point where they can exploit that opening, then you effectively auto-rotating keys every hour by deferring to the Google auth system buys you _almost nothing_. This is "more secure" in the sense that you might now get away from an attacker that has managed to compromise your line for less than an hour. Granted that's a gain, but it's a gain of somewhere between 0.001% and 1% additional security on top of the existing machinery. I highly doubt you could argue it up to more than that. This is a piddling amount of security, given that the other end of the trade is the balooning of development and deployment complexity I've outlined above.

This is _Not. Worth. It._ And if you made that trade, then you are a moron who deserves to be called that to their face.

I also notice that you're banking on enough people making this trade and effectively getting locked into your platform down the line. If that's accidental, then chalk it up as another negative to your approach, and please consider re-evaluating how you expose this as an API to the outside world. If that's _intentional_, then in addition to being a moron, you are also malicious and I have no further time or energy to spend arguing with you.
