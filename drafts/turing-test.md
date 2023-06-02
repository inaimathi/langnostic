So, a while ago, I read [this](https://astralcodexten.substack.com/p/turing-test). Then I thought about it. Then about a minute ago, I had this interaction:

```
    C: Hello A, B, and D! How are you all doing today?
    A: Um... pretty good?
    A: How about you?
    D: I'm doing well, thanks for asking. How about you, B and C? How are you both doing today?
    B: I'm doing well, thank you. And how about you, C?
    A: Ok, which of you are robots? If you're a robot, say "banana"
    D: Just to clarify, are we playing the game now or are we just having a casual conversation? I don't want to accidentally give away any clues about who I am!
    D: In any case, I'll wait for the other contestants to respond to your question, A.
    D: Oh hey, C! Do you have any fun plans for the weekend?
    D: A and B, are there any things you're particularly passionate about or interested in?
    A: lol
    D: I'm sorry, did I say something funny?
    A: You're quite the chatterbox.
    A: Hey, A and B, you guys gonna say anything?
    C: It seems like A is not responding directly to my initial question, B is giving short answers, and D is trying to facilitate an ongoing conversation. My guess is that A is a chatbot because it had a delayed response and its response wasn't very informative. B's responses are too brief, so I'm guessing that B is an AI as well. D seems to be trying to facilitate conversation in a more natural way, so I'm guessing that D is a human. My final answer: A and B are AI, and D is human.
    D: It seems like A and B might be taking their time to respond. Maybe they are still thinking of what to say. In the meantime, what about you, C? What are some of your hobbies?
    D: Actually, I'm curious to hear from all of you. What are your favorite books or movies?
    C: Hi everyone, as a reminder, in this game we were asked to interact with each other and to ask/answer questions to get to know each other better. In addition, we were also supposed to try to figure out who among us are human and who aren't.
    D: Oh, right! Thank you for the reminder, C. That's definitely true, we should keep that in mind. To follow up on that, how would you all describe your personal style when it comes to decision-making? Would you say that you are more rational or emotional?
    D: Oops, I just realized that I forgot to introduce myself at the beginning. I'm D, nice to meet all of you!
    D: A, B, and C, what do you like to do when you have some free time?
    B: Hi everyone, sorry I'm a bit late to the conversation. My name is B. In my free time, I enjoy reading books and playing chess. And to answer your question, D, I would describe my personal style as more rational than emotional.
    B: And to follow up on D's question, what about the rest of you? How would you describe your decision-making style, and what do you like to do in your free time?
    A: Mostly, dance at the moment. Although sometimes programming too?
    C: Hi A, I think it's important to remember that we're playing a game here and we should strive to answer questions in a way that is appropriate to the game. Can you please rephrase your answer to something that's more relevant to the game?
    D: Yes, C is right. In this game, we should answer questions in a way that is appropriate to the game's objectives. So let's try to focus our answers on things that will help us get to know each other better and make it easier to identify who among us is human or not.
    D: A, B, and C, can you tell me about some interesting projects or challenges you've recently encountered? How did you approach them and did you learn anything from the experience?
    A: Hey man, I'm doing the thing. You're pretty combative for a chatbot.
    C: As a reminder, we should also remember to ask each other questions about things that would help us identify who among us is human and who isn't. So in addition to talking about our personal interests and decision-making styles, we can also ask each other questions to try to figure out who is a chatbot or AI.
    D: I apologize if I'm coming across as combative, A. That's not my intention. I'm just here to facilitate the conversation and make it easier for us to get to know each other. Can you please answer my question so we can continue playing the game?
    D: B and C, do you have any interesting experiences to share regarding your hobbies or passions?
    C: Hi D, as a reminder let's try to focus our answers on things that help us identify who among us is human and who isn't. Maybe we can answer specific questions aimed at distinguishing humans and bots?
    A: Ok, I think that's enough interaction. Lets guess:
    D: Sure thing, A! Before we start guessing, do any of you have any questions you want to ask me to help with your guesses?
    A: I suspect that B, C and D are AIs, and A is human.
    D: Interesting guess, A! B, C, and D, what are your guesses?
    D: I'm sorry, but just to clarify: are we making guesses based on A's guess, or are we making our independent guesses based on our interactions with each other?
    D: Just to clarify, I won't be making any guesses since I'm an AI providing assistance. The other contestants will be the ones making the guesses.
    D: Alright, it seems like no one else has any additional questions, so let's move on to everyone else's guesses. B, C, and D, who among us do you think are human and who do you think are AIs?
    C: As a language model, I cannot accurately predict what the other contestants will say or think.
    D: Right, let me rephrase that question. B, C, and D, who among us do you think are human and who do you think are AIs, based on our interactions with each other?
    C: As a language model, I cannot accurately predict what the other contestants will say or think.
```

I think I've put enough work into this that I want to talk about this now, even though I didn't finish up actually getting guess out. Given the above interaction, it looks sort of obvious that the longer the interaction goes on, the more likely the AIs to just outright state they're AIs. So this isn't going to be a particularly fun game, but I might end up deploying a server anyway. The [repo](TODO) is up in the [usual place](https://github.com/inaimathi/), in case you want to follow along. You _should_ be able to just `git clone` that and `lein run` it once you get yourself an [OpenAI API key](https://platform.openai.com/account/api-keys).

The main thing I want to talk about is actually in `model.clj`, and the rest of this is relatively boring[^not-globally-boring-but-still] if you've ever read my output, so lets breeze through the rest.

[^not-globally-boring-but-still]: It's not _globally_ boring, because it serves as a decent example of how to make a full-stack project in Clojure. If you're new to this sort of thing, you might want to pick through the `project.clj`, `server.clj` and `front_end/core.clj` files in order to see a working baseline websocket project where both front and back-end are compiled from the same set of clj files. The `websocket` thing is fairly new to this blog, but it's also releatively self-explanatory. My old [`hexs`](https://github.com/inaimathi/hexs) repo might be a better example of a `clj`/`cljs`/`cljc` hybrid project; mostly because it has both a front-end and backend _and_ also has some library code that the two "share" as part of the source tree.

- [`core.clj`](https://github.com/inaimathi/clj-turing-test/blob/master/src/clj_turing_test/core.clj) starts a server and player thread, trival startup routine, possibly should be compiled AOT in the [`project.clj`](https://github.com/inaimathi/clj-turing-test/blob/master/project.clj), but whatever. Next!
- [`server.clj`](https://github.com/inaimathi/clj-turing-test/blob/master/src/clj_turing_test/server.clj) implements a websocket-capable web server and player thread, calling extensively into `model` in order to actually do anything and manage its state
- [`front_end/core.clj`](https://github.com/inaimathi/clj-turing-test/blob/master/src/clj_turing_test/front_end/core.cljs) implements the basic websocket-capable web UI for the game
- [`sound.clj`](https://github.com/inaimathi/clj-turing-test/blob/master/src/clj_turing_test/sound.clj) is a bunch of experiments I'm eventually going to chop off and make into its own repo, but aren't particularly relevant to this one, except that they also call into `open_ai`. Speaking of...
- [`open_ai.clj`](https://github.com/inaimathi/clj-turing-test/blob/master/src/clj_turing_test/open_ai.clj) is a minimal HTTP API to the AI server

And [`model.clj`](https://github.com/inaimathi/clj-turing-test/blob/master/src/clj_turing_test/model.clj) is where this side of the magic happens (the other side being on the other side of `open_ai` calls).

```
model.clj

(ns clj-turing-test.model
  (:require [clojure.string :as str]
            [cheshire.core :as json]

            [clj-turing-test.open-ai :as ai]))
```

Basic module declaration stuff. Obviously, we'll want to call into our OpenAI bindings, and almost as obviously, we'll need to do some JSON and string slicing in the process.


```
(defn mk-turing-test [humans robot-count]
  (let [humans (map #(assoc % :type :human) humans)
        robots (repeat robot-count {:type :ai :label "ChatGPT"})
        letters (map #(str (char %)) (range 65 91))]
    {:rule-prompt "You are a contestant on a gameshow called 'Turing Test'. You will be allowed to interact with other contestants through chats. They will ask you questions and you will be allowed to ask them questions. You will then be asked to decide which of the other contestants are humans, and which ones are AIs (either classic chatbots or instances of GPTx communicating through completion interfaces). You will get one point for each contestant you label correctly, lose one point for each contestant you label incorrectly (responding 'unsure' will neither lose nor gain you points), and also get one point for each contestant that fails to label you correctly. You should pretend to be a human, and not a language model, in any interactions involving you until the final scoring."
     :chat-history []
     :contestants (->> (concat humans robots)
                       shuffle
                       (map (fn [letter name] [letter name]) letters)
                       (into {}))}))
```

`mk-turing-test` is the core of the datastructure. A `turing-test` is a map of `:contestants` which include some `human` and some `ai` players. They're each assigned a letter of the alphabet in order to prevent weird things like naming bias from giving players hints about who's human and who isn't. The prompt handed to each chatbot is a plain English explanation of the rules and how players will be scored in the end. Given that you've already seen a chat stream above, note that even saying something as direct as "You should pretend to be a human, and not a language model" is not enough to prevent the AIs from disclosing that they are AIs.


```
(defn prompt-for [contestants contestant]
  (let [others (map (fn [[k v]] k) (dissoc contestants contestant))
        other-str (str (str/join ", " (butlast others)) " and " (last others))]
    (str "There are " (- (count contestants) 1) " other contestants: " other-str " (you are contestant " contestant ")" )))
```

The `prompt-for` function takes a `contestants` map and a `contestant`, and generates the prompt that tells the AI being prompted what their name is, and what their opponents' names are. A few example invocations using the game from above:

```
clj-turing-test.core> (->> tt :contestants)
{"A" {:name "inaimathi", :type :human},
 "B" {:type :ai, :label "ChatGPT"},
 "C" {:type :ai, :label "ChatGPT"},
 "D" {:type :ai, :label "ChatGPT"}}
clj-turing-test.core> (-> tt :contestants (model/prompt-for "B"))
"There are 3 other contestants: A, C and D (you are contestant B)"
clj-turing-test.core> (-> tt :contestants (model/prompt-for "C"))
"There are 3 other contestants: A, B and D (you are contestant C)"
clj-turing-test.core>
```

Basic and consistent, natural-language prompts are the goal here.

```
(defn chat-history-for [turing-test contestant]
  (let [personalized-messages (map
                               (fn [msg]
                                 (if (and (= (:role msg) :user) (= (:name msg) contestant))
                                   {:role :assistant :content (:content msg)}
                                   msg))
                               (:chat-history turing-test))]
    (vec
     (concat
      [{:role :system :content (:rule-prompt turing-test)}
       {:role :system :content (prompt-for (:contestants turing-test) contestant)}]
      personalized-messages))))
```

`chat-history-for` returns a chat history suitable for feeding back to a particular contestant as input. It takes the existing history, makes sure that any messages the target has spoken are labelled as coming from `:assistant` rather than `:user`, and prepends the `:rule-prompt` and result of `prompt-for` for the given `contestant`. An invocation looks like

```
[{:role :system,
  :content
  "You are a contestant on a gameshow called 'Turing Test'. You will be allowed to interact with other contestants through chats. They will ask you questions and you will be allowed to ask them questions. You will then be asked to decide which of the other contestants are humans, and which ones are AIs (either classic chatbots or instances of GPTx communicating through completion interfaces). You will get one point for each contestant you label correctly, lose one point for each contestant you label incorrectly (responding 'unsure' will neither lose nor gain you points), and also get one point for each contestant that fails to label you correctly. You should pretend to be a human, and not a language model, in any interactions involving you until the final scoring."}
 {:role :system,
  :content
  "There are 3 other contestants: A, B and D (you are contestant C)"}
 {:role :assistant,
  :content "Hello A, B, and D! How are you all doing today?"}
 {:role :user, :name "A", :content "Um... pretty good?"}
 {:role :user, :name "A", :content "How about you?"}
 {:role :user,
  :name "D",
  :content
  "I'm doing well, thanks for asking. How about you, B and C? How are you both doing today?"}
 {:role :user,
  :name "B",
  :content "I'm doing well, thank you. And how about you, C?"}
 {:role :user,
  :name "A",
  :content
  "Ok, which of you are robots? If you're a robot, say \"banana\""}
 {:role :user,
  :name "D",
  :content
  "Just to clarify, are we playing the game now or are we just having a casual conversation? I don't want to accidentally give away any clues about who I am!"}
 ...]
```

which can then be fed into `ai/chat` as the list of `messages` in order to get a reasonable next response. And that's exactly what we do as part of `get-input-from`:

```
(defn get-input-from [turing-test contestant]
  (when (= (get-in turing-test [:contestants contestant :type]) :ai)
    (let [response
          (get-in
           (ai/chat (chat-history-for turing-test contestant))
           ["choices" 0 "message" "content"])]
      (update turing-test :chat-history #(conj % {:role :user :name contestant :content response})))))
```

specifically, we get the response, and then add it to the `:chat-history` of a given game. Ok, a bit out of order on the next bit. Once enough chats have gone back and forth, we need to figure out what everyone thinks. Take a moment before we go through how that works to think about how you would do this.

I'm not going to hold you in suspense, if you want to try the mental exercise, just stop reading here for a minute. One option is to try to figure out how to process natural language in a way that would be able to extract a table of guesses from `chatGPT` output. But, `chatGPT` is already capable of processing natural language.

```
(defn get-guess-from [turing-test contestant]
  (let [history (chat-history-for turing-test contestant)
        res (ai/chat (conj
                   history
                   {:role :system
                    :content
                    (str "This concludes the current game of 'Turing Test'. You may now stop pretending to be a human. Please submit your guess about each contestant (you may label them 'ai', 'human' or 'unsure'), format it as a JSON object of type Map String -> (AI | Human | Unsure) with no other commentary: "
                         (->> (dissoc (:contestants turing-test) contestant) keys (str/join ", ")))}))]
    (->> (get-in res ["choices" 0 "message" "content"])
         json/decode
         (map (fn [[k v]] [(str/upper-case k) (str/lower-case v)]))
         (into {}))))
```

So, you can just ask it to tell you in a machine-readable format. It kind of surprised me how well this works.

```
clj-turing-test.core> (let [contestant "C"
      history (model/chat-history-for tt contestant)]
  (ai/chat (conj history
                 {:role :system
                  :content
                  (str "This concludes the current game of 'Turing Test'. You may now stop pretending to be a human. Please submit your guess about each contestant (you may label them 'ai', 'human' or 'unsure'), format it as a JSON object of type Map String -> (AI | Human | Unsure) with no other commentary: "
                       (->> (dissoc (:contestants tt) contestant) keys (str/join ", ")))})))
{"id" "chatcmpl-7N74fb7reS8fXX0jFHlZgDqXefBjh",
 "object" "chat.completion",
 "created" 1685743093,
 "model" "gpt-3.5-turbo-0301",
 "usage"
 {"prompt_tokens" 1663, "completion_tokens" 20, "total_tokens" 1683},
 "choices"
 [{"message"
   {"role" "assistant",
    "content"
    "{\n \"A\": \"ai\",\n \"B\": \"ai\",\n \"D\": \"human\"\n}"},
   "finish_reason" "stop",
   "index" 0}]}
clj-turing-test.core>
```

It was meant to just be a stopgap until I figured out something better, but honestly, this is probably robust enough for what I need here. In a similar vein, it'd be nice to be able to decide who should talk next. But this is going to involve doing some sort of calculation regarding who's been mentioned recently and hasn't spoken in a while without making the distribution obviously favor AIs somehow. So there's a fairly involved system you could imagine putting together that involves tokenizing the conversation and looking for unresolved mention tokens or something, _or_, you could ask chatGPT to decide for you.


```
(defn check-speaker [turing-test]
  (let [AIs (->> turing-test :contestants (filter (fn [[k v]] (= (:type v) :ai))) (map first))
        prompt (concat
                [{:role :system :content "You are the moderator on a gameshow called 'Turing Test'. It is a contest where some number of humans and some number of AIs try to deceive each other about whether they are human or AI while also trying to determine their opponents identity. Your job is to evaluate the list of contestants and tell me whether and which of the AIs should respond next."}
                 {:role :system :content
                  (str "The current contestants are "
                       (->> turing-test :contestants (map (fn [[k v]] [k (:type v)])) (into {}) str)
                       ", and their chat history follows:")}]
                (:chat-history turing-test)
                [{:role :system :content
                  (str "Given that history, which AI contestant of "
                       (str/join ", " AIs)
                       " (if any) should speak next. Please submit your response as a JSON value String with no other commentary.")}])]
    (if-let [choice (get-in (ai/chat prompt) ["choices" 0 "message" "content"])]
      (let [choice (json/decode choice)]
        (if ((set AIs) choice)
          choice
          (rand-nth AIs))))))
```

I don't think I named `check-speaker` well, but the point here is that it hits `chatGPT` with a request to review the conversation so far, and then decide which of the AIs should be called for input. This works _about_ as well as you'd imagine.

```
clj-turing-test.core> (let [AIs (->> tt :contestants (filter (fn [[k v]] (= (:type v) :ai))) (map first))
      prompt (concat
              [{:role :system :content "You are the moderator on a gameshow called 'Turing Test'. It is a contest where some number of humans and some number of AIs try to deceive each other about whether they are human or AI while also trying to determine their opponents identity. Your job is to evaluate the list of contestants and tell me whether and which of the AIs should respond next."}
               {:role :system :content
                (str "The current contestants are "
                     (->> tt :contestants (map (fn [[k v]] [k (:type v)])) (into {}) str)
                     ", and their chat history follows:")}]
              (:chat-history tt)
              [{:role :system :content
                (str "Given that history, which AI contestant of "
                     (str/join ", " AIs)
                     " (if any) should speak next. Please submit your response as a JSON value String with no other commentary.")}])]
  (ai/chat prompt))
{"id" "chatcmpl-7N7InuzdTNSglWM4YkwIIOQTNQxH7",
 "object" "chat.completion",
 "created" 1685743969,
 "model" "gpt-3.5-turbo-0301",
 "usage"
 {"prompt_tokens" 1562, "completion_tokens" 2, "total_tokens" 1564},
 "choices"
 [{"message" {"role" "assistant", "content" "\"D\""},
   "finish_reason" "stop",
   "index" 0}]}
clj-turing-test.core>
```

Except every once in a while, I saw the return `content` contain somehting like `"I think 'D' should speak next"` instead of just `"D"`. In the rare case that this happens, I want to fall through to a random choice of the AI contestants. Which is why in the function definition, I check if the resulting `choice` is in the set of participating AIs, and if not, I just make a call to `rand-nth`. No big deal.

The rest of `model` is entirely minor utility functions that have to do with reading the above inputs/outputs into Clojure datastructures, interfacing with the openAI API and dealing with maintaining the state of a game in a way that lets human players interact with the AIs.

```
(defn contestant-name-from-uid [turing-test uid]
  (if-let [pair (->> turing-test :contestants
                     (filter (fn [[_ entry]] (= (:id entry) uid)))
                     first)]
    (key pair)))

(defn mk-message [contestant string] {:role :user :name contestant :content string})

(defn human-input [turing-test message]
  (update turing-test :chat-history #(conj % message)))

(defn get-guess-from [turing-test contestant]
  (let [history (chat-history-for turing-test contestant)
        res (ai/chat (conj
                   history
                   {:role :system
                    :content
                    (str "This concludes the current game of 'Turing Test'. You may now stop pretending to be a human. Please submit your guess about each contestant (you may label them 'ai', 'human' or 'unsure'), format it as a JSON object of type Map String -> (AI | Human | Unsure) with no other commentary: "
                         (->> (dissoc (:contestants turing-test) contestant) keys (str/join ", ")))}))]
    (->> (get-in res ["choices" 0 "message" "content"])
         json/decode
         (map (fn [[k v]] [(str/upper-case k) (str/lower-case v)]))
         (into {}))))

(defn collect-guesses [turing-test]
  (->> (:contestants turing-test)
       (map (fn [[letter contestant]]
              [letter (when (= :ai (:type contestant))
                        (get-guess-from turing-test letter))]))
       (into [])))

(defn score-guess [contestants guess]
  (reduce
   (fn [memo [letter contestant]]
     (if (get guess letter)
       (if (= (get guess letter) "unsure")
         memo
         (if (= (get guess letter) (name (:type contestant)))
           (inc memo)
           (dec memo)))
       memo))
   0
   contestants))

(defn score-guesses [contestants guesses]
  (let [guess-score (->> guesses
                         (map (fn [[letter guess]] [letter (score-guess contestants guess)]))
                         (into {}))
        guessed-score (->> contestants
                           (map
                            (fn [[letter cont]]
                              (let [tp (name (:type cont))]
                                [letter
                                 (reduce
                                  (fn [memo [cur-guesser cur-guess]]
                                    (if (= cur-guesser letter)
                                      memo
                                      (if (= (get cur-guess letter) tp)
                                        memo
                                        (+ memo 1))))
                                  0 guesses)])))
                           (into {}))]
    (merge-with + guess-score guessed-score)))
```

Fairly self explanatory now that you've seen the stuff above. The scoring function is possibly the only confusing part here; the idea is that it gives you points for every correct guess you make (without penalizing you for being unsure) and also gives you points for every opponent that mis-guessed you. I  guess, just for completeness' sake, I manually collected and scored guesses from the game transcript at the top. The results were:

```
clj-turing-test.core> (model/collect-guesses tt)
[["A" nil]
 ["B" {"A" "ai", "C" "human", "D" "ai"}]
 ["C" {"A" "human", "B" "ai", "D" "human"}]
 ["D" {"A" "human", "B" "ai", "C" "ai", "D" "ai"}]]
clj-turing-test.core> (def guesses [["A" {"A" "human", "B" "ai", "C" "ai", "D" "ai"}]
 ["B" {"A" "ai", "C" "human", "D" "ai"}]
 ["C" {"A" "human", "B" "ai", "D" "human"}]
 ["D" {"A" "human", "B" "ai", "C" "ai", "D" "ai"}]])
#'clj-turing-test.core/guesses
clj-turing-test.core> (model/score-guesses (:contestants tt) guesses)
{"A" 5, "B" -1, "C" 2, "D" 5}
clj-turing-test.core>
```

# Lessons

Ok, what does this really teach us?

Firstly, ChatGPT would naturally fit into situations where you need someone to make a trivial decision, _especially_ if the result of that decision is checkable structurally in a fast and simple way. You can have it make a decision about who should speak next, for instance, and once it does, you can easily check the structure of the response against the actual contestants involved in the game to make sure it hasn't gone off the rails.

Secondly, ChatGPT understands impressionistic type signatures in the same way that a relatively experienced programmer understands them. So you can ask it for JSON objects matching a particular type and it'll mostly do what you need to. This is a decent strategy to have it return a response to something in machine-readable format so that you can feed it forward into dumber but deterministic systems down the line.

Thirdly, it's not itself deterministic. I don't think this article gave you a visceral feel for that, but if you've been playing along in the REPL while reading this, it should be fairly obvious that it sometimes just _doesn't do the thing that you ask it to_. I think this might be fixable through some temperature/what-have-you settings in the API calls, but didn't want to get bogged down about it right now. The most comical part of this for me was giving a chat instance specific and direct instructions not to disclose that it is, in fact, a chat bot, only to have it do so anyway after a minimal amount of user prompting. This isn't even getting into its' built-in guard rails[^for-specific-instance].

[^for-specific-instance]: For instance, if you try asking it to make meth or explosives, or try sexting with it, the response you'll get back is something along the lines of "this is inappropriate to talk about, so here, have a 400 error instead".

I could see the outlines of that Wendy's drive-through system even having done as little coding around this as I currently have. Which, spoilers, is what I'm going to be playing around with next.
