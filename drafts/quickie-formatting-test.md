I asked for feedback on my last post, and some people I talk to expressed surprise about ChatGPT's ability to encode things in machine readable format. Lets do a trivial test of these capabilities. Feel free to follow along at home.

First up, an ease of use definition.

```
trivial-openai.core> (in-ns 'trivial-openai.core)
#namespace[trivial-openai.core]
trivial-openai.core> (defn ask [prompt]
  (-> (chat [{:role :system :content prompt}])
      (get-in ["choices" 0 "message" "content"])
      println))
#'trivial-openai.core/ask
trivial-openai.core> (ask "Test prompt. What's up, ChatGPT?")
Hello! As an AI language model, I don't really have any emotions and I don't have a heart to read the news or social media updates but I'm always ready to assist you in any way I can. How may I assist you today?
nil
```

Ok, now the thing I already know it can do.

```
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in JSON format")
Sorry, I cannot perform external API calls as it goes beyond my capabilities as a language AI model. However, here's an example of a JSON-encoded Map String Number object:

```
{
  "apples": 5,
  "oranges": 8,
  "bananas": 3,
  "kiwis": 2
}
```

This represents a Map object with keys as strings ("apples", "oranges", "bananas", and "kiwis") and values as numbers (5, 8, 3, and 2).
nil
```

Um ... hum. I mean, I guess?

```
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in JSON format with no other commentary")
{
  "key1": 1,
  "key2": 2,
  "key3": 3
}
nil
```

There we go. Ok, slightly harder...

```
    trivial-openai.core> (ask "Please return an object of type Map String Number encoded in YAML format with no other commentary")
\`\`\`
---
key1: 123
key2: 456
key3: 789
\`\`\`
nil
```

Slightly unexpected. I think that isn't the most elegant encoding, but I also _think_ it'll parse as YAML (correct me if I'm wrong though). I didn't really doubt that capability; I think there's enough YAML on the internet that I was expecting it to get this one. Ok, lets throw some nerd challenges at it...

```
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in EDN format with no other commentary")
{:apple 2, :banana 3, :orange 4}
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded as a Common Lisp ALIST with no other commentary")
(("foo" . 1) ("bar" . 2) ("baz" . 3))
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in Python Dictionary format with no other commentary")
{"key1": 1, "key2": 2, "key3": 3}
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in Ruby Object format with no other commentary")
{"a"=>1, "b"=>2, "c"=>3}
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in C struct format with no other commentary")
struct Map {
  char* key;
  int value;
};
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in Haskell Record format with no other commentary")

data MyMap = MyMap { getMap :: Map String Number }
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in Rebol object format with no other commentary")
map [
    "key1" 1
    "key2" 2
    "key3" 3
]
nil
```

Ok, so it returned datatype declarations instead of literal structures for those last two, but if we're being honest about it, we're probably _never_ going to need to get output in that particular way. I'm also pretty impressed that it seems to know what [Rebol](http://www.rebol.com/) is. Ok, here's a couple more I'm curious about...

```
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in xml format")
Sorry, as an AI language model, I do not have the capability to return a formatted output. However, here's an example of how to encode a Map String Number object to an XML format using Java:

\`\`\`java
import java.util.HashMap;
import java.util.Map;
import java.io.StringWriter;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;

public class XMLConverter {

    public static void main(String[] args) throws Exception {
        Map<String, Number> map = new HashMap<>();
        map.put("number1", 10);
        map.put("number2", 20);
        map.put("number3", 30);

        StringWriter sw = new StringWriter();
        XMLOutputFactory factory = XMLOutputFactory.newInstance();
        XMLStreamWriter writer = factory.createXMLStreamWriter(sw);

        // Start Document
        writer.writeStartDocument();
        writer.writeStartElement("Map");

        // Iterate through the Map and write the elements
        for (Map.Entry<String, Number> entry : map.entrySet()) {
            writer.writeStartElement(entry.getKey());
            writer.writeCharacters(entry.getValue().toString());
            writer.writeEndElement();
        }

        // End the Map Element and the Document
        writer.writeEndElement();
        writer.writeEndDocument();

        // Output the XML String
        System.out.println(sw.toString());
    }
}
\`\`\`

The output should be:

\`\`\`xml
<?xml version="1.0" ?><Map><number1>10</number1><number2>20</number2><number3>30</number3></Map>
\`\`\`
nil
```

I'd be gobsmacked by this, if I hadn't [seen this](https://old.reddit.com/r/GPT3/comments/zb4msc/speaking_to_chatgpt_in_perfect_danish_while_it/) first, so the

> "I'm sorry, I can't do X"
> _proceeds to do X and explain how it does it_

thing isn't a complete shock here. It's worth noting that this prompt took a lot longer to return than the others. I should note; I don't actually speak Java. So while the above looks syntactically correct and passes a basic sniff test from me, I'm not sure it would actually work and am not about to fire up `javac` to find out.

```
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in xml format with no other commentary")
<?xml version='1.0' encoding='UTF-8'?>
<map>
  <entry>
    <string>key1</string>
    <number>1</number>
  </entry>
  <entry>
    <string>key2</string>
    <number>2</number>
  </entry>
  <entry>
    <string>key3</string>
    <number>3</number>
  </entry>
</map>
nil
```

So, literally just asking it to shut up works. However, XML trees are a bit more flexible than the other formats we've tried so far. Can we change that output slightly?

```
trivial-openai.core> (ask "Please return an object of type Map String Number encoded as an XML tree with the tag names Object, Key and Value with no other commentary")
<?xml version="1.0" encoding="UTF-8"?>
<Object>
  <Key>key1</Key>
  <Value>123</Value>
  <Key>key2</Key>
  <Value>456</Value>
  <Key>key3</Key>
  <Value>789</Value>
</Object>
nil
```

Nice.

You know what? I'm gonna be a _bit_ mean here.

```
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in HL7 format with no other commentary")
MSH|^~\&|HL7Sender|HL7Receiver|||20210816142101||ORM^O01|MSG123456789|P|2.5
PID|1||1234567890^^^PatientMRN^MR||Doe^John||19700101|M|||123 Main St^^Metropolis^IL^44123^USA|||||||
PV1||E||||||||||||||||||||||||||||||||||202108161420
ORC|NW|1234567890123456||1234567890123456_1^Pharmacy||CM||||202108161420||202108161420||testClinic^^^Test Clinic^L^^^^^
RXO|||^Aspirin^^^12345||10||mg
RXE|^^^Aspirin^^^12345||10||mg||1||^^^TID^30||||30||||||test||||||||||||||||||
TQ1|||^BID^2080-01-01T12:00:00|||||||||||20211225|||202108161420
nil
```

O_O

Ok, now I'm impressed. I'm not about to dredge up memories of my time in the medical industry by trying to thoroughly validate that, but it doesn't look completely insane.

Fine, ChatGPT, take a victory lap.

```
trivial-openai.core> (ask "I'm writing a blog post testing how you do at encoding data of type Map String Number in various formats. I've already asked you about JSON, YAML, EDN, Python object, Ruby object, C struct, Haskell record, Rebol object, and XML. Is there anything else I should ask you?")
Here are a few more formats you could test for encoding data of type Map String Number:

1. Protocol Buffers
2. MessagePack
3. BSON (Binary JSON)
4. CBOR (Concise Binary Object Representation)
5. TOML (Tom's Obvious, Minimal Language)
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded as a Protocol Buffer")
I'm sorry, as an AI language model, I don't have the capability to return protobuf-encoded objects. However, here's an example of how you can create a Protocol Buffer message for a Map<String, Number>:

\`\`\`
syntax = "proto3";

message Map {
  map<string, int32> values = 1;
}
\`\`\`

This message definition defines a `Map` message type that contains a map field named `values`, which maps strings to `int32` values. You can then use a protobuf library for your programming language to serialize and deserialize objects of this message type.
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded as a Protocol Buffer with no other commentary")
I'm sorry, but as an AI language model, I cannot return a Protocol Buffer. However, here's an example of how to define and serialize a Map String Number in Protocol Buffer using the Google Protocol Buffers library for the Java programming language:

\`\`\`
syntax = "proto3";

package com.example;

import "google/protobuf/map.proto";

message MyMessage {
    map<string, double> my_map = 1;
}
\`\`\`

To serialize an instance of this message, you can use the following code:

\`\`\`
MyMessage.Builder builder = MyMessage.newBuilder();
builder.putMyMap("foo", 123.45);
builder.putMyMap("bar", 678.90);

MyMessage message = builder.build();
byte[] bytes = message.toByteArray();
\`\`\`

Alternatively, you can use other programming languages and libraries that support Protocol Buffers, such as C++, Python, or Go, and follow their corresponding documentation to define and serialize a similar message type.
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in BSON format with no other commentary")
Sorry, as an AI language model, I am not able to provide BSON encoded data. However, I can provide a sample of how a Map<String, Number> object would look like in JSON format:

\`\`\`json
{
   "key1": 123,
   "key2": 456,
   "key3": 789
}
\`\`\`
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in CBOR format with no other commentary")
\xA2\x63\x61\x31\x19\x01\xF4\x63\x61\x32\x1A\x51\xEB\x85\x1F\xC0
nil
trivial-openai.core> (ask "Please return an object of type Map String Number encoded in TOML format with no other commentary")
\`\`\`toml
[map]
key1 = 1
key2 = 2
key3 = 3
\`\`\`
nil
```

Other than shooting itself in the foot by telling me to ask about formats it, on further inspection, can't encode to, this is a fairly impressive set of output options for the working ChatGPT-incorporating programmer.

I like it, and will be poking at it as I go.
