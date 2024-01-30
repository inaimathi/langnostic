- RAG - retrieval augmented generation
- Example of security risk: https://stackdiary.com/chatgpts-training-data-can-be-exposed-via-a-divergence-attack/
- Hallucinations and Misinformation: LLMs return intentionally or incidentally incorrect data
Potential hallucination and misinformatino solution
- RAG
- RLHF, PEFT

- RAG involves vector stores. A database for storing sparse vector embeddings (looks like this might be a token TRIE?). Apparently good language representations start at ~350 dimensions. Cutting edge models (OpenAI/Anthropic/etc.) use more like ~1500. Someone pointed us to [this](https://moj-analytical-services.github.io/NLP-guidance/NNmodels.html)
	- Basic procedure:
		1. Take a question for your LLM
		2. embed it into the vector store
		3. find the top k most relevant (that is, shortest distance in the encoded vector space) entries in the vector space
		4. encode those returns along with the questions `f"using this information ({relevant_information}), answer the query: {original_question}"`
- Current techniques for RAG evaluation:
	- Things like [LangChain](https://python.langchain.com/docs/get_started/introduction), [LlamaIndex](https://www.llamaindex.ai/)?
	- The idea is that you can use leading edge LLM pipelines in order to get evaluations of your RAG pipeline
- It's now possible to use PostgreSQL for vector embeddings https://medium.com/@scholarly360/postgresql-as-vector-database-bae6dd7097a1 and https://github.com/pgvector/pgvector
- [Ragas](https://github.com/explodinggradients/ragas) is another framework for monitoring/tuning RAG pipelines

Questions:
- When we're evaluating the "Rag Triangle"

```
query -> context -> response -,
 ^____________________________|
```
given that you're using an external model how do you make sure that the encoded vectors support each other? (For example, if your vectorDB contains the statements "Paris is the capital of France" and "Paris is not the capital of France", they're clearly both relevant but they don't support each other.) A: Basically, you need to use RLHF to create your vector DB. You use that to _craft_ the initial vector space, but potentially also to refine the process.


# Post talk Chatter

Ok, so what's up with graph databases?
