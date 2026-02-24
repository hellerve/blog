---
title: "Building a simple reranker"
date: 2025-08-25
---

Databricks recently published [their reranker](https://www.databricks.com/blog/reranking-mosaic-ai-vector-search-faster-smarter-retrieval-rag-agents) for Vector Search in RAGs. I probably wouldn’t have heard of it, but a friend of mine from a different life worked on it and posted about it.

It got me curious. I wrote [the embedding registry](https://book.gtoolkit.com/using-the-embedding-registry-a6rybg6oen349pdk6uf3vsipw) for [gt4llm](https://book.gtoolkit.com/gt4llm-270ytb3y5mi1voswipfyc5hti) as a simple local testing tool that behaves like a vector database, but that’s pretty much where my experience ends.

Still, I was determined to understand what a reranker is, and how it works, and to see if I could recreate one locally. And now that I’ve done that, let me share it with you! And, as a little two-for-one-deal, it also doubles as a primer on how retrieval works!

## What’s a reranker?

A reranker basically inserts between the regular vector search and you. After the vector search has found the most relevant entries in your database using a nearest-neighbor search (and it does so very quickly!), the reranker then goes more deeply (and slowly) into the candidates and  determines which ones are actually relevant, re-ordering them.

And that’s basically it! The basic use case is extremely simple. All the magic lie in the algorithms and math. As such, it was a bit scary for me to approach. Despite being comfortable with deep technical topics of all shapes and sizes, inherently mathematical problems still scare me.

Let’s approach the problem top-down then, notebook-style, and see where we end up.

## Building a reranker

I wrote a whole bunch of code to understand this problem, but I’ll try to distill it for you. Still, this is more code than I would usually share, and I also published it in a [repository]() you can browse on your own time. I will abbreviate some code, like the data loading, and remove most type annotations in the snippets. In the repository you will find the entirety of the code including scaffolding for testing it.

### Setup

I’m writing this blog post in Glamorous Toolkit, so I will set the scene by installing the requirements, GT-style. You can also add them via `pip` or `poetry`, of course.

```smalltalk
PBApplication uniqueInstance
	installModule: 'sentence-transformers==3.0.1';
	installModule: 'numpy==2.0.0'
```

Now that we’ve got this out of the way, let’s think about a data model. In this example, we’ll look at documents that have a bit of meta-information. This is not strictly necessary, but will help us with our reranking demo, and mimics a lot of real-world applications.

```python
# let’s import everything here
from dataclasses import dataclass
import numpy as np

# our main data type
@dataclass
class Row:
    id: str
    title: str
    summary: str
    body: str
    url: str = ""
    
    def _doc_text(self, max_=2000):
        return f"""title: {self.title}
summary: {self.summary}
body: {self.body[:max_]}"""
   

def load_rows(root):
    # ... here we load the documents
    rows = []
    return rows

# configure your content root here:
ROOT_DIR = "/Users/veitheller/AllMyImportantDocs/"

rows = load_rows(ROOT_DIR)
```

The code above loads all the rows (well, it would anyway). Once we have the data the real fun begins.

### The retriever

We are now ready to define the retriever. It holds a bit of weird math, bear with me. The good news is you really don’t need to understand the retriever to understand the reranker, but I didn’t want to abridge it completely.

```python
from sentence_transformers import SentenceTransformer

class Retriever:
	# we define a model here. reasonable default included
    def __init__(self,
    	model_name="sentence-transformers/all-MiniLM-L6-v2"):
        self.model_name = model_name
        self.model = SentenceTransformer(model_name)
        self.emb = None

	# this "fits" our retriever to our rows by encoding
	# them and storing the results
    def fit(self, rows):
        texts = [r._doc_text() for r in rows]
        mat = self.model.encode(
        	texts,
        	normalize_embeddings=True,
        	convert_to_numpy=True,
        	show_progress_bar=False)
        self.emb = mat.astype(np.float32)

    # querying the retriever for the nearest neighbors
    def query(self, query_text, k=20):
        q = self.model.encode(
        	[query_text],
        	normalize_embeddings=True,
        	convert_to_numpy=True)[0].astype(np.float32)
        sims = self.emb @ q  # cosine similarity
        k = min(k, len(sims))
        idx = np.argpartition(-sims, kth=k-1)[:k]
        idx = idx[np.argsort(-sims[idx])]
        return idx.tolist()

retriever = Retriever()
retriever.fit(rows)
```

In the end, all this does is work with a sentence transformer model that gets text as input and returns a vector of floats we can run regular similarity on to get to the most similar documents. This seems like magic when you first learn about it, but is a tried-and-true method to get quick text similarity.

All `fit()` does, then, is generate all these vectors for us for all of our rows. `query()`, on the other hand, takes a text, generates a vector, and runs cosine similarity on them (mostly because that’s simplest) to get the `k` nearest neighbors aka documents.

Ideally we wouldn’t write this ourselves but rely on a vector search engine of some sort, but for the purposes of this post this is good enough.

### The reranker

The next step, and the point of this blog post, is writing a reranker that will be inserted after the retriever has down its work, and do a more thorough analysis of our document.

```python
from sentence_transformers import CrossEncoder

class Reranker:
	# initialize a cross-encoder. again, a reasonable
	# default
    def __init__(self,
    	model_name="cross-encoder/ms-marco-MiniLM-L-6-v2"):
        self.model_name = model_name
        self.model = CrossEncoder(model_name)

    # select the columns we want to place emphasis on for
    # each row. this should be a method on the row, but i
    # didn’t want to spoil it for you
    @staticmethod
    def _rerank_str(r, columns):
        parts = []
        for c in columns:
            v = getattr(r, c, "")
            if v:
                parts.append(f"{c}: {v}")
        return "\n".join(parts)

    # take the candidates an reranke them based on the
    # columns we provide
    def rerank(self,
    	query_text,
    	candidate_rows,
    	columns_to_rerank):
        pairs = []
        for r in candidate_rows:
            doc_str = self._rerank_str(r, columns_to_rerank)
            pairs.append([query_text, doc_str])
        # higher is better
        scores = self.model.predict(pairs, batch_size=16)
        order = np.argsort(-np.asarray(scores))
        return [candidate_rows[i] for i in order]

reranker = Reranker()
```

And that’s it! This might be a bit of a letdown, but again we let Jesus, erm, a model take the wheel. This time we use something called a “Cross-Encoder”. Before we take a second to understand what that is, let’s understand the code around it.

The meat of the code is in `rerank()`. It will take the query text and candidates, as well as a list of columns we want to focus on. For each of the rows, we then take the  query string and those columns, and finally feed everything into the model, which will conveniently score things for us. We can then take those scores to re-sort our candidates and we are done.

### Putting it all together

Finally we can put this together into a search function that combines retrieval and reranking.

```python
def search_with_rerank(
    rows,
    retriever,
    reranker,
    query_text,
    columns_to_rerank,
    num_candidates=50,
    num_results=20
):
    idx = retriever.query(query_text, num_candidates)
    reranked = reranker.rerank(
    	query_text,
    	[rows[i] for i in idx],
    	columns_to_rerank)
    return reranked[:num_results]
```

We finish off with an example:

```python
QUERY = "retrieval-augmented generation"
COLUMNS_TO_RERANK = ["title", "summary", "body"]

search_with_rerank(
    rows=rows,
    retriever=retriever,
    reranker=reranker,
    query_text=QUERY,
    columns_to_rerank=COLUMNS_TO_RERANK,
)
```

And we’re done!

### What’s a cross-encoder anyway?

I’m going to try my best at explaining this; just know that I’m not a machine-learning expert, I’m a weirdo that fell down a rabbit hole.

In a nutshell, a cross-encoder takes two texts and compares them token-by-token via attention in a single forward pass to produce a single relevance score; because both texts are in the same sequence,  the layers can go “across” and look at the token from the other sequence (hence the “cross” in “cross-encoding”). A model that lacks the “cross” part would be a bi-encoder, and that’s basically what our retrieval is.

They’re much slower than the retrievers, though, they need to encode for every candidate rather than just encoding and then doing fast vector math, and as such we use them only *after* selecting a set of candidates. We also only select relevant parts of the document to make it a bit quicker still (fewer tokens means less work).

## Fin

There you go, now you have another big word in your vocabulary to throw around to ensure everyone knows you *actually* know ML and are *definitely* not just a poser.

In all honesty, though, this was a fun little excursion into more mathy than usual territory, and I enjoy flexing my technical muscles every once in a while to ward off any atrophy. I hope you took something away from this, if only that I’m sorely lacking in the fundamentals of Machine Learning. See you around!
