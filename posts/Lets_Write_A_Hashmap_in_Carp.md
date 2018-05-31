An exciting new feature landed in Carp a while ago, and it is perfect material
for a tutorial-style blog post!

The feature I’m talking about is generic types for structs, and in this blog
post we’ll be building a simple [hash
map](https://github.com/hellerve/hashmap). Hash maps are fun to write, and
easy to implement inefficiently, and today we’re going to do just that!

As I always do with these types of blog posts, I’ll start by defining an API,
and then we’ll try and implement the whole thing!

## An API

A hashmap is usually used for storing and retrieving values, so we need to
primarily be concerned with that. There are also a few convenience functions
that we would like to have, but for the purposes of this blog post they’ll be
left as an exercise to the reader. If you want you can also check out the
repository I linked to above where I implemented a more complete interface.

```
(let [x (Map.create)]
  (do
    (Map.put x "my-key" "my-value")
    (Map.get x "my-key")))
```
<div class="figure-label">Fig. 1: A usage example for maps.</div>

This is fairly sparse but already provides a lot of the functionality we need.

Before going any further, let’s think about how we deal with getting keys that
don’t exist. We’re in a statically typed language without any standard error
mechanism—sorry about that—, and as such, we need to opt for an implementation
using defaults or opt for the interface `zero` that returns a zero value for all
of the types it is defined in. In this implementation we’ll use zero and hope
it is defined for all of the value types we will want to put into our map. This
differs from the version I linked to above, but makes for a simpler interface,
since we don’t always have to specify a default value.

As this is a hashmap, we’ll also have to think about hashing. I’ll provide you
with an interface for a hash function, but not its implementation for all data
types. You can either also see this as an exercise or just copy-paste it from
my ready-made solution.

```
(definterface hash (Fn [a] Int))
```
<div class="figure-label">Fig. 2: An interface for hashing.</div>

Let’s get to coding!

## An implementation

First, let’s define some types using our beautiful generic syntax. To be able to
do that, we have to answer some questions as to how we want our
implementation to work, so brace yourselves for some tradeoffs!

### The types

What is a hashmap?

```
(deftype (Map a b) [
  n-buckets Int
  buckets (Array (Bucket a b))
])
```
<div class="figure-label">Fig. 3: A hashmap type.</div>

Okay, so a map is a struct that contains a number of buckets and a list of
buckets from type `a` to type `b`, where those letters are type variables. Type
variables are always written lower-case, and we have to specify them as
arguments to the type definition.

So, what are buckets?

```
(deftype (Bucket a b) [
  entries (Array (Entry a b))
])
```
<div class="figure-label">Fig. 4: A bucket type.</div>

Turns out a bucket is just an array of entries. It, too, just hands down its
type arguments to the entry type.

Right around now you’re probably asking yourself what the heck we’re doing.
Turns out that when writing a hashmap we have to deal with collisions. The
easiest way to do so is to make each list a bucket of values that we then search
linearly once we encounter a collision.

So, what is an entry?

```
(deftype (Entry a b) [key a value b])
```
<div class="figure-label">Fig. 5: An entry type.</div>

An entry is a key value pair. Here we finally use our type variables to make
those key-value pairs generic. This enables us to write generic hashmaps. One
restriction this poses on our hashmaps, though, is that the type variables `a`
and `b` always resolve to the same types in the same map. This means that you
can have a map from integers to strings or strings to floats, for instance, but
not from anything to anything.

These are all the types we need, so now it’s time to actually implement some
functionality and see how that goes.

## The functions

We will implement these functions in a top-down manner, starting with the ones
that are public and working our way down the stack to the nitty-gritty details.
Let’s start with something simple, creating a map.

### Creating

```
(defmodule Map
  (def default-nbuckets 256)

  (defn create []
    (init default-nbuckets
          (repeat default-nbuckets Bucket.empty)))
)
```
<div class="figure-label">Fig. 6: Creating an empty map.</div>

When creating a map, we have to initialize the buckets. We decide to have 256
buckets in our hashmap, which is a number that I chose completely arbitrarily.
Usually you’d want to experiment with the number a bit and see what’s best for
you: if it is lower, the initial memory penalty will be low; if it is higher,
we have less collisions.

We also use `Bucket.empty` to initialize the buckets themselves, so we have to
define that. As described on the packaging, it will create and return an empty
bucket.

```
(defmodule Bucket
  (defn empty []
    (Bucket.init []))
)
```
<div class="figure-label">Fig. 7: Creating an empty bucket.</div>

And that’s all we need to implement to be able to bootstrap maps. Now let’s try
and figure out how to put stuff into it using `Map.put`.

### Putting

```
(defmodule Map
  (defn put [m k v]
    (let [idx (Int.mod (hash k) @(n-buckets &m))
          bs (buckets &m)
          b (nth bs idx)
          e (Entry.init @k @v)]
      (set-buckets m (aset @bs
                           idx
                           (Bucket.grow b e)))))
)
```
<div class="figure-label">Fig. 8: Putting things into a map.</div>

Putting a new entry into a map is a three step process: first we need to find
the index of the bucket we need to put the element in. We do this by hashing the
key and using a modulus operation to make it fit into the number of
buckets<sup><a href="#1">1</a></sup>.

We then grow this bucket—an operation we haven’t defined yet—, which will put
the entry element into the bucket. And, finally, we write the result back into our
`buckets` array.

The meat of this function lies within `Bucket.grow`, which is surprisingly
simple as well. Have a look at it:

```
(defmodule Bucket
  (defn grow [b e]
    (set-entries b (push-back @(entries b) e)))
)
```
<div class="figure-label">Fig. 9: Putting things into a bucket.</div>

All we have to do is take the bucket and push our new entry to the back of the
array. Simple housekeeping.

We’re only missing a final puzzle-piece, retrieval, to make our hashmap useful.

### Getting

```
(defmodule Map
  (defn get [m k]
    (let [idx (Int.mod (hash k) @(n-buckets m))]
      (Bucket.get (nth (buckets m) idx) k)))
)
```
<div class="figure-label">Fig. 10: Getting things from a map.</div>

From the hashmap’s point of view, retrieving values is simple: it performs the
same hashing operation that we used in Figure 8 to locate the correct bucket,
and tells it to get the value under the key.

Next we need to implement `Bucket.get`.

```
(defmodule Bucket
  (defn get [b k]
    (let-do [e (zero)
             l (length (entries b))
             es (entries b)]
      (for [i 0 l]
        (when (= (Entry.key (nth es i)) k)
          (do
            (set! e (Entry.value (nth es i)))
            (break))))
       @e))
)
```
<div class="figure-label">Fig. 11: Getting things from a bucket.</div>

This is by far the most complicated function we’ve defined in this blog post,
but it is still fairly manageable. We initialize the element we want to return
using `zero`, which will generate a zero value. This will be the default if we
don’t find anything. Then we iterate over the entries in the bucket and, if we
find any entry whose key member matches the given key, set the element we want
to return to that entries value and return.

This is it! Those 40-ish lines of code are enough to specify a simple hashmap
type!

But is it any good?

## Caveats

There are a few things that make this hashmap implementation suboptimal.
ALthough the Carp community was very excited when I first wrote this simple
little library, I immediately cautioned against using it as our default hashmap
implementation. This is for a few reasons that I want to share with you, and maybe
you can rectify some of its flaws and make it better!

- I’m no expert in hashmaps. This might seem like a cop-out, but I’m not
  particularly well-read in the literature surrounding their implementation, and
  I’m sure there are a million better basic hashmap algorithms out there that
  I’ve never heard of.
- Even for the ones that I have in fact heard of, this one is not particularly
  good. It’s the simple textbook example of how to implement hashmaps, but it
  doesn’t hold a candle to the standard implementations in circulation today.
- It does a lot of copies. Remember all of the weird `@` glyphs everyhwere?
  Every single one of those is a copy, and this can be super costly. I’m sure
  they’re avoidable, but, truth be told, I haven’t invested a lot of time into
  getting rid of them.
- There’s no duplicate checking. Right now, when you insert the same key twice,
  the two entries will co-exist in the bucket. But only the one that was
  inserted first will ever be returned by `Map.get`. This is not a great default
  behavior, and I think either overriding or erroring would be better
  alternatives.
- We cannot currently distinguish between zero values that are actually in the
  hashmap, and values that aren’t present. One way to remedy this would be to
  introduce a result type that has an element and a boolean that tells us
  whether we found a value under that key or not, similar to how Go handles
  map access.

Some of these flaws are easily remediable, and might be great exercises for you
if you’re looking to dive into Carp! Others are a bit trickier to get rid of, as
they are concerned with the basic functionality and type shape of the hashmap
implementation.

## Fin

Today we have implemented a simple hashmap. While the implementation itself does
not reflect the state of the art in fast, memory-efficient hashmaps, it has most
of the basic functionality you’d want. It is also type-generic without any
annotations, and Carp’s borrow checker makes sure that it is memory-safe and
its performance deterministic.

I hope I was able to show you the beauty of Carp’s generic types. They are
really quite powerful, and an important step towards the expressiveness we want
from Carp.

See you soon!

#### Footnotes

<span id="1">1.</span> To prevent the modulus operation from screwing up the
                       uniformity of our hash function, we should ensure to
                       always use a power of two as the number of buckets.
