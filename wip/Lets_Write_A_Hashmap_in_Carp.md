An exciting new feature landed in Carp a while ago, and it is perfect material
for a tutorial-style blog post!

The feature I’m talking about is generic types for structs, and in this blog
post, we’ll be building a simple [hash
map](https://github.com/hellerve/hashmap). Hash maps are fun to write, and
easy to implement inefficiently, and today we’re going to do just that!

As I always do with these types of blog posts, I’ll start by defining an API,
and then we’ll try and implement the whole thing!

## An API

A hashmap is usually used for storing and retrieving values, and so we need to
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

This is fairly sparse, but already provides a lot of the functionality we need.

Before going any further, let’s think about how we deal with getting keys that
don’t exist. We’re in a statically typed language without any standard error
mechanism—sorry about that—, and as such we need to opt for an implementation
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
do that, we have to answer some questions as to how we want to our
implementation to work, so brace yourselves for some tradeoffs!

### The types

What is a hashmap?

```
(deftype (Map a b) [n-buckets Int buckets (Array (Bucket a b))])
```
<div class="figure-label">Fig. 3: A hashmap type.</div>

Okay, so a map is a struct that contains a number of buckets and a list of
buckets from type `a` to type `b`, where those letters are type variables. Type
variables are always written lower-case, and we have to specify them as
arguments to the type definition.

So, what are buckets?

```
(deftype (Bucket a b) [len Int, entries (Array (Entry a b))])
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

TODO!
