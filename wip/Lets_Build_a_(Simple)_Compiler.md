A while back, I followed [a blog
post](https://eli.thegreenplace.net/2018/type-inference/) by
[Eli Bendersky](https://eli.thegreenplace.net) on type inference in which they
show the reader how to build a simple type inference engine inspired by the
Hindley-Milner family of type systems. I was an enjoyable ride, fun enough that
I stayed around and built a simple proof of concept for both an interpreter and
a compiler backend of such a language to C. In fact, the language is simple
enough that if we make some concessions we can build both using the same simple
internal representation—so let’s do that!

Before we start, a word of caution: the compiler we end up with will be very
limited, and contain a few features that need to be fleshed out tremendously to
be actually useful. I’m leaving this as an exercise to the reader, partly
because I don’t want this blog post to become overly long, and partly because
this is where I stopped when initially developing the PoC. Following along will
give you an idea of how interpreters and compilers work internally, though,
and it will provide you with a great playground on which to explore building
programming languages further.
