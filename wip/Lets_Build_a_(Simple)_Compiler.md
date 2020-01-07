A while back, I followed [a blog
post](https://eli.thegreenplace.net/2018/type-inference/) by
[Eli Bendersky](https://eli.thegreenplace.net) on type inference in which they
show the reader how to build a simple type inference engine inspired by the
Hindley-Milner family of type systems. I was an enjoyable ride, fun enough that
I stayed around and built a simple proof of concept for both an interpreter and
a compiler backend of such a language to C. In fact, the language is simple
enough that if we make some concessions we can build both using the same simple
internal representation—so let’s do that!
