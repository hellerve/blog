A few days ago one of my friends from Recurse Center asked me to review
references—i.e. pointers—in Go with him. Primarily a Clojure developer, he had
a bit of trouble to get a feeling when to use references and when to use
values. After talking about that for about an hour he seemed to have a much
better intuition about what to use when. This, however, is not what I want to
talk about today.

What I want to talk about is an intuition about the operators involved in
working with pointers and what they mean on a type-level. They happen to be
very simple in Go, and even in C/C++, where we have such interesting—some
might use other words to describe this, e.g. “unsavory”—constructs as
pointer arithmetic.

Let's define some type signatures!

_Fair Warning_: This blog post uses Haskell notation, but you should be able to
follow without knowing the language.

## Dramatis Personae

We only need three operators to describe pointers in Go: `*` and `&`. I am
aware that those are only two operators, but one of them has two distinct
purposes—`*` is both a type-level and a value-level operator. What this means
is that we can use `*` in a type signature and in the normal flow of a program.
Let me give you an intuition of that, in Haskell notation:

```
-- * on a type level
type * t = Address t

-- The operators * and &
& :: v -> Address v
-- or:
& :: v -> * v

* :: Address v -> v
-- or, unreadably:
* :: * v -> v
```
<div class="figure-label">Fig. 1: The types of `*` and `&`.</div>

Let me explain what this means in prose: There is a type `*` that has as
parameter any type `t` and means it is an address of `t`. In Go, the address
type is opaque and stands on its own, so we won't explain it further yet. When
we later move on to C/C++, we will have to be more concrete about this.

After that we define the unary—i.e. single-argument—operators `&` and `*`,
which convert between the values `v` and the addresses of these values. If we
want to throw in some monads just for kicks, we could talk about `&` in terms
of the `return` operator, but as we have no unified operator for getting values
out of a monad, this isn't a particularly useful abstraction<sup><a href="#1">1</a></sup>.

These definitions are enough to talk about pointers in Go! Go thankfully has a
very simplistic pointer system that gives us the ability to pass around things
as pointers without making us suffer from pointer arithmetic—or enjoy it,
depending on what type of person you are. How would we achieve that, using our
little intuition?

## Addresses as values

If we want to think about pointer arithmetic, we need to go back to our earlier
point of pointers being opaque types and changing that notion. After all, this
is a deliberate restriction introduced by Go, whose primary developers
considered pointer arithmetic [a thing of the past](https://golang.org/doc/faq#no_pointer_arithmetic).
I largely agree, but there are languages where it is perfectly normal to do
calculations on pointers. If we want to extend this system to include them, we
only need to roll back the restriction, think about pointers as the thing they
truly are—things that point to the location of other things in memory—, and
construct a useful notion of memory. Memory can be thought of as a big blob of
bytes that supports indexing—in this context called addressing. A big blob of
bytes that supports indexing can be thought of as an array, if that helps your
mental model<sup><a href="#2">2</a></sup>.

If we think of memory as an array that supports indexing, all we need to know
is the size of the array to determine our pointer size—these days commonly 32
or 64 bit. This is because we do not want to waste any space, but we also want
to be able to index everything. In Haskell notation, this leaves us with:

```
type Address t = Int
-- where Int is either 32 or 64 bits long
```
<div class="figure-label">Fig. 2: The address type, demystified.</div>

And that's it! We cheated by relying on the integer type for pedagogical
purposes and because I don't know any better, but it enables us to do pointer
arithmetic.

I also learned that it is valid to do have an unused type parameter in Haskell,
although this is arguably most senseless.

## A useful abstraction

When I first thought of the operators involved in reasoning about pointers, my
intuition was that they _must_ be hard to think about. But, as it turns out,
the type signatures of the functions are rather simple, even if the underlying
mechanics are still Operating System Magic to us.

I hope my little abstraction was helpful to you. Have a beautiful day and see
you soon!

##### Footnotes
<span id="1">1.</span> I left it in there anyway, because I understand people
who use monads are very insistent to talk about everything in terms of this
occasionally useful structure.

<span id="2">2.</span> Please note that we're making things simpler than they
truly are, which is okay in this context.
