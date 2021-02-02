About one and a half years ago, [I wrote a blog
post](https://blog.veitheller.de/Carp_and_derive.html) about a Carp macro that
enables deriving interfaces for arbitrary types based on their members.

It was a fun little hack, and I enjoyed playing around with it, but it wasn’t
quite ready for production yet. The API was clumsy, it wasn’t extensible, and
it suffered from a few limitations that Carp imposed on macros back then.

A few weeks ago I revisited and revised the macro, and now [it’s made its way
into the standard library](https://github.com/carp-lang/Carp/pull/1120).

The new design is more thoughtful: the API is uniform, the design is simple,
and it can be extended to support new interfaces relatively easily and without
touching the core implementation.

In this blog post, I’d like to guide you through the design. I won’t explain
what this feature is and why I think it’s worth having in the standard library;
I encourage you to read [my first blog
post](https://blog.veitheller.de/Carp_and_derive.html) if you haven’t already,
in which I explain all of that!

## `derive` and how to use it

Let’s first look at how to derive things, and how to write your own derivers.
[The documentation](https://github.com/carp-lang/Carp/blob/master/docs/Derive.md)
should roughly cover the same grounds, but I’ll try to rephrase it a little
bit—you can switch between the explanation to see if one makes sense where the
other doesn’t!

A normal invocation of `derive` takes the type on which we should operated as
well as the interface to derive as an argument. So if we defined a simple
`Address` type, this is what we might do:

```
(deftype Address [
  street String
  number Int
  zip Int
  city String
])

; we want to be able to compare addresses and create
; empty addresses, so we derive `zero` and `=`
(derive Address zero)
(derive Address =)
```
<div class="figure-label">Fig. 1: `derive` in action.</div>

This is already better than defining equality and identity ourselves, since
they will likely just rely on the same functions on member types anyway, and
that’s exactly what `derive` does.

To drive this point home, and to learn how to add `derivers`, which is what
the pieces of code that enable you to derive new interfaces are called, let’s
re-implement the deriver for `zero`. A `deriver` is a bit of metadata and a
function that, given a type, knows how to construct an implementation of the
interface. Let’s see this in action:

```
(make-deriver 'zero []
  (fn [t]
    `(init
      @%(map (fn [_] '(zero)) (eval `(members %t))))))
```
<div class="figure-label">Fig. 2: deriving `zero`.</div>

As demonstrated in Figure 2, we have to use `make-deriver` to register a new
deriver. It takes the name of the interface as its first argument, the names of
its arguments as an array as the second argument—in the case of `zero` no
arguments are given—, and the function that actually constructs `zero` as its
third argument. In the implementation of `zero`, it just goes through the list
of the type’s members and emits a call to `zero` for each of them, wrapping the
result in one call to the type’s initializer.

All in all, this makes for a clear and simple API. But how does it work? Let’s
take a look!

## Implementing `derive`

In order to register derivers, we need a piece of mutable global state to save
them to. While I’m generally wary of adding global state, in this case it makes
sense: this state is part of the state of the system at compile time, and it is
mutable, much like function or type definition change the state of the
compiler.

The best data structure to use for this state would probably be a hashmap, but
since we are at macro expansion time and only have lists and arrays to work
with, an association list will have to do<sup><a href="#1">1</a></sup>.

```
(defdynamic derivers '())
```
<div class="figure-label">Fig. 3: An empty list of derivers.</div>

Now I’d like to dial down expectations: we’re going to build a fully functional
version of the derive mechanism in Carp, exactly as it works in Carp. But it
won’t have any of the bells or whistles, and it will be completely implemented
in the global space. If you want to check out the implementation Carp actually
uses, [here it is](https://github.com/carp-lang/Carp/blob/master/core/Derive.carp)!

Now let’s implement the entry point for the derivation system, `derive` itself.
It will look up the deriver and call it.

```
(defmacro derive [t f]
  (let [deriver (get-deriver f derivers)]
    (if (empty? deriver)
      (macro-error "no deriver found for interface!")
      (eval ((cadr deriver) t)))))
```
<div class="figure-label">Fig. 4: The actual deriver code.</div>

At this point we’re missing two important pieces: `get-deriver`, which will
return the pair from the association list that matches the interface name—that’s
why we call `cadr` on it to get the actual derivation function, as it's the
second element of the pair—, and the code that defines derivers. Since the
lookup shouldn’t be too hard, it looks like we’ll have to spend some time
wrapping our heads around how registration works. It does all the actual heavy
lifting, it seems.

But, first things first, here’s `get-deriver`:

```
(defndynamic get-deriver [f derivers]
  (if (empty? derivers)
    '()
    (if (= (caar derivers) f)
      (car derivers)
      (get-deriver f (cdr derivers)))))
```
<div class="figure-label">Fig. 5: Looking up derivers.</div>

All this function does is go through the list of pairs linearly, checking the
key at each step. If it doesn’t find anything, `'()` is returned.

Only one piece of the puzzle missing! We’ll have to implement `make-deriver`.
Let’s take care of the boilerplate first!

```
(defndynamic make-deriver [f args body]
  (set! derivers
    (cons
      (list f
        ; some sort of magical deriver
      )
      derivers)))
```
<div class="figure-label">Fig. 6: Registering derivers.</div>

Registering derivers is as simple by prepending the new deriver pair at the
beginning of the list. The first element of that list is clear: the name of the
function. But what would the actual deriver look like?

Let’s think about what it needs to do: the user will supply us with a function
that generates the body of the derived function, that’s what `body` does. We
also have the name and the arguments it takes, so all we have to do is stick
them together, right? We also need to wrap the function in the appropriate
module—named, in Carp convention, the same as the type—and tell the system that
that function implements the interface.

Let’s do that!

```
(fn [t]
  `(defmodule %t
    (defn %f %args
      %(body t))
    (implements %f %(Symbol.prefix t f)))))
```
<div class="figure-label">Fig. 7: A deriver at last!</div>

The function makes use of quasiquotation, which I explain in depth [in another
blog post](https://blog.veitheller.de/Lets_Build_a_Quasiquoter.html). We also
need to wrap the generation code in a function that takes the type, because
that’s what `derive` will call.

And that’s all we need! We’re done!

I understand if all of this seems like sleight of hand. It seems like we just
wrote a bunch of connector code, and suddenly we were done. But that’s actually
all that `derive` does: it provides a faciliating mechanism and an API.
Providing types and implementations for interfaces is left to the users of that
API.

## Fin

When I discovered how little code this feature actually required, and how easy
that code was to write, I was quite surprised, but also quite proud. I hope
some of that pride shines through in this blog post, and I hope the
implementation “clicked” in the end. If not, sitting and thinking about it
might help. After all, that’s what I did for over a year after implementing
the initial version of `derive`, and it seemed to work!

#### Footnotes

<span id="1">1.</span> Association lists are lists of pairs where the first
element is the “key” and the second the “value”. Its API is very similar to
that of a hashmap, but all lookup operations on it are of linear complexity.
In the case of derivers that is fine, since I don’t expect there to be many
more than a few dozen entries in the list for any compile. How many interfaces
can anyone need<sup><a href="#2">2</a></sup>?

<span id="2">2.</span> This is the footnote I will link to in a couple years’
time when someone comes into the chat to complain that their 500 derivable
interfaces blow up compile times.
