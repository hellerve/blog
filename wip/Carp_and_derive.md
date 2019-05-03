It’s been a while since I’ve written a technical blog post, and even longer
since I’ve blogged about Carp. Today’s blog post will right both of these
wrongs; I’ll talk about how one could implement `derive` in Carp.

Firstly, a little context is in order. Haskell has a concept called [type
classes](https://en.wikibooks.org/wiki/Haskell/Classes_and_types), which is a
way of defining generic functions that different types can implement. An example
for that would be `Eq`, the equality typeclass. All types that need to be able
to compared for equality using `==` have to implement this type class and supply
a definition of that function. This is similar to how interface functions in
Carp work—interfaces are functions that can have multiple specialized
implementations. In fact, the definition of equality look relatively similar in
Haskell and Carp.

Look at the Haskell version:

```
-- the actual Eq typeclass also defines /=
class Eq a where
  (==) :: a -> a -> Bool
```
<div class="figure-label">Fig. 1: Equality in Haskell.</div>

And then the Carp version:

```
(definterface = (λ [a a] Bool))
```
<div class="figure-label">Fig. 2: Equality in Haskell.</div>

Aside from the fact that Carp uses `=` for equality checking, this looks fairly
similar<sup><a href="#1">1</a></sup>.

Something that I sorely missed in Carp for a long time was `deriving`. What this
does is automatically infer definitions of typeclasses based on a type. This
only works for a certain set of typeclasses, but it’s fairly useful, since those
typeclasses are among the most important: `Show` defines a functions for
converting a value into a `String`, and `Eq` and `Ord` define functions for
comparing values, among others.

Carp generates some functions automatically when a type is defined, like `str`
(though you can override those definitions manually if you so desire). To me,
this has always been *almost* the best of both worlds: these utility
functions are always available if I need them, and if I need to I can still
write better versions by hand.

What I wanted to tackle though was deriving of similarly useful, but less
central functions, like `zero`—a function for generating the `zero` value of a
type, such as `0` for integers and `""` for strings.

So obviously because I’m me and I love macros, I wrote a few macros to emulate
something akin to deriving arbitrary functions! Let’s walk through them!

## An API

The API of an ideal `derive` mechanism is quite simple. We’d like to have one
form, `derive`, that just takes the type and the function to infer.

```
(deftype T [
  x Int
  y Int
])

(derive T inc)
(derive T dec)
```
<div class="figure-label">Fig. 3: Deriving incrementing and decrementing.</div>

In the example in Figure 3, both members of the type would be incremented by the
function. Of course this wouldn’t always be sensible, but probably a relatively
useful default.

A prerequisite for this to work is that all types inside the type `T` need to
implement `inc`, either by inference or definition. We won’t derive recursively.

To make our first implementation simple, let’s start by assuming that all
functions that can be derived have the signature `(λ [a] a)`, meaning that they
take a thing of that type and return another. This isn’t strictly necessary and
rules out a bunch of useful functions such as `zero` described above, but it
simplifies our implementation. I’ll also give a definition of a mechanism for
deriving more complex functions such as `=` and `zero` at the end.

## An Implementation

As always, we’ll start with a skeleton. The first thing to do is to define a
macro `derive` that takes a type `t` and a function `f`. We’ll also already open
the module of the type, so that the implementation of `f` is correctly
encapsulated, and define an empty function.

```
(defmacro derive [t f]
  (list 'defmodule t
    ; o is an arbitrary name. because we open
    ; a new function, we don’t really have to
    ; worry about hygiene
    (list 'defn f (array 'o)
      ; and now?
    )
  )
)
```
<div class="figure-label">Fig. 4: A skeleton for `derive`.</div>

Inside that function, we’ll probably have to go through the members recursively.
Recursion in Carp macros is a little tricky, and dynamic functions—that is,
functions defined using `defndynamic` that are available at compile time—are
probably a better fit. So let’s defer to such a function inside the macro.

```
(defmacro derive [t f]
  (list 'defmodule t
    (list 'defn f (array 'o)
      (derive-internal f t))))
```
<div class="figure-label">Fig. 5: A complete implementation of `derive`.</div>

And that’s it for `derive`!

Of course we’re just getting started. The actual meat of the mechanism happens
inside `derive-internal`. Well, as it turns out that’s also not true.
`derive-internal` will be a trampoline for the real recursive function, and its
sole job will be to gather the members of the type. So, how do we get at them?
Luckily, Carp has a compile-time function called `members` that returns an array
of pairs from member name to type. For `T` in Figure 3, for instance, `members`
would return `[(x Int) (y Int)]`. It looks like this is just what we need!

```
(defndynamic derive-internal [f a]
  (derive-internal2 f (members a)))
```
<div class="figure-label">
  Fig. 6: `derive-internal`, the trampoline function.
</div>

Alright, this is starting to look interesting! We have the functions we want to
derive, and the members of the type we need to work on. That’s good! Now it’s
time to think about the actual body of such a generated function. We could
define it imperatively, updating one member after the other, resetting a result
value. This might come naturally to those that come to Carp from C or even Rust.

```
(defn inc-imperative [o]
  (let [res o]
    (do
      (set! res (T.set-x res (inc (T.x &res))))
      (set! res (T.set-y res (inc (T.y &res))))
      res)))
```
<div class="figure-label">Fig. 7: An imperative version of `inc` for `T`.</div>

This is fairly clunky, and wouldn’t work with the ownership model of Carp. The
version I come up with is a style that looks very straightforward, but gets a
little tedious to write out manually for big types. For `T`, it would look like
this:

```
(defn inc-functional [o]
  (T.update-y
    (T.update-x o &inc)
    &inc))
```
<div class="figure-label">Fig. 8: A functional version of `inc` for `T`.</div>

This version leverages the `update-*` functions that Carp generates for all type
members. Those functions always take an object of that type and a reference to
a function that updates the member. This means that `(T.update-x (T.init 1 2)
&inc)`, for instance, would return `(T 2 2)`.

With a plan in mind let’s define the awkwardly named function
`derive-internal2`!

```
(defndynamic derive-internal2 [f ms]
  (if (= (length ms) 0)
    '(init)
    ; now what?
  )
)
```
<div class="figure-label">
  Fig. 9: A skeleton version of `derive-internal2`.
</div>

First, let’s make sure that we deal with all cases. If a type has no members, we
just return a new, empty type. This doesn’t make a lot of sense for `inc`, but
it’s as best a guess as any, I suppose.

Now let’s deal with the base case: one last member is left.

```
(defndynamic derive-internal2 [f ms]
  (if (= (length ms) 0)
    '(init)
    (if (= (length ms) 1)
      (list (Symbol.join ['update- (caar ms)])
            'o
            (list 'ref f))
      ; now what?
    )
  )
)
```
<div class="figure-label">Fig. 10: The base case for `derive-internal2`.</div>

Once we have just one member left, we take the name of the member using
`caar`—because it’s the first element of a pair inside an array—, prefix it with
`update-`, stick in `o`, which we decided would be the name of our parameter
in Figure 5, and also take a reference to the function `f` that we’re trying to
infer. We’ll end up with a call that looks a little like `(T.update-x o (ref
inc))`<sup><a href="#2">2</a></sup>.

That wasn’t too bad. In the recursive case, we’ll do something similar, but
we’ll also, well, recurse.

```
(defndynamic derive-internal2 [f ms]
  (if (= (length ms) 0)
    '(init)
    (if (= (length ms) 1)
      (list (Symbol.join ['update- (caar ms)])
            'o
            (list 'ref f))
      (list (Symbol.join ['update- (caar ms)])
            (derive-internal2 f (cdr ms))
            (list 'ref f)))))
```
<div class="figure-label">Fig. 11: `derive-internal2` in its final form.</div>

All that changes in the recursive case is that we don’t pass in `o` anymore.
Instead, we will give the result of the inner computations to our updater.

And that’s all there is to it!

## Caveats

This version of `derive` is still fairly limited. It only works for a subset of
all possible interface functions, some of the time. If you want to see how this
could maybe be generalized even more, I’ve created special cases for `derive`
for `zero` and `=` that use a bit of complex machinery. You can study them on
your own, at your own leisure, if you want to. I couldn’t quite figure out how
to completely generalize them yet, sadly, but I suppose that could be left as an
exercise to the reader!

## Fin

As always, thank you for reading my blog! I had a lot of fun playing around with
Carp’s macro machinery and bending it until it breaks. I’ve discovered a few
bugs in the process, some of which are fixed, [some of which](https://github.com/carp-lang/Carp/issues/409)
have not yet been fixed.

Working with macros in Carp definitely feels different than working with [Scheme
macros](https://blog.veitheller.de/scheme-macros/); in some ways, it was a
similar experience to [writing elaborate macros in Clojure](https://github.com/hellerve/talks/tree/master/clojure-berlin-march-2019),
but with types added to the mix. It’s quite the experience, and I wholeheartedly
recommend trying it out!

#### Footnotes

<span id="1">1.</span> Carp interfaces do not support grouping of functions;
                       every interface stands on its own. Haskell’s
                       typeclasses, on the other hand, often have more than one
                       function encapsulated within them. This is useful because
                       some functions don’t make sense without other ones—think
                       of monadic bind without return—, but Carp omits this for
                       now.

<span id="2">2.</span> `(ref f)` is equivalent to `&f`.
