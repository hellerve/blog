A question that comes up every once in a while on the Carp chatroom and in
Github issues is how memory management works. People who are not familiar with
Rust especially have understandable trouble grasping how to program with
references, and why some functions take references while others don’t. Today,
I will try to explain these concepts for my fellow Carpenters.

A fair note: while this blog post might be beneficial to your understanding of
the Rust borrow checker, this is not my focus. I don’t know Rust very well, and
it is entirely possible that it does things differently and I’m just confusing
your understanding of how its machinery works. I therefore suggest that you
don’t try to compare the two too much lest you fall prey to faulty assumptions.

## An unfortunate piece of syntax

I have an inkling that at least some of the confusion surrounding references
stems from the obvious comparison to references in more familiar languages such
as C and Go. It’s not only the name that is reminiscent: even the syntax is
identical. Ampersands take a thing by reference, as has always been the case.
Only that the semantics of “taking a thing by reference” are not the same at
all.

In the languages mentioned above, taking a reference has direct implications on
the memory level. In languages with first-class memory access—read C and C++
and other languages that allow for nasty things such as pointer arithmetic—,
taking a reference means that you end up with a pointer, i.e. a number
representing the point in memory where the thing lives. Languages like Go take
a safer subset of that behavior, and allow people to pass things by value or
by reference. Most of you will at least have a basic intuition about some of
this behavior, but if not, you can read my [blog post about these
concepts](https://blog.veitheller.de/References_and_Values.html) which might or
might not make sense.

This is not generally problematic—except for the class of bugs it opens—were it
not for the unfortunate design decision of many borrow-checked languages—Rust
and Carp, at least—to reuse that name and the syntax surrounding it. It is,
albeit, not the same thing at all, and I will henceforth refrain from ever
mentioning it again. Let’s look at Carp, then.

## Who owns what?

The idea surrounding borrow-checked languages is that of ownership and lending.
Capitalist at heart, these languages obsess over the flows of property. This
serves a simple goal: if you know who owns what at any given moment, you also
know when you can get rid of it.

The Carp compiler tries very hard to do as much as possible for you. It inferes
almost any type, the occasional annotation nonwithstanding, for once. And in
the same vein, it tries to make memory management as simple and automatic as
possible.

References are a piece of that puzzle. They are similar to type annotations,
but instead of helping the type checker figure out the types in your program,
it operates on the memory level. It helps let the borrow checker know who you
think should own what. You then engage in a conversation with your compiler,
effectively, working together to build a program with correct memory semantics.

As becomes apparent quite quickly, however, the process is a lot more manual
than type checking. As it turns out, ownership has for a long time been largely
implicit in our programs, and we haven’t spent quite as much time trying to
figure out the underlying theories in this domain as we have for typing.

But let’s get back to the point, shall we? By default, things are owned by the
scope they are enclosed in. Anything that is introduced by a `let` form, for
isntance, is owned by that block. If you pass it to another function as an
argument, it will take over ownership.

```
(defn test [x]
  (IO.println &x))

(defn main []
  ; because string literals are of type &String
  (let [a @"test"]
      (test a)))
```
<div class="figure-label">Fig. 1: `test` takes ownership.</div>

In Figure 1, the function `test` will take ownership over the string that is
being passed to it. This means that it now controls the memory associate to it,
and it wouldn’t be safe for our main function to reuse it. Thus, `main` gives
control over to `test`.

```
(defn test [x]
  (IO.println &x))

(defn main []
  (let-do [a @"test"]
      (test a)
      (test a)))
```
<div class="figure-label">Fig. 2: Calling `test` twice.</div>

In Figure 2, we try to call `test` twice with the same argument. This will
fail to compile, because, as I detailed above, we gave up ownership of `a`. It
is no longer ours, and, as such, we are not allowed to do anything with it
anymore. The Carp compiler will tell us that we’re trying to use a “given-away”
value.

This is not always what we want. Often, you will like to call a function on
a thing without giving up ownership. The program in Figure 2 is already a good
illustration: we want to be able to print a thing without it getting
invalidated under our feet, because we probably also want to work with it. This
is where references come in.

```
(defn test [x]
  (IO.println x))

(defn main []
  (let-do [a @"test"]
      (test &a)
      (test &a)))
```
<div class="figure-label">Fig. 3: `test` now borrows.</div>

The program in Figure 3 will compile. By handing over references to `test`—and
changing the call to `println` accordingly—we essentially tell the compiler
that we˚re not done with `a` just yet. We’ve only borrowed it to `test`.

## A tale of debugging

Especially in the beginning, I often felt like I just threw in random copies
and references until the program compiled, both in Carp and Rust. I haven’t
revisited Rust in a while, but I think it would be easier now that I’ve
mastered the conceptually simpler model in Carp.

What I want to tell you with this addendum is that it’s completely normal if
it takes you some time to develop an intuition about the concept of borrowing.
What you get in return is programs that are very likely to be correct.

Don’t be scared of compiler errors. The Carp compiler is here to help you write
better programs and guide you in the process of establishing a sound model of
the information and memory flow of your applications. Sure, all that red text
looks scary, but it is more of an admission of failure on the compiler’s part
than an indictment.

## Fin

This has been a whirlwind tour of borrow checking in Carp. It has been fairly
high-level, and not many of the implementation details have been talked about.
If you want to know more about the inner workings of the Carp compiler, I
suggest that you read my [blog post from late last
year](https://blog.veitheller.de/The_Carp_Compiler_%28as_of_2017%29.html); it
might just quench your thirst.

What you should’ve gotten from this is a better intuition when it comes to
references. If you haven’t, feel free to shoot me an e-mail or drop by the
[Gitter channel](https://gitter.im/carp-lang/Carp), where we will be very happy
to help you sort through the mess.

#### Footnotes

<span id="#1">1.</span> They might not be optimal, and a few tweaks in the
                        model might result in fewer copies and thus more
                        performant programs. But before worrying about
                        optimality, we should worry about safety.
