Welcome (back) to the fourth installment of [Carp Patterns](/carp-patterns), my
series on how to structure Carp code! Today we’re going to look at modules, one
of the fundamental ways in which we structure our code.

I’m going to try to give an overview of how you might want to design your
modules. I understand that the structure of your code and the design patterns
you use more broadly feed into each other, and I can only give you the
patterns that best fit my use cases. I want you to go out and experiment with
different ways of layering and structuring your code if you’re excited to play
around a bit after this!

Let’s have some fun with modules!

## Where do modules come from?

First, let’s look at when and where we create modules to modularize our code.

### Types and implicit modules

Every time you define a type, a module is also conceived. This module will
share the type’s name and contain a constructor, getters and setters, and
other utility functions.

This, together with the fact that you can use `defmodule` multiple times on the
same module name, gives us the first design pattern: to write modules around
our types. All functions that primarily operate on that type should go in the
module named after the type, and they should take the type as their first
argument, unless there is a good reason for them not to.

As an example, let’s consider the [`Map`](https://carp-lang.github.io/carp-docs/core/Map.html)
data type<sup><a href="#1">1</a></sup>. It is a regular Carp type, and it’s
also a module containing all of the map-related functions, such as `Map.get`
and `Map.put` for finding and putting elements into the Map, respectively.
Ideally, there will never be a module called `MapHelpers` that contains all
of the auxiliary functions the main implementation is missing, because any
package can reach into the `Map` module and add to it. Thus, auxiliary modules
are considered an anti-pattern, at least by me.

### Topic modules

Another natural place where modules emerge are when you work on a topic. There
might or might not be a type involved, but if there is it is mostly incidental
to your actual end.

An example for this in the standard library is the [`Test`](https://carp-lang.github.io/carp-docs/core/Test.html)
module. It is used for unit testing. It does contain a `State` type, but that’s
not what it’s about: you want to write tests for your code, and if you need a
type to accomplish it then so be it<sup><a href="#2">2</a></sup>. The type is
only part of the API because the functions in `Test` take and return it, but
you’re not expected to manually work with it in any way.

Other modules might not have a type at all, such as [`Geometry`](https://carp-lang.github.io/carp-docs/core/Geometry.html),
which sports a whole to functions, for converting from radians to degrees and
vice versa.

Basically, topic modules are for all the functions that do not belong to a
type. You don’t want to poluate the global namespace, so you put things in
modules, or even submodules<sup><a ref="#3">3</a></sup>.

## Module annotations

When creating your module, you usually want to expose some functionality to the
wider world. Thus, all functions in Carp are public by default. You can use
annotations to limit their visibility and who can use them, though.

Anything that is exported should have a docstring associated with it. You can
add these by using `doc`. If you want to learn more about documentation, check
out [my first post in this series](http://blog.veitheller.de/Carp_Patterns_I:_Documentation.html)!

Anything you don’t want to be seen should at least be annotated as `hidden`.
This will prevent it from accidentally being rendered in your documentation.
You can optionally also mark things as `private`, but beware that these
functions then cannot be used in macros, since they run in the context of the
calling code, i.e. *outside* of the module. So, if you don’t use macros, mark
everything internal as private, but if you do, you might  have to think about
things a little more—it’s part of the burden of a macro-monger!

## A special note for macros

A word of caution f you work with macros that work on modules: the symbols for
modules that are also types resolve to types first in a dynamic context. You
might for instance want to use `s-expr` to destructure a module, but this
might not always work. Check out this example:

```
; defines a type T and a module
(deftype T)

; you can verify this by typing `:i T` in a REPL

(s-expr T) ; => (deftype T)

; in contrast, just defining a module
(defmodule M)

(s-expr M) ; => (defmodule M)
```
<div class="figure-label">Fig. 2: Using `s-expr`.</div>

So, even though `T` is both a module and a type in Figure 2, we can only get at
the type part! This is a littler frustratring since, this also breaks dependent
code in the [`Introspect`](https://carp-lang.github.io/carp-docs/core/Introspect.html)
module:

```
(Introspect.module? M) ; => true

(Introspect.module? T) ; => false
```
<div class="figure-label">Fig. 3: `module?` being wrong.</div>

Of course this is unacceptable and will likely be fixed in the future, but for
now this is what we have to work with.

## Fin

As always, I hope this was an interesting dive into module in Carp. See you for
the next—and last—installation of this series, on dependencies!

#### Footnotes

<span id="1">1.</span> Fun fact: the current implementation of `Map` was
written by yours truly, and it’s implemented in pure
Carp. It’s a relatively simple but effective
implementation, but I’d like to have another go at it at
some point. You can check out the current version of the
code [here](https://github.com/carp-lang/Carp/blob/master/core/Map.carp).

<span id="2">2.</span> If the types of these modules are internal, as is the
case for `Test.State`, you should probably make the type
internal.

<span id="3">3</span> Usually, the module hierarchies in Carp are pretty flat,
but they don’t have to be. I expect the hierarchies to grow deeper as the
ecosystem grows.
