Welcome (back) to the fourth installment of [Carp Patterns](/carp-patterns), my
series on how to structure Carp code! Today we’re going to look at modules, one
of the fundamental ways in which we structure our code.

I’m going to try to give an overview of how you might want to design your
modules. I understand that the structure of your code and the design patterns
you use more broadly feed into each other, and that I can only give you the
patterns that best fit my use cases. I want you to go out and experiment with
different ways of layering and structuring your code if you’re excited to play
around a bit after this!

Let’s have some fun with modules!

## Where do modules come from?

First, let’s look at when and where we create modules to modularize our code.

### Types and implicit modules

Every time you define a type, a module is conceived also. This module will
share the type’s name and contain a constructor, getters and setters, and
other utility functions.

This, together with the fact that you can use `defmodule` multiple times on the
same module name, gives us the first design pattern: to write modules around
our types. All functions that primarily operate on that type should go in the
module named after the type, and they should take the type as their first
argument, unless there is a good reason for them not to.

As an example, let’s consider the `Map` data type<sup><a href="#1">1</a></sup>.
It is a regular Carp type, and it’s also a module containing all of the
map-related functions, such as `Map.get` and `Map.put` for finding and
putting elements into the Map, respectively. Ideally, there will never be a
module called `MapHelpers` that contains all of the auxiliary functions the
main implementation is missing, because any package can reach into the `Map`
module and add to it. Thus, auxiliary modules are considered an anti-pattern,
at least by me.

### Topic modules

Another natural place where modules emerge are when you work on a topic. There
might or might not be a type involved, but if there is it is mostly incidental
to your actual end.

An example for this in the standard library is the `Test` module. It is used
for testing. It does contain a `State` type, but that’s not what it’s about:
you want to write tests for your code, and if you need a type to accomplish it
then so be it<sup><a href="#2">2</a></sup>.

#### Footnotes

<span id="1">1.</span> Fun fact: the current implementation of `Map` was
                       written by yours truly, and it’s implemented in pure
                       Carp. It’s a relatively simple but effective
                       implementation, but I’d like to have another go at it at
                       some point. You can check out the current version of the
                       code [here](todo).

<span id="2">2.</span> If the types of these modules are internal, as is the
                       case for `Test.State`, you should probably make the type
                       internal.

