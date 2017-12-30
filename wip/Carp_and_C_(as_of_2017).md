I’ve written a bunch of blog posts about Carp now that have proved to be
surprisingly popular, and people have been demanding follow-ups on a
variety of subjects. One of the features that has been the subject of multiple
requests is C interoperability, which has been notably absent from my
[whirlwind tour of the language](https://blog.veitheller.de/Carp.html), modtly
because the post was too long already. Today I want to fill that hole and show
you both how C interop works and how to write packages with a clean interface.
To make it simple, I’ll use a package that I’ve already written which wraps
[stdint](https://github.com/hellerve/stdint).

We’re not going to write much Carp today, instead focusing on the C side. I
will guide you on how to write C that is suitable for interoperability,
package it up nicely, and test it with the Carp testing library that is part
of the standard library of Carp<sup><a href="#1">1</a></sup>.

## API prerequisites

Let’s get the API out of the way first, so that we can move on to the more
interesting parts. You can register types, variables, and functions defined
in C in your Carp code. You can also register macros if you pretend they’re
either variables or functions, but that’s going to hurt pretty quickly,
especially if you then try setting the variable or using the function in a
higher-order context. What I’m saying is “don’t do that, unless you’re very
sure you know what you’re doing”<sup><a href="#2">2</a></sup>.

```
typedef int MyInt;

typedef struct MyStructure {
  MyInt member1;
  float member2;
} MyStructure;
```
<div class="figure-label">Fig. 1: A perfectly useless type definition.</div>

In Figure 1 we define two kinds of types, an alias and a struct. If we want
to use those types in Carp, we have to register them. Observe their
registration in Figure 2.

```
; registering an opaque type
(register-type MyInt)

; registering a thin type
(register-type MyStructure [member1 MyInt, member2 Float])
```
<div class="figure-label">Fig. 2: The two ways of registering types.</div>

As you can see in Figure 2, there are two ways of registering types: you can
either register “opaque types”, i.e. types that are black boxes for Carp, or
“thin types”<sup><a href="#3">3</a></sup>, i.e. types whose implementation is
exposed to Carp. So what is the difference?

Opaque types can only be used by the type system. No support structure will be
generated for them and, even if they’re only aliases, you will not be able to
use them as you used the aliased types, as is the case for `MyInt` above.
Integer addition will cease to work with them, and you’ll have to reimplement
every operation you want to perform with them. And here’s the catch: as of the
time of writing—the end of 2017—, you cannot define non-opaque type aliases.
The `register-type` syntax only supports opaque types and structures. Bummer.

Let’s move on to the other way of registering types anyway. Thin types are
quite comfortable to use, because registering them will also result in the
creation of lenses implicitly. As I detailed in [my first blog post on
Carp](https://blog.veitheller.de/Carp.html), lenses are functions for
retrieving, setting, and updating members in a structure. This makes them
easier to work with, and useful on their own.

```
int MyInt_to_MINUS_int(MyInt x) {
  return (int) x;
}

MyInt MyInt_inc(MyInt x) {
  return x + 1;
}
```
<div class="figure-label">Fig. 3: Making `MyInt` less useless.</div>

The first thing you’ll notice in Figure 3 will be the somewhat awkward naming
scheme of the functions. This is so because we mangle the names by hand, which
is a bit tedious, but, in my opinion, bearable<sup><a href="#4">4</a></sup>.

It will all start to make sense again when we look at Figure 4, in which we
register the functions we’ve just created.

```
(defmodule MyInt
  (register to-int (Fn [MyInt] Int))
  (register inc (Fn [MyInt] MyInt))
)
```
<div class="figure-label">Fig. 4: Modulizing C.</div>

What we’re doing here is demangling the name: the prefix `MyInt` will become
the module name, the mangled symbol `_MINUS_` will become a dash, and so on.
The isomorphism between Figure 3 and Figure 4 should still be somewhat
apparent.

I know that the skin will crawl for some of you already. In our defense, we are
well aware that nameclashes are perfectly possible with this mangling strategy,
and [there is a Github issue](https://github.com/carp-lang/Carp/issues/120).
Maybe you’ll undertand why we haven’t changed it yet when you read through it.
Spoiler: it has to do with the fact that we’re mangling by hand and that would
be more complicated if we adopted C++ style name mangling. Also there are
conflicting standards, because nothing is ever easy.

Now that we have the tools to work C into our Carpentry, let’s try and do
something useful with it!

## Writing fish-friendly C

As I’ve said above we're going to try and wrap
[stdint.h](http://pubs.opengroup.org/onlinepubs/009695399/basedefs/stdint.h.html),
or at least parts of it. A fuller implementation can be found in the Github
repository I linked to in the beginning.

For the purposes of this blog post, let’s focus on `uint8_t`, the arithmetic
operations that are defined on it, and its conversions to and from Strings
and Ints.



## Packaging

## Testing

## Fin

#### Footnotes

<span id="1">1.</span> Full disclosure: it was written by yours truly.

<span id="2">2.</span> As programmers we should be acquainted with the fact
that we rarely truly know what we’re doing, even if we’re sure of it.

<span id="3">3.</span> I’m making up the lingo as I go here, don’t blame me.

<span id="4">4.</span> I hope that someone will throw a nifty preprocessor
or something our way, but as of now I’m perfectly content with the mechanisms
as they are. They are way better than all of the FFIs that I’ve designed,
anyway.
