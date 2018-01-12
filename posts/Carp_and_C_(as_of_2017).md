I’ve written a bunch of blog posts about Carp now that have proved to be
surprisingly popular, and people have been demanding follow-ups on a
variety of subjects. One of the features that has been the subject of multiple
requests is C interoperability, which has been notably absent from my
[whirlwind tour of the language](https://blog.veitheller.de/Carp.html), mostly
because the post was too long already. Today I want to fill that hole and show
you both how C interop works, and how to write packages with a clean interface.
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
exposed to Carp. So what's the difference?

Opaque types can only be used by the type system. No support structure will be
generated for them, and, even if they’re only aliases, you will not be able to
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
scheme of the functions.  This had to happen because we mangle the names by hand, which
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
Maybe you’ll understand why we haven’t changed it yet when you read through it.
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

First, we will have to define the data type. To make the type name idiomatic
Carp, we alias it to `Uint8` in C.

```
typedef uint8_t Uint8;
```
<div class="figure-label">Fig. 5: Renaming our type in C.</div>

In Carp, we register the type as an opaque type, because, as we remember from
the last section, we cannot make a thin type from anything other than a struct.
We will also have to include the C file. In my native modules—which are the
canonical and only examples for now—I decide to put all the native code into a
`lib` directory, and if I need more than one Carp file, put them in a `src/`
directory. I’ve put the C above in a file called `stdint_helper.h` inside
`lib`.

```
(local-include "../lib/stdint_helper.h")

(register-type Uint8)
```
<div class="figure-label">Fig. 6: Registering our type in Carp.</div>

The `..` is necessary for now, because the compiler will put all of the build
files into a directory called `out`. This is subject to change in the future;
your mileage may vary.

Alright, let’s define some functions, starting with converters from and to
integers and the mathematical functions `+`, `-`, `*`, and `/`. While we’re at
it, we can also define the comparison operations `=`, `>`, and `<`.

```
#include <core.h>

Uint8 Uint8_from_MINUS_int(int x) { return (Uint8)x; }
int Uint8_to_MINUS_int(Uint8 x) { return (int)x; }

Uint8 Uint8__PLUS_(Uint8 x, Uint8 y) { return x + y; }
Uint8 Uint8__MINUS_(Uint8 x, Uint8 y) { return x - y; }
Uint8 Uint8__MUL_(Uint8 x, Uint8 y) { return x * y; }
Uint8 Uint8__DIV_(Uint8 x, Uint8 y) { return x / y; }

bool Uint8__EQ_(Uint8 x, Uint8 y) { return x == y; }
bool Uint8__LT_(Uint8 x, Uint8 y) { return x < y; }
bool Uint8__GT_(Uint8 x, Uint8 y) { return x > y; }
```
<div class="figure-label">Fig. 7: Registering functions on our types.</div>

As you can see, we do a fair amount of mangling by hand, replacing the actual
operator symbols with their mangled names. Because we use the `bool` type,
which is not part of C by default, and we will need it in the future, we also
include the `core` header which is supplied by Carp. The Carp compiler will
ensure that it is on the path at compilation time.

These are repetitive, but necessary. The amount of repetition could also be
decreased by using C macros, but I chose not to for our sake. This is but a
tutorial after all.

In Carp land, we register all of the functions we just defined. Because we
prefixed them with the type name, we can do so in a module. We will also
define a few functions based on our new primitives.

```
(defmodule Uint8
  (register from-int (λ [Int] UInt8))
  (register to-int (λ [UInt8] Int))

  (register = (λ [Uint8 Uint8] Bool))
  (register > (λ [Uint8 Uint8] Bool))
  (register < (λ [Uint8 Uint8] Bool))
  (register + (λ [Uint8 Uint8] Uint8))
  (register - (λ [Uint8 Uint8] Uint8))
  (register * (λ [Uint8 Uint8] Uint8))
  (register / (λ [Uint8 Uint8] Uint8))

  (defn >= [a b] (not (< a b)))
  (defn <= [a b] (not (> a b)))
)
```
<div class="figure-label">Fig. 8: A beautiful Carp module.</div>

This is a little bit contrived—usually we would like to have `>=` and `<=` as
primitives—, but it is nice to see that composition and building upwards are
in fact possible.

To showcase how we allocate memory, let’s add an implementation of the `str`
interface. The `str` interface is the generic way of converting a data type
to a `String` and, thus, quite useful. For this, we need to allocate some data.
Luckily, `core.h` provides us with just the right function: `CARP_MALLOC`.

```
string Uint8_str(Uint8 x) {
    char *buffer = CARP_MALLOC(64);
    snprintf(buffer, 64, "Uint8(%hhu)", x);
    return buffer;
}
```
<div class="figure-label">
  Fig. 9: A naive stringification for our datatype.
</div>

To cut a few corners, we will just allocate 64 bytes. This will be plenty, even
wasteful, but for our purposes it is good enough. We also define our format to
be `Uint8(<data>)`, so that we know whether it is a regular `Int` or one of our
types. This is not required, but I consider it good practice. An alternative
way of writing that is with S-expressions, i.e. as `(Uint8 <data>)`, but I seem
to default to the first one; maybe I’m just not a good Lisper after all.

We can register that function like any other function in Carp. When we use it
in our code, Carp will ensure that our program does not leak and our string
will be deallocated.

Before we move on to the other aspects of writing packages in Carp, namely
packaging and testing, let me have another word about allocations. Currently,
Carp uses the system allocator. This means that on some embedded systems,
using `CARP_MALLOC` will result in compilation errors. In these cases, our
advice is to refrain from using it and compiling with `--no-core`, which will
prevent the core library from getting loaded. Currently, there is no
alternative drop-in replacement for embedded systems. We hope this will be
resolved sooner or later, but it is not an immediate priority, and we request
your patience.

## Packaging

I talked a little bit about package structure earlier. Now I want to give you
a more complete picture of the project layout I use, and why I use it. In
general, I like to be very explicit about structure and err on the side of too
many folders. Again, this is just my personal style, but it seems to work.

```
.
├── README.md
├── lib
├── my_top_level_module.carp
├── src
└── tests
```
<div class="figure-label">Fig. 10: A tree view of my project structure.</div>

The top level always stays the same in my Carp projects. I have a `README.md`
with a—hopefully informative—outline of the project, what it does, and how to
get it. Because Carp does not yet have a package manager, working with external
packages involves a lot of cloning from Github.

I then provide you with three directories, of which two are optional. The
`tests` directory is there, and it shouldn’t be optional—although I’ve been
known to break my own rules, especially when I’m just dabbling. Depending on
the complexity of the project, we will also want a `lib` directory for any C
headers, and a `src` directory for any source file that our top level project
file uses.

I usually exclude the `out` directory from git through an entry in
`.gitignore`. Some people like to push build artifacts, but they are subject to
change even in a “transpiled” language like Carp.

This entire layout is nothing new. It is the standard layout in many other
languages, and it works well. I am currently working on the most urgent
prerequisites for a package manager, and once that one is in the works, I hope
to be able to codify a simple, extensible, one-size-fits-all way of organizing
projects. Again I have to ask you to be patient with us.

## Testing

Once we’ve found a way of organizing our sources, and maybe written a little
bit of glue code to get started, we will want to start writing tests. Even in a
module like `Uint8` that is largely driven by already-working C code we at
least need some smoke tests that tell us that everything is hooked up
correctly. So, let’s write a few!

```
(load "Test.carp")
(use Test)

(load "stdint.carp")
(use Uint8)

(defn main []
  (with-test test
    (assert-equal test
                  (from-int 3)
                  (- (from-int 2) (from-int -1))
                  "subtraction seems to work"
    )
  )
)
```
<div class="figure-label">Fig. 11: Writing our first test.</div>

This test file assumes to be compiled from the project’s top level directory,
for example through `carp -x tests/<filename>`.  This should pass, and leave a
bit of hopeful green text for us to think of when our tests start to fail. From
here, we can build up a more or less complete battery of tests like [this
one](https://github.com/hellerve/stdint/blob/master/tests/uint8.carp). You’ll
likely find fewer test cases than you would like in my test suites, but I take
care to test all of the functions at least once. You are most welcome to write more
than the bare minimum, though.

## Fin

In this blog post, I hopefully gave you enough of a taste of what it’s like to
write a Carp module, with C extensions or not, to build up confidence so that
you can also try experimenting. While most of the work on Carp is contributed
back to the compiler or standard library these days, this is by far not the
only way to get started with it. You can write own projects and experiments in
Carp, and even structure them in such a way that makes it easy for other
Carpenters to reuse them.

As always, if you have any questions, suggestions or comments, holler at me
via mail!

#### Footnotes

<span id="1">1.</span> Full disclosure: it was written by yours truly.

<span id="2">2.</span> As programmers we should be acquainted with the fact
that we rarely truly know what we’re doing, even if we’re sure of it.

<span id="3">3.</span> I’m making up the lingo as I go here, don’t blame me.

<span id="4">4.</span> I hope that someone will throw a nifty preprocessor
or something our way, but as of now I’m perfectly content with the mechanisms
as they are. They are way better than all of the FFIs that I’ve designed,
anyway.
