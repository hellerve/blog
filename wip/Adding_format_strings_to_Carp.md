A few weeks ago I’ve added one of the more-often requested features to Carp:
format strings. I’m quite proud of how they turned out, and I’d like to talk
about their implementation quickly. As a little bonus, we get to talk about
Carp macros while we’re at it.

## Formatting, type-safely

The topic of format strings has come up on [the Gitter
channel](https://gitter.im/carp-lang/Carp) ever since I joined it.
Eventually, [a Github issue](https://github.com/carp-lang/Carp/issues/108)
was openend, and people started voicing their opinions on the matter. It took a
very long time until I felt confident I understood the problem and an even
longer time until I felt that I was able to tackle it. Even then, there was
a good amount of functionality missing from Carp proper that I had to add
myself. Even as a Core Contributor, this is always scary. I’d rather not make
any API decisions on my own when it concerns the core language.

Carp is by no means big enough for a commitee, so decisions are often made in a
quick chat with Erik and the other stakeholders—i.e., the Carp users interested
in that specific feature. This can lead to mistakes in the design of some
features, and sometimes we have to iterate over a piece of functionality
multiple times until we feel we are getting somewhere.

In the case of `fmt`, multiple different ways of making this work were voiced.
I’m not sure the solution we have now is perfect, or even good, but I believe
it is a good first iteration.

An important consideration was that while `printf` exists, it is a type-unsafe
variable-argument function, and we cannot express that in Carp—at least not
yet. So we needed a type-safe alternative. A fitting interface was quickly
found and implemented, and then we built a macro on top of it to deliver a
clean API to the user. This is what I want to talk about.

## A hard day’s work

As alluded to above [the final PR](https://github.com/carp-lang/Carp/pull/154)
was fairly big, and it broke the behavior of some existing functionality. This
particular beahvior wasn’t used—at least to our knowledge—, and so we consider
it okay to break things; Carp is currently versioned as `0.2`, and I would
discourage anyone who would like to use it in a production context.

The PR introduced a few dynamic functions to the compiler—that is, functions
that are executed at compile time—, a new interface, and a new macro.

The interface is fairly simple: it takes a reference to a string and a generic
thing and returns a string. The string is expected to be a `printf`-style
format string that tells us how to print the second argument.

```
(definterface format (λ [&String a] String))
```
<div class="figure-label">Fig. 1: The interface.</div>

Individual implementations can do whatever they please, of course, but all
primitive types currently call `snprintf` under the hood. See Figure 2 for an
example of how `format` is defined for integers in C.

```
string Int_format(string* str, int x) {
    int n = strlen(*str) + 32;
    char *buffer = CARP_MALLOC(n+1);
    snprintf(buffer, n+1, *str, x);
    return buffer;
}
```
<div class="figure-label">Fig. 2: `format`, as defined for integers.</div>

As you can see, the C code is not quite perfect: we allocate a fixed amount of
space, which is fairly wasteful. I would hope that someone who is less afraid
of programming efficient standard library routines in C than I am will come
along, scoff at my code, and fix it. For now I can only ask for atonement.

This mechanism is also not quite as easily extensible for complex Carp types;
see the next section for a list of caveats.

This provides us with a basis on which we can build a typesafe version of
`fmt`, which is just a variable-argument version of `format`. It is pretty much
just `sprintf`, statically checked and pulled apart by the macro system.


