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
discourage anyone who would like to use it in a production
context<sup><a href="#1">1</a></sup>.

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

Let’s take a look at how we would implement that in the macro system. Keep in
mind that it is more of a fully-fledged Lisp interpreter than a regular Lisp
macro system.

What we are going to try to do is split the string on every format instruction,
honoring escapes like `%%`, and matching them up with the arguments. Then we
will instruct the compiler to join the strings back together after `format` has
been called on them. This results in a somewhat expensive but extensible
operation, and the interface selector will ensure that it is typesafe.

Let’s start with a skeleton and work our way up from there.

```
(defdynamic fmt-internal [s args]
  (let [idx (String.index-of s \%)]
    (if (= idx -1)
      (list 'copy s) ; no more splits found
      ; do something
    )))

(defmacro fmt [s :rest args]
  (fmt-internal s args))
```
<div class="figure-label">Fig. 3: A `fmt` skeleton.</div>

Alright, we have written a macro that calls a dynamic function, because we need
recursion. The dynamic function checks only the base case for now: if there are
no percent formatters in the string anymore we just return a copy of the
string. So far, so good. The second easiest case is if we encounter an escaped
percent sign, i.e. `%%`. Let’s take care of that.

```
(defdynamic fmt-internal [s args]
  (let [idx (String.index-of s \%)
        len (String.count s)]
    (if (= idx -1)
      (list 'copy s) ; no more splits found
      (if (= \% (String.char-at s (inc idx))) ; escaped %
        (list 'String.append
              @"%"
              (fmt-internal (String.substring s (+ idx 2) len) args))
        ; other cases...
    ))))
```
<div class="figure-label">Fig. 4: Honoring escapes.</div>

The snippet above in Figure 4 checks whether the character after the escaper
is another percent sign, and if it is, emits an append instruction with a
percent sign and recurses into the next iteration of the dynamic function, with
the rest of the string.<sup><a href="#2">2</a></sup>

The next case is the last one: in it we intersperse an argument. If it’s the
last one, we call `format` and be done with it. Let’s implement that.

```
(defdynamic fmt-internal [s args]
  ; prelude as seen in Figure 4
  (let [next (String.index-of (String.substring s (inc idx) len) \%)]
    (if (= -1 next)
      (list 'format s (car args)))
      ; the long road
    ))
```
<div class="figure-label">Fig. 5: Formatting, the simple case.</div>

All we have to do is check whether there is another `%` character in the rest
of the string. If there isn’t, our new case takes effect. Note that I’ve
omitted the other cases in Figure 5 to make the snippet a little smaller.

The most complicated case is as always kept for last: we emit the same format
instruction, but also recurse and add an append.

```
(defdynamic fmt-internal [s args]
  ; prelude as seen in Figure 4
  (let [next (String.index-of (String.substring s (inc idx) len) \%)]
    (if (= -1 next)
      (list 'format s (car args)))
      (let [slice (String.substring s 0 (+ (inc idx) next))]
        (list 'String.append (list 'format slice (car args))
                             (fmt-internal (String.substring s
                                                             (+ (inc idx) next)
                                                             len)
                                           (cdr args))))))
```
<div class="figure-label">Fig. 6: Formatting, the complex case.</div>

Holy parentheses, batman! What are we doing here?

First we take the slice we currently need, which leads up to the next percent
sign. Then, we emit a call to `format` with that slice and the first argument
and recurse. In the end, we’ll append it all back together.

This is all the code we need, sans error checking. The real implementation also
checks whether we have the correct number of arguments for our format string,
but I’ll leave this as an exercise to the reader.

## Caveats

There are multiple glaring deficiencies with this implementation of `fmt`, as
far as I’m concerned. First and foremost, it will result in a lot of appends
on a lot of possibly tiny strings, and this sounds very costly to me. This
could—and hopefully will—be resolved with a smarter algorithm; for now I felt
the need to give Carp’s users a tool that scratches their itch. Next up will
be an actual remedy.

Another deficiency as I see it is that while the tool is certainly extensible,
it suffers from C’s `sprintf` syntax, especially for complex datatypes. I would
like to be able to `format` a two-dimensional vector and be able to give
different format specifiers to both arguments—currently this is not easily
possible. Sure, you could write your own extension to C’s formatting options,
but this sounds incredibly messy to me.

## Fin

In sum, `fmt` currently tries to chart uncharted territory for Carp and,
unsurprisingly, does so imperfectly. I’d be happy for any kind of input,
especially from users and people who’ve done something like this
before<sup><a href="#3">3</a></sup>.

I hope you enjoyed this installment of my series on Carp. If you’re
interested in contributing or just checking it out, its
[repository](http://github.com/carp-lang/carp) and the Gitter channel—linked to
in the introduction—are great places to start.

#### Footnotes

<span id="1">1.</span> Fun fact: I found bugs in the code while I wrote this
                       article. That’s how alpha it is.

<span id="2">2.</span> A call to `cond` would make this macro a little bit
                       prettier, but `cond` is implemented as a macro itself
                       and thus cannot be used in the evaluator.

<span id="3">3.</span> I’d also be up for stealing Rusts formatting syntax,
                       but I’m not a Rust user and thus do not know the
                       system’s upsides and downsides well enough to know
                       whether copying it would be of merit.
