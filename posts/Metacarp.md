---
title: "Metacarp"
date: 2026-09-12
---

For the past few months, I have been writing a new compiler for
[Carp](https://github.com/carp-lang/Carp). It is called
[Metacarp](https://github.com/cyberwitchery/metacarp), because it is written
in Carp and it compiles Carp and I’m better at writing code than naming things.

This is not the first Carp compiler, obviously. The reference implementation
is written in Haskell and has served us well for about a decade. I’ve
[written about it a long time ago](https://blog.veitheller.de/The_Carp_Compiler_%28as_of_2017%29.html).
That post is old enough to attend primary school now and describes a rather
different language. We’ve worked on Carp extensively since then, and both the
compiler and the library ecosystem have moved on. Just look at [the
Carpentry](https://carpentry.dev) these days and you’ll find a library for many
of your needs.

Metacarp is a second implementation of that language. It compiles the reference
suite, compiles itself, and then compiles itself again to byte-identical C. It
is split into independent libraries for the compiler phases, has a second LLVM
backend, keeps compiler sessions alive between inputs, and can incrementally
compile code into a running JIT.

It is also not a drop-in replacement yet. Its diagnostics are different, parts
of it are quite wonky, and some programs still find exciting ways to fall off
the edge.

So this is the announcement. Let’s have a look around!

## The compiler

Metacarp is an ordinary command-line compiler first. You build it with the
reference implementation:

```
carp -b --optimize main.carp
```

and get `out/carp-compiler`. Point that at a Carp program and it will write one
C translation unit:

```
./out/carp-compiler -c <path_to_stdlib> examples/squares.carp
```

Or ask it to involve Clang and run the result directly:

```
./out/carp-compiler -x -c <path_to_stdlib> examples/squares.carp
```

That example loads the regular Carp standard library, expands its macros,
infers and specializes the program, checks its ownership, emits C, builds it,
and prints the sum of the squares of the even numbers from one to ten. It’s
quite a lot of machinery to print `220`, sure, but it’s also quite fun!

Metacarp understands all the things that make implementing Carp annoying:
the compile-time language and macros, Hindley-Milner type inference,
interfaces, monomorphization, pattern matching, closures, and the ownership and
borrow rules. It derives the copiers and deleters managed values need and
inserts calls to them before producing C.

It can load the existing Core and compile the existing programs. It works on [the
Carpentry libraries](https://carpentry.dev). It’s not 100% compatible yet, but
the things that don’t quite behave as expected are also the things you only
encounter quite late in your journey.

## The compiler as libraries

The command is only one of the ways to work with the compiler. Metacarp is
around 42,000 lines of Carp as I write this, spread across libraries that
implement the individual phases:

```
source registry
  -> module loading
  -> surface parsing
  -> macro expansion
  -> name resolution
  -> type inference
  -> specialization
  -> ownership planning
  -> backend lowering
  -> C
```

This is a fairly standard compiler pipeline. The fun part is that these are
actual library boundaries.

They have their own data models, entry points, tests, and errors. You can use
the parser without the type checker, inference without a backend. You can run
ownership planning without LLVM or C (yes, LLVM exists too, more below).

I’m not going to describe every single phase, but your coding agent would
probably call each of them “load-bearing”. They feed into each other, but try to
be sensibly sliced.

Each boundary returns structured information. What does that mean? It means that
a parse failure contains source spans, and only the command-line driver turns it
into terminal output. Everything underneath it can work on it at the machine
level.

I think this is very cool! It also means that one can stop the pipeline at any
point, inspect what happened, or use the compiler for something that is not
“take this file and give me an executable”.

Naturally, I did, deviant that I am.

## The compiler as a pet

The `carp-session` library keeps a compiler alive.

Creating a session loads, expands, resolves, derives, and infers Core once. On
top of that immutable base it keeps a set of definitions supplied by its
client. A notebook cell can then be checked transiently against both without
becoming part of the session:

```
Core, loaded once
    + committed definitions
    + this cell
```

The client can commit a definition, replace it, or remove it. When a definition
changes, the session finds the definitions that depend on it and rebuilds the
affected view. A failed change does not poison the compiler, the candidate
state is thrown away and the last known good one remains available.

This gives notebook-style code expected behavior. One cell can
define a function:

```
(defn twice [x] (* x 2))
```

and another can ask for its type, use its completion info, compile `(twice 21)`.
The second cell does not need to paste the first one in front of itself or
reload the standard library. It’s just there.

The session API answers with definitions, types, diagnostics, documentation,
completions, expansions, and source-relative spans. It also has hooks that
stop after inference, after ownership planning, or after lowering. This is the
machinery behind [gt4carp](https://github.com/carpentry-org/gt4carp), where a
Lepiter page gets a warm compiler session and snippets behave like pieces of
one little evolving program. We’ll talk about that one in the future.

Stateful compilation sounds obvious when described this way. Implementing it
was anything but obvious for me.

Batch compilation gets to infer one closed program, use its solver state, and
throw everything away. A persistent session combines facts produced by
different runs and re-uses them across generations. At one point this
worked perfectly until a later cell made a previously unused polymorphic
definition reachable, at which point specialization found a type variable
whose substitution had disappeared several requests ago. If this sentence makes
no sense, welcome to my world.

The first correct fix made compiling one cell against Metacarp itself take
about thirty seconds. Caching the immutable half brought that back down to
about two hundred milliseconds on the same machine. First make it run, then
make it fast at its finest. I’m still surprised it actually worked!

## The native compiler

The regular backend emits C. There is also an LLVM backend which consumes the
same lowered program, including the same ownership plan, and produces LLVM IR
instead.

To be very clear, this isn’t a toy path. The LLVM driver has parity with
the C driver on the reference suite, including arrays, strings, closures,
generic sum types, globals, pattern matching, derived memory operations, and
the C templates used by Core. It can emit an object file and link a normal
program, but the more entertaining client is the JIT.

This is the magic of the compiler-as-a-set-of-libraries bet in full effect.

The session JIT keeps an LLVM context and ORC instance alive beside the
semantic compiler session. Definitions and their machine code survive across
cells. A later cell only needs to specialize and lower definitions which have
become newly reachable, emit a small LLVM module, and publish its symbols into
the running process. If publication fails, that module can be removed again
without breaking the preceding cells.

Against the real Core on my machine, the first cell takes around a second and a
warm cell around 60 milliseconds. The equivalent emit-C, invoke-Clang, and run
path takes around 380 milliseconds per cell. This is all local and the numbers
are probably garbarge, but the trend absolutely isn’t.

This is the magical part for me. The compiler is written in Carp. The compiler
session and JIT driver are written in Carp. The program being compiled is Carp.
And all the self-hosting generations at some point converge and produce the same
output, byte for byte.

Computers are good sometimes, especially when they don’t talk back.

## Here be bugs

Metacarp is usable, but it is not finished, to whatever degree a compiler can
be finished.

It stamps the host architecture and operating system into the build path, so
cross-compilation is mostly an aspiration. Its delete placement is scope-based
rather than liveness-based, which can keep large values around longer than
necessary. A binding consumed on one control-flow path and then reassigned can
still leak the reassigned value. Some caches are process-global, which is
particularly fun when multiple JIT clients believe they are independent.

If these sound serious, once again: welcome to my world.

The diagnostics are its own. Some are better than the reference compiler’s,
some are worse, and none are byte-compatible. They’re definitely not as good or
as friendly as I’d like.

The implementation accepts the programs in the reference suite, but calling it
a drop-in replacement would invite my readers to produce a counterexample before
they finish parsing this sentence.

Please do look, though. Plenty of people use it and think it’s interesting, and
I can handle the bug reports.

## Fin

Metacarp is a self-hosting Carp compiler, a collection of compiler libraries,
a warm incremental compiler service, a C compiler, an LLVM compiler, and a JIT.
It compiles the existing language and Core, rather than a polite little subset
invented for a demo. It can compile a file, answer questions about one notebook
cell, or hand a native function to a running program.

It can also make and break my evenings effortlessly.

I’ve had an enormous amount of fun building it. It has reached the point where
other things can be built on top of it, and I do, quite a bit.

You can [find it on GitHub](https://github.com/cyberwitchery/metacarp). It is
cool, magical, weird, and buggy. That seems like a good state for a new compiler
for Carp, a language that is cool, magical, and weird, to be in.
