Today I want to talk to you about the Carp compiler. After [my last blog
post](http://blog.veitheller.de/Carp.html) spent some time on the front page
of various internet forums and incited helthy and not so healthy discussions,
my inbox was filled with requests for clarification. In general, people seem to
be fairly interested in how the compiler works; miscommunication—possibly on my
part—led to a confused picture of how it functions in some corners of the
internet, and so I would like to give you some general information about it.

One other thing; I’m not the original author of the language, nor am I it’s
principal maintainer. I never claimed to be, but the pure fact that my blog
post was what people read made people jump to conclusions; I do not want to
usurp anyone else’s work, and so here is a shout-out to the brilliant and
nice Swede [Erik Svedäng](), who you can usually find on the [Gitter
channel](https://gitter.im/carp-lang/carp), which is our primary means of
communication these days. He is the author of Carp and the person I bug with my
questions. If you are interested in learning more about Carp or just hang
around and chat, I suggest you go there.

Now for the main event: let’s talk about the compiler, from front to back.

## The parser

The first stop in any compiler is the parser; Lisp parsers are generally
straightforward, and so I don’t want to spend too much time on its
implementation. With the implementation language of the compiler being Haskell,
we use Parsec as our primary parser framework. This means that we don’t go the
more traditional route of using a Bison/Yacc-compatible BNF grammar but rather
use a parser combinator framework. I generally prefer parser combinators,
although admittedly BNF grammars make for a nice specification of a language.
It spits out an enriched AST—the regular AST is called `Obj`, the extended
version `XObj`—that stores a little bit of additional information, such as
where the node is coming from (line, column, and file information) and a unique
identifier that we will need in the emitter.

The only thing that I consider of note here is that the reader macros `@` and
`&`, which you can read about in my previous blog post, are defined in the
parser. Carp does not have an extensible reader macro system, and is as such
closer to Clojure than Common Lisp.

## The Eval module

This is where it gets a little icky. The single largest module in the Carp
codebase is the `Eval` module. It is around a thousand lines of code, which
isn’t terrible. It’s pretty long for a Haskell module, but it’s not the largest
I’ve seen. What’s bad about it is that it does a lot of things and has no clear
boundaries around its goals. A nice way to put it would be that it’s “the heart
of the compiler”, but I’d argue that it’s just in dire need of a refactor.

Once it receives the parsed source code in AST form, it will perform a fold
over it to generate and execute Commands. Commands are the concept that makes
Carp’s REPL so different<sup><a href="#1">1</a></sup>, more of a view into the
compiler than an interactive expression evaluator. There are two commands that
will be generated at this time: `ReplEval` and `ListOfCallbacks`.

`ReplEval` is where it’s at, so I will start with `ListOfCallbacks`. It is
a simple data structure that holds a list of commands that will be performed
in sequence, enabling us to chain shortcuts. You can see that in action when
typing, for instance, `:bx` at the REPL, which is a shorthand for writing `:b`
and `:x` in sequence, or `(build)` and `(run)`. It is thus just a convenient
wrapper for us at the REPL.

As you might have guessed at this point, this leaves `ReplEval` with the
thankless job of doing _literally everything else_. It will be passed into
the `eval` function, which performs some of the things that you might expect
an interpreter to do, but some are suspiciously absent—this is just the first
pass after all, and we are not trying to interpret the source code so much as
making it palatable for our backend.

## ev(a|i)l

I’m not going to cover all the thing that the evaluator is doing, but I’ll try
to give you a brief overview of an abomination of a function that is around 450
lines of code long, and spends most of its time in various `case` statements.
That’s not quite as horrifying as CPython’s infamous `switch`, but it’s bad
enough for me to rant—sorry Erik, but I know you feel similarly.

*Disclaimer: this section is fairly mean to Carp. I love the language, but I
try to be open about its current imperfect state. This is not a marketing
campaign.*

First, the evaluator will check what general flavour of `Obj` it has received.
If it’s a list, it will call the local function `evalList`, which does most
work. The other two flavors it cares about are symbols and arrays, which get
their own special functions; if it receives something else, it will just parrot
it back to the caller; this is one of the REPL’s “features” that you will
confuse you the first time you run it, and we should probably do something
about it.

If we have a symbol in front of us, `evalSymbol` will try to look it up, and
if it fails, give you an error. If it succeeds, it will return the value, but
the REPL prints... nothing. Let’s try not to think about that one too much. It
makes sense if not used on the top-level, where we actually might need
bindings, for example in dynamic functions—we’ll talk about those in the
section about macro evaluation.

Arrays are weird as well, at least on the top-level. They are homogeneous by
design, because, you know, they’re C arrays. If you work on the REPL, however,
the type checker will not take effect, so we can do things such as writing
`[1 "hi" \2]`, and Carp won’t think twice about it. That’s all well and good,
except it really isn’t in the REPL sense. For macros, again, this makes sense,
as we will see later.

Alright, I’ve pushed it away for long enough: it is time to talk about
`evalList`.

It uses two more data structures that we haven’t talked about before, and that
we will need to cover to understand any of what follows: the environment `Env`
and the similar, but not quite the same, type environment `TypeEnv`. The type
environment registers and saves types and is needed for type inference and
checking. The regular environment is used for values and functions.

Let’s quickly go through a few of the behaviours of the function, just to give
you an intuition about how it operates: if `evalList` encounters a function or
value definition, it will expand macros in the form, try to infer its type,
save the definition, and move on; if it encounters a type definition, it will
save this type in its type environment and generate getters, setters, updaters,
and `str`, and `copy` for this type and register those as well; if it
encounters external type of function definitions that are implemented in C and
exposed through `register` or `register-type`, it will do the same as for
regular values, but take your definitions at face value and don’t try to check
anything<sup><a href="#2">2</a></sup>.

This should give you a little insight into how this function generally
operates; it is, however, also responsible for macro expansion and activating
the type checker as needed. Let’s talk about both of those before entering C
land.

### Macro evaluation

First off, the bad news: Carp’s macro system is not currently hygienic<sup>
<a href="#3">3</a></sup>. It also does not have any `gensym` capabilities yet.
Generally, it is a system of its own, closer to Clojure than anything else, but
with a few twists and embellishments of its own.

There are two general concepts that macro evaluation has to deal with: macros
and dynamic functions. Macros are fairly straightforward: they are simple
rewrite rules that, given one or more forms as input, return an output form.
The magic happens in `expand`, where a single form gets rewritten. I don’t want
to dwell on this for too long, just know that it calls `eval` whenever it finds
a macro, which calls `apply`, which calls `eval` again, and it all makes sense,
somehow. Bottom line is that I sometimes think I know how it works, but I can’t
keep it in my head long enough to reason about it deeply. Sorry if that is a bit
of a let-down.

Dynamic functions are similar, but they actually evaluate their arguments
before being applied. That simple idea makes them similar to normal functions
and useful when applied recursively, and for proper metaprogramming. Macros are
thus usually only useful for simple rewrites or as entry points to dynamic
functions.

### Type inference and checking

We’ve worked with a dynamically typed AST thus far, but this won’t do for
compilation. Instead, we’re now going to infer our types, based on only the
type information that we have from the standard functions and any annotations
the user might have left behind.

The type system relies on a simple unification solver to infer the types that
we need to set. Generic types are still valid at this point. The inference
engine just annotates the objects it receives and, perhaps surprisingly, takes
care of memory management information. It also returns any dependencies the
inferred function might have from Concretization. If those last two tasks don’t
make any sense to you, don’t worry, I’ll go over them in the following
sections.

The annotation procedure relies entirely on generating and then solving the
contstraints. Generating constraints is simple, and it takes all that we know
from our function types. Let me give you a simple example: given the statement
`(= a b)`, we know that the type of `a` must be the same as `b`. We will
generate this constraint for our solver. Should either one of the two variables
be used in a non-generic setting somewhere we can solve the constraints, and
assign both of the variables the same type. This simple yet powerful mechanism
turns out to be surprisingly useful.

Should the solver fail or find any typed holes<sup><a href="#4">4</a></sup>,
the engine will produce an error, otherwise the solver will produce a
`TypeMappings` object, which is just a mapping from generic to concrete types.
The inference engine then assigns those types to the values, and goes on to
Concretization and memory management.

#### Concretization

Concretization is a fairly large module again, but not nearly as big as the
evaluator. It will take care of emitting multiple versions of the same
polymorphic function with different concrete types, one for every usage in
our code. If you define a function `sum` that takes a list of numbers and
sums all of them, for instance, and then use them with `Double` and `Int`
types, it will both generate the appropriate functions and symbols for the
specialized code, as well as change the code to use the specialized names
instead.

This is where the generation of dependencies kicks in; dependencies are
returned from type inference if additional specialized functions need to be
generated.

This leads us to the last item on our list: memory management.

#### Memory management

The `ManageMemory` module is responsible for finding out if and when any
destructors/deleters are needed. Like the type inference, concretizer, and
evaluator, it is yet another tree walking function<sup><a href="#5">5</a>
</sup>.

Now, I haven’t actually worked on this module all that much yet, either, so
you’ll have to do with what I observed and what I found out by having Erik
explain its magic to me. Pardon if this is not as instructive as you’d hope.

// TODO: actually talk about it.

## Emitting C

While Erik would like the compiler to use an LLVM backend at one point I
personally am a fan of the C backend. Partly this is because it emits fairly
legible code and, all things considered, it is easier for me to read C than
assembly or LLVM IR, and partly because it doesn’t lock us into the LLVM
framework. Also, I’ve found the Haskell bindings to LLVM unbearable, and I’m
not alone: when I gave an introductory workshop into LLVM while at the
[Recurse Center](http://recurse.com/), I told people that using it might not be
the best idea. One of the people there did not heed my advice and he emerged
six weeks later, [battle-hardened and
weary](https://jaseemabid.github.io/2017/07/04/compiler.html). But enough of
the future, let’s talk about now!

The emitted C code is all put into one file called `out/main.c`. This file is
split into three acts, like any good piece of dramaturgy: first come the types,
a dramatis personae of our play; then the declarations, like an overture,
serving as an exposition of what is about to happen; and then the definitions
show us what they got. All the file’s a stage, and all the definitions are
mere players. Let me talk about types and declarations first, and then we will
have a word about definitions.

### Types & declarations

For both types and declarations, the process—and indeed the function—we are
using is the same: first we sort the bindings in the environment, ranking them
based on their dependencies, then we will convert them to declarations. The
only difference is what kind of environment we are operating on. For
declarations this will be the regular environment `Env`, and for types the
type environment `TypeEnv`.

Sorting the bindings is relatively easy; all of the objects have a base score,
where this base score is a bit higher for modules than for values. Type
definitions and aliases are ranked based on their dependency depth, i.e. how
many other types they have references to internally and how those, in turn,
score. We then sort them lowest first and proceed to convert them to
declarations.

There are some nooks and crannies to the way how declarations are generated—we
don’t generate anything for generic types for instance, because we already
specialized—, but the basic idea is that for regular definitions we generate
function heads for functions and left hand sides of assignments for value
definitions.

Types are a little more complex, because we generate both the struct and the
`typedef` at the same time. There, we will go through the members one by one,
get the type we inferred for it, convert it to C, and mangle the name of the
member. Generally, whenever we emit a Lisp symbol in C, we will first mangle
the name. Mangling is in this case just a simple string replacement that will
replace certain characters that are not allowed in C names with their all-caps
name, surrounded by underscores. This means, for instance, that the omnipresent
dash `-` will become `_MINUS_`.

That is all there is to emitting declarations and types. Let’s move on to
definitions!

### Definitions

Emitting definitions is arguably the most important part of the compiler, and
it makes sense that it’s thus also the most complex<sup><a href="#6">6</a>
</sup>. Conceptually, the emitter is fairly simple: we take all the bindings
in the environment, fold over them, weed out the ones that are not important
to us, like external definitions and generic types, and emit the code for each
binding.

In the following I will walk you through a few of the most important pieces of
machinery, but by no means all of them. A complete definition can be found in
the function `toC` of the Emit module.

#### Modules

Modules are conceptually simple: they just add another level of recursion,
because they are their own environment and we thus have to do the whole process
again.

#### Literals

Literals are also straightforward; most of the primitive literals we have can
be compiled to C without any changes. The only literals that need some extra
attention are strings and arrays.

For strings, we always emit two fresh variables and bind the string literal of
type `char*` and its reference of type `char**` to those. This way we can have
statically allocated strings in our binary, in a straightforward way. It might
not be the best way going forward, but thus far has proved to be fairly
reliable.

Arrays are the most complex of the literals. This is due to two reasons: they
are not just C arrays—I lied a little above—and they can contain any kind of
statement in their definition.

Carp arrays are a two-member struct of data and length. When we emit a struct
literal, we first emit a struct literal; that’s trivial because the length is
known at compile time. We then go through the list of array contents and emit
a statement that assigns the contents of the array at the index to the
statement at that index.

```
(let [x [1 2 (+ 3 4)]]
  ;...
)
```
<div class="figure-label">Fig. 1: An ordinary Carp array.</div>

That means that the array definition depicted in Figure 1 would compile to the
C code depicted in Figure 2. In the end, it comes down to a fairly
straightforward mechanism. The temporary variables reduce the legibility a
little bit, but it’s still relatively easy to look at the compilate and see
what’s happening.

```
Array _11 = { .len = 3, .data = CARP_MALLOC(sizeof(int) * 3) };
((int*)_11.data)[0] = 1;
((int*)_11.data)[1] = 2;
int _10 = Int__PLUS_(1, 2);
((int*)_11.data)[2] = _10;
Array__int x = _11;
```
<div class="figure-label">Fig. 2: The compilate of a Carp array.</div>

#### Symbols

The next item on our list are symbols. They are simple as well, because all we
have to do is take their name and path—the path being the list of modules it is
defined in—, mangle and concatenate everything, and we have a perfectly valid
C symbol.

#### Control Flow

// TODO

#### Functions and Variables

// TODO

## Compiling C

// TODO

## Fin

We talked about a lot of stuff today. While Carp certainly is a complex piece
of machinery, it works msot of its magic in a surprisingly straightforward and
concise way. The whole compiler is just under 5000 lines of code long, and you
can definitely read through it yourself if you want or need to. Its simplicity
both comes from a relatively simple—meaner spirits than I am might even call it
limited—language design and the absence of—at this point surely
premature—optimizations.

I hope I’ve whetted your appetite to jump into Carp, or even to look around its
compiler internals. If you need advice or just want to chat, you can find all
of us on Gitter; alternatively, Erik is [on
Twitter](https://twitter.com/e_svedang) and my e-mail address should be
floating around here somewhere!

See you around next time, when we talk about how to use Carp’s C interface!

#### Footnotes

<span id="1">1.</span> Some on the internet would even go so far as to call it
worse names, like “useless”, but I’d beg to differ. You’ll get a more familiar
REPL soon enough.

<span id="2">2.</span> While trusting the user to be correct when defining
their type signatures can lead to errors at C compile time and we try to
minimize those, the alternative would be to parse the actual included C, and
that sounds like an even more horrendous task to me. YMMV.

<span id="3">3.</span> I would love to fix this when I’ve got both the time and
the understanding of the current system that is required to build on top of it.
I don’t think I’m quite there yet. Drop me a message if you want to team up!

<span id="4">4.</span> Typed holes are a concept borrowed from other
type-inferred languages. It enables the user to replace any statement with a
special symbol. The constraint solver will then fail and tell you which type
it inferred for the hole. This is useful when debugging type errors.

<span id="5">5.</span> You’d think that all of this AST traversal makes for a
slow compiler, but in my experience the Carp compiler is reasonably fast—then
again, no huge projects exist to benchmark Carp’s performance as of the time
of this blog post.

<span id="6">6.</span> Of course those properties aren’t always correlated, but
very often I experience that the most important feature seems to have the most
edge cases—or maybe that is just because I thought about it more deeply than
the rest.
