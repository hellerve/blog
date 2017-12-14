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

Dynamic functions are similar, but they actually evaluate their arguments before
being applied. That simple idea makes them similar to dynamic functions and
useful when applied recursively, and for proper metaprogramming.

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

#### Memory management

## Emitting C

## Compiling C

## Fin

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
