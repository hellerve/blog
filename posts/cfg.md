---
title: "cfg: a simple configuration language"
date: 2026-03-09
---

About six years ago, during the early days of lockdown, I wrote a configuration
language in one evening. It was March 2020, I had time on my hands, and the
question seemed fun: what if a configuration format were so stripped-down that a
complete parser and pretty printer fit in a single C header file, with no
dependencies, and clear opinions about whitespace and formatting? What would we
have to give up, and would the result still be useful? Would it spark joy?

The project has been sitting on [my self-hosted git
instance](https://blog.veitheller.de/Git,_self-hosted.html) since then,
untouched since mid-2021, and I never wrote about it. Until now: meet
[`cfg`](https://git.veitheller.de/degrowth/cfg), a spartan configuration language
that answers the question above with “quite a lot”, “maybe”, and “definitely”.

I mostly forgot about this project, but when I found it again I liked it enough
to put it on my backlog to write about.

## The language

`cfg` has four types: numbers, strings, lists, and sections. That’s it. There are
no comments, no booleans, no null. Keys cannot contain spaces. Indentation is
always two spaces. Here is an example that exercises everything the language has
to offer:

```
my_string "hello, world"

my_num 42.0

my_list
  - "first"
  - 2.0
  - "third"

my_section
  inner_key "a value"
  inner_num 23.0
```
<div class="figure-label">Fig. 1: A `cfg` file showing all four types.</div>

Strings are quoted. All numbers are doubles, because I didn’t want to deal with
the headache of distinguishing integer and floating point types in a format this
small. Lists start with a name on its own line, followed by indented entries
prefixed with `- `, a hyphen and a space. Sections are just named, indented
groups of key-value pairs—they nest as you’d expect.

There is one space between the hyphen and the value in a list element, unless
the value is itself a nested structure, in which case there’s a linebreak after
the hyphen. This is all there is to know about the syntax.

You might think this is too simple, and you might be right. But it is simple
enough to implement in a few hundred lines of C, and that might be worth the
reduction.

## The implementation

The whole thing lives in a single header file, `cfg.h`. Around 720 lines of C,
no dependencies beyond libc. You drop it into your project and `#include` it.
No build system to configure, no library to link against.

The tradeoff is that we have to roll our own string, list, and map types. This
sounds worse than it is—the implementations are simple and small, even if they
aren’t battle-tested. Take it more as a case study than as an implementation.

Let me walk you through the interesting bits.

### Generics, via macro

Since we need lists of different types and a hashmap, and C doesn’t have
generics, we do the time-honored thing: preprocessor macros that stamp out
type-specific container code. A `LIST(type, cleanup)` macro generates a
growable array with push, pop, and indexed access for any type. A `MAP` macro
does the same for a hashmap using chained buckets and FNV-1a
hashing<sup><a href="#1">1</a></sup>.

The token pasting gets gnarly (`list_entry_##key_type##_##val_type` is a real
identifier in this code) but it works, and it means we get type-safe containers
without pulling in a library or giving up on C89-adjacent style.

I won’t dwell on this too long. If you’ve seen this pattern before, you know
the drill. If you haven’t, reading through the `LIST` macro in the source is a
decent introduction to the technique. I didn’t look at any other implementations
before I did this as a little challenge to myself, and if you program in C and
have never done it before, I highly recommend it.

### A tagged union

The core data type is `config_value`, a tagged union:

```
typedef struct config_value {
  char tag;
  union {
    config* section;
    string s;
    double d;
    struct list_config_value* l;
  };
} config_value;
```
<div class="figure-label">Fig. 2: The `config_value` type.</div>

The tag distinguishes sections, strings, numbers, and lists. Each variant has
a constructor (`config_section`, `config_string`, `config_number`,
`config_list`) and the whole thing is stored in a hashmap keyed by strings.

The pretty printer recurses over this structure, indenting as it goes. It’s
straightforward: strings get quoted, numbers get `snprintf`’d, lists emit their
elements with `- ` prefixes, and sections recurse with increased indentation.
Oh, the joys of one canonical representation!

### Parsing

The parser is a recursive descent parser that tracks line and column numbers for
error messages. Its entry point is `config_parse`, which hands off to
`config_parse_internal` with an initial indent level of zero:

```
parsed_config config_parse(string* s) {
  size_t line = 1;
  size_t col = 1;
  return config_parse_internal(s, &line, &col, 0);
}
```
<div class="figure-label">Fig. 3: The entry point.</div>

From there, the parser reads key-value pairs in a loop. For each pair, it
reads a key (everything up to the first whitespace), then dispatches on the
value: if the next character is `"`, it’s a string; if it’s a newline, it’s
either a list or a section (distinguished by whether the next non-whitespace
character is a `-`); otherwise, it’s a number.

```
parsed_value config_parse_value(string* s, size_t* line,
                                size_t* col, size_t indent) {
  if (s->str[0] == '"') return config_parse_string(s, line, col);
  if (s->str[0] == '\n') return config_parse_list_or_section(
                            s, line, col, indent+2);
  return config_parse_number(s, line, col);
}
```
<div class="figure-label">Fig. 4: Dispatching on value type.</div>

The most interesting bit is how lists and sections share an entry point. Both
are indented blocks following a bare key. The parser reads the indent, and if
the first character is a hyphen, it commits to parsing a list; otherwise it
recurses into `config_parse_internal` for a nested section. Indentation drives
the whole thing: when the indent level drops, the current block is done.

String parsing handles escaped characters and multiline strings. Number parsing
walks the characters, allows one decimal point and a leading minus, and hands
off to `strtod`. Error messages include line and column numbers, which is more
than some production parsers manage, some shade intended.

Again: because we only have one representation and there are no style
ambiguities, this becomes trivial. It was so nice to pare this down.

## Fin

`cfg` is not a serious contender for your next project’s configuration needs.
It has no spec, no ecosystem, and probably a whole bunch of bugs that will eat
your SLAs for breakfast. What it does have is a complete implementation
including a parser, pretty printer, and programmatic API in a single file short
enough to read (or write) in one sitting.

Not every project needs to be robust, some just need to be compact and
explorable. I wanted to know how small a useful configuration language could
get, and now I have a number: about 720 lines of C, most of it for data types.
I’ve done this with a few other things in the [degrowth](https://git.veitheller.de/degrowth/)
org of my Git instance, and they’re all more or less fun. Do check them out if
you feel like it.

See you around!

#### Footnotes

<span id="1">1.</span> [FNV-1a](http://www.isthe.com/chongo/tech/comp/fnv/)
is a simple, fast hash function that works well enough for small hash tables.
It's not cryptographic, but it doesn't need to be.
