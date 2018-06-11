In an effort to check how painful it would be to do low-level bit-fiddling in
Carp I recently implemented an implementation of [Base64
encoding](https://github.com/hellerve/base64.carp) in Carp. My goals were to
write something that’s useful while still not exceeding a few hours worth of
programming and debugging, and I think it worked!

In this blog post I want to walk you through how you Base64 works, and how you
could go about implementing it in Carp. An ability to read Lisp is required,
and if you want to truly understand what’s going on, reading [my introductory
blog post](/Carp.html) on Carp probably wouldn’t hurt either. If you have some
idea how to read Carp’s type signatures, that might help as well, but is
definitely not required as I aim to make the post understandable without their
help!

As always, we’ll start by defining an API, then go through an implementation of
the library, and then finish things up by looking at some of the caveats and
trade-offs that I made while writing this, and how you could go about making it
better.

## An API

An encoding API is relatively simple: You usually want to have a function that
encodes something, and a function that decodes it. In Base64, there is not a lot
of room for customization if you stick to the standard character set, so our
functions will be almost trivial to look at:

```
; Base64.encode : (Fn [&String] String)
(Base64.encode "hello world!") ; => "aGVsbG8gd29ybGQh"

; Base64.decode : (Fn [&String] String)
(Base64.decode "aGVsbG8gd29ybGQh") ; => "hello world!"
```
<div class="figure-label">Fig. 1: A Base64 API.</div>

The composition of the two functions is isomorphic, meaning that if we encode
and then decode—or the other way around—we should get the original string
back<sup><a href="#1">1</a></sup>.

We can construct it so that supplying different character sets is almost free,
though, so let’s do that. A character set will be represented as an array of
characters that we will supply as the first argument, so the functions we define
will end up looking like this:

```
; Base64.encode-using : (Fn [&(Array Char) &String] String)
(Base64.encode Base64.mime-charset
               "hello world!") ; => "aGVsbG8gd29ybGQh"

; Base64.decode-using : (Fn [&(Array Char) &String] String)
(Base64.decode Base64.mime-charset
               "aGVsbG8gd29ybGQh") ; => "hello world!"
```
<div class="figure-label">Fig. 2: A Base64 API, extended.</div>

The MIME character set that we expose in Figure 2 is the standard charset most
of us think about when we think about Base64-encoded text. This has the nice
property that both `encode` and `decode` are just specializations of
`encode-using` and `decode-using` with the MIME character set.

And that’s about all we need to define the interface of a useful Base64 library!
Let’s try and implement it then, shall we?

## An implementation

Before we implement this encoding, it might be interesting to learn what it
actually does and what purpose it serves. I will try and explain Base64
concisely; if you already know all about it, feel free to skip to the next
section!

### What is Base64 and how does it work?

There is something magic about the number 64. It is a power of two, namely
`2^6`. This is interesting for us, because it means that we can represent any
character of the character set using only 6 bits. This is 2 bits per character
less than ASCII, meaning that we use three fourths of the space we would be
using if we represented our text in ASCII. Notably, this doesn’t necessarily
mean that Base64-encoded text is only three fourths as long as ASCII, in fact
it is approximately 1.37 times the original size.

Why is that? The encoding is simple: We take three bytes of the source string
(that’s 24 bits), group it into four six bit groups, and look up the
corresponding characters. This means that we end up with four ASCII characters
per three that we put in, plus any padding.

Padding is necessary because we always take groups of three bytes. Thus, if the
length of the input text is not divisible by three, we append `=` to signify
that there are missing characters. We will always end up with between zero and
two `=` characters as padding. Why that is is left as an exercise to the reader.

Now that we have a basic intuition of how Base64 works internally, let’s wrap
our heads around how to actually implement it.

### The code

As I wrote above when we talked about the API of the library, `encode` and
`decode` are specializations of the more general functions `encode-using` and
`decode-using`. Let’s look at what that looks like in practice.

```
(defmodule Base64
  (doc mime-charset "The Base64 character set as specified by MIME.")
  (def mime-charset [
    \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q
    \R \S \T \U \V \W \X \Y \Z \a \b \c \d \e \f \g \h
    \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y
    \z \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \+ \/ \=
  ])

  ; encode-using and decode-using will go here

  (doc decode "Decodes a base64-encoded string using mime-charset.")
  (defn decode [str]
    (decode-using &mime-charset str))

  (doc encode "Encodes a string into base64 using mime-charset.")
  (defn encode [str]
    (encode-using &mime-charset str))
)
```
<div class="figure-label">Fig. 3: Defining encoding and decoding.</div>

The functions and definitions for the character set are almost trivial. We just
call our omniscient parent functions. What is `doc`, though? Well, you can
document functions in Carp from your source code and autogenerate documentation
from it, similar to “docstrings” in languages like Ruby and Python. The main
difference to these languages is that the documentation generator is part of
the compiler, so you only have one tool for everything. Kind of neat, huh?

Moving on, we have to implement `decode-using` and `encode-using`. The former
is a little simpler, so we should probably start with that one. While we’re at
it, I’ll also introduce you to another new companion called `sig`, which lets
you optionally specify the types of functions should you want to.

```
(defmodule Base64
  (doc decode-using "Decodes a base64-encoded string using a user-supplied character set.")
  (sig decode-using (Fn [&(Array Char) &String] String))
  (defn decode-using [charset str]
    (let-do [bytes (chars str)
             decoded []]
      (while (/= (length &bytes) 0)
        ; what to do here?
      )
      ; what to return?
    )
  )
)
```
<div class="figure-label">Fig. 4: A skeleton for `decode-using`.</div>

As we can glean from Figure 4, `sig` takes the name and signature of a function.
Usually you don’t need to do that and the compiler will infer a type for you,
but sometimes those are too general or you want to be extra explicit. In this
case, as we’ll see later, Carp will assume that the type of `charset` can be
anything, because we only use interface functions on it that could be defined
by a variety of types. It is prudent, however, that we restrict the inputs to
arrays of characters, to make the type signature explicit and informative.

What do we do in the function, though? First, we define `bytes` to be an array
of characters, created from the input string—please note that we assume the text
to be ASCII in there. We also define an empty array called `decoded`, which will
be the result of our fiddling. Then we go through the bytes until they’re empty.
All of this suggests a mutation-based approach that is not very functional, but
presumably fairly fast.

This already was a lot to take in, but buckle up! We have a lot of work to do
per iteration!

```
(defmodule Base64
  (defn decode-using [charset str]
    (let-do [bytes (chars str)
             decoded []]
      (while (/= (length &bytes) 0)
        (let-do [b0 (index-of charset @(nth &bytes 0))
                 b1 (index-of charset @(nth &bytes 1))
                 b2 (index-of charset @(nth &bytes 2))
                 b3 (index-of charset @(nth &bytes 3))]
          (set! bytes (suffix-array &bytes 4))
          ; now what?
        )
      )
      ; what to return?
    )
  )
)
```
<div class="figure-label">Fig. 5: Not so much a skeleton anymore.</div>

Okay, so now we get the index of our individual characters in the character set,
and then we remove the four characters from the byte array using a handy array
function called `suffix-array`. This is half of what we need. Now we actually
need to figure out how to stitch the original message back together.
Bit-fiddling time!

#### Footnotes

<span id="1">1. </span> This is not strictly true if we assert that ownership is
                        a piece of that puzzle. Both of these functions take
                        references and return owned strings, which means that
                        we cannot just stick them together and we will end up
                        with an owned string instead of a reference. If we do
                        not care about these semantics, however, the two
                        functions are indeed isomorphic.
