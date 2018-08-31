In an effort to check how painful it would be to do low-level bit-fiddling in
Carp I recently implemented an implementation of [Base64
encoding](https://github.com/hellerve/base64.carp) in Carp. My goals were to
write something that’s useful while still not exceeding a few hours worth of
programming and debugging, and I think it worked!

In this blog post I want to walk you through how you Base64 works, and how you
could go about implementing it in Carp. The ability to read Lisp is required,
and if you want to truly understand what’s going on, reading [my introductory
blog post](/Carp.html) on Carp probably wouldn’t hurt either. If you have some
idea how to read Carp’s type signatures—they’re very similar to Haskell’s—,
that might help as well, but is definitely not required as I aim to make the
post understandable without their help!

We’re going to learn how to read Carp function signatures, work with strings and
arrays, and how to program imperatively in a mostly functional language when in
a pinch.

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
; Base64.encode : (Fn [&(Array Int)] String)
(Base64.encode [2 3 4]) ; => "AgME"

; Base64.encode-str : (Fn [&String] String)
(Base64.encode-str "hello world!") ; => "aGVsbG8gd29ybGQh"

; Base64.decode : (Fn [&(Array Int)] String)
(Base64.decode "AgME") ; => [2 3 4]

; Base64.decode-str : (Fn [&String] String)
(Base64.decode-str "aGVsbG8gd29ybGQh") ; => "hello world!"
```
<div class="figure-label">Fig. 1: A Base64 API.</div>

The composition of the functions is isomorphic, meaning that if we encode
and then decode—or the other way around—we should get the original datum
back<sup><a href="#1">1</a></sup>.

We can construct it so that supplying different character sets is almost free,
though, so let’s do that. A character set will be represented as an array of
characters that we will supply as the first argument, so the functions we define
will end up looking like this:

```
; Base64.encode-using : (Fn [&(Array Char) Char &(Array Int)] String)
(Base64.encode-using Base64.mime-charset
                     Base64.mime-padding
                     [2 3 4]) ; => "AgME"

; Base64.decode-using : (Fn [&(Array Char) Char &String] (Array Int))
(Base64.decode Base64.mime-charset
               Base64.mime-padding
               "AgME") ; => [2 3 4]
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

Why is a format like that useful? Firstly, it’s important to realize that this
is an encoding, not a compression. This means that we do not really care about
the size, but about the character set. Typically, Base64 will be applied to
binary data, although you often see it used to encode texts on the web. What’s
important is that we want to limit the data representation to “safe” characters,
i.e. characters that cannot corrupt anything or will not be corrupted by a
system. Think of `NUL` bytes or, if that doesn’t tell you anything, escaping
strings in URLs.

Padding is necessary because we always take groups of three bytes. Thus, if the
length of the input text is not divisible by three, we append `=` to signal
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
    \z \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \+ \/
  ])

  (doc mime-padding "The Base64 padding as specified by MIME.")
  (def mime-padding \=)

  ; encode-using and decode-using will go here

  (doc decode "Decodes a base64-encoded string using mime-charset.")
  (defn decode [data]
    (decode-using &mime-charset mime-padding data))

  (doc encode "Encodes a string into base64 using mime-charset.")
  (defn encode [str]
    (encode-using &mime-charset mime-padding data))

  ; decode-str and encode-str are left as an exercise to
  ; the reader
)
```
<div class="figure-label">Fig. 3: Defining encoding and decoding.</div>

The functions and definitions for the character set are almost trivial. We just
call our omniscient `*-using` functions. What is `doc`, though? You can
document functions in Carp from your source code and autogenerate documentation
from it, similar to “docstrings” in languages like Ruby and Python. The main
difference to these languages is that the documentation generator is part of
the compiler, so you only have one tool for everything. Kind of neat, huh?

Moving on, we have to implement `decode-using` and `encode-using`. The former
is a little simpler, so we should probably start with that one. While we’re at
it, I’ll also introduce you to another new companion called `sig`, which lets
you optionally specify the types of functions should you want to.

#### `decode-using`

```
(defmodule Base64
    (doc decode-using "Decodes a base64-encoded datum using a user-supplied character set.")
  (sig decode-using (λ [&(Array Char) Char &String] (Array Int)))
  (defn decode-using [charset padding str]
      (let-do [bytes (chars str)
            decoded []]
        (while (/= (length &bytes) 0)
          ; do something
        )
        decoded))
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
to be ASCII in there (remember that it’s a Base64-encoded string, so ASCII is
hopefully fine). We also define an empty array called `decoded`, which will
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
                 third @(nth &bytes 2)
                 b2 (index-of charset third)
                 fourth @(nth &bytes 3)
                 b3 (index-of charset fourth)]
          (set! bytes (suffix-array &bytes 4))
          ; now what?
        )
      )
      decoded
    )
  )
)
```
<div class="figure-label">Fig. 5: Not so much a skeleton anymore.</div>

Okay, so now we get the index of our individual characters in the character set,
and then we remove the four characters from the byte array using a handy array
function called `suffix-array`. For some reason—this will be clearer in a
minute—we also bind the third and fourth byte in the array to a variable. Thi
is half of what we need. Now we actually need to figure out how to stitch the
original message back together.
Bit-fiddling time!

We will have to slice the bytes since each of them only represents six bits of
information. Thus we end up with three characters rebuilt like this:

- The first character is the first six bits together with the first two bits of
  the next byte.
- The second character is the last four bits of the second byte and the first
  four bits of the third byte.
- The third character is the last two bits of the third byte and the full fourth
  byte.

If that didn’t make any sense whatsoever, don’t fret! There is a nice little
abstraction that helps you think about it: think of our input bytes as a stream,
each of those bytes having only six bits of information (forget about the other
two, they will be zeroed out anyway), and our encoding being a sliding window
over them. Thus, a simplified run of our decoder looks like this:

```
/* First iteration: full first byte + 2 bits */
| 110011 | 010101 | 001101 | 101001 |
  ^ Window  ^
=> 11001101

/* Second iteration: Rest of second byte + 4 bits */
| 110011 | 010101 | 001101 | 101001 |
             ^ Window  ^
=> 11001101 | 01010011

/* Third iteration: The rest */
| 110011 | 010101 | 001101 | 101001 |
                        ^ Window  ^
=> 11001101 | 01010011 | 01101001
```
<div class="figure-label">Fig. 6: A sliding window.</div>

Hopefully, this little illustration helps you understand what I’m getting at a
little better. It might also make clearer why we chose to take four bytes: it’s
the lowest common denominator in a 3->4 encoding (three bytes input, four bytes
output). Otherwise we would have to keep track of half bytes across iterations
and that gets super ugly really quickly.

```
(defmodule Base64
  (defn decode-using [charset str]
      ; setup stuff
        (while (/= (length &bytes) 0)
          ; more setup stuff...

          (set! bytes (suffix-array &bytes 4))
          ; ((b0 & 63) << 2) | (b1 >> 4)
          (set! decoded (Array.push-back decoded
                          (bit-or
                            (bit-shift-left (bit-and b0 63)
                                            2)
                            (bit-shift-right b1 4))))
          ; ((b1 & 15) << 4) | (b2 >> 2)
          (when (/= third padding)
            (set! decoded (Array.push-back decoded
                            (bit-or
                              (bit-shift-left (bit-and b1 15)
                                              4)
                              (bit-shift-right b2 2)))))
          ; ((b2 & 3) << 6) | b3
          (when (/= fourth padding)
            (set! decoded (Array.push-back decoded
                            (bit-or
                              (bit-shift-left (bit-and b2 3)
                                              6)
                              b3)))))
      ; ...
)
```
<div class="figure-label">Fig. 7: A sliding window, unrolled.</div>

Once again, that’s a lot of code, but most of it should be fairly clear. There
is one thing that we haven’t talked about however: padding. I used a little
hack to deal with it already, though, and it has to do with those `when` guards
that might have confused you: we check whether either of the last two bytes are
padding bytes and if not, we work on them. Otherwise, we ignore them.

If bitt fiddling in Lisp land confuses you—I know it confused me in the
beginning!—, refer to the comments that i added above the individual operations
that tell you what exactly we are doing to the bytes. Depending on how
experienced you are at bit-masking and such, this might come natural to you or
not. If it’s not quite clear yet, refer back to Figure 6, and try to understand
how we build the sliding window.

Anyway, we’re done with decoding! Take a deep breath, for we are about to tackle
its inverse operation: encoding. It’s a little more complex that decoding, so
you might want to take a break before attacking this beast!

#### `encode-using`

So, why is encoding more involved than decoding? First of all, encoding is
always a mess, especially if the two formats aren’t of the same length. We have
to check for padding to make sure we don’t overrun the source string at every
iteration, we have to accumulate and shift bits right and left—it’s not the most
comfortable place to be in.

To make it simple, we will ignore the [DRY
principle](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) today and put
all of our eggs in the duplication basket. Carp is a Lisp, and there are loads
of roads you could take to reduce duplication and make the code more elegant,
but it’s easier to read and reason about if we see the structures laid bare in
front of us—or at least I believe that to be the case.

Let’s build a skeleton of the encoding function and go from there, shall we?

```
(defmodule Base64
  (doc encode-using "Encodes a base64-encoded datum using a user-supplied character set.")
  (sig encode-using (λ [&(Array Char) Char &(Array Int)] String))
  (defn encode-using [charset pad s]
    (let-do [l (length s)
             idx 0
             enc @""]
      (while (> l idx)
        ; do something
      )
      enc)
  )
)
```
<div class="figure-label">Fig. 8: An encoding skeleton.</div>

The basic structure of this function looks pretty similar to what we’ve built in
`decode-using`. The signature hints at the inversion: the last argument and the
return type are swapped. In the beginning of the function, we bind the length of
the datum to a variable, get an index variable going, and an accumulator, which
is a string this time.

We will iterate as long as the index is smaller than the length of the array we
pass in and return the accumulator at the end. So far, so sensible.

But what happens at each iteration?

First, we should make sure that we aren’t at a point where we have to pad the
string. Thus, we should somehow check the length, and do some work if we still
have a ways to go.

```
(defmodule Base64
  (defn encode-using [charset pad s]
    ; setup
    (while (> l idx)
      (let-do [b0 @(nth s idx)]
        (case (- l idx)
          1
            ; we have to add two padding characters
          2
            ; we have to add one padding character
          ; else
          (let-do [b1 @(nth s (inc idx))
                   b2 @(nth s (+ idx 2))
                   c0 (nth charset (bit-shift-right b0 2))
                   c1 (nth charset
                        (bit-or
                          (bit-shift-left (bit-and b0 3) 4)
                          (bit-shift-right b1 4)))
                   c2 (nth charset
                        (bit-or
                          (bit-shift-left (bit-and b1 15) 2)
                          (bit-shift-right b2 6)))
                   c3 (nth charset (bit-and b2 63))
                   ]
            (set! enc (str* enc @c0 @c1 @c2 @c3))
            (set! idx (+ idx 3))))))
    ; return
  )
)
```
<div class="figure-label">Fig. 9: A bit more meat on the encoding bones.</div>

Once again: that’s a lot! But once you understand this, everything what’s to
come is a breeze, so bear with me.

We decided to do the switch for padding using a `case` statement, which is
similar to a `switch` in C, only a bit more capable. We ignore the padding cases
for now and focus on a “regular” iteration. During a regular iteration, we will
get three bytes (we always get at least one, `b0`, so we factor this out of the
`case`), `b0` to `b2`. We will split them into four characters, using the
inverse of the unrolled sliding window operation we used in the decoder,
resulting in the characters `c0` to `c3`. We then add that to our accumulator
`enc` and increment the index by three.

That’s one full cycle of the encoding operation that makes four characters from
three. Our cycle with padding is similar, but simpler, since we don’t need to
do the whole dance. I’m going to show you how it looks for the first
case—resulting in two padding characters being appended—, and leave the last
case to you to mull over.

```
(defmodule Base64
  (defn encode-using [charset pad s]
    ; setup
    (while (> l idx)
      (let-do [b0 @(nth s idx)]
        (case (- l idx)
          1
            (let-do [c0 (nth charset (bit-shift-right b0 2))
                     c1 (nth charset
                            (bit-shift-left (bit-and b0 3)
                                            4))]
              (set! enc (str* enc @c0 @c1 pad pad))
              (break))
          2
            ; we have to add one padding character
          ; else case
    ; return
  )
)
```
<div class="figure-label">Fig. 10: A cycle with padding.</div>

As we can see, all we are doing is splitting our single byte into two characters
and appending those—along with the padding—to our accumulator. We then break out
of the loop.

And that’s it! We can encode Base64 data now!

## Caveats

This Base64 encoder is imperative rather than functional. Carp supports both
functional and imperative programming, and I usually prefer functional code.
The route we’ve gone down today, however, is very explicit, which I prefer for
teaching you the basics.

If you have a soft spot for functional programming, you might want to revisit
this codebase and try out different approaches for how to make this a little
more elegant.

## Fin

As always, it’s been a pleasure writing for you. This article has been
challenging to write, and I had to revisit both the code and the writing a
couple of times to make it more understandable.

Bit fiddling can seem hard and daunting, and I hope this article has helped a
little bit in showing you ways of how to navigate the complexity and weirdness
of bits and bytes.

#### Footnotes

<span id="1">1.</span> This is not strictly true if we assert that ownership is
                       a piece of that puzzle. Both of these functions take
                       references and return owned strings, which means that
                       we cannot just stick them together and we will end up
                       with an owned string instead of a reference. If we do
                       not care about these semantics, however, the two
                       functions are indeed isomorphic.
