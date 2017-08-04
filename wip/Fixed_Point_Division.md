I wrote a [blog post](http://blog.veitheller.de/Fixed_Point_Arithmetic.html)
on a library—dubbed `silly`—that implements Fixed Point Arithmetic a few days
back. At that point I wasn’t sure how to properly implement division—i.e.
without losing the fractional part. Afraid I might get the details wrong, I
decided to gloss over the problem description and implementation a little.
[Harrison Clarke](http://hclarke.ca/), a fellow Recurser, pointed me to a
code snippet in reaction to the post that solves the problem with long
division. I rewrote my code based on that, and now I actually have both
solutions [in my repo](https://github.com/hellerve/silly), one named
`silly_idiv` and the other, proper solution `silly_div`. The only reason I kept
the integral version is that it’s a whole lot faster, and for some calculations
you don’t really need the fractional part.

I’m going to walk you through my initial solution and it’s limitations, and
then look at an implementation that remedies it.

This post assumes familiarity with the C programming language. It will make
heavy use of bitwise operators.

## Fixed Point numbers

There are multiple ways how we can reasonably represent fixed point numbers, of
course. In `silly`, I used a silly—sorry—but very clear format. It is a struct
of the form:

```
typedef struct {
  unsigned sign :1;
  unsigned before :31;
  unsigned after :32;
} silly;
```
<div class="figure-label">Fig. 1: A `silly` struct.</div>

It uses bitfields to tell the compiler how wide the individual parts of the
struct are. Realizing that it really is just a 64 bit bytestring, we could also
define the `silly` data type as:

```
typedef silly uint64_t;
```
<div class="figure-label">Fig. 2: A simpler representation of `silly`.</div>

The formats are essentially equivalent—assuming the compiler actually honors
our width specifiers. With a struct, however, we get the accessors for free.
Because we don’t need those accessors and work on the whole thing while
implementing division, I decide to cheat in this post and assume that the
format is the one presented in Figure 2.

For completeness’ sake, I’ll also provide a routine that converts from the
struct representation to the `uint64_t` representation:

```
uint64_t silly_to_raw(silly s) {
  return ((((uint64_t) s.before) << 32) + s.after) |
         (s.sign ? 1ULL<<63 : 0);
}
```
<div class="figure-label">Fig. 3: Converting from structs to bytes.</div>

Don’t worry, the rest of the code will be more readable.

## Implementing integer division

Implementing division is simple if you think about our format. All we have to
do is dividing the two numbers *as if they were integers* and taking care of
the sign bit. Let's do it:

```
silly silly_idiv(silly x, silly y) {
  uint64_t sign_bit = 1UL<<63;
  // unsetting the sign bit to ignore it
  silly res = ((x & ~sign_bit) / (y & sign_bit)) << 32;

  // setting the sign bit iff only one of sign bits is set
  res |= (x & sign_bit) ^ (y & sign_bit);
  return res;
}
```
<div class="figure-label">Fig. 4: Dividing, the naive way.</div>

Great. We’ve implemented division. The sign bit part is a bit clunky, but if we
strip that away, all we are left with is one division operation and a shift
left by 32 bits. The division is integral because of that shift: we fill up the
fractional part with zeroes by shifting.

This is fairly straightforward, but it’s also not necessarily great. If we used
a smaller number type, we could promote both numbers to a higher precision and
handle the precision loss that way. But with 64-bit numbers, there is no easy
way out.


## Real division: the long form

I was stumped by this problem and couldn’t really figure out a neat way to fix
my code. This is where I left y’all in my last blog post. But Harrison sent me
[a link to a C# library](https://github.com/asik/FixedMath.Net) that
implements long form division. The main problem with this is, of course, that
it’s much slower. Sometimes you just need to bite the bullet, however, so I
reimplemented their algorithm in C. Here is a stripped down version of that
algorithm, without a whole bunch of optimizations that you can find [in the
actual implementation](https://github.com/hellerve/silly/blob/master/silly.c#L99):

```
silly silly_div(silly x, silly y) {
  // disregard the sign
  uint64_t sign_bit = 1UL<<63;
  uint64_t rem = x & ~sign_bit;
  uint64_t div = y & ~sign_bit;

  uint64_t quo = 0UL;
  int b = 33; // 64 / 2 + 1

  while (rem && b >= 0) {
      uint64_t d = rem / div;
      rem %= div;
      quo += d << b;

      rem <<= 1;
      --b;
  }

  uint64_t res = quo >> 1;
  // add the sign
  res |= (x & sign_bit) ^ (y & sign_bit);

  return res;
}
```
<div class="figure-label">Fig. 5: Dividing, the slow way.</div>

This is a lot of code. Let’s try to walk through it block by block and figure
out what it does together.
