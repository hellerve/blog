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
typedef uint64_t silly;
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

First we define two variables `rem` and `div` that are our input values without
the sign bit set. Then we define a variable `quo`, which stands for quotient.
It will hold the result of our computation. We also define the number of bits
we have to go through—named `b` in this context. We’ll have to go through at
most `<width> / 2 + 1` bits. Because we know that in our use case the width is
always `64`, I unrolled that equation; the result is `33`.

Most of the work here happens in the `while` loop. It will be executed `b`
times, but we also have a shortcut condition: if the remainder is zero, we can
stop because no more work will happen in the loop anyway.

So what is the loop doing? It’s basically performing long division in binary.
Maybe a visualizing example will help, so let’s walk through a division of
`20.0` and `8.0`. It should yield `2.5`. All of the values look like 64-bit
integers, shifted by 32 bits, because of the fraction.

```
// first iteration:
//   rem = 20.0<<32
d = rem/div; // 2.0
rem %= div; // 2.0<<32
quo += d << b; // 2.0<<33 == 4.0<<32

rem <<= 1; // 2.0<<33 == 4.0<<32
--b; // 32

//second iteration
// rem = 2.0<<32
d = rem/div; // 0
rem %= div; // 2.0<<32
quo += d << b; // d == 0, so same a before

rem <<= 1; // 2.0<<34 == 8.0<<32
--b; // 31

//third iteration
// rem = 8.0<<32
d = rem/div; // 1.0
rem %= div; // 0.0<<32
quo += d << b; // 1.0<<31 == 0.5<<32 (kind of)

[rest of the loop has no effect, we’re done]
```
<div class="figure-label">Fig. 6: Moving bits by hand.</div>

This exercise might seem a bit silly, but it visualizes the sliding window of
our division pretty well. Through all of the steps we successively build our
`quo` value. It’s basically just binary long division, though the format is
a little contrived.

We are not done yet, however. Particularly perceptive observers will have
noted that what we stored in `quo` isn’t actually `2.5`. It almost is, however.
In particular, it’s `0.5` shifted to the left by one bit. That is why, at the
end of the function shown in Figure 5, we have to shift the value in `quo`
by one bit. Then we add the sign bit back—the process for that is the same as
in the naive solution—and we’re good to go!

## Possible optimizations

I talked about possible optimizations earlier, and I don’t want to leave you
without talking about them at least for a bit. The code I borrowed from and my
own both implement two simple optimizations:

1. If the divider is divisible by 2^n, we can reduce the number of loop
iterations by four for each byte if we just shift the divider. Look at the
implementation [here](https://github.com/hellerve/silly/blob/master/silly.c#L108).
2. If the remainder has leading zeroes we can skip this number of iterations.
Think of long division: I can skip a bunch of steps if my number is padded
with `0`. This can be checked in each iteration and is implemented
[here](https://github.com/hellerve/silly/blob/master/silly.c#L114).

Neither of these optimizations interfere with the actual algorithm, meaning we
could just turn them off if we wanted to. This makes the optimizations very
low-hanging fruit: they are local, small, and have a sufficient performance
impact, such that Knuth’s famous quip [about “the root of all evil”](http://wiki.c2.com/?PrematureOptimization)
is not applicable to us.

## Fin

Implementing this was another fun exercise! I hope you enjoyed my little
walkthrough into long form division to solve a real world problem. Though the
library in which I implemented this algorithm is merely a toy and I urge you
not to use it, the underlying algorithm seems pretty solid to me.

This whole journey, like my [foray into Binary Coded Decimals](http://blog.veitheller.de/Binary_Coded_Decimal.html),
has been inspired by [Write Great Code](https://www.nostarch.com/greatcode.htm),
and I still wholeheartedly recommend it. It’s well written, ridiculously
informative, and highly inspirational. With the authors help, I might
implement floating point numbers next—if I do, I’m sure you’ll hear about it
here.
