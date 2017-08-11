A few days back, I wrote a [blog post](http://blog.veitheller.de/Fixed_Point_Arithmetic.html)
on a library—dubbed `silly`—that implements Fixed Point Arithmetic. 
At that point I wasn’t sure how to properly implement division—i.e.
without losing the fractional part. Afraid I might get the details wrong, I
decided to gloss over the problem description and implementation a little.
[Harrison Clarke](http://hclarke.ca/), a fellow Recurser, pointed me to a
code snippet in reaction to the post that solves the problem with long
division. I rewrote my code based on that, and now I actually have both
solutions [in my repo](https://github.com/hellerve/silly), one named
`silly_idiv` and the other, proper solution, `silly_div`. The only reason I kept
the integral version is that it’s a whole lot faster, and the fractional part 
isn't really necessary for some calculations.

I’m going to walk you through my initial solution and its limitations, and
then look at an implementation that remedies those.

This post assumes familiarity with the C programming language. It will make
heavy use of bitwise operators.

## Fixed Point numbers

There are multiple ways to reasonably represent fixed point numbers, of
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

The formats are essentially equivalent—assuming the compiler honors
our width specifiers. With a struct, however, we get the accessors for free.
Because we don’t need the accessors and work on the whole thing while
implementing division, I decide to cheat in this post and assume that the
format is the one presented in Figure 2.

For the sake of completion, I’ll also provide a routine that converts from the
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
do is divide the two numbers *as if they were integers* and take care of
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
strip that away, all we have left is one division operation and a shift
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
implements long form division. The main problem with this is that
it’s much slower. But sometimes you just need to bite the bullet, so I
reimplemented their algorithm in C.

Before we dive into the implementation, let me talk about the underlying
algorithm for a moment: what we’re trying to implement is a binary form of long
division. We will go through the bits as we go through the digits in decimal
long division, the form you probably learned in school.

Let’s go through the division of 2456 by 12, in decimal, just to recapitulate.
This example mirrors Figure 3-1 from “Write Great Code”:

```
      2    -> 12 goes into 34 twice
   _______
12 | 3456
     24

     2    -> subtract and pull down
   _______
12 | 3456
     24
   ------
     105

// rinse and repeat

     28
   _______
12 | 3456
     24
   ------
     105
      96


     28
   _______
12 | 3456
     24
   ------
     105
      96
   ------
       96


     288
   _______
12 | 3456
     24
   ------
     105
      96
   ------
       96
       96

// done: result is 288
```
<div class="figure-label">Fig. 5: Long division.</div>

Turns out you can do the same thing in binary. There is another example in
“Write Great Code”, Figure 3-2, that you can work through. The mechanics are
exactly the same. So, without further ado, here is a stripped down version of
that algorithm, without a whole bunch of optimizations that you can find [in the
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
<div class="figure-label">Fig. 6: Dividing, the slow way.</div>

This is a lot of code. Let’s try to walk through it block by block and figure
out what it does together.

First we define two variables `rem` and `div` that are our input values without
the sign bit set. Then we define a variable `quo`, which stands for quotient.
It will hold the result of our computation. We also define the number of bits
we have to go through—named `b` in this context. We’ll have to go through at
most `<width> / 2 + 1` bits, where `width` is the total of bits in the number.
Because we know that in our use case the width is always `64`, I unrolled that
equation; the result is `33`.

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
rem %= div; // 4.0<<32
quo += d << b; // 2.0<<33 == 4.0<<32

rem <<= 1; // 4.0<<33 == 8.0<<32
--b; // 32

//second iteration
// rem = 8.0<<32
d = rem/div; // 1.0
rem %= div; // 0.0
quo += d << b; // 4.0<<32 + 1.0<<32 == 5.0<<32

rem <<= 1; // 0.0
--b; // 31

[rest of the loop has no effect, we’re done]
```
<div class="figure-label">Fig. 7: Moving bits by hand.</div>

This exercise might seem a bit silly—those of you who studied Computer Science
might be reminded of exercises from first semester courses that then seemed 
pointless—, but it visualizes the sliding window of
our division pretty well. Through all of the steps we successively build our
`quo` value. As promised, it’s basically just binary long division, though the
format is a little contrived.

We are not done yet, though. Particularly perceptive observers will have
noted that what we stored in `quo` isn’t actually `2.5`. But it almost is.
To be precise, it’s `5.0`, the double of our expected result. That is why, at
the end of the function shown in Figure 6, we have to shift the value in `quo`
by one bit, effectively dividing by 2. Then we add the sign bit back—the
process for that is the same as in the naive solution—and we’re good to go!

## Possible optimizations

I talked about possible optimizations earlier, and I don’t want to leave you
without touching on them again briefly. Both the code I borrowed from and my
own code implement two simple optimizations:

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
library in which I implemented this algorithm is just a toy, the underlying
algorithm seems pretty solid.

This whole journey, like my [foray into Binary Coded Decimals](http://blog.veitheller.de/Binary_Coded_Decimal.html),
has been inspired by [Write Great Code](https://www.nostarch.com/greatcode.htm),
and I still wholeheartedly recommend it. It’s well written, ridiculously
informative, and highly inspirational. With the author's help, I might
implement floating point numbers next. And if I do, I’m sure you’ll hear about it
here.
