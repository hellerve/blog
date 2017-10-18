This will be a fairly short blog post about a neat trick that I found today
that makes correctly sign extending numbers in C trivial. It is based on the
version found in this [great list of bit hacks](http://graphics.stanford.edu/~seander/bithacks.html),
but generalized.

## What is sign extending, anyway?

Sign extension means growing a number—say, for instance, from 32 to 64 bits—
while preserving the sign bit. The sign bit is always the highest bit in the
numerical formats used today—this is purely by convention, but to my knowledge
always true. This means that we have to shift the most significant bit up to
the new highest position, while keeping the rest of the number intact.

If you’re coming from C, you’re probably saying “But C does that automatically!”
right around now. And that’s true for builtin types. But say you’re building
your own numerical format, like I’ve done in [the](http://blog.veitheller.de/Binary_Coded_Decimal.html)
[past](http://blog.veitheller.de/Fixed_Point_Arithmetic.html); then you’ll either need
to do that manually or rely on this beautifully elegant trick I’m about
to show you.

## Remember bitfields?

Bitfields are a great way of controlling just how many bits a particular value
should take up. Of course they’re useless if you want to save space
that’s less than the size of an `int`—or are they? Enter sign extension

```
#define extend(O,I,N) {\
  struct {signed int x:N;} tmp_;\
  O = tmp_.x = I;\
}

/* Later... */
int x; // A 9 bit number
int y; // A regular int I want to assign to
extend(y, x, 9);
```
<div class="figure-label">Fig. 1: Sign extension, macro style.</div>

Before I get into what I just did there, let me address some of the concerns of
the people who already know and are about to ragequit. I do introduce a new
binding `tmp_` here and hope for the best. Admittedly, that’s not great. A
somewhat less crude, but still not entirely secure way would be [emulating
`gensym`](http://zwizwa.be/-/c/20100825-142132), although in my opinion that,
too, is a duct tape solution at best. I also assign to a thing that was given
to me as input, which is a little shady, I’ll admit. It will, however, make the
API more pleasant to work with, and so I’ll gladly trade one vice for another.

Now, let’s get back to business: What are we doing here? We are basically
exploiting the fact that through bitfields we can defer the problem of sign
extending to a higher authority—the compiler, namely, which in turn will
probably defer the problem to the processor by using a dedicated instruction.
So, by having an immediate assignment only—and the preceding definition of a
bitfield—we can stop worrying about sign extension. Pretty cool, huh?

C++ folks are of course free to use templates and make this a little less
fragile.
