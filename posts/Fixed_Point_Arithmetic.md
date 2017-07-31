*Update:* I realized that my format actually makes sense and I intuitively
implemented it correctly. This means the lower half is a bit wonky. Sorry about
that.

I’ve always been fascinated by fixed point numbers. For the uninitiated, fixed
point numbers are related to floating point numbers.
Like their more common counterpart, they are numbers with a decimal point.
Unlike floating points, the decimal point always stays at the same location within the
number; this could lead to potentially wasted storage if either of the components
could be described with fewer bits.

Fixed point numbers are less scary than floating point numbers insofar as I can
grok how to do simple arithmetic on them, or so I thought. To prove that to
myself, I wrote another [toy library in C](https://github.com/hellerve/silly),
akin to the one I wrote [for binary-coded decimals](/Binary_Coded_Decimals.html),
on a quiet Saturday on the Greyhound bus from New York City to Washington.

Of course I ran into way more problems than I anticipated, because I’m apparently
not all that great with numbers. Addition and subtraction are
reasonably straightforward, especially if you implement a few simplification
rules, e.g. `when subtracting a negative number from a number, call the
addition procedure with the sign removed`. Dragons already start blowing smoke at
multiplication; I wanted to implement it using integers,
not floating point numbers, and I had never thought about how that would work prior
to writing this library. Once I realized what I had to do with the multiplication bits
and where to watch out for overflows, it was actually doable. I’m not
completely convinced I’ve got all of the corner cases covered, though, because
I suck at writing tests. Oh well.

Division is a whole other beast. I actually wasn’t able to write a correct
version of division on the bus; I simply lacked context. This is where I
gave up and looked around for rules how to implement these and was comforted
by the fact that I’m not the only person who isn’t able to solve this problem
by themselves. I also realized that scholars would disapprove of my format,
but I couldn’t be bothered to update it into something more appropriate. My
datastructure looks like this:

```
typedef struct {
  unsigned sign :1;
  unsigned before :31;
  unsigned after :32;
} silly;
```
<div class="figure-label">Fig. 1: A silly number.</div>

I find this format too cute to pass up<sup><a href="#1">1</a></sup>. The part
after the decimal point, however, is where I was a bit more naive than is
appropriate. The general consensus seems to be not just to store the fractional
part as binary that “behaves like an integer”. I did it anyway, and to my
surprise got through multiplication, although it is a bit more complex than if
I had used a well-studied format.

```
2.5
// means "two and half", and is encoded as
// '2'     'half'
00000010.10000000

// So don't be tempted to write
00000010.00000101 // <- INCORRECT!
```
<div class="figure-label">
  [Fig. 2: The dos and don’ts of fixed points, as found on
   the internet.](http://x86asm.net/articles/fixed-point-arithmetic-and-tricks/)
</div>

I suppose it’s good to document that I know I’m doing it wrong and should
really care more.

Back to division: the problem that I encountered—and haven’t solved yet,
because I couldn’t find a fix on the internet—is that when dividing two
numbers I suffer from a loss of precision. In multiplication I solved that by
multiplying the components individually and then shifting or masking the
result, which degrades in the same way as an overflow would. With division,
however, I need a double precision container type in which I could perform
the calculation—a `uint128_t` would be great here. Alas, this type doesn’t
exist, and it wasn’t immediately obvious how to emulate that behaviour; for
now the “library” only supports integer precision division.<sup><a href="#2">2</a></sup>

This was a fun little exercise in computational mathematics and really
foundational. In hindisght I’m a bit bummed I didn’t get to do things like
these in university, because I think those exercises are simple and
have a big return on investment. If I go back to university and work as a
TA, my students might have to trudge through horrible exercises like these.
Read this, according to personal taste, as an invitation either to pity or envy
them.

##### Footnotes

<span id="1">1.</span> If you don’t know what the trailing numbers in the
struct definitions are: these are bitfields, and they are incredibly handy.

<span id="2">2.</span> If anyone knows how to remedy this, please explain it to
me! It sounds like this is a well-known problem, but I can’t figure it out for
division.
