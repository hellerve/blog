I’ve always been fascinated by fixed point numbers. For the uninitiated, fixed
point numbers are, as the name suggests, related to floating point numbers.
Like their more common counterpart, they are numbers with a decimal point.
Unlike them, the decimal point always stays at the same point within the
number, which means that we potentially waste space if either of the components
could be described in less bits.

Fixed point numbers are less scary than floating point numbers insofar as I can
grok how to do simple arithmetic on them, or so I thought. To prove that to
myself, I wrote another [toy library in C](https://github.com/hellerve/silly),
akin to the one I wrote [for binary-coded decimals](/Binary_Coded_Decimals.html),
on a quiet saturday on the Greyhound bus from New York City to Washington.

Of course I ran into way more problems than I anticipated, because I’m really
not all that great with numbers, apparently. Addition and subtraction are
reasonably straightforward, especially if one implements a few simplification
rules, e.g. `if we try to subtract a negative number from a number, call the
addition procedure with the sign removed`. Dragons start popping up with
multiplication already, because I wanted to implement it in terms of integers,
not floating point numbers, and I had never thought about how that works prior
to that point. Once I realized what I had to do with the multiplication bits
and where to watch out for overflows, it was actually doable. I’m not
completely convinced I’ve got all of the corner cases covered, though, because
I suck at writing tests. Oh well.

Division is a whole other beast. I actually wasn’t able to write a correct
version of division on the bus, I simply lacked context. This is where I
gave up and looked around for rules how to implement these, and was comforted
by the fact that I’m not the only person who isn’t able to solve this problem
by themself. I also realized that the scholars would disapprove of my format,
but I couldn’t be bothered to update it into a more appropriate format. My
datastructure looks like this:

```
typedef struct {
  unsigned sign :1;
  unsigned before :31;
  unsigned after :32;
} silly;
```
<div class="figure-label">Fig. 1: A silly number.</div>

I find this format to cute to pass up<sup><a href="#1">1</a></sup>. The part
after the decimal point, however, is where I was a bit more naive than is
appropriate. The general consensus seems to be not to just store the fractional
part as binary that “behaves like an integer”. I did it anyway, and to my
surprise everything was fine. I’m not sure what the reasoning behind advices
like these are:

```
```
<div class="figure-label">
  [Fig. 2: The dos and don’ts of fixed points, as found on
   the internet.]()
</div>

I suppose it’s good to document that I know that I’m doing it wrong and should
really care more.

This was a fun little exercise in computational mathematics, and really
foundational. In hindisght I’m a bit bummed I didn’t get to do things like
these in university, because I think those exercises are both simple enough and
have a big return on investment. If I go back to university and end up being a
TA, my students might have to work through horrible exercises like these. Read
this, depending on your opinions on these kinds of things, as an invitation to
pity or envy my future students.
