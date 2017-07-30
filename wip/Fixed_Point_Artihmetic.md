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
I suck at writing tests.

Division is a whole other beast. I actually wasn’t able to write a correct
version of division on the bus, I simply lacked context. This is where I
gave up and looked around for rules how to implement these, and was comforted
by the fact that I’m not the only person who isn’t able to solve this problem
by themself.
