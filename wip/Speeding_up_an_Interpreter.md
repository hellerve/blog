Earlier this year, I wrote an interpreter for [a version of Brainfuck that
supports CSP-style message
passing](http://blog.veitheller.de/Brainfuck_and_Actors.html). I didn’t touch it
for a while after I built the initial bytecode VM. I considered it done.

Of course no pet project is ever done, it just sleeps until the next crazy idea
hits you. And so, seemingly out of nowhere, I started working on the interpreter
again in mid-November. At first, this was borne out of a simple desire to
explore [direct threading for VMs once more](https://en.wikipedia.org/wiki/Threaded_code#Direct_threading).
I had worked on direct threading [about a year earlier](https://github.com/hellerve/VVM/pull/1),
in the context of another silly VM that I play with when I want to try out new
optimization techniques for virtual machines, but it didn’t change much back
then. But then I saw a talk by Adrien Lamarque at [Enthusiasticon](https://blog.veitheller.de/Enthusiasticon.html)
about speeding up interpreter loops where he spent some time discussing exactly
this topic, and I decided that I’d give it another go.

This is the tale of how I got my silly virtual machine to speed up by a lot. I
started out with a VM that would render Mandelbrot in about 29 seconds, and I
ended my investigations when that same program took about 1 second to complete.
Let’s talk about some of the techniques used!

## Silliness first

Remember, I didn’t really start out with the goal of making the interpreter
fast, so I started with a bunch of silly microoptimizations. This should tell
you that my approach was a little scatterbrained.

I started by [implementing direct threading](https://github.com/hellerve/cspfuck/commit/2954d32bc6aa952478c0640846aaf7214d0fd4a3).
This required only minor changes and was overall fairly mechanical: instead of
using a `while` loop and a `switch` dispatch, I now used `goto` and labels as
values. This is, overall, fairly brittle, because it requires the indices into
the array of labels to match the opcode numbers. This kind of double
book-keeping can lead to nasty bugs, but remains fairly common in the world of
programming language implementations, be it interpreters or compilers.

It’s also unreasonably effective: it brought down the execution time to 14
seconds from 29, a more than 2x improvement. This shocked me, because I had
assumed that an infinite loop paired with a `switch` statement would compile to
a `goto`-based structure anyway. Alas, it didn’t.

I then [added more minor optimizations](https://github.com/hellerve/cspfuck/commit/fb8b05158514bd2d37a1a89247d3bcbd0b6e3d03)
on a hunch: I added a flag that would disable wraparound if needed, which
removed the need for an extra check in addition and subtraction of the Brainfuck
tape head. I also removed launching a `pthread` if our program is
single-threaded, because execution can just be handed to the evaluation loop and
finish there. To my dismay, this didn’t lead to a major speedup, but it shaved
off another second of execution time. For those of you who are keeping track at home,
we’re now at 13 seconds.

This is where I talked to Adrien, showing off my work. He seemed interested in
seeing where this journey would lead me, and evaluated some optimizations
himself. [All](https://github.com/hellerve/cspfuck/commit/a865aa2f63a3e0db18e5d16a8d414528742490a5)
[of](https://github.com/hellerve/cspfuck/commit/6d1da5676e0a4bca5891f341ec1ecc9f756fce44)
[them](https://github.com/hellerve/cspfuck/commit/f29d163a8e5c441224b0e6938f9e95bce884317a)
seemed like voodoo to me—making the tape head an unsigned type speeds things up?
Nonetheless, we ended up removing another half second of execution time.

Most importantly, though, he referred me to [a series of blog posts by Eli
Bendersky](https://eli.thegreenplace.net/2017/adventures-in-jit-compilation-part-1-an-interpreter/)
where he discusses making an interpreter faster without this kind of voodoo,
using good old-fashioned optimizations. Grudgingly I dragged myself back into
the world of reasonable performance optimizations.

## A better instruction set

Eli Bendersky observes that much fun can be had by looking at the Brainfuck
opcodes themselves and optimizing them. He specifically talks about three
possible optimizations.

Firstly, he observes that both tape head movement and cell arithmetic can be
grouped: instead of doing three increments of a cell by one, for instance, you
can increment it by three once. Simple enough, right? It turns out that these
operations are the most common operations in Brainfuck, and [by implementing
this optimization](https://github.com/hellerve/cspfuck/commit/0e448179847b36bf1b25ea5134e3aba63169a45b)
we push execution time down to eight seconds. Not too bad at all.

Secondly, he observes that `[-]` and `[+]` just decrement or increment a cell
until it is set to zero, which means that we can just add another opcode that
will set the current cell to zero instead of looping alltogether. That’s a neat
trick, but alas one I had already implemented, so no dice.

The last one is a little trickier. There are a handful of loop patterns that
can profit from optimization, namely `[->+<]`, which moves data from one cell
to another, and `[<]` or `[>]`, which move the pointer to the next cell that is
set to zero. Handy! At this point, the code runs in just under four seconds on
my machine, and I was out of ideas for optimizations to try. Except that we’re
only at the end of the first blog post by now, so there is a lot of ground to
cover in our implementation.

## Just compile the darn thing

The next three blog posts in the series are concerned with Just-In-Time
compilation, first by hand and then using LLVM. I was not interested in adding
the huge dependency that is LLVM to my tiny explorative project, so I stuck
with emitting x86 machine code by hand to the very end.

It took a lot of time for me to get everything right. I had the blueprint that
Eli Bendersky provided me with, but I wanted the cake and eat it, too: I wanted
to keep all of the optimizations I had just added, have the JIT be runnable on
OS X and Linux, and have all of my CSP machinery still work.

This whole process took me [about 15 commits](https://github.com/hellerve/cspfuck/commits/jit)
to get right, and I’m not ashamed of any of them, even though most of them have
flaws that seem obvious in hindsight. But debugging a JIT is tricky: it has all
of the complexity of writing a compiler, and your emitted code only exists in
memory. On the plus side, I learned a lot of tricks in lldb, my new favorite
debugger! Did you know that you can dump your generated machine code by using
`disassemble --start-address <addr> --count <n>` inside the debugger? I don’t
know why you would need that except if you are writing machine code or are
reverse-engineering something the old-fashioned way, but it seemed like a
godsend at the time.

In the end, I had a working JIT compiler, though, and it runs the whole
Mandelbrot program in just over a second on my machine. Running it for the first
time was magical!

Most of my process is articulated much better than I could in [Eli Bendersky’s
second blog post in the series](https://eli.thegreenplace.net/2017/adventures-in-jit-compilation-part-2-an-x64-jit/),
though by leaving the optimizations in I was able to beat his simple handrolled
JIT by more than 2x. The most novel and interesting part in my
implementation is the part where I actually do inter-thread communication,
though.

Because I wasn’t ready for yet another session of banging my head against the
wall that is machine code, I wrote [the functions that do this](https://github.com/hellerve/cspfuck/blob/jit/src/eval.c#L29..L63)
in C and [emitted JIT code](https://github.com/hellerve/cspfuck/blob/jit/src/eval.c#L172..L188)
that would call these functions with the thread context and a reference to the
memory currently pointed to by the tape head instead. This didn’t make things
easy, exactly, but it did make them doable.

And that’s the whole story of how I emitted machine code by hand for the first
time!

## Fin

I learned a lot in a very short time during this project, so I consider it a
complete success. Usually I spend more time implementing tools and libraries to
make the developer more productive, as I consider this even more important than
to make the computer execute things more quickly. But learning about the other
side of the equation made me develop more empathy for the developers on the very
back end of compilers, and the people who spend large amounts of time making our
code just a tiny bit faster. I mostly rely on tools like LLVM or on the work of
others—as I do in Carp—to get this part to work.

At some point on my journey, I remembered an unfortunate exchange that I had at
[Never Graduate Week 2018](https://www.recurse.com/blog/114-never-graduate-week-2017-how-we-planned-and-ran-our-annual-alumni-week),
a yearly alumni get-together at the [Recurse Center](https://www.recurse.com/)
(go apply!). I talked to [Liuda Nikolaeva](https://twitter.com/LiudaNikolaeva)
about compilers, a topic that she recently became interested in. She was mostly
interested in compiler optimizations, and I was overly dismissive about her
attitude, which I immediately regretted. In a way, I’m sure that this exchange
played a role in my wanting to go that route, and it made me understand
her desire more. So I have to thank Liuda and apologize to her at the same
time, and also thank Adrien for sending me down that path!

If you’re interested in learning about any part of CSPfuck or its
implementation, I’d like to encourage you to play around with it! Both the
bytecode VM and the JIT are fairly small, at around 400 and 600 lines of code
respectively. If you have any questions, feel free to reach out via e-mail or
[open an issue](https://github.com/hellerve/cspfuck/issues)!
