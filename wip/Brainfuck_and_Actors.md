I know that [in my last blog post](http://blog.veitheller.de/An_Update.html) I
said I wouldn’t write for a while, but four days before my marriage I had one
of the worst ideas I’ve ever had: why not fuse CSP and Brainfuck. Naturally I
dropped everything that I was doing and got to work.

So, without further ado I present to you
[cspfuck](https://github.com/hellerve/cspfuck), Brainfuck with actors.

## What is it?

Any cspfuck is comprised of multiple concurrent Brainfuck programs, or actors.
In a source file you write them as multiple programs separated by an empty line.
All of those actors will run as their own thread.

Thus far all we have is a version of Brainfuck that let’s you execute multiple
programs at once. While certainly useful for, you know, stuff, that’s not very
interesting. This is where my extra secret indgredient comes in: channels. Every
actor can send data to other adjacent actors (the one above and the one below)
using the new `^` and `v` primitives. These two primitives send the value
currently under the “tape head” to the other actors. Receiving a value is
directionless—although the upper channel takes precedence—and is done by using
`u`, which will receive the value into the cell under the “tape head”.

In the words of [fellow redditor u/plistig](https://www.reddit.com/r/ProgrammingLanguages/comments/8b6bm2/cspfck_brainfck_with_actors/dx4b9s0/):
“It’s an abomination! I love it!” I feel similarly, friend.

## How does it work?

This is where I show you how truly limited my knowledge of pthreads and
concurrent programming in C is.

I started by implementing a basic virtual machine for Brainfuck. This is
relatively simple. I then added support for multiple concurrent programs, which
is as simple as splitting up the programs in the parser and iterating over them
in a loop, starting a thread that runs the evaluator with each of them.

```
void eval(bytecode* code) {
  int i = 0;
  int h = 0;
  int t[30000]; // initialize to 0 somehow

  while(1) {
    bytecode c = code[i++];
    switch (c.code) {
      case INC: t[h]++; break;
      case DEC: t[h]--; break;
      case FWD: h++; break;
      case BCK: h--; break;
      case PRN: printf("%c", t[h]); break;
      case READ: scanf("%c", (char*)&t[h]); break;
      case STARTL: if(!t[h]) i = c.arg; break;
      case ENDL: if(t[h]) i = c.arg; break;
      case HALT: return;
    }
  }
}
```
<div class="figure-label">Fig. 1: A Brainfuck VM.</div>

Figure 1 was basically the entire VM part at that point.

I then added what might be the worst idea in the whole VM: pointer-based
channels. Every channel is basically represented as a pointer to an integer,
representing the value in the channel, and another integer pointer that acts as
a flag signifying whether there is new information in the channel.

```
typedef struct {
  int* up;
  int* up_written;
  int* down;
  int* down_written;
  bytecode* code;
} actor_ctx;
```
<div class="figure-label">Fig. 2: Context for each actor.</div>

The first actor will not have an up channel, the last one doesn’t have a down
channel. This is an arbitrary restriction since we could also connect the first
and last channels to form a ring-like structure, but at this point I decided for
whatever was simpler to implement.

Writing to a channel is thus simple: it is just a matter of taking whatever is
in the current cell, stuffing it into the appropriate channel, and setting the
flag to one.

```
// Writing to the up channel:
*up = t[h];
*up_written = 1;
```
<div class="figure-label">
  Fig. 3: Writing to the up channel (performed by `^`).
</div>

Reading is a blocking operation: we wait until either the flag for the upper or
lower channel is set to one, and then read from it, resetting it to zero. This
will avoid rereading the same value twice. It does mean that each channel is
bidirectional, though, and each actor could possibly read what they wrote
themselves. This could of course be rectified, but there is a whole class of
interesting concurrency bugs in the VM that I’d like to keep for research
purposes. If they bug you, feel free to send me a pull request!

## Final words

As I said in the beginning, this is probably one of the worst ideas I ever had.
As so very often with those ideas, though, I had more fun implementing it than
I had programming in a long time.

It is a nice way to leave programming for a while before the wedding, and I am
even more excited to take a break now.
