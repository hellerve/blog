---
title: "Simple Sudoku Solvers SIII, EII: Assembly"
date: 2026-10-15
---

Welcome back to another round of [Simple Sudoku Solvers](https://blog.veitheller.de/sss/)!
If you are unfamiliar with the series, I suggest you start with [the first
post](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html)
or by perusing [the backlog](https://blog.veitheller.de/sss/). Last time, in
[Agda](https://blog.veitheller.de/Simple_Sudoku_Solvers_SIII,_EI:_Agda.html),
the solver wasn’t allowed to hand us a board without a correctness proof. Today
we go in the opposite direction: assembly, where nothing is checked at all, and
the machine stares at us directly.

To be quite specific, today we’ll write AArch64 assembly for macOS, because
that’s the machine I’m sitting at<sup><a href="#1">1</a></sup>. Surprisingly,
the solver is just a little over 200 lines, and as always, [the code is on
GitHub](https://github.com/hellerve/sudoku).

## Why assembly?

Season three is about solvers, specifications, and constraints, and assembly
might seem like the odd duck. But I think it fits!

After a proof assistant that checks everything, it’s worth looking at what’s
left when nothing is checked, and nothing is proven.

- **Nothing is hidden.** There is no allocator, no garbage collector, no
  runtime, and no compiler making choices for us. The board gets copied
  because we wrote exactly the instructions that copy it.
- **The machine already knows our tricks!** We’ve been doing bit-twiddling
  with candidate masks since [the Common Lisp
  episode](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_II:_Common_Lisp.html),
  and in [Forth](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EIII:_Forth.html)
  we had to roll our own popcount and lowest-bit search. The CPU has
  instructions for most of that, and today we get to use them directly.
  So, in many ways, we gain some abstractions!
- **The stack is, uh, the stack.** In Forth we built our own stack of boards
  for backtracking. Here, the machine stack is right there, so we might as
  well use it.

Since there are no types, everything is a number, and the only thing
standing between us and a wrong answer is our own diligence. Fun!

## A quick primer on reading AArch64

If you’ve never read ARM assembly before, here is what you need for this
post.

AArch64 has 31 general-purpose registers, `x0` to `x30`, each 64 bits wide.
Writing `w0` instead of `x0` means “the lower 32 bits of register 0”, and since
all of our numbers are small, we’ll use `w` registers most of the time. `sp` is
the stack pointer.

Instructions read like `op destination, source, source`, so `add w0, w1, w2`
means `w0 = w1 + w2`. Memory is only touched by loads and stores: `ldrb w9,
[x19, w21, uxtw]` loads the byte at address `x19 + w21` into `w9`, and `strb`
stores one. `[x7], #1` means “load from `x7`, then add 1 to `x7`”, which is
handy for walking through memory.

We call functions with `bl`, which jumps and stores the return address in
`x30`, and we return with `ret`, which jumps back to `x30`. Arguments go in `x0`
and up, and the result comes back in `x0`. A function may freely overwrite
`x0` to `x17` (`x18` is reserved), but has to restore `x19` to `x28` before
it returns, so those are where we keep anything we need to survive a call.

Labels like `1:` are local, and `b.ne 1b` means “branch to the closest label
`1` going **b**ackwards, if the last comparison was **n**ot **e**qual”. `1f`
looks **f**orwards. Comments start with `//`.

That should be enough to get through it. Everything else I’ll explain as we go.
I recommend keeping your favorite drawing tool (pen and paper work well) handy
to simulate the stack and register movement in your head. I’ve always had
trouble with the mnemonics, so I read assembly differently than most other
programming languages, modelling stack and register state as I go. Maybe that
works for you as well!

## The board

The board is the same flat array of 81 bytes that we’ve been using for most of
the series, with `0` for an empty cell. We put it in the data section (which
means it’s baked into the executable quite literally), with the puzzle already
filled in:

```
        .data
        .p2align 4
board:  .byte 8,0,0,0,0,0,0,0,0
        .byte 0,0,3,6,0,0,0,0,0
        .byte 0,7,0,0,9,0,2,0,0
        .byte 0,5,0,0,0,7,0,0,0
        .byte 0,0,0,0,4,5,7,0,0
        .byte 0,0,0,1,0,0,0,3,0
        .byte 0,0,1,0,0,0,0,6,8
        .byte 0,0,8,5,0,0,0,1,0
        .byte 0,9,0,0,0,0,4,0,0
```

This is the same puzzle we used in the Agda episode, and it’s one of the harder
ones I use for checking. `.p2align 4` aligns the board to 16 bytes. Strictly
speaking we don’t need that, but we’re about to copy it around in 16-byte
chunks, and it doesn’t cost us anything, so we might as well do it.

There is one global board, and the solver mutates it in place. Whatever is in
`board` when `solve` returns is our answer.

## The solver

As always, we’ll go outside-in. This is `solve` in its entirety, and it is the
longest function in the file, so we’ll take it in pieces.

```
solve:
        stp     x29, x30, [sp, #-64]!
        mov     x29, sp
        stp     x19, x20, [sp, #16]
        stp     x21, x22, [sp, #32]
        str     x23, [sp, #48]
        bl      propagate
        cbz     w0, fail
```

The first five lines are the prologue, which is the ceremony every function
that calls other functions usually goes through (it’s also the main thing that
distinguishes functions and labels inside the block). We reserve 64 bytes on
the stack, save our return address (`x30`) and the frame pointer (`x29`), and
save the callee-saved registers we’re about to use. `stp` stores a pair of
registers in one go, and the `!` means “update `sp` first”.

The rest is familiar. We propagate, and if propagation hits a dead end, we
fail. `cbz` is “compare and branch if zero”, which comes up a lot, because it’s
basically an `if` statement.

Next up is finding the cell with the fewest candidates:

```
        adrp    x19, board@PAGE
        add     x19, x19, board@PAGEOFF
        mov     w20, #-1                // best cell
        mov     w21, #10                // its candidate count
        mov     w22, #0                 // cell
mrv:
        ldrb    w9, [x19, w22, uxtw]
        cbnz    w9, 1f
        mov     w0, w22
        bl      cands
        fmov    s0, w0
        cnt     v0.8b, v0.8b
        addv    b0, v0.8b
        fmov    w9, s0
        cmp     w9, w21
        b.hs    1f
        mov     w21, w9
        mov     w20, w22
        mov     w23, w0                 // its candidates
1:      add     w22, w22, #1
        cmp     w22, #81
        b.lo    mrv
        tbnz    w20, #31, done          // no empty cell left
```

The first two lines load the address of the board. On AArch64, an instruction
is always 32 bits wide, so you can’t fit a full address into one. Instead,
`adrp` gets the address of the 4KB page the board is on, and `add` adds the
offset within that page. You’ll see this pair a lot. It’s a weird little
ceremony if you ask me, but nobody did.

Then we loop over all 81 cells. Filled cells are skipped. For empty cells, we
get the candidate mask from `cands` and count its bits, and if the count beats
the best one so far, we remember the cell, the count, and the mask.

Counting bits is the first weird, delightful bit. Base AArch64 has no
instruction for counting the bits in a general-purpose
register<sup><a href="#2">2</a></sup>. It does have one for its vector
registers, though. `cnt` counts the bits in each of eight bytes, and
`addv` adds those eight counts up. So we move our mask over to the vector
unit with `fmov`, count there, and move the result back. It’s four instructions
instead of the nine-iteration loop we wrote in Forth where we implemented
bit-counting by hand.

`w20` starts out as `-1` (all ones in binary). If it’s unchanged after the
loop, there are no empty cells left, and we’re done. `tbnz` tests a single bit
and branches if it is set, and bit 31 is the sign bit, so this is a cheap “is
it negative?” check, and another one of those idioms you just kind of have to
know.

Note that we never check if the best cell has zero candidates. Propagation
already returns a dead end for that, so by the time we select one, every cell
has at least one candidate.

Now for the backtracking:

```
        sub     sp, sp, #96
        copy81  sp, x19
try:
        rbit    w9, w23
        clz     w9, w9
        strb    w9, [x19, w20, uxtw]
        bl      solve
        cbnz    w0, found
        copy81  x19, sp
        sub     w9, w23, #1
        and     w23, w23, w9 // drop the digit we tried
        cbnz    w23, try
        add     sp, sp, #96
```

Before we try anything, we make room on the stack and copy the board there.
The board is 81 bytes, but the stack pointer has to stay aligned to 16 bytes,
so we take 96, more ceremony.

This is basically what we did in Forth with the board stack, except that it
lives on the “real” stack, grows by itself with recursion, and gets popped when
we return. We don’t have to pick a maximum depth this time, though the depth is
bounded anyway, since every level fills at least one cell.

Then we try each candidate. `rbit` reverses the bits of the mask and `clz`
counts the leading zeros, which together give us the position of the lowest
set bit. That’s the trailing zero count we also used before, in [Rust](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_V:_Rust.html),
and as we’ll see in a minute, the position of the bit maps to the digit.
Finally, we write the digit into the cell and recurse!

If the recursive call succeeds, we’re done. Otherwise we copy the saved board
back and drop the digit we just tried from the mask. `x & (x - 1)` clears the
lowest set bit of `x` (still one of my favorite little tricks from the series),
and when the mask is empty, we’ve run out of options.

The rest is bookkeeping:

```
fail:
        mov     w0, #0
        b       1f
found:
        add     sp, sp, #96
done:
        mov     w0, #1
1:      ldr     x23, [sp, #48]
        ldp     x21, x22, [sp, #32]
        ldp     x19, x20, [sp, #16]
        ldp     x29, x30, [sp], #64
        ret
```

We set the return value and undo the prologue, in reverse. The only subtle part
is the order of the labels. `found` has to give the 96 bytes of board back
before falling through into `done`. `fail` and `done` can be reached without
them, so they have to navigate around `found`.

You won’t notice right away if you mess any of this up (ask me how I know). If
you’re lucky it’ll just crash, if you’re not, it’ll return somewhere random.
There are no types to catch you, and nothing checks that you restore what you
saved. Functions by hand.

### Copying boards

`copy81` is a macro. The assembler has a simple macro system, which we use to
avoid writing the same eight lines twice (you know that I’m a macro fiend):

```
.macro copy81 dst, src
        ldp     q0, q1, [\src]
        stp     q0, q1, [\dst]
        ldp     q0, q1, [\src, #32]
        stp     q0, q1, [\dst, #32]
        ldr     q0, [\src, #64]
        str     q0, [\dst, #64]
        ldrb    w9, [\src, #80]
        strb    w9, [\dst, #80]
.endm
```

`q0` and `q1` are 128-bit vector registers, so one `ldp` moves 32 bytes. Two
pairs and a single give us 80 bytes, and one more byte gets us to 81.

It’s basically `memcpy`, specialized to our board!

## Propagation

Propagation is the loop we’ve written way too often at this point. We fill
every cell that has a single candidate, and keep going until nothing changes.

```
propagate:
        stp     x29, x30, [sp, #-48]!
        mov     x29, sp
        stp     x19, x20, [sp, #16]
        str     x21, [sp, #32]
        adrp    x19, board@PAGE
        add     x19, x19, board@PAGEOFF
again:
        mov     w20, #0 // changed?
        mov     w21, #0 // cell
cell:
        ldrb    w9, [x19, w21, uxtw]
        cbnz    w9, next
        mov     w0, w21
        bl      cands
        cbz     w0, dead
        sub     w9, w0, #1
        tst     w0, w9
        b.ne    next // more than one candidate
        rbit    w9, w0
        clz     w9, w9
        strb    w9, [x19, w21, uxtw]
        mov     w20, #1
next:
        add     w21, w21, #1
        cmp     w21, #81
        b.lo    cell
        cbnz    w20, again
        mov     w0, #1
        b       1f
dead:
        mov     w0, #0
1:      ldr     x21, [sp, #32]
        ldp     x19, x20, [sp, #16]
        ldp     x29, x30, [sp], #48
        ret
```

There are two small departures from earlier versions.

First, we don’t need popcount here. We only care whether a mask has exactly
one bit set, and our `x & (x - 1)` trick can answer that. It’s zero exactly
when there was at most one bit. `tst` computes the `and` without storing it,
so we don’t even need an extra register.

Second, the contradiction check is folded in. In Forth, we had a separate
`contradiction?` pass after propagation. Here, if any empty cell has no
candidates, we nope out on the spot with `cbz w0, dead`. It means that an
invalid input puzzle can pass, but surely no one would hand us the wrong input?

## Candidates

This is where the bit tricks pay their dues. We need the set of digits that
don’t appear in a cell’s row, column, or box. As in every other bitmask
solver so far, we collect the digits we’ve seen in a mask and invert it at
the end.

There is one change from before, though. Usually digit `d` lives in bit `d-1`,
so we have to skip empty cells, because there is no bit -1.

Here, digit `d` lives in bit `d`. Empty cells are 0, so they set bit 0, and we
throw that bit away at the end. That means we never have to check whether a cell
is empty, and no branching is needed.

It also means that the position of a bit is the digit, which is what saved us
the `+ 1` in `solve` and `propagate`.

Since we do this for 27 cells, it too gets its own macro:

```
.macro mark used, at
        ldrb    w10, \at
        lsl     w10, w6, w10
        orr     \used, \used, w10
.endm
```

`w6` holds 1, so this loads a cell, shifts 1 left by its value, and `or`s it
into the mask. With that, the candidate function becomes three loops:

```
cands:
        adrp    x9, board@PAGE
        add     x9, x9, board@PAGEOFF
        mov     w2, #9
        udiv    w3, w0, w2       // row
        msub    w4, w3, w2, w0   // col
        mov     w5, #0           // used digits
        mov     w6, #1

        mul     w7, w3, w2
        add     x7, x9, w7, uxtw // first cell of the row
        mov     w8, #9
1:      mark    w5, "[x7], #1"
        subs    w8, w8, #1
        b.ne    1b

        add     x7, x9, w4, uxtw // first cell of the column
        mov     w8, #9
2:      mark    w5, "[x7], #9"
        subs    w8, w8, #1
        b.ne    2b

        mov     w2, #3
        udiv    w3, w3, w2
        mov     w8, #27
        mul     w3, w3, w8       // box row offset
        udiv    w4, w4, w2
        mul     w4, w4, w2       // box column offset
        add     w3, w3, w4
        add     x7, x9, w3, uxtw // first cell of the box
        mov     w8, #3
3:      mark    w5, "[x7]"
        mark    w5, "[x7, #1]"
        mark    w5, "[x7, #2]"
        add     x7, x7, #9
        subs    w8, w8, #1
        b.ne    3b

        mvn     w0, w5
        and     w0, w0, #0x3fe
        ret
```

There is no modulo instruction, so to get the column we divide (`udiv`) and
then compute `cell - row * 9` with `msub`, which multiplies and subtracts in
one go. This seems like a clever trick at first, but again, this is one of those
tricks you’ll just find online and then try to remember every time until it’s
hard-wired in your brain.

The row loop walks nine consecutive bytes, and the column loop walks with a
stride of nine. The box loop unrolls the three cells of each box row and then
skips ahead by nine. The box offset is the same arithmetic we did in Forth, 27
for every block of rows, and 3 for every block of columns. `cands` doesn’t call
anything, so it only uses registers it may clobber, and we can skip the
prologue entirely. This might not be a good idea in big programs written by
multiple people, but it’s fine between lazy friends.

At the end, `mvn` inverts the mask and the `and` keeps bits 1 to 9. That throws
away bit 0, where the empty cells went, and all the bits above 9, which the
inversion set (but which we don’t care about).

And that’s about it!

## How fast is it?

On my machine, the whole program takes about 4 milliseconds, and roughly 1.7
of those are the operating system starting a process. The Python reference takes
about 70 milliseconds for the same puzzle, and Agda took 2.7 seconds. I didn’t do
anything clever to make it fast, either. It recomputes candidates from scratch
every time, just like the Forth solver did. There just isn’t anything to slow us
down except our own laziness.

## Fin

We wrote a Sudoku solver in about 200 lines of assembly, and we used native bit
instructions to do what we’ve been emulating before. We kept our backtracking
boards on the “real” stack, and we learned some cool new tricks like
`x & (x - 1)`.

This episode remains the surprise guest this season, but I did enjoy writing it.
If the Agda episode was about trusting nothing but our spec, this was about
trusting nothing, period. Nothing verified that we restored our registers or
that our stack stayed aligned.

Personally, I prefer the proof, but it was fun to find out how to do it with
just our CPU.

In the next episode, we go even lower: Verilog, where there are no
instructions anymore, only wires and blinkenlights. See you there!

#### Footnotes

<span id="1">1.</span> If you’re on Linux on an ARM machine, two things change,
according to untested claims by a language genie. System calls take their
number in `x8` and use `svc #0`, and `write` is number 64 there. Addresses are
written as `adrp x9, board` and `add x9, x9, :lo12:board`.

By the way, Apple doesn’t promise that system call numbers stay stable, and
officially you should go through the C library. For a blog post, I think we can
live with that. Just harden your production version a bit.

<span id="2">2.</span> Newer revisions of the architecture added one, but I
didn’t want to depend on that, and I also wanted to show what you’ll see in a
lot of other places.
