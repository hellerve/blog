---
title: "Simple Sudoku Solvers SII, EIII: Forth"
date: 2026-03-21
---

Welcome back to another round of [Simple Sudoku Solvers](https://blog.veitheller.de/sss/)!
If you are unfamiliar with the series, I suggest you start with [the first
post](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html)
or by perusing [the backlog](https://blog.veitheller.de/sss/), otherwise hold on tight,
because today we’re stepping into the desert.

Forth is a language that feels like arriving at a ghost town. The streets are empty, the
signs are weathered, and the few inhabitants look at you funny. But once you learn the
local customs, you discover that everything you need is right there, and has been for
decades, you just have to hold it right.

Today we’ll be using [GNU Forth (gforth)](https://gforth.org/) as our runtime, which is
the most widely available Forth implementation and the one I’d recommend for following
along. As always, [the code is on GitHub](https://github.com/hellerve/sudoku).

## Why Forth?

Forth is not popular (well, maybe for a certain crowd maybe it is). It is most certainly
not trendy. All it has is words, and it turns out that’s enough!

Here’s why I enjoyed work with it:

- **Radical minimalism.** Forth gives you a stack, a dictionary of “words” (think: functions),
  and gets out of your way. There are no types, no syntax to speak of, and almost no
  built-in abstractions (though I do not know what the different implementations
  offer). The language specification fits in a few pages.
- **Everything is on the stack.** There are no local variables (well, almost). Function
  arguments, intermediate values, return values, everything lives on the data stack.
  Reading Forth code means mentally simulating a stack machine, which is both a
  meditative exercise and a mild form of torture. I definitely have “first
  semester of undergrad” flashbacks whenever I do that exercise. I really sucked
  at it then, and maybe you can still tell.
- **Opacity as a feature.** Forth code is pretty hard to read for me. But it’s a trade-off,
  like most syntax. The extreme brevity and regularity of the language mean that once you
  can read it, you can read *all of it*. There are no dark corners, no hidden machinery.
  What you see is what gets executed. [In a talk](https://www.youtube.com/live/EkbcI3KgUuY?t=8360s)
  I gave a few years ago I talked about abstractions and how comments are implemented in
  Forth, because it’s a nice mind-bender.
- **Real-world power.** Forth has been used in embedded systems, spacecraft (it ran on
  [the Philae lander](https://www.youtube.com/live/EkbcI3KgUuY?t=8360s)), telescopes, and
  boot loaders. It’s a language that punches well above its weight class in terms of what
  you can do with it.

I’m not going to pretend I’m a Forth expert. Writing this solver was a lot of staring at
stack diagrams, muttering “swap... over... nip...” under my breath, and occasionally getting
it right. But I did come away with a genuine appreciation for the language’s strange beauty,
and I expanded my horizon, and maybe you will, too.

## A quick primer on reading Forth

Before we dive in, a few things will help you read the code.

Forth code is a sequence of **words** separated by spaces. Words are executed left to right.
Each word can push values onto or pop values off **the stack**. That’s basically it.

By convention, stack effects are documented in parentheses: `( before -- after )`. For
example, `( n -- n n )` means “takes one value and leaves two copies of it”. The most
common stack manipulation words you’ll see are:

- `dup` — duplicate the top value `( a -- a a )`
- `swap` — swap the top two `( a b -- b a )`
- `over` — copy the second value over the top `( a b -- a b a )`
- `nip` — remove the second value `( a b -- b )`
- `drop` — remove the top value `( a b -- a )`

Control flow uses `if ... else ... then`. Yes, `then` comes last, it closes the conditional,
it doesn’t start one. Loops use `do ... loop` with `i` as the loop variable. `:` starts a
word definition, `;` ends it. And `\` starts a line comment.

With that, you should be able to follow along even if you’ve never touched Forth. If it still
feels confusing, that’s normal. It gets better, I think.

## The implementation

As with all our solvers, we’ll go outside-in, starting with the high-level solver and working
our way into the guts. But first, a quick look at how we represent the board.

### Board and geometry

```
create board  81 allot

: board@  ( idx -- val )  board + c@ ;
: board!  ( val idx -- )  board + c! ;
```

`create board 81 allot` reserves 81 bytes of memory. That’s our 9×9 grid, flattened into a
single array. `board@` and `board!` read from and write to it by offsetting from the base
address. `c@` and `c!` are Forth’s byte-level memory access words. The `c` stands for
“character” and is a remnant from Forth’s early days.

Since we’re working with a flat array, we need helpers to find the starts of rows,
columns, and boxes:

```
: row-start  ( idx -- r )  9 /  9 * ;
: col-start  ( idx -- c )  9 mod ;
: box-start  ( idx -- b )  dup 27 / 27 *  swap 9 mod 3 / 3 * + ;
```

These convert a cell index (`0–80`) into the starting index of its row, column, or 3x3 box.
`box-start` is the gnarliest, it finds the top-left corner of the box by separately computing
the row contribution (which block-row times 27) and the column contribution (which
block-column times 3). It’s not elegant, but it’s correct, and by now you might even know all
the magic numbers (we’ve seen all of them before).

### The solver

Let’s look at the main event. Fair warning: this is the most involved ~~function~~word in the
whole program.

```
: solve  ( -- solved? )
  propagate 0= if false exit then
  solved?       if true  exit then
  find-mrv dup 0< if drop false exit then
  dup candidates swap                     \ ( mask idx )
  9 0 do
    over 1 and if
      push-board
      i 1+  over  board!
      recurse if  drop drop true  unloop exit  then
      pop-board
    then
    swap 1 rshift swap
  loop
  2drop false ;
```

Let me break this down, because there’s a lot happening in these few lines.

The first three lines are our familiar pattern: propagate (fail if inconsistent), check if
solved (return if so), find the MRV cell (fail if none). `find-mrv` returns an index or
`-1` as a marker, so we check for that.

Then things get properly Forth-y. We compute the candidate mask for our chosen cell and
`swap` to get the stack into `( mask idx )` order. This pair will live on the stack for
the entire loop—our implicit “local variables”.

The loop goes from 0 to 8 (nine iterations, one per possible digit). At each step:

1. `over 1 and` peeks at the lowest bit of the mask without consuming it—is this digit a
   candidate?
2. If yes: `push-board` saves the entire board (more on this shortly), `i 1+ over board!`
   fills in the digit (`i` is the loop counter, so `i 1+` gives us the digit 1–9), and
   `recurse` tries to solve the resulting board.
3. If `recurse` succeeds `drop drop` removes our mask and idx, `unloop` cleans the loop’s
   bookkeeping off the return stack, and we `exit` with `true`.
4. If it fails: `pop-board` restores the saved board, and we try the next candidate.
5. `swap 1 rshift swap` shifts the mask right by one bit to line up the next candidate,
   keeping `idx` on top.

If we exhaust all candidates without finding a solution, `2drop false` cleans up the mask
and index and signals failure to our caller.

A few things worth noting here. `recurse` is Forth’s mechanism for self-reference: a word
can’t refer to itself by name during its own definition (the word isn’t in the dictionary
yet!), so `recurse` exists as an explicit “call the word currently being defined”. And
`unloop` before `exit` is required to clean up the loop’s internal state from the return
stack. Without it, we’d corrupt the return path. This is a detail that for me,
as someone who’s written a fair amount of compilers, comes semi-naturally, but I
don’t think it does for most other programmers.

### The backtracking stack

This is probably the most Forth-specific piece of the whole solver. In every other
language, we either copied the board, relied on immutable structures, or made stack
copies the compiler optimized for us. In Forth, we do it ourselves.

```
50 constant max-depth
create board-stack  max-depth 81 * allot
variable sp   0 sp !

: push-board  ( -- )
  board  board-stack sp @ 81 * +  81 move  1 sp +! ;
: pop-board   ( -- )
  -1 sp +!  board-stack sp @ 81 * +  board  81 move ;
```

We pre-allocate a stack of 50 board snapshots (`50 x 81 => 4,050 bytes` should be acceptable).
`sp` is our stack pointer. `push-board` copies the current board into the next free slot using
`move` (Forth’s `memcpy`) and bumps the pointer. `pop-board` decrements and copies back.

This is raw but completely transparent. There’s no hidden allocation or copying strategy to
reason about. You’re just moving bytes between two regions of memory. It’s as close to the
metal as we’ll get in this series, at least until assembly in season three (stay
tuned and whatnot).

50 levels of depth is extremely generous for our purposes. In practice, even the hardest
Sudoku puzzles rarely need more than a handful of branching levels. But memory is cheap and
we’re a paranoid bunch.

### Propagation

Propagation is split into three small words working together:

```
: solved?  ( -- flag )
  true  81 0 do  i board@ 0= if drop false leave then  loop ;

: contradiction?  ( -- flag )
  false
  81 0 do
    i board@ 0= if  i candidates 0= if drop true leave then  then
  loop ;

: fill-singles  ( -- changed? )
  false
  81 0 do
    i board@ 0= if
      i candidates
      dup popcount 1 = if  lowest-bit 1+  i board!  drop true
                      else  drop  then
    then
  loop ;

: propagate  ( -- ok? )
  begin fill-singles 0= until  contradiction? invert ;
```

`solved?` checks if every cell is nonzero. `contradiction?` checks if any empty cell has zero
candidates. `fill-singles` scans for cells with exactly one candidate and fills them in, returning
whether it changed anything. `propagate` ties them together by running `fill-singles` until it
reports no changes (`begin ... until`) and then checking for contradictions.

I want to stay on `fill-singles` for a moment, because it showcases how Forth handles what
other languages would express with mutable flags and return statements. We start with
`false` on the stack as our flag of change. For each empty cell, we compute candidates,
check the popcount, and if it’s 1, we extract the digit (`lowest-bit 1+`—the lowest set
bit’s position plus one gives us the digit), write it to the board, `drop` the leftover
mask, and push `true` as our new flag. Otherwise we just `drop` the mask and move on.
At the end of the loop, whatever boolean sits on top is our answer.

There’s no assignment to a variable or return statement. The flag materializes as a side
effect of the computation. Expressing control through data flow, if you want to sound dramatic.

### Candidate computation

We use the same bitmask trick as in Common Lisp, Rust, APL, and Carp: a 9-bit mask where
bit `k` represents digit `k+1`.

```
: digit>bit  ( d -- bit )  1-  1 swap lshift ;
511 constant full-mask

: row-used  ( idx -- mask )
  row-start  0
  9 0 do  over i + board@  dup 0> if digit>bit or else drop then  loop
  nip ;

: col-used  ( idx -- mask )
  col-start  0
  9 0 do  over i 9 * + board@  dup 0> if digit>bit or else drop then  loop
  nip ;

: box-used  ( idx -- mask )
  box-start  0
  3 0 do  3 0 do
    over j 9 * i + + board@  dup 0> if digit>bit or else drop then
  loop loop
  nip ;

: candidates  ( idx -- mask )
  dup row-used  over col-used or  over box-used or  nip  full-mask xor ;
```

The three `*-used` words follow the same pattern: start with the anchor index and an
accumulator of `0`, loop over the relevant cells, and `or` each nonzero digit’s bit
into the accumulator. The `nip` at the end discards the anchor, leaving just the mask.

`candidates` combines all three and flips the result. The `dup ... over ... over ... nip`
thing-a-majig keeps the index alive across three word calls without ever storing it in
a variable (pure stack threading). If you trace through it carefully, you’ll see the
index getting duplicated, consumed by each `*-used` call, duplicated again, consumed
again, and finally discarded when we’re done.

I find `box-used` particularly satisfying with its nested `do` loops and the `j 9 * i +`
offset computation. It’s doing the same 3x3 walk we’ve done in every solver, but here
the loop variables `i` and `j` come from Forth’s own loop mechanism, and the whole thing
reads almost like pseudocode (albeit in postfix).

### Bit helpers

Since gforth doesn’t give us popcount or trailing-zero-count as primitives, we roll our
own, it’s not our first rodeo:

```
: popcount  ( mask -- n )
  0 swap  9 0 do  dup 1 and  rot +  swap  1 rshift  loop  drop ;

: lowest-bit  ( mask -- pos )
  0  begin  over 1 and 0=  while  1+  swap 1 rshift swap  repeat  nip ;
```

`popcount` walks through 9 bits, adding each to an accumulator. `lowest-bit` shifts until
it finds a set bit and counts how many shifts it took. Neither is clever, but
they get the job done.

### MRV selection

The last piece: finding the cell with the fewest candidates for branching.

```
variable mrv-best

: find-mrv  ( -- idx )
  10 mrv-best !
  -1
  81 0 do
    i board@ 0= if
      i candidates popcount
      dup mrv-best @ < if
        mrv-best !  drop i
      else drop then
    then
  loop ;
```

Here we break our “no variables” streak. `mrv-best` holds the current best candidate
count, and the top of the data stack holds the current best index (starting at `-1` as a
marker). For each empty cell, we compute the popcount and compare. If it’s an improvement,
we update both.

This is one of the few places where a variable felt genuinely more natural than pure stack
manipulation. Keeping both the best count and the best index on the stack while also juggling
the loop counter and the candidate count probably would have been possible, but the resulting
stack acrobatics would have made the code much harder to follow for very little benefit. Even
my madness has its limits.

## Fin

Forth asks more of you than almost any other language. It requires more mental computation and
more discipline. A lot of the language features we’ve used over the last 1.5
seasons are notably absent.

And yet, there is something deeply compelling about it. The transparency of every word doing
exactly what it says, nothing being hidden, and the entire system fitting in one person’s head
without it being too inscrutable. The way you build up from nothing, defining words that become
the vocabulary for your specific problem. By the end, `solve` reads almost like a sentence:
propagate, check if solved, find the best cell, try each candidate, recurse. The fact that this
was built on nothing but a stack and some memory still feels like a magic trick to me.

Our solver is about 140 lines, on the longer side in terms of raw code, but also the simplest
in terms of what the language gives us to work with. No macros, no monads, no constraint
solvers, no type inference. And yet we have all that we need.

If you’ve made it this far, thank you for walking through the desert with me. The next stop has
considerably more amenities: we’re heading to Smalltalk, a language that is Forth’s philosophical
opposite in almost every way. See you there!
