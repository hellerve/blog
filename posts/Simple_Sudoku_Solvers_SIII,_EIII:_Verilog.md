---
title: "Simple Sudoku Solvers SIII, EIII: Verilog"
date: 2026-11-13
---

Welcome back to another round of [Simple Sudoku Solvers](https://blog.veitheller.de/sss/)!
If you are unfamiliar with the series, I suggest you start with [the first
post](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html)
or by perusing [the backlog](https://blog.veitheller.de/sss/). Last time, in
[assembly](https://blog.veitheller.de/Simple_Sudoku_Solvers_SIII,_EII:_Assembly.html),
we told the CPU exactly what to do, one instruction at a time. Today we go one
level lower and build the machine directly in Verilog.

Verilog is a hardware description language. You don’t write a program that
will at some point run on hardware. Instead, you describe a circuit, and the
compiler turns that description into gates and flip-flops. We’ll run it in a
simulator, because I don’t have a Sudoku chip lying around (future work), but
the solver is real hardware in the sense that a synthesis tool accepts it and
tells us how many gates it would take. Still counts, even with sim2real TBD.

The solver is about 145 lines, plus 40 lines of ceremony to feed it a puzzle
and print the result. You can run it with `iverilog -o sudoku sudoku.v && vvp
sudoku`, using [Icarus Verilog](https://github.com/steveicarus/iverilog). I
didn’t test it anywhere else. As always, [the code is on GitHub](https://github.com/hellerve/sudoku).

## Why Verilog?

Season three is about solvers, specifications, and constraints, and somehow
also about hardware and the lowest of low levels. That’s because hardware is
possibly the purest version of a specification (and, to be quite honest,
because I felt like it).

So, what’s cool about Verilog?

- **Everything happens at once.** In most solvers so far, we looped over the
  81 cells one after the other or wrote the constraints that will then execute
  a loop-like structure for us. In hardware, we do away with the loop. Instead,
  we use 81 copies of the same circuit that all compute their result in the
  same clock cycle. Sounds weird? We’ll get there!
- **Time is a first-class citizen.** Nothing happens “next” unless a clock edge
  fires, and we’ll have to decide what our solver does each cycle, which turns
  our recursive search into a state machine (surprisingly new to us!).
- **There is no recursion and no heap.** Every bit of memory is an explicitly
  conjured flip-flop. Backtracking needs a stack, and we’ll have to build it
  ourselves, much like in
  [Forth](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EIII:_Forth.html).

I think this was the strangest episode so far, for me. I don’t mean that in a
bad way.

## A quick primer on reading Verilog

If you’ve never read Verilog before, here is what you need for this post.

A `module` is a circuit with inputs and outputs. `wire [8:0] x` is a bundle of
nine wires, numbered 8 down to 0. `reg` is a bit of a misnomer: it’s a value
we assign in a procedural block, and whether it becomes an actual register is
actually dependent on our usage of it.

There are two kinds of blocks we care about. `always @*` describes
combinational logic. Its outputs are a pure function of its inputs, and they
change whenever the inputs do. Think of it as a very big expression.

`always @(posedge clk)`, on the other hand, describes sequential logic. It
runs once at every rising clock edge, and the `<=` assignments inside it all
happen together at the end of that edge. That’s where all our state lives.

`for` loops exist, but in synthesizable code they are unrolled completely. A
loop over 81 cells is 81 copies of its body. Keep that in mind, it will matter.
It’s also very cool, so it’s worth keeping in mind anyway.

## The board

The board is the same 81 cells as always, but since hardware doesn’t really do
arrays of bytes, it is one 324-bit vector, with four bits per cell and `0` for
an empty cell:

```
module sudoku (
  input  wire         clk,
  input  wire         start,
  input  wire [323:0] puzzle,
  output reg          done,
  output reg          solved,
  output reg  [323:0] board
);
```

We have three input wires and output `reg`s (remember, not necessarily
registers). That’s the whole interface of our chip.

We put a puzzle on the `puzzle` wires, hold `start` for one cycle, and wait
until `done` goes high (think in currents). Then, if `solved` is set, `board`
has the answer. Otherwise, the board is unsolvable.

Reading a cell is a slice, which we wrap in a small function.

```
function [3:0] at(input [323:0] b, input integer i);
  at = b[4 * i +: 4];
endfunction
```

Slices work like in many other languages, except that the operators `+:` and
`-:` encode a direction from the start index. Here, we say “start at `4 * i`
and take the next `4` bits”.

Units are the 27 rows, columns, and boxes we always care about. We describe
them with a function that tells us which cell is the `k`-th member of unit
`u`:

```
function [6:0] member(input integer u, input integer k);
  if (u < 9)       member = u * 9 + k;
  else if (u < 18) member = k * 9 + (u - 9);
  else             member = ((u - 18) / 3) * 27 + ((u - 18) % 3) * 3
                          + (k / 3) * 9 + k % 3;
endfunction
```

The first 9 are the rows, then come the columns, and the icky expression at
the end there is the box arithmetic with our usual set of magic numbers.

Fun fact: this looks like it does division at runtime, but it doesn’t. We only
ever call it with constants from unrolled loops, so the synthesis tool
computes all of it ahead of time (much like an optimizing compiler would).
What is left is just the wiring, and the box arithmetic decides which wires
connect.

## Bitty digits

Like in many previous installments, we track digits as bitmasks. And like in
our assembly episode, when it’s convenient we use the trick where digit `d`
lives in bit `d`, and an empty cell lands in bit 0, which we throw away:

```
function [8:0] bit_of(input [3:0] d);
  reg [9:0] wide;
  begin
    wide = 10'b1 << d;
    bit_of = wide[9:1];
  end
endfunction
```

In hardware this is a decoder, four wires in, ten wires out, one of them lights
up. Dropping bit 0 is just not connecting it. We don’t need a branch, and in
this case not even an instruction. These sorts of tricks are annoying the
higher up the stack we go, but they come naturally to us when we’re thinking in
circuits and currents.

We also need the opposite direction, from a single bit back to a digit, and a
popcount:

```
function [3:0] digit(input [8:0] onehot);
  integer d;
  begin
    digit = 0;
    for (d = 1; d <= 9; d = d + 1)
      if (onehot[d - 1]) digit = d;
  end
endfunction

function [3:0] popcount(input [8:0] m);
  integer b;
  begin
    popcount = 0;
    for (b = 0; b < 9; b = b + 1)
      popcount = popcount + m[b];
  end
endfunction
```

We are back to having to write our own popcount, regretfully, but it doesn’t
have to be smart. It’s a chain of adders in the end, unrolled by our magical
synthesis tool again.

## Everywhere all at once

This is where Verilog differs from everything else we’ve done!

The following block is combinational. Every clock cycle, it looks at the whole
board and computes, for all 81 cells at the same time, everything the solver
could want to know.

First, the units:

```
always @* begin
  for (u = 0; u < 27; u = u + 1) begin
    seen[9 * u +: 9] = 0;
    clash[u] = 0;
    for (k = 0; k < 9; k = k + 1) begin
      clash[u] = clash[u] | |(seen[9 * u +: 9] & bit_of(at(board, member(u, k))));
      seen[9 * u +: 9] = seen[9 * u +: 9] | bit_of(at(board, member(u, k)));
    end
  end
```

That’s quite a few symbols, but it almost looks familiar I think.

For each of the 27 units we `or` together the digits it contains into `seen`.
While we do that, we check if a digit was already there, and if so, the unit
has a `clash`. No other solver in the series has needed this check so far, and
you’ll see in a minute why we need it now. Hint: it has to do with the parallel
nature of the system.

Then the cells:

```
  dead = |clash;
  full = 1;
  any_forced = 0;
  forced = board;
  best = 0;
  best_n = 10;
  for (i = 0; i < 81; i = i + 1) begin
    cands[9 * i +: 9] = 0;
    if (at(board, i) == 0) begin
      full = 0;
      cands[9 * i +: 9] = ~(seen[9 * (i / 9) +: 9]
                          | seen[9 * (9 + i % 9) +: 9]
                          | seen[9 * (18 + (i / 27) * 3 + (i % 9) / 3) +: 9]);
      if (popcount(cands[9 * i +: 9]) == 0) dead = 1;
      if (popcount(cands[9 * i +: 9]) == 1) begin
        any_forced = 1;
        forced[4 * i +: 4] = digit(cands[9 * i +: 9]);
      end
      if (popcount(cands[9 * i +: 9]) < best_n) begin
        best = i;
        best_n = popcount(cands[9 * i +: 9]);
      end
    end
  end
end
```

The candidates are the usual not-yet-taken numbers, and we compute them for all
cells. We end up with four answers:

- `dead`: some unit has a clash, or some empty cell has no candidates left. The
  board is unsolvable from here.
- `full`: there are no empty cells. We are done.
- `forced`: a copy of the board where every cell with exactly one candidate is
  filled in.
- `best`: the index of the empty cell with the fewest candidates, our MRV cell.
  We also keep `best_n` to check whenever we find a cell if it’s a better
  candidate.

This reads much like the solver loop we’ve written many times, but it works
completely differently, because there is no loop. The circuit that is
synthesized has 81 candidate calculators and a long chain of comparators for
the minimum that are all settling at once.

Why the clash check? Here, because of parallelism, `forced` fills in every
forced cell in the same cycle. If two cells in the same unit are both set to the
same number, we write both of those in the same cycle and nothing checks at the
end. Instead, we check the unit for duplicates at the next cycle, and treat a
clash like any other unsolvable branch.

So we’ve successfully parallelized our work, including our mistakes! Fun!

## A state machine

Since there is no recursion, the search becomes a state machine with three
states:

```
localparam RUN = 2'd0, BACK = 2'd1, HALT = 2'd2;
```

`RUN` propagates and branches, `BACK` backtracks, and `HALT` is the stop
condition.

For backtracking we need a stack. The entry has the board before a guess,
the cell we guessed in, and the candidates we haven’t tried yet:

```
reg [323:0] saved [0:80];
reg [6:0]   where [0:80];
reg [8:0]   rest  [0:80];
reg [6:0]   sp;
reg [1:0]   state;
```

Every guess fills at least one cell, so the search can’t go deeper than 81, and
81 entries are enough to hold our search tree. Math! Logic!

Unfortunately, we have to pay for every entry in hardware, no matter if we end
up using it or not.

I originally called the second array `cell`, but it turns out `cell` is a
reserved word in Verilog. It’s used for configurations, which I had never heard
of before. Shows what I know. I’m just documenting this because `where` is a bit
of an odd name, I think, but I didn’t think to pull out the thesaurus<sup><a
href="#1">1</a></sup>.

Anyway, time to look at the actual states.

### Running

Here is a `RUN` clock cycle:

```
always @(posedge clk) begin
  if (start) begin
    board  <= puzzle;
    sp     <= 0;
    done   <= 0;
    solved <= 0;
    state  <= RUN;
  end else case (state)
    RUN:
      if (dead) state <= BACK;
      else if (any_forced) board <= forced;
      else if (full) begin
        done   <= 1;
        solved <= 1;
        state  <= HALT;
      end else begin
        saved[sp] <= board;
        where[sp] <= best;
        rest[sp]  <= pick & (pick - 1);
        sp        <= sp + 1;
        board[4 * best +: 4] <= digit(pick & -pick);
      end
```

Side note: the first cycle is spent initializing, then we fall through to our
`case` structure.

`RUN` is our whole solver as an `if` chain. If the board is dead, we backtrack.
If there are forced cells, we fill all of them and come back next cycle to
continue our work. If the board is full, we’re done and set `HALT` and all our
stopping variables.

Otherwise we dance. `pick` is the candidate mask of the MRV cell:

```
wire [8:0] pick = cands[9 * best +: 9];
```

We push the current board, the cell, and the remaining candidates, and then
write the lowest candidate into the board. `pick & -pick` isolates the lowest
set bit, and `pick & (pick - 1)` clears it, so together they split the mask
into what to try now vs. what to try later. That’s my official favorite trick
from the last episode making a second appearance as a subtractor and a row of
`and` gates.

This is also the place that tripped me up most when I was learning Verilog for
this: all of these `<=` assignments happen together at the clock edge. We write
`board` into the stack and change one cell of `board` in the same cycle, and
the stack still gets the old board, because everything on the right-hand side
was read before the edge. The timing is very explicit, and my intuitions are
still very much developing.

### Backtracking

The `BACK` state looks at the top of the stack:

```
wire [8:0] left = rest[sp - 1];
wire [8:0] low = left & -left;
```

and then does one of three things:

```
    BACK:
      if (sp == 0) begin
        done  <= 1;
        state <= HALT;
      end else if (left == 0) sp <= sp - 1;
      else begin
        board <= saved[sp - 1];
        board[4 * where[sp - 1] +: 4] <= digit(low);
        rest[sp - 1] <= left & (left - 1);
        state <= RUN;
      end
  endcase
end
```

If the stack is empty, we’ve tried everything, and there is no solution.

If the top entry has no candidates left, we pop it and stay in `BACK`, so the
next cycle looks at the entry below. So we do one backtracking step per cycle,
at worst 81 times. This seems wasteful at first, but it’s actually beautiful in
its simplicity I find.

Otherwise we restore the saved board, try the next candidate, and go back to
`RUN`. The entry stays on the stack with one fewer candidate, ready to come
back if this one’s a dud, too.

This, too, is the same copy-and-restore dance number from assembly, but it’s
copying a board in one cycle. A register that is 324 bits wide is just 324
flip-flops that all load at the same time. If you design the board you can do
whatever, it turns out.

And that’s it! We’ve solved the board!

## How fast is it?

Our usual hard puzzle takes 7,639 cycles. The puzzles from the repository take
between 5 cycles (easy) and 411 cycles (evil), and a puzzle with two 8s in the
first row is rejected in 2 cycles, because the clash check catches it right
away.

That number is hard to compare with the other episodes. If the circuit ran at
1 MHz, 7,639 cycles would be about 7.5 milliseconds. But I haven’t placed and
routed it, so I don’t know how fast it could really be made<sup><a href="#2">2</a></sup>.
The 81 comparator chain we built above would probably be the limit, but again, I
have no intuitions on how to measure this.

The simulation, on the other hand, is slow. Icarus simulates every wire of
that circuit on every cycle, and running the hard puzzle takes about 11 seconds
on my machine. That makes it the slowest solver in the series by a wide
margin, beating even Agda. That’s not a fair comparison, but I think it’s funny
anyway, because we came to the land of speed and wires and ended up with
the slowest simulations of the series.

As for size, I let the genie guide me and ran the solver through
[Yosys](https://github.com/YosysHQ/yosys) targeting Lattice iCE40, a family of
small FPGAs that hobbyists like. It comes out at about 35,000 lookup tables
and 28,000 flip-flops, almost all in the stack. That is a lot more than any
iCE40 chip I’ve found has, so we’d need a bigger FPGA, or a stack in block
RAM, which would cost us a cycle per access.

If you want to build this circuit, be my guest! Do send pictures and/or
measurements, though, please.

## Fin

We built a Sudoku solver out of (simulated) wires and flip-flops, and made the
thing nicely parallel. Through our little state machine, we also added a new
tool to our bag of tricks. Time will tell if we need it again.

I still think this one fits the season better than it might have seemed on
paper. We really did specify Sudoku in hardware, in a way, in a very lop-sided
chip.

In the next episode, we stop writing search code entirely once again: we hand
the constraints to z3 and let it find the answer for us, much like in Prolog and
miniKanren, but differently. See you there!

#### Footnotes

<span id="1">1.</span> We can’t all be Kent Beck.

<span id="2">2.</span> Synthesis turns the description into gates. Placing and
routing then decide where on the chip each gate goes and how the wires run
between them, and only then do you know how long a signal takes to get through
the slowest path, which sets the maximum clock speed through the annoyances of
physics. I didn’t do that step by hand, and I don’t know whether there is a tool
for doing it automatically for me.
