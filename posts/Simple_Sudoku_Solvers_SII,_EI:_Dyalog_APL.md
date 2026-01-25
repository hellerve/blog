Welcome to the first post of the second season of [Simple Sudoku Solvers](https://blog.veitheller.de/sss/)!
If you are unfamiliar with the series, I suggest you start with [the first
post](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html),
otherwise buckle up, because we are going to look at a lot of strange symbols today!

Today we’re going to look at APL, a strange old language fallen out of time known for
its terseness and strange alien symbols. Truth be told, I could never get into it, and
still can’t. Nonetheless, writing a Sudoku solver seemed like the perfect weird challenge
to get my hands dirty on and give it an honest shot. And now I’m going to share the
bloody results with you!

Fair warning: in my efforts to make this code as “APL-like” as possible, I’ve done a few
iterations of golfing on it. Now the program truly looks like APL, but might also be
somewhat inscrutable to those not in the know. To be clear, if I hadn’t written this
code, I probably would have trouble following it myself.

Another thing to note is that in this installment, contrary to prior episodes, I will
try to explain more or less every expression, in an attempt to be a bit friendlier to
my reader.

With all of the encouraging stuff out of the way, let’s dive right in!

## Why APL?


APL is a language that looks like someone spilled a box of alien maths onto your
screen and then insisted this was a good idea. And maybe they were right, I’m not smart
enough to answer this question.

A few things make APL a fun fit for Sudoku even for mere mortals:

- **Everything is an array.** Sudoku is a 9×9 array problem. Matrices and arrays are APL’s food.
  It sounds like a match made in heaven.
- **Decoding indices and slicing submatrices is built-in.** We essentially just lean on basic
  operators to slice and dice our rows, columns, and boxes.
- **Set-ish operations are effortless.** “Candidates are digits minus used digits” is basically
  one operator. So is everything else if you’re creative enough.
- **Conciseness is not optional.** APL will not let you hide from your data model: if you choose
  a representation that’s awkward, it will show immediately. It is also unforgiving toward clarity
  of thought, which is both scary and cool.

This post is not an APL primer. Hopefully I’ll explain enough to follow along, but if you’ve
never used APL before, the best strategy is: keep reading, copy/paste code into an interpreter,
and allow yourself to be confused for a bit.

The dialect of APL I’m using is [Dyalog APL](https://www.dyalog.com/), which seems to be the
industry standard as far as I can tell. It’s a commercial offering, but you can download the
engine and IDE for free if you’re using it mostly for play like me. It also comes with a program
called `dyalogscript`, which is a way of running Dyalog programs from the terminal instead of
their somewhat foreign IDE. Unfortunately it’s pretty underspecified and typing APL is awkward
to begin with, so I spent way too much time trying to get this part to work. I recommend just
running it in their environment instead.

## The implementation

As always, we initially represent the board as a matrix (9x9), where `0` means “empty” and
`1..9` are known digits. The solver is the basic shape of filling, MRV, and
backtracking/depth-first search. This time, however, we’ll lean harder on array operations
for candidate computation, and keep everything else simple and obvious (well, if you know how
to read it).

In this installment, we’ll use the 9-bit boolean mask trick we’ve used in previous installments
to add extra flavor and because its uniform length plays well with APL’s standard operations. It
forces us to compute masks and convert stuff, but since our actual implementation is only ten
ultra terse lines, we can afford it.

Importantly, internally we’ll deal with a single array of 81 values to facilitate quicker
operations. So we’ll start by getting a matrix, and then flatten it, before printing it as
a 9x9 matrix instead. The high-level looks like this (`⍝` is the comment delimiter):

```
⍝ input matrix m (← is the assignment operator)
m←↑(
  (0 0 3 0 2 0 6 0 0)
  (9 0 0 3 0 5 0 0 1)
  (0 0 1 8 0 6 4 0 0)
  (0 0 8 1 0 2 9 0 0)
  (7 0 0 0 0 0 0 0 8)
  (0 0 6 7 0 8 2 0 0)
  (0 0 2 6 0 9 5 0 0)
  (8 0 0 2 0 3 0 0 9)
  (0 0 5 0 1 0 3 0 0)
)
⍝ flattened p
p←,m
⍝ S is the solver, 9 9⍴ prints it, assigning to ⎕ prints
⎕←9 9⍴S p
```

We also set indexing to be 0-based (it’s 1-based by default), and compute the space of
possible digits `D`:

```
⍝ the ⋄ sigil separates multiple expressions on one line
⎕IO←0 ⋄ D←1+⍳9
```

With that out of the way, let’s look at the algorithm!

### The main solver

Staying true to the terseness of APL, we named the main solver function `S`. It’s a unary
function, and in APL we receive that argument as `⍵`. With that out of the way, the rest
of the algorithm should be self-evident:

```
S←{
  b←P ⍵ ⋄ 0=≢b:⍬ ⋄ ~0∊b:b
  i←⍸0=b ⋄ cs←↑(b∘C)¨i ⋄ l←+/cs ⋄ k←l⍳⌊/l ⋄ idx←i[k]
  sols←(⊂∘∇)¨({(⍵@idx)⊢b}¨cs[k;]/D)
  ⊃(((0<(≢∘⊃)¨sols)/sols),⊂⍬)
}
```

Sarcasm notwithstanding, the code is actually not as bad as it looks. We start by propagating
using `P`—more on that below—, and assigning that to `b`. We return `⍬` if we get it from `P` here,
since this is our unsatisfiability marker (`0=≢b:⍬`). If there are no zeros on the board, on the
other hand, we are done and return the board `b` (`~0∊b:b`).

With the preconditions out of the way, we can move on to the main branching algorithm. We get
the indices `i` of the cells set to zero (`i←⍸0=b`) and compute the candidates (`cs←↑(b∘C)¨i`)
and their respective counts (`l←+/cs`). We then pick the cell with the fewest candidates
(`k←l⍳⌊/l`) and find its index (`idx←i[k]`).

After all of this, we are ready to solve the branches. We turn the 9-bit mask into the candidate
digits (`cs[k;]/D`), and produce a new board for all of them (`{(⍵@idx)⊢b}¨`, `idx` being the
index, and `@` the functional/copying update). We pipe that into the solver, box each result, and
assign the vector of boxes to `sols` (`sols←(⊂∘∇)¨`). All of that is one expression.

Finally, we just need to return the first match. We unbox each solution and get its length
(`(≢∘⊃)¨sols`), filter for the non-empty ones (`0<`), and take the first one, returning `⍬`
if we can’t find anything (`⊂⍬`).

Quick sidenote: you might ask yourself why we box `sols` just to unbox it again. This is due
to a feature of Dyalog APL, which will happily mix the results of `∇¨` into a regular array if
it can, which breaks our filter/take first logic. Dyalog’s shape coercion rule details are a bit
much to get into here, but you can just think of it as “if the shapes fit, smush them”, and
that’s not what we want here.

So, what we still need to define for this to click is the candidate search `C` and propagation
`P`.

### Propagation

We will now tackle propagation, which thankfully is a bit less involved (the next part will be
simpler still!):

```
P←{
  b←⍵ ⋄ i←⍸0=b ⋄ 0=≢i:b
  cs←↑(b∘C)¨i ⋄ l←+/cs ⋄ 0∊l:⍬
  f←l=1 ⋄ ~∨/f:b
  b[f/i]←(f⌿cs)+.×D ⋄ ∇b
}
```

To remind ourselves: here we repeatedly fill any cell that has exactly one candidate until we
cannot any longer. We return `⍬` on contradiction.

First, we assign our argument to `b`, because I’m a wuss who wants descriptive names (`b←⍵`).
Then, we get the indices of empty cells again (`i←⍸0=b`, identical to above), and compute the
candidates (`cs←↑(b∘C)¨i`, again the same) and their counts (`l←+/cs`). Here we need to guard
against impossibility: if any empty cell has 0 candidates, the current board is impossible and
we return `⍬` (`0∊l:⍬`). Then we get the boolean masks of all cells with exactly one
candidate (`f←l=1`) and again guard: if there are no cells we can fill, we return the board
(`~∨/f:b`).

Finally, we can fill the board (`b[f/i]←(f⌿cs)+.×D`)! This boils down to just getting the indices
of what we need to fill (`f/i`), selecting the corresponding masks (`f⌿cs`), and converting it
to a digit by computing the dot product between rows and D (`(+.×D)`). The rest is an array
assignment.

The last expression is tail recursion: we will try to go for another round of propagation (`∇b`).

### Candidate search

Compared to the previous snippets, candidate search is almost easy to grasp:

```
C←{
  m←9 9⍴⍺ ⋄ r c←9 9⊤⍵ ⋄ r0 c0←3×⌊(r c)÷3
  ~(D∊0~⍨∪m[r;],m[;c],,m[r0+⍳3;c0+⍳3])
}
```

Here we get a board and index and compute a mask. In “dyadic” APL functions (one argument
goes left and one goes right in the call), the arguments are `⍺` and `⍵`.

So first we reshape the board back into a 9x9 matrix (`m←9 9⍴⍺`), because rows, columns, and
boxes fall out of that more easily. We then decode the index into a row/column index pair
(`r c←9 9⊤⍵`) that we can use for the matrix and get the start index of the box
(`r0 c0←3×⌊(r c)÷3`, either `0`, `3`, or `6`). We then collect the row, column, and box values
(`m[r;], m[;c], ,m[r0+⍳3; c0+⍳3]`), make sure they’re unique (`∪`), and remove 0 (`0~⍨`).
We then turn it into a candidate mask (`~(D∊<expr>)`) and return it!

In my opinion, this is where APL truly shines. Being able to easily express column, row, and
box indices is a superpower you only get from a language that takes arrays and matrices as
fundamental.

And this concludes our tour de force! Congratulations, you made it!

## Fin

I don’t really know much about APL, but I think we did a pretty good job of staying faithful
to how you can work with it. I probably missed a whole bunch of things that are obvious to
more experienced sigil slingers, but the easy immutable updating using `@`, incredibly simple
candidate selection, and very declarative search algorithm (despite the scary syntax, it felt
like just describing “solve map over candidates, then pick first non-empty“) really did
resonate with me.

Thank you for tuning in for the first part of the second season of our little Sudoku solver
series. The next few episodes should be somewhat less esoteric while still scratching that
“mind-bending” itch. See you around!
