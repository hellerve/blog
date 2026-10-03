---
title: "Simple Sudoku Solvers SIII, EI: Agda"
date: 2026-10-03
---

Welcome back to another round of [Simple Sudoku Solvers](https://blog.veitheller.de/sss/)!
If you are unfamiliar with the series, I suggest you start with [the first
post](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html)
or by perusing [the backlog](https://blog.veitheller.de/sss/). Today we open
season three, which is all about solvers, specifications, and constraints, and
we start with Agda.

Agda is a dependently typed programming language and a proof assistant, which
are the same thing if you squint or [the Curry-Howard
Correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)
if you’re fancy. Types can talk about values, so a type can say not just “this
is a board” but “this is a board that solves that puzzle”. That’s the one weird
trick that type theorists absolutely want you to know, and we’re going to use it
today: our solver will not be allowed to hand us a board without also handing us
a proof that it’s a valid solution. Sudoku proved correct.

The solver is 137 lines, and you can build it with `agda -i . -l
standard-library-2.4 --compile sudoku.agda`, assuming the standard library is
registered on your machine. Agda is a bit unfriendly to people who want to use
it, I find. As always, [the code is on GitHub](https://github.com/hellerve/sudoku).

## Why Agda?

We already did Haskell, and Agda looks a lot like Haskell with more Unicode.
Why bother?

- **Types are specifications.** In [the Haskell
  episode](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_IV:_Haskell.html)
  our solver returned a `Maybe Board`. We get a board or nothing. In Agda we can
  write down what a solution is as a type, and then demand an inhabitant of
  that type. The typechecker enforces the rest. Doesn’t make any sense yet?
  Don’t worry, it will!
- **Every function terminates.** Agda is total(ly awesome). In programming
  languages, totality means that it refuses any function it cannot prove
  terminates, and a backtracking search is its natural enemy. We’ll have to
  convince it that our implementation terminates, and I think the way we do
  that is quite neat.
- **Proofs are programs.** A proof in Agda is just a value of a type. That
  means we can compute proofs instead of writing them by hand, which is how
  we’re going to get away with doing very little actual proving in this post.
  That’s good, because even Lean gives me a headache.

I want to be clear up front that we are not proving the search correct. That
would be a much longer post, and probably best written by someone else. Instead,
we’ll prove that whatever the search gives us is correct, and to be honest
that’s already better than what you get in most other languages.

One thing I want to mention before we embark on this journey: this solution is
probably the slowest so far. The hardest puzzle I use to check with takes about
2.7s to solve, where the Python solution takes 0.07 seconds. I don’t know how
to optimize Agda, but I also didn’t want to embark on that journey.

## A board

The board is the same flat array of 81 numbers we’ve used for most of the
series, with `0` for an empty cell. Cells are indexed by `Fin 81`, the type of
natural numbers below 81, so an out-of-bounds lookup is a type error. Already
we’ve got some nice type guarantees we haven’t really had so far.

```
Board : Set
Board = Vec ℕ 81

Cell : Set
Cell = Fin 81

cells : List Cell
cells = toList (allFin 81)

row col box : Cell → ℕ
row i = toℕ i / 9
col i = toℕ i % 9
box i = (toℕ i / 27) * 3 + (toℕ i % 9) / 3
```

`Vec ℕ 81` is a list of naturals that carries its length in its type, and
`allFin 81` is the vector of every index from 0 to 80. The arithmetic should
be familiar by now (sans Unicode).

From these three functions we build the 27 units, meaning the rows, columns, and
boxes, as lists of cells:

```
unit : (Cell → ℕ) → ℕ → List Cell
unit f k = filterᵇ (λ i → f i ≡ᵇ k) cells

units : List (List Cell)
units = concatMap (λ f → map (unit f) (upTo 9)) (row ∷ col ∷ box ∷ [])
```

Row 3 is every cell whose `row` is 3, and so on. It’s a little wasteful, but
we compute it just once, so I think we can live with the overhead.

## The specification

This is new. Before we write any search, we write down our wishes:

```
Digit : ℕ → Set
Digit v = 1 ≤ v × v ≤ 9

Keeps : ℕ → ℕ → Set
Keeps given v = given ≡ 0 ⊎ given ≡ v

Solution : Board → Board → Set
Solution p s = Pointwise Keeps p s
             × VA.All Digit s
             × LA.All (λ u → Unique (map (lookup s) u)) units
```

These all return `Set`, which in Agda means they are types, not values. Read
`×` as “and” and `⊎` as “or”.

`Solution p s` is the type of evidence that `s` solves the puzzle `p`, and it
says three things. First, `s` keeps every given of `p`. At each position the
puzzle is either empty or agrees with the solution. Second, every cell of `s` is
a digit from 1 to 9. Third, in every unit, the digits are unique. Them’s the
rules!

This says nothing about how to find a solution yet. There is no MRV, no
candidates, no backtracking. But the concept is there, we just need to figure
out how to do the work now.

## Deciding the specification

A type on its own doesn’t do anything. We need a way to get a value of
`Solution p s` for a given board, and here the standard library does all the
heavy lifting:

```
solution? : (p s : Board) → Dec (Solution p s)
solution? p s = VP.decidable (λ g v → (g ≟ 0) ⊎? (g ≟ v)) p s
         ×? VA.all? (λ v → (1 ≤? v) ×? (v ≤? 9)) s
         ×? LA.all? (λ u → unique? (map (lookup s) u)) units
```

That’s a lot of glyphs, admittedly, but we’ve done Dyalog APL before, so this
shouldn’t be too scary anymore.

`Dec A` is either `yes` with a proof of `A`, or `no` with a proof that `A` is
impossible. It’s a boolean of sorts, but one that proves why it’s true or false.
The standard library already knows how to decide equality of naturals (`≟`),
ordering (`≤?`), whether every element of a list or vector satisfies a
decidable property (`all?`), and whether a list has duplicates (`unique?`).
It also knows how to combine decisions with `×?` and `⊎?`. So our decision
procedure is the specification again, just slightly more machinistic.

For the purposes of this post, that’s the entire proof effort. We never write
a proof term by hand. The checker runs, and if it says `yes`, the proof is
attached and ready to go.

### The search

Now for the part that we already know how to do, the search. We get candidates
by subtracting the digits of a cell’s peers from 1 through 9, with the peer
lists computed once up front:

```
peer : Cell → Cell → Bool
peer i j = (row i ≡ᵇ row j) ∨ (col i ≡ᵇ col j) ∨ (box i ≡ᵇ box j)

peers : Vec (List Cell) 81
peers = V.map (λ i → filterᵇ (peer i) cells) (allFin 81)

candidates : Board → Cell → List ℕ
candidates b i = filterᵇ (λ d → not (any (_≡ᵇ d) used)) (map suc (upTo 9))
  where used = map (lookup b) (lookup peers i)
```

This almost looks like Haskell if you squint, and its approach should be more
familiar again.

`mrv` walks the empty cells and keeps the one with the fewest candidates. It’s a
fold with more ingredients than I’m comfortable with, but it’s not terribly
complex either:

```
mrv : Board → List Cell → Maybe (Cell × List ℕ)
mrv b []       = nothing
mrv b (i ∷ is) = pick (candidates b i) (mrv b is)
  where
    pick : List ℕ → Maybe (Cell × List ℕ) → Maybe (Cell × List ℕ)
    pick cs nothing        = just (i , cs)
    pick cs (just (j , ds)) with length cs <ᵇ suc (length ds)
    ... | true  = just (i , cs)
    ... | false = just (j , ds)
```

Pattern matching on booleans feels a bit odd, admittedly, but we do what we
got to do.

Then the search itself. Usually I just show you the solution, but in this case
I hit a fun snag I want to show you!

## Agda refuses

My first version of `search` was a straight transliteration of the recursive
solvers from earlier episodes:

```
search : Board → Maybe Board
search b with mrv b (empties b)
... | nothing       = just b
... | just (i , ds) = branch b i ds

branch : Board → Cell → List ℕ → Maybe Board
branch b i []       = nothing
branch b i (d ∷ ds) with search (b [ i ]≔ d)
... | just s  = just s
... | nothing = branch b i ds
```

`with` is Agda’s way of pattern matching on an intermediate result, and the
`...` lines are its cases. `b [ i ]≔ d` is the board with cell `i` set to `d`.

But Agda said no:

```
error: [TerminationIssue]
Termination checking failed for the following functions:
  branch
Problematic calls:
  search (V.updateAt b i (Function.const d))
  ...
```

Fair enough. `branch` calls `search` on a board that is the same size as the
one it started with, and Agda cannot know whether this ever stops. We know it
stops, because every call fills one empty cell and there are only so many of
them. Can we encode that knowledge somehow?

Turns out we can! We tell Agda exactly that by passing the number of holes
along and counting it down:

```
search : ℕ → Board → Maybe Board
search zero    b = just b
search (suc n) b with mrv b (empties b)
... | nothing       = just b
... | just (i , ds) = branch n b i ds

branch : ℕ → Board → Cell → List ℕ → Maybe Board
branch n b i []       = nothing
branch n b i (d ∷ ds) with search n (b [ i ]≔ d)
... | just s  = just s
... | nothing = branch n b i ds
```

Now `search` takes `suc n` and hands `n` to `branch`, which hands the same `n`
back to `search` or recurses on a shorter list. Everything gets structurally
smaller, and the termination checker is happy. This is usually called fuel, and
it’s often a hack, where you guess a big enough number and hope. It was new to
me, and very funny and horrifying when I learned about it.

Here it isn’t a guess, though, because the hole count is linked to the depth of
the search tree, so I think we get a pass.

There is no propagation phase, for the same reason as in [the miniKanren
episode](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EV:_Racket_miniKanren.html)
and [the SQL one](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EVI:_SQL.html).
A forced cell is just an MRV cell with one candidate. And a dead end is an MRV
cell without any candidates: `mrv` picks it first, `branch` gets an empty list, and
we return `nothing`. Keeping propagation as its own loop would have meant
convincing the termination checker once again, and I’m too lazy to do that.

## Putting it together

Now we can write the function every post needs, `solve`.

```
solve : (p : Board) → Maybe (Σ Board (Solution p))
solve p with search (holes p) p
... | nothing = nothing
... | just s with solution? p s
...   | yes ok = just (s , ok)
...   | no  _  = nothing
```

Look at the type. `Σ Board (Solution p)` is a pair of a board and a proof that
this board solves `p`. The `p` in the type is the actual argument, which is what
makes this dependent. `solve` can’t return a board that merely looks solved. The
only way to build that pair is to have `solution?` say `yes`, which, if you ask
me, is just beautiful.

One caveat: the search is untrusted. It could be completely wrong, and `solve`
would still cover its invariants. I played with this by breaking it on purpose.
I dropped boxes from `peer`, and handed it an empty board. The search quickly
returned a board which had three 1s in the first box, and `solve` returned
`nothing`.

That’s a real weakness of our solution. `nothing` means both “no solution” and
“the search messed up”. Proving that the search finds a solution whenever one
exists is left as an exercise to the reader<sup><a href="#1">1</a></sup>.

## Fin

We solved Sudoku again, and for the first time in the series, we also proved
some things about our solution. The specification is three lines, the checker
is three lines, and the search is still basically unchanged.

I think this is a good way to open the season, because the other five episodes
will lean on the same split between what a solution is and how to find one, and
they’ll be much weirder. But first, in the next one, we go in the opposite
direction entirely: assembly, where there are no types and nothing is checked
at all, except maybe our sanity. See you there!

#### Footnotes

<span id="1">1.</span> I learned that this has a name. A *certifying* algorithm
returns its answer together with evidence that a simple checker can verify, so
you only have to trust the checker. Here the checker is `solution?`, and since
Agda checked it against `Solution`, you only really have to trust the three
lines of specification. If the check is a bit more complex, you might want more
assurance than that.
