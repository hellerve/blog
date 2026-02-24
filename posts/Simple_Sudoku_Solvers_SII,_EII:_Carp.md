---
title: "Simple Sudoku Solvers SII, EII: Carp"
date: 2026-02-25
---

Sudoku’s back, baby, and it brought a personal friend. Today we’re going to talk
about [Carp](https://github.com/carp-lang/carp), a language I used to co-maintain
and have recently begun diving back into. As always, if you don’t know what this
is, I suggest [starting out at the beginning](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html)
or by perusing [the backlog](https://blog.veitheller.de/sss/).

I know I’m biased, but it’s just a pleasure to work in and with Carp, and I intend
to do so much more again this year. [I’ve written extensively about Carp and its
patterns](https://blog.veitheller.de/carp-patterns/), and as per usual, I won’t
have enough time to cover a lot of the language, but I invite you to try it if
what you see tickles your fancy!

Also as always, [the code is on GitHub](https://github.com/hellerve/sudoku) for
your reading pleasure.

## Why Carp?

I have to be honest here and say that Carp made the list mostly because I have a
soft spot for it, but it also happens to lend itself quite well to solving Sudoku
puzzles:

- **Lisp and its affordances.** It has macros, it has expressions everywhere, and
its patterns are fairly expressive! We’re not trying to code golf here, but
having a rich language definitely helps in making a Sudoku solver more pleasant
to read and write.
- **“Low level” friendliness.** For a Lisp, Carp is pretty performant and
low-level. I wouldn’t compare it to C or Rust in this regard, but it does ride
the line between “expressive” and “performant” pretty well in my opinion.
- **Strong type system.** Having both `Maybe` (you might know it as `Option`)
and `Result` (you might know it as `Either`) as well as a rich ecosystem of
expression will help us below.
- **Natural FFI.** If we want to, we can always escape to C. In this tutorial I
won’t be doing that, but I did experiment with shelling out to C for `popcount`
and the result was pretty nice still!

## The implementation

Alright, long time readers will know the patterns right now, so I’ll keep this
pretty brief: our board is an array of 81 integers where `0` means the cell is
not filled out. Candidates is a 9-bit mask.

Here’s a taste of how we work with it:

```
(def all-mask 0b111111111)

(defn digit-mask [d] (if (= d 0) 0 (bit-shift-left 1 (- d 1))))
(defn mask-has? [m d] (/= (bit-and m (digit-mask d)) 0))

(defn mask-popcount [m]
  (let-do [c 0]
    (for (d 1 10) (when (mask-has? m d) (set! c (+ c 1))))
    c))
```

It would be nice if `popcount` were a primitive, but alas, we make do.

Indexing also becomes quite easy:

```
(defn grid-get [g i] @(Array.unsafe-nth g i))

(defn idx-row [i] (/ i 9))
(defn idx-col [i] (Int.mod i 9))

(defn row-indices [i]
  (let [s (* 9 (idx-row i))]
    (Result.unwrap-or-zero (Array.range s (+ s 8) 1))))

(defn col-indices [i]
  (let [c (idx-col i)]
    (Result.unwrap-or-zero (Array.range c (+ c 72) 9))))
```

Alright, let’s get to solving this thing!

### The main solver

The main solver is quite straightforward, but not exactly compact:

```
(defn solve [grid]
  (Maybe.and-then (propagate-singles grid)
    &(fn [g]
       (if (solved? &g)
         (Maybe.Just g)
         (Maybe.and-then (choose-branch-idx &g)
           &(fn [idx]
              (let [mask (candidates-mask &g idx)]
                (let-do [res (Maybe.Nothing)]
                  (for (d 1 10)
                    (when (Maybe.nothing? &res)
                      (when (mask-has? mask d)
                        (let-do [g2 @&g]
                          (Array.aset! &g2 idx d)
                          (set! res (solve g2))))))
                  res))))))))
```

So, what is happening here? We’re doing a lot of chaining of `Maybe`s, basically,
but we should probably explain what `Maybe.and-then` does: it takes a `Maybe` and
if there is a value inside, it applies a function to that value. The function is
expected to also return a `Maybe` itself.

What this means is that control flow is a bit more implicit: whenever we call
`and-then`, we are basically saying “if there’s nothing here, just return”.

With that out of the way, the function becomes: `propagate-singles` on the grid,
meaning fill everything out. If we have `solved?` the puzzle, we return it,
otherwise we `choose-branch-idx` (not being able to choose a branch means we
don’t have a solution) and we go into another round of solving! The entire flow
from `candidates-mask` onwards basically is just the next attempts at solving.

And that’s all we do here! From here, let’s look at how we fill out the puzzle.

### Propagation

Propagation, too, is rather long in this version, but pretty straightforward:

```
(defn propagate-singles [grid]
  (if (valid-grid? &grid)
    (let-do [ok true
             changed true
             g grid]
      (while-do (and ok changed)
        (set! changed false)
        (for (i 0 81)
          (when (and ok (= (grid-get &g i) 0))
            (let [m (candidates-mask &g i)
                  n (mask-popcount m)]
              (cond
                (= n 0) (set! ok false)
                (= n 1)
                  (do
                    (Array.aset! &g i (mask-single->digit m))
                    (set! changed true))
                ())))))
      (if ok (Maybe.Just g) (Maybe.Nothing)))
    (Maybe.Nothing)))
```

Here we’re running a loop that scans empty cells, finds any with exactly one
candidate, fills those in, and repeats until we reach a fixpoint. The two flags
`ok` and `changed` are our book-keeping. `ok` is for those cases where we’ve
reached an unsolvable board, and `changed` keeps track of whether we’ve reached
a fix point or are still changing things.

`mask-single->digit`, referenced above, extracts the digit from a one-bit mask:

```
(defn mask-single->digit [m]
  (let-do [r 0]
    (for (d 1 10) (when (and (= r 0) (mask-has? m d)) (set! r d)))
    r))
```

On to candidate search!

### Candidate search

Candidate computation is a few helpers stacked on top of each other. We’ve
already seen row and column indexing; boxes are the missing piece:

```
(defn box-start [i]
  (let [r (idx-row i)
        c (idx-col i)]
    (+ (* 27 (/ r 3)) (* 3 (/ c 3)))))

(defn box-indices [i]
  (let [s (box-start i)]
    [ s (+ s 1) (+ s 2)
      (+ s 9) (+ s 10) (+ s 11)
      (+ s 18) (+ s 19) (+ s 20) ]))
```

The magic numbers are unrolled positions in the box. It looks a bit dumb, but it
works. We mostly need those functions because with them, collecting used digits
becomes a fold:

```
(defn used-from-indices-except [g i idxs]
  (Array.reduce
    &(fn [acc k]
       (let [kk @k]
         (if (= kk i)
           acc
           (let [v (grid-get g kk)]
             (if (= v 0)
               acc
               (bit-or acc (digit-mask v)))))))
    0
    &idxs))

(defn cell-used-except [g i]
  (bit-or
    (bit-or (used-from-indices-except g i (row-indices i))
            (used-from-indices-except g i (col-indices i)))
    (used-from-indices-except g i (box-indices i))))

(defn candidates-mask [g i]
  (let [used (cell-used-except g i)]
    (bit-and all-mask (bit-not used))))
```

`used-from-indices-except` or’s each placed digit into an accumulator, skipping
the cell itself and any empty cells. `cell-used-except` takes the union of row,
column, and box. `candidates-mask` flips the bits and masks to 9 bits. And that's
pretty much it!

### Branch selection

`choose-branch-idx` is our MRV heuristic:

```
(defn choose-branch-idx [g]
  (let-do [best-count 10
           best-idx -1]
    (for (i 0 81)
      (when (= (grid-get g i) 0)
        (let [c (mask-popcount (candidates-mask g i))]
          (when (and (> c 0) (< c best-count))
            (do
              (set! best-count c)
              (set! best-idx i))))))
    (if (= best-idx -1) (Maybe.Nothing) (Maybe.Just best-idx))))
```

We start `best-count` at 10 (more than any cell could have), scan empty cells, and
update on improvement. The result comes wrapped in `Maybe` so it fits right into
`solve`'s `and-then` chain.

By the way, at the time of writing `Maybe.and-then` isn't in the standard library
([an PR exists that emerged after I wrote my
solution](https://github.com/carp-lang/Carp/pull/1479)), but Carp lets you extend
any module, so we just add it:

```
(defmodule Maybe
  (defn and-then [m f]
    (match m
      (Maybe.Just v) (@f v)
      (Maybe.Nothing) (Maybe.Nothing))))
```

And that is really all there is to it! We’ve solved Sudoku yet again.

## Fin

It was a delight to come back to Carp for this, like visiting an old friend
that doesn’t judge and lets me move freely between imperative and functional
programming at the speed of thought.

There are many different idiomatic ways to solve Sudoku in Carp. We could have
gone fully functional with folds and the like, or fully imperative with loops
everywhere. Carp allows me to do either or both and mix the two as I see fit,
helping me build a pragmatic but aesthetic solution that’s always pretty
performant. And I rarely have to worry about details I don’t care about while
doing so.

Thank you for sticking around for episode two of season two! See you next time!
