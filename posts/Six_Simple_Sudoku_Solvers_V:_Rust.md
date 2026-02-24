---
title: "Six Simple Sudoku Solvers V: Rust"
date: 2025-10-20
---

You know what time it is: puzzlin’ time! In case you are new here, this is a series about building six different Sudoku solvers in six different programming languages. [Check the first part](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html) for more information on the algorithm as well as a reference implementation.

It is time to get **efficient** with Rust. We’re going to try for minimal allocations, use zero-cost abstractions, and absolute performance without sacrificing legibility (looking at you, [Haskell](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_IV:_Haskell.html)).

## Why Rust?

The value proposition of Rust is pretty well-established compared to other languages in this roster, I’d say. Still, let’s list some of the things we can leverage for Sudoku:

- **Affine types and static compilation**. No GC and an optimized binary make your algorithms go zip!
- **Rich ecosystem of good container types**. Today, we’ll only really use arrays, but it’s good to know we have vectors, trees, ropes, and more specialized data types in our back pocket when we need them, through the standard or high-quality third-party packages (crates).
- **Fine-grained controls**. Between annotations such as `#[inline]`, using fast intrinsic operations, and a myriad of compiler flags, we can make the compiler optimize exactly what we want to.

## The implementation

By now, you should be familiar with the structure: outside in, solver first. But before we do that, we’ll work on the representation.

Like in our previous adventure in [Common Lisp](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_II:_Common_Lisp.html), we’ll be using bitmasks to do our dirty work. Brush up on what we did then over in the Common Lisp article, because I’ll take some of the background information as given here.

```
type Board = [[u8; 9]; 9];
type Mask  = u16;
const ALL: Mask = 0x1FF;

#[derive(Clone, Copy)]
struct State {
    b:   Board,
    row: [Mask; 9],
    col: [Mask; 9],
    bx:  [Mask; 9],
}

#[inline] fn bit(d: u8) -> Mask { 1 << (d - 1) }
#[inline]
fn box_idx(i: usize, j: usize) -> usize { 
	(i / 3) * 3 + (j / 3)
}
```

I’m leaving the types and helpers here for reference. Most of them should be pretty clear; what exactly `box_idx` is doing will be cleared up later.

### The solver

We’ll go into the entry point first, but we’ll have to take another quick detour. In our Rust solution, we’re not working with `Board`s only, we also work with `State`s. And we need a function to get from a board to a state (`init_state`) . Since this is mostly housekeeping, I’m eliding it here. If you want to check it out, [check out the repository version](https://github.com/hellerve/sudoku/blob/main/sudoku.rs).

```
fn solve(mut s: State) -> Option<Board> {
    if !propagate(&mut s) { return None; }
    if solved(&s) { return Some(s.b); }
    let (i, j, mut m) = find_mrv(&s)?;
    while m != 0 {
        let lb = m & (!m + 1);
        let v = 1 + lb.trailing_zeros() as u8;
        let mut t = s;
        assign(&mut t, i, j, v);
        if let Some(sol) = solve(t) { return Some(sol); }
        m ^= lb;
    }
    None
}
```

We’ll again be a bit more explicit about our return values. Like in our Haskell version, we’ll use `Option` (there, we used `Maybe`, but it’s the same concept) to signify the presence or absence of a solution.

As with all our algorithms, we go through a step of propagation. If it fails, the Sudoku is unsolvable and we return `None`. If it is solved, we return the value. Otherwise, we find candidates and go into our depth-first search loop. The loop body looks a little different than what we are used to, though.

We iterate by going over each bit in the mask, stopping until we’ve tried each candidate (i.e. each set bit). We get our candidate by getting the lowest set bit (`lb`) and finding the trailing zeros (remember, this is a bitmask, so a digit is basically the “index” of the bit), making a stack copy (`t`, no heap allocation!), mutating it, and passing it to `solve`. If we don’t get a solution to return, we flip the bit in the mask, and do it again (or exit the loop if the mask is now `0`).

Note that `trailing_zeros` is going to end up unrolling into an intrinsic, meaning it is extremely fast.

So far, so good. Let’s dive deeper. Before we move on, I want you to notice `assign`, and remember that we have a state that keeps track of the bitmasks for rows, columns, and boxes. We will have to take a look at that.

#### Assignment

Assignment is not just updating the cells, it’s also taking care of keeping the state bitmasks updated.

```
#[inline]
fn assign(s: &mut State, i: usize, j: usize, v: u8) {
    s.b[i][j] = v;
    let m = bit(v);
    s.row[i] |= m;
    s.col[j] |= m;
    s.bx[box_idx(i,j)] |= m;
}
```

We do the update, then we update the row, column and box mask by setting our bit. The helpers `bit` and `box_idx` we defined above are becoming helpful here. Notice that `#[inline]` is pretty much filling the same gap as using macros to ensure inlining did when we were working on Common Lisp.

### Propagation

```
fn propagate(s: &mut State) -> bool {
    loop {
        let mut changed = false;
        for i in 0..9 {
            for j in 0..9 {
                if s.b[i][j] == 0 {
                    let m = cand_mask(s, i, j);
                    if m == 0 { return false; }
                    if m.count_ones() == 1 {
                        let v = 1 + m.trailing_zeros() as u8;
                        assign(s, i, j, v);
                        changed = true;
                    }
                }
            }
        }
        if !changed { return true; }
    }
}
```

We’re opting for an unconditional `loop` here, breaking out once the board doesn’t change anymore.

In each pass, we iterate over the board and get our candidate mask for each unfilled cell. If it is `0`, the board is unsolvable. Otherwise, we use another fast intrinsic, `count_ones()`, to check the number of candidates (equal to the number of bits set). If it is `1`, we have a solution, so we set it and move on.

This is all about as fast as you can be. It uses integer or bit operations and does nothing but blaze over a data structure that easily fits into any cache.

#### Computing candidates

Since we have our bitmasks set, candidates aren’t so much computed as combined.

```
fn cand_mask(s: &State, i: usize, j: usize) -> Mask {
    ALL ^ (s.row[i] | s.col[j] | s.bx[box_idx(i, j)])
}
```

All we have to do is a medium-sized logical operation to combine the taken row, column, and box values and flip them (since the candidates are their inverse). Pretty slick, right?

All we still have to do is find MRV candidates.

### Finding search candidates

```
fn find_mrv(s: &State) -> Option<(usize, usize, Mask)> {
    let mut best: Option<(usize, usize, Mask, u32)> = None;
    for i in 0..9 {
        for j in 0..9 {
            if s.b[i][j] == 0 {
                let m = cand_mask(s, i, j);
                let k = m.count_ones();
                if best.map_or(true, |(_,_,_,bk)| k < bk) {
                    best = Some((i, j, m, k));
                    if k == 1 { break; }
                }
            }
        }
    }
    best.map(|(i,j,m,_)| (i,j,m))
}
```

We’re extremely explicit here in searching for our best candidate. We do not keep any intermediate lists, since we want to be efficient. Instead, we keep the recent best in a variable, iterate over the board, and check for each field how many candidates we have there and whether that’s better than what’s in our previous best candidate. It’s simple, it’s imperative, it’s efficient.

And just like that, we’ve built our fastest solver yet with the lowest allocation overhead!

## Fin

We’ve written pretty efficient code today, and it wasn’t even a painful experience. Such is the magic of Rust. It proves that writing “low-level” code doesn’t have to be a gruelling experience, and can actually be quite fun!

We could even parallelize things using something like [Rayon](https://docs.rs/rayon/latest/rayon/), but here, we might instead reach for another tool... The next—and last—programming language we’re going to look at: Elixir! Stay tuned!
