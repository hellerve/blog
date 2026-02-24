---
title: "Six Simple Sudoku Solvers III: Prolog"
date: 2025-09-29
---

Welcome back to another Sudoku puzzling sesh! In case you are new here, this is a series about building six different Sudoku solvers in six different programming languages. [Check the first part](./Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html) for more information on the algorithm as well as a reference implementation.

In this installment, we will look at implementing a solver in Prolog using the SWI flavor of Prolog. We will learn about backtracking and constraint definition in Prolog, as well as implement the shortest solver in terms of line count. But don’t worry, we’ll make up for it by talking about concepts and hopefully learning a lot!

## Why Prolog?

Simply put, Prolog was made for these types of problems. Backtracking, searching, and solving are all handled by the language implementation. All we need to do is translate boards into constraints.

If we want, we can do a lot of optimizations and turn a lot of knobs to get dramatic changes, but we do not need to to solve the main problems. As a result, the core algorithm is only about 25 lines of code.

For completeness I want to mention that we will also be using a library with the unassuming name of [Constraint Logic Programming over Finite Domains (clpfd)](https://www.swi-prolog.org/pldoc/man?section=clpfd). This gives us the ability to reason over integers as well as other abilities. It is used in most Prolog programs that I’ve touched, but is not strictly speaking required.

Prolog is not the right tool for all programming problems. But when it is, it’s delightful.

## The implementation

As always, we will be following the outside-in path and go from the solver to its constitutent parts. Today, those parts will look a little different than they usually do, however, since we do not need to do any candidate selection ourselves.

### The solver

Firstly, a brand new solving algorithm.

```
length_(L, Xs) :- length(Xs, L).

solve(Rows) :-
    length(Rows, 9),
    maplist(length_(9), Rows),
    append(Rows, Vars), Vars ins 1..9,

    maplist(all_different, Rows),
    transpose(Rows, Cols),
    maplist(all_different, Cols),
    blocks(Rows),

    labeling([ff], Vars).
```

I assume that most of my readers are not fluent in Prolog—and neither am I—, but honestly, there is not that much going on in there. We define a function, assert a length for the board and a length for its constitutent rows. Then we assert that all cells are in the domain `1..9`, and that they are `all_different` across rows, columns, and blocks (`blocks` is a custom function we will get into later). 

Finally, we let the constraint solver take over using `labeling`. We can choose how it searches for candidates using the first parameter. Our choice, `ff`, stands for `fail-first`, but we could tune it to our needs if necessary. We could, for instance, use `labeling([ffc, bisect, down], Vars)` to get a solver that fails first with tiebreak (`ffc`), splits the domain in half (`bisect`), and goes downwards into the domain (i.e. from `9` to `1`).

But wait. We skipped a step, didn’t we? How do we know what variables we need to solve for? Simple, before passing the array into `solve`, we call this beauty on it:

```
z2v(0, _).
z2v(N, N) :- integer(N), N > 0.

rows_from_zeros(PZero, Rows) :-
	maplist(maplist(z2v), PZero, Rows).
```

I trust this needs no further explanation, but just in case it does: when we call `rows_from_zeros` with our board and a fresh variable, the zeros on the board will be replaced with new fresh variables that our solver can then use in `labeling`.

In case you haven’t seen Prolog and haven’t quite gotten the way variables work here—I don’t blame you—, here’s a quick primer: variables in Prolog are variables in the truest sense, and they can be passed around in any state (with a known or unknown value). You can conceptualize them as simply a target for constraints, and having a value is basically a very narrow constraint. Of course, this is not quite accurate, but it gives you a better intuition about Prolog than trying to work with concepts from most industry standard programming languages will.

### Defining the block constraint

The last piece of the puzzle is the constraint for blocks, which is the only one we can’t define out of the box.

```
blocks([]).
blocks([A,B,C|Rs]) :- blocks3(A,B,C), blocks(Rs).

blocks3([], [], []).
blocks3([A,B,C|R1], [D,E,F|R2], [G,H,I|R3]) :-
    all_different([A,B,C,D,E,F,G,H,I]),
    blocks3(R1, R2, R3).
```

This code is partitioning the lines into threes and calling `blocks3` on each (the recursive base case is the first line, which pattern matches on the empty list and does nothing). Then we pull three cells out of each of those three rows and assert that all nine values are different.

In the end, this is a recursive definition of a sliding window across the board with a single assertion. Not necessarily the cleanest, but it gets the job done.

## Fin

And just like that, we implemented a Sudoku solver in Prolog! We didn’t really implement any logic, but that’s by design. By leveraging the ability of Prolog to declare constraints and let it do the dirty work we condensed our contribution into how to express the rules of Sudoku and labeling the board.

Prolog is a language that, in my opinion, is sorely underutilized, including by yours truly. I hope that through this blog post, I could at least inspire you to take a look at the language, because if nothing else, it will expand your view of what programming can look like.

See you next time for Haskell, in which we’ll go full-on functional programming and data types!
