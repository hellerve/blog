---
title: "Simple Sudoku Solvers SII, EVI: SQL"
date: 2026-08-11
---

Welcome back to another round of [Simple Sudoku Solvers](https://blog.veitheller.de/sss/)!
If you are unfamiliar with the series, I suggest you start with [the first
post](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html)
or by perusing [the backlog](https://blog.veitheller.de/sss/). Today we close out
season two with SQL, and our solver will be a single query.

I said at the end of [the last episode](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EV:_Racket_miniKanren.html)
that I was scared of this one. This was partly because of me not knowing how to
do it “properly” ahead of time, but mostly because SQL gives you nowhere to put
things and its shape is just so weird and different. There are no variables to
stash a board in, no function to call recursively, not even a loop.

We’re using SQLite (3.43.2, whatever ships with my machine), and the solver is
~80 lines of a single `WITH RECURSIVE`. You can run it with `sqlite3 < sudoku.sql`,
with no database, because we never create a table. As always, [the code is on
GitHub](https://github.com/hellerve/sudoku).

## Why SQL?

Let’s be honest here. Mostly because it’s fun. I don’t think anyone should write
an algorithm like this in SQL, but it’s the perfect way to learn how to actually
write a query in anger.

Let me try to steelman the choice anyway:

- **It’s the most used declarative language.** Back in [the Prolog
  episode](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_III:_Prolog.html)
  we handed the board to a constraint engine and it did the searching for us. SQL
  is declarative too, and vastly more widely deployed, but it has no solver in it.
  But I would argue that more people will be doing something weird with `WITH
  RECURSIVE` than are going to touch Prolog, which is sad, but gives us an
  excuse to build the search out of joins and call it an educational journey.
- **A recursive CTE is a search if you squint hard enough.** It’s the place where
  SQL and traditional programming meet and it admits to a loop. It’s also very
  much the shape we want, with a queue of states, a rule to choose successor
  states, and a stopping condition. You can use it for walking an org chart in
  your analytics engine, or to emulate backtracking search in your Sudoku
  solver.
- **Constraints!** As we learned in our declarative episodes, Sudoku is
  primarily a constraint problem, and joins are constraints, just in SQL. `NOT
  EXISTS` is our new best friend.

If all of this was a bit unclear as yet, don’t worry! We’re rolling up our
sleeves right away to put our propaganda to practice.

## Representing boards

Every other solver in this series got to keep a board somewhere and mutate or
rebind it. Here, the state is a row in the recursive CTE, so whatever the search
needs to know has to fit in that row’s columns. That forces our hand a bit, but
we can make it work.

The board is one 81-character string, with `0` for an empty cell:

```
puzzle(s) AS (
  VALUES ('306508400'
       || '520000000'
       || '087000031'
       || '003010080'
       || '900863005'
       || '050090600'
       || '130000250'
       || '000000074'
       || '005206300')
),
```

It looks alright if a little compressed.

The concatenation is only there so the puzzle is readable as a grid, we could
just as well make it one big digit string.

On an algorithmic level, a cell at index `p` is `substr(s, p + 1, 1)`, and filling
cell `p` with digit `z` is `substr(s, 1, p) || z || substr(s, p + 2)`. That’s
not entirely pretty, but we can look past it.

The obvious alternative is a table of 81 cells, one row per cell, which is
probably the more principled design at first glance (and I started with it!). It
gets awkward quite quickly, though, because a branch has to duplicate the whole
board, so every row needs a board id, and suddenly I’m managing bindings by hand
and having to think about building a garbage collector for dead branches.

One string per board avoids all of that. It’s not elegant, exactly, but it is
small and not in the way.

## The implementation

### Geometry

First we need to count.

```
seq(n) AS (VALUES (0) UNION ALL SELECT n + 1 FROM seq WHERE n < 80),

pos(p, r, c, b) AS MATERIALIZED (
  SELECT n, n / 9, n % 9, n / 27 * 3 + n % 9 / 3 FROM seq
),

digits(z) AS MATERIALIZED (
  SELECT CAST(n + 1 AS TEXT) FROM seq WHERE n < 9
),
```

Later on I learned that this is a pretty standard thing for an SQL codebase.
Esotericism in programming is everywhere when you look hard enough.

`pos` gives every cell index its row, column, and box, using the same modulo
arithmetic we’ve used since the Python reference. Two cells are peers when they
share any of `r`, `c`, or `b`. That’s basically our only Sudoku rule!

`digits` holds `'1'` through `'9'` as *text*, because the board is a string and
we’ll be comparing these against `substr` results. Already we can see the
consequences of our actions and have to work with a stringly typed system.

The `MATERIALIZED` hints tell SQLite to compute these once instead of re-deriving
them at every use. They’re not required, but I do like to benchmark my
solutions, especially the weirder ones, and on the hardest of our four test
puzzles they take the runtime from 1.73s down to 1.10s. Good enough for
me<sup><a href="#1">1</a></sup>.

### The search

Here’s the eye of the storm, with the cell-picking subquery left out for now:

```
search(s, p, holes) AS (
  SELECT s,
         (SELECT ... /* MRV pick */ ),
         length(s) - length(replace(s, '0', ''))
  FROM puzzle
  UNION ALL
  SELECT substr(s, 1, e.p) || z.z || substr(s, e.p + 2),
         (SELECT ... /* MRV pick, on the new board */ ),
         holes - 1
  FROM search, pos e, digits z
  WHERE e.p = search.p
    AND NOT EXISTS (
      SELECT 1 FROM pos q
      WHERE (q.r = e.r OR q.c = e.c OR q.b = e.b)
        AND substr(s, q.p + 1, 1) = z.z)
  ORDER BY 3
),
```

Our query has three columns: the board (`s`), the cell we’ve decided to branch on
next (`p`), and how many empty cells are left (`holes`). The base case is the puzzle
itself. The recursive case takes a state, joins it against the cell we picked
(`e.p = search.p`) and against every digit, keeps the digits that no peer already
holds, and emits one child row per surviving digit. That’s a lot of words, but
the general flow is luckily quite simple.

`NOT EXISTS` models all the rules of Sudoku. A digit is legal at a cell when
there’s no cell sharing its row, column, or box that already holds it. It’s
obfuscated behind the `SELECT` and `WHERE` clauses, but that’s all that’s
encoded here.

So what happens to a dead end? No legal digit means no rows join, which means no
children, and the branch simply stops existing. We never write a failure case,
we just stop the search. Quite cool!

Note also what happened to propagation. Most solvers in this series have had a
propagation step that fills in cells until nothing changes anymore. This one
doesn’t have it, for similar reasons as in [the miniKanren
episode](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EV:_Racket_miniKanren.html).
A cell we can fill in is just an MRV cell with one candidate, so the join
produces one child and the “branch” is deterministic. Recursion itself is the
way that the board gets filled!

### Picking a cell

I had trouble with this part, but I hope you won’t.

We want the empty cell with the fewest legal digits. The natural way to write
that is to order the empty cells by a count

```
SELECT m.p FROM pos m WHERE substr(s, m.p + 1, 1) = '0'
ORDER BY (SELECT count(*) FROM digits d WHERE NOT EXISTS (...)) LIMIT 1
```

which fails

```
Parse error near line 1: no such column: s
```

Correlated references to the outer query resolve fine through nested `WHERE` and
`ON` clauses, but not through a subquery sitting in an `ORDER BY`. I don’t know
whether that’s deliberate<sup><a href="#2">2</a></sup>, but it means that this
way doesn’t work.

What works is to stop nesting and to do a join instead. I want to be transparent
here and say that I had to consult an LLM (OpenAI Codex, GPT 5.6 Sol) to
understand what was going wrong and how to approach this differently, but the
code is mine.

```
(SELECT m.p
 FROM pos m LEFT JOIN digits d
   ON NOT EXISTS (
     SELECT 1 FROM pos q
     WHERE (q.r = m.r OR q.c = m.c OR q.b = m.b)
       AND substr(s, q.p + 1, 1) = d.z)
 WHERE substr(s, m.p + 1, 1) = '0'
 GROUP BY m.p
 ORDER BY count(d.z), m.p
 LIMIT 1)
```

Every empty cell is joined against each digit legal in it, then grouped, and
then we can use `ORDER BY` to do the count for us. Again, unreasonably ugly, but
it works.

It’s important that it’s a `LEFT JOIN`. Why?

A cell with no legal digits still produces one row, with `d.z` null, so
`count(d.z)` is 0 and it sorts to the very front. A board with an impossible
cell therefore picks that cell, finds no digit to join against, and dies on
the next step. Dead-end detection is almost a side effect of our query
structure!

The `m.p` tiebreak is only there because it will make sure the runs are
reproducible.

### Depth-first `ORDER BY`

A recursive CTE is a bit like a queue: SQLite takes a row off of the query,
computes its children, appends them, and repeats. Append and pop from the
front means we have a FIFO, which implies breadth-first. But breadth-first
search on a search tree this wide means we hold an exponentially growing
list of half-finished boards. This is the same conundrum [the fair search in
miniKanren](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EV:_Racket_miniKanren.html)
had to solve. Luckily, like last time, we can fix it with a single line of
cleverness.

SQLite lets the recursive select carry an `ORDER BY`, which turns that queue into
a priority queue. Our third column is `holes`, and it decreases with depth, so:

```
ORDER BY 3
```

pops the most-filled board first, which is depth-first search.

Magic.

Now to be honest, on all of my test puzzles, deleting `ORDER BY 3` changes the
runtime by... nothing at all. Not even a little bit, unless it’s smaller than
what `time` detects for me.

MRV is good enough that the search space never gets wide enough for our
“optimization” to matter. Memory also doesn’t seem to budge much.

I left the clause in because it’s correct and because the traversal really is
different, and because technically correct is still correct. I just wish I had
been actually correct also.

### SQL specialist extra cred

One more for those that know more than me about SQL engines.

I like to play with the boards and test my solvers in various ways. One of my
favorite things is to give it an empty board. This solver hung, and I had to
kill it after 3 minutes.

After a bit of puzzlement, I realized search itself was fine. If I only ask for
`solution` (the first solved board), it comes back in under a second with a
random valid board.

Okay, okay, let’s dive into this, this is weird. What if I ask for a solution or
the fallback separately?

```
SELECT ... FROM solution                             -- fast
SELECT 'x' WHERE NOT EXISTS (SELECT 1 FROM solution) -- also fast
SELECT ... FROM solution
  UNION ALL
SELECT 'x' WHERE NOT EXISTS (SELECT 1 FROM solution) -- hangs
```

Either reference alone is fast. Both in one statement hangs, even with
`MATERIALIZED`. I have no idea why.

My LLM’s guess is that the second reference defeats the streaming evaluation
that lets `LIMIT 1` stop the recursion early, and an empty Sudoku board has
around 6.7×10²¹ solutions to hammer through. Whether that’s a hallucination or
not I cannot tell you, and I’m going to stop before I hop into a rabbit hole
half-way through a 24-part (as of yet!) rabbit hole.

Real puzzles usually have one solution, though, so they’re fine.

But in SQL (and that is my personal takeaway here) you don’t get to control how
your code runs. You describe its meaning, and then the query planner comes
along and does its magic (or not).

## Fin

SQL, I maintain, was the scariest one this batch, because it was so weird and
foreign, and because I refuse to write readable code.

The algorithm was the same, but it looks nothing like anything we’ve seen
before, because it’s not imperative or functional, and it’s not Prolog-style
declarative either. It’s just *weird*.

And I’ll be the first to admit the result is not something I’d want to
maintain. But I don’t think that’s SQL’s fault, really. I programmed in a
query language and it was dumb enough to give me enough power to make that
work. Barely, but still.

And with that, we close season two! We did [APL](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EI:_Dyalog_APL.html),
[Carp](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EII:_Carp.html),
[Forth](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EIII:_Forth.html),
[Smalltalk](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EIV:_Smalltalk.html),
[handrolled miniKanren](https://blog.veitheller.de/Simple_Sudoku_Solvers_SII,_EV:_Racket_miniKanren.html),
and now SQL. This one was a doozy. The first one was about paradigms, and this
one was about my somewhat weirder friends.

Season three is already mapped out, smashing together solvers, specifications,
and constraints: Agda, assembly, Verilog, z3, Futhark, and Alloy. As if SQL
wasn’t enough. Half of those aren’t even really programming languages, and most
of them I’ve never really touched, so I’m all set for a lot of fun, and a lot of
pain.

It’ll be a little while yet (I like to get a headstart on writing the solvers
before I go off and write about them), but [the series
page](https://blog.veitheller.de/sss/) is there for your perusal in the meantime.

Thank you for solving Sudoku with me, in twelve different ways. See you around!

#### Footnotes

<span id="1">1.</span> Side note: our fastest solutions solve those puzzles in
fractions of a millisecond, so we’re not setting any performance records here.

<span id="2">2.</span> I only checked SQLite. I don’t know whether other engines
resolve the same reference, but as I learned the `ORDER BY` on a recursive term
is a SQLite extension anyway, so a portable version of this query would need to
be architected differently anyway. The things you learn by doing cheap magic
tricks!
