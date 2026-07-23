---
title: "Simple Sudoku Solvers SII, EV: Racket/miniKanren"
date: 2026-07-23
---

Welcome back to another round of [Simple Sudoku Solvers](https://blog.veitheller.de/sss/)!
If you are unfamiliar with the series, I suggest you start with [the first
post](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html)
or by perusing [the backlog](https://blog.veitheller.de/sss/). Today we return to
logic programming, but this time we’re bringing our own logic language.

Back in [the Prolog episode](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_III:_Prolog.html)
we handed our board to an industrial-strength constraint system and let it do
all the work. That was delightful, but the engine stayed a black box. Today we
do the opposite: we write the logic engine ourselves, in Racket, and then write
the solver on top of it. The engine is µKanren, the famously tiny core of the
miniKanren family of relational languages, from [a 2013 paper by Jason Hemann
and Daniel P. Friedman](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf).
The reference implementation in the paper is under 40 lines of Scheme. Ours will be a
little chattier, but the entire file—engine, syntactic sugar, and solver—comes
in at 168 lines, with zero dependencies. It’s not quite the smallest of our
solvers, but it’s the only one that also implements a logic programming
language, so totally worth it.

As always, [the code is on GitHub](https://github.com/hellerve/sudoku).

## Why Racket/miniKanren?

We already have Prolog, and we already have Common Lisp. Why do the same thing
again, but different?

- **Racket is a language for building languages.** It’s the most deliberate
  incarnation of the Lisp idea that syntax is negotiable. Usually that gets
  demonstrated by building a `#lang`, but it starts smaller: a handful of
  macros can turn a pile of higher-order functions into something that feels
  like a language. That’s exactly what we’ll do today. I’ve
  [written many posts about Scheme macros before](https://blog.veitheller.de/scheme-macros/),
  if you want to go deeper. They’re where DSLs and language design meet, and
  some of my favorite programming gems are elaborate macros.
- **miniKanren is logic programming you can hold in your head.** Prolog gave
  us familiarity with backtracking. miniKanren gives us backtracking as a library,
  and µKanren shrinks that library to its bones: unification, goals, and streams.
  Nothing is magic, because there is no room for magic in so few lines, but the
  code definitely feels a little witch-y.
- **Owning the engine has its perkes.** Because the solver and the engine live
  in the same file, the solver is allowed to peek into the engine’s internals.
  We’ll use that liberally, and I’d argue it makes for a nicer solver than any
  off-the-shelf logic library would. It also requires you to understand both
  parts, so making it small is not optional!

## A µKanren primer

µKanren is built from three ideas: variables and substitutions, goals, and
streams. Let’s take them in turn.

A logic variable is just a tagged value, and a substitution is a map from
variables to the values we’ve learned about them. `walk` looks a term up until
it bottoms out, and `unify` records what has to be true for two terms to be
equal:

```
(struct lvar (idx) #:transparent)

(define (walk t s)
  (if (lvar? t)
      (let ([b (hash-ref s t #f)])
        (if b (walk b s) t))
      t))

(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(equal? u v) s]
      [(lvar? u) (hash-set s u v)]
      [(lvar? v) (hash-set s v u)]
      [else #f])))
```

If the two terms are already equal, we’re done. If either is an unbound variable,
we bind it to the other term. Otherwise unification fails and we return `#f`. The
paper uses association lists, but we use Racket’s immutable hashes, because it buys
us cheap lookups without changing any shapes.

A *goal* is a function that takes a state (a substitution plus a counter for
minting fresh variables, hello [gensym](https://blog.veitheller.de/Scheme_Macros_X:_Meta_macros.html#act-ii-generating-symbols))
and returns a stream of states in which the goal holds. The simplest goals are the
equality goal and the one that introduces a fresh variable:

```
(define mzero '())
(define (unit st) (cons st mzero))

(define ((== u v) st)
  (let ([s (unify u v (car st))])
    (if s (unit (cons s (cdr st))) mzero)))

(define ((call/fresh f) st)
  (let ([c (cdr st)])
    ((f (lvar c)) (cons (car st) (add1 c)))))
```

The doubled parentheses in `(define ((== u v) st) ...)` are Racket shorthand
for a curried function: `(== u v)` returns the actual goal, which then takes a
state. Succeeding means returning a one-element stream, failing means
returning the empty one.

The last piece is streams, and this is where the search lives. A stream is
either empty, a state consed (push-front) onto a stream, or a thunk—a paused
stream that will produce more when poked. `mplus` merges the answers of two
goals, `bind` feeds every answer of one goal into the next, and `disj` and
`conj` package those up as “or” and “and”:

```
(define (mplus $1 $2)
  (cond
    [(null? $1) $2]
    [(procedure? $1) (λ () (mplus ($1) $2))]
    [else (cons (car $1) (mplus (cdr $1) $2))]))

(define (bind $ g)
  (cond
    [(null? $) mzero]
    [(procedure? $) (λ () (bind ($) g))]
    [else (mplus (g (car $)) (bind (cdr $) g))]))

(define ((disj g1 g2) st) (mplus (g1 st) (g2 st)))
(define ((conj g1 g2) st) (bind (g1 st) g2))
```

That’s all we need! Unification, four combinators, and a stream type and we’re
off. Keep `mplus` in the back of your mind, though—we’ll come back to it,
because I changed one line in it, and that line is my favorite part of this
post, spoiler alert.

## The sugar

Written with raw combinators, everything is binary and everything is a lambda,
which gets old fast. This is where Racket gets to shine: a few `syntax-rules`
macros give us arbitrary arity conjunction and disjunction, `fresh`, and even
Prolog-ish `conde`:

```
(define-syntax-rule (Zzz g) (λ (st) (λ () (g st))))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (conj* g ...)]
    [(_ (x xs ...) g ...)
     (call/fresh (λ (x) (fresh (xs ...) g ...)))]))

(define-syntax-rule (conde (g ...) ...)
  (disj* (conj* g ...) ...))
```

`Zzz` (the name is from the paper, and I refuse to change it, because names are
an illusion) wraps a goal in a thunk so that a recursive goal doesn’t recurse while
we’re still *constructing* it, only when the stream actually pulls on it. It is
the delay that makes infinite, or realistically at least very deep, searches possible,
and we’ll use that in a second!

I’ll be honest: the solver barely uses `fresh` and doesn’t use `conde` at
all. But a miniKanren without them wouldn’t feel like a language, and each of
them is three lines, so I kept them. Sue me.

## The solver

The board plumbing is plain Racket, and by episode eleven we could write it
in our sleep, in any language: a vector of 81 cells, a precomputed `peers` table
listing the indices each cell shares a row, column, or box with, and a candidate
function that subtracts the peers’ digits from one through nine:

```
(define (cands b n)
  (remove* (for/list ([k (vector-ref peers n)]
                      #:unless (zero? (vector-ref b k)))
             (vector-ref b k))
           digits))
```

The interesting part is how the solver plugs into the engine. Every given digit
stays a plain number, and every empty cell becomes a logic variable. The whole
solver is then a single recursive goal:

```
(define ((solveo cells) st)
  (define s (car st))
  (define b (for/vector ([c cells])
              (let ([v (walk c s)])
                (if (lvar? v) 0 v))))
  (define empties
    (for/list ([n 81] #:when (zero? (vector-ref b n)))
      (cons n (cands b n))))
  (define g
    (cond
      [(null? empties) succeed]
      [(memf (compose null? cdr) empties) fail]
      [else
       (define mrv (argmin (compose length cdr) empties))
       (disj/list
        (for/list ([d (cdr mrv)])
          (conj (== (vector-ref cells (car mrv)) d)
                (Zzz (solveo cells)))))]))
  (g st))
```

Because we own the engine, `solveo`<sup><a href="#1">1</a></sup> is allowed to
`walk` the board under the current substitution and see, in ordinary Racket, which
cells are still open. With a packaged miniKanren we’d have to squeeze this through
`project` or similar escape hatches, but we own everything. Then the familiar
algorithm plays out: no empty cells means we succeed, an empty cell without
candidates means we fail, and otherwise we take the most constrained cell and try
each of its candidates.

Notice that our phases all vanish! Assignment is unification: filling in
a cell is `==`, and the substitution is the board history. Backtracking is
equivalent to the stream: each candidate is one branch of a `disj`, and when a
branch fails, its stream simply empties and `mplus` moves on to the next. And
propagation disappeared entirely<sup><a href="#2">2</a></sup>: a filled cell is
just an MRV cell with exactly one candidate, in which case the `disj` above has
one branch and is deterministic. The fixpoint loop we’ve written so many times
is the engine recursing through `Zzz`.

### My favorite part

Now, about that one line I teased above. µKanren’s actual `mplus` reads:

```
[(procedure? $1) (λ () (mplus $2 ($1)))]
```

and mine reads:

```
[(procedure? $1) (λ () (mplus ($1) $2))]
```

Looks like a typo, right? What does the line even do?

The paper swaps the arguments every time it hits a thunk. That makes the
search *interleave*: it takes turns between branches instead of committing to
one, which is the fair thing to do, and fairness is one of miniKanren’s big
ideas. If one branch is an infinite rabbit hole, a fair search still finds
answers in the others. Prolog, famously, does not.

I started with the paper’s version, and all of my test puzzles solved
quickly. Then I fed it an empty board, and it let me have a really long typing
break. The DFS version solves an empty board in 2 milliseconds. I killed the
fair version after a minute.

On an open board almost every guess is completable, so hardly any branch fails,
and instead of racing down one branch to a solution, the fair search keeps rotating
through an exponentially growing frontier of half-finished boards. Fairness is a
virtue when branches might not terminate, but our branches always terminate!
They’re just numerous, so fairness only exhausts us for no reason. Prolog chose
unfair depth-first search for exactly this kind of workload, and with a one-line
change, so did we.

I think that’s the real payoff of building the engine yourself. Even things like
search order become a decision you can influence yourself, and that’s really
cool.

## Fin

We solved Sudoku again, and along the way we implemented a logic programming
language, which I think is the best ratio of “languages built” to “lines
written” on my blog (and we’ve written a lot of languages together!). The
engine half is so tiny I’d even call it cute, the solver half barely differs
from the algorithm we’ve written all along, except that its assignments are
unifications and its backtracking is a lazy stream.

Racket is really good for building languages, even without trying! We didn’t use
any of the explicit language-building features, and we still ended up with
something cool.

This was the penultimate episode of season two. Next time we close the season
out with SQL, in which our solver will be a query. It’s the one I was scared of
this season, but I think I got it now. See you then!

#### Footnotes

<span id="1">1.</span> All the *Kanren functions end in `o`. Why? I thought it
was a weird quirk for the longest time, until I read [The Reasoned Schemer](https://mitpress.mit.edu/9780262535519/the-reasoned-schemer/)
for the first time, in which they explained it’s an ASCII version of using a
subscript `°`, such that `solveo` should really be `solve°`. Why the `°`? It’s
supposed to look like the upper part of a `?`, because Scheme predicates end
in it, so relations need a related marker. Whether that resemblance makes any
sense I will leave to you.

<span id="2">2.</span> My first version kept propagation as a separate phase:
it collected every forced cell from one snapshot of the board and asserted
them all at once. That version cheerfully “solved” Inkala’s board with two 9s
in the first row, because two peers can each be forced to the same digit and
nothing ever rechecks cells once they’re filled. Filling one cell per pass
fixed it, and then the separate phase folded into the MRV branch on its own.
