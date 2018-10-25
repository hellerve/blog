It’s time for another blog post on Scheme macros! This time we’re going to look
at conditionals, and as a bonus I’ll show you a cool trick on how to simulate
`if`.

The structure should by now be familiar: we think of an API, then we implement
it, and then we reflect on what we just did. The code we will be writing will be
written in [zepto](https://github.com/zepto-lang/zepto)—we’ll be using mostly
R5RS-compliant Scheme functionality—, and the code is [online](/assets/if.zp)
as always. Let’s go!

## An API

There are many conditionals in Scheme that we can try and reimplement. We will
build our conditionals from the ground up, and first I’ll show you a little
mindbending way of implementing `if` as a macro.

We want to support both `if-then` and `if-then-else`. Both of these forms should
be familiar to you:

```
; if-then
(if condition then-clause)

; if-then-else
(if condition then-clause else-clause)
```
<div class="figure-label">Fig. 1: The archetype of `if`.</div>

So far, so simple. There are at least two other well-known conditionals that we
can implement as macros on top of that, and that’s just what we’ll do! Enter
`cond` and `case`:

```
; cond is a multi-clause if
(cond
  (condition1 clause1)
  (condition2 clause2)
  ; ...
  (conditionN clauseN)
  (else else-clause))

; case is a switch statement
(case expr-to-switch-on
  ((comparator1 ... comparatorN) body)
  ((comparator1 ... comparatorN) body2)
  ; ...
  (else else-clause))
```
<div class="figure-label">Fig. 1: The archetypes of `cond` and `case`.</div>

These two forms are far more expressive than `if`, but also far more
specialized. `cond` is great if you have a lot of clauses with different
conditions, and `case` is useful for a situation in which you would reach for
`switch` in C-like languages—but it works on arbitrary types.
