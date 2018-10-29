In this edition of [my series on Scheme
macros](http://blog.veitheller.de/scheme-macros/), we’re going to take a look
at [Eiffel](https://en.wikipedia.org/wiki/Eiffel_%28programming_language%29);
not so much the programming language itself, mind you, but one of its core
concepts, a concept it popularized in the programming languages community:
[Design By Contract](https://en.wikipedia.org/wiki/Design_by_contract). Design
by contract is a powerful framework for specifying pre- and postconditions for
your functions and methods. It is often used for runtime type checking, but it
can be used to check for arbitrary invariants, e.g. the length of lists or the
shape of your input data.

As a motivating example, let’s look at some Eiffel code that uses contracts:

```
-- N.B. You actually need to wrap this in a class
-- to make it work
add (a, b: INTEGER): INTEGER is
  require
    a_strictly_positive: a > 0
    b_strictly_positive: b > 0
  do
    Result := a + b
  ensure
    result_strictly_positive: Result > 0
  end
```
<div class="figure-label">Fig. 1: A function with contracts in Eiffel.</div>

Even if you’ve never seen the language or know how it works—I can’t say I’ve
ever programmed in it myself—, you might be able to grasp this particular bit
of code: adding strictly positive numbers.

Let’s try and implement this in a Scheme macro! As per usual, the code is
written in zepto and [online](https://github.com/hellerve/dbc).

## An API

Ideally, we’d like to have regular functions augmented with a declarative way of
defining pre- and postconditions. The syntax we’re going to use in this blog
post—and the simplest I could come up with—, looks a little like this:

```
(defcontract (double xs)
  (pre (list? xs) (all? number? xs))
  (post (list? *ret*) (all? number? *ret*))
  (map (lambda (x) (* x 2)) xs))
```
<div class="figure-label">Fig. 2: A function with contracts in zepto.</div>

Shown in Figure 2 is a simple function that doubles a list of numbers. Written
without contracts, it might look something like this:

```
(define (double xs)
  (map (lambda (x) (* x 2)) xs))
```
<div class="figure-label">Fig. 2: The same function, sans contracts.</div>

It is decidedly less verbose, but the only real difference is the different form
name and the missing conditions. The conditions are contained in the `pre` and
`post` forms, respectively, and in this case we’re just checking that the
parameter `xs` that we’re given is in fact a list of numbers and that the return
value `*ret*`—a variable that our macro will have to generate—is of the same
type. Simple enough.

I think it’s time we try our hands at this, shall we?

## An Implementation

TODO

## Notes

TODO

## Fin

TODO
