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
written in zepto and [online](https://github.com/hellerve/dbc). This installment
should be pretty simple for you if you’ve gone through the series sequentially.
It mostly serves to solidify some of the concepts and intuitions involved in
macro hackery.

I’d also like to add that this project and blog post are inspired by [the work
of a dear friend of mine](https://github.com/ds2643/dbc), who implemented a
Design-By-Contract macro in Common Lisp and wants to port it to Carp. Way to go!

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

First of all, let’s observe that some contract functions might have both pre-
and post-conditions, some might only have either, and some might have none, even
if a contract function without any contracts might not be obviously useful.

This means that we’ll have to add forms for all of these. Let’s build a skeleton
that accomodates all possibilities:

```
(define-syntax defcontract
  (syntax-rules (pre post)
    ((_ nforms body)
      ; form 1
    )
    ((_ nforms (pre cls ...) body)
      ; form 2
    )
    ((_ nforms (post cls ...) body)
      ; form 3
    )
    ((_ nforms (pre precls ...) (post postcls ...) body)
      ; main form
    )
  ))
```
<div class="figure-label">Fig. 3: A big skeleton for `defcontract`.</div>

That’s a little too much boilerplate for my taste, but sometimes utility wins 
out.

The easiest way to derive the simplified forms is probably to base them on the
most general form, i.e. the one with both pre- and postconditions. Then we can
just add an empty set of whatever set of conditions we do not use.

This description might be a little obtuse, so here it is in code form:

```
(define-syntax defcontract
  (syntax-rules (pre post)
    ((_ nforms body)
      (defcontract nforms (pre) (post) body))
    ((_ nforms (pre cls ...) body)
      (defcontract nforms (pre cls ...) (post) body))
    ((_ nforms (post cls ...) body)
      (defcontract nforms (pre) (post cls ...) body))
    ((_ nforms (pre precls ...) (post postcls ...) body)
      ; TODO
    )
  ))
```
<div class="figure-label">
  Fig. 4: A big skeleton with deferred responsibilities.
</div>

All we’re doing here is providing default arguments for our implementation. No
actual work has been done yet, all of that may happen in our last special form.
We already have a nice API, though, and that’s pretty great!

Now we’re ready to do the grunt work. As so very often in this series, we’ll
have to define a function dynamically, so we’ll start by capturing the
environment and adding our macro-expansion/evaluation pipeline.

```
(define-syntax defcontract
  (syntax-rules (pre post)
    ; our other forms

    ((_ nforms (pre precls ...) (post postcls ...) body)
      (with-environment env
        (eval
          (macro-expand
            ; what to expand here?
          env)))))
```
<div class="figure-label">Fig. 5: We’re almost at the good stuff.</div>

Okay, now that we have an environment and an evaluator in place, we need to
start to actually think about what our generated function needs to do. Before
doing any of its actual work, it needs to check all of the preconditions. Then
we need to evaluate the body and check the postconditions, and then we’re done.
Sounds about right? All right, let’s try that!


```
(define-syntax defcontract
  (syntax-rules (pre post)
    ; our other forms

    ((_ nforms (pre precls ...) (post postcls ...) body)
      (with-environment env
        (eval
          (macro-expand
            `(define nforms
              (begin
                ,@(map ($ (list 'assert %)) 'precls)
                body
                ,@(map ($ (list 'assert %)) 'postcls)))
          )
          env)))))
```
<div class="figure-label">Fig. 6: Templating our function.</div>

Okay, so we create a template, filling in the information—`nforms` and
`body`—that we’ve been given by the caller. Before and after executing `body`,
we insert our conditions, wrapping them in `assert` and wrapping that whole list
of expressions in `unquote-splicing`—the reader macro that we use is `,@`—,
which will flatten the list into our template. [This is a good overview of
different forms of quoting](https://8thlight.com/blog/colin-jones/2012/05/22/quoting-without-confusion.html),
if this confuses you—I know it confused me in the beginning!

But we’re not quite done. The return value of the functions will be wrong! In
particular, it will be the result of the last `assert` form that we spliced in.
We can fix that and the problem that the variable `*ret*` that we wanted to
provide to our users isn’t in place yet in the same minor refactor. I’ll only
show the templated function body in this next snippet:

```
`(define nforms
  (begin
    ,@(map ($ (list 'assert %)) 'precls)
    (let ((*ret* body))
      (begin
        ,@(map ($ (list 'assert %)) 'postcls)
        *ret*))))
```
<div class="figure-label">Fig. 7: The final function template.</div>

Okay, this is a little more complex, but basically all we did was use a local
binding to bind the result of `body` to the appropriate variable name and return
it at the very end. Perfect! We’re done!

That wasn’t too bad! In exactly 20 lines, we added a very capable macro for
design by contract emulation.

Before we move on to the notes, however, let me quickly acknowledge that
`assert` isn’t present in all Lisps—in fact, it isn’t even present in zepto!
For this, you can add a polyfill, expressed in yet another macro:

```
(define-syntax assert
  (syntax-rules ()
    ((_ form)
      (if (not form)
        (begin
          (error "Assertion" 'form "failed!")
          (exit 1))))))
```
<div class="figure-label">Fig. 8: An `assert` polyfill.</div>

## Notes

This small macro is capable, but fairly limited. The most obvious thing that’s
missing for people who are coming from Eiffel is probably Invariants. Invariants
are similar to contracts, but they operate on a value level and check that
variables can only hold a certain set of values, and that functions respect
that. You can read about invariants some more [here](https://en.wikipedia.org/wiki/Class_invariant).
Invariants are historically mostly interesting to object-oriented languages,
but they need not be.

An interesting macro that I could think of would be something like this:

```
(definvariant x ((< x 40) (> x -10)) 10)
```
<div class="figure-label">Fig. 9: Another fun macro.</div>

Ideally, this variable would check whether the value it’s assigned is valid
given the constraints. Implementing this is left as an exercise to the
reader—but please note that this might be way more involved than the little
macro that we talked about today, so don’t be frustrated if you don’t arrive at
a suitable implementation. Sometimes pondering problems is much more fun
than actually solving them!

## Fin

The philosophy of design by contract is much more than the simple macro that
we’ve built here, and I don’t want to do it injustice by presenting our results
as if we just reimplemented Eiffel.

Design by contract in particular is of interest because it adds a certain type
of error checking capabilities to a language that, at least in Lisp, smell like
a crutch for avoiding static types.

Thus, in our next blog post, we’ll explore gradual typing, contracts, and how to
add more crutches to our system! Stay tuned!
