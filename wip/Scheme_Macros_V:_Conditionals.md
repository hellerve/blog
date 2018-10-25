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
<div class="figure-label">Fig. 2: The archetypes of `cond` and `case`.</div>

These two forms are far more expressive than `if`, but also far more
specialized. `cond` is great if you have a lot of clauses with different
conditions, and `case` is useful for a situation in which you would reach for
`switch` in C-like languages—but it works on arbitrary types.

And that’s all we will be doing today. Let’s jump right in!

## Implementing Conditionals

If we want our own conditionals, we should probably start by implementing our
own form of `if`.

### Implementing ìf`

Today I want to show you a weird way of defining `if`, and for that we first
need to implement our own boolean values, because we will mostly rely on those
in our conditional. Behold our alternative booleans:

```
(define (my-true x y) (x))

(define (my-false x y) (y))
```
<div class="figure-label">
  Fig. 3: `true` and `false` as contrived functions.
</div>

Alright, so this is already weird. Both booleans are functions that take two
values, where `true` executes the first and throws away the second, and `false`
does the inverse—and for collision avoidance purposes we prefix these names with
`if`. This might not make any sense yet, so let’s immediately follow them up
with a macro implementing `if`, again prefixed:

```
(define-syntax my-if
  (syntax-rules ()
    ((_ condition tbody)
      (my-if condition tbody nil))
    ((_ condition tbody fbody)
      (condition
        (lambda () tbody)
        (lambda () fbody)))))
```
<div class="figure-label">
  Fig. 4: `if` as a macro.
</div>

Okay, so all `if` does is take a condition and either one or two forms. If we
Provide it with only one, we provide `nil` as the default else clause. If we
provide both, we call the condition with both expressions wrapped in anonymous
functions. Wait, what?

Basically, what we’re doing here is outsourcing the behaviour of `if` to the
booleans themselves<sup><a href="#1">1</a></sup>. This might still be a little
puzzling, so let’s think of how we would use this and what this would do.

```
; what we would write:
(my-if my-true
  (write "yes") ; this branch gets executed
  (write "no"))

; what it would be turned into by macro expansion:
(my-true
  (lambda () (write "yes"))
  (lambda () (write "no")))
```
<div class="figure-label">
  Fig. 5: `my-if` before and after macro expansion.
</div>

Remember, all that booleans are are functions that take two other functions and
pick which one to execute. This actually works out just fine!

It should also make a little more sense at this point why we wrap the bodies in
lambdas. If we didn’t do this, both of the calls to `write` would be executed
when we pass it into the booleans rather than, as we prefer, deferred until we
know which of them to execute.

That was not a lot of code, but it’s fairly profound—or at least I felt that way
when I first discovered this trick. So you might want to step back and think
about this for a while before continuing.

#### Caveat

One important caveat needs to be discussed before we move on: this version of
`if` by itself is nifty, but useless. All of the comparators need to be
reimplemented to return the two new boolean functions; if you want to go for
something like this in your language, you have to integrate it more deeply with
your language and libraries, so that they return the right version of booleans.

### Implementing `case` and `cond`

While this was fun, we’re going to step back for a second now and use regular
`if` statements for the rest of this blog post. The reason for this is simple:
we will use functions that return old-style booleans, and we would have to
reimplement all of those to leverage our new macro.

Let’s start with `case`.

#### `case`

#### `cond`

## Notes

## Fin

#### Footnotes

<span id="1">1.</span> Fun fact: this is exactly what [Smalltalk](https://pozorvlak.livejournal.com/94558.html)
                       does, but in an object-oriented rather than in a
                       functional way. This is a nice example of how Lisp and
                       Smalltalk often feel similar, with one being very
                       functional/macro-oriented and the other doing everything
                       with objects.
