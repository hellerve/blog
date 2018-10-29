It’s time for another blog post on Scheme macros! This time we’re going to look
at conditionals, and as a bonus I’ll show you a cool trick on how to simulate
`if`. If you want to go back to the list of posts on Scheme macros that have
been written, [check out this list](/scheme-macros/)!

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
; n.b. in scheme you’d have an extra set of
;      parentheses around the condition-clause
;      pairs
(cond
  condition1 clause1
  condition2 clause2
  ; ...
  conditionN clauseN
  else else-clause)

; case is a switch statement
(case expr-to-switch-on
  (comparator1 ... comparatorN) body
  (comparator1 ... comparatorN) body2
  ; ...
  else else-clause)
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

Today I want to show you a weird way of defining `if`. To do this, we first
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
puzzling, so let’s think of how we could use this and what it would do.

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

Remember, booleans are just functions that take two other functions and
pick which one to execute. This actually works out just fine!

The reason for wrapping the bodies in lambdas should also make a little more 
sense at this point. If we didn’t do this, both of the calls to `write` would 
be executed when we pass it into the booleans rather than, as we prefer, 
deferred until we know which of them to execute.

That was not a lot of code, but it’s fairly profound—or at least I felt that way
when I first discovered this trick. So you might want to step back and think
about this for a while before continuing.

#### Caveat

One important caveat needs to be discussed before we move on: this version of
`if` by itself is nifty, but useless. All of the comparators need to be
reimplemented to return the two new boolean functions; if you want to go for
something like this in your language, you have to integrate it more deeply with
your language and libraries so that they return the right version of booleans.

### Implementing `cond` and `case`

While this was fun, we’re going to step back for a second now and use regular
`if` statements for the rest of this blog post. The reason for this is simple:
we will use functions that return old-style booleans, and we would have to
reimplement all of those to leverage our new macro.

Let’s start with `cond`.

#### `cond`

On a fundamental level, `cond` is fairly simple. It is fairly similar to
`if-elif-else` in a lot of other languages—Scheme just uses different
terminology according to its tradition.

With that in mind, all we have to do is to implement a macro that rewrites the
`cond` expression to be nested `if` special forms.

Let’s start with a skeleton:

```
(define-syntax my-cond
  (syntax-rules (else)
    ; cases
  )
)
```
<div class="figure-label">Fig. 6: A skeleton for `cond`.</div>

So far, so simple. All we have to do is catch `else` as a keyword, because we
will be using it as a literal.

Now we’ll implement our base cases, namely the `else` case or the last pair of
condition and clause. Both of these can be at the end of our macro, so we’ll
have to handle them.

```
(define-syntax my-cond
  (syntax-rules (else)
    ((_ else result) result)
    ((_ test result) (if test result))
    ; recursive case
  )
)
```
<div class="figure-label">Fig. 7: `cond` with base cases implemented.</div>

Alright, that’s still pretty straightforward. All we do in the case of `else` is
fill in the `result` we’re given, because it will always execute. In the case of
a final conditional, we’ll make this a single-branched `if`. But what about the
case when there are still multiple branches to go? We’ll have to use recursion!

```
(define-syntax my-cond
  (syntax-rules (else)
    ; base cases

    ((_ test result
        rest ...)
     (if test
       result
       (my-cond rest ...)))
  )
)
```
<div class="figure-label">Fig. 8: The recursive case of `cond`.</div>

That isn’t so bad! We’re compiling the recursive case into an `if` form in which the
conditional is—well—the conditional, and the first clause is the clause that we
paired with it. The other clause will be determined by the recursive expansion
from the next call to `my-cond`.

Phew, that was fun! Now on to `case`, which is a little more complex, but—at
least in theory—fairly similar.

#### `case`

At a first glance, `case` looks similar to the more well-known `switch`
statements in other languages, sans `break` and with arbitray types. Our
variation on the theme will always work with lists of comparators, to make
implementing it a little more straightforward<sup><a href="#2">2</a></sup>.
Extending it to single-value comparators is left as an exercise to the reader.

We’ll start with a skeleton that looks remarkably similar to the one that we
made for `cond` in Figure 6.

```
(define-syntax my-case
  (syntax-rules (else)
    ; ...
  )
)
```
<div class="figure-label">Fig. 9: A skeleton for `case`.</div>

Only the name of the macro has changed. If we go look at the base cases,
however, we will already notice a few differences:

```
(define-syntax my-case
  (syntax-rules (else)
    ((_ key
       else result)
      result)
    ((_ key
       (atoms ...) result)
     (if (in? '(atoms ...) key)
       result1))
    ; recursive case ...
  )
)
```
<div class="figure-label">Fig. 10: The base cases for `case`.</div>

The rule for `else` has hardly changed, we’ve merely added the `key`. The other
base case now checks whether `key` is in the list of `atoms` that we’ve been
passed. The function `in?` is an addition by zepto that works on any collection,
the equivalent in Scheme would be `member`.

With that out of the way, we can look at the recursive case again, which is
really just the second base case with a little bit of recursion mixed in,
similar to what we did in `cond`.

```
(define-syntax my-case
  (syntax-rules (else)
    ; base cases

    ((_ key
       (atoms ...) result
       clause clauses ...)
     (if (in? '(atoms ...) key)
       result
       (my-case key clause clauses ...)))
  )
)
```
<div class="figure-label">Fig. 11: The recursive case for `case`.</div>

All we’re doing differently in this case is, again, recursion. What’s different
this time is that we keep passing down the `key` we’ve been given.

This was pretty simple! But with these two small macros we’ve added multi-branch
conditionals and `switch`-style branching, which are fairly powerful constructs.
Exciting!

## Notes

Now it is time to dampen the excitement a little. While it’s true that we’ve
added very powerful constructs to our arsenal with this post, it’s important to
realize that, as shown in this post, they are actually incomplete.

First of all, none of them do any error checking! If the wrong number of
arguments is passed, we'll probably get very confusing errors—it’s hard to
generate good errors from within the macro expander, and very few Lisp
implementations do.

More importantly, there are a few subtle flaws. The way it is written right now,
`case` will re-evaluate whatever is passed into it as `key` for every branch,
for example. This is trivially fixable, but it’s not the only flaw that’s
lurking in there. There are [better, more powerful implementations](https://github.com/zepto-lang/zepto-stdlib/blob/master/extra.zp#L3)
in the wild that you should probably review before rolling your own solution.
The implementations above are great as pedagogical tools, but maybe not for your
next production system.

Similarly, you should evaluate what it means for your system to implement `if`
in the way described above. I find it to be conceptually neat, but it might come
at a runtime cost, depending on the system you build this on. It also means that
you need to update all of your primitives that return booleans to return either
of the functions we’ve defined above—there are a lot of moving parts involved
in replacing a fundamental value like this.

That being said, noone says you can’t have fun with these constructs. Try adding
to them! Some of the missing functionality was talked about above, and maybe you
can think of more features to add to your new favorite conditionals!

You can also play around with syntax. As I noted above, `case` and `cond`
usually have an extra set of parentheses around the condition-clause pairs.
Maybe that’s not your thing, but you’d like to add `=>` between them to add
visual grouping? Or maybe you’d like to throw out the `else` special keyword,
because strictly speaking it’s not necessary? All of that is possible, and not
very hard to implement. Try it out and see for yourself!

## Fin

In this blog post, we’ve implemented a bunch of conditionals, something that we
might have thought of fundamental and necessarily provided by the language. With
the power of macros, this needn’t be true. We can add our own abstractions, even
on the boolean plane.

See you soon, and happy hacking!

#### Footnotes

<span id="1">1.</span> Fun fact: this is exactly what [Smalltalk](https://pozorvlak.livejournal.com/94558.html)
                       does, but in an object-oriented rather than in a
                       functional way. This is a nice example of how Lisp and
                       Smalltalk often feel similar, with one being very
                       functional/macro-oriented and the other doing everything
                       with objects.

<span id="2">2.</span> This is similar to writing a `switch` with multiple
                       `case` labels leading to one execution path.
