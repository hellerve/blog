It’s time for another post [in my series on Carp patterns](/carp-patterns).
This time we’re going to be looking at macros, and the dynamic parts of your
code more generally.

There are many different opinions when it comes to macros, when to use them,
and how to organize them. Different Lisp communities have different best
practices, and each has good reasoning behind them. While the use of macros is
somewhat discouraged in Clojure, for instance, they are ubiquituous and widely
embraced by the Common Lisp community.

It’s no surprise, therefore, that most of the literature on the craft of macros
comes from the Common Lisp tradition. This is not to say they do not exist in
other Lisps—one of my favorites is actually “Mastering Clojure Macros”. If you
want to read more about this topic, you can check out my reference section
below.

At no point is it my intention to pretend that my opinion is the only valid way
of going about constructing your macros. This post is meant to give you some
hints and guidelines that I wish I would have had when I started out writing
macros in Carp. To this end, we will look at the REPL and how it facilitates
development, dynamic functions and macros and what makes them different from
each other, and when to use either. After having read this blog post, you
should feel ready to write your own first macro, simple or complex.

For those of you that don’t know: I also wrote [a series of blog posts on
Scheme macros](https://blog.veitheller.de/scheme-macros). Scheme’s macro system
is interesting in its own right, but quite different from Carp’s, which is
inspired by Common Lisp’s and Clojure’s macro systems. Nonetheless, many of the
underlying techniques stay the same, and you might be able to take something
away from reading the series.

With all of this out of the way, let’s get this party going!

## The REPL is your friend

First off, let’s look at the Carp REPL in the context of macros. The REPL tries
to be somewhat smart about when to evaluate and when to compile and run.
One decision that it makes is that when it encounters macros, it just expands
them. This can sometimes lead to somewhat awkward situations, but it’s amazing
when you’re working on your macros. Writing a little macro or helper and being
able to see whether its output makes sense without any ceremony is extremely
helpful.

Programming with dynamic functions—which you define using `defndynamic`—is what
most feels like working with a REPL in the traditional sense: You define
something, you call it, and you get some output. You can do more or less the
same thing with static functions, of course, but because of the compile-and-run
cycle this can sometimes feel a little sluggish. When working with dynamic
values on the other hand, the fidelity of the REPL is quite high.

I personally often use it for prototyping, even for multi-line functions,
something the Carp REPL handles quite gracefully. Try it out, you might like
it!

## Dynamic functions and macros—two sides of the same coin?

Depending on your background and proficiency with Carp terms, the last few
paragraphs might have confused you: what are dynamic functions, and how are
they different from regular functions? How are they different from macros? And
when do I use what?

Dynamic functions are functions that run at compile time. They evaluate their
arguments, which contrasts to macros, and they are untyped and interpreted,
which makes them different from regular functions. Let’s look at an example:

```
; will be typed as (Fn [a] a), compiled, and run
(def arg 1)
(defn id [x] x)
(id arg) ; => 1


; will be typed as Dynamic, and evaluated directly
(defdynamic arg 1)
(defndynamic id [x] x)
(id arg) ; => 1

; will be typed as Macro, and evaluated directly
(defmacro id [x] x)
(id arg) ; => arg
```
<div class="figure-label">Fig. 1: Functions, dynamic functions, and macros.</div>

This means that they are good at different things: Functions are your program’s
bread and butter. You use them to construct the program you want to build and
eventually run.

Macros, on the other hand, are a way of transforming source code. You take in
expressions as they are, and manipulate them directly, possibly spitting out
other expressions.

Dynamic functions, then, are a way to program at compile time. This can be used
for plumbing in macros, but also to generate source code. The main difference
is that any expressions that you passed in are evaluated first and thus you
can’t pass in S-expressions directly without first quoting them.

I usually use macros when meta-programming, and dynamic functions for plumbing.
I can always use `eval` if I end up needing to evaluate something. This keeps
the concepts neat and separated, and I always know what to reach for when.

## An example

To drive our point home, let’s try to recreate a little macro from Carp’s
standard library: the threading macro. For those of you who are unfamiliar with
it, the threading macro is a convenient way to chain multiple function
applications on one input, and its symbol is `=>`.

```
; will increment 1 and then double it
(=> 1
    (inc)
    (* 2))
```
<div class="figure-label">Fig. 2: `=>` in action.</div>

The way it works internally is that it goes through each function in the chain
and puts the result of the last chain in as the first argument to the next
expression. Thus, it would rewrite the example from Figure 2 to
`(* (inc 1) 2)`. You can use the REPL to confirm that.

Let’s try and recreate this. We will first write a macro that takes in an
initial input and a series of expressions:

```
(defmacro our=> [input :rest exprs]
  ; now what?
)
```
<div class="figure-label">Fig. 3: A skeleton for `our=>`.</div>

Now we can use `reduce` to go over the list of values and fold them together,
like this:

```
(defmacro our=> [input :rest exprs]
  (reduce (fn [acc f] (cons (car f) (cons acc (cdr f))))
          input
          exprs)
)
```
<div class="figure-label">Fig. 4: `our=>`, first version.</div>

And already we are done. But let’s just say we would like to pull out the
function we defined with `fn` in Figure 4, because we also want to write
`==>`, which puts the argument in the last position, and we want to share
as much code as possible. Here’s how this could work:

```
(defndynamic thread-helper [input combine-f exprs]
  (reduce combine-f input exprs))

(defmacro our=> [input :rest exprs]
  (thread-helper input
                 (fn [acc f]
                   (cons (car f) (cons acc (cdr f))))
                 exprs))

(defmacro our==> [input :rest exprs]
  (thread-helper input cons-last exprs))
```
<div class="figure-label">Fig. 4: `our=>` and `our==>`.</div>

And, as it turns out, `reduce` is nothing but a dynamic function itself, so if
you want to go down that rabbit hole, you could even write your own reducer for
this! In the end, dynamic functions are nothing but the computational building
blocks for your macros.

For now, though, let’s stop and try to come to a conclusion.

## Fin

I hope that this blog post helped you understand what macros and dynamic
functions are in the context of Carp and how to use them, and that it took
away some of the fear you might initially have when embarking on your journey
into the realm of metaprogramming!

See you soon!

#### References

* [Greg Hendershott: Fear of Macros](https://www.greghendershott.com/fear-of-macros/all.html)
* [Doug Hoyte: Let Over Lambda](https://letoverlambda.com/)
* Colin Jones: Mastering Clojure Macros
* Paul Graham: On Lisp
