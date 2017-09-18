After exploring how to implement a [module system](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html)
and [generic functions](http://blog.veitheller.de/Scheme_Macros_I:_Generics.html)
in Scheme macros, this time we’ll explore how to reimplement
[let-style](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Lexical-Binding.html)
local bindings. As a little extra, we’ll also explore a way of defining `letrec`
that I’ve never seen used before, partly because it’s somewhat inefficient—but
at least it gets rid of mutable state.

As always, we’ll first define an API, and then implement it bit by bit,
learning about the intricacies of well-crafted macros on the way.

Some experience with Scheme macros—being able to read through them should
suffice—is assumed. Experience with `let` is not required.

## The API

`let` is an integral part of any Scheme. It is the standard way to create
local bindings for most implementations. Some implement it through macros,
some opt for a special form. Today we’re going to see how to implement a simple
form of `let` and its derivatives as a macro.

```
(let ((x 1)
      (y 2))
  (+ x y))
```
<div class="figure-label">Fig. 1: A simple use case of let.</div>

As you can see in Figure 1, `let` takes a list of pairs, where the first value
is the variable name and the second value is what we bind the name to. Simple
enough, but fairly powerful, because it gives us something a lot of languages
have lacked for a long time: block scope. The values bound by `let` shall not
leak outside its body, which makes it a powerful building block for
abstractions.

This is not the full power of `let`, though. `let` can also be used for
looping, like so:

```
(let my-loop ((x 1))
  (if (> x 10)
    (write "We’re done!")
    (my-loop (+ x 1))))
```
<div class="figure-label">Fig. 2: A silly loop.</div>

This is very similar to a named function, but it’s ephemeral. The name
`my-loop`, too, will not leak outside of the context introduced by `let`. We
parametrize every call to that function-like thing with the new values for the
local bindings, much like we would for a function.

Bindings inside the `let` block do not know of each other. That means that a
construct like the following is not valid:

```
(let ((x 1)
      (y (+ x 1)))
  (+ x y)) ; this will complain about the absence of y
```
<div class="figure-label">Fig. 3: Blowing up `let`.</div>

All is not lost, however. This is where `let*` comes into play. It is defined
for exactly this purpose: being able to reference earlier bindings from later
ones. The above construct suddenly becomes valid Scheme if we add that little
asterisk.

There is another limitation to `let`, of course: bindings cannot be recursive.
While you are able to define functions in `let`, making them call themselves is
not allowed. Let’s consider another example:

```
(let ((my-func (lambda (x)
                (if (x > 10) x (my-func (+ x 1))))))
  (my-func 3))
```
<div class="figure-label">Fig. 4: `let` is letting us down. Geddit? Sorry.</div>

And again, yet another construct comes to the rescue: `letrec`. It allows us to
define recursive functions in its bindings. But, of course, we lose the ability
of `let*`: we cannot reference later bindings anymore. That’s what `letrec*` is
for...

At this point, you might ask yourselves a simple question: why? Why don’t we
make `letrec*` the default, if it’s the most capable of all of these
constructs? This is where programming language history comes into play: Scheme
is very old. If you’ve read [some of my earlier musings](http://blog.veitheller.de/Pattern_Matching,_A_Thing_Of_The_Past.html),
you’ll know that Scheme appeared in 1975. That makes it only slightly younger
than C (1972, according to Wikipedia), the oldest programming language most of
us still use. Back then, efficiency mattered. We say it matters now, but when
the best computer most programmers use has 9KB of memory and an add takes
5 microseconds<a href="#1"><sup>1</sup></a>, efficiency truly matters—I
imagine people took a lot of coffee breaks back then. Now, when efficiency
truly matters, you don’t want to waste any cycles for features you don’t need,
and sometimes a fancy programming language feature does exactly that: waste
cycles. So back in the day it was a great idea to make the least expensive
feature the default, and if people needed it, they could upgrade to one of
the fancier versions.<a href="#2"><sup>1</sup></a>

For now, let’s try and implement the simplest version first: plain `let`.

## let

At it’s core, `let` is quite simple: if we just want to introduce local
bindings, we can just rewrite `let` expressions to a lambda that takes a bunch
of arguments and is immediately called with the values that were provided in
the form. Let’s build a simple macro that does exactly this:

```
(define-syntax let
  (syntax-rules ()
    ((let ((var val) ...) body ...)
      ((lambda (var ...) body ...) val ...))))
```
<div class="figure-label">Fig. 5: `let`, implemented.</div>

That doesn’t look so bad, does it? Let’s walk through it together before we
make our version feature-complete.

This definition of `let` makes heavy use of ellipses—the `...`—to destructure
the form. We take all of the variable names—named `var` here`—and put them in
the argument list of the lambda. Then we take the body and set it as the body
of the lambda. Finally, we take all of the values—names `val` here—and call our
lambda with them. Let’s look at the example from Figure 1, before and after
transformation:

```
; before:
(let ((x 1)
      (y 2))
  (+ x y))

; after:
((lambda (x y) (+ x y)) 1 2)
```
<div class="figure-label">
  Fig. 6: The example from Figure 1, before and after macro expansion.
</div>

It’s that simple! In fact, it’s so deceptively simple that it took me a while
to believe that this actually was all `let` required to work. I’m still
astounded to this day when I look at it.

To digest our marvel, let’s look at how to implement the labels from Figure 2.
We will add another syntax rule to our definition of `let`, which means we end
up with the following definition:

```
(define-syntax let
  (syntax-rules ()
    ; the rule from Figure 5
    ((let label ((var val) ...) body ...)
      ((lambda ()
        (define label (lambda (var ...) body ...))
        (label val ...))))))
```
<div class="figure-label">Fig. 7: A simplistic definition of labels.</div>

This is also fairly simple! It’s likely not the version you’ll find in the
books, because PLT purists have found a more theoretically sound implementation
over the years, but it’s good enough for us: it just wraps the expansion in
another lambda, so that we have a local closure in which we can call our label.
That’s a lot of lingo, so let’s look at another example:

```
; before:
(let my-loop ((x 1))
  (if (> x 10)
    (write "We’re done!")
    (my-loop (+ x 1))))

; after:
((lambda ()
  (define my-loop (lambda (x)
                    (if (> x 10)
                      (write "We’re done!")
                      (my-loop (+ x 1)))))
  (my-loop 1)))
```
<div class="figure-label">
  Fig. 8: The example from Figure 2, before and after expansion.
</div>

And that’s it! We’re done with `let`. And because that didn’t require a lot of
code, we’ll now look at `let`s more handy brother, `let*`.

## let\*

## letrec—a unique approach

## Caveats

## Fin

#### Footnotes
<span id="1">1.</span> No, really. At least according to the numbers on
[Wikipedia](https://en.wikipedia.org/wiki/PDP-7).

<span id="2">2.</span> My reading of the original lambda papers tells me that
`let` was called `labels` back then. I’m not positive the “fancier” versions
even existed.
