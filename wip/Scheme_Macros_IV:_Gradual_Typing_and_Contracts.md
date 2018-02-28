It’s time for another installation of my series on Scheme macros. Previously,
we talked about defining [modules](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html),
[generic functions](http://blog.veitheller.de/Scheme_Macros_II:_Generics.html),
and [local variable binding](http://blog.veitheller.de/Scheme_Macros_III:_Defining_let.html)
using macros. This time I want to write about gradual typing, and how we can use
it to add some contracts to our code, without programming
overhead<sup><a href="#1">1</a></sup>.

As usual for the articles in this series, a basic understanding of Lisp is
required. You should be able to follow along without being a proficient Lisp
programmer, however.

Let’s start by defining an API!

## The API

All we want to have in this system is a way of defining functions that get
typechecked automatically. This means we only have to define a single point
of entry. I’ll call it `define-typed` for now.

```
(define-typed (add-mul x y z)
  (* (+ x y) z))
```
<div class="figure-label">Fig. 1: Defining a simple typed function.</div>

This function looks like any function that we would define in Scheme. We
literally only changed the `define` keyword.

When we look at the function body it is pretty easy to figure out that this
function expects numbers as input. But it is probably not as easy for the
computer to figure out. Let’s try to help it figure out how to do this!

## Implementation

We will have to write a macro to parse the head and body of the function. As
is customary in my blog posts in this series, we will start out by creating a
macro skeleton.

```
(define-syntax define-typed
  (syntax-rules ()
    ((_ head body)
      ; TODO
    )))
```
<div class="figure-label">Fig. 2: A macro skeleton.</div>

Alright, we’ve already separated the head and the body of the function. So far,
so good.

## Recap

## Fin

#### Footnotes

<span id="1">1.</span> Fun fact: I’ve wanted to write about gradual typing for
                       almost as long as I had this blog. See also [this
                       issue](https://github.com/hellerve/blog/issues/8) on
                       Github.
