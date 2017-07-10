One of the most powerful—and, for a large part of the programmer community,
most dreaded—features of Lisp is its metaprogramming. In this series I'll
attempt to give a few interesting cases for how we can level up our code
through the use of macros.

This time we will look at how to implement a module system. Many Lisps already
come with a module system out of the box—both R7RS and Common Lisp provide a
facility for packaging and namespacing libraries and modules—, but it is still
helpful to know how these abstractions are built and look from the inside.

The module system we're about to implement is a stripped-down, simplified
version of the actual [zepto module system](https://github.com/zepto-lang/module).

## An API

As per usual in my tutorial-style blog posts, we're going to take the route of
defining an API first and then trying to implement it. Let's look at a simple
module and the syntax involved in defining it.

```
(defmodule math
  (export add sub (m :as mul))

  ; Optional requires through a (require ...) form

  (define (op f x y) (f x y))

  (define (add x y) (op + x y))
  (define (sub x y) (op - x y))
  (define (m x y) (op * x y)))
```
<div class="figure-label">Fig. 1: Defining a module.</div>

Now, the aesthetics of this are obviously in the eye of the beholder, but this
system provides us with all of the facilities a module system is generally
expected to provide. What about importing defined modules then?

```
; import functions take the form <namespace>:<function>
; will import everything in the module
(import math)

; will import everything in the module namespaced as m
(import math m)

; will only import the add function from math
(import math:add)

; will only import the add function from math, aliased
(import math:add math:add-renamed)
```
<div class="figure-label">Fig. 2: Importing from a module.</div>

These are the basic building blocks we need for importing from a module. We can
even rename things if we want to!

This API might not be as simple as it could be, but it is as capable a module
system as I usually need. In fact, in many ways it is better than the system
zepto has today, because this version has way less cruft and a better user
interface, at least in my opinion. We also return the function implementation
from the call to `import`, because we can get it for free<a href="#1"><sup>1</sup></a>
and it might be useful.

This concludes our little overview over the API. Let's get down to business!

## Here be dragons, but they're tame

## Recap & Outlook

## Fin

##### Footnotes

<span id="1">1.</span> This is an implementation detail, but as it pertains to
  the implementation of the API I felt like it should be mentioned.
