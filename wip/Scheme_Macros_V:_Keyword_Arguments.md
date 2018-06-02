In the fifth installment of [my series on Scheme macros](/scheme-macros), we’re
going to examine how to implement keyword argument functions in a single macro.

Keyword arguments are a very prominent feature of many programming languages,
including Ruby, Python, Clojure, Julia, and OCaml (where they are called labeled
arguments. If you don’t know any of these, think of keyword arguments as a way
to name arguments on the call site, and optionally provide defaults for them
should the caller not specify them. Let me illustrate it using Python:

```
def my_kw_fun(a, b, c=0):
  return a + b * c

# a=10, b=3, and c defaults to 0
my_kw_fun(10, 3) # => 10

# a=3, b=10, and c=4
my_kw_fun(3, 10, c=4) # => 43
```
<div class="figure-label">Fig. 1: Using keyword arguments in Python.</div>

This is a very concise way to offer default values for some arguments, which I
often find very convenient and can result in much cleaner APIs. Of course it
can also be abused, as any powerful language feature can, but it more often than
not improves readability and clarity.

Let’s implement it in Scheme.

## An API

We already used the macro in a previous blog post in this series,
[Deconstructing Classes](/Scheme_Macros_IV:_Deconstructing_Classes.html). As a
quick refresher, let’s look at how its API looks in zepto:

```
(defkeywords (my-kw-fun a b) (c :default 0)
  (+ a (* b c)))

(my-kw-fun 10 3) ; => 10
(my-kw-fun 3 10 :c 4) ; => 43
```
<div class="figure-label">Fig. 2: Using keyword arguments in zepto.</div>

The code in Figure 2 above is basically equivalent to the Python version in
Figure 1. It is a little less concise, but functionally equivalent, so let’s
try and figure how to implement this!

## An implementation

As always, we’ll start with a simple macro skeleton.

```

(define-syntax defkeywords
  (syntax-rules ()
    ((_ nargs kwargs body)
      ; definition goes here
    )))
```
<div class="figure-label">Fig. 3: A skeleton for `defkeywords`.</div>

As we saw in Figure 2 above, the macro will take three arguments, `nargs`,
`kwargs`, and `body`. But what do we do with them?

TODO

## Caveats

TODO

## Fin

TODO
