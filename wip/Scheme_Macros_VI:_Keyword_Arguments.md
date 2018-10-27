In the fifth installment of [my series on Scheme macros](/scheme-macros), we’re
going to examine how to implement keyword argument functions in a single macro.

Keyword arguments are a very prominent feature of many programming languages,
including Ruby, Python, Clojure, Julia, and OCaml (where they are called labeled
arguments). If you don’t know any of these, think of keyword arguments as a way
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

Let’s implement it in Scheme. A complete implementation can be found in zepto’s
[standard library](https://github.com/zepto-lang/zepto-stdlib/blob/master/keywords.zp).

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
Figure 1. It is a little less concise, but functionally equivalent. This makes
it simple to test and easy for our users to figure out!

Let’s try and figure how to implement this!

## An implementation

Today we’ll try to write a single, big macro. Most of this section—except a
brief aside for a helper function—will be about `defkeywords`, so buckle up!
Don’t forget to take breaks and think through all of the steps we take; don’t
be afraid to go back at any point if you lose track of anything at all.

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

First let’s capture the environment so that we can add bindings to it. We need
to do this because we will define a function that we build dynamically from the
info we got. As we’ve seen before, in zepto we do this using `with-environment`
and passing the constructed value into `eval`.

```
(define-syntax defkeywords
  (syntax-rules ()
    ((_ nargs kwargs body)
      (with-environment env
        ; definition goes here
      ))))
```
<div class="figure-label">Fig. 4: Capturing the environment.</div>

We’ll now do two things: we’ll need to generate the function from the
information we’re given, and into this weave the keyword handling somehow. Let’s
try and build a function first, and see what extra work is required to get to
the keyword arguments:

```
(define-syntax defkeywords
  (syntax-rules ()
    ((_ nargs kwargs body)
      (with-environment env
        (eval (macro-expand
         `(define
             ,(reduce (flip cons)
                      (cons 'args)
                      (reverse 'nargs))
           (begin
             ; what now?
             ,body
           )
         )
        ) env)
      ))))
```
<div class="figure-label">Fig. 5: A functions skeleton inside the macro.</div>

Okay, so now we have an empty function in our macro. We need to `eval` and
`macro-expand` it, and quasi-quote the body to use it as template text. At the
end of that function we’ll call the actual function body, so we can add that
already. But what in the world is that `reduce` expression?

Well, what we need to have is a dotted list with rest arguments. In plain
english, this means that if our function signature looked like
`(my-kw-fun a b)` before, we need to rewrite it to `(my-kw-fun a b . args)` to
catch all of the extra arguments that the keyword function caller might pass
into our function. To this end, we need to rewrite the list into a dotted
list. In zepto, you construct dotted list with one-valued `cons`, in this case
with `(cons 'args)`, which will give us this `( . args)` we’re after. Then,
we’re prepending the arguments in reverse order until we arrive at our original
list, but with a new element and a new type. This is all just a very fancy way
of saying there are multiple weird list types in zepto, and it’s sometimes a
little awkward to transform one list into another.

After we have that out of the way, let’s look at what we need to do. We now know
where we’ll get those arguments, namely in a list called `args`. They’re weird
and untreated though, so we’ll have to transform them a little bit. Let’s try
our hands at that, shall we? For the purposes of the next few figures, I’ll
pretend that our templated function is a top-level thing, so you can ignore all
of the cruft we already wrote:

```
`(define
    ,(reduce (flip cons)
             (cons 'args)
             (reverse 'nargs))
  (begin
    (with-environment env
      (let ((dict (apply make-hash args)))
        ; ... what do we do here?
      ))

    ,body
  )
)
```
<div class="figure-label">
  Fig. 6: More environment captures and a dictionary.
</div>

The next change is fairly straightforward as well: we’re capturing the
environment again—a different environment this time, inside of the function—,
anticipating the need to dynamically define something once more. Then we use a
neat little trick to create a dictionary from the pairs of names and values that
we’ve been passed. Remember, the rest arguments will be given to us in this
format: `:key value`. This means that we can take all of them and pass them into
`make-hash` as is, which will create a dictionary from the values it’s been
given, grouping them up into pairs of two. Our keys will be the variable names,
and the values will be, well, the values.

That’s great! Now we just have to define them:

```
`(define
    ,(reduce (flip cons)
             (cons 'args)
             (reverse 'nargs))
  (begin
    (with-environment env
      (let ((dict (apply make-hash args)))
         (map
           ($ (let* ((k (car %))
                     (s (atom->symbol k)))
             (eval `(define ,s
                       ,(if (in? dict k)
                         (dict k)
                         (eval (get-from % 1)))) env)))
            (quote ,(treat-keywords 'kwargs)))))

    ,body
  )
)
```
<div class="figure-label">Fig. 7: Wat.</div>

By Mimir’s beard! What did we do there?

To be fair, it has been a while since I wrote that macro and when I looked at
that again, I wanted to kick my old self in the butt for not writing any
comments—one of my nastiest code habits. I was eventually able to decypher it,
however, and I will help you decypher it too!

We `map` over the list of keyword arguments we’ve been given—except we’re using
a little helper called `treat-keywords` to make the keyword argument format
uniform—we’ll get to that function in a second. These will be pairs of names and
their default values (which default to `nil`).

For each of these arguments, we’ll get the key `k` and transform it into a
symbol `s`. We’ll then `define` that symbol to be either whatever we find in the
dictionary, which will be the values we’ve been passed, or the default value
we’ve defined in the header.

This is actually all we need to do. At this point we’ll just let the body of the
function run, and everything will be in place as we need it to be. Awesome!

For completeness’ sake, let’s look at `treat-keywords`:

```
(defne (treat-keywords args)
  (case (length args)
    ((0) [])
    ((1) `((,(car args) nil)))
    (else
      (let ((key  (car args))
            (meta (cadr args)))
        (if (eq? :default meta)
          (++ `((,key ,(caddr args)))
              (treat-keywords (cdddr args)))
          (++ `((,key ,nil))
              (treat-keywords (cdr args))))))))
```
<div class="figure-label">Fig. 8: Pure plumbing.</div>

All `treat-keywords` does is split a list into a list of pairs of names and
their defaults, or `nil` if none was given. The first two cases in the `case`
form are base cases, the last one is a recursive case that checks whether the
`:default` key is there and either uses the next argument or `nil` as the
default. No black magic!

While there is a whole lot to take in here, we’ve just defined keyword arguments
in just under 40 lines of code. Isn’t that pretty awesome? Yay us!

## Notes

TODO

## Fin

TODO
