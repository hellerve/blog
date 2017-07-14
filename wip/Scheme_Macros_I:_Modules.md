One of the most powerful—and, for a large part of the programmer community,
most dreaded—features of Lisp is its metaprogramming. In this series I'll
attempt to give a few interesting cases for how we can level up our code
through the use of macros.

This time we will look at how to implement a module system. Many Lisps already
come with a module system out of the box—both <a href="http://trac.sacrideo.us/wg/wiki/ModuleSystems">R7RS</a>
and <a href="http://weitz.de/packages.html">Common Lisp</a> provide a
facility for packaging and namespacing libraries and modules—, but it is still
helpful to know how these abstractions are built and how they look on the inside.

The module system we're about to implement is a stripped-down, simplified
version of the actual [zepto module system](https://github.com/zepto-lang/module).
The complete code used in this post can be downloaded [here](/assets/modules.zp).

This post assumes a certain familiarity with the `define-syntax` Scheme form.

## An API

As per usual in my tutorial-style blog posts, we're going to go the route of
first defining an API and then trying to implement it. Let's look at a simple
module and its defining syntax.

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

There are many ways to represent modules in your language. The single most
intuitive way to implement them, however, is storing them as a nested hash
map in my opinion. The outer keys are the module names, whose values are
inner hashmaps of function names to functions.

If we compile the module from Figure 1 manually, we might—in JSON-like
notation—end up with something like this:

```
{
  "math": {
    "add": /* some function representation */,
    "sub": /* some function representation */,
    "mul": /* some function representation */
   }
}
```
<div class="figure-label">Fig. 3: A representation of a module.</div>

Something interesting is going on here already, because the function `m`
has already been renamed to `mul`. Why is that? We can be sure that we
never refer to it by its old name, because in a perfect world, the module
user wouldn't even know that it was aliased. They'd just see that there is a
function called `mul` in the module.

In zepto, the module system is just one big hashmap called `*modules*` that
we operate on. This gives us no thread safety—although I hope importing
modules will never lead to race conditions—, but enables us to write our own
module system easily if we have to. It also leaves the possibility of
monkey-patching open to the user, although I was never a big fan of that
concept. Ruby users might disagree.

## Defining a module

How do we go from the module definition in Figure 1 to an entry in our
hash map, then? Macros, of course. Let's define a macro `defmodule` that takes
in the definition above and just evaluates the module body in a new scope.

```
(define-syntax defmodule
  (syntax-rules (export)
    ((module name (export exports ...) body ...)
      (let ()
        body ...))))
```
<div class="figure-label">Fig. 4: A module system, well almost.</div>

The `body ...` form will expand to the module body, meaning that we evaluate
it in a `let` form that binds no new variables. This is a short trick to define
a new scope and assures that the functions don't leak. If they did, we wouldn't
define much of a module system.

All we need now is to take the exported forms and put them in
the hash maps. Their closures will persist, meaning that the non-exported
functions and variables will live on without leaking out of their defined
scope.

Let's try to make a skeleton of how we would set the values in the hash map,
using zepto's `hash:set!` function which updates a hash map's key-value pair
destructively.

```
(define-syntax defmodule
  (syntax-rules (export)
    ((module name (export exports ...) body ...)
      (let ()
        body ...
        (hash:set! *modules* (->string 'name)
          (make-hash
              ; produce values here
        ))))))
```
<div class="figure-label">Fig. 5: An update skeleton.</div>

The function `hash:set!` takes three arguments: the hash map, the key—here 
a string representation of the module name–, and a value. In this case, we
know that the value will be a hash map, so I already put the `make-hash`
function in the skeleton. `make-hash` in zepto can also operate on a list of
key-value pairs, which we will leverage. But how do we generate a list of
key-value pairs—in this case a list of names and functions?

In this macro, we already have a list of names that we want to export,
helpfully called `exports`. This means that all we have to do is a list
transformation, which sounds a lot like an application of `map`. Using `map`
and the zepto shorthand for unary lambdas `$`—it will bind the argument to
`%`—we arrive at the following transformation:

```
(map
  ($ (if (list? %)
       ; the aliasing case
       (list (->string (caddr %)) (eval (car %)))
       ; the regular case
       (list (->string %) (eval %))))
  '(exports ...)))
```
<div class="figure-label">Fig. 6: A convoluted list transformation</div>

This definition packs a punch, so let's walk through it slowly. An
illustrative example to make this clearer can be found in Figure 7. We go over
the list in export one by one. In each iteration of the mapping function, the
element will be bound to `%`, as mentioned before. If it is a list, we assume
that it is an aliased form—defined as `(foo :as bar)`—, which means that the
list will be a pair of the last element `(caddr %)` as a string and the
implementation of the original name, which we get by evaluating the symbol.
If `%` is not a list, however, we just stringify the symbol for naming it
and evaluate it to get the implementation. This might not make immediate sense,
and I arrived at it step by step, but this concludes our definition of
`defmodule`.

```
(exports add sub (m :as mul))
;; =>
'((add <function: add>) (sub <function: sub>) (mul <function: m>))
```
<div class="figure-label">Fig. 7: Example inputs and outputs.</div>

Well, almost. In Figure 1 we also said that we can require other source files,
which is not accounted for in this definition. For brevity, I won't
go into a step-by-step derivation of how to do this, but it is included in
the implementation that accompanies this post. You can also try to
implement it yourself, if you wish, in which case I will give you the following
hint to get started: if you want to load in a list of source files, you'll have
to capture the environment, and then map over the files and load them into that
environment. An implementation of that is given in lines 27–42 of the file I
link to at the beginning of this post.

## Importing

Now we come to the part that makes this system actually usable: importing.
Importing is simple: we retrieve the functions we stored earlier and bind them
to the appropriate name. There is a renaming import and a non-renaming import
in our API, let's define the renaming import first:

```
(define-syntax import
  (syntax-rules ()
    ((import name) ; non renaming
      (import name name))
    ((import name as) ; renaming
      ; here be dragons
    )))
```
<div class="figure-label">Fig. 8: We cheated.</div>

Isn't that cheating? We used a little trick to make the non-renaming case even
simpler, by defining it as the renaming case, “renaming” to the real name.
That's not very interesting, but it reduces code duplication by a lot.

What do we have to do for the renaming case then? If we look at the API for
`import` from Figure 2 again, we realize that there are basically two cases:
either we want to import everything from a module or a specific function.
We can distinguish between those two cases by checking whether the symbol
contains a colon, since this character is reserved as a separator between
module and function names<a href="#2"><sup>2</sup></a>.

Updating our skeleton to reflect our new insights, we end up with:

```
(define-syntax import
  (synax-rules ()
    ; the non-renaming case
    ((import name as)
      (let ((strname (if (symbol? 'name) 
                        (->string 'name)
                        name)))
        (if (in? strname #\:)
          ; import single function
          ; import module
        )))))
```
<div class="figure-label">Fig. 9: A more complete skeleton.</div>

This looks reasonable, but something important is missing. By using a `let`
expression, we introduce a new scope, which makes defining functions in the
`if` body useless, since it will not be available in the scope above. What we
need to do, thus, is capture the environment in the `let` variable capture.
zepto has a mechanism for getting the current environment, `current-env`. If
we bind it to a variable `env` and pass that on to `eval`, it will evaluate the
expression in the environment.

Equipped with our new knowledge, we can try to implement importing a single
function. We will need to split the module and function names, look them up in
the module map, and bind the function to the given name, like this:

```
(define-syntax import
  (syntax-rules ()
    ; the non-renaming case
    ((import name as)
      (let ((env (current-env))
            (strname (if (symbol? 'name)
                          (->string 'name)
                          name)))
        (if (in? strname #\:))
          (let* ((split (string:split strname #\:))
                 (_mod  (car fullname))
                 (fun   (cadr fullname)))
            (eval `(define ,(->symbol 'as)
                           ,((*modules* _mod) fun))
                  env))
          ; import module
        )))))
```
<div class="figure-label">Fig. 10: We're getting closer.</div>

In the above code, we're basically just piecing together information we already
have into a `define` form and evaluating it in the parent context. This is
indeed all we need for importing single functions. People who are not familiar
with zepto might be confused by the lookup function: hash maps and vectors are
callable and will return the value if given a key as argument, which is why we
are applying `_mod` and `fun` to the `*modules*` variable, which houses all of
our modules.

Importing a whole module is then just a matter of iterating over the contents
of the module, getting the namespacing right, and doing our little evaluation
trick to bind the functions. Let's try that as well.

```
(define-syntax import
  (syntax-rules ()
    ; the non-renaming case
    ((import name as)
      (let ((env (current-env))
            (strname (if (symbol? 'name)
                          (->string 'name)
                          name)))
        (if (in? strname #\:)
          ; import single function
          (hash:kv-map
            ($
              (eval `(define ,(string->symbol
                                (++ (->string 'as) ":"
                                    (->string (car %))))
                             ,(cadr %))
                    env))
            (*modules* strname)))))))
```
<div class="figure-label">Fig. 11: Importing, completed.</div>

This is again a tad wordy, but it basically does what was promised: it maps
over the key-value pairs in the module—using `hash:kv-map`—and for each
function stitches together a name from the key and the module name or alias,
then binds that name to the function—the hash map value. This uses zepto's
`$` shorthand for unary functions again.

And with this we are done. We have implemented the promised module system.
While some bits of the code are a bit gnarly, it's not overly complex. Most of
the time invested is spent on wiring that depends on the input data.

## Recap

This has been a wild ride, but we now have a working version of a module
system. It is relatively simple and limited, but serves as a good foundation
for something that caters better to your needs.

There are a few low-hanging fruit that you could work on to get started
making this the truly awesome module system your language deserves, or just as
an exercise.

* Try implementing loading required files yourself and see how your solution is
  different from mine.
* Currently loading and importing are two separate steps. Write a `require`
  form that unifies the two in a sensible API. Maybe readd that into the former
  exercise.
* Implement extending modules, i.e. being able to add functions to a module
  after it has been defined. An example for that can be found in the actual
  [zepto module system](https://github.com/zepto-lang/module).

All of these are definitely achievable, and most of them have been implemented
in zepto already, so you can search for solutions if you're stuck.

## Fin

Thank you for reading this far! This has been a fairly long blog post; I hope
it was worthwhile! As the name suggests, I'm planning to make these posts a
series. The next thing I want to cover is generic functions through
protocols—commonly known as interfaces or sometimes traits. If that sounds
appealing to you, be sure to check back soon! And if you have any other macro
ideas that you want me to write about, contact me.

##### Footnotes

<span id="1">1.</span> This is an implementation detail, but as it pertains to
  the implementation of the API I felt like it should be mentioned.

<span id="2">2.</span> This is not enforced by the implementation and purely
based on convention.

