This is a continuation of [an earlier post](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html),
in which I talked about the power of Scheme macros in the context of modules.
Modules aren’t the only use case of macros, though, and so this time I want to
talk about generic functions as an another case study.

Generic functions are a milestone in any programming language. Most languages
use some kind of interface structure, but the naming is incosistent. I’ve seen
the terms interfaces, protocols, traits, and type classes floating around.
zepto calls them protocols.

Let’s define protocols as a structure, define an API, and then go about
implementing them! As always, you can [get the code to follow
along](/assets/generics.zp).

## What are proctols, anyway?

In zepto, protocols are very similar to type classes in Haskell. They are any
number of functions that take an element of the type we want to define the
protocol for as a first argument and a predefined number of arguments after
that. A protocol is just the declaration of that contract, and an implementation
is an implementation of these functions for a specific data types.

All we need to do is define two macros `defproto` and `defimpl` that do the
necessary plumbing to register a new protocol and a new implementation of that
protocol, respectively.

I suspect that all of this was a bit abstract and hard to follow. In the next
section we will define an API for the two macros that hopefully clear things up
a little.

## An API

Let’s start with the API of `defproto`. It will take a protocol name, a number
of functions, and their respective arguments. As an illustrating example, let’s
define a collection protocol that defines the functions `car`, `cdr`, and `null?`.

```
(defproto collection
  (car 1)
  (cdr 1)
  (null? 1))
```
<div class="figure-label">Fig. 1: A simple collection protocol.</div>

As we can see above, all of the functions take exactly one argument. The form
for defining functions is a list of pairs. Let’s keep this in the back of our
heads for now and move on to an implementation of that protocol for strings.

```
(defimpl collection string?
  ((car string:head)
   (cdr string:last)
   (null? (lambda (str) (eq? 0 (string:length str))))))
```
<div class="figure-label">Fig. 2: An implementation of the collection protocol
for strings.</div>

The form for implementing a protocol looks fairly similar to defining a
protocol. It takes three arguments: the name of the protocol, a function that
returns true if the input matches our expectations—most commonly a typechecking
function, but it can be arbitrary, more on that later—, and a list of function
names and their implementations. The implementations can be any flavor of
callable, from a lambda that’s defined inside the form to a symbol that
references any kind of function anywhere else.

This API makes for a relatively slick interface, although it is not necessarily
feature-complete. Type safety is not really a thing in this API, although Lisps
are generally not known for that anyway. It also doesn’t allow us to define
functions with a variable number of arguments, but we will rectify that during
our implementation of the interface. For now we just observe that the API is
nice enough to be usable, but fairly suboptimal when compared to the utopian
version.

Now for an implementation. These are going to be the most advanced macros on
this blog yet, but they’re still among the simpler macros that you might
encounter when programming Lisp.

## Implementation

We talked about defining and implementing protocols a lot above. If you squint,
you might see some parallels to defining and importing modules, which I talked
about [in a previous post](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html).
This connection might admittedly not be obvious, and it only occurred to me in
hindsight, but I’ll still use the connection to guide you through the
implementation.

To get started with registering and looking up generics we need some structure
to save them in. As in the previous post, hash maps come to the rescue. We will
need two of those in our implementation, I call them `*protocols*` and `*impls*`.
`*protocols*` will hold the definitions of protocols, while `*impls*` will hold
the implementations. This means we will have to duplicate some structure, but it
also makes the structure of the individual hash maps much simpler.

`defproto` is actually the more complex macro of the two, but we will start with
it, because without it defining ìmplementations is useless, and useless code
shouldn’t be what we strive for.

## Defining protocols

As always, let’s start with a simple skeleton. We need a macro `defproto` that
takes a name and a number of functions.

```
(define-syntax defproto
  (syntax-rules ()
    ((defproto name functions ...)
      ; define the protocol
      )))
```
<div class="figure-label">Fig. 3: A skeleton for defining protocols.</div>

This is well and good, but what do we actually have to do inside that macro?
There are various ways to go about this, but the way I like most because of its
simplicity is defining a dispatch function that will ensure the contract
is kept—in this case, this only concerns the number of arguments—and then
dispatches the appropriate function. This means we will have to define a number
of functions in the macro. Using a pattern that we discussed in the first post
in this series, we will capture the environment and then map over the functions
were given. After we’ve done that, we can register them in our hash map.

Let’s try to extend our skeleton with what we know.

```
(define-syntax defproto
  (syntax-rules ()
    ((defproto name functions ...)
      (let ((env (current-env)))
        (map
          (lambda (fun)
            (eval
              ; do something here
              env)))
          'functions)
        (hash:set! *protocols* (symbol->string 'name) 'functions)))))
```
<div class="figure-label">Fig. 4: An extended skeleton, almost useful.</div>

We almost got it! The only part that’s missing is the actually crucial part.
