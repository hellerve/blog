This is a continuation of [an earlier post](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html),
in which I talked about the power of Scheme macros in the context of modules.
Modules aren’t the only use case of macros, though, and so this time I want to
talk about generic functions as an another case study.

Generic functions are a milestone in any programming language. Most languages
use some kind of interface structure, but the naming is inconsistent. I’ve seen
the terms interfaces, protocols, traits, and type classes floating around.
zepto calls them protocols.

Let’s define protocols as a structure, define an API, and then go about
implementing them! As always, you can [get the code to follow
along](/assets/generics.zp).

## What are protocols, anyway?

In zepto, protocols are very similar to type classes in Haskell. They are any
number of functions that take an element of the type we want to define the
protocol for as a first argument and a predefined number of arguments after
that. A protocol is just the declaration of that contract, and an implementation
is an implementation of these functions for specific data types.

All we need to do is define two macros `defproto` and `defimpl` that do the
necessary plumbing to register a new protocol and a new implementation of that
protocol, respectively.

I suspect all of this was a bit abstract and hard to follow. In the next
section we'll define an API for the two macros and hopefully clear things up
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
   (cdr string:tail)
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
functions with a variable number of arguments, but we'll rectify that during
our implementation of the interface. For now we can just note that the API is
nice enough to be usable, but fairly suboptimal when compared to the utopian
version.

Now for an implementation. These are going to be the most advanced macros on
this blog yet, but they’re still among the simpler macros you might
encounter when programming Lisp.


## Implementation

We talked a lot about defining and implementing protocols above. If you squint,
you might see some parallels to defining and importing modules, which I talked
about [in a previous post](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html).
This connection admittedly might not be obvious, and it only occurred to me in
hindsight, but I’ll still use the connection to guide you through the
implementation.

To get started with registering and looking up generics we need some structure
to save them in. As in the previous post, hash maps come to the rescue. We will
need one of those in our implementation; I call it `*impls*`. `*impls*` will hold
the implementations of a given protocol.

## A data format

The data format I’m going to present right now might feel a little arbitrary;
this is largely to offer a reference for later, when you
want to retrace the traversals and manipulations we do in the macros. First,
a view of what `*impls*` looks like after we’ve defined and implemented the
`collection` protocol above:

```
#{"collection"
    [<function: string?>,
      #{"car" <function: string:head>,
        "cdr" <function: string:tail>,
        "null?" <lambda>}]}
```
<div class="figure-label">Fig. 3: `*impls*` after a defintion of the
first protocol.</div>

The structure is a hash map, holding the protocol names as keys and as values a
pair of the predicate and another hash map of the function names and their
implementation. I suggest not worrying too much about it for now and reading
on. The definitions of both `defproto` and `defimpl` will hopefully make much
more sense with this structure in mind.

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

This is all well and good, but what do we actually have to do inside that macro?
There are various ways to go about this, but the simple way I like most 
is defining a dispatch function that will ensure the contract
is kept—in this case, this only concerns the number of arguments—and then
dispatches the appropriate function. This means we will have to define a number
of functions in the macro. Using a pattern discussed in the first post
in this series, we'll capture the environment and then map over the given functions.
After that, we can register them in our hash map.

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
        (hash:set! *protocols* (symbol->string 'name)
                   'functions)))))
```
<div class="figure-label">Fig. 4: An extended skeleton, almost useful.</div>

We're almost there! The only part missing is what’s actually crucial. Now
we need to take care of the scaffolding function. What will it do? We want it to
look up the implementations we have and find the one that is appropriate in our
case. We will also need to check whether the number of arguments is actually
correct. Why do we have to do this manually instead of relying on function
arity? The reason is two-fold: it simplifies the scaffolding function and it
will allow us to extend the function to work with overloaded/variable argument
functions.

Let’s define this mythical scaffolding function. It will be quasi-quoted to
enable us to inject information.

```
`(define ,fun (lambda args
  (let ((impls (*impls* ,fun-name))
        (type (car args)))
    (if (eq? (length args) fun-nargs)
      (let ((funs (filter
                    (lambda (v) (eval (list (car v) 'arg)))
                    impls)))
        (apply ((cadr funs) ,fun-str) args))
      (error ,fun-str "takes" ,fun-nargs
             "arguments, was given" (length args))))))
```
<div class="figure-label">Fig. 5: The scaffolding function.</div>

There are a few variables in there whose definition I’ve omitted. If you want
to see where they’re defined, I invite you to study the complete implementation
of generics linked to at the top of the page. Here is a quick list to get you
up to speed:

* `name-str`: the stringified name of the protocol.
* `fun-name`: the stringified name of the function.
* `fun-nargs`: the expected number of arguments as specified by the user.

Now, what does this code actually do? We’re defining a function that takes a
variable number of arguments, checks whether the number of those is equal to
what is expected in the contract, and if not, throws an error. If it is,
it filters the list of function implementations for the ones whose predicate
matches—this is the second argument to `defimpl`, `string?` in Figure 2—,
and takes the first of these functions, calling it with the arguments.
Quite dense, but all of the setup we need.

Now, optionally, we can define a variable number of arguments. All we need to
do is define an alternative to the number of arguments—for the sake of this
article I’ve chosen the atom<sup><a href="#1">1</a></sup> `:varargs`—that we
insert where we would normally specify the number of arguments—writing e.g.
`(car :varargs)` instead. Then we just need to check for this atom in our
`if` at the beginning of the scaffolding function.

```
`(define ,fun (lambda args
  (let ((impls (*impls* ,fun-name))
        (type (car args)))
    (if (or (eq? (length args) fun-nargs)
            (eq? :varargs fun-nargs))
      (let ((funs (filter
                    (lambda (v) (eval (list (car v) 'arg)))
                    impls)))
        (apply ((cadr funs) ,fun-str) args))
      (error ,fun-str "takes" ,fun-nargs
             "arguments, was given" (length args))))))
```
<div class="figure-label">Fig. 6: We support variable arguments now!</div>

This actually concludes our definition of `defproto`. We can now relax a little
more with a definition of `defimpl`.

## The implementation of implementations

All the `defimpl` macro has to do is to put the functions we register into the
`*impls*` macro in such a manner that our scaffolding function finds it. Let’s
define a skeleton again and then start to fill in the blanks.

```
(define-syntax defimpl
  (syntax-rules ()
    ((_ name pred funs)
      ; housekeeping goes here
    )))
```
<div class="figure-label">Fig. 7: A `defimpl` skeleton.</div>

The `defimpl` macro takes a protocol name, a predicate function, and the
function implementations we need to provide to fulfill the protocol contract.

All we really have to do here is to look up the protocol and register the new
implementation. In order to keep this implementation simple we omit all error
checking. The [reference implementation for zepto](https://github.com/zepto-lang/zepto-stdlib/blob/master/zpgenerics.zp)
comes with all kinds of checks that ensure we respect the contract.

Let's fill in the blanks with some lookup and an update skeleton:

```
(define-syntax defimpl
  (syntax-rules ()
    ((_ name pred funs)
      (let* ((name-str (symbol->string 'name)))
        (hash:update! *impls* name-str ; update function
          )))))
```
<div class="figure-label">Fig. 8: A complete `defimpl` skeleton.</div>

Here we just convert the name to a string and hand it over to `hash:update!`,
which implements destructive assignment of a hash map’s key-value pair.
`hash:update!` takes a hash map, a key and a function that will be handed the
old value—or `nil` if the key doesn’t exists—and should return the new value.

Equipped with that knowledge, updating the list is as simple as making sure
we replace `nil` with an empty list and append the new implementation to it.

```
(define-syntax defimpl
  (syntax-rules ()
    ((_ name pred funs)
      (let* ((name-str (symbol->string 'name)))
          (hash:update! *impls* name-str
            (lambda (impl)
              (let ((impl (if (truthy? impl) impl [])))
                (++ impl (list pred (make-hash nfuncs))))))))))
```
<div class="figure-label">Fig. 9: The finished version of `defimpl`.</div>

The new implementation is a pair of the predicate, by which we will filter the
list of implementations when we look up the function to execute in `defproto`,
and a hash maps of the functions and names. That’s all!

## Wrapping up

This is a capable, but very brittle sketch of a generics system. It has all
the moving parts. But, as mentioned before, we don’t actually check whether our
protocols and their implementations make sense. This is unacceptable in the
real world, but it’s also a very simple issue to fix if you want to build
upwards from here.

This was the second post in my little series on how we can utilize the power
of Scheme macros. If you would like me to continue this series and have any
requests for future posts, I invite you to open a [reminder issue on
Github](https://github.com/hellerve/blog/issues). Take care and see you very
soon!

##### Footnotes

<span id="1">1:</span> Atoms are equivalent to keywords in Common Lisp and
Clojure.
