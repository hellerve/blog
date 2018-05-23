It’s time for another installation of my series on Scheme macros. Previously,
we've talked about defining [modules](//blog.veitheller.de/Scheme_Macros_I:_Modules.html),
[generic functions](//blog.veitheller.de/Scheme_Macros_II:_Generics.html),
and [local variable binding](//blog.veitheller.de/Scheme_Macros_III:_Defining_let.html)
using macros. This time I want to write about classes and how we can define an
interface for object-oriented programming. We will be going down a similar route
as we did with both modules and generic functions, and if you read those posts,
the definitions we explore here might come naturally to you. As always, the code
is online, this time in form of [a zepto package](https://github.com/hellerve/bach)
that I wrote a few years ago.

As usual, we will start by defining an API, then slowly walk through one
possible way of implementing it before wrapping up and concluding with caveats
and possible extensions. Are you excited? I’m excited!

## An API for OOP

We’re going to implement a single-inheritance system, more similar to Smalltalk
than to Java. Let’s sketch out an API for defining classes. As always, we’re
going to do so in my own little Lisp called [zepto](https://github.com/zepto-lang/zepto).
This time we’re going to rely on some of the metaprogramming APIs of the
language, and a concept that I call atoms. Called `keywords` in some other
Lisps, they are symbols prefixed with a colon, like `:this`. They always
evaluate to themselves. I will cover why they are useful in another blog post,
for now, you know what they are.

```
(class MyClass
  (properties
    :mykey
    (:myotherkey :default 0))

  (functions
    (->string (lambda (self)
      (++ "<A Person: " (->string (Person:get-mykey)) ">)))))
```
<div class="figure-label">Fig. 1: Defining a simple class.</div>

Okay, what’s going on here? We define a class, give it the name `MyClass`, and
then separate properties from functions. Properties can optionally have
defaults, and functions take a reference to `self`<sup><a href="#1">1</a></sup>.

What functions get generated from this definition?

```
; we’re able to define instances
(define instance (MyClass :mykey "foo"))

; we can also get and set properties
(MyClass:get-mykey instance) ; => "foo"
(MyClass:set-mykey instance "bar") ; => MyClass

; we can call the functions we defined
(MyClass:->string instance) ; => "<A Person foo>"

; we can check whether somehing is an instance of the class
(MyClass? instance) ; => true

; we can get all the properties associated with a class
(MyClass:get-properties) ; => [:mykey, :myotherkey]
```
<div class="figure-label">Fig. 2: A whole lot of generated functions.</div>

Wow, that’s a lot of generated code. Some of them are necessary to make classes
useful, and some are just nice to have in there and interesting to implement.

So far, all we have implemented is a kind of typed hashmap with generated
accessors—which is sufficient for some languages to be object-oriented. We also
want to have inheritance, though, because that makes the whole implementation
more fun and interesting.

```
; the first argument is the parent class
(inherits MyClass MyOtherClass)
```
<div class="figure-label">Fig. 3: An interface for inheritance.</div>

We will limit ourselves to single inheritance for two reasons. Firstly, I like
it better that way. More importantly, though, it avoids a discussion we
otherwise would need to have about how to best resolve inheritance order. There
are different ways to go up the inheritance chains, and [some of
them](https://www.python.org/download/releases/2.3/mro/) are quite interesting.
It is, however, a discussion I’d like to avoid for the purposes of keeping this
blog post short and crisp.

We will also only inherit functions. In the scheme we are implementing, this is
a little simpler, but can also easily lead to bugs. If you want to work on this
some more, I have some pointers for you at the end. The whole thing feels a bit
more like prototypes than classes, really, but none of that is not fixable.

Anyway, let’s try to write a little bit of code, shall we?

## Implementing classes

We’re going to do something simple but sloppy by defining all functions and
classes directly in the environment instead of keeping track of our objects
in another data structure. We will talk about this tradeoff a little more when
wrapping up.

### Implementing inheritance

I’m saying all of this because we’re going to start by implementing the
simpler part of our API: inheriting. As always, let’s start with a skeleton
macro.

```
(define-syntax inherits
  (syntax-rules ()
    ((_ parent child)
      ; do something
      )))
```
<div class="figure-label">Fig. 4: A skeleton for inheritance.</div>

Okay, so we are getting the parent first, then the child. At this point, both
of them have already been defined. We will thus reach into the environment and
pull out all of the functions associated with both classes. This is where zepto
specifics come into play, because we will be using the functions
`with-environment`, `env->hashmap`, and `hash:keys`. All of those are fairly
straightforward, and I’ll talk about them a little bit when we discuss the
implementation

```
(define-syntax inherits
  (syntax-rules ()
    ((_ parent child)
      (with-environment env
        (let* ((funs (env->hashmap env))
               (names (hash:keys funs))
               (filter-names
                  (lambda (name)
                    (filter ($ (string:starts-with %
                                  (++ (->string name) ":")))
                            names)))
               (parent-funs (filter-names 'parent))
               (child-funs (filter-names 'child)))
          ; do something
        ))))
```
<div class="figure-label">Fig. 5: An extended skeleton for inheritance.</div>

Okay, this is a little weird, but I promise it is not as scary as it seems at
first. First, we use `with-environemnt` to bind the current interpreter
environment to a name called `env`. We then transform this environment into a
hashmap where the keys are the names and the values are the objects bound to
those names, and give it the name funs. We only need the names, so we get all
the hash keys using `hash:keys`. Then we define a filter function called
`filter-names` that reaches into those names and filters them by prefix. I
should at this point probably explain the weird `($ ... % ...)` syntax: this is
just a shorthand for `(lambda (%) ...)` to save typing.

When we’re done with all that, we are ready to filter the environment for
anything that starts with the name of the parent and a colon and the name of the
child and a colon. We assume these to be the parent and child functions.<sup><a href="#2">2</a></sup>

Okay, so now we have the parent and child functions. What do we do with them?
We call `map` on them, of course. That usually solves our problems. Let’s
write a mapping skeleton and then think about what we could actually do to
make these functions work.

```
(define-syntax inherits
  (syntax-rules ()
    ((_ parent child)
      (with-environment env
        (let* ; our bindings
              ; ...
          (map (lambda (parent-fun) ...)  parent-funs))))))
```
<div class="figure-label">Fig. 6: Mapping over functions.</div>

Okay, this looks reasonable. We map over the parent functions, because we need
to inherit those. But what do we need to do? First, we need to find out the
new name the function should have. Maybe we can just use string substitution?

```
(define-syntax inherits
  (syntax-rules ()
    ((_ parent child)
      (with-environment env
        (let* ; our bindings
              ; ...
          (map (lambda (parent-fun)
                  (let ((nfun (string:substitute parent-fun
                                          (->string 'parent)
                                          (->string 'child))))
                  ; ...
                  )
               parent-funs))))))
```
<div class="figure-label">
  Fig. 7: Mapping over functions II: Electric Boogaloo.
</div>

Alright, this looks about yanky enough to be correct. Now we need to check
whether we already have a function of that name in the class, and define the
new function otherwise.

```
(define-syntax inherits
  (syntax-rules ()
    ((_ parent child)
      (with-environment env
        (let* ; our bindings
              ; ...
          (map (lambda (parent-fun)
                  (let ; inner bindings...
                    (unless (in? child-funs nfun)
                      (eval `(define ,(string->symbol nfun)
                                     ,(funs parent-fun))
                              env))))
               parent-funs))))))
```
<div class="figure-label">Fig. 8: `eval` your way to freedom.</div>

Don’t you just love the smell of `eval` in the morning? In this case we use it
to define the new function in the environment we started at (the one we obtained
using `env`). If we didn’t use that environment, this `define` would be local
to the lambda we execute it in, and basically useless. Important side note:
remember that `funs` is the environment as a hashmap here. We can reach into
that hashmap by calling it with a key, like so: `(hash key) ; => val`. We use
this to get the actual function we are looking at from the name<sup><a href="#3">3</a></sup>.

Okay, so what are we doing, from start to finish? We reach into the environment
and pick out all of the functions of parent and child. Then we go through the
functions of the parent, rename them for the child, and if they are not defined
in the child, we defined them using a templated `eval`.

This approach is highly flawed, and I will talk a bit about why and how
in the conclusion, but for now we can feel pretty good about ourselves: we
basically implemented inheritance!

### Implementing `class`

Implementing the `class` form will be much more work, but in many ways it will
be simpler, so don't despair at the walls of code I’m about to throw at
you! You might want to take a little breather before continuing, though, for I
also took one before writing this part. There’s a lot of ground to cover, and
you might want to stretch your legs a little first.

As before, we start with a simple skeleton to break the ice. The `class` macro
takes a name and a number of forms.

```
(define-syntax class
  (syntax-rules (properties functions)
    ((_ name (properties props ...) (functions funs ...))
      ; do something
      )))
```
<div class="figure-label">Fig. 9: A skeleton for `class`.</div>

Okay, that doesn’t look too bad. So what do we do with these values now?
Basically, we “just” have to define a few templates in which to insert the names
and properties and then define the functions bound to the class we are looking
at. That means we have to parse the `properties` and `functions` variables a
bit<sup><a href="#4">4</a></sup>.

Let’s go through those function templates one by one. All of these individual
functions will be simple, I promise. All of the complexity will come from the
composition of those building blocks.

#### Typechecking and getting properties

Let’s begin by defining two simple functions, the function that checks whether
an object is an instance of the class we’re defining, and a function that
returns the properties of the class.

```
(define-syntax class
  (syntax-rules (properties functions)
    ((_ name (properties props ...) (functions funs ...))
      (with-environment env
        (begin

          ; the typechecking function
          (eval `(define (,(->symbol name "?") obj)
                  (and (hash? obj)
                        (eq? (hash:keys obj) (quote ,'props))))
                 env)

          ; get-properties
          (eval `(define (,(->symbol name ":get-properties"))
                  (quote ,'props)) env)

          ; ... to be continued
          )))))
```
<div class="figure-label">
  Fig. 10: Defining the first functions on our object.
</div>

As before, we get the environment that we start out with, so that we can extend
it. Then we begin evaluating templates. The name of the typechecking function
will be the name of the class plus a question mark. It takes one argument and
checks whether it is a hashmap and the keys are equal to the properties we
received. This is a little primitive, but very simple.

`get-properties` itself just returns the list of properties. Very simple, right?

#### Getting and setting properties

I think now we are ready to define our getters and setters.

```
(define-syntax class
  (syntax-rules (properties functions)
    ((_ name (properties props ...) (functions funs ...))
      (with-environment env
        (begin

          ; ... type checking and get-properties

          ; the setters
          (map ($ (let ((% (if (list? %) (car %) %)))
                (eval
                    `(define (,(string->symbol
                        (++ (->string 'name) ":get-"
                            (->string (atom->symbol %)))) self)
                                 (self ,%)) env)))
               'props)

          ; the getters
          (map ($ (let ((% (if (list? %) (car %) %)))
                (eval
                    `(define (,(string->symbol
                        (++ (->string 'name) ":set-"
                            (->string (atom->symbol %)))) self val)
                      (hash:set self ,% val)) env)))
               'props)

          ; to be continued
          )))))
```
<div class="figure-label">Fig. 11: Getting and setting properties.</div>

This is a little more involved, isn’t it? The good news is that they’re almost
identical. The bad news is that even one of these forms is kind of complex.
Let’s walk through the getters first.

We map over the properties that we defined, because we have to create a getter
for each of them. First, we check what form we have in front of us. If it’s
a list, we assume that it’s a form with default value and take the first
argument. Otherwise we just take the symbol as is.

Then we enter a templated `eval` again. We stitch together a name from the
type and property, and a body that will just look up the value in the hashmap.

The only thing that changes in the setter is that, in the body, we set the
value in the hashmap rather than getting it.

Operationally, all of this is quite straightforward: we just wrap hashmap
accessors. Of course all of it is a little complicated because we dynamically
create these functions, but the fact remains that the core of our functionality
is very slim.

#### Instance functions

So, what’s missing? We have to define the initializer and the user-provided
functions. Let’s start with the simpler part, the functions that the user
defined.

```
(define-syntax class
  (syntax-rules (properties functions)
    ((_ name (properties props ...) (functions funs ...))
      (with-environment env
        (begin
          ; a whole lot of functions

          ; defining user functions
          (map ($
                (eval `(define
                        ,(string->symbol (++ (->string 'name) ":"
                                             (->string (car %))))
                         ,(cadr %))
                        env))
               'funs)
          ; to be continued
          )))))
```
<div class="figure-label">Fig. 12: Creating user-defined functions.</div>

This is very similar to what we did with getters and setters. We map over the
functions, stitch together a name, and bind the function to it as is. And that’s
all we have to do for this part of the definition.

#### The initializer

Now all that is left for us to do is create an initializer. We’re going to make
this easy on ourselves and reuse another macro named `defkeywords`. I will talk about
the implementation of this macro in another installment of this series; for now
I’ll just give you a little tutorial on how to use it, and then we will see how
we can use it to implement a simple initializer.

```
(defkeywords (mykeywordfn myregulararg) (:mykeywordarg default 0)
  (+ myregulararg mykeywordarg))
```
<div class="figure-label">Fig. 13: An example usage of `defkeywords`.</div>

In a nutshell, `defkeywords` adds another form to definitions that define optional 
arguments and their defaults. This is a very useful form in general, but you might have 
realized that it also is very similar to the form we use to define properties. We can use 
that to make the initializer implementation extremely simple.

```
(define-syntax class
  (syntax-rules (properties functions)
    ((_ name (properties props ...) (functions funs ...))
      (with-environment env
        (begin

          ; all of our other functions ...

          ; generating our initializer
          (eval
            (macro-expand
              (list 'defkeywords (list 'name)
                                 (list:flatten 'props)
                  (cons 'make-hash
                      (list:flatten
                        (map ($
                          (if (list? %)
                            (list (car %)
                                  (atom->symbol (car %)))
                            (list % (atom->symbol %))))
                        'props)))))
            env))))))
```
<div class="figure-label">
  Fig. 14: Using `defkeywords` for our initializer.
</div>

This form, too, follows the general form of evaluating a template. But because
`defkeyword` is a macro, we also manually have to call `macro-expand` in zepto.
But what actually are we expanding and evaluating?

What we want to end up with is a definition using `defkeywords` named after the
class, with no regular arguments, and all of the properties as keyword
arguments. This is what we do in Figure 14 above. The only work that we have to
do to get to this point—other than concatenating the whole shebang—is flattening
the properties list.

The body of the function should just create hashmap from the given properties.
For this we use the function `make-hash`. For the arguments we map over the
properties once more and make key-value pairs, from the atoms that the macro
was passed to the symbols that end up being defined in the function body.

This is a little arcane, so let’s look at one example expansion:

```
(class MyClass
  (properties
    :mykey
    (:myotherkey :default 0))
  ; ...
)

; the initializer expands to:

(defkeywords (MyClass) (:mykey
                        :myotherkey :default 0)
  (make-hash (list :mykey mykey) (list :myotherkey myotherkey)))
```
<div class="figure-label">Fig. 15: Expanding the initializer by hand.</div>

This should help clear things up a little.

This concludes our implementation! Let’s think a little bit about whether it is
any good and how you could improve it if you felt so inclined!

## Caveats

I alluded to multiple weaknesses in the class implementation we just built. Now
it's time to review them, and to think about how to solve them. If this post
excited you, I encourage you to try and come up with possible solutions for
these problems; I’m happy to help you solve them if you shoot me a message!

Here is an unabridged list fit for crushing hopes and dreams:

- We’re not inheriting properties. This is both easily solvable and very bad,
  because every time a superclass references one of its own properties, we will
  have a bad time. You could rewrite the constructor using `get-properties` of
  both the parent and the child when inheriting. Don’t forget to rewrite
  `get-properties` itself too!
- We can’t actually use any functions of the superclass that we overwrote. There
  is no runtime resolution order, just flat functions operating on glorified
  hashmaps. This could be solved using a class registry (could simply be
  another global hashmap).
- While we’re on the topic of a class registry, let’s think about how we looked
  up the functions when inheriting. We just pulled out functions that fit a
  naming scheme. Anyone could inject functions into our unsuspecting environment
  that also fit this name. A class registry could fix this too, by making sure
  no extraneous functions end up in our class definitions.
- The type checking primitive is both too simple—which can be solved, again,
  with a class registry—and buggy. It doesn’t work with default values, because
  we do not strip them out of the `props` value that we receive in the macro.
  `get-properties` suffers from the same bug.
- For the sake of brevity, we do no error checking whatsoever. What if we put
  in numbers instead of symbols, or variables instead of function bodies? A
  mature system should check for that and make sure that the user gets
  actionable error messages.

None of these problems is unsolvable. They might require a decent amount of
work, but it’s worth reminding yourself that the system you are starting with is
less than 50 lines of code and is doing a whole lot of things for us already.

## Conclusion

Two years ago, while working on zepto, I asked myself how CLOS worked. Instead of
looking at the source right away, however, I tried implementing my own little
class system, and then compared it to CLOS. Of course my system ended up being
orders of magnitude more primitive and clunky, but it was a fun little exercise
and taught me more about object-oriented programming than that dreaded third
semester in college when I had to implement design patterns in Java.

It also was an excuse for me to dive deeper into how a better function
templating system could work. Above we mostly just interpolated `define` forms
and pushed them into `eval`. This could very simply be abstracted into a neater
API that better expresses intent without having to wade through all of
the boilerplate. Dynamically generating functions is fun, but maybe next time
we’ll learn how we can have the cake and eat it, too.

I hope you got as much out of reading this as I got out of writing it! See you
very soon!

#### Footnotes
<span id="1">1.</span> `self` is alternatively called `this` in other languages.

<span id="2">2.</span> This is not necessarily true. We could easily generate
                       another function that fits this naming scheme, but
                       doesn’t actually belong to the class. If we want
                       to avoid this bug, we need to keep track of the classes
                       in another data structure. See [my blog post on
                       implementing generics](//blog.veitheller.de/Scheme_Macros_II:_Generics.html)
                       for one possible method using a hashmap.

<span id="3">3.</span> Unquoting `parent-fun` would have a similar effect, I
                       just want to make sure we are not using an accidently
                       shadowed binding. Unlikely, but possible.

<span id="4">4.</span> For those of you who aren’t as familiar with reserved
                       words in `syntax-rules`, let me give you a brief intro:
                       the first argument to `syntax-rules` is an optional list
                       of reserved words that you can treat as literals in the
                       pattern matching head. This makes it easier to define
                       more complicated control structures, and is perfect for
                       our use case. For more information I suggest you look at
                       subchapter 3.3 of [this wiki
                       page](http://www.shido.info/lisp/scheme_syntax_e.html).
