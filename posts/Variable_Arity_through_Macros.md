First off I’d like to apologize for the scary title, but I didn’t have anything
snappy at hand.

What we’re going to look at today is how it is possible to add variable arity
to a statically compiled language without any runtime overhead. In our example
we’re going to do this for Carp in macros—because why wouldn’t we?—, but this
can be done easily and even more gracefully inside the compiler. Using macros
as our base doesn’t change the implementation details much and it makes for a
standalone exhibition of the technique without having to write a compiler from
scratch, though, so that’s a big plus for all of us!

Let’s get started, shall we?

## An API

As we always do in these tutorials, let’s first think of an API. The one that
I settled on looks like this on the definition side:

```
(defmulti add-fn
  [] 0
  [x] (+ x 1)
  [x y] (+ x y))
```
<div class="figure-label">Fig. 1: Defining a function with multiple bodies.</div>

This function, when called with no arguments, will return `0`. When called with
one argument, it will increment the argument. When called with two, it will add
them.

This is how you might call that function, then:

```
(addr) ; => 0
(addr 1) ; => 2
(addr 4 3) ; => 7
```
<div class="figure-label">Fig. 2: Calling a function with multiple bodies.</div>

Simple enough, right? And that’s really the entire API.

## TL;DR

Because this tutorial is fun and all, but some people just might want to know
the technique, let me sketch it out really quick. If you don’t want spoilers or
get intimidated by a dense jumble of lingo, you might want to skip this
section.

Firstly, we observe that we actually have all the information at hand to
dispatch statically: we know how many arguments are passed at the call site,
and what those functions are.

We can thus define a set of functions with the correct bodies under aliases and
keep the mapping from the argument count to the name around. We also define a
rewrite rule (in a macro system, we define a macro dynamically) that is called
at the call site and replaces the call to the one that is found in the mapping.

We end up with something like this for the example above:
- In our definition we define `addr0`, `addr1`, and `addr2`, and a macro
  `addr`.
- At the call site the macro `addr` looks at the number of arguments it’s being
  passed and replaces itself: `(addr)` becomes `(addr0)`, `(addr 4 3)` becomes
  `(addr2 4 3)` and so on.

And that’s the gist of it. If you want to support optional arguments with
default values, too, you can look [at my implementation over here](https://github.com/carpentry-org/defmulti),
since that makes everything a little more messy, but is perfectly possible.

## An Implementation

So, how would we accomplish something like this? We have a relatively powerful
macro system at our disposal in Carp, so it should get out of the way most of
the time, but how would we even get started?

### Part I: Book-Keeping

Well, first off, let’s take care of the definition part. We need somewhere to
register our variants. For that we’re going to use a map.

```
(defdynamic *defmulti-registry* {})

(defmacro defmulti [name :rest forms]
  (do
    (set! *defmulti-registry
      (Map.put *defmulti-registry* name {}))
    (defmulti-register name forms)))
```
<div class="figure-label">Fig. 3: Defining the skeleton of `defmulti`.</div>

We have an entry point. The map `*defmulti-registry` is going to be where we
put all the information that we have about the forms. Inside `defmulti`, we’re
just going to initialize that value and call a registration function. That
function will mangle the forms into a digestible form.

But how do we keep the functions around? We define functions of course. Inside
our registry, we will keep track of which function had which arity.

If that sounds a little abstract, here it is in action:

```
(defndynamic defmulti-register [name forms]
  ; no forms? we are done
  (if (empty? forms)
    '()
    ; build our thing!
    (let-do [s (gensym-with name) ; a new name for the form
             args (car forms)
             body (cadr forms)]
      ; build a function: from
      ; [] 0
      ; to
      ; (defn generated-name [] 0)
      (eval
        `(defn %s %args %body))

      ; update the map. clunky, but we just
      ; insert <number-of-args>-><generated-name>
      (set! *defmulti-registry*
        (Map.put *defmulti-registry*
          name
          (Map.put
            (Map.get *defmulti.registry name)
            (length args)
            s)))

      ; do it for the rest of the forms
      (defmulti-register name (cddr forms)))))
```
<div class="figure-label">Fig. 4: Registering a multi arity function.</div>

Okay, that wasn’t exactly easy, but at least it was quick. The most annoying
part was updating the map, honestly.

But what about the call sites?

### Part II: Magic

To cut the Gordian knot, all we have to do is define a macro inside our macro.
Macro-defining macros have featured prominently in this blog before, because I
think they are an underappreciated and understudied area of macro-wrangling.
They are also sometimes a nightmare to wrap your head around, and those two
factoids might possibly be related.

Alright, what do we do in this fancy shiny macro? We look up what function
should be called there instead.

Alright, that might be a little vague again, so let’s start with the easy bit:
refactoring `defmulti`.

```
(defmacro defmulti [name :rest forms]
  (do
      (set! *defmulti-registry*
        (Map.put *defmulti-registry* name {}))
      (defmulti-register name forms)
      (eval
        `(defmacro %name [:rest args]
          (defmulti-lookup (quote %name) args)))))
```
<div class="figure-label">Fig. 5: Refactoring `defmulti`.</div>

Alright, that macro isn’t so bad I guess. It just calls `defmulti-lookup` with
the name and arguments. So what does `defmulti-lookup` do?

```
(defndynamic defmulti-lookup [name args]
  (let [registry (Map.get *defmulti-registry* name)]
    (let [f (Map.get registry (length args))]
      (if (nil? f)
        (macro-error
          (str "No arity " (length args)
                " version of " name " found!"))
        (cons f args)))))
```
<div class="figure-label">Fig. 6: Looking up and rewriting the call site.</div>

Okay, not so bad. We look up the registry for the name, then for the number of
arguments provided. If we can’t find it, we raise an error. So far so good.

But then, the magic trick: we just add the name we found to the beginning of
the arguments and return it. What?

It’s a rewrite rule. Basically, we’re telling the system to patch the region
where our macro was called, and to just replace the symbol with the one it
should be. And that’s it!

## Caveats

Okay, okay, that was cool, and weird, and maybe your head is spinning a little bit.
Mine certainly did when I stumbled upon this technique.

Now there are two important caveats with this version of the code:

1. There is almost no error-handling.
2. We don’t have the coolest feature in this kind of system yet: optional
   arguments and default values.

You can either try adding them yourself or read through [my implementation
on GitHub](https://github.com/carpentry-org/defmulti) to see how I did it.

Have fun!

## Fin

As we have done many times before, we’ve taken macros and bent them to our
will to shape what would otherwise be a language feature.

[In my larger series on Scheme macros](https://blog.veitheller.de/scheme-macros/)
we already talked about keyword arguments, but the implementation I presented
there heavily relied on the dynamic nature of Scheme and it had a runtime cost
associated with it. The system here needs no runtime support whatsoever, and
I’d argue for that reason that this implementation is strictly superior.

In the coming weeks I plan on working a little bit on Carp from inside the
[Glamorous Toolkit](https://gtoolkit.com/), so if you’re interested in that,
stay tuned!
