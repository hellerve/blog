Initializing complex objects can be a pain. Unintuitive data dependencies and
aggregate members can make your life hard. As so very often, there is a design
pattern that helps simplify that process: [the Builder
pattern](https://en.wikipedia.org/wiki/Builder_pattern).

Although the builder pattern comes from object-oriented design, it’s also useful
for functional languages; in fact, today I’m going to talk about it in the
context of Carp!

We’re going to look at how building complex data types works in idiomatic Carp,
and how we can make it even better using macros. Sounds good? Alright, let’s go!

## Building things in Carp

When defining a type in Carp, the compiler already creates getter, setter and
update functions automatically. Combined with the threading macro `=>`, this
makes for a nice interface already:

```
(deftype MyType [
  int Int,
  str String,
])

(defn main []
  (println*
    &(=> (MyType.init 1 @"")
         (MyType.set-str @"string")
         (MyType.update-int Int.inc)
     )
  )
)
```
<div class="figure-label">Fig. 1: Building a simple type in Carp.</div>

But this falls short in several respects. The most obvious problem is that this
only works on all-or-nothing data types. If you had an array or map as members,
for instance, the ergonomics get thrown overboard. If you wanted to append
to an array or add a new key-value pair to a map, the best we can do with what
Carp provides us is something like this:

```
(deftype (MyType2 a b c) [
  l (Array a),
  m (Map b c),
])

(defn main []
  (println*
    &(=> (MyType2.init [] {})
         (MyType2.update-l
           (fn [l] (Array.push-back l @"elem")))
         (MyType2.update-m
           (fn [m] (Map.put @"key" @"value")))
     )
  )
)
```
<div class="figure-label">Fig. 2: Building a complected type in Carp.</div>

And already ergonomics get thrown overboard. That’s the first case we’d like to
fix with our builder. The next problem is the classical builder problem: we
have to provide a set of initial values. Ideally, we’d like to provide them bit
by bit and maybe even have values that weren’t supplied to the builder be
`zero`.

Ideally, we’ll end up with this:

```
(deftype (MyType3 a b c) [
  i Int,
  l (Array a),
  m (Map b c),
])
(builder-for 'MyType3)

(defn main []
  (println*
    &(=> (MyType3Builder.init)
         (MyType3Builder.set-i 1)
         (MyType3Builder.add-to-l @"elem")
         (MyType3Builder.emit))))
; => (MyType3 1 [@"elem"] {})
```
<div class="figure-label">
  Fig. 3: Building a complected type using the builder pattern.
</div>

As always with my blog posts, the solution to this problem is based on macros.
First, we’re going to write macros that create setters for arrays and maps, as
a kind of warm-up exercise. Once we’ve gotten this to work, we’ll try and write
a macro that when it’s passed a datatype emits a builder for it.

## Emitting setters

For the sake of this blog post, we’re only going to emit array and macro
updaters that adds items. If you want to try your hands on a little challenge
later, you can try to add removal or transmutation functions yourself.

What should the interface look like? The way that would lead to the simplest
code would pass both the type and the property to us, like this:

```
(array-builder MyType3 l)
(map-builder MyType3 m)
```
<div class="figure-label">Fig. 4: A setter emission API.</div>

This makes it easy for us to figure out what to wrap. The next question we
should ask ourselves is what the generated code should look like. Let’s look at
the code that `map-builder` has to emit—the array version would be extremely
similar.

```
(defmodule MyType3
  (defn add-to-m [obj k v]
    (update-m obj &(fn [map] (Map.put map k v))))
  (defn add-to-m! [obj k v]
    (Map.put! (m obj) k v)))
```
<div class="figure-label">Fig. 5: A desugared version of the builder.</div>

We emit both in-place and functional updaters, because its cost is close to
zero. The code should be pretty self-explanatory: in both cases we reuse
existing functionality—`Map.put`—on our property.

Now that we have an API and a template, let’s get to generating!

```
(defmacro map-builder [md prop]
  (list 'defmodule md
    (list 'defn (Symbol.join ['add-to- prop])
                (array 'obj 'k 'v)
      (list (Symbol.join ['update- prop]) 'obj
        '(ref (fn [map] (Map.put map k v)))))
    (list 'defn (Symbol.join ['add-to- prop '!])
                (array 'obj 'e)
      (list 'Array.push-back! (list prop 'obj) 'e))))
```
<div class="figure-label">Fig. 6: The `map-builder` macro, in full.</div>

I decided to show you the whole code rather than building it up iteratively as I
usually do, just because we have so much material to get through. If you look
at the code we want to emit in Figure 5, however, it should be pretty clear
what this macro does: it is basically just a template.

For completeness’ sake, let’s look at `array-builder` as well, although it is
extremely similar.

```
(defmacro array-builder [md prop]
  (list 'defmodule md
    (list 'defn (Symbol.join ['add-to- prop])
                (array 'o 'e)
      (list (Symbol.join ['update- prop]) 'o
            '(ref (fn [a] (Array.push-back a e)))))
    (list 'defn (Symbol.join ['add-to- prop '!])
                (array 'o 'e)
      (list 'Array.push-back! (list prop 'o) 'e))))
```
<div class="figure-label">Fig. 7: The `array-builder` macro, in full.</div>

And that’s all the code we need for making the API of array and map building
better! If you want a bit of a deeper dive into this piece of code, look at
`builder.carp` in [the Carp snippets repository](https://github.com/carpentry-org/snippets).
It’s almost exactly the same code, and it’s heavily commented for your
convenience.

So, where are we at? We can already make code like this work now:

```
(defn main []
  (println*
    &(=> (MyType3.init 0 [] {})
         (MyType3.add-to-m @"key @"value")
         (MyType3.add-to-l @"elem"))))
```
<div class="figure-label">Fig. 8: Setters in action.</div>

This is a huge improvement, but I think we can do better! Let’s look at how to
emit a builder for the entire type!

## Rummaging through types

We’ll have to go through the type’s members, similar to what we did in [my blog
post about `derive`](https://blog.veitheller.de/Carp_and_derive.html). As I
explained there as well, we’ll have to use `members` to get the member names and
types, but, unlike last time, we’re actually going to use the type this time.

But before we get ahead of ourselves, we should probably dream up an API again.
We want to emit a new module for each type that we hand to the macro, emitting
the code we need to incrementally create that type. So the API looks a little
like this:

```
(builder-for 'MyType3)
```
<div class="figure-label">Fig. 9: Our ideal builder API.</div>

And the code that should be generated looks like this:

```
; this type should be the same as the original,
; but all fields are optional
(deftype MyType3Builder [
  i (Maybe Int),
  l (Maybe (Array a)),
  m (Maybe (Map b c)),
])

(defmodule MyType3Builder
  ; we’re adding setters that accept direct values
  (defn build-i [b e] (set-i b (Maybe.Just e)))
  (defn build-l [b e] (set-l b (Maybe.Just e)))
  (defn build-m [b e] (set-m b (Maybe.Just e)))

  ; we also build a generator
  (defn emit [b]
    (MyType3.init
      (Maybe.get-or-zero (i b))
      (Maybe.get-or-zero (l b))
      (Maybe.get-or-zero (m b))
    )
  )
)

; we’ll also emit type builders if applicable
(array-builder MyType3 l)
(map-builder MyType3 m)
```
<div class="figure-label">Fig. 10: A large pile of generated code.</div>

That’s a lot of code to generate, but most of it is fairly formulaic. Let’s
build a macro that generates that code!

### The main macro

The main macro is the glue that holds everything together. It will defer most of
its work to dynamic functions, and will thus be relatively simple.

```
(defmacro builder-for [t]
  (do
    (generate-builder-type t)
    (cons 'defmodule
      (cons (generate-builder-name t)
        (cons (generate-emitter t)
              (generate-setters (members t)))))
    (generate-type-builders t))
)
```
<div class="figure-label">Fig. 11: The `builder-for` macro.</div>

The structure of this macro closely follows the structure of the generated code
in Figure 9, so it should be relatively easy to figure out which function does
what.

We’re going to go through the helper functions in order of appearance, but
immediately break that rule by starting with `generate-builder-name`. We did
something similar above in the map and string builders, so generating the name
should be fairly simple. Let’s take a look:

```
(defndynamic generate-builder-name [t]
  (Symbol.join [t 'Builder]))
```
<div class="figure-label">Fig. 12: The `generate-builder-name` macro.</div>

This will generate the name `MyType3Builder` from `MyType3`.

Next up: generating the builder type.

### Generating the builder type

Generating the builder type should be fairly straighforward as well. We can
reuse the `generate-builder-name` helper function to generate the type name, and
then iterate through the original members to get our new members.

```
(defndynamic generate-builder-type [t]
  (list 'deftype (generate-builder-name t)
    (generate-builder-type-body (members t))))
```
<div class="figure-label">Fig. 13: The `generate-builder-type` macro.</div>

Once again we defer to a helper to generate the members. It should recursively
build an array of members, wrapping the old members inside `Maybe`.

```
(defndynamic generate-builder-type-body [ms]
  (if (= (length ms) 0)
    []
    (append [(caar ms) (list 'Maybe (cadar ms))]
            (generate-builder-type-body (cdr ms)))))
```
<div class="figure-label">Fig. 13: The `generate-builder-type-body` macro.</div>

And we just generated a type! One down, three to go!

### Generating the emitter

The emitter is also relatively simple. We need to go through the members again,
this time wrapping each of them in a function call, in order. As before, we’re
going to use a combinatiom of two dynamic functions, one for the skeleton and
one that recurses over the members.

```
(defndynamic generate-emitter [t]
  (list 'defn 'emit (array 'b)
    (cons (Symbol.prefix t 'init)
      (generate-emitter-body (members t)))))
```
<div class="figure-label">Fig. 14: The `generate-emitter` macro.</div>

While this is a little bit more involved than the type shim, there is no magic
involved. We’re using a new function, `Symbol.prefix`, that adds a module to
a function—e.g. `(Symbol.prefix 'Maybe 'apply)` would evaluate to `Maybe.apply`.

The function that generates the emitter body looks a lot like
`generate-builder-type-body` in Figure 13. They both go through the members
recursively, and generate some code. The code is arguably even simpler, though.

```
(defndynamic generate-emitter-body [ms]
  (if (= (length ms) 0)
    ()
    (cons (list 'Maybe.or-zero (list (caar ms) 'b))
          (generate-emitter-body (cdr ms)))))
```
<div class="figure-label">Fig. 15: The `generate-emitter-body` macro.</div>

These two functions are all we need to generate the emitter body. We have the
type and the emitter, now we just need to emit the setters and finally the type
builders.

### Generating the setters

You know the drill by now. We’re going through the members and generate some
code.

```
(defndynamic generate-setters [ms]
  (if (= (length ms) 0)
    ()
    (cons
      (list 'defn (Symbol.join ['build- (caar ms)])
                  ['b 'e]
            (list (Symbol.join ['set- (caar ms)])
                  'b
                  '(Maybe.Just e)))
      (generate-setters (cdr ms)))))
```
<div class="figure-label">Fig. 16: The `generate-setters` macro.</div>

No news here. The emitter pattern is a little more complex, but other than that
we’re not doing anything new.

### Generating the type builders

Alright, the last element on our list, and we’re mixing it up a little, too!
For this part we’re going to have to check the type and only emit things when
we encounter types we know how to treat.

```
(defndynamic generate-type-builders [t]
  (generate-type-builder t (members t)))
```
<div class="figure-label">Fig. 17: The `generate-type-builders` macro.</div>

```
(defndynamic generate-type-builder [t ms]
  (if (= (length ms) 0)
    ()
    (cons (generate-type-builder-for t (caar ms) (cadar ms))
          (generate-type-builder t (cdr ms)))))
```
<div class="figure-label">Fig. 18: The `generate-type-builder` macro.</div>

```
(defndynamic generate-type-builder-for [t m typ]
  (if (list? typ)
    (if (= (car typ) 'Map)
      (list 'map-builder t m)
      (if (= (car typ) 'Array)
        (list 'array-builder t m)
        ()))
    ()))
```
<div class="figure-label">Fig. 19: The `generate-type-builder-for` macro.</div>
