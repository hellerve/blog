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
    (=> (MyType.init 1 @"")
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
    (=> (MyType2.init [] {})
        (MyType2.update-l (fn [l] (Array.push-back l @"elem")))
        (MyType2.update-m (fn [m] (Map.put @"key" @"value")))
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
(builder-for MyType3)

(defn main []
  (println*
    (=> (MyType3Builder.init)
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

## Emitting accessors

For the sake of this blog post, we’re only going to emit array and macro
updaters that adds items. If you want to try your hands on a little challenge
later, you can try to add removal or transmutation functions yourself.

What does it mean to build a setter?
