About 3 months ago, I woke up with an idea in my head:
maps<sup><a href="#1">1</a></sup>, as they are often used in programming, are
frozen computation. I had a vague idea how to represent various types of
computation and data structures in terms of maps, and I wrote [a prototype of
a programming language](https://github.com/hellerve/mae) to see what it would
look like. I mostly left it alone after that.

I had assumed that one could make something akin to a lambda calculus with
maps, but at this point computation (as functions) was a separate concept from
data (as maps). I shared the language with a few esolang-minded friends from the
[Recurse Center](https://recurse.com/), and gave a little lightning talk about
it. Someone suggested that it was similar to [Peano’s arithmetic using
sets](https://en.wikipedia.org/wiki/Peano_axioms), and I saw the connection
right away. At that point something clicked.

I realized that, if I wanted to, I could define a (pure) function as simply a
possibly infinite, lazy set that maps inputs to outputs by computation. In
fact, this is not even that much of a mind-bending idea.

Equipped with the knowledge that would enable me to reconcile the
differences between functions and maps, I got to work again, and produced a
version of mae that simply had finite data maps, and infinite computation
maps. Well, and names. And an incidental Lisp syntax, because it is easy to
parse even without a parser.

Let’s discover how to define absolutely everything as maps while slowly
descending into the realm of the unspeakable, shall we?

## `{}`

First, let’s define booleans.

```
; false is the empty map
(def false {})

; true is the map with one entry
(def true {{} {}}))
```
<div class="figure-label">Fig. 1: `true` and `false`.</div>

We define `false` to be the empty map. This will come in handy later.
Similarly, `true` as the map with one entry, mapping the empty map to the empty
map, is a very simple and somewhat sleek definition.

Of course, we could also define any map that isn’t empty to be `true`, but I’d
like to make that explicit.

```
; empty is the same as false
(def empty {})

; empty? maps its input to whether it is equal to
; the empty map or not
(def empty? {(m) -> (= m empty)})

; neg negates booleans by mapping false to true
; and true to false
(def neg {true false false true})

(def truthy? {(m) -> (neg (empty? m))})
```
<div class="figure-label">Fig. 2: Truthiness.</div>

We introduced a different concept in Figure 2, computational/infinite maps, and
this bears explanation. Computational maps take a list of inputs in
parentheses and an expression that turns this into an output, separated by an
arrow (`->`). You could think of this as an anonymous function with arguments
and body, but where would be the fun in that?

Knowing this, when we look at `empty?`, we see that it takes a map `m` and
computes emptiness by comparing it to the empty map `{}`. `=` is a builtin.

Side tidbit to understand how we use `neg`: data maps can be indexed into by
applying them to an argument. This means that if I were to call `(neg true)`,
this would be the same as using `neg[true]` in some of the more
conventional—read: boring—programming languages.

With booleans out of the way, let’s define conditionals. `if` should be enough
to be worthwhile.

```
; if takes a condition, a then and an else branch
; and produces the result of either branch
(def if
  {(cond then else) ->
    ({true then false else} cond)})
```
<div class="figure-label">Fig. 3: `if` as a map.</div>

Using all we know about maps now, we conjure conditionals from the abyss by
building a boolean map and indexing into it with our condition. We map `true`
and `false` to our branches and then use `cond` to select the right one.

This is well and good, but we usually expect `if` to be lazy, and to only
compute the branches when it takes them. We can  rectify this by summoning
another set of our trusty parentheses.

```
(def if
  {(cond then else) ->
    (({true then false else} cond))})
```
<div class="figure-label">Fig. 4: lazy `if` as a map.</div>

The difference is barely noticeable, but deep. Let’s see it in action before I
even try to explain it:

```
; prn soils our beautiful pure garden by cementing
; the pillar of outputting values (“printing”) in
; its middle

; old: it will print both true and false
(if true (prn true) (prn false))

; new: it will only print true
(if true {() -> (prn true)} {() -> (prn false)})
```
<div class="figure-label">Fig. 5: `if` in action.</div>

This particular spell, like many others, was taken from [the scripture of
λ](https://en.wikipedia.org/wiki/Lambda_calculus#Logic_and_predicates),
reimagined to include our new prophet `{}`.

We now have booleans and a way of working with them.

## `{{} {}}`

Even further in the eldritch deeps lurk numbers. They have a convenient way of
being summoned through literals, but do not let their form deceive you: numbers
are maps.

```
0 ; => {}
1 ; => {{} {}}
2 ; => {{} {} {{} {}} {}}
```
<div class="figure-label">Fig. 6: Numbers and their true forms.</div>

Each number contains the previous number as its key alongside all the numbers
before it. This form shows how linked `0` and `1` are to the concept of truth,
resembling `true` and `false`. Every hammer is both a weapon and a tool.

To work with numbers, we need to be able to increment them and decrement them.

```
; add and rem are primitives
; add merges two or more maps
; rem removes a key (and its value) from a map

; filter filters a map using a predicate—we
; will define it later

; inc takes a number and returns the number
; one greater by folding it into itself
(def inc {(n) -> (add n {n {}})})

; dec takes a number and returns the number
; one lesser by removing the entry that, when
; removed, is equal to the map
(def dec
  {(n) ->
    (filter {(k v) -> (neg (= (rem n k) k))} n)})
```
<div class="figure-label">Fig. 7: Incrementing and decrementing.</div>

The structure of our number sigils draw the magic circles that transform them.

Our powers include arithmetic now. Because I gave you a glimpse of our full
powers of folding and filtering, let’s use them to grow and shrink our numbers.

```
; foldr is reduce, it accumulates

; we trick foldr into bending into addition
(def +
  {(m n) -> (foldr {(k v acc) -> (inc acc)} m n)})

; a sleight of the other hand, and we have
; subtraction
(def -
  {(m n) -> (foldr {(k v acc) -> (dec acc)} m n)})
```
<div class="figure-label">Fig. 8: Growing and shrinking.</div>

We clap our hands, and the numbers dance.

## `{{} {} {{} {}} {}}`

Our spellbook needs a last chapter. Let us fill it with control structures:
folding, filtering, and mapping.

First, let’s rip apart the cult of λ and take their favorite spell, whose
names gleam in the night: `car` and `cdr`. Through alchymical experiments no
calculus should ever endure, we bind them to `this` and `next`. `this` gives
us the first entry in a map, `next` gives us the rest. Calling `this` again
on the result of `this` will give us the key, calling `next` will give us the
value. Such is the name of the beast of our creation.

Take a deep breath, and stare into the void until it burns the definition of
`size` into your editor.

```
; size shows a map’s true manifold
(def size
  {(m) ->
    (if (empty? m)
      {() -> 0}
      {() -> (inc (size (next m)))})})
```
<div class="figure-label">Fig. 9: `size`.</div>

Let us use it to bind `foldr`.

```
; our ultimate spell, foldr, burns maps into
; maps, it bends and warps until many become one
(def foldr
  {(f e m) ->
     (if (empty? m)
       {() -> e}
       {() ->
         (if (= (size m) 1)
           {() -> (f (this m) (next m) e)}
           {() -> (f (this (this m))
                     (next (this m))
                     (foldr f e (next m)))})})})
```
<div class="figure-label">Fig. 10: `foldr`, our ultimate power.</div>

Click your tongue one last time, and `map` and `filter` flicker into existence.

```
; map transforms
(def map
  {(f m) ->
    (foldr {(k v m) -> (add m (f k v))} {} m)})

; filter takes away
(def filter
  {(f m) ->
    (foldr
      {(k v m) ->
        (if (f k v)
          {() -> (add m {k v})}
          {() -> m})}
      {} m)})
```
<div class="figure-label">Fig. 11: The children of the fold.</div>

Close your eyes and look around in your mind. Survey the valleys and peaks of
thought, and let creation shine through your heart into the world.

## Fin

You awake, sweating. You realize you wanted to write, but you cannot remember
what it was you meant to say. There is a tome in front of you, written in your
hand.

On your computer, there is a browser window with two tabs open, one telling
the tale of “the thing on the doorstep”, and the other of job interviews and
database consistency.

Your eyes feel heavy, and your heart feels empty. What the fuck just happened?

#### Footnotes

<span id="1">1.</span> I mean hash maps or dictionaries here, not the Open
Street or Google kind.
