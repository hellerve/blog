As some of you might know, I recently got enamoured with a new programming
language, [Carp](https://github.com/carp-lang/carp). While you might have caught
me fawning over it already, in this post I want to give you a little
introduction into the language and its concepts and maybe you’ll understand why
I decided to work on it. A little word of caution before we begin, though: the
language is in a pre-alpha stage and is thus subject to change. The syntax and
APIs I’m about to show you might change in the future and my post might become
obsolete. It won’t be the last time you’ll hear me talk about Carp anyway, so
I suggest you be on the lookout for follow-ups.

## A palatable fish from a reputable source

Let me get those dry, hard facts out of the way first. Carp is a compiled Lisp
that utilizes type inference and a mechanism akin to Rust’s borrow checker to
produce a language that is both functional and fast. Due to its lack of a
garbage collector it can be used for hard realtime systems while still not
requiring the programmer to do manual memory allocations. Instead, the concepts
the user has to think about are those of ownership and references versus
values. Those are familiar concepts to systems programmers, even more so if
they happen to have prior experience with Rust. It provides machinery we know
and love from other modern programming languages, such as a module system and
metaprogramming capabilities—in its infamous Lisp ways—, and, most delicious of
all, it has near-seamless C interoperability.

```
(defn main []
  (IO.println "Hello, Carp!"))
```
<div class="figure-label">
  Fig. 1: The inaugural program, as seen in any piece of literature ever
  produced in programming.
</div>

Carp compiles to C. This is an unusual design choice and seems almost
anachronistic in a world where building a compiler more often than not means
working with LLVM. It also likely doesn’t matter much, because chances are your
machine has a C compiler<sup><a href="#1">1</a></sup>. What’s interesting about
this, though, is that the C code that is produced is actually decent to look at.

```
int main() {
    string _4 = "Hello, Carp!";
    string *_4_ref = &_4;
    IO_println(_4_ref);
}
```
<div class="figure-label">
  Fig. 2: The compiler’s output for the program above. `IO_println` is a
  predefined function.
</div>

In the snippet above you can already see the borrow checker in use, if you’re
looking at the treatment of the string: it is stack allocated, then
dereferenced and passed to the function `IO_println`. Ownership is the simple
idea on which the memory model of Carp is based: which function owns which
references, and who’s in charge of cleaning them up. The Rust programmers among
you will know what I mean.

That’s not really what it’s about, though, at least not in our day-to-day
business. So let’s talk a bit about the language proper.

## Plating the meal

In this part I want to give you a little overview of what Carp looks like
and how one works with it. Alas, there is a lot of ground to cover and I
probably won’t be able to give you anything more than a glimpse of what you
can do with Carp. As I mentioned before, though, I plan on writing more about
it in the future. When Carp is a bit less in flux and I have some downtime I’d
like to write a good, exhaustive tutorial to get people started. Until then
this will have to do.

### The REPL

The first thing you’ll want to do after installing will be playing around. When
you run `carp` with no arguments, you’ll be greeted by a slick
REPL<sup><a href="#2">2</a></sup>. “Play with me”, it seems to say. If you do
decide to play with it, however, you’re in for a surprise.

```
Welcome to Carp 0.2.0
This is free software with ABSOLUTELY NO WARRANTY.
Evaluate (help) for more information.
鲮 (def x 1)
鲮 (+ x 10)
int _3 = Int__PLUS_(x, 1);
```
<div class="figure-label">Fig. 3: Wait, what?!</div>

An enigmatic little beauty, it won’t tell you the results of your typings right
away. Instead it will barf generated code back at you, like a leaky compiler
sewer. This REPL isn’t meant to be played with like that, you see. You’ll have
to adhere to the program/build/run cycle so typical of compiled languages, but
with a twist. You can regain some of the rapid development experience the
developer has when programming in an interpreted language by making the
program/build/run cycle itself interactive. That’s exactly what we’re seeing
here: We’re in the compiler.

```
Welcome to Carp 0.2.0
This is free software with ABSOLUTELY NO WARRANTY.
Evaluate (help) for more information.
鲮 (def x 1)
鲮 (defn main [] (IO.println &(Int.str (+ x 10))))
鲮 (build)
Compiled to './out/a.out'
鲮 (run)
11
```
<div class="figure-label">Fig. 4: Compiling, interactive.</div>

The above snippet showcases the whole cycle. First, we define a variable called
`x`. Then we define the `main` function, which will serve as an entry point to
our program. In it, we print the result of the computation `x + 10`. As you can
see, there is a certain amount of ritual involved; bear with me, I will explain
what those glyphs mean later. For now, you’re a stranger in a foreign land, and
I’m your handwavy guide.

Then we `build` our program, to which our compiler helpfully remarks that the
program was, in fact, compiled.<sup><a href="#3">3</a></sup> Lastly, we `run`
it. Lo and behold, 1 and 10 do equal 11! Tamensi movetur!

This marks the end of my introduction into Carp’s REPL. I suggest you play
around with it some more on your own if you want. It is really quite different,
but also quite fun.

### The language

Carp is a Lisp, and as such it doesn’t have a lot of syntax. So, let’s forget
about the parens, and dive right into keywords. Just know that semicolons start
line comments. There is also a [brief language
guide](https://github.com/carp-lang/Carp/blob/master/docs/LanguageGuide.md) in
the Carp repository. We will cover roughly the same bits here, so feel free to
skip this part and read the official document instead.

```
(def x 1) ; defines a variable called x and binds it to 1
(defn y [a] ; defines a function y that takes one argument a
  (+ a 1)) ; and which adds 1 to a
```
<div class="figure-label">Fig. 5: Defining things.</div>

Defining variables and functions looks similar to Clojure, meaning that you use
`def` to define variables and `defn` to define functions. Global variables can
only have stack-allocated types right now, but this is listed as a bug and will
probably change fairly soon.

There are a few different kinds of literals.

```
100     ;; Int
100l    ;; Long
3.14f   ;; Float
10.0    ;; Double
true    ;; Bool
"hello" ;; String
\e      ;; Char
[1 2 3] ;; (Array Int)
```
<div class="figure-label">Fig. 6: All data literals.</div>

As you can see, there aren’t that many native data types. That’s alright,
though, because we can define our own data types. They are similar to structs
with a few autogenerated functions associated with them, meaning they can only
have data. I personally like my data types like that.

```
(deftype Point2D [x Int, y Int])

(def p (Point2D.init 1 0)) ; initialize a point

; we can now access and update the points members
(Point2D.x p) ; => 1
(Point2D.set-x p 2) ; => (Point2D 2 0)
(Point2D.update-x p dec) ; => (Point2D 0 0)
(Point2D.str p) ; => "(Point2D 1 0)"
```
<div class="figure-label">Fig. 7: Defining and working with a data type.</div>

Updating things can get quite tedious using that syntax. Luckily, we have
threading macros that allow you to apply a whole lot of updates at once without
hassle.

```
; using our point from Figure 7
(=> p
    (Point2D.update-x dec)
    (Point2D.update-y dec)) ; => (Point2D 0 -1)
```
<div class="figure-label">Fig. 8: The threading macro in action.</div>

This mechanism is in best Lisp tradition: simple and powerful, versatile and
elegant. Though, let me be frank with you here: when you are working with Carp
for some time, you’ll likely discover bugs. A source of some of the more
interesting bugs historically has been the combination of the module system
with defining data types. I’m currently working on fixing some name mangling
problems with types in modules, for instance. As I said, Carp is still in flux,
and sometimes we have to get our hands dirty.

With this off my chest, I’m ready to guide you deeper into the heart of the
wild. We have so much more ground to cover; for now let’s talk some more about
special forms!

```
(let [x 1] ; let is used local bindings
  (+ x 1))

(do ; group multiple, possibly side-effecting functions together
  (IO.println "hi")
  1)

(if (= 1 2) ; if is used for branching
  (IO.println "Math is broken")
  (IO.println "Math is fine"))

(while (< 1 x) ; while is used for looping
  (+ x 1))

(for [i 0 10] ; ... and so is for
  (IO.println "print me tenfold!"))

(def x 1)
; set! is used for resetting, ignore the & for now
(set! &x 2)

(address x) ; get a C-style pointer from a value
```
<div class="figure-label">Fig. 9: A few special forms.</div>

That’s a lot! But all of these forms are quite essential for programming, and
so I have to get them out of the way! If you looked at the official
documentation, you’ll see that I have missed `ref` and added `for`. The reasons
for this are simple: `for` is a macro, but super useful, and I’ll talk about
`ref` later, when we talk about the memory model some more.
[Hold your horses!](https://www.youtube.com/watch?v=bWqLiy9TLdg)

We’re about halfway through, and I think it’s time for a breather. We have yet
to talk about modules and macros, and those advanced topics will probably
require your utmost attention. If you have time and the view from your room is
nice, I suggest you make yourself a nice, calming beverage, hot or cold, and
look out the window for some time, asking yourself why you haven’t discovered
this magnificent language sooner. That’s what I did when I discovered it,
anyway.

<hr/>

Welcome back, traveler! I hope you’ve left weary and woe behind, ready to take
on a new challenge. I certainly am excited to tell you about modules!

```
(defmodule Math
  (defn add [x y]
    (+ x y))

  (defn sub [x y]
    (- x y))

  ; TODO: write more stuff
)
```
<div class="figure-label">
  Fig. 10: A module with everything your math heart desires.
</div>

Okay, defining a module is simple enough. You basically just wrap whatever you
want to encapsulate into a `defmodule` form and give it a name. But how do we
actually use it? Well, there are two ways to get to the juicy meat inside the
module’s shell. We can either just load the source file and then use everything
in a qualified manner, prefixing the function name with the module name and a
period, or we can load the file and then `use` the module. Using the module
means making everything inside directly accessible, but it can also introduce
name collisions. If you’ve ever programmed Python, think of it as the difference
between `import foo` and `from foo import *`.

```
; with the module we defined in figure 10
(Math.add 1 2) ; works out the box

(use Math)
(add 1 2) ; works after use form
```
<div class="figure-label">Fig. 11: Using a module.</div>

Now, this is all well and good, but there is another little twist to this.
Modules also have some interesing properties for the type checker. Note, for
instance, that `+` is not generic. It is defined for every numerical type, and
the typechecker then chooses the appropriate function according to its
signature. I think that merits a bit of illustration.

```
(use Double)
(use Int)

(+ 1 1) ; uses Int.+
(+ 1.0 1.0) ; uses Double.+
(+ 1l 1l) ; errors, because no + for longs is known
```
<div class="figure-label">Fig. 12: Addition, complected.</div>

Up until now I spared you the necessary `use` statements to avoid confusing
you, but now that you’re learning to walk on your own you can look at all the
previous figures and see that they’re full of lies. Whenever I used `+` or `-`
or `inc` or `dec` I would have needed to either qualify it or `use` the
appropriate modules. Sorry about that, but it was for your own good.

You can also define your own datatypes within modules, but note that the
dot-syntax then needs to be nested as well. If, for instance, `Point2D` were
defined within `Math`, we would have to write `Math.Point2D.set-x` and so on.

That’s all the magic there is to modules, which means that we can move on to
macros! Are you excited? I’m excited!

### The memory model

### The libraries

### Testing and benchmarking

### Fin

#### Footnotes

<span id="1">1.</span> It’s arguably more likely that your machine has a C
                       compiler than the LLVM library.

<span id="2">2.</span> Readline support thanks to yours truly.

<span id="3">3.</span> There are shortcuts for building and running. These are
                       prefixed with a colon, and are `b` and `x`, respectively,
                       and can be combined in any order. `:bx` is your friend
                       for a quick development cycle.

