As some of you might know, I recently became enamoured with a new programming
language, [Carp](https://github.com/carp-lang/carp). While you might have caught
me fawning over it already, in this post I want to give you a little
introduction into the language and its concepts, and maybe you’ll understand why
I decided to work on it. A little word of caution before we begin, though: the
language is in a pre-alpha stage and is thus subject to change. The syntax and
APIs I’m about to show you might change in the future, making my post
obsolete. It won’t be the last time you’ll hear me talk about Carp anyway, so
I suggest you be on the lookout for follow-ups.

*Warning*: This post is *very* long. You’ll have a good initial grasp on the
language once you’ve read it, but feel free to skip the parts that you don’t
care about. If you only care about how it works with memory, for instance, feel
free to skip the language guide. If you plan on learning Carp, however, this is
probably the most exhaustive resource in existence at the time of writing.

## A palatable fish from a reputable source

Let me get those dry, hard facts out of the way first. Carp is a compiled Lisp
that utilizes type inference and a mechanism akin to
Rust’s<sup><a href="#1">1</a></sup> borrow checker to produce a language that
is both functional and fast. Due to its lack of a garbage collector it can be
used for hard realtime systems while still not requiring the programmer to do
manual memory allocations. Instead, the concepts the user has to think about
are those of ownership and references versus values. Those are familiar
concepts to systems programmers, even more so if they happen to have prior
experience with Rust. It provides machinery we know and love from other modern
programming languages, such as a module system and metaprogramming
capabilities—in its infamous Lisp ways—, and, most delicious of all, it has
near-seamless C interoperability.

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
machine has a C compiler<sup><a href="#2">2</a></sup>. What’s interesting about
this, though, is that the C code produced is actually decent to read.

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
looking at the treatment of the string; it is stack allocated, then
dereferenced and passed to the function `IO_println`. Ownership is the simple
idea on which the memory model of Carp is based: which function owns which
references, and who’s in charge of cleaning them up. The Rust programmers among
you will know what I mean.

That’s not really what it’s about, though, at least not in our day-to-day
business. So let’s talk a bit about the language itself.

## Plating the meal

In this part I want to give you a little overview of what Carp looks like
and how one works with it. Alas, there is a lot of ground to cover and I
probably won’t be able to give you anything more than a glimpse of Carp's 
potential. As I mentioned before, though, I plan on writing more about
it in the future. Until Carp is a bit more stable, and I have the time 
to write a good, exhaustive tutorial, this will have to do.

### The REPL

The first thing you’ll want to do after installing will be to play around. When
you run `carp` with no arguments, you’ll be greeted by a slick
REPL<sup><a href="#3">3</a></sup>. “Play with me”, it seems to say. If you do
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
program was, in fact, compiled.<sup><a href="#4">4</a></sup> Lastly, we `run`
it. Lo and behold, 1 and 10 do equal 11! Tamensi movetur!

This marks the end of my introduction into Carp’s REPL. I suggest you play
around with it some more on your own (if you want). It is really quite different,
but also quite fun.

### The language

Carp is a Lisp, and, as such, it doesn’t have a lot of syntax. So, let’s forget
about the parens, and dive right into keywords. Just know that semicolons start
line comments. There is also a [brief language
guide](https://github.com/carp-lang/Carp/blob/master/docs/LanguageGuide.md) in
the Carp repository. We will cover roughly the same bits here, so feel free to
skip this part and read the official document instead.

### Defining things

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

#### Types & Literals

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
though, because we can define our own data types. They are similar to structs,
and have a few autogenerated functions associated with them, meaning they can 
only have data. That's how I personally like my data types to be.

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
with defining data types. For instance, I’m currently working on fixing 
some name mangling problems with types in modules. As I said, Carp is 
still in flux, and sometimes we have to get our hands dirty.

#### Special forms

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
so I have to get them out of the way. If you look at the official
documentation, you’ll see that I have omitted `ref` and added `for`. The reasons
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

#### Modules

Welcome back, traveler! I hope you’ve left weary and woe behind, ready to take
on a new challenge. I am certainly excited to tell you about modules!

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

Until now I've spared you the necessary `use` statements to avoid confusion. 
But, now that you’re learning to walk on your own, you can look at all the
previous figures and see that they’re full of lies. Whenever I used `+` or `-`
or `inc` or `dec` I would have needed to either qualify it or `use` the
appropriate modules. Sorry about that, but it was for your own good.

You can also define your own datatypes within modules, but note that the
dot-syntax then needs to be nested as well. If, for instance, `Point2D` were
defined within `Math`, we would have to write `Math.Point2D.set-x` and so on.

That’s all the magic there is to modules, which means that we can move on to
macros! Are you excited? I’m excited!

#### Macros

If you’ve ever programmed in Lisp, you probably know about macros. I also
wrote [a](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html)
[series](http://blog.veitheller.de/Scheme_Macros_II:_Generics.html)
[of](http://blog.veitheller.de/Scheme_Macros_III:_Defining_let.html) blog posts
about writing Lisp macros, but let me try to sum the most important ideas up
for those of you who don’t have the time to do all of the research: since Lisp
code is essentially only lists, we can easily rewrite it programmatically. Lisp
macro systems exploit this fact; the compiler introduces a separate step into
its compilation toolchain that evaluates macros and expands their use. It’s
essentially a small interpreter that is geared towards rewriting Lisp forms
into other forms. This enables us to introduce interesting new syntax without
changing the language proper, and in fact that is how `for` and the threading
macro `=>` are defined in Carp.

This tremendous power is easily abused and indeed macros have for a long time
had the reputation of being too powerful and leading to programmers writing
their own languages on top of their implemenation Lisp that only they
themselves understand. [I renounce any catalogue of
despair](https://kar.kent.ac.uk/33611/7/paper.pdf). It is perfectly feasible to
write maintainable and understandable macros, and tying the programmers hands
to avoid bad code isn’t exactly what I want my language to do. But this is a
topic for another day. For now I will show you how to write macros in Carp, and
you can make up your own mind as to whether you want your code to make use of
them. Yet another caveat before we begin, though: I’m planning on rewriting the
macro system to a full-featured, hygienic piece of craftmanship. These things
take time, however, and for now I’m going to show you the current state of the
art.

```
(defmacro incr [x]
  (list '+ x 1))
```
<div class="figure-label">Fig. 13: A simple incrementor macro.</div>

As you can see in Figure 13, defining macros looks somewhat similar to defining
functions. The main difference is the body, which constructs a list instead of
applying the `+` function. Please note that the `list` keyword can only be used
within macros, and it is used to make a list from everything following it. The
`+` function is also quoted, which is a fancy Lisp term for saying that instead
of looking up the value of symbol right now, the runtime will just pass it as
is, not caring whether it’s actually defined or not.

So what is the value of doing that here? Doing this enables us to write
`(incr x)` instead of `(+ x 1)` wherever we want. The same would be possible
with a function, though, right? Well, kind of. But at runtime, there will be
no function. Instead, the macro system will have transformed `(incr x)` into
`(+ x 1)` *directly*, within its context. Beautiful, isn’t it?

Maybe that just knocked you out of your knickers, but I know that back in the
day before I knew Lisp macros, it would certainly not have impressed me very
much. So, let’s look at a more involved example, and take advantage of all of
the exciting features the Carp macro runtime has to offer: infix math!

```
(defdynamic rewrite-infix [form]
  (if (= (count form) 0)
    (list)
    (if (= (count form) 1)
      (car form)
      (list (car (cdr form))
            (car form)
            (rewrite-infix (cdr (cdr form)))))))

(defmacro infix [:rest form]
  (rewrite-infix form))
```
<div class="figure-label">Fig. 14: Infix math.</div>

Figure 14 contains code that rewrites infix math expressions—that is, math that
follows the conventional form of `1 + 2 * 2`—to Lisp-compatible prefix math.
Most Lisps provide some kind of mechanism to do that, and even one of the more
famous books on programming in Scheme<sup><a href="#5">5</a></sup> includes an
implementation of that.

The above code contains a slew of new concepts; let me walk you through them.
First, you will notice the definition `defdynamic`, which we haven’t
encountered before. Dynamic functions are functions that you can use from
within a macro, but not during runtime. They’re the basic building blocks for
abstraction during macro evaluation, so to speak. From the outside, they’re
very similar to regular functions, but they have a whole host of functions
that only work within them. Some of these functions are used in the snippet
in Figure 14, like `car`, `cdr`, `cons`, `list`, `quote`—though we use the
reader macro `'` instead—, and, somewhat surprisingly, `array`.

At this point I expect all of the old Lisp hackers who have found this blog
are screaming in terror, no `cons`, `car`, or `cdr`? The audacity! The
blasphemy! I too had to squint at this in disbelief. But it makes sense:
lists are replaced by random access arrays in Carp. Lists only exist during
macro evaluation, where they are linked lists of code. Arrays, however, are
random access data structures, like C arrays—Carp does compile to C, after all.
This removes a bit of the beautiful abstraction of Lisp—one data structure
to rule them all—and it makes runtime metaprogramming nigh impossible, but it
does make sense for a language compiled to C. And, as seen in Figure 14, it’s
not really that much harder to write a macro like that.

For all of the people above who don’t know what `cons`, `car`, or `cdr` are
and didn’t appreciate my tangent directed at only the enlightened few Lispers 
scoffing at me in their ivory tower<sup><a href="#6">6</a></sup>, these 
functions are the pinnacle ofworking with lists in Lisp. `car` takes a list 
and returns its first element,`cdr` takes the rest—i.e. everything but the 
first element—, and `cons` takes an element and a list and prepends the element 
onto the list. These functions are incredibly handy for working with linked 
lists, but again, Carp works with array, and it really doesn’t make sense here.

In general, the functions listed above are overly generic and not
incredibly useful in the context of Carp. Including them certainly poses a
trade-off, and in my opinion the maintainers took the right step in allowing
these constructs only where they made sense. Feel free to disagree.

There is at least one more syntactic item we haven’t looked at yet, and that
is `:rest`. This little beauty is, like its friends, not available in Carp
proper. It signifies that this macro is variadic, that is, it can have a
varying number of arguments. The symbol that comes after `:rest` will bind all
of the “overflowing” parameters in a list. Let’s look at a few examples:

```
; none of this will compile
(defmacro macro1 [:rest x]
  x)

(macro1 1 2 3) ; x=(1 2 3)

(defmacro macro2 [a b :rest c]
  a b c)

(macro2 1 2 3 4 5) ; a=1, b=2, c=(3 4 5)
```
<div class="figure-label">Fig. 15: Illustrative macros.</p>

As you can see, you can also have variadic macros that take a certain number
of parameters, but then a variable number of extra ones. For the Lispers: it’s
equivalent to `(a b . c)`. For the Pythonistas: it’s equivalent to
`(a, b, *c)`.

This concludes our short—by which I mean approximately 3000 words—whirlwind
tour of the language. This is enough to get you started, but Carp has more up
its apparently very large sleeve: I’ve omitted type annotations and C interop,
both of which I will cover in later posts.

Next up we'll talk about references and values!

### The memory model: references & values

Somewhere down the road in your programming career, you’ll have to think about
memory, no matter the programming language you chose. Some of us choose
languages that are very explicit about who owns what (like Rust), or even let
you choose who has to allocate and free it (like C). Others include a
garbage collector (Python, Ruby, Go, JavaScript, and the like) to avoid the
cognitive complexity manual memory management introduces. But memory will rear
its ugly head sooner or later, be it through contention, corruption, races, or
any other of the countless horsemen of the software apocalypse it employs.

Yes, I’m being melodramatic here, but sleepless nights and loss of hair are not
unheard of when dealing with these types of problems, so I couldn’t be dramatic
enough, not even with a skull in my hand and leggings on, declaiming: “To
collect or not to collect...”. I’m not sure whether you like the picture of
me in leggings, but I’d rather avoid it, so I’ll try to have you believe in
the Carp way.

Who owns memory in Carp? Simple: whoever created it, unless it is returned to
the encompassing scope. That means that at the end of each function or `let`
form, things defined inside it get destroyed. Within this block, it is free
to do with this datum as it pleases, even lending it to another function is
alright. Remember the function `ref` I talked about earlier? Yeah, that’s what
that is. And because this is done fairly often, Carp decided to sprinkle a bit
of syntactic sugar on top of that construct, in the form of the abbreviation—or
reader macro, if you like—`&`. The two constructs `(ref "string")` and
`&"string"` are functionally equivalent.

```
; println takes a reference to a string,
; but str returns a normal string, so we have
; to add &
(IO.println &(Int.str 1))
```
<div class="figure-label">Fig. 16: A simple use case of references.</div>

This also explains the ampersand we used in Figure 4. I hope it's all starting
to make sense.

The inverse of referencing is copying. This can either be used through `copy` or
the abbreviation `@`. Be aware that in the current iteration of Carp,
`copy` without a namespace—or `@`—will be interpreted as `String.copy`, and
the type system will complain if you pass anything else. The current workaround
is either `use`ing the type you’re copying, or referencing the fully qualified
function. This might be fixed in the future, or so I hope.

```
@1 ; uh oh

(Int.copy 1) ; this is fine

(use Int)
@1 ; this is fine now
```
<div class="figure-label">Fig. 17: Copying stuff and its oddities.</div>

That’s all there is to memory in Carp. The [official
documentation](https://github.com/carp-lang/Carp/blob/master/docs/Memory.md)
has a little bit of additional information and illustration, if you
need it.

### The libraries

Libraries are an integral part of any language. No programming language will
get adoption without a proper ecosystem, no matter how well it is designed. And
this is where things get sad in Carp. There is no dependency managment, no
package manager. I plan on porting my package manager for zepto,
[zeps](https://github.com/zeps-system/zeps), to Carp sooner or later, but that
might take a while. Also, there's not much of a standard library yet, though
the team and I are working on that too, having introduced a few hundred lines
of new code to the system in the last few weeks. There is a light at the end of
the tunnel, sed ars longa, vita brevis.

Nonetheless, I want to introduce you to at least a few libraries that exist
right now and show you how to use them. There is an emberassing scarcity of
documentation, but we plan on changing that as well.

Here is an exhaustive list of modules with a little bit of preliminary
information, in alphabetical order:

* Array: includes a few functions for working with arrays. All of them are
         generic.
* Bench: a simple benchmarking library, for testing the performance of small
         snippets of code. Modeled after Rust’s benchmarking tooling.
* Bool: includes a few functions for working with boolean values.
* Char: includes a few functions for working with and converting boolean values.
* Double: see above, but for Doubles. Also includes all mathematical functions
          from `math.h`.
* Float: see above, but for Floats.
* Geometry: includes very few functions for geometrical computations.
* IO: includes a few functions for input and output, like printing, reading
      lines, and exiting.
* Int: see above numerical modules, but for Ints.
* Long: see above numerical modules, but for Longs.
* Macros: non-namespaced macro utilities, like `for`, `cond` and the threading
          macros.
* sdl: includes bindings to the SDL (Simple DirectMedia Layer) library.
* Statistics: includes statistical functions, like `mean`, `median`, and
              `winsorize`.
* String: includes all kinds of utilities for working with and converting
          strings.
* System: includes a few functions for working with the OS, such as timing,
          seeding the random number generator, etc.
* Test: includes a testing library.
* Vector: includes an implementation of 2D, 3D, and n-dimensional vectors.

Your best bet for getting detailed information for these modules is currently
by looking at the [source](https://github.com/carp-lang/Carp/tree/master/core),
the [examples directory](https://github.com/carp-lang/Carp/tree/master/examples),
or, for a few modules, the [test cases](https://github.com/carp-lang/Carp/tree/master/test).
We hope this will change soon.

A few of my babies are the testing and benchmarking libraries, and I intend to
write about them specifically in future posts.

### Fin

As promised, this post was fairly long. You were warned. I hope it gave you
a taste of what Carp is like. I’ll write a few more practical posts in the
future, but I needed to help you understand the syntax to be comfortable with
showing you the fun things we can do with Carp. I hope you agree.

#### Footnotes

<span id="1">1.</span> I’m going to compare Carp to a whole lot of programming
                       languages; you don’t need to know all—or any—of them to
                       understand what I’m talking about.

<span id="2">2.</span> It’s arguably more likely that your machine has a C
                       compiler than the LLVM library.

<span id="3">3.</span> Readline support thanks to yours truly.

<span id="4">4.</span> There are shortcuts for building and running. These are
                       prefixed with a colon, and are `b` and `x`, respectively,
                       and can be combined in any order. `:bx` is your friend
                       for a quick development cycle.

<span id="5">5.</span> I am talking about “Simply Scheme: Introducing Computer
                       Science” by Harvey and Wright, Chapter 18.

<span id="6">6.</span> Where is that ivory tower, anyway? I’ve written
                       thousands of lines of Lisp and built some Lisps on my
                       own, so I’d appreciate an invite, guys n’ gals, lest I
                       doubt its existence.
