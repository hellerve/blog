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

In this part I want to give you a little overview over what Carp looks like
and how one works with it. Alas, there is a lot of ground to cover and I
probably won’t be able to give you anything more than a glimpse of what you
can do with Carp. As I mentioned before, though, I plan on writing more about
Carp in the future. When Carp is a bit less in flux and I have some downtime
I’d like to write a good, exhaustive tutorial to get people started. Until then
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

### The compiler

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

