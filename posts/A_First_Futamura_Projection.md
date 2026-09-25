---
title: "A First Futamura Projection"
date: 2026-09-21
---

For years I used [compfuck](https://github.com/hellerve-pl-experiments/compfuck),
my little Brainfuck-to-Carp compiler, to explain the shape of a Futamura
projection, even though I knew it wasn’t quite accurate and not quite how
Futamura projections worked. I felt that I captured the general feeling,
however, and that was interesting in itself.

At [Lambda Lounge](https://www.youtube.com/watch?v=wwwa5TG70UA), I wrote an
interpreter, moved its work into compile time, and showed how we’d arrived at a
compiler. I think that evokes much the same emotion as the first Futamura
projection, because once we know the program, we can do some of the
interpreter’s work ahead of time. But `compfuck` explicitly chooses which code
to emit for each instruction. A partial evaluator derives that code by
specializing an interpreter. It’s more general, and a bit more profound, even
though the resulting compiler is maybe not quite as powerful.

This time, we’ll go backwards first. We’ll turn compfuck back into an
interpreter, then figure out which parts we can evaluate with only the source
program available. If you’re wondering why we’d do that when we already have
a working compiler, that’s fair! The useful case is when we have an interpreter
and no compiler yet, or when the manual solution we reached for before is not
as easily available as in Brainfuck. `compfuck` gives us something small and
familiar on which to work through the idea, even if we already have a
perfectly valid execution environment..

I’ll assume a little familiarity with Carp macros and Brainfuck. My
[original post](/Compiling_at_Compile_Time.html) introduces both, and even
leaves turning the compiler into an interpreter as an exercise. Let’s finally
do that.

## Going backwards

The old compiler’s instruction dispatch contains branches like this:

```clojure
\> (list 1 '(set! h (+ h 1)))
```
<div class="figure-label">Fig. 1: Compiling a move to the right.</div>

When we encounter `>`, we return how far to advance through the source and a
piece of code that will move the tape head later. Inside that pair is a quoted
operation. In the interpreter, we perform it instead:

```clojure
\> (set! h (+ h 1))
```
<div class="figure-label">Fig. 2: Interpreting a move to the right.</div>

The other simple instructions likewise become ordinary operations. The
recursive walk through the source is basically the same idea, although it now
executes instructions instead of collecting code fragments.

The question is whether we can recover those fragments by evaluating the
parts of this interpreter that only depend on the source. What does that mean?

Well, given `,[.-]`, we already know that the first instruction reads input,
and that the loop body prints and decrements a cell. We don’t know how often
that loop will run.

The result we’re after looks like this (without the surrounding ceremony):

```clojure
(do
  (Array.aset! t h (IO.Raw.get-char))
  (while (/= &0 (Array.unsafe-nth t h))
    (do
      (IO.print &(str (Char.from-int (copy (Array.unsafe-nth t h)))))
      (Array.aupdate! t h &(fn [x] (max 0 (Int.dec x)))))))
```
<div class="figure-label">Fig. 3: What remains of the interpreter for ,[.-].</div>

The source traversal has disappeared, while the operations on the tape and
the loop remain. This is called the *residual program* in Futamura speak, the
code left over after our partial evaluator completes. Our old compiler
assembled something much like it. This time we want to infer it from the
interpreter’s source.

That is the [first Futamura projection](https://www.cs.utexas.edu/~novak/jonesgomardsestoft.pdf),
which we can write down as simply:

```text
compiled = specialize(interpreter, program)
compiled(input) = interpreter(program, input)
```
<div class="figure-label">Fig. 4: Specializing an interpreter to a program.</div>

It takes an interpreter and a program, and it returns another program with all
the statically-knowable bits evaluated.

Now let’s make that work.

## Keeping the interpreter around

We need an ordinary function that we can call, but we also need its source
for specialization. A small definition macro will give us both:

```clojure
(defmacro definterpreter [name args body]
  `(do
    (defn %name %args %body)
    (meta-set! %name "source" %body)))
```
<div class="figure-label">Fig. 5: Defining a function and remembering its body.</div>

This expands to a normal `defn` and stores the original body as metadata on
the function. The backtick constructs code, and `%` inserts a value into it.
We’ll retrieve the body using `(meta interpret-compf "source")` later. Keeping
it around means we get the forms we wrote, before Carp expands and type-checks
them.

Our bracket search is needed at both runtime and compile time, so we’ll give
it a similar wrapper:

```clojure
(defmacro defstatic [name args body]
  `(do
    (defn %name %args %body)
    (defmodule Dynamic (defndynamic %name %args %body))))

(defstatic search-matching-bracket [s i m]
  (if (or (= i (String.length s)) (= m 0))
    i
    (let [f (String.char-at s i)]
      (search-matching-bracket s (inc i)
        (if (= f \]) (dec m) (if (= f \[) (inc m) m))))))
```
<div class="figure-label">Fig. 6: A scan that’s available in both phases.</div>

`defstatic` defines a compiled function and a compile-time function from the
same body. The scan still counts nesting depth, as in the old compiler. I’ve
changed it to advance an index rather than repeatedly take the tail of the
string, because I’ve gotten older and more careful. Starting just after
`[`, with depth one, it returns the position just after the matching `]`.

Both of these macros are basically general-purpose plumbing, but they’re cool
and I wanted you to see them.

With that in place, here’s the interpreter:

```clojure
(definterpreter interpret-compf [p i t h]
  (if (= i (String.length p))
    h
    (let [f (String.char-at p i)]
      (if (= f \])
        h
        (let [incr (if (= f \[) (- (search-matching-bracket p (inc i) 1) i) 1)]
          (do
            (case f
              \+ (Array.aupdate! t h &(fn [x] (mod (Int.inc x) 255)))
              \- (Array.aupdate! t h &(fn [x] (max 0 (Int.dec x))))
              \> (set! h (+ h 1))
              \< (set! h (- h 1))
              \. (IO.print &(str (Char.from-int (copy (Array.unsafe-nth t h)))))
              \, (Array.aset! t h (IO.Raw.get-char))
              \[ (while (/= &0 (Array.unsafe-nth t h))
                   (set! h (interpret-compf p (inc i) t h)))
              ())
            (interpret-compf p (+ i incr) t h)))))))
```
<div class="figure-label">Fig. 7: The compiler, turned back into an interpreter.</div>

`p` and `i` are the program and our position in it. `t` is a reference to the
tape, and `h` is the head’s position. Reaching the end of the string or a
closing bracket returns the head. Otherwise we execute an instruction and
recurse. Characters outside the eight Brainfuck instructions do nothing, and as
is customary in brainfuck we can use them for comments.

For `[`, we repeatedly interpret the body starting at the next character.
Each call returns when it reaches `]`. Since the body can move the head, we
keep its returned position for the next iteration. Afterwards, `incr` takes
us past the whole loop. All other instructions advance by one character.

## The knowable

This is the fun bit: we build an interpreter for the parts of Carp that we need
to understand `interpret-compf`.

Let’s divide the interpreter’s work. The program `p` and position `i` are
known during specialization. So are the instruction `f`, the bracket scan,
and the amount `incr` by which we advance. In partial evaluation terminology,
these are *static*.

We’ll leave the tape and head entirely *dynamic*, meaning they belong to the
residual program<sup><a href="#1">1</a></sup>. Even though their initial values
are known, we won’t follow them during specialization. That also means we
won’t execute the `while` even if it’s fully knowable. Left as an exercise to
the reader.

This division is particularly convenient for our interpreter. Every `if` and
`case` test, and every single-binding `let`, belongs to the source traversal.
We can evaluate those. The runtime operations don’t refer to any of those
static local variables. We’re making these decisions by hand, which keeps
our specializer small and limits it to this interpreter’s structure.

Here’s the whole thing:

```clojure
(defndynamic specialize-compf [body expression bindings]
  (let [known (fn [form] (eval (list 'let bindings form)))
        recur (fn [form] (specialize-compf body form bindings))]
    (if (or (not (list? expression)) (empty? expression))
      expression
      (case (car expression)
        'if (recur (if (known (cadr expression)) (caddr expression) (cadddr expression)))
        'let (specialize-compf body (caddr expression)
               (append bindings (array (car (cadr expression)) (known (cadr (cadr expression))))))
        'case (recur (case-internal (known (cadr expression)) (cddr expression)))
        'interpret-compf
          (let [saved (gensym)]
            `(let [%saved %(last expression) h %saved]
              %(specialize-compf body body
                (array 'p (known (cadr expression)) 'i (known (caddr expression))))))
        (map recur expression)))))
```
<div class="figure-label">Fig. 8: Specializing the interpreter’s source traversal.</div>

There are a few nested list accesses here, so let’s walk through them.
`body` is the complete interpreter body, `expression` is the part we’re
currently looking at, and `bindings` holds the static names and values.
`known` evaluates an expression inside a `let` containing those bindings,
`recur` continues specialization with the same bindings.

For an `if`, we evaluate its test and continue with the selected branch.
For a `let`, we evaluate its binding, add it to our environment, and continue
with its body. `case-internal`, a helper from Carp’s standard library,
turns a `case` into nested `if` expressions, which we already handle.

A recursive call needs a little more care. Its program and next source
position are static, so we can unfold another copy of the interpreter body
with those values. The tape reference is passed through unchanged. The head
is a runtime argument, though, and we save it under a fresh name and bind a
local `h` for the unfolded call. That preserves the function’s local cursor and
returned value.

Everything else is rebuilt by recursively visiting its parts. This includes
`while`. We specialize its body, but retain the loop and its condition.
We never need to determine the number of iterations.

For `,[.-]`, the recursive call inside the loop starts at the `.`. We retain
the output operation, then the decrement, then reach `]` and return `h`.
That finishes specializing the loop body. The interpreter’s `while` is still
there to repeat it at runtime. This is how we get the code from Figure 3,
along with the cursor bindings omitted there.

## Putting it back together

We can now pull ot the interpreter body and specialize it with a known
program and starting position:

```clojure
(defndynamic compile-compf [p i]
  (let [body (meta interpret-compf "source")]
    (specialize-compf body body (array 'p p 'i i))))

(defmacro compf [prog]
  `(defn main []
    (let [tape (Array.replicate 30000 &0)
          t &tape
          h 0]
      (ignore %(compile-compf prog 0)))))
```
<div class="figure-label">Fig. 9: The compiler’s entry point.</div>

The public API remains `(compf "some Brainfuck")`. The macro inserts the
initialization as before, and splices in the specialized body. We ignore its
returned cursor because `main` has nothing left to do with it. Carp compiles
the result in the usual way.

And it works!

```
鲤 (load "compfuck.carp")
鲤 (expand '(compf "+[.+]"))
=> ; "pretty" printed for you (no amount helps)
(defn main []
  (let [tape (Array.replicate 30000 (ref 0)) t (ref tape) h 0]
    (let [_ (do
              (Array.aupdate! t h (ref (fn [x] (mod (Int.inc x) 255))))
              (let [gensym-generated1042 h h gensym-generated1042]
                (do
                  (while (/= (ref 0) (Array.unsafe-nth t h))
                    (set! h
                      (let [gensym-generated1043 h h gensym-generated1043]
                        (do
                          (IO.print (ref (str (Char.from-int (copy (Array.unsafe-nth t h))))))
                          (let [gensym-generated1044 h h gensym-generated1044]
                            (do
                              (Array.aupdate! t h (ref (fn [x] (mod (Int.inc x) 255))))
                              (let [gensym-generated1045 h h gensym-generated1045] h)))))))
                  (let [gensym-generated1046 h h gensym-generated1046]
                    h))))]
      ())))
```
<div class="figure-label">Fig. 10: Specializing Brainfuck.</div>

Easy, right?

## Fin

I think the similarity that made compfuck useful for explaining the projection
is still there. We know the source, do the work that we can, and leave a program
for runtime. But now we have a general mechanism to fold an interpreter into our
pipeline.

As is often the case, the more general idea is considerably more intricate than
this little implementation. We supplied the division between static and dynamic
work ourselves, and relied on the interpreter’s particular structure. A partial
evaluator that can make these decisions for a larger language has more work
to do. But, given enough power, an interpreter becomes a way to obtain compiled
programs without separately writing their instruction translations, which is
bonkers to me.

Today, we folded a compiler into an interpreter, and then took the scenic route
back. Not a bad use of an afternoon.

See you soon!

#### Footnotes
<span id="1">1.</span> Unfortunately, this is a different use of “dynamic”
from Carp’s `defndynamic`, whose functions run at compile time. Here it means
that we leave the computation until the generated program runs. We have two
different static/dynamic divides in the same post, and I apologize.
