This is a blog post about [a cute hack I threw together on a long train
ride](https://github.com/hellerve/compfuck). The gist of it is that I wrote a
Brainfuck compiler that emits Carp code, in Carp. It’s entirely based on macros
and dynamic functions, meaning that it executes at compile time. Basically, you
have a compiler running inside another compiler.

The techniques I used to build this are not super advanced, but people seemed
interested in knowing how it works, so I decided to write a little tutorial
about it. You’ll learn a little bit about Brainfuck, a little bit about Carp,
a little bit about macros, and a little bit about compilers—or so I hope.

Let’s get this party started!

## Brainfuck and Carp, Partners in Crime

Because this blog post doesn’t intend to be a comprehensive introduction either
to Brainfuck or to Carp, I’m only going to talk about the parts of both
languages that we’re going to use for building our little compiler.

### The rules of the game: Brainfuck

We’ll quickly review the rules of Brainfuck for those of you unfamiliar with
it, though I’d really like to recommend you read [a more comprehensive
resource](https://en.wikipedia.org/wiki/Brainfuck).

Brainfuck is a language that emulates a tape, composed of a number of cells
containing numbers, and a tape head that looks at a given cell at a time—very
similar to a classic [Turing
machine](https://en.wikipedia.org/wiki/Turing_machine). Programs control the
tape head. The head can move forwards and backwards on the tape, increment and
decrement values in cells, read and write current values as ASCII characters
to the screen, and loop. This leaves us with eight operations in total, namely:

- `>`: Move forwards one cell on the tape.
- `<`: Move backwards one cell on the tape.
- `+`: Increment the value in the current cell.
- `-`: Decrement the value in the current cell.
- `.`: Print the value of the current cell as an ASCII character.
- `,`: Read in an ASCII character as a integer and put it into the current
  cell.
- `[`: If the value in the current cell is not zero, continue to the next
  character. If it is, jump to the matching `]`.
- `]`: If the value in the current cell is not zero, jump back to the matching
  `[`. If it is zero, continue to the next character. Together, the
  instructions `[` and `]` effectively form a `while (value != 0)` loop with
  the instructions between them as body.

For the purposes of this blog post, this is all you need to know about
Brainfuck.

### Carp at Compile Time

I’ve [written about](/Implementing_the_Builder_Pattern_in_Carp.html) [Carp
macros](/Carp_and_derive.html) before. They are similar to macros in other
Lisps, meaning that they are functions that run at compile time and are
often—but not always—used to generate code. This is perfect for our use case,
and we’re going to write a few macros and dynamic functions—compile time
functions that, unlike macros, evaluate their arguments—that act as our
compiler.

Carp also already has a few functions that manipulate strings at compile time,
which should come in handy for us. These functions have legitimate use
cases—inside the Carp standard library, we use it for the `fmt` macro, among
other things—, but today we’re going to bend them as far as they will.

The API that I envision for this blog post is simple. We will have one function
called `brainfuck-translate` that will serve as an entrypoint into our
compiler. It will take in a literal string and generate executable Carp code
that is then consumed by the Carp compiler to generate a binary that will
execute the Carp equivalent of the Brainfuck that was passed in originally.

```
; this will print all ascii characters
(brainfuck-compile "+[.+]")
```
<div class="figure-label">Fig. 1: The full API of our compiler.</div>

Now that we have a plan for attack, let’s write some code.

## A Translation Job

To start with, we’ll have to think about what we want to generate. Importantly,
we’ll have to generate a wrapper function that contains the Carp code. We could
name that function, or give the user the opportunity to name the function
themselves, but in this blog post, we’re just going to generate a `main`
function so that we can generate an executable without any hassle.

Our main function will also need to setup a tape and tape head for the program
to use. Traditionally, the tape is 30,000 cells long, and we represent it as
an array of bytes. Then the head just has to store the current index.

Let’s try and create that in a Carp macro!

```
(defmacro brainfuck-compile [prog]
  (list 'defn 'main []
    (cons 'let-do
      (cons (array 't '(Array.replicate 30000 &0)
                   'h 0)
        (brainfuck-compile-body prog 0)))))
```
<div class="figure-label">Fig. 2: The entry point.</div>

So, what happens when we run this, assuming that `brainfuck-compile-body` has
been stubbed out? We end up with a skeleton for a Brainfuck program that looks
like this:

```
(defn main []
  (let-do [t (Array.replicate 30000 &0) ; our tape
           h 0] ; our tape head
    ; our program goes here
  )
)
```
<div class="figure-label">Fig. 3: A compiled stub.</div>

Already, this is not bad! We have all the setup, and we know where our code must
go! With all of that out of the way, it is time to look at actually compiling
the code, a job for `brainfuck-compile-body`.

We’re going to iterate through the program string until we’ve reached the end,
compiling from start to finish. Sometimes we might need to jump back to the
start of the loop, so we pass in an index of where we currently are.

```
(defndynamic brainfuck-compile-body [prog pi]
  (if (= pi (String.length prog))
    '()
    (let [chr (String.char-at prog pi)]
      (if (= \] chr)
        '()
        (let [incr-c (brainfuck-compile-instruction
                        chr prog i)]
          (let [incr (car incr-c)
                compiled (cadr incr-c)]
            (cons compiled
                  (brainfuck-compile-body
                    prog (+ pi 1)))))))))
```
<div class="figure-label">Fig. 4: Compiling the body.</div>

Compiling the body is pretty straightforward as well: we check whether we’ve
reached the end of the string. If we have we terminate, returning an empty
list. If not, we take the character that we want to compile right now. If it’s
a closing parenthesis, then we terminate as well, because we’re inside a loop
body that was closed<sup>[1](#1)</a></sup>.

If none of the premature exit conditions apply, we compile the character using
`brainfuck-compile-instruction`. This will return a pair of an incrementor
value that will tell us by how much we have to advance the index—that will be
explained in more depth shortly—, and the compiled isntructions. We
desconstruct that pair using the `car` and `cadr` list operations, and recurse
into the next instruction. That way we’llincrementally build our instructions
until we end up with a full program.

At this point it might seem like we haven’t done an awful lot of compiling,
which is true. Most of the raw translation work happens in
`brainfuck-compile-instruction`, which is, at it’s core, extremely simple.
Let’s take a look, shall we?

```
(defndynamic brainfuck-compile-instruction [c prog i]
  (if (= \+ f)
    (list 1 '(Array.aupdate! &t h &(fn [x] (mod (Int.inc @x) 255))))
  (if (= \- f)
    (list 1 '(Array.aupdate! &t h &(fn [x] (max 0 (Int.dec @x)))))
  (if (= \> f)
    (list 1 '(set! h (+ h 1)))
   (if (= \< f)
    (list 1 '(set! h (- h 1)))
   (if (= \. f)
    (list 1 '(IO.print &(str (Char.from-int @(Array.nth &t h)))))
   (if (= \, f)
    (list 1 '(Array.aset! &t h (Char.to-int (IO.get-char))))
   (if (= \[ f)
    (let [end (search-matching-bracket
                  (String.suffix-string s (+ i 1)) 1)]
      (list (+ end 1)
        (list 'while '(/= &0 (Array.nth &t h))
          (cons 'do
            (let [subs (String.substring s (+ i 1) end)]
              (brainfuck-compile-body subs 0))))))
    '(1 ())))))))))
```
<div class="figure-label">Fig. 5: Compiling individual instructions.</div>

I’ve decided to use a bit of idiosyncratic formatting by putting all of the
nested `if` expressions on the same level, because it’s basically one big
`if-else` construct. At runtime, we could use `cond` for that, but since that
is a macro as well, we can’t use it at compile time. Bummer.

Let’s look at what the individual instructions compile to. `+` and `-` use
`Array.aupdate!` to increment and decrement the value under the tape head
in place, respectively. `>` and `<` just `set!` the value of the tape head.
`.` and `,` use the `IO` module to do their work. This is all pretty basic
stuff. The only thing that stands out here is `[`, which generates slightly
more complex code, namely a `while` loop. The body of that loop is generated
by `brainfuck-compile-body` again, which is pretty cute. Now the early return
of that function when encountering a `]` makes sense, too: we call the same
function for compiling the top-level program body as all the nested loop
bodies.

The incrementor that we return also falls into place now: since we’re taking
care of loops in a bulk, we need the higher level to jump past that loop when
we go back.

By noew we’re mostly done, but what about `search-matching-bracket`? It will
just hunt for the index of the matching bracket that closes our loop, and
return its index. It’s mostly plumbing, but I’ll dump it here anyway:

```
(defndynamic search-matching-bracket [s m]
  (if (Dynamic.or (= (String.length s) 0) (= m 0))
    0
    (let [f (String.char-at s 0)]
      (inc (search-matching-bracket
            (String.tail s)
            (if (= \] f) (dec m) (if (= \[ f) (inc m) m)))))))
```
<div class="figure-label">Fig. 6: Hunting for the matching bracket.</div>

Basically, it keeps track of its current loop depth by keeping an index. Once
that index is `0` or the string is empty<sup>[2](#2)</sup>, we’re done, if not
we’ll keep hunting. We get the first character and decrement the depth if it is
a `]` and increment if it’s a `[`, otherwise we leave it untouched. Then we
recurse using the rest of the string and the updated depth.

And that’s it! In just under x45 lines we wrote a compile-time Brainfuck
compiler!

## What’s Next?

While the code for this compiler is indeed very concise, it’s not exactly
pretty or graceful. If you wanted to make this better, you could tackle such
things as good errors or proper function names instead of compiling everything
into `main`. There is a lot to be desired here, but you can start from a
working prototype!

The compiler is also not exactly fast. The compile-time Carp evaluator is a
simple tree-walking interpreter, and it’s not optimized for speed. Still there
are some low-hanging fruits in speeding this up, like avoiding rescanning the
string when looking for closing loop parentheses. You can have pretty good fun
with that I think!

You could also turn this compiler into an interpreter almost without effort.
I’ll leave figuring out how—and maybe doing so—as an exercise to the reader.

## Fin

Today we built a compiler entirely in macros. While this sounds impressive,
when you get right down to it it was just a lot of string mangling, and a
little bit of emitting code. This is not actually all that uncommon: if you
don’t use a parser generator or framework, parsers tend to be extremely big.
I guess that means we need a compile-time parser combinator framework.

All jokes aside, building this thing was a lot of fun, and I hope it was also
fun to read and reason through it all. Have a good one, and see you soon!

#### Footnotes

<span id="1">1.</span> This leads to a compiler bug that will terminate the
                       compiler if there is an unmatched closing parenthesis,
                       but for the purposes of this blog post that’s good
                       enough.

<span id="2">2.</span> Like in [1](#1), this is actually a bug. We should
                       probably error out and tell our user that something went
                       wrong, but that would be handling errors gracefully, and
                       we don’t do any of that stuff around these parts.
