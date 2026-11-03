---
title: "A Second Futamura Projection"
date: 2026-11-03
---

[Two months ago](/A_First_Futamura_Projection.html), we turned
[compfuck](https://github.com/hellerve-pl-experiments/compfuck) back into an
interpreter and wrote a specializer that, given a Brainfuck program, folded
the interpreter away and left behind the operations we couldn’t specialize.
That’s the first Futamura projection, which is what I likened compfuck to
historically in lazy shorthand. If you missed the article and this whole
paragraph refuses to make sense to you, start there!

Of course, that article was only the first step. We’re not done with our
Origami piece.

Specifically, we ended up with a way to obtain compiled programs without
writing instruction translations by hand. That’s great, but we still ran the
specializer on every single program, walking through the interpreter’s source
each time. The specializer and the interpreter were always the same, though, we
just had a different Brainfuck program. This is wasteful and we can do better.

You might also have a hunch about how to deal with this now: if part of the
input is fixed, we can specialize. And the second Futamura projection does
exactly that to the specializer itself:

```text
compiler = specialize(specializer, interpreter)
compiler(program) = specialize(interpreter, program)
```
<div class="figure-label">Fig. 1: Specializing a specializer to an interpreter.</div>

We feed the specializer to itself, with the interpreter as its known input.
What comes out is a program that takes a Brainfuck program and produces the
residual code directly, without ever executing the interpreter. That’s a
compiler, delightfully weird.

It might sound like a parlor trick, and it took me a while to believe it would
produce anything sensible, even after the time it took for me to understand it.
But it does work, and the compiler we get at the end looks a lot like the one I
wrote by hand years ago. Let’s see how we get there.

## Why our old specializer won’t do

To specialize a program, the specializer has to understand it. Our old
specializer understood exactly one program, the interpreter. For instance, it
knew that `p` and `i` were static and that every `if` and `let` belonged to the
source traversal. We decided a whole bunch of things by hand, basically making
a mini DSL for Carp-like sources.

This won’t scale to the specializer itself. It has static and dynamic parts
mixed in a bunch of places, it calls helper functions, and its recursive calls
don’t all terminate if you naively unfold them. So the first step is to write
a specializer that is general enough to specialize itself, without tricks this
time.

There is a second, sneakier problem I glossed over a bit last time, because I
wasn’t quite sure yet how to solve it when I wrote that piece: the old
specializer evaluated static expressions by handing them to Carp’s `eval`. If
we specialize that, the best we can do is leave a call to `eval` in the output,
and our “compiler” would still interpret pieces of the interpreter at compile
time. To get rid of that, the specializer needs to evaluate static code itself,
in a way that can be specialized in turn. If that doesn’t make sense yet, no
worries. It’s easier to show than to tell.

Just one more thing before we continue, though, because I want to be honest:
for the purposes of this tutorial, I opted not to make this an absolutely
generic implementation of the second Futamura projection. We’ll still cut
corners where it’s expedient without undermining the conceptual purity. I’ll
also call it out when it happens.

## An introspective specializer

Last time, `definterpreter` stored the interpreter’s body as metadata. Now a
macro called `remember` stores the parameters as well, and `defmix`—`mix` is
the name handed down by the forebears of this technique—does the same for the
specializer’s functions and collects them in a list. That list is the
specializer as a program. The bracket search is stored like the interpreter,
so the interpreter is now a program with two functions, an interpreter loop
and a helper function.

Carp expands macros before the body gets stored, so `case`, `cond`, `and`, and
`or` arrive as `if`s and `let`s<sup><a href="#1">1</a></sup>. Our specializer
only has to understand `if`, `let` with a single binding, `quote`, function
calls, and a handful of primitives. It’s basically a small Lisp, and the
specializer is written in it, too (sans the macro’s syntactic sugar).

### Which bits are static?

We keep last time’s split into static `p` and `i` and dynamic `t` and `h`,
but replace the hand-made decisions with a rule:

```clojure
(defmix mentions? [e syms]
  (cond
    (symbol? e) (member? e syms)
    (or (list? e) (array? e))
      (if (empty? e) false (or (mentions? (car e) syms) (mentions? (cdr e) syms)))
    false))
```
<div class="figure-label">Fig. 2: Does an expression use any of these variables?</div>

An expression that doesn’t mention any *dynamic* variables can be evaluated
during specialization, and an expression that doesn’t mention any *static*
variables has nothing to specialize, so we leave it alone. Everything else
gets taken apart.

The rule is purely syntactic, so it has plenty of blind spots. An expression
that only mentions static variables gets run during specialization even if it
has side effects. Neither of our programs has one, so we’ll leave it at that.
Its semantic generalization is left as an exercise to the reader.

### Our very own evaluator

Before we can fold things, we have to evaluate.

Last time, `known` handed static expressions to `eval`. We need to get rid of
that `eval`, though, so we write an evaluator in and for our small Lisp:

```clojure
(defmix ev [prog e names vals]
  (cond
    (symbol? e) (List.nth vals (index-of e names))
    (not (list? e)) e
    (= (car e) 'quote) (cadr e)
    (= (car e) 'if)
      (if (ev prog (cadr e) names vals)
        (ev prog (caddr e) names vals)
        (ev prog (cadddr e) names vals))
    (= (car e) 'let)
      (ev prog (caddr e) (cons (car (cadr e)) names)
          (cons (ev prog (cadr (cadr e)) names vals) vals))
    (empty? (lookup (car e) prog))
      (ev-primitive prog e names vals)
    (memo (ev prog
              (caddr (lookup (car e) prog))
              (cadr (lookup (car e) prog))
              (ev-each prog (cdr e) names vals)))))
```
<div class="figure-label">Fig. 3: Evaluating static code.</div>

Semantically, this is easy, but it’s a lot of `car`ing and `cdr`ing, so
let’s go through it in turn:

- If we encounter a symbol, we look up the variable for it.
- If we encounter a non-list, we return it as is.
- If we encounter a quoted value, we return the value.
- If we encounter an `if`, we evaluate the condition, and then evaluate the
  corresponding branch.
- If we encounter a `let`, we add a new name and value, and the right side
  also gets evaluated for good measure.
- If we encounter a symbol that’s not in the program, we try to evaluate a
  primitive.
- Otherwise we evaluate the function and call `memo`. We get to that in a
  second, it’s my favorite little hack of the whole thing.

Let’s check out `ev-primitive` for good measure.

```clojure
(defmix ev-primitive [prog e names vals]
  (cond
    (= (car e) 'car) (car (ev prog (cadr e) names vals))
    (= (car e) 'cdr) (cdr (ev prog (cadr e) names vals))
    ; ... and so on, for 18 primitives
    (= (car e) 'String.char-at)
      (String.char-at (ev prog (cadr e) names vals) (ev prog (caddr e) names vals))
    (macro-error (str "unknown primitive " (car e)))))
```
<div class="figure-label">Fig. 4: Applying primitives.</div>

The primitives are what both programs need: a few list operations,
comparison, a bit of arithmetic, and string access. Anything else, including
the specializer’s own helpers, is a function in the program and gets
evaluated from its source. We cheat a bit here, and map this Lisp’s operations
one-to-one to Carp, which is nice, because it’s trivial, but also a bit
horrendous, because, well, look at the code.

Last time’s bindings array is now split into `names` and `vals`. When we
specialize the specializer, `names` is known and `vals` is not, so a lookup
like `(index-of e names)` happens ahead of time and only `(List.nth vals 2)`
is left over. With names and values in one list, every lookup would have to
search at compile time.

To see this in action, here is the body of `ev` specialized to the expression
`(+ i 1)`, with `i` static:

```clojure
(+ (List.nth vals 0) 1)
```
<div class="figure-label">Fig. 5: An evaluator, specialized away.</div>

The evaluator is gone. What’s left is the expression itself, reading `i` from
whatever slot the compiler keeps it in. The dispatch over primitives
disappeared, because which primitive we apply is known.

If you take only one thing away from this post, it should probably be
Figure 5. It’s our whole trick! But we’re not done yet.

### Walking the code

The specializer proper is called `mix` (a nod at those before us, again). It
takes the program, the variables, an expression, and the environment (as two
lists), and returns residual code:

```clojure
(defmix mix [prog dyn e names vals]
  (cond
    (not (mentions? e names)) e
    (not (mentions? e dyn)) (lift (ev prog e names vals))
    (and (= (car e) 'if) (not (mentions? (cadr e) dyn)))
      (if (ev prog (cadr e) names vals)
        (mix prog dyn (caddr e) names vals)
        (mix prog dyn (cadddr e) names vals))
    (= (car e) 'let)
      (mix prog dyn (caddr e)
        (cons (car (cadr e)) names)
        (cons (ev prog (cadr (cadr e)) names vals) vals))
    (= (car e) 'memo) (mix-memo prog dyn (cadr e) names vals)
    (empty? (lookup (car e) prog))
      (mix-each prog dyn e names vals)
    (unfold prog
            dyn
            (lookup (car e) prog)
            (cdr e)
            (cadr (lookup (car e) prog))
            (cdr e)
            names
            vals)))
```
<div class="figure-label">Fig. 6: The specializer.</div>

Again: quite a lot of `car`ing and `cdr`ing, so let’s take it slow.

The first two lines are the rule we mentioned above. Code without static
variables is returned as-is. Code without dynamic variables gets evaluated,
and `lift` turns the value back to code, quoting it if necessary.

`memo` is a special case that calls `mix-memo`. We’ll defer it one more time.

The rest are the same branches as last time, except that an `if` only picks a
branch if its condition is static, and calls to functions in the program are
unfolded. It looks quite odd, but if you squint, you might see the resemblance.

### Unfolding and a stop condition

`unfold` is the unfolding of the interpreter’s recursive call we had last time,
but generalized for any function in the program:

```clojure
(defmix unfold [prog dyn f args params rest names vals]
  (cond
    (empty? params)
      (let [static (static-params (cadr f) args dyn)]
        (memo (mix prog
                   (dynamic-params (cadr f) static)
                   (caddr f)
                   static
                   (static-args prog args dyn names vals))))
    (or (not (mentions? (car rest) dyn))
        (= (car params) (car rest)))
      (unfold prog dyn f args (cdr params) (cdr rest) names vals)
    (list
        'let
        (array (car params)
               (mix prog dyn (car rest) names vals))
        (unfold prog
                dyn
                f
                args
                (cdr params)
                (cdr rest)
                names
                vals))))
```
<div class="figure-label">Fig. 7: Unfolding a call.</div>

Let’s start at the bottom this time, in the catch-all call to `list`. It means
that dynamic arguments get a `let`. This happens unless the caller passes a
variable of the same name (condition just above).

Last time we needed to insert fresh names here, but our interpreter always
passes `t` and `h` on unchanged, so with the condition above we just skip
the `let` entirely<sup><a href="#2">2</a></sup>.

But there’s that `memo` thingie again. What is it?

When the specializer runs normally, `memo` is the identity function and does
nothing.

```clojure
(defndynamic memo [x] x)
```
<div class="figure-label">Fig. 8: `memo` in our regular world.</div>

But as we saw above, when the specializer reads a call wrapped in `memo`,
it calls `mix-memo` instead.

```clojure
(defmix mix-memo [prog dyn call names vals]
  (let [f (lookup (car call) prog)]
    (cons (residual prog f
                    (static-params (cadr f) (cdr call) dyn)
                    (static-args prog (cdr call) dyn names vals))
          (dynamic-args prog dyn (cdr call) names vals))))
```
<div class="figure-label">Fig. 9: `memo` as seen by the specializer.</div>

`mix-memo` doesn’t unfold. Instead, it generates a separate function
specialized to the call’s static arguments, remembers it, and emits a call to
that function.

We need this because of what happens when the specializer specializes itself.
When we unfold `mix` on the interpreter’s body and reach the interpreter’s
recursive call, `unfold` calls `mix` on the interpreter’s body again. With
the interpreter’s source known, that is the same specialization we started
with, and unfolding it would go on forever. The interpreter never had this
problem, because each of its recursive calls moved to a new, known position
in the Brainfuck program. Here, we need an explicit base case marker.

The same happens in `ev`. When we specialize the evaluator, it meets calls
whose arguments aren’t known yet, like the bracket search. Unfolding those
wouldn’t stop either, so they get their own functions as well.

And that’s the whole reason `memo` appears in the recursive and mutually
recursive cases that are not clearly reductive. Once we’ve seen a call before
and specialized it, we just emit a call to the already known name, closing
the loop.

Deciding where to place these markers is, as far as I understand, one of the
harder problems in partial evaluation. I put the two we need there by hand.
Doing this automatically is, say it with me now, left as an exercise to the
reader.

## Two projections

We are ready to project now! The first one, with the new specializer:

```clojure
(mix interpreter '(t h) (caddr (car interpreter)) '(p i) (list ",[.-]" 0))
; => (do (Array.aset! t h (IO.Raw.get-char))
;        (do (while (/= (ref 0) (Array.unsafe-nth t h))
;              (set! h (do (IO.print (ref (str (Char.from-int (copy (Array.unsafe-nth t h))))))
;                          (do (Array.aupdate! t h (ref (fn [x] (max 0 (Int.dec x))))) h))))
;            h))
```
<div class="figure-label">Fig. 10: The first projection, again.</div>

Alright, looking “good”. Looks like the specializer is working. But that’s
the easy case, right?

For the second case, we hand the specializer to itself.

```clojure
(defdynamic compf-compiler
  (let [specializer (mix-source)]
    (residual specializer (lookup 'mix specializer) '(prog dyn e names)
      (list interpreter '(t h) (caddr (car interpreter)) '(p i)))))
```
<div class="figure-label">Fig. 11: The second projection.</div>

To understand the call, let’s recap:

- `residual` is the function behind `memo`. It specializes a function in a
  program to values for some of its parameters.
- Here the program is the specializer, the function is `mix`, and the known
  values are the interpreter, its dynamic variables, its body `e`, and the
  `names` (`p` and `i`).
- The only thing left unknown is the values of `p` and `i`.

Two functions come out. The first is the compiler, and I’ll give it to you
lightly abridged and re-indented, but true to its mindboggling form:

```clojure
(defndynamic mix1055 [vals]
  (if (= (List.nth vals 1) (String.length (List.nth vals 0)))
    (quote h)
    (let [vals (cons (String.char-at (List.nth vals 0) (List.nth vals 1)) vals)]
      (if (= (List.nth vals 0) \])
        (quote h)
        (let [vals (cons (if (= (List.nth vals 0) \[)
                           (- (ev1056 (cons (List.nth vals 1) (cons (inc (List.nth vals 2)) (cons 1 (quote ())))))
                              (List.nth vals 2))
                           1)
                         vals)]
          (cons (quote do)
            (cons (let [vals (cons (List.nth vals 1) vals)]
                    (if (= (List.nth vals 0) \+)
                      (quote (Array.aupdate! t h (ref (fn [x] (mod (Int.inc x) 255)))))
                      ; ... the same for \-, \>, \<, \., and \,
                      (if (= (List.nth vals 0) \[)
                        (cons (quote while)
                          (cons (quote (/= (ref 0) (Array.unsafe-nth t h)))
                            (cons (cons (quote set!)
                                    (cons (quote h)
                                      (cons (mix1055 (cons (List.nth vals 3) (cons (inc (List.nth vals 4)) (quote ()))))
                                            (quote ()))))
                                  (quote ()))))
                        ())))
              (cons (mix1055 (cons (List.nth vals 2) (cons (+ (List.nth vals 3) (List.nth vals 0)) (quote ()))))
                    (quote ())))))))))
```
<div class="figure-label">Fig. 12: Behold, a compiler.</div>

This takes some squinting (your eyes must be tired by now), but bear with me.
`vals` starts out as the program and the position. Each `let` in the
interpreter `cons`es its value onto the front, so after the first one,
`(List.nth vals 0)` is the current character<sup><a href="#3">3</a></sup>.

The first lines check for the end of the program and a closing bracket, and
return the symbol `h`. Then we dispatch on the character, and each branch
returns a quoted piece of code: `+` returns the tape update, `>` returns
`(set! h (+ h 1))`, and so on. The loop builds a `while` around a recursive
call that compiles its body. Finally, we compile the rest of the program, and
put both into a `do`. Easy-peasy, or something like that.

The second function is the bracket search:

```clojure
(defndynamic ev1056 [vals]
  (if (if (= (List.nth vals 1) (String.length (List.nth vals 0))) true (= (List.nth vals 2) 0))
    (List.nth vals 1)
    (let [vals (cons (String.char-at (List.nth vals 0) (List.nth vals 1)) vals)]
      (ev1056 (cons (List.nth vals 1)
                (cons (inc (List.nth vals 2))
                  (cons (if (= (List.nth vals 0) \]) (dec (List.nth vals 3))
                          (if (= (List.nth vals 0) \[) (inc (List.nth vals 3)) (List.nth vals 3)))
                        (quote ()))))))))
```
<div class="figure-label">Fig. 13: Bracket search. Ain’t she pretty?</div>

`search-matching-bracket` wasn’t even in our list of required parts. During
the first projection, our evaluator runs it from source. When we then
specialized the specializer, the evaluator got specialized to its body, and
it spit out a compiled version.

The interpreter is gone, and so is the specializer. There is no machinery,
no `ev`, `mix` or source walks. What’s left is a function from Brainfuck
to Carp that dispatches on characters and returns quoted code.

What’s funky to me is that here is the core of the compiler I wrote by hand
[back in 2019](/Compiling_at_Compile_Time.html):

```clojure
(defndynamic compile-compf-char [c s i]
  (case c
    \+ (list 1 '(Array.aupdate! &t h &(fn [x] (mod (Int.inc x) 255))))
    \- (list 1 '(Array.aupdate! &t h &(fn [x] (max 0 (Int.dec x)))))
    \> (list 1 '(set! h (+ h 1)))
    ; ...
```
<div class="figure-label">Fig. 14: Our hand-written compiler.</div>

It’s the same idea! Across two blog posts, we started with that compiler that
I got from hand-dynamicizing an interpreter, turned it back into an
interpreter, and specialized our way back to it.

It took a lot more machinery than writing it by hand, but it automatically
determined which code to emit for each character. And that’s our origami in its
full glory!

## Fin

This was quite a bit more work than the first projection.

It’s not even completely general, some of the decisions are still mine: which
variables are static, which primitives exist, and where to put `memo` all
needed a guiding hand in our implementation (though as I alluded to, they’re
not insurmountable). We also make a bunch of assumptions about the code playing
nice and coloring inside our lines, which we probably shouldn’t in the real
world.

But I find it remarkable that it works at all! We wrote an interpreter and a
specializer. Neither of them knows anything about compiling Brainfuck. But
feed one to the other, and a compiler just falls out, plus a bracket
search that was needed but didn’t get asked for.

There is, of course, a third projection, where we specialize the specializer
to itself and get a program that turns interpreters into compilers. I’m not
going to promise anything. Our specializer would have to unfold its own memo
machinery, and I have a feeling that would get ugly quick. This implementation
also already took twice as long to make as the last one (figuring out how
to make it explainable was most of the work), and I might not have enough time
on my hands for more card tricks. But maybe I do.

See you soon!

#### Footnotes
<span id="1">1.</span> Last time I said that the stored source is the
interpreter before Carp expands it, but that wasn’t true. Carp expands the
macros in the code a macro returns before running it, and that includes the
body we hand to `meta-set!`. Whoops. Our old specializer handled that just
fine, it simply never entered its `case` branch and I didn’t notice.

<span id="2">2.</span> Strictly speaking, the unfolded code now shares `h`
with its caller, and an assignment inside it changes the caller’s `h`. That’s
fine for our interpreter, because every call hands its `h` back and the
caller continues with that. We could also just bind it again, but
`(let [h h] ...)` compiles to `int h = h;`, which C rightfully rejects. That’s
why last time’s version went through a `gensym`, but we can just avoid the
whole `let` instead!

<span id="3">3.</span> The third `let` is the one from the expanded `case`
statement, which binds the current character once more. That’s why the
dispatch reads `(List.nth vals 0)` again after it.
