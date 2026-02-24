---
title: "Six Simple Sudoku Solvers II: Common Lisp"
date: 2025-09-18
---

Hello, friend! In case you are new here, this is a series about building six different Sudoku solvers in six different programming languages. [Check the first part](./Six_Simple_Sudoku_Solvers_I:_Python_(Reference).html) for more information on the algorithm as well as a reference implementation.

In this installment we will focus on an implementation in Common Lisp using the SBCL runtime, which is the one I am most familiar with. The dialects are not very far from each other, but the deeper you go into the stack, the more little hang-ups you will have, so I recommend following along using SBCL rather than another Common Lisp runtime.

What I want to show in this solver are how the uniqueness of Lisp macros helps explore the problem space—naturally—, but also how an advanced Common Lisp runtime allows the user to optimize things tremendously when needed without losing that expressive power.

Let’s dive right in!

## Why Common Lisp?

I have a bit of a bias towards the Lisps and Smalltalks of the world. Putting that aside, I do think it’s very instructive to see how a "high level" language with very many abstractions can still get very nitty gritty and optimized when the need arises, and how a good runtime encapsulates this such that it doesn’t feel like an escape hatch to a quasi-FFI.

And I like macros.

## The implementation

Okay, I’ll explain less about the solver’s algorithm this time. If you need a refresher, refer to the first part and check what we did there.

I’ll also hide a few functions that are only there for housekeeping (like copying boards, etc.). If you want to see them, check out [the repository](https://github.com/hellerve/sudoku). The full algorithm is around 130 lines with a Sudoku puzzles baked in, despite our best efforts to optimize the heck out of it.

### A primer on bitmasks

Our main data structure is still a two-dimensional array of numbers. In fact, we describe the type as `(simple-array (unsigned-byte 4) (9 9))`. In SBCL, specialized arrays are contiguous and unboxed, so indexing is fast and cache-friendly. It will likely pack into bytes, not bits, but that’s already quite good.

But this implementation comes with an optimization. Instead of storing candidates in a set, we store them as a small bitmask of 9 bits.

This is primarily an optimization. The board is a tiny grid, and our digits 1-9 can fit into 9 bits in a mask (yes, 4 bits if we used tightly packed numbers, but then the cost of our operations would go up).

We can then implement all set operations as a single expression of very few mathematical operations:

- Set addition becomes: `mask = mask | (1 << (digit - 1))`
- Set union becomes: `mask | othermask`
- Size becomes: `bitcount mask`

The rest can be encapsulated in macros so that the user won’t even notice much (you’ll get a taste of that below).

### Prologue

Before we start, a bit of ceremony.

```
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(deftype cell () '(unsigned-byte 4))
(deftype mask () '(unsigned-byte 16))
(defconstant +digits+ #b111111111)

```

This tells the compiler to optimize for speed and throw everything else out of the window, as well as defining some types. Already we’re looking at bits, so you know it’ll be good!

### The solver

As before, we will go over the high level solver function first.

```
(defun solve (b)
  (unless (propagate b) (return-from solve nil))

  (when (solved-p b) (return-from solve b))

  (multiple-value-bind (i j m) (find-mrv b)
    (when (null i) (return-from solve nil))
    (do-set-bits (d m)
      (let ((nb (copy-board b)))
        (setf (cell nb i j) d)
        (when-let (res (solve nb))
          (return-from solve res))))
    nil))
```

Yes, yes, I know that’s a lot of parentheses, let’s try to move past it. At a first glance, there isn’t that much difference from our reference implementation. We do that weird `do-set-bits` thing, though. What is that?

```
(defmacro do-set-bits ((d m) &body body)
  (let ((mm (gensym "MM")) (lb (gensym "LB")))
    `(do ((,mm ,m (logand ,mm (1- ,mm)))) ((zerop ,mm))
       (let* ((,lb (logand ,mm (- ,mm)))
              (,d  (integer-length ,lb))) ,@body))))

```

HOLY COW! What in the world is this?

Well, as we explained above in this solution we are working with bitmasks to represent our candidates. As such, we need a different way to iterate over them than the loop we had in our reference implementation. We could just write a loop, something like this:

```
(loop for d from 1 to 9
  when (logbitp (1- d) m) do
  ; do something to d
 )
 ```

This would be roughly equivalent, but it would be slow. Instead, the macros above implements a branchless walk that is faster. Doing it without a macro would result in something like this:

```
(do ((mm m (logand mm (1- mm)))) ((zerop mm))
       (let* ((lb (logand mm (- mm)))
              (d  (integer-length lb))) 
	      (let ((nb (copy-board b)))
    	    (setf (cell nb i j) d)
        	(when-let (res (solve nb))
          	(return-from solve res))))
```

But this didn’t really spark joy to look at, so I abstracted the mathy candidate search away and put it into a macro!

There are a few more custom macros in that tiny bit of code (`when-let` is one of them), but they are mostly plumbing, so I’ll leave that for the purposes of this blog post.

### Propagation

Next up is propagation, aka. a single solver attempt.

```
(defun propagate (b)
  (do-fixpoint (changed)
    (do-cells (i j)
      (when (zerop (cell b i j))
        (let ((m (candidates-mask b i j)))
          (when (zerop m) (return-from propagate nil))
          (when (single-bit-p m)
            (setf (cell b i j) (mask->digit m)
                  changed t))))))
  t)
```

What are `do-fixpoint` and `do-cells` here? You guessed right, more macros. I don’t think we need to explain those, though, since they just abstract away something that would otherwise distract from the code algorithm, as good macros should.

Generally, not much is different from our reference implementation in Python. We go through the cells until we hit a fixpoint in the board changing (i.e. there is nothing more to fill out), we get the candidates for each cell. If any cell has no candidates, the puzzle is unsolvable, if it is set to a single bit, we know we have a solution for that cell and set the marker for another scan of the board.

### The candidates mask

The helpers are also mostly straightforward.

```
(defun row-mask (b i)
  (with-mask (m)
    (dotimes (j 9)
      (let ((v (cell b i j)))
        (when (> v 0) (ior! m (digit->mask v)))))))

(defun col-mask (b j)
  (with-mask (m)
    (dotimes (i 9)
      (let ((v (cell b i j)))
        (when (> v 0) (ior! m (digit->mask v)))))))

(defun box-mask (b i j)
  (with-mask (m)
    (do-box (r c i j)
      (let ((v (cell b r c)))
        (when (> v 0) (ior! m (digit->mask v)))))))

(defun candidates-mask (b i j)
  (let ((v (cell b i j)))
    (logandc2 +digits+ (logior (row-mask b i)
                               (col-mask b j)
                               (box-mask b i j)))))
```

This is a lot of code, but it’s pretty straightforward. The math is a little different, because we are using bitmasks, but as last time what we have to do is look at the rows, columns, and boxes. The nice thing is that this time we don’t have to play around with sets and can rely on logical operations instead.

It looks like `with-mask` might be magic here, but remember that bitmasks are just integers, so a helper to create and return a mask just boils down to a one line macro.

```
(defmacro with-mask ((m) &body body) `(let ((,m 0)) ,@body ,m))
```

Do we really need that? Possibly not, but it reduces noise that distracts from the algorithm. By just saying “build me a mask and return it” we can remove some ceremony from our mask functions and, again, focus on the algorithmically interesting sections.

More than that, however, I also use macros for little functions to ensure inlining. Some examples:

```
(defmacro mask->digit (m) `(integer-length ,m))
(defmacro digit->mask (d) `(ash 1 (1- ,d)))
(defmacro ior! (place expr)
	`(setf ,place (logior ,place ,expr)))
```

These all could be functions, but this way I ensure things are inlined but I still retain the abstraction. This is definitely a matter of preference, but for this article I decided to go for it just to show you what is possible.

## MRV candidate selection

The only thing that is left to do is our candidate selection for depth-first search.

```
(defun find-mrv (b)
  (let ((best-i nil)
        (best-j nil)
        (best-m 0)
        (best-k 10))
    (do-cells (i j)
      (when (zerop (cell b i j))
        (let* ((m (candidates-mask b i j))
               (k (logcount m)))
          (when (< k best-k)
            (setf best-k k best-i i best-j j best-m m)))))
    (values best-i best-j best-m)))
```

All we do is go through all cells, collect the number of candidates for all cells that are still `0`, and return the index with the lowest number. It’s probably the most imperative and straightforward bit of code (I’ll be honest, I got a little tired of trying to be clever and elegant).

The thing is, even if Common Lisp—rightfully or not—has this reputation of attracting people with a predilection for being clever, you really don’t have to be. You can write straightforward functional or imperative code if you want to, and the compiler will do a decent job of compiling either into reasonable instructions. There are facilities to be really inventive when you need to—or when the mood strikes—, but this isn’t always necessary or advisable.

## Fin

And there we have it! A sudoku solver in Common Lisp that might defy a few prejudices you might hold, apart from the parentheses.

We wrote some mathy bit-twiddling code, admittedly macro-heavy, and we produced some reasonable machine code (load the file and call `disassemble` on any of the symbols to check it out!).

I want you to walk away with two main points: firstly, that representing known small-to-medium sized candidate  sets as bitmasks might be worth it if performance is extremely important. Secondly, that macros, whether it’s in Common Lisp or any other language, do not need to be used to write DSLs. Often, they can just be a handy tool to remove parts of the code that distract from your ideas, or even as an optimization technique.

See you next time for the opposite end of the abstraction ladder when we will look at Prolog and rely on a system designed to backtrack and solve constraints to solve all our problems for us!
