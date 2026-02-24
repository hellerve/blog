---
title: "Scheme Macros X: Meta macros"
date: 2025-11-23
---

In this tenth installment of [my series on Scheme macros](https://blog.veitheller.de/scheme-macros/), we will be looking at some meta macros—macros that make the act of writing macros easier and cleaner.

Specifically, we’re going to look at macros for the following:

- Defining simpler macros more cleanly using `defmacro`,
- generating fresh symbols when we need them using `gensym`, and
- ensuring single evaluation of, for instance, macro arguments using `once`.

All of these are well-known macros and techniques, but we’ll be writing our own very simple implementations to gain an understanding of how an implementation like that could work. This means that for all of them, we are limiting their power. I’ll leave you with references to go deeper into a possible implementation for all of them. We’re not just going to scratch the surface, but we’re also not going spelunking today.

As always, we’re going to use my little (mostly dead) Scheme dialect [zepto](https://github.com/zepto-lang/zepto) for the implementation, but all of this should be portable with small tweaks.

## Act I: defining macros as a macro

I’ve written more `define-syntax` macros than I can remember, and I still find them awkward to write and look at. I understand their power, but I still prefer a simpler function-like definition most of the time. Something like this:

```
(defmacro my-when (test body ...)
  (if test
  	(begin body ...)
  	#f))
  
(my-when #t (write "hello"))
(my-when #f (write "nope"))
```

It has less power, but also less overhead, and captures our intent. It’s perfect for simple macros with a single clause and without literal sets. Luckily, we can define this short-hand quite easily.

```
(define-syntax defmacro
  (syntax-rules ()
    ((_ name (args ...) form)
      (define-syntax name
        (syntax-rules ()
          ((name args ...)
            form))))))
```

If you’ve followed along for this entire series, this should read almost naturally. In case the nested `define-syntax` throws you off, however, here’s an explanation: we define a macro that takes its argument and splices it into another macro definition, such that we get this transformation:

```
(defmacro my-when (test body ...)
  (if test
  	(begin body ...)
  	#f))
  	
; expands to
 
(define-syntax my-when
  (syntax-rules ()
    ((my-when test body ...)
      (if test
        (begin body ...)
        #f))))
 ```

It’s a nice little shorthand that makes reading the macro much easier!

If you want to learn more about how someone could implement a “full definition” of this, I encourage you to take a look at [how Racket implements `define-macro`](https://github.com/racket/racket/blob/8f0f634de986fe7f7c511477bff89711c615711a/racket/src/ChezScheme/examples/compat.ss#L204).

## Act II: generating symbols

Sometimes, we just need to generate a unique identifier. Macro hygiene handles this for us, but occasionally we will need to splice in an identifier into a piece of syntax. In a lot of Lisps, that is exactly what `gensym` does.

```
(defmacro x () (gensym))

(write (x))
(write (x))
```

The identifier printed out should be unique across calls to `x`.

Again, we can implement this quite simply with a counter:

```
(define gensym-prefix "GENSYM-")
(define gensym-counter 1000)

(defmacro gensym ()
  (begin
    (set! gensym-counter (+ gensym-counter 1))
    (string->symbol (++ gensym-prefix (->string gensym-counter)))))
```

Here, too, our newly minted `defmacro` makes things quite readable and short. All we do is increment our counter, append it to a prefix, and return the resulting symbol. It’s not exactly the cleanest code, but it approximates what every implementation of `gensym` does, including [the implementation for Carp by yours truly](https://github.com/carp-lang/Carp/blob/master/core/Gensym.carp).

In zepto, compile time and runtime are not separated and thread safety is not an issue, so this is fine. In other languages, this might be a bit more of a problem, which is why they choose a different implementation path.

For instance, [in Racket this is implemented in C](https://github.com/racket/racket/blob/8f0f634de986fe7f7c511477bff89711c615711a/racket/src/bc/src/symbol.c#L980), although it does essentially the same thing. Still, reading small definitions like these are good exercises to understand a virtual machine’s implementation, so if you are interested in how a Scheme might be implemented in C, this might help.

## Act III: ensuring single execution

Macros have the inherent problem of dealing with syntax and thus behaving differently than we are used to from functions. Consider this:

```
(defmacro square (x)
  (* x x))
	
(macro-expand '(square (+ 1 5)))
; => (* (+ 1 5) (+ 1 5))
```

Already we are duplicating work. Occasionally we’d like to be able to ensure a piece of code is really just evaluated once. Something like this:

```
(defmacro square (x)
  (once (x)
    (* x x)))
```

This macro is a classic known originally as `once-only`, implemented by the brilliant Peter Norvig. I first learned about it in the book [Let over Lambda](https://letoverlambda.com/index.cl/guest/chap3.html#sec_6) by Doug Hoyte.

Now, unfortunately we cannot do this in Scheme, since it would break hygiene, so we’d have to rely on something like this:

```
(define-syntax once
  (syntax-rules ()
    ((_ ((t1 e1) (t2 e2) ...) body ...)
     (let ((t1 e1) (t2 e2) ...) body ...))))

(defmacro square (s)
  (once ((t s))
    (* t t)))
```

Since this is boring and essentially boils down to a small wrapper around `let`, I believe we can do better if we are willing to throw hygiene under the bus and `eval` our way to success! This is left as an exercise to the reader.

But fret not! We will still explore this macro, just in a setting that is more suited to it and where hygiene is not a concern. I’ll reach for Carp, but you might just as well look at it in Common Lisp or Clojure.

```
(defndynamic replacerfn [arg]
  [(gensym) arg])

(defndynamic generate-let [acc replacer]
  (append acc [(list 'quote (car replacer)) (cadr replacer)]))

(defndynamic generate-reverse-let [acc replacer]
  (append acc [(cadr replacer) (list 'quote (car replacer))]))

(defmacro defmacro! [name args body]
  (let [replacer (map replacerfn args)]
      (eval
        `(defmacro %name %args
          %(list 'list '(quote let) (reduce generate-let [] replacer)
            (list 'let (reduce generate-reverse-let [] replacer)
                body))))))

(defmacro! square [y] `(* %y %y))

(eval (square (do (macro-log "hi") 10))) ; will print "hi" once

```

A few years ago, I implemented a version of this with a few more capabilities [for a talk at the Recurse Center](https://github.com/hellerve/talks/blob/45d68eeb969ca3e11063f7df767e4222e3dbe657/recurse-february-2021/examples.carp#L41-L49). The version here is derived from that. For `once`, attacking the code from various angles can help you get a bite on it.

This version essentially bakes `once` into the definition of `defmacro`, making our macro more robust against re-evaluation.

Nonetheless, I will explain it to you. It essentially relies on a two-way binding at different times. Let’s go by evaluation order instead of definition order. The inner `let` will be evaluated first. It binds all symbols to their respective gensyms. This will then be evaluated to rewrite the body—every occurrence of the original variable will be replaced by the gensym.

Let’s illustrate it (this is not exactly what happens, but might help with the intuition:

```
; original pass:
(defmacro! square [y]
  `(* %y %y))
  
 ; first pass:
 (defmacro square [y]
   (let [y 'gensym-symbol]
     `(* %y %y)))
     
 ; evaluated first pass:
 (defmacro square [y]
  `(* %gensym-symbol %gensym-symbol))
  ```

In the second pass, the outer `let` kicks in. It will be added to the definition rather than executed, and will bind the generated symbols to the original variable:

```
 (defmacro square [y]
  `(* %gensym-symbol %gensym-symbol))
  
 ; expanded:
 (defmacro square [y]
   (let [gensym-symbol y]
    `(* %gensym-symbol %gensym-symbol)))
 ```

And this is the code we finally end up with, ensuring that `y` will only be evaluated once, stored in a variable, and then rewritten.

If this was a bit much, don’t fret! This macro is often described as one of the pinnacles of the craft, and Peter Norvig wrote “If you can understand how to write and when to use once-only, then you truly understand macros.” It’s a macro worth studying, but one that feels slippery even after you’ve implemented it a few times in various languages.

## Fin

And that concludes today’s session on macros! We’re getting into truly advanced territory now, implementing macros to make writing macros more convenient.

I hope you enjoyed this session, I know I did! These macros are near and dear to my heart, and each one represents another layer of the macro onion unpeeled!

Let me know whether you liked this one, and if you have any more macro or language feature requests! I’ll be sure to put it in the backlog! See you around!
