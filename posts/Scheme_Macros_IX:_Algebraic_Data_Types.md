---
title: "Scheme Macros IX: Algebraic Data Types"
date: 2025-09-16
---

It’s been six and a half years since my last installment [in my series on Scheme macros](https://blog.veitheller.de/scheme-macros/), and I thought it was high time for another one.

If you’re new here (or, you know, you’ve only been around for the last six years and haven’t looked at the backlog), in this series we implement things usually reserved for language features as Scheme macros.

I haven’t written a Scheme macro in years. The last big one I’ve written is probably for my contribution to the paper [“Efficient module-level dynamic analysis for dynamic languages with module recontextualization”](https://dl.acm.org/doi/10.1145/3468264.3468574), and that was four years ago and using Racket. Time to combat atrophy, and do something cool! I dusted off [zepto](https://github.com/zepto-lang/zepto), the Scheme I built a decade or so ago, and got to work (to my surprise, it even compiled after some minor GHC-mandated updates).

I decided it is time to implement algebraic data types (ADTs, also known as sum types), i.e. types with multiple constructors, and a pattern matching API to go with it, all in about 27 lines of code. But be warned, it’s pretty dense!

## An API

First, let’s look at an API we want to implement. We want to be able to define a type like so:

```
(defadt Expr
  (Lit n)
  (Add x y)
  (Mul x y))
```

This defines a type `Expr` that knows how to construct literals, add, and multiply. An expression calculator could then look like this:

```
(define (eval-expr e)
  (match e
    ((Lit n) n)
    ((Add x y) (+ (eval-expr x) (eval-expr y)))
    ((Mul x y) (* (eval-expr x) (eval-expr y)))
    (else (error "bad Expr"))))
```

It pattern-matches on the expression type and binds into variables. As an extra challenge, I also want to be able to write guard expressions:

```
(match (Lit 5)
  ((Lit n) :when (negative? n) 'neg)
  ((Lit _) 'pos)
  (else 'not-a-lit))
```

These are arbitrary expressions that I can use for the components of my ADT.

And that’s about it! Let’s crack our knuckles and get to work!

## An implementation

Let’s start with the ADT. For the purposes of this blog post, we will ignore all other affordances Scheme might have for us (record types and the like), and we will conjure data types out of nothing but lists.

### Defining ADTs

```
(define-syntax defadt
  (syntax-rules ()
    ((_ Name) (begin))
    ((_ Name (Ctor field ...) rest ...)
     ; what do we do here?
     ))))
```

First we define a based case for our macro, since it will be recursive. Then we define the head clause of our main rule, where we take one of the constructors. What do we do with it, though? We need to at least define a function named after the constructor that constructs the data type, and probably also a function that checks for the data type. Taking our example from above and the `Lit` expression that takes a literal, we would define `(Lit <n>)` and `(Lit? <to-check>)`, then go to our next constructor. So, let’s do just that:

```
(define-syntax defadt
  (syntax-rules ()
    ((_ Name) (begin))
    ((_ Name (Ctor field ...) rest ...)
     (begin
       (define (Ctor field ...) (cons 'Ctor (list field ...)))
       (eval (macro-expand
       	`(define (,(string->symbol (++ (symbol->string 'Ctor) "?")) v)
       		(and (list? v) (eq? (car v) ','Ctor))))
       	(current-env))
       (defadt Name rest ...)))))
```

This is a bit ugly, mostly due to me forgetting most of how I do things in zepto. The first definition is easy, it literally just makes a function that takes all the fields and puts them in a list that we then tag with the name of the type, such that `(Lit 5)` would end up being encoded as `'(Lit 5)`.

So far, so good. The second definition is what I am not quite happy with, since it calls `macro-expand` and `eval` manually, a sign that we don’t know how to do something. My problem here was that I needed to construct the symbol `ConstructorName + ?` and splice that in the definition, and I forgot how to do it. In the end, the check is literally just a check whether we have a list and whether the constructor name is the first argument. Good enough for us.

We then recurse, removing the first constructor, such that we eventually reach the empty base case.

### Matching ADTs

The meat of our definition lies in the matcher, however. Let’s build a skeleton as before.

```
(define-syntax match
  (syntax-rules (:when else)
    ((_ e (else body ...)) (begin body ...))
    ((_ e ((Ctor vars ...) :when g body ...) rest ...)
       ; match with guard
       )
    ((_ e ((Ctor vars ...) body ...) rest ...)
       ; match without guard
       )
    ((_ e) (error "match: no clause matched"))))
```

We define a `match` macro with the special keywords `else` and `:when`. The `else` clause is easy, it just unconditionally executes the body. Likewise, a matcher without anything will just error. The complicated branches are the ones that actually do the matching, with or without guard.

We will need recursion once more, and we will start with the slightly simpler matcher without guard expression:

```
(define-syntax match
  (syntax-rules (:when else)
    ((_ e (else body ...)) (begin body ...))
    ((_ e ((Ctor vars ...) :when g body ...) rest ...)
     ; with guard
     )
    ((_ e ((Ctor vars ...) body ...) rest ...)
     (let ((tmp e))
       (if (and (list? tmp) (eq? (car tmp) 'Ctor))
           (apply (lambda (vars ...) body ...)
                  (cdr tmp))
           (match tmp rest ...))))
    ((_ e) (error "match: no clause matched"))))
```

Oof, that’s a handful. So first we create a temporary variable for our matchable expression (this makes sure that it gets evaluated once, not multiple times). We then check if it quacks like a duck (it is a correctly tagged list), and if so, we apply the value to an anonymous function that’s made up of the match variables and body.

Let’s go through an illustrative example:

```
(match (Lit 5)
  ((Lit x) (* x 5)))
  
 ; expands to (partial expansion before let etc)
 
 (let ((tmp (Lit 5)))
 	(if (and (list? tmp) (eq? (car tmp) 'Lit))
 		(apply (lambda (x) (* x 5)) '(5))
 		(error "match: no clause matched")))
```

You can also play around with it using `macro-expand` on your own time, but be warned: `let` and `and` are also macros, and the expanded expression is actually a bit more complicated!

The guarded clause is only slightly more complicated.

```
(define-syntax match
  (syntax-rules (:when else)
    ((_ e (else body ...)) (begin body ...))
    ((_ e ((Ctor vars ...) :when g body ...) rest ...)
     (let ((tmp e))
       (if (and (list? tmp) (eq? (car tmp) 'Ctor))
           (apply (lambda (vars ...)
                     (if g
                       (begin body ...)
                       (match tmp rest ...)))
                  (cdr tmp))
           (match tmp rest ...))))
    ((_ e ((Ctor vars ...) body ...) rest ...)
     ; without guard
    ((_ e) (error "match: no clause matched"))))
```

It essentially does the same thing, but has an `if` clause inside the `lambda` that will go to the rest of the match if the clause doesn’t hold.

Why there and not in the outside `if`? We need access to the bound variables, and we only have that inside of the `lambda` expression. The generated code will be quite ugly, but we do not really care, since noone should ever need to see it.

And that’s it! We’ve implemented a full ADT and pattern matcher.

## Next steps and exercises

A few potential next steps could be:

- Add a function that checks for the main algebraic data type (i.e. `Expr?`). This requires fiddling a bit with the recursion scheme in the macro.
- Unify the guarded and unguarded macro clauses. This requires rewriting the macro form to funnel one style into the other (probably unguarded into guarded using a `:when true` form).
- Add exhaustiveness checks in the `match` macro.
- Add nested destructuring for `match`.

There are even more things you could spend time on, like `let`-style destructuring and the like. The possibilities are endless!

## Fin

In this blog post, we implemented full ADT support and pattern matching in two macros and under 30 lines of code. Not too shabby, right?

Let me know if you want me to revive this series, I have a few more idea for funky macros up my sleeve!
