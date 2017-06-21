Because I enjoyed writing [my last blog post](http://blog.veitheller.de/Pattern_Matching,_A_Thing_Of_The_Past.html)
so much, I decided to give this format another shot. One of the reviewers
pointed me to [a chapter of Beautiful Code](http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html)
that details the implementation of what is possibly the most simple regular
expression engine ever, written by Rob Pike for educational purposes. The
chapter is written by Brian Kernighan, and it's a promising preview for the
book.

As in the previous post, the algorithm is suprisingly compact.
It's originally written in C, and the code is very nice to look at. I've
deviced to transcribe it into Scheme for the purposes of this blog post. If
you are familiar with C and aren't interested in Lisp I urge you to read the
original article, because it is very illuminating. If you're up for another
round of parenthetical goodness, though, I'm happy to provide it!

## What is it & how can I use it?

This fairly restricted regular expression engine tries to emulate `grep`
without the `-E` option provided, i.e. without enabling its extended regular
expression engine. Figure 1 shows which special characters are provided.
An important difference between most regular expression engines and this one
is that the `*` wildcard matches lazily. This stays true to the algorithm
shown in the book.

```
^: matches the start of the string
$: matches the end of the string
.: matches any one character
*: matches zero or more occurrences of the
   character that precedes it, lazily
```
<div class="figure-label">
  Fig. 1: The meta-characters provided by the algorithm.
</div>

You might ask yourself how the algorithm handles escaping. The short answer:
it doesn't. This means that literal matches of any of the characters are
impossible. This is most likely due to the pedagogical intention of the
implementation, as escaping would unnecessarily complexify the algorithm.

The interface provides a single function called `match` that takes a regular
expression and a string to match. It will return a boolean that signals whether
the match was successful. Figure 2 showcases the API by example.

```
(match ".*md" "i_am_markdown.md")       ; => true
(match ".*md" "i_am_not_markdown.html") ; => false
(match "^...chron" "anachronism")       ; => true
(match "^...chron" "parachronism")      ; => false
(match "^...chron$" "anachronism")      ; => false
```
<div class="figure-label">
  Fig. 2: Example usage of the algorithm.
</div>

While this is by far not a complete implementation of a regular expression
engine, it is useful enough to be interesting. This makes for a compelling
code golf exercise, and Rob Pike did a great job in keeping it terse yet
readable. In this blog post we will try and do the same for Scheme. The source
code is available [here](/assets/regexp.zp).

## The implemenetation

Before we start I have to warn you that this implementation is not
R5RS-compliant, meaning it will most likely not run on many implementations of
Scheme–it does run on zepto, though. The reason why I didn't restrict myself to
standard Scheme is pure convenience: `car`, `cdr`, and `length` in zepto are
generic functions that accept any type that implements the `traversable-collec`
protocol. Protocols in zepto are fairly interesting (and I might write a blog
post about them in the future). However, for the purpose of this blog post it's
sufficient to say that `car` will return the first character of the string
while `cdr` will return the string from the second character onwards. `length`
will return the length of the string.

The following definitions could serve as a drop-in replacement:

```
; substitute all occurrences of car with string-car
(define (string-car s) (string-ref s 0))

; substitute all occurrences of cdr with string-cdr
(define (string-cdr s) (substring s 1 (length s)))

; substitute all occurrences of length with string-length
```
<div class="figure-label">
  Fig. 3: Drop-in replacements for R5RS Scheme.
</div>

With this out of the way, let's implement the top-level function `match`.

```
(define (match regexp text)
  (if (eq? (car regexp) #\^)
    (match-here (cdr regexp) text)
    (let loop ((text text))
      (cond
        ((null? text) false)
        ((match-here regexp text) true)
        (else (loop (cdr text)))))))
```
<div class="figure-label">
  Fig. 4: The top-level matching function.
</div>

First we test whether the string begins with a `^` character, which forces the
match to begin at the start of the input. We defer the actual matching to a
function called `match-here`, which, as we now postulate, will return a boolean
signalling whether a match was found at exactly this location in the string. We
strip off the first character before passing it over because we've already
treated it, it's the `^`.

If there is no such character we try to apply `match-here` in a loop that
slices the first character of the text off progressively until we either run out
of text to match, in which case we return `false`, or we find a match.

```
(define (match-here regexp text)
  (cond
    ((null? regexp) #t)
    ((and (> (length regexp) 1)
          (eq? (cadr regexp) #\*))
      (match-star (car regexp) (cddr regexp) text))
    ((and (eq? (car regexp) #\$)
          (eq? (length regexp) 1))
      (null? text))
    ((and (not (null? text))
          (or (eq? (car regexp) #\.)
              (eq? (car regexp) (car text))))
      (match-here (cdr regexp) (cdr text)))
    (else #f)))
```
<div class="figure-label">Fig. 5: The `match-here` function.</div>

The `match-here` function is the bread-and-butter function of our algorithm.
It deals with most of the special cases and the regular case, and does most
of the heavy lifting.

We dispatch on a number of case, in descending order:

* If the regular expression is empty, we say that the match has succeeded.
* If the second character in our match is a `*` wildcard, we call `match-star`,
  which we haven't defined yet, with the first character (which is the character
  that is supposed to match), the rest of the regular expression, and the text.
* If we're at the end of the regular expression and encounter a `$` character,
  we're supposed to terminate the string. We therefore test whether `text` is
  empty.
* If we are supposed to match a regular character, we check for equality (if the
  character is a `.` wildcard, we skip that part). If they're equal we tail-call
  `match-here` with the tails of both the reular expression and the text to
  match.
* In all other cases the match has failed.

We get all of that logic in a handful of lines, which is pretty neat, if I
do say so myself. We almost have a working regular expression engine now. All
we have left to do is define a function `match-star` that takes care of the `*`
wildcard. How hard can it be, right? With the newly-found confidence we've
gained from defining `match-here` we try our hands on `match-star` and get:

```
(define (match-star chr regex text)
  (let loop ((text text))
    (cond
      ((match-here r t) #t)
      ((or (null? text)
           (and (not (eq? (car text) chr))
                (not (eq? chr #\.))))
        #f)
      (else (loop (cdr t))))))
```
<div class="figure-label">Fig. 6: An implementation of `match-star`.</div>

`match-star` is a looped function that, for every iteration, checks whether
the string currently under scrutiny can be handled by the `match-here` function.
This is where we rely on laziness for the `*` wildcard, because if `match-here`
can take over it will, regardless of whether `match-star` could also match the
input. If `match-here` cannot consume the input, we'll check whether the
wildcard can consume the first character instead, and do the check over again.

This is all of the code needed for the matcher! It is, much like the pattern
matching algorithm we looked at last time, reasonably simple in its idea.
Playing around with it reveals that it works quite well to boot.

## Exercises for the reader

Like last time I will propose a couple of exercises one could do to deepen
the understanding of the algorithm:

* Make the `*` wildcard matcher greedy
* Add `+` and `?`
* Make it iterative rather than recursive.

In a language with [tail-call optimization](https://en.wikipedia.org/wiki/Tail_call),
the recursive version is probably more performant while retaining its elegance.
In a language that lacks them, however, we might think of an iterative
version as a performance optimization—I doubt the algorithm will often run into
stack overflow problems.

## Fin

Once again, I marvel at the beauty of this finely crafted algorithm. I don't think
I could ever think of them myself, so I am grateful for the likes of Steele,
Sussman, and Pike for coming up with algorithms that please me aesthetically.

See you soon!
