When I started my [reading list](https://github.com/hellerve/ptolemy)
in January of this year, I decided to give rereading the
[Lambda Papers](http://library.readscheme.org/page1.html)
a shot. To the uninitiated: in the latter half of the 1970s, Guy Steele and
Gerald J. Sussman worked on Scheme. This was an extremely productive time for
both of them, and they published a series of now-famous papers collectively
dubbed the “Lambda Papers”. They are mostly concerned with Scheme and how it
compares to other languages of the time, and neat tricks that they discovered
were possible while working on it.

I've reread a good amount of the papers by now, and they are all as excellent as
I remembered—maybe even more so now that I've gathered a fair amount of knowledge
of what makes a Scheme and how to program in it. But most importantly I've
gained an appreciation for sections that went unappreciated when I first read
the papers. Today I want to talk about one of them, found in
[“Scheme, an Interpreter for Extended Lambda Calculus”, pages 10–12](http://repository.readscheme.org/ftp/papers/ai-lab-pubs/AIM-349.pdf).
This paper is startling for a bunch of reasons, the most anxiety-inducing of
which is that Guy Steele was 21 years old when this paper was published, and
it is ground-breaking in more than one respect.

The part I want to talk about is not about one person's gifts, however. I want
to talk about an algorithm that implements a very capable pattern matching
system in 38—admittedly very dense—lines. I didn't understand it the
first time I read the paper, but the second time it completely blew my mind. In
this post, I want to attempt to walk you through the original source, which I do
not think is still runnable in modern implementations of Scheme. If you want to
follow along using a slightly more modernized version, you can download
[an attempt at a transcript into zepto](/assets/patternmatch.zp). Excuse the
plug, but I like to put my language to use every once in a while.

## Consumer information

Before I explain the algorithm I want to show you the API. It exposes only one
function, aptly called `match`, that is relatively simple and intuitive.
Let's first look at the examples from the paper, and then construct our own,
to make sure we understood the interface.

This first example is fairly involved and it's okay if it doesn't make sense at
first glance–especially since the authors didn't choose a very simple example–,
but I'm using it because here it's the only example in the paper.

```
; from the paper
(match '(A !B ?C ?C !B !E)
       '(A X Y Q Q X Y Z Z X Y Q Q X Y R))
; => (((E (Z Z X Y Q Q X Y R))
;      (C Q)
;      (B X Y))
;     <continuation1>)

(<continuation1>)
; => (((E (R))
;      (C Z)
;      (B (X Y Q Q X Y)))
;      <continuation2>)

(<continuation2>)
; => nil
```
<div class="figure-label">
  Fig. 1: A few examples of function invocations and their result.
</div>

Let's go through the invocations and the return values one by one.

First, let's observe that the prefixes hold real semantic meaning. The `?`
character will match any one character, similar to `.` in regular expressions.
The `!` character, on the other hand, holds the same meaning as `.*` in regular
expressions, i.e. it will match zero or more of any character. But, unlike in
regular expressions, it will match non-greedily. This means that the match
expression, transcribed into PCRE, would be `A (.*) (.) \2 \1 (.*)`. Don't worry
if that doesn't tell you much yet, I have another example that you can try to
wrap your heads around.

It is important to note at this point, however, that these special prefixed
symbols are similar to capture groups. These groups will make up the return
value of the `match` function.

```
E=Z Z X Y Q Q X Y R
C=Q
B=X Y
```
<div class="figure-label">Fig. 2: The match groups.</div>

For now, let's try to decypher the return values. It gives us back a two-element
list, the first element of which is another list. The second element is a
continuation that we'll ignore for now. The nested list represents the matched
captured groups and contains pairs of names and the matched values. It will
tell us that it bound `C` to `Q`, `E` to a long list, and `B` to `X Y` in the
first call. Figure 2 tries to glue those back together, which will,
unsurprisingly, result in the input list.

```
input: (A !B ?C ?C !B !E)
substituting B: (A X Y ?C ?C X Y !E)
substituting C: (A X Y Q Q X Y !E)
substituting E: (A X Y Q Q X Y Z Z X Y Q Q X Y R) == input
```
<div class="figure-label">Fig. 3: Rebuilding the input from the matches.</div>

Now, let's look at that continuation business. The contiunation will actually
give us the facilities to try the same algorithm again, and try to match
different values. If that's possible, calling the continuation will give us
the next possible match. In case of failure it will return `nil`.

```
; Exercise: Can you figure out what the function
; should return, given the following inputs?
(match '(A !C B) '(A B C D E X X S B))

(match '(A !C !C ?B !D ?B) '(A F D A D F))

(match '(?X !Y ?Z ?Z !Y) '(A B B C D D B B C))
```
<div class "figure-label">Fig. 4: An exercise for the reader.</div>

That just about covers the API of the algorithm, and, looking at just that, we
could be tempted to assume that the algorithm is long, complex, and daunting to
understand—at least that's what I did. It's rather short, though, and completely
understandable. Let's try to reimplement it, shall we?

## A setup of sorts

Before we start with the actual algorithm, we need a few helper functions. Those
are given up front in the paper, and so I shall do the same, because they're
generally useful. The helper functions are `nfirst` and `nrest`, respectively,
which people familiar with functional languages may know as `take` and `drop`.
Everyone else will be happy to hear that these algorithms, given a list and a
number `n`, just take the first `n` elements of the input list, or drop them
and return the rest. I'll give to alternative implementations of each, one from
the paper, and one in zepto.

```
; Original: nfirst
(define (nfirst e n)
  (if (= n 0)
    nil
    (cons (car e) (nfirst (cdr e) (- n 1)))))
; zepto: nfirst
(define nfirst (flip take))

; Original: nrest
(define (nrest e n)
  (if (= n 0) e (nrest (cdr e) (- n 1))))
; zepto: nfirst
(define nrest (flip drop))
```
<div class="figure-label">
  Fig. 5: Two implementations of `nfirst` and `ndrop` each.
</div>

A few notes seem in order at this point: firstly, I'd like to say that I
modernized the example given in the paper very slightly. Specifically, I introduced
the first-class definition of functions, as opposed to binding a lambda to a
name, because I consider it readable. Secondly, the zepto version is cheating
a bit by using modern functional primitives to do the dirty work. Those
functions are, however, tail-recursive and safe by default, so it is a big win.
It's also shorter to use them.

Before moving on, I want to address why more modern languages
decided to flip the argument list, moving from having the list as the first
argument to having the number first. The issue being addressed here is
composability. It turns out that you want to express `give me the first 5
elements of a given list` more often than `give me the first n elements of list
[1..n]`. We can curry—that is, partially evaluate—functions, and
define `first5` as `(curry take 5)`. In short, we learned how to be more
modular and pattern-oriented, which is what ~~object-oriented~~ functional
programming is all about.

Now, depending on which Scheme you are running, you either have reader macros
at your disposal or you don't, both of which are fine. The original paper
assumes there is a reader macro that transforms symbols with leading `?`
characters into a list of the form `(THV <symbol>)` and symbols with leading
`!` characters into a list of the form `(THV* <symbol>)`. For reasons of
compatibility I will instead define a few matching functions, that, in zepto
form, look like this:

```
(define (? sym) (eq? #\? (car (symbol->string sym))))
(define (! sym) (eq? #\! (car (symbol->string sym))))
(define (plain? sym) (not (or (? sym) (! sym))))
(define to-plain (compose string->symbol
                          cdr
                          symbol->string))
```
<div class="figure-label">
  Fig. 6: Distinction functions.
</div>

The Scheme of your choice should provide functions that are at least equivalent.
If you just follow along, you will need to keep in mind that the function
`plain?` matches regular symbols, and the functions `?` and `!` match symbols
that start with these letters, respectively. `to-plain` creates a plain symbol
from a special symbol by stripping away the leading special character.

Now, let's go ahead and implement the actual pattern matching!

## The meat of it

Take a deep breath now, 'cause we're diving in heads-first. If you're
overwhelmed or feel like you don't understand what's going on, do something
else for a bit, come back, reread the bits that were unclear to you and try
again. If you don't feel like this post makes any sense to you, don't worry
about it. The implementation of the algorithm—simple as the idea itself is—is
dense and complex, and might not make sense on the first read-through.

First, let's define a function that shares the API of `match` as seen above.
It will take a pattern and an expression, do some magic, and return a result.

```
(define (match pattern expression)
  (define (matchfun p e res cont)
    ;do some magic here
    )
  (matchfun pattern expression '() (lambda () nil)))
```
<div class="figure-label">
  Fig. 7: A setup for the `match` function.
</div>

You might ask yourself why there is an inner function defined within `match`,
and for good reason. While the gain might not be immediately obvious, for now
just think about it like this: it's easier to make everything a tail call if
we carry around the state in arguments, while it also makes initializing the
state we might need obvious. In this case, we initialize the result list to
be an empty list—passed as `res`—andt the continuation to return as a simple
function that returns `nil`, passed as `cont`. The latter makes it easy to
tell the user there are no more valid matches: remember that when all the
possible patterns have been returned, we want to return `nil`.

Let's begin fleshing out our inner function `matchfun`. First we want to check
whether or not we've consumed all of the patterns.

```
(define (matchfun p e res cont)
  (if (null? p)
    (if (null? e)
      (list res cont)
      (cont))
    ; more magic...
  ))
```
<div class="figure-label">
  Fig. 8: Our base case that tells us when to stop matching.
</div>

When we have consumed all of the patterns, a valid match requires us to
have consumed all of the given expressions as well. If we have, we return a
list of our result list `res` and our continuation `cont`. If, on the other
hand, there are more expressions to match, the match isn't valid and we have
to call into our continuation directly, which will either fail and return `nil`,
or backtrack into a previous state—a mechanism we haven't defined yet.

Let's move on to the easy part of our pattern matcher: if we encounter a plain
symbol in our pattern list, we just try to match it verbatim.

```
(define (matchfun p e res cont)
  ; base case goes here
  (cond
    ((plain? (car p))
      (cond
        ((null? e) (cont))
        ((eq? (car e) (car p))
          (matchfun (cdr p) (cdr e) res cont))
        (else (cont))))
  ; other cases here
  ))
```
<div class="figure-label">
  Fig. 9: The plain case.
</div>

We use the function `plain?` we defined in the previous section to see whether
the symbol is in fact a plain old symbol. If it is, we have to check whether we
have anything to match against—and if not call the continuation—, and if we do
call `matchfun` again with the rest of the lists, passing the other two
arguments merrily along. If we don't, we defer to the continuation again. From
now on, I will stop highlighting when we call the continuation. As a general
rule we'll always do that if there is no way to progress in our current state
and we have to yield.

It's time to move on to the second least-complicated case, matching one of
any character.

```
(define (matchfun p e res cont)
  ; base case
  (cond
    ; literal case
    ((? (car p))
      (if (null? e)
        (cont)
        ((lambda (v)
          (if (eq? v #f)
            (matchfun (cdr p) (cdr e)
                      (cons (list (to-plain (car p))
                                  (car e))
                            res)
                      cont)
            (if (eq? (car e) (cadr v))
              (matchfun (cdr p) (cdr e) res cont)
              (cont))))
          (assq (to-plain (car p)) res))))
    ; more cases
  ))
```
<div class="figure-label">
  Fig. 10: Matching any one character.
</div>

Okay, this is an order of magnitude more difficult to understand than the plain case.
Let me explain what we had to do, and then associate that with how we actually
implemented that behavior. If we find a symbol `p` that matches any one
character, we see whether there is anything left to match. If there is, we have
to check whether the match will align with our previous matches, i.e. whether
the `p` we matched previously was matched to the same symbol. If there was no
previous occurrence of `p`, we just record the current match and try to match
the rest with a recursive call. If it was there but did not resolve to the same
match, we're trapped in a dead end and have to yield. If it was there and
matches our previous occurrences, we just move along and don't need to record
any new symbol.

How do we actually implement this in the above code? The answer is sadly not as
elegant as I would like it to be, because regular Scheme—especially not the
first iteration of it—did not have hash maps or dictionaries or any fast
key-value data structure. Instead, the above code is implemented in terms of
[association lists](https://en.wikipedia.org/wiki/Association_list). Association
lists are basically a poor man's implementation of a key-value data structure,
with `O(n)` time for everything except insertion. It's nothing more than a list
of pairs of the structure `(key value)` with a fancy name.

I would love to implement this in terms of a faster key-value data structure,
but sadly there is no big unifying structure that I could use—zepto does have
hash maps, even hash map literals, but that's beside the point if we want to
stick to a format that every Scheme implementation can run.

If you already know Scheme's API for `association lists`, then the above code
snippet should be fairly straight-forward. If you do not, maybe pointing out
that `assq` is a function that takes in a key and an association list and
returns the key value pair will help. In the code above, this pair is handed
to a lambda where it is bound to the name `v`. We then check whether `v` is
truthy and either insert it and move on or try to match the current value
to the old value. All in all, this is a big hack around a clunky data
structure which happens to work quite well.

Now we've arrived at the last part of our matcher, the `!`. This means we need
to match zero or more characters.

```
(define (matchfun p e res cont)
  ; base case
  (cond
    ; literal and ? cases
    ((! (car p))
      ((lambda (v)
        (if (eq? v #f)
          (letrec ((match* (lambda (n)
                  (if (> n (length e))
                    (cont)
                    (matchfun (cdr p) (nrest e n)
                              (cons (list (car p)
                                          (nfirst e n))
                                    res)
                              (lambda ()
                                 (match* (+ n 1))))))))
            (match* 0))
          (if (< (length e) (length (cadr v)))
            (cont)
            (if (eq? (nfirst e (length (cadr v)))
                     (cadr v))
              (matchfun (cdr p)
                        (nrest e (length (cadr v)))
                        res cont)
              (cont)))))
        (assq (car p) res)))
    ; more cases
  ))
```
<div class="figure-label">
  Fig. 11: Matching any zero or more characters.
</div>

The structure of the case is similar to the one we just examined, with a few
important differences. We don't need to check whether or not the list is empty,
because that would be a valid match, so we move into the closure immediately
to find out if we've already encountered this symbol. If we have, we
simply check whether the two lists are equal, and then either progress or
yield.

The really interesting bit is what happens if we encounter the symbol for the
first time, in which case we define a function called `match*` that will
progressively try to call the match function, at first with an empty list bound
to our symbol. It will also set `cont` to a version of itself with a longer
list. If we ponder that for a while we see that this means it will try to go
on with a minimal list and then come back and grow the list if we can't proceed
otherwise. This is a fairly simple idea, although the implementation tends to
disagree and admittedly is a little hard to read.

We're almost done, all we have to do is add another case to our top-level
expression that will call `cont` if none of the cases we handled makes sense.
This should never happen provided the user hands us valid arguments, but we
shouldn't rely on that.

```
(define (matchfun p e res cont)
  ; base case
  (cond
    ; regular matchers
    (else (cont))))
```
<div class="figure-label">
  Fig. 12: A catch-all matcher.
</div>

This is it! We've just built a pattern matching system with back-tracking in
Scheme.

## What remains to be done

The code I presented above is dense and very conservative. The Scheme or Lisp
of your choice will probably provide many more advanced features that are
suited to simplify the code—hash maps come to mind, for which even a
[SRFI](https://srfi.schemers.org/srfi-69/srfi-69.html) with reference
implementations exists. I limited myself to pure Scheme as specified in the
R5RS standard, though—except for the helper functions, where I took some
liberties. It would be a worthwile exercise to transcribe the algorithm into a
more modern version of Scheme. I'm positive it would make the code more
readable, and quite certainly more performant.

Other worthwile exercises I could think of:
* supporting arbitrarily deeply nested match expressions
* adding head-tail patterns to the nested expressions, although a valid
  workaround would be `(?H !T)`
* throwaway patterns, e.g. with `_` and `!_`
* Make the `!` matcher greedy

There's plenty of work that could be done to improve this algorithm, but I
believe that the fundamental simplicity of the idea is what makes the algorithm
appealing to begin with. Thus any kind of work should strive for the same
conceptual simplicity instead of just tacking on features blindly, at
least in my book.

## Fin

When I first understood this algorithm, I was stunned. Surely it can't be that
simple, but somehow it is. It took me by surprise because it is not the paper's
main event, rather, it was meant to be an illustration of Scheme as a language.
I'm not sure if anyone ever took it at face value and tried to play around with
it more, whether it had any impact on the implementation of logic or functional
programming languages, or if everyone silently agreed that it was just a
toy. Prolog had been around for three years at that point, providing a capable
unification and pattern matching engine already. I've never ventured to
understand the implementation of logic programming, but having seen the
algorithm I presented today—and having enjoyed it thoroughly—I believe I might
appreciate looking at their underpinning.

This has been my longest blog post to date by far. I hope you enjoyed reading
it as much as I enjoyed writing it. I tried to make the post accessible to a
lot of the programming world, though I understand both Lisp and the algorithm
at hand might be confusing for a non-negligible percentage of programmers. If
you have any questions, comments, or criticism, please reach out to me via
e-mail or any medium you can get a hold of me in.

Lastly, I want to thank all of the awesome people who reviewed my blog post
and without whose feedback this would've been much more of a mess! You know
who you are and what you did and I'm really grateful for all of it.

See you next time!
