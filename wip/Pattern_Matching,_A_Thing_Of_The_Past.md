When I started my [reading list](https://github.com/hellerve/ptolemy)
in January of this year, I decided to give rereading the
[Lambda Papers](http://library.readscheme.org/page1.html)
a shot. To the uninitiated: in the latter half of the 1970s, Guy Steele and
Gerald J. Sussman worked on Scheme. This was an extremely productive time for
both of them, and they published a series of now-famous papers collectively
dubbed the “Lambda Papers”. They are mostly concerned with Scheme and how it
compares to other languages of the time, and neat tricks that they discovered
were possible while working on it.

I've reread a good amount of them by now, and they are all as excellent as I
remembered—maybe even more so now that I've gathered a fair amount of knowledge
of what makes a Scheme and how to program in it. But most importantly I've
gained an appreciation for sections that went unappreciated when I first read
the papers. Today I want to talk about one of them, found in
[“Scheme, an Interpreter for Extended Lambda Calculus”, pages 10–12](http://repository.readscheme.org/ftp/papers/ai-lab-pubs/AIM-349.pdf).
This paper is startling for a bunch of reasons, the most anxiety-inducing of
which is that Guy Steele was 21 years old when this paper was published, and
it is ground-breaking in more than one respect.

The part I want to talk about is not about one persons gifts, however. I want
to talk about an algorithm that implements an extremely capable pattern matching
system in 38—admittedly very dense—lines. I didn't understand what it meant the
first time I read the paper, and the second time it completely blew my mind. In
this post, I want to attempt to walk you through the original source, which I do
not think is still runnable in modern implementations of Scheme. If you want to
follow along using a slightly more modernized version, you can download
[an attempt at a transcript into zepto](/assets/patternmatch.zp). Excuse the
plug, but I like to put my language to use every once in a while.

## Consumer information

Before I explain the algorithm I want to show you the API. It is relatively
simple and intuitive, and exposes only one function, aptly called `match`.
Let's first look at the examples from the paper, and then construct our own,
to make sure we understood the interface.

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

This is fairly involved and I don't expect you to understand it, especially
since the authors didn't choose a very simple example. Let's go through the
invocations and the return values one by one.

## A setup of sorts

## The meat of it

## Putting it all together

## Fin
