Some of you might have heard of
[`bc`](https://en.wikipedia.org/wiki/Bc_%28programming_language%29) before. I
know that the people who attended my lightning talk at Recurse Center last
Thursday have, and everyone whom I've bothered by ranting about it before or
after. I will write a quick blog post about it, followed by a shameless
plug—hint: I've been bitten by the
[reimplementation bug](https://github.com/hellerve/bc) yet again.

`bc` is one of those commandline gems that are easy to overlook but are
extremely handy if you know about them. It's a simple C-like programming
language—how often do you see “simple” and “C” used in the same sentence?—that
is mostly used from within shell scripts if one needs to do quick, yet possibly
advanced calculations, a task that is not trivial in most shell languages. It is
also, admittedly, not an all too common task.

```
define int_gcd(x, y) {
  while (y > 0) {
    r = x % y
    x = y
    y = r
  }
  return x
}
```
<div class="figure-label">Fig. 1: A simple BC program for calculating the
Greatest Common Denominator of two integers.</div>

It is a handy tool, however. Whenever you find yourself trying to pipe a string
into Perl or Python to evaluate a mathematical instruction, I suggest you use
`bc` instead. It has better startup time and is actually relatively fun to use;
it mostly does what you would expect from a dynamically typed language with
C-like syntax.

I have used `bc` as a calculator before, but wasn't really aware that it
actually is a fully fledged programming language. That changed as soon as I
started to reimplement the language, for the worst possible reason: I wanted to
build a better REPL.

[![asciicast](https://asciinema.org/a/75ay3m4mx5i93tbfu7dahc55u.png)](https://asciinema.org/a/75ay3m4mx5i93tbfu7dahc55u)
<div class="figure-label">Fig. 2: A screencast of my `bc` reimplementation in
action.</div>

The REPL features a nice realtime preview, readline capabilites, and tab
completion—coming soon, courtesy of an
[independent contributor](https://github.com/hellerve/bc/pull/1). It is a
pleasure to use, even if the underlying `bc` implementation does not completely
mimic the standard. Some of deviations are bugs that I still have to fix, some
of them deliberate design decisions. Time will tell which are which.

I had a lot of fun implementing this little language. It didn't require a lot of
code, and my grasp on Haskell—the implementation language—has solidified greatly
while doing it. The interpreter is written in a purely functional manner,
without Monads or fancy tricks to mimic statefulness. I am somewhat pleased with
how it turned out, even if it is a little rough around the edges.

If you toy around with it, do tell me what you think. I know there are some
issues with the way whitespace is consumed by the parser—the less spaces you use
the better—, and IO isn't implemented, which is one of the deliberate decisions
on my part. I hope you enjoy it!
