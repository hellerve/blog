Almost exactly a year ago today I was in a small room in a venture capitalist’s
apartment, surrounded by 20 close friends and friends of friends, preparing to
announce version 0.9.6 of [zepto](https://github.com/zepto-lang/zepto), the
language’s first stable release. It was supposed to be a stable alpha that
people could download and play around with. Version 0.9.7 was already in the works, 
with exciting things, like a more robust parser, a better
traceback format, a faster hashmap implementation, and loads of other minor
bugfixes. It never saw the light of day.

The commits came less and less frequently until at last, two months later, I
pronounced the project dead to me. I haven’t worked on it since, although some
of my systems, such as the static site generator of this blog, the cowsay
prompt that greets me every time I open a terminal, and some miscellaneous
scripts I use, still depend on it. The language still works, although some
of the native extension that depend on outdated Haskell packages do not
anymore.

Now, one year after my optimistic announcement, I want to tell you what zepto
was, what it stood for, why it was great in many ways, and, most importantly,
why I decided that zepto is, in fact, a failure.

## A room of one’s own

I think most of us, at one point in our career, question our favorite
language’s—or languages’—ways. We realize that the creators and
[BDFLs](https://en.wikipedia.org/wiki/Benevolent_dictator_for_life) that we
used to worship are not nigh-infallible; they are mere humans. And so, in
youthful rebellion, we go full-on iconoclast and write a language of our
own. Or we sit in our study—does anyone still have those?—and quietly write
code, trying to figure out how things fit together, and how we could improve
the systems we’re currently working with, incorporating the stone tablets of
our mothers and fathers, full of weird type signatures, grammars specified in
Backus-Naur notation, and oh so many parentheses. And then, maybe we have a
great idea. Or maybe not.

zepto was one idea that I had. I had worked on a few Lisps before, in C and
Haskell, and decided I liked them a lot. I didn’t know any Lisp when I embarked
on that journey: I learned Haskell and Lisp both while writing zepto’s
ancestors, and, to a large extent, zepto itself. You can still tell: the
Haskell code that fuels zepto is pretty terrible, and so are the early
libraries that I wrote. It was a bad idea to incorporate those into the
standard library, but I did, accumulating cruft from zepto’s inception.
Starting out with code from the now-infamous [Write Yourself A Scheme in
48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
probably didn’t help either. [The new
version](https://wespiser.com/writings/wyas/00_overview.html) wasn’t around
back then.

I ended up working on zepto for almost two years, devoting a lot of nights,
weekends, and my [bachelor thesis](http://github.com/hellerve/bachelor-thesis)
to it, while working full-time. My bosses were always very supportive, even
encouraging. The CTO at [Bright](http://brig.ht/) hosted a Clojure meetup and
made me present an early version of zepto, to very lukewarm, paternalistic
feedback. I was told things like “we all did that once”. Noone seemed to
realize zepto was different! I now know my sales pitch tried to get people to
try zepto in an underhand way, telling people how similar zepto was to the
language they were currently using instead of telling them how it was
different, even better than their tools. I lacked confidence, and for good
reason: I myself didn’t know what zepto was or what it was meant to do.

About a year in, I eventually realized that I needed a focus, and I formed
three core principles: naïveté, malleability, and friendliness. What at first
felt constraining ended up being the most liberating thing I could’ve done for
zepto and my work on it. Having a goal mattered, it motivated me to push
towards them, and I had clear scales on which to grade conflicting solutions
to the same problem.

### Naïveté

Naïveté or, less pretentiously, naivety is an important goal in my world.
If the inferior-but-simpler solution solves the problem at hand well enough and
is not likely to cause any problems in the future, I’ll gladly stick with it.
I’m very much not a purist.

This meant that most of the libraries I wrote for and in zepto are fairly
naive. Some of them are very slow. I was able to get things working and, when I
realized that there was a bottleneck, go fix it, instead of crafting the best
solution for much longer and then realizing that it mattered very little for
the overall performance of a tool.

I think naivety is important as a language starts out: the designers need to
catch up with the more established languages, and need to start making theirs
usable; even with a good FFI this might not always be trivial. So many
libraries need to be wrapped in an idiomatic interface or, in the worst case,
be written from scratch.

It also encourages the actual writing of software in the new language, and that
makes catching errors and discovering needs so much easier than looking down
the idiomatic ivory tower to see what the people on the bottom might need.

### Malleability

Everything in zepto was malleable: it had `#lang` constructs similar to Racket,
but much more primitive. Functions were able to hook themselves up into the
system by providing a function that would translate the file into zepto’s AST,
and zepto took over from there. It’s a simple idea, but fairly powerful.

```
(zepto:implements-lang <my-fun> "my-language")
```
<div class="figure-label">
  Fig. 1: Hooking up a new parser can be so simple.
</div>

On the other side of the implementation, I also planned—but never really
finished—a nanopass compiler that would work on zepto’s AST. The user would
have been able to add passes and backends. I actually managed to
write—mostly—working backends for Clojure and JavaScript.

In the end I think this was one of the factors that most contributed to zepto’s
death. But we’ll talk about that later.

### Friendliness

The most important feature of zepto was friendliness, at least for me. This
meant that a friendly community was encouraged—although the “community” never
grew larger than a handful people—, but also a certain developer-friendliness
in all the features of the language. Intuitive, beautiful APIs played a part in
that, of course, but I think every language thinks it has great APIs. More
important were tools like [zeps](https://github.com/zeps-system/zeps/), the
language package manager, written in zepto. It supported tests, bootstrapping
new projects from templates, sandboxing, documentation generation, and all of
the features that a developer now expects from a language. I used it a lot, and
it was actually a pleasure. It was also tremendously helpful for onboarding new
people.

## Why it failed

This is the part where I’ll start speculating. As I see it there were many
reasons why zepto failed, and some of them also plague languages that are
multiple orders of magnitude more popular.

But let’s get the real main reason out of the way: working on a project as big
as zepto for two years, nights and weekends only, is draining. Cloning the repo
now reveals that the zepto repository and all of its submodules clocks in at
around 15.000 lines of code, and zeps is another 2.500, both mostly written in
zepto. And that doesn’t even include a fair share of the modules that I wrote
and didn’t include in the standard library or any unmerged feature branches,
and there are quite a few of those. These numbers are not gigantic, but enough
where, after two years, you ask yourself “why am I doing this?”.

And that gets to the heart of it: while in the beginning I was content to
answer that question simply with “becuase it’s fun”, I realized that it got
less fun as time dragged on. And it didn’t seem as if anyone would be
interested in taking over the development of my little baby—I can’t blame them,
what with the aforementioned cruft and all.

I got more serious and more ambitious over time, but I failed to meet most of
the goals I set out for myself. Some of them were even at odds with each other.
A nanopass compiler with pluggable backends, but also a foreign function
interface? How’s that supposed to work exactly? And what about all of the
complex data types zepto supports out of the box? Those questions never got an
answer, even though I knew they needed one if I was to push forward.

Having everything be malleable didn’t help. It complicated every part of the
design, and made the language more opaque. If everyone can have their language,
noone has any way to communicate. I’m still not a fan of limiting a language
just because a feature can be abused, instead hoping that people are
responsible, in the Kantian humanist sense<sup><a href="#1">1</a></sup>. But
I’ll write about operator overloading and macros another time.

In summa, two main reasons are to blame for the death of zepto: my own
inability to work on it anymore, and the lack of a technical goal for it. It
had plenty of goals, and a general ideology, but I didn’t know how to best
reach a state where the system was close to its Platonian idea.

## Fin

Maybe I gave up on zepto too soon; maybe a general regrouping and rewrite
would’ve helped. But as readers of this blog know, I never lack ideas for side
projects, and I’m currently in a love affair with Carp. I’m just grateful
for the time I spent on zepto, for I learned a lot, and happy I have time to
tackle other challenges again.

```
(define (goodbye)
  (write "Goodbye, zepto!"))
```
<div class="figure-label"> Fig. 2: Saying goodbye.</div>

##### Footnotes

<span id="1">1.</span> The word I’m looking for is a translation of the word
“Mündigkeit”, a German term Kant used extensively, notably in his “Beantwortung
der Frage: Was ist Aufklärung?”. It’s very close to “responsibility”, but I
don’t think it was translated that way.
