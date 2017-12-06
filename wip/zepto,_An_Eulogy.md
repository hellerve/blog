Almost exactly a year ago today I was in a small room in a venture capitalist’s
appartment, surrounded by 20 close friends and friends of friends, preparing to
announce version 0.9.6 of [zepto](https://github.com/zepto-lang/zepto), the
language’s first stable release. It was supposed to be a stable alpha that
people could download and play around with. Version 0.9.7 was already being
worked on, with exciting things, such as a more robust parser, a better
traceback format, a faster hashmap implementation, and loads of other minor
bugfixes. It never saw the light of day.

The commits came less and less frequently until at last, two months later, I
pronounced the project dead to me. I haven’t worked on it since, although some
of my systems, such as the static site generator of this blog, the cowsay
prompt that greets me every time I open a terminal, and some miscellaneous
scripts that I use, still depend on it. The language still works, although some
of the native extension that depend on outdated Haskell packages do not
anymore.

Now, one year after my optimistic announcement, I want to tell you what zepto
was, what it stood for, why it was great in many ways, and, most importantly,
why I decided that zepto is, in fact, a failure, all things considered.

## A room of one’s own

I think most of us, at one point in their career, question their favorite
language’s—or languages’—ways. We realize that the creators and
[BDFLs](https://en.wikipedia.org/wiki/Benevolent_dictator_for_life) that we
used to worship are not nigh-infallible; they are mere humans. And so, in
youthful rebellion, we go full-on iconoclast and write a language of our
own. Or we sit in our study—does anyone still have those?—and quietly write
code, trying to figure out how things fit together, and how we could improve on
the systems we’re currently working with, incorporating the stone tablets of
our mothers and fathers, full of weird type signatures, grammars specified in
Backus-Naur notation, and oh so many parentheses. And then, maybe we have a
great idea. Or maybe not.

zepto was one idea that I had. I had worked on a few Lisps before, in C and
Haskell, and decided I liked it a lot. I didn’t know any Lisp when I embarked
on that journey: I learned Haskell and Lisp both while writing zepto’s
ancestors, and, to a large extent, zepto itself. You can still tell: the
Haskell code that fuels zepto is pretty terrible, and so are the early
libraries that I wrote. It was a bad idea to incorporate those into the
standard library, but I did, accumulating cruft from zepto’s inception.

I ended up working on zepto for almost two years, devoting a lot of nights,
weekends, and my [bachelor thesis](http://github.com/hellerve/bachelor-thesis)
to it, while working full-time. My bosses were always very supportive, even
encouraging. The CTO at [Bright](http://brig.ht/) hosted a Clojure meetup and
made me present an early version of zepto, to very lukewarm, paternalistic
feedback. I was told things like “we all did that once”. Noone seemed to
realize zepto was different! I now know my sales pitch tried to get people to
try zepto in and underhand way, telling people how similar zepto was to the
language they were currently using instead of telling them how it was
different, even better than their tools. I lacked confidence, and for good
reason: I myself didn’t know what zepto was or what it was meant to do.

I eventually, about a year in, realized that I needed a focus, and I formed
three core principles: naïveté, malleability, and friendliness. What at first
felt constraining ended up being the most liberating thing I could’ve done for
zepto and my work on it. Having a goal mattered, it motivated me to push
towards them, and I had clear scales on which to grade conflicting solutions
to the same problem.

### Naïveté

Naïveté or, less pretentiously, naivety is and important goal in my world.
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
