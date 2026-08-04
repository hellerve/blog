---
title: "A More Immediate Intuitive Appeal"
date: 2026-08-04
---

Alonzo Church published [The Calculi of
Lambda-Conversion](https://press.princeton.edu/books/paperback/9780691083940/the-calculi-of-lambda-conversion)
in 1941. On page 41, weighing up the formalisms that had been proposed for
the intuitive notion of an effective procedure (a bit of a fight between him
and Turing), he grants that Turing’s has “a more immediate intuitive appeal”
than his own.

It’s a generous sentence, and broadly correct (although I find Church’s
construction, the lambda calculus, more appealing), and it makes me sad every
time I think about it.

I should say where this comes from, because I am not a logician and the
foundational crisis is not my day job. I don’t really have a business reading
these books. But when I was eighteen I read [Logicomix](https://en.wikipedia.org/wiki/Logicomix),
a graphic novel about Bertrand Russell and the search for a secure foundation
for mathematics, and it handed me a story I’ve been unravelling ever since. The
names and stories in the book fascinated me, and so I read Frege, then Russell and
Whitehead (I gave up on the Principia Mathematica, admittedly), then Hilbert,
Wittgenstein (his Tractatus is a big ol’ letdown in my opinion), Gödel, and
eventually Turing and Church. I tried to read the real literature, badly and out
of order, because noone was grading me and I would just find papers and books
more or less randomly over time.

Fifteen years of that, and I’d struggle to tell you what it’s for. It’s fun and
gratifying, but most of it is over my head, and the notation is arcane and
archaic (though Frege’s [Grundgesetze der
Arithmetik](https://archive.org/details/bub_gb_LZ5tAAAAMAAJ) is a real
notational gem, even if you take nothing else away from it).

Church’s sentence made me cry, which is both a sign that I’m a huge nerd, and
that I’m too far into the bikeshed to turn around now.

## What he actually gave away

The concession didn’t start in 1941, and it wasn’t small. Church had already
[reviewed](https://fermatslibrary.com/s/review-of-on-computable-numbers-with-an-application-to-the-entscheidungsproblem) Turing’s [On Computable Numbers](https://www.cs.virginia.edu/~robins/Turing_Paper_1936.pdf)
for the *Journal of Symbolic Logic* in 1937 (they both published their systems in
1936), and it was in that review that he coined the phrase “Turing machine”. He
named the thing that would eclipse his own for the years to come. In the same
review he wrote that computability by a Turing machine “has the advantage of
making the identification with effectiveness in the ordinary (not explicitly
defined) sense evident immediately”.

For completeness’ sake (no pun intended), there’s a third party in this, and in
my opinion he matters more than is commonly understood. Gödel had not accepted
lambda-definability as a definition of effective calculability at all. Kleene
later reported that he regarded the proposal as “thoroughly unsatisfactory” (ouch!),
and it seems he only came around to the thesis once Turing’s formulation appeared.
Of that one he said it was “correct [...] beyond any doubt” (OUCH!), and
later that “we had not perceived the sharp concept of mechanical procedures
sharply before Turing, who brought us to the right perspective”<sup><a href="#1">1</a></sup>.

That’s likely why Church stated his thesis in terms of recursiveness rather than in
terms of the calculus he had built (though the [SEP treats it as a bit of a puzzle still](https://plato.stanford.edu/entries/church/supplementA.html)).
Gödel wasn’t buying what he was selling, and although I don’t know if this is
a reflection of a broader sentiment, it was clear enough that Church came to
believe Turing’s method to be more intuitive himself.

## Why can’t we just get along?

There’s more to this that I personally find strange and sad, though.

By the time Church wrote that sentence, the question of *power* was closed. Kleene
had shown in 1936 that the lambda-definable functions are exactly the general
recursive ones. Turing had shown in 1937 that his computable functions are exactly
the lambda-definable ones. The three formalisms were provably equivalent for the
application that people were arguing about.

Nothing was at stake mathematically. Every remaining disagreement was about
which story a person could be brought to believe more quickly. And on that
question, the tape and the head and the little machine that shuffles along it
beat an algebra of substitution, even for mathematical geniuses amongst
themselves.

I find that a bit deflating. I personally find lambda prettier (more on that
below), but there’s also something structural at play.

For the longest time, I believed that in mathematics (and science at large) the
argument ends when the proof is formalized. Here it did not end, even though
everyone knew that the proofs were equivalent, and people quibbled over,
basically, rhetoric, personal taste, and, honestly, a bit of ego.

But back to Turing machines and lambda calculus.

## Why I still like lambda

I should define beauty, because the whole problem here is basically that it’s
famously in the eye of the beholder.

Turing’s model persuades his readers by metaphor. It creates a picture of a
clerk with a paper tape, and you check the picture against your own sense of
what following a rule is like, and it matches. You can even do your own little
play-through. That’s a real intellectual achievement and I don’t want to be sniffy
about it, because it takes genuine skill to find such a powerful picture for
such a subtle result. But you get persuaded by the image. Without it, you just
have a very tedious formal object, and I had real trouble with the mechanics of
it in the paper.

Lambda calculus persuades, when it does, much more subtly. We start with
functions and application and nothing else. No numbers, no booleans, no
conditionals, no data of any kind. And then those things turn out to already
be there, if you are willing to look at functions from a slightly different
angle, and you conjure them from the depths of application. I’ve been talking
about this for years, [like when I talked about conditionals as macros](https://blog.veitheller.de/Scheme_Macros_V:_Conditionals.html)
(it’s just a Church trick), or when I worked on
[mae](https://blog.veitheller.de/Maps_Are_Everything.html), which was basically
just a fun re-encoding of the idea.

So the beauty, I think, is not that it’s clever (it is!), but that it’s *less*.
Nothing was added to get arithmetic. [I love using my hammer for every nail,
after all](https://www.youtube.com/live/EkbcI3KgUuY?t=8360s).

But that quality is also, for most people, what makes it a bad argument. It’s
not something you can hand to a skeptic. Gödel wanted to see that the formal
notion caught the informal one, and a picture of a clerk does that in a sentence.
An encoding of numerals as function application only does once you spent a lot
of time in la-la-land with Church. Subtlety and persuasiveness pull against each
other, and Church, to his credit, saw it himself.

E pur si muove.

## The shape that won the west

There’s a false consolation available here. I don’t buy it.

It goes like this. Church lost foundations, but he won everything else. Nobody
computes with a Turing machine, lambda abstraction ate programming. It went
into Lisp, into ML and Haskell, and into the mainstream. Java has closures.
Python does, and JavaScript, and most other languages under the sun. When people
say “lambda” in 2026 they are, whether they know it or not, living in the house
that Church built.

But that’s not the whole story, of course.

When we look at the shape of a Turing machine rather than its [Brainfuckian](https://blog.veitheller.de/Brainfuck_and_Actors.html)
offshoots, we get a linear, addressable store, a position in it, a state, and
one small change per step. Which is also a description of every computer I have
ever used. A stack, a heap, and a program counter walking forward through
addresses make my processor go Brrr<sup><a href="#2">2</a></sup>.

The lambda calculus has none of these affordances. It doesn’t need to compute,
it flows.

People did try to build the other shape in hardware, but it went badly. Lisp
machines were real products from real companies and, as I understand it, they
died with the AI winter. Some people say they were amazing, and I want to
believe, but I never actually used them.

What survived and thrived is a tape machine with somewhat better ergonomics.

So maybe (though I’m not so sure about that either) the closures did win the
notation war, but they run on a bloodless architecture, if you allow me some
pathos at this point.

Every lambda becomes a stack frame and a pointer into linear memory. Lift the
hood, and you will see man-made horrors beyond your comprehension<sup><a
href="#3">3</a></sup>.

## Fin

I don’t have a resolution. The crux is that, given a clear argument and a
beautiful one, the clear one wins, no matter how sophisticated the audience is.
In an ideal world, that “win” wouldn’t matter. The formalisms are equivalent,
after all!

Unfortunately, winning is how these ideas become foundational. That’s not a
scandal, it’s just how persuasion works, and I can’t blame anyone for wanting
“a more immediate intuitive appeal”.

But I feel like an iconoclast for liking the weird thing, and I suspect that’s
the actual reason this bothers me. I have said recently that I’d like to be
[the programmer equivalent of a musician’s musician](https://blog.veitheller.de/legacy.html).
It didn’t occur to me then that this is the same trade Church made. He’s a
mathematician’s mathematician.

Nothing wrong with that.

#### Footnotes

<span id="1">1.</span> I want to make clear that this is secondhand material,
and while Gödel was known for not mincing words, this is Kleene’s retelling in
1981, a 1951 lecture by Gödel, and an undated manuscript that I found floating
around the internet when digging (they’re in his Collected Works, though, so I’d
think they’re legit).

<span id="2">2.</span> Let me be clear again that I’m talking about resemblance,
not a clear lineage. Whether Turing’s paper actually influenced the
stored-program architecture is apparently disputed by people who know a whole
lot more about this than I do, and von Neumann’s EDVAC draft famously cites
nobody at all. He was just handed the tablets on Mount Sinai, I suppose.

<span id="3">3.</span> Haskell’s laziness is a graph mutated in a heap on a
register machine. We try so hard, and yet we fail.
