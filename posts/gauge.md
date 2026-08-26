---
title: "Can one hear the gauge of a string?"
date: 2026-08-25
---

In 1966, Mark Kac published a paper in the American Mathematical Monthly called
“Can One Hear the Shape of a Drum?”. A drum head has a set of resonant
frequencies, which are the eigenvalues of the Laplacian on whatever shape the
drum happens to be (just trust me). If I hand you the frequencies, can you guess
the shape? Kac credited the question to Bochner and the phrasing to Lipman Bers,
so I have to credit Bers as much as Kac for this blog post, since the title is
what sent me down this path.

You can actually hear the shape. The asymptotics of the eigenvalues work out to
the area (I learned that this is Weyl’s law from 1911). The next term then gives
us the perimeter, and the term after that the number of holes. So you can
actually “hear” how big the drum is as well as its edge and its holes!

The rest took until 1992, when Gordon, Webb, and Wolpert exhibited two
differently shaped drums with identical spectra.

Anyway, the answer is no and I don’t own a drum.

I do play the guitar, though, badly, and I realized that, while strings are
simpler, they also have a property we might reconstruct: gauges. My strings go
anywhere from `.009` to `.052` (yes, I play a few very different styles), and
maybe switching gauges changes the feel more than the sound, but there’s also
something different about an E4 on my high E string and an E4 on B string, even
if I wind the B string such that they’re tuned the same.

So, naturally, a string also has a spectrum. Which begs the question: can one
hear the gauge of a string?

A disclaimer before we start: none of the physics here is new, I just couldn’t
find anyone who actually answered my question the way I posed it. I just stick
formulas in other formulas until they give me an answer.

## The trivial version

Suppose we have an ideal string. It’s perfectly flexible, uniform along its
length, and fixed at both ends. Then its frequencies are

```
f_n = (n / 2L) · √(T/μ)
```
<div class="figure-label">Fig. 1: The frequencies of an ideal string of length `L`, tension `T`, and linear density `μ`.</div>

That’s it. The fundamental and integer multiples of it.

The spectrum is an infinite list of numbers and it contains exactly one number
here. Every overtone is determined by the first one. The harmonic series, the
most satisfying property about vibrating strings, is trivial here and probably
sounds extremely boring, just a repetition ad infinitum.

Let’s suppose we know the tension, the length, and the material. For a
perfectly round wire of density `ρ` and diameter `d` we have `μ = ρπd²/4`, so

```
d = (1 / L·f₁) · √(T / πρ)
```
<div class="figure-label">Fig. 2: Gauge from a single note, given tension, length, and material.</div>

Hearing the spectrum is optional. You just need to hear one note and measure
three things.

But this is annoying, and it also doesn’t model the real world: A `.010` at
a given tension and a `.020` at four times that tension work out to the same
fundamental and therefore are indistinguishable in this setup. An ideal string
doesn’t let you hear its size. The answer is, therefore, “no”.

We can make this more interesting.

## Enter Kac

The analogue of Kac’s question isn’t about a uniform string anyway. His drum
could be any shape. So let the gauge vary along the length, `d(x)`, and ask
whether the spectrum determines the profile.

Before we despair at the math, the answer has already been worked out by people
much more persistent than me. It turns out spectrum doesn’t determine the
profile.

A single spectrum doesn’t pin down the mass density of an inhomogeneous string
(yeah, try parsing that one). Bottom line: there are different strings that
sound identical, and you need richer data than one list of eigenvalues to recover
the density<sup><a href="#1">1</a></sup>.

This is a one-dimensional case, Kac works in two dimensions, and even at his
time the one-dimensional case had been solved already.

## Enter stiffness

Ideal strings aren’t real, real strings aren’t ideal. A steel or nylon wire
resists being bent (I suppose gut does as well, but luckily I don’t play baroque
music). That stiffness is going to be bolted onto the wave equation as a fourth
derivative, I am told:

```
μ ∂²y/∂t² = T ∂²y/∂x² − EI ∂⁴y/∂x⁴
```
<div class="figure-label">Fig. 3: The stiff string, with Young’s modulus E and second moment of area I.</div>

For pinned ends<sup><a href="#2">2</a></sup> this gives

```
ω_n² = (T/μ)·(nπ/L)² + (EI/μ)·(nπ/L)⁴
```
<div class="figure-label">Fig. 4: The spectrum of a stiff string.</div>

and now we have a second number, finally. The `n²` term is our friend `T/μ`
from earlier. `n⁴` term is a new guy. It grows faster and carries `EI/μ`,
which means the overtones are no longer redundant. We’re getting somewhere!

The overtones now drift progressively sharp of the harmonic series. We get
more distinguishing information about the properties of the string!

Musicians and piano technicians call this inharmonicity, and it’s usually a
nuisance, unless you’re a 20th century New Music weirdo. Harvey Fletcher
worked out the standard form for piano strings in
1964 with a single coefficient `B`:

```
f_n ≈ n·f₀·√(1 + B·n²),   with   B = π³Ed⁴ / (64·T·L²)
```
<div class="figure-label">Fig. 5: Fletcher’s inharmonicity coefficient.</div>

I’m just going to take this at face value and move on. Thanks, Harvey.

There’s a `d⁴` in there, which is promising. It turns out that it does work:
for a solid round wire, `I = πd⁴/64` and `μ = ρπd²/4`. This means that the
stiffness term reduces to

```
EI/μ = (E/ρ)·(d²/16)
```
<div class="figure-label">Fig. 6: The stiffness term for a solid round wire.</div>

Tension disappears (not in me, though, I’m still very tense at this point)!

Now fit the two coefficients to a recorded spectrum and know what the string is
made of. The gauge is determined by how badly the overtones misbehave (or how
much they rock, depending on what kind of musician you are). Estimating `B`
from recordings is also, I am told, a solved problem in music information
retrieval, and the dependence on diameter has been measured on actual plucked
guitar strings. We don’t have to speculate.

Except the fit gives us `A` and `C` in `ω_n² = A·n² + C·n⁴`, and `A` still has
the tension in it. So everything hangs on `C`, which for our round wire is

```
C = (E/16ρ) · (π⁴/L⁴) · d²
```
<div class="figure-label">Fig. 7: The quartic coefficient, spelled out.</div>

That’s `d/L²`, not `d`. We get the gauge relative to the length, so someone has
to supply a length. Practically speaking, we could say we “hear the gauge” if we
know the instrument and its fretboard length. If we’re only given frequencies,
we’re out of luck.

Still: we get a different answer!

The idealized string from textbooks hides its gauge. A real one with a “flaw”
in it lets us reconstruct it. Imperfections to the rescue.

## More reality checks

I play guitar, which means I know strings aren’t always that simple. We assumed
a solid cylinder here, such that `μ` and `I` are both functions of the same `d`.
This makes gauge recoverable. We have two numbers, one is unknown, job done.

A wound string breaks our formulation. For many string instruments, this
replaces at least some of the strings, and they look like this: you take a thin
core and wrap wire around it. I never asked myself why, because I don’t really
ordinarily do physics. It’s somewhat understandable though: the mass goes up,
the bending stiffness stays roughly that of the core. You get low strings that
can still be played and bent and not steel cables.

But this completely screws us! The string still gives you `T/μ` and `EI/μ`, but
the physics stops letting you collapse them into the diameter, because `I` is
no longer `πd⁴/64`. You hear something about the core and something about the
total mass, but the buck stops here for me. I don’t even know how to start this
problem.

So the answer flips again, and we’re back to “no”.

## Final tally

Collecting all our findings roughly in order:

- **Ideal uniform string, unknown tension**: Nope.
- **Ideal uniform string, known tension, length and material**: Yup, without
  knowing the spectrum.
- **Ideal string with a varying profile `d(x)`**: Nope, solved decades before
  the drum.
- **Real stiff wire, known material and length**: Yup. Inharmonicity gives it
  away.
- **Real stiff wire, spectrum and nothing else**: Nope. Gauge and length are
  degenerate together.
- **Wound string**: Maybe? A nope for me, dog.

## Fin

Fletcher published the stiff string in 1964, two years before Kac’s paper that
tipped me off appeared on the scene, and the inverse problem for the
inhomogeneous string is even older than that.

What I couldn’t find was anyone phrasing it as a question, and going through the
ladder of findings. And it’s quite cool: the ideal problem is unsolvable, the
real world makes it solvable, and then it quickly becomes too complicated again.

I don’t have a grander conclusion than that. I’ve been letting guitar strings
bite my hand for almost exactly 20 years and the question had never occurred to
me before.

If you do work out the wound string, do tell me. I’m curious if dubious that
I’ll understand it.

#### Footnotes

<span id="1">1.</span> The entry point is G. M. L. Gladwell, “Inverse Problems in
Vibration”, if you want the engineering version rather than the analysis. H. P. W.
Gottlieb’s “Isospectral strings” comes with examples. I am not going to pretend
to have read the analysis literature properly or understood any of it, and I
don’t need to. We press on.

<span id="2">2.</span> Pinned ends are convenient, but of course not quite
what a real string is. Clamped ends add corrections. There is literature on
that, too, and I don’t understand it either. For the argument here it doesn’t
matter, since the `n⁴` term stays.
