---
title: "Bitter Lessons and Premature Optimizations"
date: 2026-10-18
---

Donald Knuth wrote “premature optimization is the root of all evil” in 1974, in
a paper called [“Structured Programming with go to
Statements”](https://dl.acm.org/doi/10.1145/356635.356640). It’s high on the
leaderboard of the most quoted sentences in our field, and I would wager that
only a fraction of the people quoting it have read the paragraph it comes from.

Here it is, with the part I think is important but quite often missed:

```
We should forget about small efficiencies, say about 97% of the time:
premature optimization is the root of all evil.

Yet we should not pass up our opportunities in that critical 3%. A good
programmer will not be lulled into complacency by such reasoning, he will be
wise to look carefully at the critical code; but only after that code has been
identified.
```
<div class="figure-label">Fig. 1: Knuth, unabridged-ish.</div>

The argument is about where to spend your effort. Don’t waste your time on code
that doesn’t matter, find the code that does, and then optimize the hell out of
it. In the very next sentence he explains how you find it. Of course you
measure, because “the universal experience of programmers who have been using
measurement tools has been that their intuitive guesses fail”<sup><a
href="#1">1</a></sup>.

That’s not what the sentence turned into, of course. Culturally, “premature
optimization” became shorthand for “don’t optimize”, and the word *premature*
becomes irrelevant. Someone suggests making a thing faster, someone else recites
the sentence, and the conversation is over. This happens without anyone having
established that the optimization was premature, or what premature means
(presumably we’ll get to it when the first requests start timing out).

This post is not about that, though. Instead, look at it as a historical
precedent, because I think the Bitter Lesson is going through the same thing
right now.

## What Sutton said

In 2019, Rich Sutton published a short essay called [“The Bitter
Lesson”](http://www.incompleteideas.net/IncIdeas/BitterLesson.html). It opens
like this:

```
The biggest lesson that can be read from 70 years of AI research is that
general methods that leverage computation are ultimately the most effective,
and by a large margin.
```
<div class="figure-label">Fig. 2: Sutton’s thesis, in his own words.</div>

He walks through chess, Go, speech recognition, and computer vision. Each time,
researchers built what they knew about the domain into their systems. It
helped in the short term, then it plateaued, and eventually it lost to methods
that leaned on ever-cheaper computation. His verdict: “The two methods that seem
to scale arbitrarily in this way are search and learning.”

It’s a good essay, and I agree with most of it, begrudgingly. But a few
things are worth pointing out, because, again, the nuance tends to get lost.

Sutton isn’t saying that algorithms don’t matter. Finding methods that make
good use of computation matters a great deal! He also isn’t only talking
about training, since search at play time is one of his main examples. And
the claim is about how to build methods that keep improving as computation
scales, not about every system those methods end up in.

But it’s also a pretty constrained essay that leaves many things unsaid, and
people are starting to put words in its mouth already.

## Unsaid things

The thing I see most often is a slide from general methods to general models to
general systems, as if they were the same thing. A general learning method can
produce a very narrow model. A general model can be used to train a
specialist, for instance by fine-tuning it on a single task, or by using it as
a teacher for a model that only ever sees that task. And none of this says
anything about whether the hardware it runs on should be general. That would
required quite a different argument.

The essay also isn’t a theory of cost. It doesn’t tell you to run a large
generative model when a small classifier does the job. It doesn’t tell you how
to weigh one training run against the billions of inferences that come after
it, or when the cost of keeping a system general starts to exceed what you get
out of it, or when engineering effort pays for itself in operating cost. This
is hard, bespoke work, and only tangentially related to engineering (though it
can be influenced by it).

A method that scales better is not necessarily a system that’s cheaper to run.
A general method might be the right way to find a solution, and a specialized
system might be the right way to deploy and run it!

I’d argue that the choice between the two is mostly economic. Generality
saves you from engineering each solution separately. Specialization can save
you recurring cost. That matters more as a workload becomes predictable
and its volume grows, because the engineering cost gets amortized. This is true
for software, for models, and for physical machines, with very different
numbers attached to each, many of which I don’t know, because my expertise is
pretty narrow in this domain.

Specialization doesn’t always win, of course. But we should do the calculation
and apply diligence instead of just assuming the answer. If anything, general
learning methods might make specialization cheaper, because they let us derive
the specialist from the generalist instead of building it by hand.

## Folding napkins

I got here for a reason.

A few days ago I listened to [the Latent Space interview with Jason Ma of Dyna
Robotics](https://www.latent.space/p/dyna). Dyna adapts one large generalist
model to new tasks, and their biggest deployment folds thousands of napkins a
day at the restaurant chain Din Tai Fung. When co-host Vibhu asks whether a
specialized machine couldn’t do that, Ma answers that napkins are deformable,
and that adapting a general model gets them to production quality faster. A
few minutes later, swyx sums it up:

```
The term that comes to mind is kind of like bitter lesson for robotics,
right? You, instead of the specialized robots, which is a lot of industrial
engineering, you build a generalized platform that can- Do a last-mile
fine-tuning [...]
```
<div class="figure-label">Fig. 3: swyx, around 41:07.</div>

Nobody reacts, but Ma’s reasons are about developing the capability, and
swyx’s summary is about the machine. And the important part, I think, gets
dropped, namely what it costs to operate a general-purpose robot at a napkin
station, day after day, for years, with many of its actuators and sensors
disused<sup><a href="#2">2</a></sup>.

And deformable objects are hard, sure. But the robot isn’t folding napkins on
the back of a moving truck. It’s standing in the back of a restaurant, in an
environment someone controls.

As I understand it, specialized machinery usually works by changing the
environment until the task is simple enough for a simple machine. If we
skip that step, I think we’re arguing that an early Ford-style assembly line
staffed with humanoids beats a dark factory full of stationary robots, each
built for one job.

For what it’s worth Boston Dynamics went the other way with Stretch, their
warehouse robot. They [built “a simpler robot”](https://spectrum.ieee.org/stretch-is-boston-dynamics-take-on-a-practical-mobile-manipulator-for-warehouses)
for the job, “with pieces from Spot and Atlas”, while Atlas continues as a
separate humanoid project. As far as I can tell, the generality there lives in
the process that produces the machine rather than in the machine itself.

## Just scale it

This is, then, what actually bothers me. Someone points out an inefficiency or
a limitation of an architecture, and proposes looking into a different
approach. Someone else says “just scale it”, or “hand-engineering doesn’t
work”, or simply “we’re Bitter-Lesson-pilled” (I’m way too old for this, by
the way). From there, the conversation is dead in the water. Throw in some
Knuth if you want to, we’re arguing from authority from here on out.

These statements borrow authority from a historical observation without
showing why it applies to the problem at hand. And they invert the burden of
proof. The person proposing an optimization now has to argue against the
presumed inevitability of scale, while the person advocating scale doesn’t have
to show why it’s the better answer right here, right now.

It’s become a bit: instead of using Sutton to question our architectural
intuitions, we’re already using him to defend the status quo.

## The irony

Sutton’s essay is, at its core, a criticism of researchers getting attached to
their own ideas about how intelligence works. He even names the mechanism:
“There are psychological commitments to investment in one approach or the
other.” And then:

```
We have to learn the bitter lesson that building in how we think we think does
not work in the long run.
```
<div class="figure-label">Fig. 4: Sutton, on attachment.</div>

I think the meme risks encouraging attachment to a different set of ideas: that
generality is always desirable, that scale will fix present architectural
problems, that specializing is a lack of ambition, and that engineering effort
is wasted if computation could theoretically learn the solution.

That’s basically the failure Sutton described. And because the original
essay is genuinely insightful, its authority makes the new dogma harder to
question, even if they just absorbed the headline.

## Fin

Of course I don’t think the Bitter Lesson is wrong, and I also don’t think
everyone invoking it misunderstands it. My problem is that its authority
increasingly lets people skip thought, the same way Knuth’s sentence did. Both
essays ask for more work, not less! Knuth argues for measuring before
optimizing, Sutton for checking our intuitions against what actually holds up
as computation grows.

So, instead of arguing about it, let’s do the work.

#### Footnotes

<span id="1">1.</span> A paragraph earlier he goes even further: “In
established engineering disciplines a 12% improvement, easily obtained, is
never considered marginal; and I believe the same viewpoint should prevail in
software engineering.” I aim to be as unapologetically pointed in my
arguments as he is one day.

<span id="2">2.</span> I’m not a roboticist. I’ve never built a robot that
does anything useful, and I’m sure people at Dyna have thought about unit
economics a lot more than I have. My complaint is about the shape of the
argument, not about their business.
