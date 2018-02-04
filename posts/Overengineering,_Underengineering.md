In my job as a consultant, I get to see a lot of different companies and teams.
This is a big privilege; oftentimes they are struggling and need me to help
them figure out how to get out of all kinds of unfortunate situations. I’ve
worked with amazing teams that solved challenging problems as well, don’t get
me wrong, it’s just not the norm.

And then there are times when the collaboration is just magical. Today
I want to talk about a particularly enjoyable experience.

## Some background

For almost a year, I helped [ecix](https://www.ecix.net/), an Internet Exchange
Provider, stay on top of their infrastructure. We built traffic monitoring
systems, internal resource management software, and solved a lot of thrillingly
hard problems together. My coworker and informal supervisor at the company was
[Matthias Hannig](https://github.com/mhannig/). We became close friends, and he
profoundly changed a lot of my views on technical matters.

We were tremendously productive together, despite our often disparate
intuitions. We often have different gut reactions when it comes to designing
systems, and our code reads very differently. How, then, did we work well
together, and manage to churn out robust systems in reasonable time?

## The magic condiment

Matthias likes to solve things “the right way”. His solutions are often very
clean, often dazzlingly complex, but incredibly robust. As I’ve [written
before](https://blog.veitheller.de/Going_Static.html), the quality that I value
most is simplicity. A lot of the software I work on in my spare time is
extremely simplistic, to the point where it approaches being unusable. But it
always solves a problem.

Naturally, I dismissed many of Matthias’s ideas as overly complex. He, in turn,
would often be taken aback by the naivety of my solutions. What saved us was a
deep mutual respect.

Sometimes, we would admit that one idea was obviously superior, and go with it.
I like to believe that the ratio was balanced, and if it wasn’t, neither of us
cared. What we care about is building good software, after all. Somehow our
egos never got in the way—although both of us are quite fond of ourselves.

Sometimes, we would merge our ideas, balancing over- and underengineering.
Don’t get me wrong, I’m still a proud underengineer; building the bare minimum
and seeing whether that’s enough is my favorite pastime. But sometimes it
isn’t enough, and even I know that before trying it. Or the minimal version
isn’t as conceptually beautiful as an alternative that might be a little more
work to build, but conceptually simpler.

I’d like to highlight that last point. Sometimes the idea that is the least
work to build isn’t simple at all. It might be conceptually confused, or
architecturally suboptimal. [I’ve talked about abstractions
before](https://blog.veitheller.de/Abstractions.html); Matthias played an
important role in shaping those ideas. In fact, I first had the idea while 
going for a smoke together with him.

## So?

There are three things that I take away from my reflections on this time; the
first being that a well-rounded team can be small, but it can’t agree on
everything. Even the most experienced programmers will have fundamental
disagreements here and there, and that’s vital for the product to be any
good.

The second idea is that it’s okay if your instinct is to over- or underengineer
the systems you build, but you need to choose your team mates accordingly. This
ties back into the first point: if you know your tendencies are one-sided, you
need to find someone who disagrees with you, and talk to them.

The third idea is decidedly technical: abstractions are a good way of keeping
the balance between conceptual and technical minimalism; they help build
systems that are simple even when their implementation isn’t. What sounds like
a syllogism starts to make sense if you think of any well-made modular piece of
software: you might be able to describe what they do in a few words, even if
they’re dazzlingly complex. You can repeat that on any level of granularity;
you might, for instance, know what a function does without knowing how it does
it.

And maybe there’s a fourth, implied lesson: don’t let your ego ruin what
could’ve been a fruitful collaboration.
