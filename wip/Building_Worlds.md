Time and again we hear that software engineers should behave more like civil
engineers. We’re careless, our projects often fail, and all in all we’re lacking
professionalism. All of this is true, and yet I disagree with the conclusion.

To me, software engineering is nothing like civil engineering. We’re mostly
unencumbered by the rules of the real world. There is no physical environment
that dictates the design of my software<sup>[1](#1)</sup>.

If I wanted to make it sound mythical, I’d say that we build
worlds<sup>[2](#2)</sup>, and that’s a good title for this blog post. But it’s
not actually a good analogy<sup>[3](#3)</sup>. Most of our programs are worlds
embedded in other worlds—say, Operating Systems—, which are in turn embedded
and so on until we reach the haptic universe again. All of the worlds that we
embed our shaky edifices in influence them—and, alas, often make them even more
preciously fragile.

This distinction does not seem particularly useful at first, even pedantic. But
it’s useful if we use this notion to guide us deeper into a narrative.

# A narrative

All of the models we build interact with their environments; if they don’t, they
are not useful, they will simply heat our computers. That means that every
single program has an interface that ties it to some outside world, which could
be “real” in the haptic sense or not.

But what goes on inside the program is up to the programmer. The constraints
will surely guide the process, but how the program “perceives” the world—if I
were less obtuse, I would probably call that its “architecture” or “system
design”—is not yet fixed<sup>[4](#4)</sup>.

This is not true at all when we talk about the physical world. As Zach Tellman
notes in his talk [“On Abstraction”](https://www.youtube.com/watch?v=x9pxbnFC4aQ),
we have to use deductive reasoning to understand the physical world, and
inductive reasoning to build our systems. Physicists are descriptive,
programmers are prescriptive.

But so are civil engineers, to an extent.

What is different there is that the unboundedness of the possibilities in the
abstract world become coercive; in the words of Zach Tellman, “No one tries to
add a carwash to a bridge when it’s halfway built”, yet people do all the time
when it comes to software, whether the world we built along the way allows for
this or not<sup>[5](#5)</sup>.

So, do civil engineers have it easy?

Malleability can be as beautiful as it is dangerous. We *can* add a carwash to a
bridge when it’s halfway built, and it doesn’t even have to be messy. How to do
this is left as an exercise to the reader.

## Fin

Zach Tellman asserts that one of the fundamental problems of software is how to
deal with change, and I agree. But that we can deal with change in the first
place, and shift our universe so that it accomodates these new facts and
requirements, is one of the most beautiful things about software developments.

In my opinion, the people that know most about what we do are artists,
philosophers, and mathematicians, no matter how often you’ll call me an
engineer.

#### Footnotes

<span id="1">1.</span> That is, of course, untrue. I often care about the real
                       world in my programs, because I need to earn money, and
                       that often means that I have to taint my programs with
                       certain representations of the haptic universe, like
                       inventories or seating arrangements. Even worse:
                       sometimes I have to care about the physics of the real
                       world, because my program wastes too many clock cycles.
                       But this is not primarily what I’m getting at in this
                       post, so let’s postpone this discussion.

<span id="2">2.</span> The requirements and rules that govern *these* worlds
                       seem to change almost daily, which would make living in
                       them fairly stressful.

<span id="3">3.</span> If you really think you’re building worlds, you’ll end up
                       with a severe case of megalomania, and that’s fairly bad
                       for business.

<span id="4">4.</span>  Sometimes these assumptions take over the interfaces
                        and everything goes downhill from there. I’ve written
                        about [abstractions vs. implementations](https://blog.veitheller.de/Abstractions.html)
                        before and touched on that, and Zach Tellman has more
                        [thoughts on abstractions](https://www.youtube.com/watch?v=x9pxbnFC4aQ)
                        than I could ever internalize.

<span id="5">5.</span> If you’ve ever asked yourself why the wrong abstraction
                       was chosen but the people seemed experienced enough to
                       make a better decision, this could have been the reason.
                       I’ll talk about legacy software at one point on this
                       blog, I promise.
