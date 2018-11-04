We all complain about legacy software. It’s very rare that I work with a company
that speaks highly of the software that has been in place for a long time. In
fact, I often get hired to “fix” such systems. Thus, I have to spend a lot of
time looking at other people’s code, wondering why it is there, and reassemble
it in a way that is better on some metric—readability, speed, and size are the
most common ones.

Refactoring can be one of my favorite activites in programming. If you have the
time for it, it’s often rewarding, interesting, and makes you think very deeply
about the domain you’re operating in. Of course, that poses the question why and
how legacy systems develop and why noone takes the time to refactor them.

In this blog post, I want to talk a bit about what I think are some of the
biggest reasons that legacy systems are often synonymous with “big balls of
mud”.

## Time is Money

Most programmers don’t like to think about money<sup><a href="#1">1</a></sup>.
Yet, for most of us, it’s the sole reason we work on software—at least in our
day jobs. If what we’re doing does not add value, we have no space in the
economic sphere<sup><a href="#2">2</a></sup>.

What does that mean for legacy software? We’re often in a situation where we’re
under pressure to push out features, often more than one at a time. This puts
us in a position where we have to make trade-offs, both qualitatively—what
feature should I work on first?—and quantitatively—how much time do I have to
spare to work on this? These questions are hard, and at least one of them
shouldn’t be answered by a programmer, but by a product person, and yet we have
to find an answer.

The most intuitive—and, in my experience, most often used—metric for choosing
what to work on becomes “What sounds least dreadful?”. The more dreadful the
task becomes, the less time we allocate for it, because want to be done as
quickly as possible. I’d argue that both of these metrics are counterproductive.

All other things being equal, my rational self would rather work on the task
that makes the least excited: getting it done alleviates the most psychological
pressure<sup><a href="#3">3</a></sup>. I’d also like to spend as much time on it
as possible to address the core issue, and make whatever puts me off of the task
less daunting.

#### Footnotes

<span id="1">1.</span> It is for this reason first and foremost that I believe
                       we shouldn’t call ourselves “software engineers”. To be
                       real engineers, we’d have to spend some time thinking
                       about the ROI of our work and the expected lifetimes of
                       our components, just as a mechanical engineer thinks
                       about the wear of the components they’re putting into
                       their products.

<span id="2">2.</span> I’m very much an anti-capitalist, but I like to function
                       in whatever system is put in front of me without
                       sacrificing my morals. For capitalism, it means that I
                       have to confront money head-on, and not be afraid of it.

<span id="3">3.</span> Being rational is hard, and I’ve been putting off things
                       just like anyone else; but I’m working on it!
