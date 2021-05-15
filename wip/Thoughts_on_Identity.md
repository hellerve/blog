Identity is a complex thing. Philosophers have been thinking about it for
millenia, it lies at the heart of such fields as semantics and ontology, and
I’d argue that everyone has, at some point, thought about it.

In computer science, and logic at large, identity is a fundamental topic, and
yet it is not something that my peers often talk about—but maybe I’m just
hanging out with the wrong folks.

Today, I want to spend some time capturing some embryonic observations on
identity. Maybe I will succeed in making them interesting to technologists, or
maybe it will remain idle thought. Maybe I will flesh them out at some point,
or maybe I won’t. I’m content with either.

What I cannot do, and will not attempt, is capturing the essence of identity as
a concept. I’m not a philosopher, and even if I were, giving definitive answers
is not something I am very good at.

## The illusion of identity

It seems simple to have an intuitive understanding of identity, especially in
the natural world: the pot on my stove is an entity, distinct from all other
pots. The countertop next to it is a bit worn, and I can identify it by looking
at all the individual stains and imperfections. But what if I were to take the
lid of the pot, or refurbish the countertop, removing all the stains and
planing it? They would still be the same pot and the same countertop.

This is true for living organisms as well. If I dye my hair and decide to
become a carpenter, many of my features would change, but I would still be
myself.

In some programming languages, values are defined by where in memory they lie.
They are mutable. A complex object stays the same object, even if a property
changes. For smaller objects this might not be as clear: if I change a
coordinate of a two-dimensional vector, even if it is in place, is it still the
same vector? This might be a clash of the conceptul vector, and the vector
object as it exists in the “real”, or in this case virtual, world, or it might
be an indication that something about my definition of identity is not quite
adequate<sup><a href="#1">1</a></sup>.

Other languages define it by equality: things being equal are identical. But
this might fall short in certain areas: two file objects might not compare the
actual file descriptors, but only the name of the opened file, and the mode in
which they were accessed. Is their identity the same, since they are considered
equal?

## The multiplexity of identity

Then, perhaps, it might make more sense to think of identity as not one fixed
lens, but a collection of classifications? Maybe all BILLY shelves from IKEA
are the same to me, or I only care whether this `Option` contains a value or
not, ignoring what the contained value actually is.

These might be considered classifications or groupings, or they might serve as
our identities in various contexts.

When I say “I identify as a mostly heterosexual cis male, a father, a husband”
this might not give you what you think of as a complete identity. It does not
tell you anything about me as a person. But it might be enough for you in a
certain environment.

Not all identities must be groupings. I can identify the number 5 by describing
it as “the number that comes after 4”<sup><a href="#2">2</a></sup> or “the next
prime number after 3”, among other things. Both are exact, and they only overlap
in the fact that they describe the number 5.

As such, identity is dictated by context, and can change as they change. While
I might only care that an `Option` is non-empty in one function, the next
function I pass it to might need to inspect the value it contains.

## The contextuality of identity

Some identity is meaningful mostly through context. A key might not be
unique—different keys for different locks might overlap—, but its identity is
described by the lock it was designed to open, even if, incidentally, it is
able to open all kinds of other locks<sup><a href="#3">3</a></sup>.

The same is true for some APIs: they were written to be used by one consumer.
That they ended up all over your codebase, polluting it with leaky abstractions
that make no sense, is an artifact of its identity, its use case.

This is often described as the model, the interface, and the environment. Some
systems thinkers think about the model and the interface, and argue that the
environment should not matter. But the model does not exist in a vaccuum, and
making that assumption leads to brittle design. It is, in essence, sloppy
thought.

## The siren’s call of identity

As with all highly abstract, fundamental ideas it is easy to get lost in them.
“Zen and the Art of Motorcycle Maintenance” stylizes this as “the high country
of thought”, and the book tells a vivd story of what can go wrong if you lose
your way.

Personally, I like to dip in and out of that “hight country of thought”, just
for a bit to entertain myself and maybe bring back something of value for the
rest of my life. Sometimes that works, sometimes I feel like my inquiries are
too shallow to provide real value.

In the end, all that matters to me is finding new ways to think about what I
do for a living, and my life at large.

#### Footnotes

<span id="1">1.</span> Another exmple to drive the point home: think of an
collection of objects in memory. If I need to change the location of the
collection because I needed to grow it in size and find a new contiguous block
of memory for it, does the identity of the elements it contains change? Their
location did.

<span id="2">2.</span> If you want to be mathematically precise about it, you
would probably say something like “the natural number that is the successor to
4”, but that would likely mean less to the majority of people you encounter in
your life.

<span id="3">3.</span> Locking mechanisms are weird and wonderful that way. If
you dig deep enough, [you might find keys that open all sorts of
things](https://www.youtube.com/watch?v=a9b9IYqsb_U).
