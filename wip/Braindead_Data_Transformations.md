I've released a stupid but potentially [useful tool](http://github.com/hellerve/manipulator)
today. It might be a bad idea—I am not completely sure yet—but the idea was
so intriguing that I just could not resist building it. It is a simple data
retrieval and transformation tool for Python datastructures, and like so
many other tools that I've developed in the last few months the idea came
from a conversation I had with a coworker. The underlying theme was simple:
data transformation is always a pain, and often it seems like it does not
need to be. Select the data you want to change and apply some transformation
to it. How hard can it be?

One of my favorite implementations of that idea is [Clojure](http://clojure.org/),
a name that should ring a bell if you are developer. Clojure emphasizes the use
of standard data types, and working with them is always fun, because most functions
work fairly generically on all of the types thanks to [the seq abstraction](https://clojure.org/reference/sequences)
and [transducers](https://clojure.org/reference/transducers), two simple yet
amazingly powerful concepts that are central to how Clojure works with data.
I have tried to emulate that concept in [zepto](https://github.com/zepto-lang/zepto),
sometimes well, sometimes not so much. It is just amazingly rewarding to have
algorithms instantaneously work because the functions it uses compose nicely.
I would use it more if I did not require a JVM on the machines I interact with,
which at the time is a deal breaker for my clients and me, sadly.

At work I mostly use Python. And I love Python. I know my way around it pretty
well, I've written hundreds of thousands of lines of code in it by now—a chunk
of which can be found on [my Github](https://github.com/hellerve/). But sometimes
working with it feels suboptimal, and I think that is mostly due to the lack of
composability in the core of the language. Maybe it is just my bias as a fan of
functional programming that makes me shiver whenever I see the reimplementation
of large swaths of code in different parts of project because of some badly
constructed class or module that intertwines all kinds of unrelated behaviour,
or maybe it is a real problem. I feel unqualified to judge that due to a
fundamental lack of objectivity. Let me rant about the tool for a second
instead.

## Making the best of my mess

`manipulator` is relatively hacky. The initial idea was to apply CSS-like queries
to data to retrieve relevant parts of it or update specific parts of the data set.
CSS was designed for selecting specific parts of a website, so it was relatively
clear that it would probably be better than any ad-hoc, badly specified DSL I could
come up with. Nonetheless, I decided to use a fairly small and weirdly twisted
dialect of CSS to make it work better with how Python data is structured—building
what amounts to an ad-hoc, badly specified DSL.

Against all odds, it seems to work fairly well for me. The code base is tiny and
really easy to understand, which is a good thing in my book. Still, I would not
suggest you use it if it does not scratch a *really bad* itch in your code. The
reason for that is really simple: it is simply not idiomatic Python. It looks
relatively functional from the inside—if you squint hard enough and ignore the
mutations, that is—, which is not generally a good starting point for Python in
my opinion. I find myself writing functional, or rather procedural, Python code
very often for small data-driven tools, because I find the data flow is much more
easy to reason about like that. Another toolset of mine, [hawkweed](http://github.com/hellerve/hawkweed),
tries to make actual functional constructs work well in Python. It has not been
very popular, and I think that this has two primary reasons. Firstly, the
documentation is very suboptimal, because it has largely been a tool for my own
code or people who know the librarys capabilities already. The docstrings within
the code are extensive, but discovering what functions are actually there is
hard. This approach puts the onus of learning about the tools usefulness on
the user, which, in my experience, does not work very well.

The second reason is rather simple: Python is not a functional language. Most
Python programmers I know do not think in terms of higher order functions, currying,
and other constructs that are generally employed by people who work primarily in
functional languages to manage complexity, and so they do not understand how these
programs can be useful for abstractions. This is not primarily due to willful ignorance,
but because Python was not designed to work like that. It took me a long time to
accept that programming languages are not what I want them to be. Sure, I can bend
them until they fit into the box that I work in, but that is not how I should go
about things if I want to work productively with other people.

## Back on track

I still use `manipulator`. I have designed it to work with a set of small internal
tools for a client of mine. These tools interact with a database and transform data
into different shapes for monitoring and analytics. This is a perfect fit for a tool
like that: you know the shape of the input data, you know how to transform it, mostly
statically, only influenced by the input data itself. It is almost a pure system and
lends itself well to reasoning.

My conclusion from this little experiment is that sometimes hacks are acceptable, if
they are kept local and do not taint any other part of the program but rather enrich
the overall programming experience. Your mileage may vary.
