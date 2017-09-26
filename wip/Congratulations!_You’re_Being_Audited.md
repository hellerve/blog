I’ve been back at my job from my brief stint at the [Recurse Center](https://www.recurse.com/scout/click?t=3f214b4d8605308d27685ebd4548905e)
for over a month now and have largely settled back into a regular work week.
For those of you who don’t know it, [I work at Port Zero](http://port-zero.com/),
a small but efficient security consultancy from Berlin. While my business card
says something about “Software Engineer”, these days I audit about as much code
as I write for our clients. I figured that a considerable amount of my readers
might not know what an audit entails and what to expect from it, especially if
it’s focused on security. Let me tell you about it!

## The process is a lie

First things first: unless your company is seeking accreditation in some
restricted field of tech, chances are that there either is no standard process
or the people who audit you will, for various reasons, not adhere to them. Of
course most of us with some experience will look at a standard set of
checklists and see whether we see anything from there—e.g. usage of Python’s
[`pickle`](https://docs.python.org/2/library/pickle.html) module on
a user-supplied datum—but oftentimes security holes aren’t that simple to find
and require the interplay of various different components in your system. These
are the times when automated analysesi & tools often don’t cut it anymore and
someone like me appears on the scene.

In the following I’ll talk a bit about my methods and tools. It’s not magic,
just a trained eye and a helathy dose of masochism. The former is required to
read through large amounts of code of varying quality and make a ton of notes
over prolonged periods of time.<sup><a href="#1">1</a></sup>

## These magic sausage fingers

I review code in a variety of programming languages, but the process changes
surprisingly little on a micro-level. When I scan through lines of code and
functions, all languages are created equal.<sup><a href="#2">2</a></sup> The
differences come with the architecture, or the interplay of components.

Secure programs are a subset of correct programs. As such, security bugs are a
subset of bugs in general, and sometimes a “regular” bug can have security
implications. This means that I, as an auditor, are constantly on the lookout
for code smells. If something icky is going on an attacker might be able to use
some subtle behaviour of the code to take control of the app.

To illustrate that last point, let’s assume that you maintain a magic web
application that herds unicorns. Users upload their unicorns, you assign them
an ID number, and the unicorn gets to frolic through the meadows with its new
friends. One of your colleagues thought that 32 bit is too much wasted space
for an ID, so they create their own ID class that creates a random number for
each unicorn based on its mane color. Sadly they forgot to take care about
uniqueness, and so sometimes a unicorn gets assigned a number that was already
assigned. What sounds like a regular bug quickly turns into a security problem
when it turns out that due to the lack of checks—you didn’t think ID creation
could ever fail—your application crashes every time. If a unicorn-hating hacker
or competitor ever finds out that this bug exists, they will send a whole bunch
of unicorns with very similarly-looking manes to your service and constantly
crash it. This is a very silly transcription of a simple security bug I found
at one of my clients—these things happen, and we found it in time and fixed it.
Bugs happen. Sometimes they’re benign, but as soon as they’re dependent on user
input and reproducible it could turn into a security incident at any point in
time.

#### Footnotes

<span id="1">1.</span> You might think it’s more gratifying to look at bad
code, since you can add more business value and address more needs that way.
That’s not true for me: I enjoy well-written and architected code almost as
much as good prose. I actually like my job. I don’t enjoy reading bad code all
that much, and standing in front of the development team and talking them
through a truck load of bugs they produced is not pleasurable for me—I’m not a
monster.

<span id="2">2.</span> This isn’t strictly true: there are some differences
between languages that are memory-safe and those that aren’t, because the
auditor has to watch out and think through another class of bugs when memory
management comes into play. Of course there’s also the garbage collector, but
that’s mostly a problem for the perfomance people.
