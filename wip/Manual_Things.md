I’ve realized something important about my interests in the last few months: I
don’t actually care about compilers. Some of you might shrug and move on now,
but some of you who are familiar with my work and what I’ve talked about and
done in the past, this might come as a surprise.

What I really care about is tools, for programmers and tinkerers, for
explorative work in one of the most abstract domains of the industrial world.
For that to realize, I needed to get familiar with one of the most refined—and
obscure—tools in our industry, [Pharo](https://pharo.org) and the [Glamorous
Toolkit](https://gtoolkit.org). I’ve gone from dismissal over grudging
cooperation and burning passion to the realization that I want to help build
something like this. Not necessarily because I think it is cool and fun to play
with—although it is!—but because I think that it’s the only way through which
we can achieve any kind of reusability of software.

Now that we’ve got all of the bold claims out of the way, I’d liek to tell you
a little bit about how I ended up here and then substantiate those claims a bit
more.

## What I did this summer

I’ve spent the last two months or so working with Smalltalk, or more accurately
with Pharo and the Glamorous Toolkit I linked to above. I’m helping to build a
large-scale Software Portfolio Management System. At the end of the day, it’s
about budgeting, accounting and planning.

I was very reluctant at first: I was going to program in a language I had never
touched and that seemed very self-important, building a system for an
environment that I didn’t know anything about or care much for. It seemed like
a drag to me.

I decided to change my mindset and see it less as one of my usual consulting
gig, and more as a way to learn and grow. This reframing made learning
Smalltalk much more enjoyable and productive. I wasn’t sold right away, though.
I now know why: learning the language took a few hours, learning to use the
programming environment productively took around a week—and I’m still learning
new stuff most days! This fact is made ever more prominent by the fact that you
rarely get very far by looking up stuff on the Internet, much less Stack
Overflow. The ways of accessing documentation and information inside Pharo are
amazing, but you have to learn them in order to become even a little
productive. This is different from how I am used to work and makes for somewhat
of a steep learning curve in the first few days<sup><a href="#1">1</a></sup>.

There are other odds and ends that are different, like packages and working
with version control and such. It helped knowing why they were different in
order to enjoy them as a breath of fresh air rather than getting frustrated.
The Pharo team put a great deal of thought into how they organize both the
internals and the UI of every piece of machinery in the system, and gladly
communicate them. This is absolutely crucial if you don’t want to annoy more
seasoned people with an existing workflow; after all, if you want us to change
our ways, you better have the arguments to get us on board. And boy, are those
arguments good. So good, in fact, that I’m not going to just blindly parrot a
barrage of convincing arguments about problems you don’t even have—or maybe not
know you’re having?—and instead am going to challenge you to give Pharo a try,
see what about it doesn’t make sense to you, and try to find out why. If you’re
stuck or unable to find information about it, you can always ask around on the
mailing lists or chats, or just [send me an
e-mail](mailto:veit@veitheller.de)—although I’m by no means an expert.

I’m usually a Vim and command line person, because a bulk of my work crosses
language boundaries and involves accessing remote machines and the like, and
doing this from an IDE has always been tedious enough to make me stick to tools
that, while they are more primitive, compose much better.

The project I’m currently involved in is different. I’m working on a desktop
application with a fat client—at least for now—, and I don’t really have to
touch the command line all that much. But even now I have other projects
going on where I still use my old workflow. This means I have to switch
contexts every time I switch projects even more than usual. This puts Pharo in
a situation where it has to offer me productivity gains beyond these losses.

For ordinary IDEs the question “do I gain more productivity using you than I
lose?” could always be answered with no. This is due to a variety of reasons,
mostly that they are glorified text editors with more colors and buttons. This
is obviously a dramatization, but it’s true that there is a smattering of tools
that could be part of any IDE—and have been in the past—but aren’t. A lot of
the arguments surrounding this issue are technical, even though it has been
demonstrated that much more sophisticated interactions than pure text editing
can be implemented on machines that are much more constrained than most
developers have on their hands at the moment.

Pharo is not usually constrained by technicalities. The people working on the
system are primarily researchers. Researchers are only constrained by their
imagination, and often their funding. This has led to Pharo being the most
sophisticated IDE I’ve ever worked with, if not the most stable.

Let’s move from the abstract to the concrete now, and examine some of the tools
that are useful, present in Pharo, and not really as ubiquitous as I’d expect.

## Tools for productivity

#### Footnotes

<span id="1">1.</span> For me, working through the [Online
 Course](https://mooc.pharo.org/) was the single most helpful thing. It has a
nine week syllabus, but I was able to get through the first five weeks in a
matter of days, after which I felt proficient enough to actually start working
on my own. I can thus highly recommend it if you want to start playing around
with Pharo.
