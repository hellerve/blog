---
title: "cyberwitchery"
date: 2026-10-31
---

I’m starting a company. It’s called [cyberwitchery](https://cyberwitchery.com),
and it’s a small product and consulting studio. For now, it’s just two people:
[Erik Svedäng](https://eriksvedang.com) and me.

If you’ve read this blog for a while, you might know Erik as the author of Carp
and the person I’ve been bugging with questions for the better part of a
decade. Now I get to bug him professionally, which is nice. At cyberwitchery,
he currently mostly works on alembic, which I’ll get to in a second, and
a bit of [metacarp](https://blog.veitheller.de/Metacarp.html).

Some of you have already seen the name cyberwitchery. It was on my slides this
summer, it’s in the URL of [metacarp](https://github.com/cyberwitchery/metacarp),
and I promised [a sizable announcement](https://blog.veitheller.de/talks.html)
in a post a few weeks ago.

This is that announcement, so let’s have a look around!

## What we sell

We have a few products, two of which are public right now.

The first is [alembic](https://cyberwitchery.com/alembic.html). It moves
infrastructure data between systems like NetBox, Grafana, Arista AVD, and
Ansible, through a vendor-neutral data model you describe yourself. The core
and the inventory adapters are open source, and there is a commercial layer on
top for driving labs, DNS, and the rest of the operational tooling and for
integrating with your CI/CD pipelines and your chat.

The second is [divinate](https://cyberwitchery.com/divinate.html). It collects
the technical evidence behind security reviews and compliance work, and derives
claims from that evidence. It is meant to run continuously, to guard things
from drifting and to make sure your guarantees always match reality. It’s still
early and doesn’t have a commercial story, but it’s out there.

Others are not open source, and they remain under wraps for now.

Next to the products, we do consulting, talks, and workshops: network
automation, security, technical due diligence, and languages and developer
tooling. If you’ve hired me in the last six years, none of this will surprise
you. It’s more or less what I offered independently, with hopefully less
key-person risk and a prettier invoice. [The prices](https://cyberwitchery.com/#consulting),
too, stay comparable.

## Why bother

So why change anything, if the work is mostly the same?

The first reason is a bit selfish. For years, my professional work and my
private technical interests lived in the same place: my name, my GitHub
account, my blog. That’s fine when you’re one person, but it got harder to tell
where the consultant ends and the guy who builds arcane, weird shit in his
evenings begins, both for me and, I think, for the people hiring me. With
cyberwitchery, the professional half has a home of its own, and I get to be
less judicious about what I put on here, not that I ever paid it much mind
(that much should be obvious).

And of course I violated that principle immediately by making metacarp a
cyberwitchery project. I’m only human, and a company that makes developer tools
can reasonably own a compiler, I’d argue. Also, I wrote “we build hard software”
on the website, so we better bring receipts.

The second reason is scope. As one person, I can only take on projects that fit
into one person’s calendar. With two of us, and a company to put things under,
we can do bigger and broader things, both for clients and for ourselves. I also
have people I can trust to collaborate with, I can share the burden of my
projects, maybe even take a vacation<sup><a href="#1">1</a></sup>.

## Just because

The “do bigger and broader things for ourselves” part is what I’m most excited
about, if I’m being honest.

There is [a paper series](https://cyberwitchery.com/papers.html), for instance.
It’s short write-ups of problems we find interesting, published as PDFs that
look like papers. They’re not peer reviewed, and they’re not always about
anything we sell. The first one is about [a trolley game in which your choices
are remembered](https://cyberwitchery.com/papers/cw001-every-pull-a-lesson.pdf),
the second about [a drawing machine that tries to learn my
taste](https://cyberwitchery.com/papers/cw002-teaching-personal-taste-to-silicate.pdf),
the third about [metacarp](https://cyberwitchery.com/papers/cw003-metacarp.pdf).

And there’s merch! I’ve always been too cheap to do cool stuff with my own
money, but if it’s company money it’s totally different, right?

As a small taster, we printed postcards with generative art on the front,
five designs in total. Each of them puts the logo in the middle of a different
algorithm: streamlines, tiles, circles, particles, and lines. The back has the
algorithm itself, condensed into a few lines of pseudocode, so you can draw
your own if you want to<sup><a href="#2">2</a></sup>.

<img src="./assets/postcards.jpeg" alt="the five cyberwitchery postcards, printed">
<div class="figure-label">Fig. 1: Postcards!</div>

Here is the smallest one, from the back of the circles card:

```
for x, y in grid(100):
  d = random(40, 90)
  circle(x + 50, y + 50, d)
```
<div class="figure-label">Fig. 2: A complete artwork sans logo.</div>

Not strictly necessary for a consultancy, but it’s good for the soul. I think
a small company without external investors to answer to should be allowed to be
weird, and I’d rather spend some of our time on things like these than pretend
we only care about billable hours. If you do want to pay for some art hours,
though, I won’t say no, of course. Otherwise, just approach me at one of the
upcoming conferences to get your very own artwork!

There is more to come. Starting cyberwitchery opened a door that I can’t seem
to close anymore, so now I’m thinking about clothing that doesn’t suck,
affordable full size art prints, and a book I’ve been writing. Not all on the
same timeline, naturally<sup><a href="#3">3</a></sup>.

## Fin

So that’s cyberwitchery: two people, a handful of products, consulting, and a
growing pile of things we’re making because they seem cool and we like cool
shit. If you want to work with us, [book a call](https://cal.com/cyberwitchery)
or write to [contact@cyberwitchery.com](mailto:contact@cyberwitchery.com). If
you just want a postcard, ask me the next time you see me.

This blog isn’t going anywhere, just to be clear, and neither are my personal
projects. cyberwitchery has [its own log](https://cyberwitchery.com/log) and
[GitHub org](https://github.com/cyberwitchery) for the professional things,
and I’ll keep building and writing about whatever I want here.

That’s all, folks! See you around!

#### Footnotes

<span id="1">1.</span> I’ve taken a grand total of one week off this year,
which is better than last year, and terrible generally.

<span id="2">2.</span> If you ask me nicely I might also just share my code,
but that might take away the thrill of the hunt.

<span id="3">3.</span> If you follow me online, it might be obvious, but I’ve
been going pretty pedal to the metal for the past 2 years or so. A bit of a
normalization and regular people timelines might be good for the ol’ noggin.
Founding a company might seem like a step in the wrong direction if this is
the goal, but I assure you that independent life is just as wild.
