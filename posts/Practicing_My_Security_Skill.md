In my life as a programmer, I’ve always had a side project. Something that
interested me and kept me sharp. Conversely, I’ve had a harder time practicing my
security skills; the projects often felt weird and artificial, and the practice
was distinctly less fun. And I know I’m not alone in this.

A lot of CTF practice games feel artificial. [Hack the Box](https://www.hackthebox.com/)
never spoke to me, just like [Leetcode](https://leetcode.com/) never did: it’s
deliberate practice, but it feels more like Duolingo than going to a foreign
country for a while<sup><a href="#1">1</a></sup>.

But, just as I attribute most of the skill that I have as a programmer to my
constantly changing and always obsessively followed side projects, my security
work also thrives off of practice. The obvious conclusion is that I need to
practice more like I do for programming.

## Security practice as play

I approach my side projects playfully. Every once in a while, I slide [into
something more serious](https://github.com/carp-lang/carp/) or [scratch a real
itch](https://github.com/cyberwitchery/netbox.rs), but I usually just try to
explore an idea or learn more about a domain.

I had to learn that I can approach security projects similarly: writing [a
minimal debugger](https://git.veitheller.de/degrowth/d) helped me understand how
to poke around in memory and processes better. Similarly, making all CI
pipelines over at [the GitHub presence](https://github.com/cyberwitchery/) of
[cyberwitchery lab](https://cyberwitchery.com) include static analysis, SBOM
generation, and coverage (needed or not) helps me understand compliance and
quality gates and the involved tools.

This is not to say that all my side projects are artificial exercises. Some of
my explorations fill real gaps for me, such as [`sbom-diff`](https://github.com/cyberwitchery/sbom-diff),
which allows me to scan deltas between SBOMs quickly, and
[`unsafe-budget`](https://github.com/cyberwitchery/unsafe-budget), which lets me
limit how much `unsafe` code I allow in my Go and Rust projects (other analyzers
to follow as I need them).

And this isn’t limited to application security and secure code, either. I try
fun things on the Hetzner and DigitalOcean boxes I shepherd for private purposes,
though this rarely leads to a GitHub repository.

And while none of these projects are necessarily similar to the work I do in the
security space, where I might more often think of [SSDLCs](https://blog.veitheller.de/Simple_SSDLCs.html)
or how to structure network policies between services in Kubernetes clusters,
they train my mindset and keep my knowledge of the tooling and ecosystem fresh
and up-to-date.

## Giving yourself space

I like to give myself many distinct spaces for exploration. I’ve made the
idiosyncratic choice of making distinct GitHub organizations for my [programming
language experiments](https://github.com/hellerve-pl-experiments) and
[project-based learning](https://github.com/hellerve-project-based-learning/)
efforts. If I were honest, most of my repositories would have to go into my
project-based learning bucket, but oh well.

Security is no different. I have a few virtual machines that are just there to be
banged up and stripped for parts for security projects. But because I am the
weirdo that I am, I like to make my own labs and build them myself rather than
using a third-party service. It just feels materially different to have your own
space to build and break in.

In these little worlds I control everything: what services run, what kernel
modules are installed, and how I move laterally or lock myself out. I can be
attacker and defender, and while playing chess against yourself is often quite
boring, it forces you to think more deeply on every move if you want to get
somewhere.

## Play is inherently useless

Many of my toy applications look great until you use them and realize they’re
just sketches. But that’s okay. Not everything needs to be an application. Some
artifacts are their own point and need no product-market fit to have a right to
exist.

I think most programmers intuitively know that. In my experience, security folks
often think differently, and their play is often paired with a certain amount of
utility. That’s great if you can make it work, but it shouldn’t constrain your
explorations. You might never find a [Sun Ray](https://en.wikipedia.org/wiki/Sun_Ray)
workstation in your engagements, but that doesn’t mean you shouldn’t learn how
to break its kernel<sup><a href="#2">2</a></sup>.

## Fin

I like building things and breaking them equally. Breaking things, while in
my opinion inherently easier, is also a craft that can and should be practiced if
you take it seriously.

I hope that in this blog post I was able to give you some small ideas for
building a personal practice routine that works for you, and I made clear that it
doesn’t have to be useful or look like anyone else’s. It needs to be fun,
engaging, and keep you motivated to get better and try more new things.

#### Footnotes

<span id="1">1.</span> A notable exception is Alex Popov’s [Kernel Hack
Drill](https://github.com/a13xp0p0v/kernel-hack-drill), which I found fun because
of the kernel exploit angle, and the self-managed route that I took. So I’m not
saying it *can’t* work, just that it doesn’t usually speak to me.

<span id="2">2.</span> Just to be clear: I have no idea how `exec` OS worked or
what it did. I did, however, once see a SPARCStation in a data center rack. It
couldn’t be turned off, because it was still running the GIS service.
Unfortunately, I didn’t ask any follow-up questions.
