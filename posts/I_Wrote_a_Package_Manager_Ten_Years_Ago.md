---
title: "I Wrote a Package Manager Ten Years Ago"
date: 2026-09-07
---

Someone recently volunteered to take a stab at package management in
[Carp](https://github.com/carp-lang/carp). I promised I’d review and help, and
that I’d look into what I had done the last time I tried to solve the problem.

Because, you know, this isn’t the first time I’ve thought about the problem.
Ten years ago, I wrote [zeps](https://github.com/zeps-system/zeps), the package
manager for [zepto](https://github.com/zepto-lang/zepto), a Lisp I was working
on enthusiastically at the time. When I moved on to Carp, I even [threatened to
port it](https://blog.veitheller.de/Carp.html). I never did, and looking at it
today, I’m kind of glad I didn’t.

I remembered being very proud of `zeps`. I also know that I was 23 and, despite
having written a programming language, still mostly a junior developer with big
ambitions. So I expected the code to be terrible, like most of my code.

And it is indeed not great, but it’s much better than I thought. More
interestingly, some of it feels surprisingly modern. There are ideas in there
that I would still like to preserve for Carp, mixed with ideas I never finished
and problems I apparently didn’t know existed.

Let’s take a tour.

## The right shape

`zeps` called itself a package manager, but installing packages was only part of
what it did. It had commands for creating, installing, running, testing, and
documenting packages. You could launch a REPL with a package preloaded, create
a local sandbox, search the (planned but never finished) registry, publish
releases, and install tools onto your `PATH`.

The command surface looked roughly like this:

```
zeps new
zeps install
zeps run
zeps repl
zeps test
zeps doc
```

I still think language tooling should work like this. In my mind, the package
manager isn’t only a way to download code. It describes what working in the
language looks like. You work on your package, and your package manager should
be a companion. Ideally, you don’t need 15 tools.

To be clear, that wasn’t a novel idea in 2016. I was familiar with Clojure at
the time and probably copied a good deal of it from Leiningen, which already
had `lein new`, `lein test`, `lein repl`, and `lein deps`. Cargo existed as
well, but I don’t think I had ever looked at Rust at the time. But I was
copying the right thing, and I’m a bit surprised by how true to its ideas
`zeps` ended up being.

Templates, for instance, were packages themselves. `zeps new lean thing`
looked for an installed template called `lean`, loaded it, and let it build the
new project. Thus, no special machinery was needed to make a new package
template.

`zeps run` could install a package that wasn’t present, run it, and remove it
again afterwards. `zeps repl` loaded a package into an interactive session.

There were two documentation commands, one to inspect documentation in the
terminal and one to produce an HTML page for the whole package. The docstring
format even had a field for the computational complexity of a function, which
I find adorable, and a bit misguided. But mostly adorable.

There was also a project-local mode that redirected package installation from
`~/.zeps` into the current project. I called it sandboxing, but it really was
just a project-local environment. I didn’t want to touch the security part of
things. The need it addressed was real, of course. I should have just called
it a virtual environment instead.

None of these mechanisms were particularly polished. Most of them had the
right shape, though, I think.

## Versions, allegedly

Then we get to dependency resolution.

`zeps` had a semantic versioning library. It understood constraints such as
`>=1.0.0`, `==1.2.0`, and `<2.0.0`, and packages could declare dependencies in
their manifest. When it encountered one, it called `install` recursively.

That is more or less the whole thing.

There was one mutable slot for each package in `~/.zeps`. zeps did not build a
dependency graph before installing anything, could not resolve conflicting
constraints, did not detect cycles, and had no lockfile. It had no durable
record of which versions made up a working application. Two installs at
different times could produce different results, and a failed installation
could leave the package directory in whatever state the preceding shell
commands had produced. I never encountered this problem, of course, because I
didn’t have any users.

So I had basically implemented the syntax of version management without most
of the semantics. I cosplayed!

To be fair to myself, some of this was still being worked out in language
ecosystems at the time. Lockfiles were not universal, Go modules were still two
years away, and package managers disagreed about global versus local
installations (some things never change). But dependency solvers and reproducible
builds weren’t unknown or novel ideas. I simply didn’t know much about them,
and the Shai-Huluds of the world were still a decade away.

This is the part of zeps that is least useful to me when thinking about Carp.
There is a semver parser in the repository, but there is no package management
design hiding underneath it.

It’s also the hardest part of the system. To this day, I know what I don’t
want (Maven! Manual conflict resolution! Unreadable, undiffable lockfiles!).
What I actually want is much harder to articulate.

## Trust me

The security model has aged even better, by which I mean much worse. It was
quite funny to me to revisit, and the only moment where I cringed a bit.

A package manifest was a zepto file, and zeps read it by evaluating it. It
supported arbitrary code before and after installation. Installing from GitHub
meant cloning a repository, checking out a tag, evaluating the manifest,
installing its dependencies, running its hooks, and copying it into place.
Registry downloads had no integrity check, though one was planned. Installing
a command-line tool created a link in `/usr/local/bin` and tried again with
`sudo` if that failed.

Can you tell me all the ways in which this is a security nightmare? It might
be too hard to enumerate, honestly. But the idea of executable package manifests
especially wasn’t unknown. `setup.py` set a precedent, and before and after
scripts were absolutely normal.

And in fairness to my younger self, I did understand that publishing packages
needed some form of authentication. `zeps keygen` generated a 2048-bit RSA key
pair, and `zeps register` refused to publish without one. My plan was to
eventually sign releases fully, but at the time of abandonment, I just had my
little RSA library and exactly zero checks, so make of that what you will.

Honestly I think that that was a pretty good instinct from naive young me
building a package manager for his hobby Lisp. Package signing was not a new
idea, of course, but it certainly wasn’t part of every language’s ecosystem. I
was pretty early among language package managers (system package managers often
were already appropriately paranoid).

## Back to Carp

I opened `zeps` because I wanted to understand what I could contribute to the
new attempt. Unfortunately, I don’t think the code itself answers how package
management in Carp should work in any way.

There is something I would like to keep though: the idea that the package
manager is an integral tool for the full development workflow that should
support its user the entire way.

When I review the new design, I’ll be looking for machinery that `zeps` lacked:
a real model for package identity and dependency resolution, a reproducible
description of the selected graph, and an answer to what’s trusted at every
point between fetching a package and running the code. It needs to treat
installation as a security boundary, and the developer as a consenting,
informed adult worthy of support.

## Fin

I like looking at old code of mine, even when it’s terrible. I always see how
I’ve grown, how I’ve developed and matured, and how I’ve stayed the same person
with the same terrible taste and ideas.

I don’t think old code, no matter how bad, should ever really be embarrassing.
Unless you’re causing your colleagues on-call to rip their hair out at 3am.
Then, blameless culture or not, maybe a pizza on you is the right thing to do.
