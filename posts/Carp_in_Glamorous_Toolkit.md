---
title: "Carp in Glamorous Toolkit"
date: 2026-09-26
---

I started [gt4carp](https://github.com/carpentry-org/gt4carp) in 2020. It is an
IDE for Carp in [Glamorous Toolkit](https://gtoolkit.com), or at least it has
been threatening to become one for about six years.

The first version had a parser, syntax highlighting, a custom Lepiter snippet,
and the beginnings of a project browser. I gave a talk about it in 2022 and
[posted the recording](https://blog.veitheller.de/Implementing_an_IDE_in_GT.html)
the following year. The repository was labelled “heavy WIP” for, I believe, its
entire existance, even though there was no progress on the work for a few years.
The booklet that explains how I built the custom language integration had some
interested readers, though.

This summer I returned to it. Carp now has
[Metacarp](https://github.com/cyberwitchery/metacarp), whose compiler phases
are libraries and whose compiler sessions can stay alive between inputs. That
was the missing piece to execute on some of my vision for Carp. In many ways, I
had already talked about this [at CurryOn in
2018](https://www.youtube.com/watch?v=Q1BVfGIhwZI)<sup><a href="#1">1</a></sup>.
Carp is a language that provides so much, and a good development environment
should use all of that information.

And through Metacarp, I got a little closer to that vision. gt4carp can now
run a warm compiler per notebook page or project, annotate and complete code,
keep values alive for inspection, edit definitions through a project browser,
and debug the native program in the Carp source it came from. It also uses a
whole lot of information live, from docs to expansion to lifetime boundaries.

It remains heavy WIP, naturally, as everything I work on always is. But it’s a
much more entertaining kind of heavy WIP now, because there is actual progress
to be had. Let’s have a look!

## The browser

The project in this screenshot is Metacarp itself. We’re looking at its
parser, and there’s a small example open called `Surface.a-parsed-module`.

[![Browsing a parser example, evaluating it in the project playground, and inspecting the resulting surface forms.](/assets/gt4carp/browser.png)](/assets/gt4carp/browser.png)

The example parses a little bit of Carp:

```
(deftype Point [x Int y Int])
(defn origin [] (Point.init 0 0))
```

and returns a `SurfaceModule`, which is what the parser hands to the next
compiler phase. In the playground below the definition, we can call it:

```
(Surface.a-parsed-module)
```

The playground uses the project’s compiler session, so everything the project
defines is in scope. That includes its parser, its types, and the example.
The inferred return type appears next to the expression, and hovering over the
function shows its documentation.

Evaluating and inspecting it opens the result in the next column. The
`SurfaceModule` has a Forms view with two rows: the type definition and the
function definition we just parsed. Each row can be opened as a surface node
in another column. We can look at what the parser produced while its source
is still right there, because the values decide how they want to be presented,
GT style.

I think this is quite fun! The compiler phases being libraries means that
calling the parser is a regular function call, and the IDE lets us work
with live values. The compiler is a particularly entertaining project to browse
this way, but the playground works on other, less meta-horrific projects too.

I made the decision that a Carp project is a Git repository. You point `gt4carp`
at a repo and it asks the compiler to load it the way that a build would. The
browser then shows its modules and definitions, along with declared signatures,
documentation, visibility, implemented interfaces, examples, all without having
to fully compile it.

Opening a definition shows its source and documentation. Editing it replaces
the definition through the byte span the compiler recorded, writes the file,
and commits the new definition into the warm project session. So we still have
files under the hood, we just never look at them, because who cares? I want to
see the functions, their documentation, and their examples, not the ceremony
around them.

The green example label in the screenshot comes from a little bit of metadata
on an ordinary function:

```
(meta-set! a-parsed-module "example" true)
```

The browser figures out what an example belongs to from the types involved
both return types and body). The one above is an example of `SurfaceModule`
because it returns one. It’s also an example of the functions it calls,
including `Surface.parse`. I don’t have to maintain another hierarchy, which is
good because I would forget, and I frequently did when working on GT (sorry,
Tudor!).

Types become particularly fun to browse. Their view includes the functions
which operate on them, the implementations they participate in, their derived
members, and examples which produce them. The compiler has already established
these relationships. `gt4carp` makes the liminal space of “not in my program,
but in the compiler’s view of it” visible and browsable. In that way, it is
exactly what I talked about in 2018.

## The values

The `SurfaceModule` in that right-hand column is still in a running Carp
program. This deserves some explanation, I think.

Metacarp compiles the cell into a native program. That program evaluates the
expression, registers the result, and then does something slightly strange for
a compiled language. It stays alive.

The result becomes a remote object in GT. It has a type, a printed form, and a
set of views. Opening one asks the still-running Carp program for its contents.
Selecting an item can return another live Carp value. Expanding a tree asks the
program for that value’s children.

In other words, the inspector is looking at an object in another native
process, and that process participates in the inspection. The Forms view
above is supplied by Carp code which knows how to look inside a
`SurfaceModule`. Very normal stuff. Not terrifying at all.

Views in Carp are written using `defview`. A string, for instance, has a text
view:

```
(defview string-value [s String]
  (=> (NB.text)
      (NB.title @"String")
      (NB.content @s)))
```

and another view which presents its bytes as a table. Arrays, maps, sets,
bags, boxes, pairs, `Maybe`, and `Result` have structural views. A record gets
a raw field view automatically. Programs can add text, list, tree, columned,
or bitmap views for their own types.

This is probably my favorite part of the whole experience. `gt4carp` doesn’t
flatten every result into a generic JSON value and reconstruct an approximate
object on the GT side, because that would be cheating, and cheating is bad.
Carp code decides how Carp values want to be seen. A view is a function which
still has access to the value it describes.

The implementation is appropriately silly. The cell program starts a tiny
HTTP server and exposes object and view endpoints. GT holds a remote reference
containing the host port and object id. The two processes then politely ask
each other questions until the next evaluation kills the value host. If that
sounds like madness, you’re welcome.

There are lifetime and security implications here, as you might imagine. The
value is native code in a native process, not a lovingly sandboxed data model.
An inspector can also outlive its host if another evaluation replaces it. But
when it works, a Carp value inspects like a Pharo object, and I think that is
somewhat magical.

![A live Carp value with program-defined views in the GT inspector.](/assets/gt4carp/value.png)

Of course, there is also a Pharo side to the object, and that one can be
verwritten and have its own views, too. I have a whole declarative layer on
the Pharo side to decide which class represents which Carp data type. It’s
very flexible, but also well-trodden ground. [Phlow for
Python](https://book.gtoolkit.com/phlow-for-python-dyl98a2rcczf8f4tdf7e1pstj)
also offers these capabilities.

## Snippets

The same machinery works in a notebook (in fact, a playground is just a single
notebook snippet with some context). Carp code gets its own Lepiter snippet,
with highlighting, inferred types inline, diagnostics, completion, and
documentation on hover. Each page gets its own compiler session, so we
can build up a program across snippets.

One snippet can define a function:

```
(defn twice [x] (* x 2))
```

and a later one can use it:

```
(twice 21)
```

Definitions accumulate as snippets run. The second snippet sees the first one
throughout its lifetime. Core (the standard library) was already loaded and
inferred when the session started, so every little expression does not drag
the whole standard library through the compiler again. This way we manage
millisecond compile times for simple snippets, even in huge contexts.

![A gt4carp notebook page with inferred types and diagnostics.](/assets/gt4carp/snippets.png)

A small server written in Carp sits between GT and Metacarp. It keeps one warm
session per page or project and speaks a simple length-prefixed JSON protocol.
GT starts it when needed, sends it source, and gets structured compiler answers
back.

It hands us back the original spans for everything, which required some
machinery, but it means error messages are delightful. Worthy tradeoff.

## The debugger

Carp compiles to native code, so the debugger eventually has to deal with
LLDB. This could mean that pressing Debug drops you into a large C translation
unit full of generated names, if you’re lucky.

It doesn’t, most of the time! (Sometimes it does, but I’m working on it.)

Metacarp’s backends can attach Carp source locations to the generated program.
gt4carp starts `lldb-dap`, sets a breakpoint where the snippet’s program
begins, hides compiler-generated frames and variables, and decodes Carp’s
mangled names before showing them<sup><a href="#2">2</a></sup>.

A breakpoint in a snippet therefore stops in the snippet. Stepping moves
through Carp source. The stack contains Carp-ish names rather than exciting
sequences such as `C4_main__F0_Z1_U`.

This feature rests on an absurd tower of conventions I established together
with the other stakeholders (all me). Metacarp and `gt4carp` must agree about
symbol mangling. Generated C or LLVM must carry the right line map. Relative
source paths have to resolve from the debugger’s working directory. GT counts
characters while the compiler reports UTF-8 bytes, so I have to translate. I
get one wrong and the debugger stops in `_dyld_start` and stares blankly at me.

Native debugging is hard, y’all.

![A Carp program stopped in its source in the native debugger.](/assets/gt4carp/debugger.png)

Luckily there’s also a macro-level debugger that’s been there since 2022 (I’ve
since completely rewritten it), and it’s much more straightforward. Also much
more boring, because it just works, and who cares about that.

## Here be bugs

gt4carp is usable, but it’s absolutely not finished. Far from it, if it’s to
live up to my own goals one day.

The server is a binary compiled with Metacarp and linked against the notebook
runtime. I don’t yet have a nice way to distribute it with the GT package, so
the setup currently assumes the sort of machine on which I develop Carp. That’s
already a fairly specific audience, and I have no reason to believe anyone else
has ever gotten it working.

Cells also do not have identities yet. The server sees source which defines
`foo`, then source which defines `bar`, but cannot tell whether the latter is a
new snippet or the former snippet after a rename. Old definitions can remain as
ghosts. Reopening a module across cells runs into the same problem. The
semantics are, quite frankly, a bit undercooked as of yet.

Warm state can degrade when the server cannot mirror a definition or load. It
then takes slower compiler paths so that evaluation, annotation, and completion
continue to agree. This is correct and occasionally mysterious from the outside,
because the slowdown is intrasparent and somewhat jarring. GT currently handles
the degradation indicator by not showing it to you and just freezing, which is
lovely, but not quite what I envision.

The project browser trusts source spans recorded when a file was loaded. An
external edit at exactly the wrong time can make them stale. A remote inspector
can keep references after its value host has died. The native debugger knows more
about Metacarp’s symbol names than it should have to. It’s all a bit too loose
for my comfort.

In short, there are several opportunities for excitement, and I am excited to
continue work on what I consider part of my magnum opus thus far (only very
slightly too grandiose).

## Fin

The parser example is small, but it’s an encapsulation of what I wanted to
achieve for a long time. I can open a function, call it with the project
context, learn about the compiler’s model of it, and look inside the value it
returns. If the value needs a better view, that view can be written in Carp or
Pharo and just extend the environment on the fly.

There is plenty left to make less fiddly. But I think being able to move
between the source and the objects it produces makes the language much more
pleasant to explore. It certainly makes a compiler a more interesting thing
to have open on my screen.

I’ve wanted something like this for a long time. The repository is [on
GitHub](https://github.com/carpentry-org/gt4carp), still calls itself
heavy WIP, and is finally becoming weird enough to be dear to me.

#### Footnotes

<span id="1">1.</span> Back then, my friend [David Schmudde](https://schmud.de/)
was in the audience and asked “so, what of what you talk about is there right
now?”, to which I had to reply nothing. Well, David, it took me 8 years, but
we’ve made it, baby!

<span id="2">2.</span> There’s a weird flicker sometimes where we start in C
before switching to Carp that I haven’t quite figure out. I’m sure I’ll get
there.
