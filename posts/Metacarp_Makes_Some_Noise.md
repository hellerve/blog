---
title: "Metacarp Makes Some Noise"
date: 2026-10-22
---

I built an audio plugin with [Metacarp](https://blog.veitheller.de/Metacarp.html).

It’s a (deliberately) small [CLAP](https://github.com/free-audio/clap) effect
with a text editor, a Compile button, and a status label. The editor contains
one Carp function:

```
(sig process (Fn [Float] Float))
(defn process [x]
  x)
```

Audio enters, the function returns it unchanged, and everyone has successfully
wasted a small amount of electricity.

Change the body like so:

```
(Float.* x 0.5f)
```

press `Compile`, and the plugin becomes a gain effect. The new function is
compiled to native code and swapped in while the audio keeps playing.

This is an unsupported experiment. It has one text box and no sensible audio
features. But it’s also a metacarp compiler session living inside a digital
audio workstation and replacing a function in real time, and I think that’s
enough reason to show it off.

![Fig. 1: A plugin.](/assets/metacarp_audio.png)

Let’s make some noise!

## The plugin

The plugin has two main pieces.

A small Objective-C shell implements the CLAP ABI and a plain Cocoa interface.
It receives audio buffers from the host, owns the editor and the button, and
holds the address of the active Carp function. It’s also completely
LLM-generated, because learning Cocoa didn’t seem like it would spark joy.

The engine is Carp. It owns a warm metacarp compiler session and a persistent
LLVM JIT. When the editor submits new source, the engine treats it as a
transient cell, runs it through the compiler, and asks the JIT for the address
of `process`.

The whole trip looks roughly like this:

```
Carp source
  -> warm Metacarp session
  -> what carp do (macros, inference, ownership, etc.)
  -> LLVM lowering
  -> persistent ORC JIT
  -> float (*)(float)
  -> audio callback
```
<div class="figure-label">Fig. 2: A pipeline.</div>

The only user-facing ABI I committed to is `(Fn [Float] Float)`. One sample
goes in and one sample comes out. It’s not a particularly good interface for an
audio effect I’m told, but it’s hard to misunderstand (mostly because it’s
small) and it invites experimentation.

It also means the first natural metacarp audio plugin is a bad volume knob.
We’ll fix that in a bit.

## The switcheroo

Compilation happens on a worker thread. When it succeeds, the plugin publishes
the new function address through an atomic. When it fails, the address doesn’t
change and the old function keeps running.

The audio thread loads the pointer at the beginning of a block and calls it for
every sample:

```c
fn = atomic_load(&active_process);

for (frame = 0; frame < frames; frame++) output[frame] = fn(input[frame]);
```
<div class="figure-label">Fig. 3: An effect.</div>

The shell doesn’t do anything in the audio callback but load a pointer and
make a very large number of small calls, which makes it predictable.

That little `fn` up there is deceiving, though, because it ain’t as simple as
it pretends to be.

The callback might have loaded the old address just before the worker
publishes its replacement. Unloading that JIT generation right away would
leave the audio thread executing potentially garbage memory. That’s bad for
music and several other things (though very fun if you want to ROP gadget
your way to success).

The plugin doesn’t quite bury its head in the sand, but it’s also not
particularly clever. Successful generations stay resident until the plugin
instance is destroyed. Every callback that already holds an old address can
finish safely. The price is that pressing Compile slowly fills up executable
memory.

A less lazy implementation would retire generations by epoch. Publish a new
generation, wait until no callback can still be running the old one, then free
it not on the audio thread. This is entirely achievable (a phrase I like to
use for work I haven’t done and have no ambitions of doing).

## The warm compiler

This whole thing only feels immediate because the compiler is meant to work
with it. I’ve written about [the session and the
JIT](https://blog.veitheller.de/Metacarp.html) before, but here’s a short
refresher, because it’s cool and testing it was the main point of this
challenge.

Metacarp loads and infers Core once, and the session, the LLVM context, and
the JIT all persist between edits. A new version of `process` is compiled
against that existing world and published under a fresh native symbol. The
plugin does minimal work for every compiled function, even if that function
happens to use a large amount of standard library code.

On my machine, the bundle smoke test (also vibecoded) measured:

```
cold identity compilation       806.2 ms
warm gain replacement            63.3 ms
scalar JIT call                   3.49 ns
```
<div class="figure-label">Fig. 4: Some random numbers.</div>

The numbers don’t really matter here, but they’re far enough apart to make a
coherent story I think. The cold compile is noticeable. The warm compile feels
immediate enough for a button, and the indirect call per sample is cheap enough
that it isn’t the silliest part of the design after all.

The same smoke test loads the actual plugin bundle without a DAW, constructs
and activates it, sends audio through the identity function, and destroys it
again. It turns out you can test a surprising amount of DAW code if you do it
smartly! I had no idea, which is why I didn’t do it, the genie did.

## Some actual noise

Playing with volume is not all that exciting, but even a stateless function on
individual samples can do more than scale.

It can bend, for instance.

```
(sig process (Fn [Float] Float))
(defn process [x]
  (* 0.2 (Float.tanh (Float.* x 32.0f))))
```
<div class="figure-label">Fig. 5: A more interesting effect.</div>

This drives the signal hard and squashes it back into the range `[-1, 1]`, then
I make it even smaller to pull the mixer out of the red. Quiet samples get
louder, loud ones flatten out. As such, it’s a soft clipper, which is
distortion in a nutshell.

Change the constants and press Compile again, and the drive changes live.
That’s the first thing I made the plugin do that was actually fun to play with.

## Prompts for the curious

Much here is left intentionally blank. I will probably not continue development
on this, but I’d be interested to hear what other people come up with.

### Stateful thoughts

The current function is pure and scalar. A useful effect needs parameters and
usually some state: filters, delay lines, envelopes, oscillators, or at least
something more interesting than a fixed curve.

State makes live replacement more fun. A new implementation might have a
different state layout. The plugin then has to decide whether to throw away
the old state, reinterpret it and pray, or run an explicit migration before
the new function becomes visible to the audio thread.

I’ll leave that, too, as an exercise for the reader.

### The sandbox

The plugin compiles code from a text box into the process of your digital audio
workstation. There’s no sandbox.

Carp code can crash the host, access files, open sockets, corrupt memory, or
take their sweet time in the audio callback. The shell avoids a lot of realtime
sins, but it can’t prove that the function you give it is well-behaved.

How would an effective sandbox for VST code look like?

### Platforms

Only the macOS interface exists. Instead of Cocoa, could it run on, say, Qt?

### Bug roundup

I also know at least of some bugs:

- Plugin state isn’t saved, so the source buffer disappears with the instance.
- Successful native generations stay in memory indefinitely.
- Compiler work is serialized across plugin instances because some metacarp
  caches are process-global.
- The ABI only accepts `(Fn [Float] Float)`, and both stereo channels go through
  the same function.

There’s probably more, too, I just don’t know about it yet.

## Fin

The plugin turns a float into another. It does so by keeping a compiler and an
LLVM JIT inside the host, compiling Carp on demand, and atomically replacing
native code while the audio thread keeps running.

That’s cool, magical, weird, wonderful, and extremely buggy.
