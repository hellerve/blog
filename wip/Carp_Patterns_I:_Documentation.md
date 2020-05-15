I’ve had the opportunity to spend a lot of quality time with Carp lately, and
it has been very fun! The community has been buzzing with activity, and a lot
of good exciting things have landed in the core compiler and standard library.

With all of that in mind, I’d like to start [a little series](carp-patterns)
about design patterns and current best practices in Carp that I’ve been using
for my own projects. They are definitely not set in stone and things are in
flux quite a bit at all times, but I’ve found that a few things just work for
me, and I want to share those!

To kick things off I want to talk about documentation, a topic I have very
strong feelings about: I love to write, and I love to build APIs, and making
a cohesive document that explains how things fit together and why the design
as it is excites me.

## Starting off

Let’s pretend we are starting a new project. I usually try to always use the
same template for my projects, and the current one happens to be [a GitHub
template on the carpentry organization](https://github.com/carpentry-org/template).
It already comes with a documentation generation script, so I don’t have to do
anything. I just fill in the modules I want to document and we’re off to the
races!

The first decision to make is whether one module will be enough, or whether
breaking your library up into multiple modules is a good idea. I personally
have two different ways of documenting my APIs, and this is the deciding
factor.

If it’s only one module, as with [my path package](https://veitheller.de/path/),
I like to have the documentation on a single page. All of the relevant
functions are one page, and they might refer to each other, as is the case with
[`absolute?`](https://veitheller.de/path/#absolute?) and
[`relative?`](https://veitheller.de/path/#relative?).

This sort of configuration can be achieved by setting the following in
`gendocs.carp`:

```
(Project.config "docs-generate-index" false)
```
<div class="figure-label">Fig. 1: Let’s set some flags!</div>

You will then just end up with a file named after your module. I usually rename
it to `index.html` and put it in a subdirectory when I push it to the server,
to get nice pretty URLs.

If, on the other hand, my project consists of multiple modules, I like to
generate an index and put some general information about the goal of the
library on the index page. This can be observed in [my wrapper around
libhydrogen](https://veitheller.de/hydrogen/). There we have a nice short high
level description of the goals of the library, a little usage example, and a
list of links to the right that will let us inspect all the modules.

This can be achieved by not setting the configuration flag we looked at in
Figure 1, and instead setting the `docs-prelude` switch. Here’s what that looks
like:

```
(Project.config
  "docs-prelude"
  "This is my **awesome** package."
)
```
<div class="figure-label">Fig. 2: It sure is!</div>

All in all, your documentation script will probably only be a few lines long,
and mostly consist of documentation itself.

Now that we have a documentation script, how do we actually properly document
anything?

## Documenting your library

Writing documentation is not easy, but I can report that at least Carp rarely
gets in your way. By default the output of the Carp documentation compiler is
the name of the function as an anchor—the ID is the name of the function as
well—, the type signature, and the names of the arguments. You can optionally
provide documentation for your function as prose, and if you publish your
package, this is highly encouraged. The canonical way to document your
functions is using the `doc` command.

```
(doc my-function "adds two numbers, `a` and `b`.")
(defn my-function [a b] (+ a b))
```
<div class="figure-label">Fig. 3: Documenting a trivial function.</div>

`doc` takes the name of the function to document, and a string (also referred
to as “docstring”). That docstring will also be added when rendering your
documentation, and if you ask for information about a function in the repo
using the `info` function or the `:i` shorthand, the documentation will also
be displayed there. You can write markdown in your docstring, and it will be
converted to HTML when the documentation is generated!

Docstrings also work for modules.

```
(doc MyModule "is a module for containing all of my awesome functions!")
(defmodule MyModule
  (doc my-function "adds two numbers, `a` and `b`.")
  (defn my-function [a b] (+ a b))
)
```
<div class="figure-label">Fig. 4: Documenting a trivial module.</div>

The module docstring will be rendered at the top of the file, between the
module name and the functions. I often use it as a summary or quickstart
section!

Sometimes you don’t want to expose a function across module boundaries. It is
internal, and documenting it would be a pain. If that is the case, you should
mark it as `hidden`—and possibly as `private`, to enforce that rule.

```
(defmodule MyModule
  ; enforces no cross-module use
  (private my-private-fn)
  ; hides the binding from the outside
  (hidden my-private-fn)
  (defn my-private-fn []
    (println* "I do secret stuff"))
)
```
<div class="figure-label">Fig. 5: Marking an internal function.</div>

And that’s all!

### How does `doc` work, anyway?

*This is a bit of an technical aside, feel free to skip it.*

Carp has a very powerful meta system. Every symbol has a hashmap associated
with it from which every macro and dynamic function—and the compiler!—can read,
and to which they can write arbitrary string keys with arbitrary data. This is
how we do all annotations in Carp, including the ones that the compiler itself
understands: `doc`, `sig`, `private`, and `hidden`. Yes, we even do type
signatures this way!

In fact, the `doc` command is just a macro:

```
(defmacro doc [name string]
  (eval (list 'meta-set! name "doc" string)))
```
<div class="figure-label">Fig. 6: The `doc` macro.</div>

This is simple and extensible enough to enable a variety of purposes. One of my
favorite properties that doesn’t really do anything is `todo`.

```
(todo my-fn "write documentation!")
(defn my-fn []
  (let-do [a 1.0]
    (while true
      (let [b (inc (/ 1.0 a))]
        (if (= a b)
          (break)
          (set! a b))))
    a))
```
<div class="figure-label">Fig. 7: Can you guess what this does?</div>

## Fin

Documentation in Carp is one of my favorite parts of writing a new library. I
get to be creative and write, and I get to arrange my documentation in a format
that makes me proud. It is also usually a pretty fast process, which is
important to me—I maintain a lot of packages, and they change semi-regularly
due to updates in Carp. Having a lightweight process that works for all of my
projects is important to me, and the one I have in Carp is fun as well.

I hope you’ll have fun with it as well! See you next time!
