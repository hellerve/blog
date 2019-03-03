*TL;DR because the introduction is a bit long: We’re going to play around with
Python’s metaprogramming facilities to build a plug-in system for Python’s
parser!*

In the summer of 2016, I flew to Bilbao to attend EuroPython. I was [still
working](/zepto,_A_Eulogy.html) on [zepto](https://github.com/zepto-lang/zepto),
and used some of my downtime to mull over some of the concepts that I wanted to
put in the language, or had already implemented. I told a lot of people about
it, too, and many seemed very interested in it, if only for all the ideas that
I had.

Naturally, I eventually started to think about how I could get some of the
concepts into Python in the hopes that people would then have an easier time
understanding what they meant. Most of the features seemed impossible to graft
onto the language without touching the interpreter itself, which would make it
both tedious to implement and for the curious to build.

Then I thought of one of my favorite talks at the time, in which [David Beazley
talks about metaprogramming in Python for three hours](https://www.youtube.com/watch?v=sPiWg5jSoZI).
Among other things he builds a loader and importer for XML files that follow
a certain schema, like a declarative way to define classes. While this is
certainly at least a little silly, in my mind I connected that to a feature that
zepto “borrowed” from Carp: the `#lang` reader shorthand.

This concept blew my mind when I first encountered it: by writing `#lang <name>`
at the top of any Racket file, you tell Racket to first pass the contents of
that file to a transformer that says it can read in that source code and
transform it into regular Racket<sup><a href="#1">1</a></sup>.

This might all sound kind of weird and technical, but what this is saying is
that you can plug in your own parser before Racket’s!

That was super cool to me, and I added a simplified version in zepto. Because
the [procedure that loads source files](https://github.com/zepto-lang/zepto/blob/master/zepto/load.zp)
is written in zepto itself, I was able to just add a quick check to the loader
that sees whether the source file starts with a `#lang` directive and whether
any function is registered that promises to be able to transform that code. It
is then passed onto the function, which is expected to return regular
S-expressions—another lisp-y construct—that we can then expand and evaluate as
usual<sup><a href="#2">2</a></sup>.

So I decided to whip up a version of that for Python and demo it in a lightning
talk. Sadly, all of the spots for talks were already taken, so I shelved the
project and moved on.

A few days ago I realized that this project might actually make an interesting
instructional blog post, however, and here we are. What we’re going to do is
add a few Python metaclasses that will enable us to have a crude version of the
`#lang` directive for Python files!

The source code for this project is [on
Github](https://github.com/hellerve/a_bad_idea/), if you want to follow along.

Just to be clear, I don’t advertise you use this for anything other than your
personal enjoyment. While this concept works well for a language like Racket,
it doesn’t work super well in Python.

## An API

We’ll begin by defining an API. The module will be called `a_bad_idea`, and
anyone who wants to implement a new language will first have to register their
parser function. Let’s see how we would do that for a JSON file, for instance:

```
import json
import a_bad_idea

def json_parser(code, module):
  module.value = json.loads(code)

a_bad_idea.add_implementation("json", json_parser)
```
<div class="figure-label">Fig. 1: A simple JSON loader.</div>

Note that the module that we get back in this case would have one definition in
it named `value`, which is a little weird, but could potentially be convenient.
Imagine having a configuration file in JSON that you want to read in; just
importing it would be so much simpler, right?

We will also check that the file extension matches the name in the directive as
well. This might seem redundant, but while we’re at it and are learning about
the black magic of metaclasses, why not do some extra credit work, right?

## The code

Alright, let’s get to writing code! First of all, let’s think about how we could
make that API possible. I’ll go for the simplest solution: a global dictionary
in our namespace:

```
LANGS = {}

def add_implementation(name, parser):
  LANGS[name] = parser
```
<div class="figure-label">Fig. 2: The code for adding languages.</div>

That was easy! Now we can register parsers, which is great, but they won’t
actually do anything yet. For that, we’ll need to talk about how finding and
loading modules works in Python. The `sys` module has a property called
[`meta_path`](https://docs.python.org/3/library/sys.html#sys.meta_path), which
are essentially module finders. They need to have a class method called either
`find_spec` (since Python 3.4)  or `find_module` (in prior versions), which will
try to find the module that is being imported and then return another class that
actually imports it. But we’re getting ahead of ourselves here! Let’s first
insert our finder into that list:

```
import sys

class Finder:
  @classmethod
  def find_module(cls, fullname, path):
    pass

sys.meta_path.insert(0, Finder())
```
<div class="figure-label">Fig. 3: A skeleton for our finder.</div>

Alright, so as it currently stands our finder will not do anything. We’re
implementing `find_module` because it’s called as a fallback in modern versions
of Python as well, which means that it’s the only function that works across
versions. If you want to make this a little prettier, you could also implement
`find_spec` and just pass the arguments on to `find_module`.

Now we need to make our finder actually do things. It will need to go through
all the directories in the path, find the first module that is elligible, and
return it. If we don’t find anything, Python will resort to the other loaders
in the list.

```
ìmport os

class Finder:
  @classmethod
  def find_module(cls, fullname, path):
    # we need to get through the path
    for dirname in sys.path:
      # get the filename without extension
      basename = os.path.join(dirname, fullname)

      # iterate through our implementations
      for extension in LANG:
        # build the real filename
        fname = '{}.{}'.format(basename, extension)

        # and make sure it’s a file (and not a directory)
        if os.path.exists(fname) and not os.path isdir(fname):
          # check if we have the right directive and
          # return our loot
          lang = _get_lang(fname)
          if lang and lang == extension:
            return Loader(fname, lang)
```
<div class="figure-label">Fig. 4: A real finder.</div>

That’s a bit of a mouthful, but it’s mostly comments, and while the code uses
some lesser known functions of the Python standard library, it should be
somewhat straightforward. `sys.path` is a list of the directories that Python
looks into when you import a module.

There are two things in the code in Figure 4 that we need to implement before
we’re done. Firstly, we need to check for the directive and then we need to
implement a loader. Because the latter is a little more work, let’s start with
the former.

```
def _get_lang(filename):
  # we get the first line
  with open(filename) as f:
    first_line = f.readline()
  # check it for the directive
  if first_line.startswith('#lang '):
    # and get the rest of the line, stripping off the newline
    words = first_line.split(' ')
    return ' '.join(words[1:])[:-1]
```
<div class="figure-label">Fig. 5: Checking for the directive.</div>

As we alluded to earlier, this step is basically redundant, and just there for
show. We’re also not parsing anything yet, operating at a level of files and
strings, which might come as a surprise to you.

Now let’s fiddle with some modules, shall we?

```
import imp

class LangLoader:
  def __init__(self, fname, lang):
    self.fname = fname
    self.lang = lang

  def load_module(self, name):
    # first we check whether we already have that module
    if name in sys.modules:
      # if so, just reuse that
      mod = sys.modules[name]
    else:
      # if not, we’ll create one and add it to the cache
      mod = imp.new_module(name)
      sys.modules[name] = mod

    # set some minimal metadata
    mod.__file__ = self.filename
    mod.__loader__ = self

    # get the parser
    parser = LANGS[self.lang]
    # execute and return the module
    parser(_get_code(self.fname), mod)
    return mod
```
<div class="figure-label">Fig. 6: A custom loader.</div>

Again, this might seem like a lot, but it’s actually quite straightforward. We
use [`sys.modules`](https://docs.python.org/3/library/sys.html#sys.modules) to
store and retrieve imported modules. It’s generally a bad idea to just fiddle
with the internals of Python willy-nilly, but in the module finder and loader
there’s just no way around it, and it’s actually a little bit fun as well.

Getting and executing the parser is a little anti-climactic; we just pass in
the freshly created module and the code and hope for the best. But it makes
sense if you think about it: there’s actually not that much mechanic in a loader
if it doesn’t have to parse anything. It’s just setting the playing field for
our user-supplied parsers and that’s it.

We’re not quite done yet, however. For completeness’ sake, we’ll also have to
implement `_get_code`, which is a very simple function that just reads the file
and strips of the first line, because we don’t want to have the `#lang`
directive in the string we pass to the user.

```
def _get_code(filename):
  with open(filename) as f:
    contents = f.read()
  return "\n".join(contents.split("\n")[1:])
```
<div class="figure-label">Fig. 7: Preparing our source code.</div>

And that’s it! In just over 50 lines of code, we added a plugin-based parser
system to Python! Now all that’s left to do is writing some parsers, but I’ll
leave that as an exercise to my beloved readers.

## Fin

While we weren’t quite able to replicate the power and wonder that you feel when
writing languages using Racket, we were able to build an extension to Python
without ever touching the core of the language, and I think that’s quite
beautiful.

If you want to learn more about writing languages using Racket, I recommend
checking out Matthew Butterick’s fantastic [Beautiful
Racket](https://beautifulracket.com/), in which he guides you through the
process of writing a few little languages in Racket. It’s quite fun, and I had
a blast reimplementing the languages from the book in zepto back in the day!

#### Footnotes

<span id="1">1.</span> This is quite a bit of a simplification, but I didn’t
                       think the actual mechanics actually matter that much. If
                       you want to dive a bit more deeply into how it all comes
                       together technically, I suggest looking at the [Racket
                       documentation](https://docs.racket-lang.org/guide/Module_Syntax.html#%28part._hash-lang%29)!

<span id="2">2.</span> I want to note here that this is much, much simpler than
                       the machinery in Racket, which relies on reader macros.
                       Racket also has tremendously more tooling around the
                       concept, which shouldn’t come as a surprise.
