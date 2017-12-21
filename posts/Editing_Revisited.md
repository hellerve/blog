In [a previous post](//blog.veitheller.de/Braindead_Editing.html) I talked
about [an editor](https://github.com/hellerve/e) I built. It's become my
primary editor now, but over the months I've become increasingly frustrated by
its rigidity. This week I finally decided to do something about it, and added
full Lua scripting abilities to `e`.

Adding a scripting language was the obvious choice. It both shifts the responsibility
to the user—if you want a feature, implement it yourself—and allows for a
simple way to persist settings and customizations, in the form of a resource
file.

In this post I'll both walk you through the process of how I set up Lua in
my editor and how the API works. All of the code currently lives in a feature
branch, and I already opened a PR, to “review” and merge it sometime
this week—reviewing your own code is a tad silly, but oh well.

## Interop doesn't have to be hard

Lua has an astoundingly good interoperability story with C. It works rather
seamlessly as even I, as someone who's never really programmed Lua, was able to
get this up and running in just over a day, writing less than 150 lines of glue
code.

The weight that Lua incurs on the editor is considerably greater than I'd like,
but still laughable if we talk about real sizes. The binary is around 44
Kilobytes if compiled without and 256 Kilobytes if compiled with Lua
support. This means that we increased the binary size by roughly 480%, or, in
real terms, 212 Kilobytes. So, while this is a significant investment, it
is also rather negligible if you consider the size of any comparable editor
today<a href="#1"><sup>1</sup></a>. And this doesn't even take into account
that I haven't done any optimizations for binary size, which makes this whole
calulation rather pointless to begin with.

I decided to vendor the latest version of Lua. While this is not the prettiest
solution for a variety of reasons, it ensures that I can keep the build
mechanisms simple to reason about. I'll probably have to change that in the
future.

More importantly, [Lua's C API](https://www.lua.org/manual/5.3/manual.html#4)
is refreshingly simple and pretty. Within about an hour of work, I was able
to type expressions into my editor's prompt and have them evaluated by a
Lua virtual machine that lives inside my editor. Unlike the rest of my editor
I use global state to represent the Lua virtual machine, for reasons of
simplicity. I'm not sure how to resolve this, but I have a few ideas that I
might try out before merging the PR.

I set up the global Lua virtual machine when it's used for the first time,
at which point I register all of the needed C variables and functions. The
C function I register can be called from within Lua to change the editor
state. Those functions do all kinds of things that we need for scripting an
editor, like getting the window size, moving around, getting and setting
variables, inserting and deleting text, and so on.

All of this is possible by placing the current editor context on the Lua stack
as an opaque value that is not affected by garbage collection. This is
important because otherwise the virtual machine might free memory from under
our feet; but we need to have the current editor context available in the
C callback functions that are called by Lua. This might seem a bit odd at
first, but I assure you that it is programmatically sound—or at least does a good job pretending
to be.

Most of the code pertaining to the Lua virtual machine can be found [at this
location](https://github.com/hellerve/e/blob/lua/src/editor.c#L1231) in the
editor. It's fewer than 250 lines of mostly simple C code that exposes
various editor functions in a Lua-friendly format. Go check it out if you're
intrigued. As always, you'll find both the documentation lacking and not a
single comment in the source, but this shouldn't surprise anyone who's ever
worked with me. Where's the fun in reading well-explained code, anyway?

## An API of sorts

The first step in building Lua integration into my editor was registering the
`l` key to open a prompt where you can type Lua code and have it be evaluated
when you press enter. I've also added support for a resource file—`.erc` in
the user's home directory by default, tweakable at compile time—, and added a
small library to interact with the editor and register custom commands.

There's just a handful of functions and variables to work with, but they are
in fact enough to add useful and interesting features to the editor. I might
add more in the future, if anyone has good arguments for adding to the list.
So, without further ado, here's a complete listing of the API:

```
-- print something in the status line
message("string")

-- insert text at the current cursor position
insert("string")
-- insert text at the current position (appends a newline)
insertn("string")
-- delete a number of characters at the current position
delete(number)
-- move to a given cursor position
move(number, number)
-- open another file, closing the current file
open("string")
-- prompt the user for input
string = prompt("input: %s")

-- get the current cursor position
number, number = get_coords()
-- get the window size
number, number = get_bounding_rect()
-- get the editor text
string = get_text()
-- get tab width
number = get_tab()
-- set tab width
number = set_tab()
-- get filename
string = get_filename()

-- a table containing custom edit keys
keys = {}
-- a table containing custom meta commands
meta_commands = {}
```
<div class="figure-label">Fig. 1: `e`'s Lua API.</div>

The `keys` and `meta_commands` variables might not be immediately obvious, so
let me give you an example for both of them. Suppose you want to register a
custom command in meta mode—a mode accessible by typing `:` and behaving
similar to that mode in Vim—, called `hi`, you just add a function containing
the actions you want to execute to `meta_commands`, like so:

```
meta_commands["hi"] = function()
  insert("hi")
  message("hi inserted")
end
```
<div class="figure-label">Fig. 2: A silly meta command.</div>

The next time you type `:hi`, `hi` will be inserted at the current cursor
position and your status bar will helpfully tell you what just happened.
This system is simple, yet tremendously powerful.

The `keys` variable works similarly. If you want to register a custom function
that will be run whenever a given key is pressed in initial mode—normal mode
for Vim users—, you just register it in the `keys` dictionary.

I've certainly seen fancier editor integrations before, but so far this seems
to do the job just fine. And, considering I'm the only user of this editor, I
feel like I have the right to only implement what I need in order to
be productive.

## This is fun!

Implementing this feature has been incredibly gratifying. It went quicker than
expected, and there is something oddly satisfying about typing Lua code in my
editor and have it reply back to me, even if I've done little work in order to
achieve this.

I'll definitely keep working on my editor; if you enjoyed this post, try it out
and tell me what you think!

##### Footnotes
<span id="1">1.</span> Vim is around 1 Megabyte, while Emacs weighs an
astounding 2.5 Megabyte. And that's just the binary.
