I've been asked to add syntax highlighting to this blog by friends and peers a few
times now. I evidently didn't listen to them, though, and that's not because I
don't value their contribution. I thought about syntax highlighting in my blog posts
for some time and then decided it was doing more harm than good in that particular
context. Let me explain.

## Why syntax highlighting is good

Syntax highlighting has obvious advantages. It reduces the cognitive load of the
programmer by preparsing the source and annotating the source with colours.
Color-coding individual parts of the source is helpful for skimming and scanning
source code, as it provides a crutch for the imperfections of perception.

Syntax highlighting is especially helpful for beginners. If you don't know what
some of the tokens mean you can infer it from the color the highlighting engine
marks them in, which might help diving into the unfamiliar territory of a new
programming language. I found an interesting study conducted by the Psychology
of Programming Interest Group while thinking about syntax highlighting that
you can read [here](http://www.ppig.org/sites/default/files/2015-PPIG-26th-Sarkar.pdf).
You might say that a sample size of ten is rather small, but it's the best study
I've found and it seems methodologically okay, so I'll settle. A more in-depth
opinion on this article can be found in my [reading list](https://github.com/hellerve/ptolemy).

Another point I want to mention is that syntax highlighting makes it easier
to spot some easy-to-miss errors like misplaced parentheses, brackets, or quotes.
It can be hard to make sense of a series of nested parentheses—think Lisp—and
highlighting opening and closing parentheses on hover is one of my favorite visual
cues in Vim.

Personally, I use syntax highlighting whenever I code. [IPython](http://ipython.org/),
my favorite Python shell, even has live highlighting in the REPL—its features are
generally incredibly handy and I would suggest every professional Python programmer
use it. But reading code as motivating examples or illustrations in a blog post
is very different from how one usually operates with code, and the needs I have
here are different from the needs I have in my terminal.

## Why syntax highlighting is bad
