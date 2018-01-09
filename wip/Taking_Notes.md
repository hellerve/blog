I recently came across a [blog post](https://peterlyons.com/leveling-up)
by fellow Recurser [Peter Lyons](https://peterlyons.com). The whole blog post
resonated with me, but, the section about [taking
notes](https://peterlyons.com/leveling-up#your-work-journal) in particular made
such a big impression that I decided to implement it right away. I wrote a [tiny
utility](https://github.com/hellerve/notes) for note-taking on the CLI, and
I’ve been using it for the last few days.

Let me tell you a little bit about it. I won’t talk about my motivations for
starting to take notes, because Peter said it better than I possibly could, but
I will share the observations and insights I’ve gathered while writing the tool
and using it for the first few days.

I mostly work in my terminal, e-mail client, and browser<sup><a href="#1">1</a></sup>.
Most ideas, flashes of inspirations, and frustrations happen in the
terminal. That’s why I definitely need a tool that I can call on the command
line, quickly, and that lets me quickly scribble up a note. Latency should be
non-existent, and I should be able to compose both quick notes and longer, more
complex ideas. That’s why I settled on a simple command line interface that is
mode-driven and intuitive.

I think having a low-friction interface is key to developing a habit like this,
at least for me. I resorted to writing my own tool, because, as I detailed
above, I yearned for a tool that is intuitive. In this case, this means
intuitive _for me_, and, because the tool is so simple, I felt like I could
comfortably write the one I needed in one sitting.

Quick notes should be the easiest to get to, because it’s likely that I compose
them in a hurry. As such, any list of arguments whose first element is not a
known command is interpreted as a note. That enables me to just punch in the
name of the command and then the note without any delay.

Composing more elaborate notes should drop me into an editor, much like
composing a long commit message does. For this, I decided to honor the
`$EDITOR` path variable. Not because I need it per se, but because it is good
practice and I consider it standard behavior. I refuse to adhere to
[YAGNI](https://en.wikipedia.org/wiki/You_aren%27t_gonna_need_it) when I’m not
at work.

Searching and dumping the logs will probably happen less frequently, and so I
spent less time on those features. Because the logs are stored in a regular
file, I can use any text wrangling tool that my shell and Operating System
provide, and that should often be more than enough. Of course I included the
features anyway, for completeness’s sake and because I was on a roll.

All in all, my tool still suffers from feature envy when compared to Peter’s
tooling, but for now it's more about actually taking notes and developing a habit
than having the perfect system. Like most of my private tools,
including a lot of the scripts powering this blog, it is a work in progress.

Why did I tell you all of this? Reading this blog post again, it pretty much
feels like a braindump, a note about notes. Maybe I felt the urge to write
something that’s less refined than my usual output. Maybe taking notes whetted
my appetite for quicker, more instantaneous modes of written communication.
Or maybe I’m just very excited about having another productivity tool in my
belt.

Whatever it is, I thoroughly enjoy it.

#### Footnotes
<span id="1">1.</span>  Lately [Rambox](http://rambox.pro/) has also been in my
list of open applications, because it neatly separates the communication tools
from the rest of the world and reduces the amount of tabs I have open, but
that’s an implementation detail.
