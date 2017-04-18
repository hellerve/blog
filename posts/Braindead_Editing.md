I've had the urge to do some programming close to the
metal again lately and was looking for a useful project
that I could work on in C. Inspired by antirez's
[kilo](https://github.com/antirez/kilo) I decided to work
on [a terminal editor](https://github.com/hellerve/e) that
looks and feels a lot like Vim, but caters more to my needs.
This meant that I had to make it snappier, provide defaults
that are saner (to me, all of this is completely personal),
and does not provide a scripting language for me to get lost
in.

That last idea might sound like a non-feature, but I don't need
all that many features in my day-to-day editing. I normally
use tmux to run multiple Vim and zsh sessions simultaneously,
which provides me with the ability to edit and work with
Git or issue shell commands without having to resort to fancy
plugins. I strongly believe that making my tool more focused
also enables me to make it better.

`e`—I love short names and mnemonics—is C99- and POSIX-compliant,
has no external dependencies to the best of my knowledge,
is just over 1.4k lines of code long, and the resulting binary is
44 Kilobytes small on my machine. The feature list looks pretty
complete, though, at least from where I'm standing:

- Incremental search (and replace)
- Multiple modi (similar to Vim)
- Mnemonic movement (feels like Vim, just different enough for you
  to be frustrated)
- Limitless Undo (until memory runs out)
- Extensible syntax highlighting
- No global state in the library part (just in `main.c`); in theory,
  one should be able to run multiple editors in one process
- Can be used as a library
- Ships with syntax highlighting for C/C++ (stable) and Markdown (experimental)

Of course there are bugs galore, but it seems to run fine on my
machine–at least well enough to write this essay on it. I haven't
battle-tested it yet, though, so don't take my word for it. I also
have to fix Unicode support, which is one of those moments where
you realize how old C really is. Only Python 2.x is worse (sorry folks).

Anyway, I've been working on it nights and weekends for the last 10 days
or so, and it's been a blast. Now I'm at a point where I'm ready to share
it with the world, even though I'm sure the code quality is abysmal,
because I haven't written any C in years, quite literally.

[![asciicast](https://asciinema.org/a/e164s5tnu3okht44go6uhyju4.png)](https://asciinema.org/a/e164s5tnu3okht44go6uhyju4)
<span class="label">Fig. 1: A screencast of `e` in action (opens in a new tab).</span>

If you have any questions, comments, or suggestions, be sure to open
a Github issue. I'm ready to support y'all if you're interested. And
there is plenty of work that needs to be done if you would like to
work on a project like that. I'm open to Pull Requests of any kind
and am happy to do my part if you need some help getting
started.
