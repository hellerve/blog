A few days ago, [we landed pattern matching in Carp](https://github.com/carp-lang/Carp/pull/192).
This was borne of a long-standing desire to [wrap a regular expression library
for the standard library](https://github.com/carp-lang/Carp/issues/118). For a
long-time we weren’t sure which library to use, until late in February I looked
into what Lua and Python use. Python’s implementation is unsurprisingly large
and fairly complex. In fact, the matching function is about as large as Lua’s
entire engine.

So I opted for Lua, and, within a few days, had a working version with a clean
API that we could merge into Carp proper. Let me tell you about it!

## I love Lua

Before we get into the beauty that is Lua pattern matching, let me make a
confession: I’ve never really programmed Lua. It is the scripting language in
[my editor of choice](https://blog.veitheller.de/Editing_Revisited.html) and
I’ve written a fair bit for my own configuration, but beyond that I’ve not
really worked with it.

I do have an appreciation for it, though, and it comes purely from me dabbling
in the implementation of programming languages: Lua is lightweight, has a small
codebase, and is fairly simple to optimize. What’s not to like?

Its implementation of pattern matching is no exception. It’s hardly 500 lines
of code long, and although it’s not a true regular expression engine, in many
ways I would argue that it is superior.

I was able to read through the code, understand it, and adapt it to Carp in
less than an evening. That alone should be testament to just how readable—and
maintainable—the code is. If you have time and are intrigued [I suggest you
read through it](https://github.com/lua/lua/blob/master/lstrlib.c#L349).

## I love remixing

All I had to do was remix the code<sup><a href="#1">1</a></sup>. But just how
did I do it? I would like to answer that question in a backwards manner, and
start by giving you the resulting API. Then I will give you a little bit of
insight into where we are, and a quick overview of the nasty bits.

```
; finds the index of a regex inside another string
find : (Fn [&String &String] Int)

; returns the match groups in a match
match : (Fn [&String &String] (Array String))

; returns the match groups of all matches
global-match : (Fn [&String &String] (Array (Array String)))

; substitutes a pattern n times. Passing -1 will result
; in substitution of every occurrence of the pattern.
substitute : (Fn [&String &String &String Int] String)

; will check whether there are any valid matches of
; the regex in the string
matches? : (Fn [&String &String] Bool)

; will return the matched string
match-str : (Fn [&String &String] String)
```
<div class="figure-label">Fig. 1: The API, as defined in the initial PR.</div>



#### Footnote

<span id="1">1.</span> I even asked for permission and gave it proper
                       attribution! The Lua mailing list rocks!
