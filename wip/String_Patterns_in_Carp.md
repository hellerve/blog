A few days ago, [we landed string pattern matching in
Carp](https://github.com/carp-lang/Carp/pull/192).
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
start by giving you the resulting API and the syntax we’re using for our
patterns. Then I will briefly address why I constantly say “patterns” instead of
“regular expressions”, and, lastly, give you a little bit of insight into where
we are, and a quick overview of the nasty bits.

### An API

```
; finds the index of a regex inside another string
find : (Fn [&Pattern &String] Int)

; returns the match groups in a match
match : (Fn [&Pattern &String] (Array String))

; returns the match groups of all matches
global-match : (Fn [&Pattern &String] (Array (Array String)))

; substitutes a pattern n times. Passing -1 will result
; in substitution of every occurrence of the pattern.
substitute : (Fn [&Pattern &String &String Int] String)

; will check whether there are any valid matches of
; the regex in the string
matches? : (Fn [&Pattern &String] Bool)

; will return the matched string
match-str : (Fn [&Pattern &String] String)
```
<div class="figure-label">Fig. 1: The API, as defined in the initial PR.</div>

Let’s go through the functions one by one and see what they’re about.

`find` will find the index of a pattern within a string. On failure, it will
return `-1`. Any captures will be dropped.

`match` will return the match groups within the first match as a list. We can
have at most 16 captures right now. This is an arbitrary restriction, but it is
useful to reduce the possibility that a very complex pattern uses up too much
CPU time.

`global-match` works very much like `match`, but it returns the match groups of
all the matches in the string. This means that you’ll get back a nested array,
one per match.

`substitute` takes a pattern, a string in which to substitute the pattern, the
string that the pattern should be substituted with, and the number of times we
should substitute the pattern. If the number is `-1`, we will substitute all
occurrences of the pattern. It always works from the beginning of the string to
the end, meaning that you can’t substitute just the second or third pattern at
the moment.

`matches?` is a simple boolean function to check whether there are any matches
of a pattern in a string. To check whether a string is a perfect match of a
pattern, you could use the anchors `^` and `$`.

Lastly, `match-str` will return the text of the match.

### A DSL

The pattern language is almost identical to Lua’s, which you can read about
[here](https://www.lua.org/manual/5.3/manual.html#6.4.1). The most obvious
difference is that we use `\` as our escape character, and we added `\n`, which
matches all kinds of newline characters, and `\t`, which matches tabulation
characters. We also added pattern literals, which allow us to avoid the dreaded
double escaping you have to perform in certain string-based APIs.

Let’s try and break it down with an example. Imagine a Turtle and a Koala are
running a university, and you are one of their students. Sadly they provide
the curriculum in a format that reads a little clumsily, and you want to
use Carp and its patterns to clean them up. This is the format:

```
Professor Koala: A guide to eating eucalyptus (K 101)
Professor Turtle: Flapping your flippers just right (T 101)
Professor Turtle: Data science with Carp (T 201)
Professor Koala: Learning languages quickly (K 201)
[...]
```
<div class="figure-label">Fig. 2: A perfectly sensible curriculum.</div>

Time to roll up our sleeves! We want to parse all of this into a struct named
`Course` that contains information about the professor, the name of the course,
the name of the major, and the course number.

```
(deftype Course [professor String
                 name      String
                 major     String
                 number    Int])

(deftype Curriculum [courses (Array Course)])
```
<div class="figure-label">Fig. 2: Courses to work with.</div>

Alright, we defined a type for our data. Let’s now get to parsing it!

```
(defn parse [curriculum]

  ; we begin by defining an empty array of courses
  ; and splitting out curriculum into lines
  (let-do [courses []
           lines (String.lines curriculum)]

    ; for each line, we match our pattern (not
    ; yet defined) and add it to our list
    (for [i 0 (count &lines)]
      (let [line (nth &lines i)
            groups (match pat line)
            prof @(nth &groups 0)
            name @(nth &groups 1)
            major @(nth &groups 2)
            num (Int.from-string (nth &groups 3))
            course (Course.init prof
                                name
                                major
                                num)]
        (set! courses (push-back courses course)))
    )

    ; now we wrap it into a curriculum, and we’re
    ; done!
    (Curriculum.init courses)))
```
<div class="figure-label">
  Fig. 3: Matching scaffolding<sup><a href="#2">2</a></sup>.
</div>

This is a big chunk of a program, but it doesn’t do much except pulling the data
out of one representation and pushing it into another, the one we desire. The
crucial part is `(match pat line)`, which does most of that work. But we
haven’t actually defined `pat` yet! Let’s do that very quickly!

```
(def pat #"([^:]+): ([^\(]+) \(([A-Z]+) (\d+)\)")
```
<div class="figure-label">Fig. 4: Look what the cat dragged in!</div>

By Odin’s beard! What is that abomination? It’s a perfectly normal pattern, I
assure you, and one that most regular old [PCRE](https://www.pcre.org/) regex
libraries understand to boot.

Let’s dissect it: firstly, let’s note that pattern literals in Carp use `#""` as
syntax. This makes it easy to differentiate them from regular strings, which
of course aren’t prefixed with a hash symbol.

The first section is a matching group already, as denoted by the parentheses.
This will be the first thing in the array we get back from `match`. It matches
anything that isn’t a colon. We express this by using an inverted group, which
uses `[^<members>]` as syntax. Anything not in the group will be matched.
We match this one or more times, which is what `+` stands for, and then close
our group. At this point, we will have matched `Professor Koala` in the first
line.

Then we consume the colon and space `: ` and move on to the next group, which
uses the same trick to match anything up until the parentheses begin—that’s why
we tell it to match anything but an opening parenthesis (`[^\(]`) one or more
times (`+`). Note that this means that the name of a course can never include a
parenthesized section. We’re at `Professor Koala: A guide to eating eucalyptus`
now.

Both of these are premature optimizations. We could just as easily use `.` to
match anything instead of inverted groups, but that would probably slow us down
quite a bit<sup><a href="#3">3</a></sup>.

Lastly, we match the parenthesized section. Because parentheses are reserved
words in our pattern DSL we have to escape them (`\(` and `\)`, respectively).

Now we make another assumption: identifiers for majors will always be one or
more uppercase letters. This leads us to the group `[A-Z]`. If we wanted to
match any letter character, we could use `\l` instead, which even respects the
user’s locale. The downside is that this would make our pattern not a
PCRE-compliant regular expression anymore. Now, we’ve matched `Professor Koala: A guide to eating eucalyptus (K`.

The very last thing we want to match is the course number, which we encode as
one or more digits (`\d+`). In our data-munging code in Figure 3 we then use
`Int.from-string` to read it into a number. All match groups are always strings,
no matter whether they could clearly something else. You’ll have to convert them
yourself<sup><a href="#4">4</a></sup>.

And this brings us to the end of our program. If you want to test this out, be
my guest! I’ve prepared a full listing of this code and an accompanying `main`
function. You can find it [here](/assets/curriculum.carp)<sup><a href="#5">5</a></sup>.

### Patterns versus regular expressions

At this point you’re probably fed up with me calling those damn things patterns
all the time. We all know that we’re talking about regular expressions!

Well, kind of. Except that the engine that Lua and, to that end, Carp implement
is not a regular expression engine. The Lua team has been very adamant about
this, and I plan to mimic that behaviour—it is their baby, after all!

So, what’s different? Most importantly, one of the main operators of regular
expressions is completely missing: alternation. That’s right, no `|`. This is
very important, because that little thing is often where the fun begins in a
regular expression engine, and its omission is one of the main reasons why
Luas’s engine is so small. The engine also implements some non-standard
operators such as `\l` mentioned above, but alternation is definitely the big
one.

I don’t want to sugar-coat it: the lack of altenration is definitely a drawback.
I’m not entirely sure whether Carp will eventually replace patterns with a PCRE
library, for exactly this reason. Which brings me to the next point: where we
currently are.

### Current status

Support for patterns was added in early March 2018, so as of the time of writing
of this article, the API and its implementation are very much in their alpha.

I’d like to say that the happy path of patterns—i.e. correct patterns that
actually run—is mostly stable. Erroneous patterns are less than ideal, but more
about that below.

I’ve discovered a few errors while preparing my blog post and playing around
with patterns and I assume you will, too. Such is life. I’m still working on
them, though, and would appreciate any bug reports. My preferred medium for this
is [the Carp issue tracker](https://github.com/carp-lang/carp/issues), but if for
one reason or another you’d prefer not to use it, notifying me through email or
[Gitter](https://gitter.im/carp-lang/Carp) are fine, too. I will then file an
issue myself.

One of the primary concerns I currently have is that Carp does not have a way
to report errors to the user. This means that the modus operandi for erroring
in patterns is printing to the standard output and returning `NULL`, currently,
which is the worst possible solution.

I am currently thinking about a pattern result type that has all the information
that we need encoded in it, i.e. whether a pattern succeeded or failed, where
the match occurred, and what the text of the match is. This would probably
simplify a lot of the already simple code. Stay tuned!

A cool thing that also landed in Carp shortly after patterns is compile-time
validation of pattern literals in the parser. I wrote that code one morning
during a meeting and I’m less than sure that it will works in all cases, but so
far it seems relatively stable.

We’re also not entirely clear on whether we will eventually need a
PCRE-compliant regular expression engine. As the main standard library author and maintainer at the moment, I’d rather not deal with that. If you have helpful
input on that front, I’d definitely like to hear it as well!

## Fin

I’ve had a ton of fun working on my first fully-fledged pattern matching
library. I’ve been surprised at how easy to implement the basic operations of
regular expressions are [when I wrote about it last year](https://blog.veitheller.de/Regular_Expressions_Made_Simple.html).
I also wrote about [a mostly forgotten pattern matching algorithm](https://blog.veitheller.de/Pattern_Matching,_A_Thing_Of_The_Past.html),
and marveled at its clarity. This time was no exception.

I’ve not worked on pattern matching engines a ton, let alone full-blown PCRE
engines. There are tons of [fancy optimizations](https://swtch.com/~rsc/regexp/)
in that space that I’ve heard of but have never implemented myself. I assume it
will be no different for a few of my readers—I know how well-read y’all are. I
therefore also assume that a number of you are under the impression that those
engines are large pieces held together by black magic and the occasional
ingenious line of code.

But like with anything in Computer Science, this is untrue. I know it now
because I worked on such an engine myself, and survived. It was a similar
experience as when I designed my [first programming language](https://github.com/zepto-lang/zepto),
or when I wrote [my first editor](https://github.com/hellerve/e), or tried my
hands [at a debugger](https://github.com/hellerve/d), or learnt how [simple
browser rendering engines work](https://github.com/hellerve/r). All of these
experiences were magical and addicting, because they opened my mind to what’s
possible. And now it happened again.

So, I guess I’ll leave you with a simple piece of advice: open any box you can
find. It’s the most rewarding thing you can do, and it will only very rarely
blow up in your face!

See you next time!

#### Footnote

<span id="1">1.</span> I even asked for permission and gave it proper
                       attribution! The Lua mailing list rocks!

<span id="2">2.</span> The parser loop could be more elegantly expressed as a
                       `map` operation, but I decided against that for
                       paedagogical reasons; it would require me to define
                       another function, and, in my opinion, that would make the
                       idea a bit harder to grasp.

<span id="3">3.</span> This is because we will have to backtrack much more. In
                       general, if we have assumptions such as “The professor
                       name will never contain a colon” or “The course name
                       will never contain a parenthesized section”, it is
                       generally worth encoding them.

<span id="4">4.</span> Hetereogeneous arrays are definitely one of the useful
                       data structures that are missing from most strongly-typed
                       languages. Then again, you’re better off explicitly
                       defining a struct type anyway.

<span id="5">5.</span> The program assumes that you’ve got the curriculum data
                       from Figure 2 stored in a file called `curriculum.txt`.
