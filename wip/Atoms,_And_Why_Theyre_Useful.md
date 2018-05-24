Today I want to talk about a programming language feature that goes by many
different names. Elixir, Erlang, and [zepto](http://github.com/zepto-lang/zepto)
call them atoms. Common Lisp and Clojure call them keywords. Smalltalk and Ruby
call them symbols. But all of those stand for basically the same thing: a thing
that evaluates to itself. The implementation details differ a little bit from
language to language, with some languages having a registry—basically a giant
table—that stores the terms to guarantee uniqueness and others translating them
to some kind of unique identifier at compile time.

When you first encounter this construct, you might not immediately recognize its
value. After all, most languages have a concept that seems to fit the bill
anyway; don’t strings provide the same basic functionality.

Let’s look at a couple of different use cases for atoms—which is what I will call
them in this blog post—, and explore why I think they are extremely useful.

## The semantic argument

I will try to make a case for them on a language semantic basis by comparing them
to what we might already find in a language: strings and identifiers (in Lisp we
call them symbols).

From a semantic perspective, strings are meant to represent text. While we use
them for flags and signalling between functions in many applications written in
languages that do not have atoms, this is not what they were made for<sup><a href="#1">1</a></sup>.
A string should be used either for interfacing with the user of our system or as
a piece of data that you want to apply some transformation on—say a compiler,
linter, or even a HTTP request handler.

One perfectly valid usecase of symbols is to use them for signalling between
functions.

#### Footnotes

<span id="1">1.</span> Admittedly, in many languages you would some kind of enum
                       type for that.
