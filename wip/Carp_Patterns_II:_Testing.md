In the second installment of [my series on my personal view on current best
practices in Carp](/carp-pattens), we’re going to talk about testing! This is
one of my favorite topics, right after [documentation](/Carp_Patterns_I:_Documentation.html).

The standard library of Carp comes with a testing framework that is good enough
for the kind of unit tests that I write, although it is fairly spartan.
Nonetheless, it will give you a good foundation on which to base your tests
and make sure that the hardest part of your CI process is getting Carp
installed on the CI server.

## Baby’s first assertion

If you use [the Carp project template on carpentry](https://github.com/carpentry-org/template)
you already get a test file in `test/tests.carp`. It will contain one failing
test:

```
(load "Test.carp")
(use Test)

(deftest test
  (assert-equal test
                "TODO"
                "Implement me"
                "TODO: implement me"
  )
)
```
<div class="figure-label">Fig. 1: A simple failing test.</div>

All flavors of `assert` from `Test` will take a state as the first argument,
which will be created by the `deftest` macro. Then they take the things to
compare or assert against—usually one, in the case of for instance
`assert-true`, or two, in the case of comparisons—, and finally they take a
message to display for the test. If we run the test script using `carp -x
test/tests.carp`, we will get colorful output, telling us which tests pass and
fail.

Often this is enough for my unit testing needs, especially in simple functional
libraries. If I use `assert-exit` or `assert-signal`, I can even test exit
codes and signal conditions with it.

### Structuring your tests

I like putting all of my tests together, as a large battery. As the number of
tests increases and their scope broadens this might get unwieldy. I
have two go-to methods to rectify this.

If I want to split up my tests by module, I usually put the tests in different
files. This ensures that I can easily trigger just the tests that target the
module that I’m currently working on.

Inside these files, I can split up my test functions into multiple groups. I
move from using `deftest` to `with-test`, like this:

```
; sorry for the long code example;
; i wanted to share how to split up a lot of code

(defn test-parsing []
  (do
    (println "## Parsing")
    (with-test test
      (assert-equal test
                    "scheme"
                    (scheme &(Uri.parse "scheme://domain.tld"))
                    "we can parse the domain"
      )
    )))

(defn test-stringification []
  (do
    (println "## Stringification")
    (with-test test
      (assert-equal test
                    "scheme://domain.tld"
                    &(str &(Uri.parse "scheme://domain.tld"))
                    "we can stringify our representation again correctly"
      )
    )))

(defn main []
  (Array.sum &[
    (test-parsing)
    (test-stringification)
  ]))
```
<div class="figure-label">Fig. 2: Testing a URI library.</div>

Basically, you have a bunch of functions that run tests, and maybe the print
out information about the test name in the beginning. But what is the
`Array.sum` in `main` doing? I’ve found it to be a neat trick to set the exit
status of failing test suites appropriately: since `with-test` returns the
number of tests that have failed, summing the results of all of the test
functions together will result in a non-zero exit code if at least one of the
test cases failed, making sure CI notices.

Realistically, writing any of this by hand should not be necessary, but the
`Test` module hasn’t been touched in a while, and noone has come up with a
nicer API. I guess a macro that generates a bunch of the repetitive code in
the test functions and another one that gathers all of the test functions by
looking for the prefix `test-` could be enough for me personally, but I’ll
leave this as an exercise for the reader.

### A note on running your tests

Finally, I’ll leave you with a pattern that some in the community use to speed
up their development workflow: they use file watchers to recompile and run the
tests when a Carp file changes. If you have `nodemon` installed, for instance,
you can use this spell to conjure up such a watcher process:

```
nodemon -e carp -x "carp -x --log-memory tests.carp || exit 1"
```
<div class="figure-label">
  Fig. 3: Executing your tests every time a file changes.
</div>

There are many such file watchers around, so you can just use whichever you
prefer.

## An Unfinished Story

That’s all you need to get started writing tests for your Carp projects. The
kinds of tests that you can write with any sort of framework are limited to
unit tests, but integration tests and end-to-end tests are definitely possible
with a little bit of elbow grease and possibly shell scripting.

In Carp proper, we have a few shell scripts that run Carp programs that are
supposed to generate error messages, and compare the output to an expected
file using `diff`. That’s simple, low maintenance, and does not require new
contributors to install any software. If I had to do something like that for my
other Carp projects, I would probably copy a page out of the compiler’s book.

Nonetheless, there are a lot of opportunities to make the testing story in Carp
better, even with little effort. Try it!

## Fin

This second installment of my series on patterns for Carp development again
didn’t have much to do with actually writing code, but don’t fret! Next time
we’re going to take a look at macros: how to write good, idiomatic, maintainable
code that uses Carp’s powerful compile time system to the fullest!

Stay tuned!
