I sinned. In my last two blog posts on Carp, in which I talked about [Carp and
C interop](http://blog.veitheller.de/Carp_and_C_%28as_of_2017%29.html) and
[Carp’s format
strings](http://blog.veitheller.de/Adding_format_strings_to_Carp.html), I
committed the same error twice. Finally someone on Reddit’s wonderful
[r/programminglanguages](https://reddit.com/r/programminglanguages) pointed me
to a fix, and I want to share it with you. I’m sure most of the C programmers
that read this blog already know of it, but I still want to document this bit
of behaviour, because it’s useful, and it was new to me.

## Generating cruft

In both of the blog posts linked to above, we implemented the `str` interface
for our types. This more often than not involves writing C, especially when we
want to implement the interface for types defined in C.

In both of the implementations presented in the blog posts, I allocate too many
bytes. This is justified in two distinct ways: in one of the blog posts I
explain how to interface C and Carp, and the function is thus described as
“good enough”; in the other I explicitly ask for “atonement” and ask people to
fix it for me.

```
string Int_format(string* str, int x) {
    int n = strlen(*str) + 32;
    char *buffer = CARP_MALLOC(n+1);
    snprintf(buffer, n+1, *str, x);
    return buffer;
}
```
<div class="figure-label">Fig. 1: An offen(ding|sive) function.</div>

## Thank you, Reddit

A redditor known as 
[u/basic-gongfu](https://www.reddit.com/user/basic-gongfu) gave me something
much more useful: pointers on how to fix it.

He told me that `snprintf` has a bit of useful behaviour: if you pass it a
`NULL` pointer and `0` as the first two arguments, it will return the number
of bytes that _would be_ needed if we wanted to hold the full string. Without
the `0` byte at the end, because C was made to infuriate
you<sup><a href="#1">1</a></sup>.

This means we can fix the code from Figure 1! Here is an illustration of the
principle and how to apply it:

```
string Int_format(string* str, int x) {
    int n = snprint(NULL, 0, *str, x);
    char *buffer = CARP_MALLOC(n+1);
    snprintf(buffer, n+1, *str, x);
    return buffer;
}
```
<div class="figure-label">Fig. 2: Figure 1, fixed.</div>

The only thing that has changed is the second line, where we replaced the
brittle `strlen` and constant-based version with a call to `snprintf`. This
helps us in two ways: it is less brittle, relying solely on a library function
to work, and it will not allocate garbage but instead enables us to allocate
exactly as much memory as we need.

## Fin

I want to thank basic-gongfu profusely for making me aware of this bit of
behaviour. We’ve [incorporated](https://github.com/carp-lang/Carp/pull/172)
it into Carp already, removing a bug that has been on our radar for
a while and Carp’s memory footprint in one fell swoop.

I’d like to think that, once again, being upfront about the shortcomings of my
code has helped in getting feedback and help. Of course this fix it not largely
attributable to me, but I do think that an honest way of communicating the 
state of your projects helps people find a way to voice their concerns and help 
make your creations better with their experience.

And you get to learn cool stuff in the process, so that’s nice.

#### Footnotes
<span id="1">1.</span> This tip isn’t explicitly documented in the piece of
  [official documentation](http://www.cplusplus.com/reference/cstdio/snprintf/)
  that I looked at, and I would’ve likely lacked the imagination to use
  `snprintf` like that myself.
