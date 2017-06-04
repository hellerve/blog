It is about time I wrote a second post on
[GCC Builtins](https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html). In
[the last installment](http://blog.veitheller.de/Builtin_Goodies_I.html) we
learned how to help our branch predictor to figure out which branches to
speculatively execute. This time we will learn figure out how to tell our
compiler that a segment of code will never be reached if it cannot figure that
out on its own.

I have already written a bit about when and when not to use these very specific
and obscure features. The short answer is you probably don't want to use them at
all unless you really want to; you're also limiting yourself to GCC, which might
or might not be okay for you. That's all I am going to say about that matter
this time. Consider yourselves warned!

And now, without further ado, let's tell our compiler to look the other way!

## Nothing to see here, move along

There are times when our compiler cannot possibly know about control flow.
Consider the example of [`e`](https://github.com/hellerve/e), an editor [I wrote
about previously](http://blog.veitheller.de/Braindead_Editing.html). The main
event loop looks more or less like this:

```
void event_loop(editor e) {
  while (1) {
    redraw_screen(e);
    e = process_key(e);
  }
}

int main() {
  editor e = setup_editor(e);
  event_loop(e);
}
```
<div class="figure-label">Fig. 1: An artistic rendition of `e`s event loop.</div>

This loop obviously never terminates. Instead, my library calls `exit()`
internally when certain keys are pressed, and a registered cleanup function is
called. This is all well and good, but I declared my `main()` function to return
an `int`. GCC will helpfully complain about this—provided we turn on certain
warning levels or turn on `-Wreturn-type` manually:

```
main.c: in function 'main':
t.c:61:1: warning: control reaches end of
non-void function [-Wreturn-type]
```
<div class="figure-label">Fig. 2: GCC being unhappy with me.</div>

If I put `__builtin_unreachable();` at the end of the function, though, the
error vanishes. To be clear from the get-go: this has implications on the
generated machine code. It will prevent GCC from creating code to return from
the function. More importantly, though, it means less warnings and clear, stated
intent, which is what makes me write it.

It is also useful if we're writing inline assembly that performs jumps. I'm not
sure how many of my readers do that, but I'm sure all of them are eager to know
that `__builtin_unreachable` comes to the rescue there as well. Let me give you
a contrived example:

```
void helper() {
  asm("jump_towards_me:");
  puts("hi!");
}

int main() {
  asm("jmp jump_towards_me");
}
```
<div class="figure-label">Fig. 3: Jumping around for no good reason.</div>

Don't do this at home. This is not good. If you need to do something like that,
you're probably better off using
[setjump/longjump](http://man7.org/linux/man-pages/man3/longjmp.3.html), which I
know I've used before for implementing tail recursion. But it is tedious,
brittle, and best avoided. Still, isn't it a great comfort that even in this
case we can make the error go away by inserting a single
`__builtin_unreachable`?

## Builtins are amazing!

The more I read about them, the more I appreciate GCC's builtins, even if I
sacrifice portability by using them. They're just damn handy.

Next time, we will explore how to minimize cache-miss latency by using a
GCC builtin, so stay tuned!
