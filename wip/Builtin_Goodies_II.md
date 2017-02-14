It is about time I wrote a second post on [GCC Builtins](https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html). In [the last installment]() we learned how to help our branch predictor to figure out which branches to speculatively execute. This time we will learn figure out how to tell our compiler that a segment of code will never be reached if it cannot figure that out on its own.

I have already written a bit about when and when not to use these very specific and obscure features. The short answer is you probably don't want to use them at all unless you really want to. That's all I am going to say about that matter. Consider yourselves warned!

And now, without further ado, let's tell our compiler to look the other way!

## Nothing to see here, move along

`__builtin_unreachable()`
