Large swaths of the computer science community—the part I know, anyway—equate the C programming language with speed. C is almost half a century old and the compilers we typically use are battle-hardened beasts, highly optimizing and very smart. Of course writing your code in C does not automatically result in performant programs. We need a good knowledge of our problem domain, choose appropriate algorithms and datastructures, benchmark, audit, and test. But that is not what I want to write about this time. What I want to tell you about, over a series of blog posts, are some internal intrinsics of your compiler that help you make the most of your code and highly optimize certain sections of your program. I suggest you go through the “benchmark, test, and audit” step first and make sure that your algorithms and data structures are truly appropriate before resorting to what I am about to show you, though. They are also local to one specific C compiler—GCC—, so if you are using Clang or ICC or whatever the cool kids are using these days, you're out of luck here. With this little *caveat lector* out of the way, let me take you to the world of GCC's [builtin functions](https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html).

## A second warning and a let-down

Turns out I am not entirely done with throwing caveats around. The section I linked to above is compiler-internal territory, and it assumes a good grasp on C and compiler concepts. If you are a professional C programmer chances are you are already familiar with most of the terminology used. If you are not, be sure to double-check every one of the functions presented there and your understanding of them. They might in fact be detrimental to your program's performance if used incorrectly, so when in doubt, do not use them.

For this exact reason I will also not go into every function listed on that page. I have only used a few of these functions in production and read code using a handful more, and I do not want to show you how to use a tool I've never touched myself. Of course the number of blog posts would also be much higher than I would like it to be if I explained every one of the functions, and not all of them are equally useful. I will probably only explain a handful of them and then get bored with this installment.

With all of the rubble out of the way, let's dive right into the first of my favorites.

## Taking control over the processor with `__builtin_expect`

`__builtin_expect` is fairly obscure, but tremendously useful. It will basically tell the processor which branch is more likely to be executed. This will help the [branch predictor](https://en.wikipedia.org/wiki/Branch_predictor) and help in making the speculative execution of one of the branches more likely. Now, there is a major drawback to this, and the GCC documentation page mentions it explictly: “`programmers are notoriously bad at predicting how their programs actually perform`”.

When should we use this, then? Either you know through your profiling which one of your branches executes more often than the other, or the branch is handling the rare case of an operating system error, set debugging options, or the like.

Let me give you the real world use case of the [jemalloc](https://github.com/jemalloc/jemalloc/blob/c0cc5db8717dd1d890bd52b687d9eef64a49554f/include/jemalloc/internal/util_types.h#L72) memory allocator. It aims to replace `malloc`, bringing goodies like thread-safety into the ring. In their codebase, they have [two macros](https://github.com/jemalloc/jemalloc/blob/c0cc5db8717dd1d890bd52b687d9eef64a49554f/include/jemalloc/internal/util_types.h#L72) abstracting over `__builtin_expect`, `likely` and `unlikely`. These two are basically annotations to tell the compiler which of the statements are likely and unlikely to execute that are stubbed out if we are using any other compiler. In this instance they really do make sense—we want our memory allocation to be fast and optimized, and we probably a good amount about the “normal” behavior of our code, i.e. its control flow. This code base then fulfills the requirements to make using this function sensible.

```C
// Artistic recreation of the jemalloc code
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

//And how to use it
#define assert(c) do {          \
  if (unlikely(!(c))) {         \
    printf("%s:%d: assert fail",\
           __FILE__, __LINE__); \
    abort();                    \
  }                             \
} while (0)
```

So, should you go throwing around `__builtin_expect`s all over your code? Probably not. If you are writing highly optimized code that requires every bit of CPU time, this function is for you. And, although I probably shouldn't say that, it feels powerful to use it—and this is probably be the best indicator for why you shouldn't. If you go around throwing random bits of optimizations around, it's likely you're not only doing more harm than good, but also that you and your coworkers will be having a bad time a few months down the road. With that being said, there are good reasons to use this kind of optimization when you know what you're doing and you now have another tool in your utility belt. See you next time!
