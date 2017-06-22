*This post is the third installment in an ongoing series on
[GCC Builtins](https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html).
For parts I and II, click [here](http://blog.veitheller.de/Builtin_Goodies_I.html)
and [here](http://blog.veitheller.de/Builtin_Goodies_II.html), respectively.*

In this third post we will again learn to help our compiler
figure out how to make assumptions about our code, which in turn might help
make our code more performant.

This time we will look into “prefetching”, i.e. moving data into memory that
we're sure we will need soon. Our compiler is somewhat smart about that, but even
today we can sometimes do better than the machine—although I'd be very wary
and go down that rabbit hole only when I need to, which fankly isn't all too often.

Let's tell GCC to grab our stuff!

## Could you get that for me?

The builtin we need to get GCC to emit data prefetch instructions is
`__builtin_prefetch(addr, ...)`, where `addr` is a valid memory address and
the optional arguments are `rw` and `locality`, both of which are integers.
Before we get into the semantics of the arguments, allow me to clear things
up for my more inquisitive readers: data prefetch instructions aren't
available on all platforms. A list of supported targets can be obtained
[here](https://gcc.gnu.org/projects/prefetch.html#targets). The builtin,
however, takes care of validating whether or not such an instruction exists,
and will simply generate a `nop`. I wasn't able to verify whether that
stunts the collapsing of code paths—as `asm("nop")` would do—, because all of
my machines generate prefetching instructions.

As for the arguments, `addr` can be any expression that represents or generates
a valid address. Even complicated ones such as `&(l[i+1].str)` seem to work.
The first optional argument `rw` must be a compile-time boolean flag, i.e. `0`
or `1`, that signals whether we read (`0`) or write (`1`) to the memory address
we prefetch, where reading is the default. The second optional argument
`locality` must also be given as a constant value in the range `[0..3]`, where
`3` is the default. It tweaks the degree of temporal locality, meaning that if
we want to access the memory address multiple times—such as in a long loop—we
can keep this value as it is. We might want to tweak it if we only access it
once or more. Both of these values should probably only ever set after
experimenting with their perforamnce implications, because I'm not sure they
make a huge difference. Then again, this blog post is basically on
micro-optimizations, so maybe my audience will disagree.

```
__builtin_prefetch(&a[0], 1);
for (i = 0; i < n; i++) {
  a[0] += a[i] + b[i];
  /* Do some other stuff */
}
```
<div class="figure-label">Fig. 1: An example of how to use the builtin.</div>

The example above is a tad silly, but it illustrates the point. We opted for
the highest degree of temporal locality, because we will need the address
during all of the loop. For a less silly example, look at [this implementation
of binary search](/assets/binsearch.c), where the access is hard to optimize
for the compiler, but easy to optimize for the programmer. The implementation
is pretty standard, but if you toggle the prefetch option you might get
some pretty telling differences. Using `perf` I get the following results:

```
> perf stat ./fetch
Performance counter stats for './fetch':

  16325.496220 task-clock:u (msec) #  1.000 CPUs utilized
             0 context-switches:u  #  0.000 K/sec
             0 cpu-migrations:u    #  0.000 K/sec
     2,003,706 page-faults:u       #  0.104 M/sec
34,011,082,658 cycles:u            #  1.760 GHz
 6,989,566,005 instructions:u      #  0.21  insn per cycle
   975,131,080 branches:u          # 50.458 M/sec
   102,786,380 branch-misses:u     # 10.90% of all branches

  16.327586407 seconds time elapsed

> perf stat ./nofetch
Performance counter stats for './nofetch':

  20601.422096 task-clock:u (msec) #  1.000 CPUs utilized
             0 context-switches:u  #  0.000 K/sec
             0 cpu-migrations:u    #  0.000 K/sec
     1,857,587 page-faults:u       #  0.090 M/sec
38,226,536,467 cycles:u            #  1.856 GHz
 4,057,610,809 instructions:u      #  0.11  insn per cycle
   870,122,085 branches:u          # 42.236 M/sec
   129,432,512 branch-misses:u     # 14.88% of all branches

  20.602873818 seconds time elapsed
```
<div class="figure-label">
  Fig. 1: A methodically questionable benchmark (the output was reformatted to
  fit on the page).
</div>

I also learned that using `perf` on your Webfaction VM makes Webfaction
unhappy, and it will open tickets because you exceed your memory limit. Oops.

The performance gain seems to come largely from a decrease in cache load
misses—you will also see an increase in cache loads, but this leads to
a better memory access pattern. I would've liked to show you the output of
`perf` with these misses enabled, but that'll have to wait until I have a
Linux machine that will doesn't blow up my hosting plan when I run tests on it.

## A quick recap

I ran into a lot of walls while testing the builtin. It
turns out that most applications don't need manual prefetching—which is fairly
obvious if you think about how smart compilers are in this regard—, and it's hard
to find a problem with random enough memory accesses to validate claims of
speedup. Chances are that your codebase has only a very limited set of sections
where optimizing in this way makes sense, and I wouldn't spend much time on it.
I also learned that detailed performance measurement in OS X doesn't really
seem to be a thing. I'm not really an expert in that field, though, and would
love some input in what's useful for detailed benchmarking on Macs.

Having said that, I still think this is a useful tool to have in your
optimization toolbox and that understanding it might help you understand memory
access patterns, caching, and pipelining, concepts that I have only working
knowledge of.
