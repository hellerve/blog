---
title: "Tracing np.add, all the way down"
date: 2026-09-02
---

The notes for this blog post have been sitting in my drafts folder for half
a year now. I’ve done a little work on NumPy itself in the past year. Nothing
notable, but enough to have to find my way around the source. That gave me the
idea to write this, but then, as other obligations overshadowed my NumPy
contributions, it just started rotting quietly. One or two NumPy releases
later I finally picked it up again, retraced my steps, and here we are.

Here’s the premise: `np.add(a, b)` might well be among the most executed
lines of numerical Python in the world, and most of us have a working mental
model that’s equivalent to “it adds the arrays, in C, quickly”. That model is
correct, but there’s a lot of machinery between the Python call and the loop
that does the adding, and I think it’s a fun machine to take apart. So today
we’ll trace a single call, `np.add(a, b)` with two `float64` arrays, from
the Python entry point all the way down to the SIMD kernel, reading the
actual NumPy source as we go. We’ll learn a lot, I hope!

Everything below is pinned to [NumPy 2.5.2](https://github.com/numpy/numpy/tree/v2.5.2),
the current release as I write this, and all links point into that tag. The
internals move around between versions<sup><a href="#1">1</a></sup>, so if
you’re spelunking along at home, check out the matching tag. I’ll assume
you’re at least somewhat comfortable reading C, but no NumPy internals
knowledge is required, that’s what we’re here for.

## The map

Before we dive in, here’s the treasure map, so you always know where we are:

```
np.add(a, b)                      (Python)
   │
   ▼
ufunc_generic_fastcall            (C: parse arguments)
   │
   ▼
__array_ufunc__ override check    (may divert to other libraries)
   │
   ▼
promotion & dispatch              (find the float64 loop, cache it)
   │
   ▼
trivial loop  or  NpyIter         (iteration strategy)
   │
   ▼
DOUBLE_add                        (the actual inner loop, SIMD)
```

Each of these is a section below. Let’s start at the top.

## np.add is an object

The first thing to know is that `np.add` is not a normal Python function.
It’s an instance of `numpy.ufunc`, a C-defined type<sup><a href="#2">2</a></sup>:

```python
>>> type(np.add)
<class 'numpy.ufunc'>
>>> np.add.nin, np.add.nout
(2, 1)
>>> len(np.add.types)
22
>>> np.add.types[11:14]
['ee->e', 'ff->f', 'dd->d']
```

A ufunc is, at its core, a bundle of inner loops. One small C function per
supported type signature, plus metadata about how many inputs and outputs
there are. `np.add` ships 22 of them, though I should say that `types` only
lists the classic ones: loops registered the modern way (more on that
distinction later) live in an internal mapping on the ufunc that Python
never sees. The one we’re chasing today is
`dd->d`: double, double, to double. The whole rest of this post is
about how NumPy gets from your call to that one entry, and what happens once
it’s found. Other paths may vary, it’s a big piece of kit!

Let’s dive into the cave.

## Into the C

When Python sees `np.add(a, b)`, it calls the ufunc object. The `ufunc` type
implements the vectorcall protocol, so the call lands in
[`ufunc_generic_vectorcall`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/ufunc_object.c#L4905),
which immediately forwards to the real workhorse,
[`ufunc_generic_fastcall`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/ufunc_object.c#L4561).
That function is long, but it reads like a checklist, and it _is_ the
skeleton of the whole operation. Heavily abbreviated:

```c
static PyObject *
ufunc_generic_fastcall(PyUFuncObject *ufunc,
        PyObject *const *args, Py_ssize_t len_args, PyObject *kwnames,
        npy_bool outer)
{
    /* ... extract inputs, outputs, and keyword arguments ... */

    /* We now have all the information required to check for Overrides */
    PyObject *override = NULL;
    errval = PyUFunc_CheckOverride(ufunc, method,
            full_args.in, full_args.out, where_obj,
            args, len_args, kwnames, &override);
    /* ... if an override was found, return its result ... */

    /* ... convert arguments to arrays, extract their DTypes ... */

    PyArrayMethodObject *ufuncimpl = promote_and_get_ufuncimpl(ufunc,
            operands, signature, operand_DTypes, ...);

    /* Find the correct descriptors for the operation */
    if (resolve_descriptors(nop, ufunc, ufuncimpl, ...) < 0) {
        goto fail;
    }

    /*
     * Do the final preparations and call the inner-loop.
     */
    errval = PyUFunc_GenericFunctionInternal(ufunc, ufuncimpl,
            operation_descrs, operands, casting, order, wheremask);

    /* ... wrap the outputs and return them ... */
}
```

Parse, check for overrides, pick a loop, run it, wrap the result. Looks like
we have a plan! Now for the interesting parts.

The light is getting dim in our cave.

## An escape hatch

Before NumPy commits to doing any work, it asks the arguments whether they’d
rather do it themselves (always a good modus operandi).
[`PyUFunc_CheckOverride`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/override.c#L206)
walks all inputs and outputs and looks for a non-default `__array_ufunc__`
method, the protocol defined in [NEP 13](https://numpy.org/neps/nep-0013-ufunc-overrides.html)<sup><a href="#3">3</a></sup>.
If any argument has one, NumPy calls it and returns whatever it produces,
and none of the machinery we’re going to talk about below ever runs.

This is the hook that makes `np.add(dask_array, cupy_array)` behave with third
parties! Libraries like Dask and CuPy implement `__array_ufunc__` and take
over. We can play that tune ourselves in four lines:

```python
class Diverted:
    def __array_ufunc__(self, ufunc, method, *inputs, **kwargs):
        return f"intercepted {ufunc.__name__}.{method}"

>>> np.add(np.arange(3), Diverted())
'intercepted add.__call__'
```

Our object does nothing but win the argument about who computes. For our trace
we’ll assume both arguments are plain `ndarray`s, so nothing is overridden and
we continue rappelling.

## Choosing a loop

Next, NumPy has to get from “two `float64` arrays” to “that one `dd->d`
entry”. This is promotion and dispatch, and it lives in
[`dispatching.cpp`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/dispatching.cpp),
whose header comment is the best documentation of the process I’ve found
anywhere, so let me just quote it, typos and all:

```
The process of dispatching and promotion can be summarized in the following
steps:

1. Override any `operand_DTypes` from `signature`.
2. Check if the new `operand_Dtypes` is cached (if it is, got to 4.)
3. Find the best matching "loop".  This is done using multiple dispatching
   on all `operand_DTypes` and loop `dtypes`.  A matching loop must be
   one whose DTypes are superclasses of the `operand_DTypes` (that are
   defined).  The best matching loop must be better than any other matching
   loop.  This result is cached.
4. If the found loop is a promoter: We call the promoter. It can modify
   the `operand_DTypes` currently.  Then go back to step 2.
5. The final `ArrayMethod` is found, its registered `dtypes` is copied
   into the `signature` so that it is available to the ufunc loop.
```

A few translations are in order. The `signature` is what you fix explicitly
when you call `np.add(a, b, dtype=...)`; in our call it’s empty. A “promoter”
is a registered helper that handles cases where no loop matches directly by
rewriting the requested types and letting dispatch run again. Confusingly, the
everyday mixed case, `np.add(int32_array, float64_array)`, doesn’t even use
one: when dispatch comes up empty there, it falls back to the ufunc’s _old_
type resolution machinery (`PyUFunc_AdditionTypeResolver`, in our case) to
pick the common types, and then re-enters dispatch with those, landing on
`dd->d`.

And the cache in step 2 matters a lot! The full resolution only happens the
first time you call a ufunc with a given combination of types. For an
ordinary cacheable case like ours, every later call with the same types is a
single hash lookup on the DType classes. It’s pure machinery, but it’s
useful.

To understand what we now have, we have to engage in some archaeology and
word-slinging.

What [`promote_and_get_ufuncimpl`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/dispatching.cpp#L1047)
returns is a `PyArrayMethodObject`, the modern (post-[NEP 43](https://numpy.org/neps/nep-0043-extensible-ufuncs.html))
representation of “one concrete implementation of an operation for concrete
DTypes”. For `float64` addition, though, the ArrayMethod is a thin wrapper
around something much older. When the actual loop is needed,
[`get_wrapped_legacy_ufunc_loop`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/legacy_array_method.c#L203)
calls [`PyUFunc_DefaultLegacyInnerLoopSelector`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/ufunc_type_resolution.c#L1531),
which does exactly what one would write the first time around! It walks the
ufunc’s `types` table, entry by entry, until it finds `dd->d`, and returns
`ufunc->functions[i]`, a plain C function pointer. The wrapper that adapts
it to the modern interface is adorable:

```c
static int
generic_wrapped_legacy_loop(PyArrayMethod_Context *NPY_UNUSED(context),
        char *const *data, const npy_intp *dimensions, const npy_intp *strides,
        NpyAuxData *auxdata)
{
    legacy_array_method_auxdata *ldata = (legacy_array_method_auxdata *)auxdata;

    ldata->loop((char **)data, dimensions, strides, ldata->user_data);
    if (ldata->pyerr_check && PyErr_Occurred()) {
        return -1;
    }
    return 0;
}
```

That `ldata->loop` call is the “classic” ufunc inner-loop interface,
`PyUFuncGenericFunction`, unchanged for decades. It’s an array of data
pointers, an element count, a stride per operand, and an opaque payload. Every
builtin numeric loop in NumPy still has this shape. Everything above it exists
to line up memory so that calling it is correct<sup><a href="#4">4</a></sup>.

We are deep in the cave now. It’s almost entirely dark.

## To iterate or not to iterate

We have a loop, now we need to feed it. That’s
[`PyUFunc_GenericFunctionInternal`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/ufunc_object.c#L2192),
and it makes one interesting decision:

```c
/*
 * This checks whether a trivial loop is ok, making copies of
 * scalar and one dimensional operands if that should help.
 */
int trivial_ok = check_for_trivial_loop(ufuncimpl,
        op, operation_descrs, casting, buffersize);
/* ... */
if (trivial_ok && context.method->nout == 1) {
    /* Try to handle everything without using the (heavy) iterator */
    int retval = try_trivial_single_output_loop(&context,
            op, order, errormask);
    if (retval != -2) {
        return retval;
    }
}

return execute_ufunc_loop(&context, 0,
        op, order, buffersize, casting, op_flags, errormask);
```

The fast path comes first: if the shapes match, nothing needs broadcasting or
casting, and every operand is 1-D or contiguous,
[`try_trivial_single_output_loop`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/ufunc_object.c#L869)
calls the inner loop once over the entire data. No iterator is
constructed at all. For the everyday `np.add(a, b)` of two well-behaved
same-shape arrays, this is the path you’re on.

Everything else goes through [`execute_ufunc_loop`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/ufunc_object.c#L1061)
and `NpyIter`, NumPy’s general array iterator. The construction flags are a
compact summary of everything it takes care of for the inner loop:

```c
npy_uint32 iter_flags = ufunc->iter_flags |
             NPY_ITER_EXTERNAL_LOOP |
             NPY_ITER_REFS_OK |
             NPY_ITER_ZEROSIZE_OK |
             NPY_ITER_BUFFERED |
             NPY_ITER_GROWINNER |
             NPY_ITER_DELAY_BUFALLOC |
             NPY_ITER_COPY_IF_OVERLAP;
```

Broadcasting, buffering of misaligned or casting operands, overlap detection
between inputs and outputs, all of it is the iterator’s problem. The actual
execution afterwards is almost anticlimactic:

```c
int res;
do {
    res = strided_loop(context, dataptr, countptr, strides, auxdata);
} while (res == 0 && iternext(iter));
```

The iterator hands the loop one contiguous-ish chunk at a time and advances
the pointers in between. Dust off your hands, you’re done.

But: both paths also share two bits of bookkeeping worth knowing about. First,
unless the loop needs the Python API (our `float64` loop doesn’t, the `object`
dtype loop does), NumPy releases the GIL around the whole thing, using a
threshold so tiny arrays don’t pay the overhead. Second, floating-point status
flags are cleared before and checked after the loop, which is where our old
trusty `RuntimeWarning: overflow encountered in add` comes from. The inner loop
itself never checks anything, it just computes and sets CPU flags.

One caveat before we descend further (it bit me in a benchmark for this post).
Strides alone don’t evict you from the trivial path. The trivial loop accepts
any 1-D array and simply hands the actual stride to the inner loop, so even
`a[::2]` is handled in a single call, no iterator in sight. I rebuilt NumPy
with its ufunc tracing enabled to check, and it takes something the trivial
path actually rejects, like a non-contiguous N-D view (think `base[:, ::2]`)
or an `out=` that overlaps an input unsafely, to make it print “Making
iterator”. (The overlap check is finer than you’d guess: `out=a[1:]` reading
from `a[:-1]` spins up the iterator, while the other direction, where the
write safely trails the read, does not). Hold that thought,
it’ll matter in a minute.

We’re in the central room of the cave now. Hushed whispers only, the ceiling
is very fragile.

## The loop itself

Before we look at the inner loop, let’s give it something to chew on. Same
number of elements, same operation, but one version strides over every
second element:

```python
n = 10_000_000
a, b, out = (np.random.rand(2 * n) for _ in range(3))
contig_a, contig_b, contig_out = (x[:n].copy() for x in (a, b, out))
strided_a, strided_b, strided_out = (x[::2] for x in (a, b, out))

np.add(contig_a, contig_b, out=contig_out)     # 3.30 ms per call
np.add(strided_a, strided_b, out=strided_out)  # 8.58 ms per call
```

A factor of 2.6 on my machine, for the same number of additions. And we know
from the last section that both calls arrive here the same way. The trivial
path, one call into the inner loop, real strides and all (that’s the thought
you were supposed to hold, at ease now). So whatever explains the gap has to
live inside `DOUBLE_add` itself. Part of it is memory bandwidth—the strided
version touches twice the memory!—, but let’s see about the rest.

So what does `DOUBLE_add` actually look like? Here I have to disappoint you
first, because there is no file in the repository containing a function called
`DOUBLE_add`. The elementwise loops live in template files like
[`loops_arithm_fp.dispatch.c.src`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/loops_arithm_fp.dispatch.c.src),
which use NumPy’s in-house templating language, because every big project
grows a config format and a templating language:

```c
/**begin repeat
 * Float types
 *  #type = npy_float, npy_double#
 *  #TYPE = FLOAT, DOUBLE#
 *  #sfx  = f32, f64#
 */
/**begin repeat1
 * Arithmetic
 * # kind = add, subtract, multiply, divide#
 * # intrin = add, sub, mul, div#
 * # OP = +, -, *, /#
 */
NPY_NO_EXPORT void NPY_CPU_DISPATCH_CURFX(@TYPE@_@kind@)
(char **args, npy_intp const *dimensions, npy_intp const *steps, void *NPY_UNUSED(func))
{
    npy_intp len = dimensions[0];
    char *src0 = args[0], *src1 = args[1], *dst = args[2];
    npy_intp ssrc0 = steps[0], ssrc1 = steps[1], sdst = steps[2];
    ...
```

At build time, a small script ([`conv_template.py`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_build_utils/conv_template.py))
expands each `repeat` block for every value combination, so this one
function body becomes `FLOAT_add`, `DOUBLE_add`, `FLOAT_subtract`, and so
on, eight functions from one template. Note the signature: it’s the
`PyUFuncGenericFunction` we met two sections ago. We’ve arrived. Don’t touch
the cave walls, please.

Inside, the function is a cascade of specializations. There’s a branch for
the reduction case, then SIMD-accelerated versions for the common stride
patterns (both inputs contiguous, one input a scalar), written against
NumPy’s “universal intrinsics”, a portable SIMD abstraction where
`npyv_add_f64` maps to whatever the target CPU calls adding a vector of
doubles. And when no specialization fits, it falls through to the loop you
would have written (though maybe you’re a bit less terse):

```c
loop_scalar:
    for (; len > 0; --len, src0 += ssrc0, src1 += ssrc1, dst += sdst) {
        const @type@ a = *((@type@*)src0);
        const @type@ b = *((@type@*)src1);
        *((@type@*)dst) = a @OP@ b;
    }
```

And there’s the rest of our 2.6x performance gap. The SIMD specializations all
guard on their operands being contiguous or scalar, so the strided call from
our benchmark fails every check and spends its whole life in this scalar
branch, while the contiguous call gets the vectorized one. What’s left of
the gap after the bandwidth tax is the price of ending up down here in the
sad loop.

All this way down, and at the bottom of NumPy there’s a pointer-bumping
`for` loop adding two doubles. We’re all just fumbling in the dark after all.

But there’s a tiny grotto that hangs off of the main hall. Let’s take a look
before we go, there couldn’t possibly be any eldritch horrors lurking in the
deep with us.

## Where the loops come from

One mystery remains: how do those generated functions end up in the
`ufunc->functions` table that the dispatch walked? The answer is, as ever, more
code generation. The entire builtin ufunc inventory is defined in a Python
dictionary in [`generate_umath.py`](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/code_generators/generate_umath.py#L355).
Here’s the extremely readable entry for `add`:

```python
'add':
    Ufunc(2, 1, Zero,
          docstrings.get('numpy._core.umath.add'),
          'PyUFunc_AdditionTypeResolver',
          TD('?', cfunc_alias='logical_or', dispatch=[('loops_logical', '?')]),
          TD(no_bool_times_obj, dispatch=[
              ('loops_arithm_fp', 'fdFD'),
              ('loops_autovec', ints),
          ]),
          [TypeDescription('M', FullTypeDescr, 'Mm', 'M'),
           TypeDescription('m', FullTypeDescr, 'mm', 'm'),
           TypeDescription('M', FullTypeDescr, 'mM', 'M'),
          ],
          TD(O, f='PyNumber_Add'),
          indexed=intfltcmplx
          ),
```

I might need a drink.

This is the 22-entry `types` list from the beginning of the post, in its
source form. You can read the whole design of `add` off of it: booleans
reuse `logical_or`, the float and complex types (`'fdFD'`) get their loops
from `loops_arithm_fp`, integers get auto-vectorized loops, datetimes get
hand-written special cases, and `object` arrays simply call Python’s own
`PyNumber_Add`. At build time this dictionary is rendered into a C file,
`__umath_generated.c`, containing the function table and a call that
constructs the `np.add` object when you import NumPy.

There’s one final (I promise!) trick hiding in that generated file. The
`dispatch=` markers above mean the loop is compiled *multiple times*, once
per interesting CPU feature set (the file suffix `.dispatch.c.src` tells the
build system to do this). Which compiled variant actually gets installed into
the table is decided at import time by a macro that expands to a chain of
runtime CPU checks. The [documentation in the build config](https://github.com/numpy/numpy/blob/v2.5.2/meson_cpu/main_config.h.in#L226)
shows the shape of the expansion with a made-up example (illustrative only,
the features are per-loop and there is no AVX-512 variant of our `add`):

```c
NPY_CPU_DISPATCH_CALL(func = add);

// Unwrapped version:
func = NPY_CPU_HAVE(AVX512_SKX) ? add_AVX512_SKX :
           (NPY_CPU_HAVE(AVX2) ? add_AVX2 :
               add);  // baseline
```

The real targets for each loop family live in `meson.build`. For
`loops_arithm_fp` they are `X86_V3, X86_V2, ASIMD, NEON`, where `X86_V3` is
the `x86-64-v3` microarchitecture level, which is to say AVX2. So the
`DOUBLE_add` pointer sitting in the table has already been chosen for
your particular CPU when `numpy` was imported. You can even ask NumPy which
variant you got:

```python
>>> from numpy.lib import introspect
>>> introspect.opt_func_info(func_name="add", signature="float64")
{'add': {'ddd': {'current': 'baseline(NEON NEON_FP16 NEON_VFPV4 ASIMD)',
                 'available': 'baseline(NEON NEON_FP16 NEON_VFPV4 ASIMD)'}}}
```

On my ARM-based Mac there’s only one candidate, since NEON is unconditionally
available. On an x86 machine you’d see the `X86_V3`/`X86_V2` ladder instead.

Okay, okay, time to jumar back up. It’s getting spooky in here.

## Fin

Let’s walk the route once more, to recap. `np.add` is an object holding a
table of 22 loops. A call parses arguments, offers everyone a chance to
override, resolves the input types to one table entry (cached after the
first time), picks an iteration strategy (a single call if the memory is
friendly, `NpyIter` if it isn’t), releases the GIL, and runs a
CPU-specific, template-generated C function that, in its most general
branch, is a plain `for` loop.

To stay with our metaphor, what impressed me most while exploring this is how
visible the geology is. The inner loop signature and the `types` table are
decades old, and they’re still the core of the machine. The `ArrayMethod`
layer, the override protocol, and the SIMD dispatch were each deposited on top
without displacing their history. The file that wraps the old interface is
literally called `legacy_array_method.c`, and it’s not deprecated machinery, it
_is_ the machinery, running an unfathomable number of additions every single
day. Now that is a legacy.

But the old strata aren’t frozen in place, and I have evidence, because I
stumbled into it. I read the source on the main branch first and then
re-verified everything against the 2.5.2 tag before quoting it, and one of
my searches failed, because the legacy loop lookup from earlier has already
been refactored on main. Instead of the selector scanning the `types` table
every time a loop is set up, the scan now runs once, when the `ArrayMethod`
is registered, and the found loop is cached on it. The selector, and its
`TODO` comment wishing for “a loop selection acceleration structure, like a
hash table”, both live on, they just do their work once instead of on every
call now.

The motive is the best part: this belongs to a family of changes preparing
NumPy for free-threaded Python, where lazily initialized global state turns
from a code smell into a real problem<sup><a href="#5">5</a></sup>. Somebody
looked at one of the most-executed codepaths in numerical Python, decades in,
battle-proven if anything ever was, and reworked it, because the world had 
changed. The oldest layer is still being tended to, even with all the deposits
on top. It’s beautiful.

I hope you enjoyed the descent! If this post made you want to go further,
building NumPy from source and putting a breakpoint in
`ufunc_generic_fastcall` is a surprisingly pleasant afternoon. Just keep a
beverage at hand. See you around!

*Thanks to [Nathan Goldbaum](https://github.com/ngoldbaum) for reviewing this
blog post!*

#### Footnotes

<span id="1">1.</span> I mean it! Part of the machinery described in this
post has already been reworked on NumPy’s main branch, as we’ll see in the
Fin. People still hack on `np.add` between releases.

<span id="2">2.</span> Specifically, it lives in the C extension module
`numpy._core._multiarray_umath`. The unwieldy name is, apparently, a
historical artifact: the array machinery (`multiarray`) and the ufunc
machinery (`umath`) were separate C modules for most of NumPy’s life, and
were merged by [NEP 15](https://numpy.org/neps/nep-0015-merge-multiarray-umath.html).
If you’ve ever wondered why older NumPy guides reference modules that no
longer exist, that’s why. The joys of software refactoring.

<span id="3">3.</span> Not to be confused with `__array_function__` from
[NEP 18](https://numpy.org/neps/nep-0018-array-function-protocol.html),
which is the analogous protocol for non-ufunc functions like
`np.concatenate`. My old notes for this post confidently claimed both are
checked on the ufunc path; they are not, ufuncs only consult
`__array_ufunc__`. The notes came from an LLM conversation, which is a
lesson in itself.

<span id="4">4.</span> Nothing about the ArrayMethod layer requires
wrapping old-style loops, it exists just so that new dtypes can
register loops natively. The string dtype work in NumPy 2.0 is an
example of modern loops: [here is where `StringDType` registers its `add`
loop](https://github.com/numpy/numpy/blob/v2.5.2/numpy/_core/src/umath/stringdtype_ufuncs.cpp#L2732),
and it never appears in `np.add.types`, which only reflects the classic
table. That mapping I mentioned at the top, the one Python never sees, is
where such loops live. For the builtin numeric types, though, the “legacy”
interface remains the real thing.

<span id="5">5.</span> This came to me by way of NumPy core developer
[Nathan Goldbaum](https://github.com/ngoldbaum) reviewing a draft of this post
(thank you!). One example from the same family: [a fix](https://github.com/numpy/numpy/commit/c412bedf8e4b0b14f455d3b2cee8034899543e85)
moving reduction-initial-value setup from call time to ufunc
initialization, motivated by multithreaded reductions. The dispatch cache
itself, a `PyArrayIdentityHash`, also recently got a concurrency-minded
overhaul, described in Quansight’s [“Scaling NumPy on free-threaded
Python”](https://labs.quansight.org/blog/scaling-numpy-on-free-threaded-python).
