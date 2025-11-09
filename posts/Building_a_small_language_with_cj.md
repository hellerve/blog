In [a recent blog post](https://blog.veitheller.de/cj:_Making_a_minimal,_complete_JIT.html),
I unveiled [cj](https://github.com/hellerve-pl-experiments/cj), a small,
dependency-free JIT compiler framework in C. In theat post, I discussed the idea
of implementing a small programming language in it.

Of course, once I had had the idea, [my fingers couldn’t stop typing until it was
done](https://github.com/hellerve-pl-experiments/cj/blob/master/examples/minilang.c).
So now we have a small, Lisp-y (admittedly by syntax only) language implemented
using `cj`, in a total of around 400 lines of C, 300 of which are the parser and
AST. Here’s the language:

```
# main is our entry point, call calls other functions
(def main (x) (sub (call inc x) 15))

# add and sub do math, all functions are unary
(def inc (x) (add x 2))
```

Since building a JIT-compiled language in less than 100 lines is cool in any
language, today I want to walk you through how the implementation works. A few
caveats before we begin, however: for the purposes of this post we’ll focus on
the code generator; the parser and any error handling are out of scope for
brevity (you can find them in the version checked into the repository).

So, let’s dive right in!

#### Prelude: An API and an AST

Before we implement anything, let me give you a quick rundown of the `cj`
functions we will be using today. This is intended as a reference, jump
back here if you need clarification on any of the bits we’re using!

```
// Our main CJ context that we’ll carry around
cj_ctx* cj = create_cj_ctx();

// we generate a scratch, a place from which we get our registers
// platform-independently
cj_builder_scratch scratch;

// we initialize it
cj_builder_scratch_init(&scratch);

// ... and eventually release it
cj_builder_scratch_release(&scratch);

// we generate a frame for each function
typedef cj_builder_frame;

// initializing a function and saving the info in the frame
cj_builder_fn_prologue_with_link_save(cj, /*optional stack bytes*/0, &frame);

// and eventually we return something from the frame
cj_builder_return_value(cg->cj, &frame, (cj_operand)value);

// we can make operands (a register, memory, address, value, etc.)
cj_operand constant = cj_make_constant(42);
cj_operand dst = cj_builder_scratch_acquire(&scratch);

// we can assign things (basically a wrapper around MOV instructions)
cj_builder_assign(cj, dst, constant);

// we can add and subtract the same way
cj_add(cj, dst, constant);
cj_sub(cj, dst, constant);

// we can generate labels we can jump to
cj_label entry;
cj_mark_label(cj, entry);

// and then jump to them with arguments (in our case we just need one)
cj_builder_call_unary(cj, &scratch, entry, constant);

// in the end, we generate something executable
cj_fn function = create_cj_fn(ctx);

// we can just call it
function();

// or find labels in it
void (*fn)(void) = cj_resolve_label(cj, function, entry);

// and call those
fn();
```

#### The AST

I know I said we wouldn’t be looking at the parser code, but we at least need to
know the data structure we will be working with once parsing is done. So for the
purposes of this blog post we will pretend that there is a magical function
called `parse_expr(input_string)` that gives us back an AST.

So, what does our AST for the simple language we defined above look like?

```
typedef enum { NODE_NUM, NODE_PARAM, NODE_ADD, NODE_SUB, NODE_CALL } node_kind;

typedef struct node node;
struct node {
  node_kind kind;
  int value;
  int target;
  char name[32];
  node *left;
  node *right;
  node *arg;
};
```

So we have numbers, parameters, and calls, and because we are lazy we also bake
the `add` and `sub` primitives in as special tokens. A `node` then has that
token type and a bunch of other things we don’t know about yet, as well as some
children (`left`, `right`, and `arg`).

All of these values are filled conditionally: `value` is filled if we have a
number, `name` if we have a parameter, `left` and `right` if we are dealing with
an `add` or `sub` node. Calling is a little special: it has an `arg` (simple
because all functions are unary), but it also has a `target`, which is the label
we are going to jump to. More about that later, since that part is on us, not
the parser. This AST also serves as our intermediate representation, so it mixes
parse- and compile-level concepts for the sake of simplicity.

The AST for our example program would look something like this:

```
// (def inc (x) (add x 2))
// (def main (x) (sub (call inc x) 15))
node X       = (node){ .kind=NODE_PARAM };
node TWO     = (node){ .kind=NODE_NUM, .value=2 };
node FIFTEEN = (node){ .kind=NODE_NUM, .value=15 };

node INC_BODY  = (node){ .kind=NODE_ADD, .left=&X, .right=&TWO };

node CALL_INC  = (node){ .kind=NODE_CALL, .name="inc", .arg=&X }; // target set later
node MAIN_BODY = (node){ .kind=NODE_SUB,  .left=&CALL_INC, .right=&FIFTEEN };
```

And that concludes what we need to know about the AST!

#### A note on `def`

Actually, hold on. None of this deals with functions yet. We need to add another
wrinkle to this.

I designed the parser to parse one expression at a time. This is normal, but
it’s also an incomplete grammar. Our example above has at least two expressions
(two functions), and we need to be able to deal with that.

We also do not have an AST node for functions. This is odd, since functions are
definitely part of the AST, but in our minimal language where functions are
always the top-level concept, we can get away with something simpler:

```
typedef struct {
  char name[32];
  char param[32];
  node *body;
  cj_label entry;
  int (*fn)(int);
} function;
```

The function structure is mostly self-explanatory: we keep the name and
parameter, and the expression that constitutes the function body. And once
again we do not create a separate intermediate representation; instead, the
function that we emit as well as the label we assign it—more on that
later—get inserted directly into the same structure. Not clean, but it serves
us well.

Our functions from above would thus look like this:

```
function inc   = { .name="inc", .param="x", .body=&INC_BODY };
function main  = { .name="main", .param="x", .body=&MAIN_BODY };
```

We are now ready to sketch a quick and dirty main function:

```
int main(void) {
  static const char *input = "(def main (x) (sub (call inc x) 15))\n"
                                    "(def inc (x) (add x 2))\n";
  function functions[MAX_FUN];
  int function_count = 0;

  while (<input end condition>) functions[function_count++] = parse_function(input);
  
  // ... here goes the rest
}
```

The [actual parser](https://github.com/hellerve-pl-experiments/cj/blob/fa7405823721187454a01e071421fdbe40179e20/examples/minilang.c#L281-L293)
has a lexer and is generally a bit more fiddly, but none of this matters for
this tutorial.

Let’s take a quick peek at how we parse the function to see what we start with,
and then we can get started with the compiler for real:

```
static function parse_function(input) {
  function fn;
  memset(&fn, 0, sizeof(fn));
  
  fn.name = parse_function_head(input);
  fn.param = parse_function_param(input);
  fn.body = parse_expr(input);
  
  return fn;
}

```

So by the time we get our grubby little hands on the function, the parse-y bits
have been filled, but the compile-y bits are still TBD.

Now our real job begins!

#### Part I: Resolving calls

The first thing we need to do is to resolve function calls first so we can jump
between things later. After parsing, we call a function named `resolve_calls()`
that looks like this:

```
static int find_function(function *fns, int count, const char *name) {
  for (int i = 0; i < count; i++) {
    if (strcmp(fns[i].name, name) == 0) return i;
  }
  return -1;
}

static void resolve_calls(node** all_nodes, int num_nodes, function *fns, int count) {
  for (int i = 0; i < num_nodes; i++) {
    node *n = &all_nodes[i];
    if (n->kind == NODE_CALL) n->target = find_function(fns, count, n->name);
  }
}
```

This is relatively trivial code: we iterate over all the nodes we parsed (you
can check how we do that in the full implementation), find all call nodes, and
fix up its target. For our simple example, this is good enough.

It completes our nodes and sets the target of all calls correctly, ensuring
that we can jump to them correctly when time comes.

Next up: compiling functions!

#### Part II: Compiling functions

A note before we begin: it’s important to remember that what we are doing here
during the compile process is essentially filling a buffer. A lot of this code
looks similar to an interpreter, but none of the code actually gets executed
here. Instead, we just push all of the code into a big buffer, and then generate
a function from it later. Keep this in mind as we move along.

Before we actually emit code for the functions, we need to assign a label for
each of them. You can think of a label as a place for us to jump to from a GOTO,
only in machine code. So go ahead and do this really quickly in our main
function:

```
int main(void) {
  static const char *input = "(def main (x) (sub (call inc x) 15))\n"
                                    "(def inc (x) (add x 2))\n";
  function functions[MAX_FUN];

  // parse-y and resolve-y bits ...

  cj_ctx *cj = create_cj_ctx();
  for (int i = 0; i < function_count; i++) functions[i].entry = cj_create_label(cj);

  // ... here goes the rest
}
```

What we do here is create a `cj` context (the bits that store all of its state),
and then we assign a label to each of the functions. We will use these in our
backend when calling the functions.

So one half of the functions struct is filled. We need to fill the other half,
and generate the callable version of the function.

```
typedef struct {
  cj_ctx *cj;
  function *functions;
  cj_builder_scratch scratch;
} codegen;

int main(void) {
  static const char *input = "(def main (x) (sub (call inc x) 15))\n"
                                    "(def inc (x) (add x 2))\n";
  function functions[MAX_FUN];

  // setup ...

  codegen cg = {.cj = cj, .functions = functions};
  for (int i = 0; i < function_count; i++) emit_function(&cg, &functions[i]);

  // ... here goes the rest
}
```

In our main function, we just wrap the context and the list of functions in a
single structure called `codegen` for ease of access (and tuck in a
`cj_builder_scratch`, which we will learn about later), and call
`emit_function` on each of our functions. What does `emit_function` look like,
then?

```
static void emit_function(codegen *cg, function *fn) {
  cj_builder_scratch_init(&cg->scratch);
  cj_mark_label(cg->cj, fn->entry);
  cj_builder_frame frame;
  cj_builder_fn_prologue_with_link_save(cg->cj, 0, &frame);
  cj_operand result = emit_expr(cg, fn->body);
  cj_builder_return_value(cg->cj, &frame, result);
  cj_builder_scratch_release(&cg->scratch);
}
```

What a simple function, right?! Nonetheless, it’s doing some very complicated
book-keeping and we should probably go through it line-by-line.

First we initialize the `cj_builder_scratch` thingie we created. This structure
gives us fresh registers for a frame, so we re-initialize it for every function;
we will see it in action below<sup><a href="#1">1</a></sup>.

We then set the label of the function our emitted code using `cj_mark_label`;
when the function is called, we jump there.

Next, we emit a prologue. What is a prologue? Basically, each architecture has
a different calling convention, and there are common chores we have to do when
initializing a frame, initializing registers and stack pointers and so on. The
“high level” API of `cj` abstracts this for us. Well, kind of. We still need to
do some special work for arm64, which is expressed by the `with_link_save` part
of the function. It basically saves the previous caller’s information on the
stack before we begin (only on arm64).

Then we finally are ready to emit the function body, calling `emit_expr`. It
will return the value of the last expression (a `cj_operand`) that we can then
mark as the return value and release the scratch.

#### Part III: Compiling expressions

So, how do we actually compile this AST now? Let’s look at `emit_expr` to figure
it out!

```
static cj_operand emit_expr(codegen *cg, node *n) {
  switch (n->kind) {
  case NODE_NUM: {
    cj_operand dst = cj_builder_scratch_acquire(&cg->scratch);
    cj_builder_assign(cg->cj, dst, cj_make_constant((uint64_t)(uint32_t)n->value));
    return dst;
  }
  case NODE_PARAM: {
    cj_operand dst = cj_builder_scratch_acquire(&cg->scratch);
    cj_builder_assign(cg->cj, dst, cj_builder_arg_int(cg->cj, 0));
    return dst;
  }
  case NODE_ADD:
  case NODE_SUB: {
    cj_operand lhs = emit_expr(cg, n->left);
    cj_operand rhs = emit_expr(cg, n->right);

    if (n->kind == NODE_ADD) cj_add(cg->cj, lhs, rhs);
    else cj_sub(cg->cj, lhs, rhs);

    cj_builder_scratch_release(&cg->scratch);
    return lhs;
  }
  case NODE_CALL: {
    cj_operand arg = emit_expr(cg, n->arg);
    return cj_builder_call_unary(cg->cj, &cg->scratch, cg->functions[n->target].entry, arg);
  }
  }
}
```

That one is a lot, so we’ll go through all the cases in turn. For now we just
focus on perceiving that we have a big `switch` statement for each of the AST
nodes we have defined.

Let’s start with the number node.

```
case NODE_NUM: {
  cj_operand dst = cj_builder_scratch_acquire(&cg->scratch);
  cj_builder_assign(cg->cj, dst, cj_make_constant((uint64_t)(uint32_t)n->value));
  return dst;
}
```

Here we’re using our scratch to create a register and then assign the constant
in our node to it (this boils down to a `MOV` instruction).

Parameters work similarly:

```
case NODE_PARAM: {
  cj_operand dst = cj_builder_scratch_acquire(&cg->scratch);
  cj_builder_assign(cg->cj, dst, cj_builder_arg_int(cg->cj, 0));
  return dst;
}
```

The key difference between this and the number code is that we put the argument
value in the register instead of a constant. `cj` takes care of handling these
arguments for us by respecting the platform’s calling convention, locating the
appropriate register, and returning it as an operand we can pass into the
assignment, all in `cj_builder_arg_int`, which returns an operand based on the
parameter index.

Addition and subtraction are the first recursive cases:

```
case NODE_ADD:
case NODE_SUB: {
  cj_operand lhs = emit_expr(cg, n->left);
  cj_operand rhs = emit_expr(cg, n->right);

  if (n->kind == NODE_ADD) cj_add(cg->cj, lhs, rhs);
  else cj_sub(cg->cj, lhs, rhs);

  cj_builder_scratch_release(&cg->scratch);
  return lhs;
}
```

We emit our left and right arguments (since they are expressions themselves),
and then emit either an `ADD` or a `SUB` expression. Then we can release one of
our scratch arguments, and return the left-hand side expression. Why the
left-hand side expression? Both `ADD` and `SUB` will store the result of their
respective operation in the register on the left, so that is the one we will
return. This also explains why we release one argument only: the right-hand side
can be forgotten about, but the left needs to be preserved.

The final trick is calling:

```
case NODE_CALL: {
  cj_operand arg = emit_expr(cg, n->arg);
  return cj_builder_call_unary(cg->cj, &cg->scratch, cg->functions[n->target].entry, arg);
}
```

We emit our argument first, then emit a call to the function we want to call.
The high-level API of `cj` takes care of this for us once again, taking care of
argument initialization, calling labels, and cleaning up after the call. One
final complication is that we need to know the label of what we’ll call. In this
expression, we just use the node’s `target` field we defined above in Part I.

We now have everything in place, and can generate code and call it!

#### Part IV: Execution

As a reminder, this is the code we want to generate:

```
# main is our entry point, call calls other functions
(def main (x) (sub (call inc x) 15))

# add and sub do math, all functions are unary
(def inc (x) (add x 2))
```

We now have generated all the code and are ready to call our JIT-compiled
function:

```
int main(int argc, char **argv) {
  // all of our setup

  cj_fn module = create_cj_fn(cj);

  int main_idx = find_function(functions, function_count, "main");
  int (*main_fn)(int) = cj_resolve_label(cj, module, functions[main_idx].entry);
  int result = main_fn(55);
  printf("main(55) = %d\n", result); // will print 42

  // cleanup goes here
}
```

All we do is generate all of our code, find the offset of our main function, and
call it! If this seems like magic, it kind of is: `create_cj_fn` tells the
machine that this buffer is not code anymore but data instead. The call to
`cj_resolve_label` calculates the offset into that buffer that leads us to our
label and gives us the pointer to it. By casting it to a function pointer, we
can then just call it.

And that’s it! In just over 90 lines of code, we compiled a non-trivial program
and executed it, *all at runtime*. Pretty cool, right?

### Fin

This was a lot, certainly. Admittedly, even with the “high level” helpers,
building anything with `cj` is still very low-level (by design). I still hope it
is somewhat accessible for all, and that, if you so desire, you explore the
[other examples](https://github.com/hellerve-pl-experiments/cj/tree/master/examples)
`cj` ships with. See you around!

#### Footnotes

<span id="1">1.</span> Register names and purposes are backend-dependent, and
I wanted to build a way to get to registers when I need them without having to
think about any of that. What we explicitly do not do is manage the registers
for the user, doing full [register
allocation](https://en.wikipedia.org/wiki/Register_allocation). I might get
there in the future, but currently `cj` does not provide this.
