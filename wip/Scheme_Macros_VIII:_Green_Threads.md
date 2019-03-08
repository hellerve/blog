In this eighth installment of [my series of blog posts on scheme
macros](https://blog.veitheller.de/scheme-macros/), we’re going to look at a
feature that is increasingly common in modern languages: [green threads](https://en.wikipedia.org/wiki/Green_threads).
In a nutshell, greenthreads are a lightweight threading facility removed from
operating system threads to save the cost of context switching on the operating
system level—oftentimes language-level threads can switch much faster.

Implementing green threads in Scheme is not hard, but it requires a somewhat
firm grasp of [continuations](https://en.wikipedia.org/wiki/Continuation). As
this series is first and foremost about macros, I won’t have time to explain
continuations from the ground up. I’ll try to help you develop an intuition 
for what they are as we start to work with them, and gradually fill in any gaps that
we’ll need to fill in order to understand the code. This is probably not a
very good introductory text on continuations, and it's definitely not exhaustive.

With all that out of the way, let’s start implementing some green threads!
As always, the code for this blog post is [on Github](https://github.com/hellerve/pi).

## An API

As per usual, we start by defining an interface. Our scheduling will be based on
[cooperative multitasking](https://en.wikipedia.org/wiki/Cooperative_multitasking),
which means that threads will voluntarily—and explicitly—give up control by
calling `yield`. They will communicate via channels, which are basically
glorified [FIFO](https://en.wikipedia.org/wiki/FIFO_%28computing_and_electronics%29)
queues. Don’t worry if you don’t know some or even any of these concepts, we’ll
talk a little more about them as we go.

Let’s look at some code first, though!

```
; channels are defined by chan:empty
(define channel (chan:empty))

; you can create new threads by calling fork
(fork
  (lambda ()
    (begin
      (write "first thread")
      ; and you can take values from channels
      ; using chan:take
      (write (++ "Received: "
                 (chan:take channel))))))

(fork
  (lambda ()
    (begin
      (write "second thread")
      ; similarly, you can put values into
      ; channels using chan:put
      (chan:put channel "Hello, World!"))))

; here the main thread voluntarily gives up
; control to the forked ones using yield
(yield)
```
<div class="figure-label">Fig. 1: A simple multi-threaded program.</div>

Let’s think about the control flow in Figure 1 a bit. First we create a global
channel and two forked threads. Then our main thread gives up control. Our
scheduling is using a [round robin](https://en.wikipedia.org/wiki/Round-robin_scheduling)
algorithm, which is a fancy way of saying “first come first served”. That means
that the first thread we created will also be the first one to gain
control. It writes to the standard output and then tries to take a value from
the channel. Because there is no value in the channel yet, the thread will
give up control. This will activate the second thread, which will also print
and then put a string into the channel and exit. The first thread will activate
again, take from the channel, print, and exit. Then our main channel will exit
as well.

It is important to note that all of this happens concurrently—i.e. tasks will
overlap—, but not parallel—nothing will ever happen at the same time. If
we had a facility that allows us to run different continuations in different
operating system threads, we could potentially also add parallelism. But we’ll
not open that can of worms, partly because it would complicate the
implementation, and partly because it’s easier to reason about deterministically
concurrent programs, which is important in the context of a blog post.

There is another function that we haven’t looked at yet, called `chan:select`.
It allows the user to select from multiple different channels, and is the
single most complicated piece of machinery in the whole system. It’s also a
pretty big macro, so we’ll have a lot of fun with it. Before we do that,
however, let’s look at its use case:

```
(define chan (chan:empty))
(define chan2 (chan:empty))

(fork
  (lambda ()
    (for 2
      ((chan:select
        (chan (lambda (x) (write x)))
        (chan2 (lambda (x) (write x))))))))

(fork (lambda () (chan:put chan2 "hello")))
(fork (lambda () (chan:put chan  "world")))

(yield)
```
<div class="figure-label">Fig. 2: `chan:select` in action.</div>

I won’t go through the control flow of Figure 2 with you, and I hope that this
example illustrates somewhat well what `chan:select` is for.

And that’s the whole API! If you’re anything like me, you’re eager to get to
implementing that thing right now, so let’s get crackin!

## An Implementation

Our implementation can be divided roughly into two parts: the core of the 
execution engine, which yields, forks, and schedules, and the channel part, 
which passes messages around.

### Forking, scheduling, yielding

To ease into the waters, we’ll start with the simplest pieces of machinery
first. Perhaps surprisingly, one of the simplest things we can do in our model
is forking.

```
(define ready [])

(define (fork fn)
  (set! ready
        (++ ready
            (lambda (_) (begin (fn) (sched))))))
```
<div class="figure-label">Fig. 3: Forking a thread.</div>

What are we doing here? First we define a global list called `ready` that’ll
serve as the list of threads that are ready to execute. To fork, we’ll have to
append to that list. We cannot simply append the function `fn` that we are given
in `fork`, however. We’ll have to wrap that function in a unary lambda that
ignores its argument—we’ll explain why that is in a second. The lambda will
execute our user-defined function and then call `sched`, a function that we
haven’t defined yet, but which will serve as a kind of book-keeping function.

Let’s look at `sched` next to get a more complete picture of what happens when
we schedule between threads.

```
(define (sched)
  (if (not (null? ready))
    (let ((cont (head ready)))
      (begin
        (set! ready (tail ready))
        (cont '())))))
```
<div class="figure-label">Fig. 4: Our scheduler.</div>

That’s our complete scheduler. All it does is check whether our list of threads
that are ready to execute is empty, and if it isn’t, it will take the first one,
set the list to all elements but the one we just took, and call the first task
with an empty list.

Now we know why the lambda we create inside `fork` takes an argument, but we
still don’t know why we have to pass that empty list. The answer has to do with
continuations, and we’ll reveal it when we look at how `yield` works.

Yielding works through continuations, so it is now time to explain what those
are in brief. In a nutshell, continuations are the complete state of your
program as a value. If you have an interpreter, it might be the interpreter
state that is passed around inside the evaluation loop. Continuations are useful
in a lot of ways, and many of them have to do with control flow.

What makes Scheme continuations even more interesting is that you can call them.
When calling them, you essentially resume execution where you left off when you
generated the continuation value. If we’re thinking about the interpreter state
again, it’s basically telling your interpreter that the state we cached is
now its current state.

This is tremendously powerful, and a little mind-boggling when you hear about it
for the first time. It’s also completely normal to have a little trouble with
the concept in the beginning. Let’s look at the continuations in use to solidify 
some of our intuitions!

```
(define (yield)
  (call-with-current-continuation
    (lambda (c)
      (begin
        (set! ready (++ ready c))
        (sched)))))
```
<div class="figure-label">Fig. 5: Yielding.</div>

On the surface level, yielding looks somewhat similar to forking. We use a
famous Scheme function called `call-with-current-continuation`—often abbreviated
as `call/cc`—that takes a function that takes the continuation as an argument.
The function we’re using in this context is a lambda that takes a continuation
called `c`, appends it to the end of the list of threads ready to execute, and
calls our scheduler. Effectively it tells the scheduler to call our continuation
again, after all of the other things that were scheduled before are executed.

Calling continuations in zepto requires exactly one argument, which will be the
return value of the continuation. `c` is thus a unary function, and because we
don’t want the scheduler to care whether the thread it’s waking is a
function—which happens when we `fork`—or a continuation—which happens when we
`yield`—we need to provide exactly one dummy argument.

At this point, we’re done with implementing green threads themselves, and it
took us around 15 lines of–admittedly–a little dense code. We could stop here
and call it a day, except we haven’t written a single macro yet, and we
want to have message passing as well. So let’s look at channels, and see whether
we can write a few macros there.

### Channels

We have a bunch of low-hanging fruit to harvest in our representation of
channels also, so let’s implement those first.

At the very core, a channel is a list of messages that need to be processed in
order. As I already told you above, this is also called a FIFO queue! Because
we want our implementation to be a bit more explicit about data types, we’ll
make channels a proper data type with one member—the list of messages.

```
(define-struct chan (contents))
```
<div class="figure-label">Fig. 6: The definition of channels.</div>

This definition will buy us a unary constructor called `chan:make-chan`, a type
predicate called `chan:chan?`, and member getters and setters.

Let’s implement `chan:empty`, the function we’ll want our users to call when
creating a new channel.

```
(define (chan:empty) (chan:make-chan []))
```
<div class="figure-label">Fig. 7: Our user-facing channel constructor.</div>

So far, so simple. We can now create empty channels, and play around with them
a little. We’ll have to take care of `chan:put` and `chan:get` next.

```
(define-syntax chan:put
  (syntax-rules ()
    ((chan:put chan msg)
      (set! chan (chan:make-chan
                   (++ (chan:get-contents chan)
                       msg))))))
```
<div class="figure-label">Fig. 8: Putting messages into channels.</div>

Putting things into channels is simple: we get the old contents, append the
message to that list, and set our channel to be a new channel with those
contents. Simple enough, and our first macro in this blog post!

Taking messages from channels is a bit more complex: if there is no message
currently in the channel, we want to give up control flow. Luckily, our
facilities for control flow are already fully formed, so we can just call back
to them!

```
(define-syntax chan:take
  (syntax-rules ()
    ((chan:take chan)
      (let ((c (chan:get-contents chan)))
        (if (null? c)
          (begin
            (yield)
            ; this is a useful little hack to
            ; allow for on-demand recursion in
            ; this macro
            (eval (macro-expand (list 'chan:take
                                      'chan))))
          (begin
            (set! chan (chan:make-chan (tail c)))
            (head c)))))))
```
<div class="figure-label">Fig. 9: Taking messages from channels.</div>

To me, `chan:take` is mostly straightforward, but a few things probably need
explanations. If the channel is empty we yield and then execute `chan:take`
again. While the recursion certainly looks a little ugly, it is there for good
reason, and an interesting building block in your macro-building toolchain: had
we just written `(chan:take chan)` here as we usually would when recursing, the
macro would have expanded itself again, and we would have infinite recursion in
our macro, something which is both very annoying and pretty hard to debug. By
expanding the macro on-demand and then evaluating it, we save ourselves from
that grisly fate.

If on the other hand the channel is not empty, we remove the first element from
the contents and return it.

We could conceivably stop here, but the macros we’ve seen thus far have been
relatively tame, and I don't believe that’s why you're here. So, as our pièce de
résistance we will now implement `chan:select`, by far the most complex macro
of this session.

#### Selecting channels

`chan:select` will need to be recursive, both right away and on-demand; call
other macros; and have multiple rules. Before we start I want to acknowledge
that it is quite a bit of code—about 30 lines—, and it took me a while to write
it and get it right. There is nothing wrong if you take some time to
understand it—I banged my head on it for quite a while.

Let’s start with our simplest case: having just one channel and a corresponding
function to execute.

```
(define-syntax chan:select
  (syntax-rules ()
     ((chan:select (chan fn))
       (fn (chan:take chan)))
     ; other cases
  )
)
```
<div class="figure-label">Fig. 10: `chan:select` with simple case.</div>

This isn’t too bad. We’re simply calling the function with the result of
`chan:take`. We can at this point even ignore that `chan:take` is a macro!

Next, let’s tackle the recursive case, which comes at the very bottom of the
macro definition.

```
(define-syntax chan:select
  (syntax-rules ()
     ; other cases

     ((chan:select (chan fn) alt ...)
       (let ((c (chan:get-contents chan)))
         (if (null? c)
           (chan:select "internal"
                        ((chan fn) alt ...)
                        alt ...)
           (begin
             (set! chan (chan:make-chan (tail c)))
             (fn (head c))))))
  )
)
```
<div class="figure-label">Fig. 11: `chan:select` with our recursive case.</div>

What’s going on here? It looks like a half-unrolled version of `chan:take`!
Well, that’s because it is. `chan:select` behaves just like `chan:take` except
that it doesn’t block when a channel is empty, and because it doesn’t block we
have to reimplement the whole thing.

Note that most of the code is exactly the same except the section that gets
executed if the channel is empty, which mirrors what we explained in the last
paragraph. Let’s focus on the difference!

We’re calling a recursive case of `channel:select`, and we tag it with
`"internal"`, which is an old Scheme trick to differentiate between different
cases of a macro to achieve state-machine-like behavior. We also pass not only
the alternatives that are left, but the complete set of alternatives as a first
list; we’ll explain why when we walk through the other cases.

The internal cases will have to go through the rest of the channels and check
whether any of them are not empty, and if they are, recurse. Let’s look at how
we could implement that.

```
(define-syntax chan:select
  (syntax-rules ()
    ; simple and base cases

    ; i only use _ here to shorten the line
    ((_ "internal" alts (chan fn) alt ...)
      (let ((c (chan:get-contents chan)))
        (if (null? c)
          (chan:select "internal" alts alt ...)
          (begin
            (set! chan (chan:make-chan (tail c)))
            (fn (head c))))))

    ; recursive case
  )
)
```
<div class="figure-label">
  Fig. 12: The second recursive, internal case of `chan:select`.
</div>

In this second recursive case we replicate a bunch of the work we did before.
The only thing that is different is again the case in which the channel is
empty, where we throw away the channel-function pair that we just checked. Now
we’re actually recursing in a way that will gradually take away cases, and we’re
ready for the final base case!

```
(define-syntax chan:select
  (syntax-rules ()
    ; simple case

    ((chan:select "internal" alts (chan fn))
      (let ((c (chan:get-contents chan)))
        (if (null? c)
          (begin
            (yield)
            (eval (macro-expand (cons 'chan:select
                                      'alts))))
          (begin
            (set! chan (chan:make-chan (tail c)))
            (fn (head c))))))

    ; recursive cases
  )
)
```
<div class="figure-label">
  Fig. 13: The internal base case of `chan:select`.
</div>

Finally, this looks almost exactly like `chan:take`, except we’re recursively
calling `chan:select` instead! Now it should also be clear why we had to
preserve the list of alternatives completely throughout the recursion, for we
need it if we recurse on-demand as before.

That was a lot of code! Depending on how familiar you are with green threads,
channels, and concurrency, this might also have been a bit over your head. But I
hope that you took away at least a few good tricks that you can work with when
writing macros!

## Fin

The good news is that we’re done. The—potentially—bad news is that this post
marks the end of my series on macros, at least for now. I’ve shown you most of
the interesting macros I’ve written over the years, and explained almost
everything I know about writing them, and I say “almost” only because I’m sure
I have missed things along the way.

That said, if you have any ideas for new posts on macros, or other interesting
macros I could write/teach you about, let me know! In the meantime, you can
browse [the back catalogue](/scheme-macros).
