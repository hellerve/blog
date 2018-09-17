On the 15th of September I gave my first recorded talk on Carp at clojuTRE 2018.
I had a lot of fun, and now the recording is online. I want to talk a bit about
my experience, but first I’ll share the talk itself with you!

<iframe width="560" height="315"
        src="https://www.youtube.com/embed/BQeG6fXMk28" frameborder="0"
        allow="autoplay; encrypted-media" allowfullscreen>
</iframe>

With that out of the way, let’s talk a bit about the parts you can’t see in this
video. It was my first talk at a major conference—to me, that means more than
100 people—and I was scared appropriately. My slot was the first one after
lunch, and I decided not to eat in order to be at my full mental capacity. I
walked around the entire conference venue, controlled my breathing, chatted a
little with other people, that kind of stuff.

All in all, I think it went well. I would have preferred if I wouldn’t have used
so many filler words, especially in the beginning. I’ll work on it. I also
didn’t answer the questions as well as I could have. Some I didn’t answer at
all, because I went off on a tangent instead, and that’s not great. I did talk
to a few of the askers afterwards and gave better answers, but they are not
recorded. In case you want to know what I would like to add to my answers in
retrospect, here goes:

*Question:* Do you do type checking before or after macro expansion?

*Answer:* I’m happy with the answer in the talk. We do that afterwards, because
it facilitates metaprogramming.

*Question:* I’m a little bit clueless about borrow-checking, and it seems to me
to make it very hard to make applicative data structures or persistent data
structures that share parts, like lists that share their tail or something like
that. So how does that work in Carp and if it doesn’t, who in their right mind
would make a Lisp with borrow checking?

*Answer:* As to the “who would do that” part, well—yours truly. As for the data
structures, we don’t have them, so that answer is true, but it’s certainly not
impossible to have them. Carp does a bunch of internal safe mutability based on
borrowing semantics instead, which do not enable some of the “undo” semantics of
Clojure, but make it more efficient. Of course, I decided to talk about the Carp
data structures instead of answering the question.

*Question:* Thanks, that was a nice presentation, I’d love to try out Carp. But
why should I try Carp and not Rust?

*Answer:* I’m happy with that answer also. If you’re looking to play with a
mature language, or just want to try a borrow checker, try Rust. If you’re
excited about the promise of Carp with its type inference and focus on
simplicity, you can help us make it better by trying it out and giving us
feedback.

*Question:* You mentioned earlier that you use modules rather than namespaces.
So, are these like ML-style modules? Do you have something like functors as
well?

*Answer:* No, we decided to use modules mostly for grouping, either around
functionality or around types. Noone forces you to use modules, either, but
they are very useful for encapsulation.

These answers are more concise and more informative, and I should probably work
on my focus when answering questions. While some of the tangents I went into
might have been interesting, they ended up obscuring the actual answers.

I should also have mentioned where the code actually lies, and that there is a
Gitter channel that people can join if they are interested. Oh well, hindisght
is 20/20.

I’m looking forward to applying all of these ideas to my next talk on Carp! If
you want to see what I’ll be up to in the future, you can look at my [Carp talks
repo](https://github.com/hellerve/carp-talks)!

Finally, if you want me to speak about Carp somewhere, [reach out to me](mailto:veit@veitheller.de)!
I can speak for as short or long a time period as you would like me to.
