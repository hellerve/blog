A question that comes up every once in a while on the Carp chatroom and in
Github issues is how memory management works. People who are not familiar with
Rust especially have understandable trouble grasping how to program with
references, and why some functions take references while others don’t. Today,
I will try to explain these concepts for my fellow Carpenters.

A fair note: while this blog post might be beneficial to your understanding of
the Rust borrow checker, this is not my focus. I don’t know Rust very well, and
it is entirely possible that it does things differently and I’m just confusing
your understanding of how its machinery works. I therefore suggest that you
don’t try to compare the two too much lest you fall prey to faulty assumptions.

## Who owns what?

## Fin
