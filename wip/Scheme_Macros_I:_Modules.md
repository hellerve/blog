One of the most powerful—and, for a large part of the programmer community,
most dreaded—features of Lisp is its metaprogramming. In this series I'll
attempt to give a few interesting cases for how we can level up our code
through the use of macros.

This time we will look at how to implement a macro system. Many Lisps already
come with a macro system out of the box—both R7RS and Common Lisp provide a
facility for packaging and namespacing libraries and modules—, but it is still
helpful to know how these abstractions are built and look from the inside.

Let's dive right in!
