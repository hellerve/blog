It’s time for another installation of my series on Scheme macros. Previously,
we talked about defining [modules](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html),
[generic functions](http://blog.veitheller.de/Scheme_Macros_II:_Generics.html),
and [local variable binding](http://blog.veitheller.de/Scheme_Macros_III:_Defining_let.html)
using macros. This time I want to write about gradual typing, and how we can use
it to add some contracts to our code, without programming
overhead<sup><a href="#1">1</a></sup>.

#### Footnotes

<span id="1">1.</span> Fun fact: I’ve wanted to write about gradual typing for
                       almost as long as I had this blog. See also [this
                       issue](https://github.com/hellerve/blog/issues/8) on
                       Github.
