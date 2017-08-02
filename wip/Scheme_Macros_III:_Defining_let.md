After exploring how to implement a [module system](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html)
and [generic functions](http://blog.veitheller.de/Scheme_Macros_I:_Generics.html)
in Scheme macros, this time we’ll explore how to reimplement
[let-style](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Lexical-Binding.html)
local bindings. As a little extra, we’ll also explore a way of defining `letrec`
that I’ve never seen used before, partly because it’s somewhat inefficient—but
at least it gets rid of mutable state.
