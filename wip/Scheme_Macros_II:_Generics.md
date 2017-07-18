This is a continuation of [an earlier post](http://blog.veitheller.de/Scheme_Macros_I:_Modules.html),
in which I talked about the power of Scheme macros in the context of modules.
Modules aren't the only use case of macros, though, and so this time I want to
talk about generic functions as an another case study.

Generic functions are a milestone in any programming language. Most languages
use some kind of interface structure, no matter whether they call them
interfaces, protocols, traits, or type classes. zepto calls them protocols.

Let's define protocols as a structure and then go about implementing them!
